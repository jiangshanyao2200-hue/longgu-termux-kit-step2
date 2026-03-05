use super::*;
use portable_pty::{CommandBuilder, PtySize, native_pty_system};
use std::sync::Mutex;

// ===== 注释链（PTY：交互终端与长任务）=====
//
//（1）A 工具入口
//（2）mcp 工具 pty 只负责解析；实际执行在 core 主线程：
//（3）core.rs:spawn_tool_execution（tool=pty）
//（4）-> AsyncEvent::PtyToolRequest
//（5）-> pseudo_terminal.rs:handle_pty_tool_request -> spawn
//（6）-> AsyncEvent::PtyReady
//
//（1）B 输出与结束
//（2）AsyncEvent::PtyOutput -> handle_async_event_pty_output
//（3）-> vt100 parser
//（4）AsyncEvent::PtyJobDone -> 
//（5）handle_async_event_pty_job_done -> 导出/摘要 ->（可选）回灌模型上下文
//
//（1）C 渲染与交互
//（2）ui.rs:draw_chat（主聊天）
//（3）pseudo_terminal.rs:draw_pty_panel（Terminal 面板）
//（4）handle_pty_view_key / touch drag / mouse wheel：只影响 PTY
//（5）面板滚动与焦点

static PTY_JOB_SEQ: AtomicU64 = AtomicU64::new(1);
static ACTIVE_PTY_COUNT: AtomicU64 = AtomicU64::new(0);
static PTY_DONE_EXPORT_SEQ: AtomicU64 = AtomicU64::new(1);

pub(super) fn next_pty_job_id() -> u64 {
    PTY_JOB_SEQ.fetch_add(1, Ordering::Relaxed).max(1)
}

pub(super) fn active_pty_count() -> u64 {
    ACTIVE_PTY_COUNT.load(Ordering::Relaxed)
}

fn next_pty_done_export_id() -> u64 {
    PTY_DONE_EXPORT_SEQ.fetch_add(1, Ordering::Relaxed).max(1)
}

#[derive(Debug, Clone)]
pub(super) enum PtyControl {
    Input(Vec<u8>),
    Resize { cols: u16, rows: u16 },
    Kill,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PtyFocus {
    Terminal,
    Chat,
}

pub(super) struct ActivePtyGuard;

impl ActivePtyGuard {
    pub(super) fn new() -> Self {
        ACTIVE_PTY_COUNT.fetch_add(1, Ordering::Relaxed);
        ActivePtyGuard
    }
}

impl Drop for ActivePtyGuard {
    fn drop(&mut self) {
        ACTIVE_PTY_COUNT.fetch_sub(1, Ordering::Relaxed);
    }
}

#[derive(Clone, Debug)]
pub(super) struct PtySpawnInfo {
    pub(super) job_id: u64,
    pub(super) saved_path: String,
    pub(super) status_path: String,
}

pub(super) struct PtyUiState {
    pub(super) ctrl_tx: mpsc::Sender<PtyControl>,
    pub(super) parser: vt100::Parser,
    pub(super) cols: u16,
    pub(super) rows: u16,
    pub(super) pending_cols: u16,
    pub(super) pending_rows: u16,
    pub(super) pending_since: Option<Instant>,
    pub(super) scroll: u16,
    pub(super) scroll_applied: u16,
	pub(super) owner: MindKind,
	pub(super) user_initiated: bool,
	pub(super) job_id: u64,
    pub(super) cmd: String,
    pub(super) saved_path: String,
    pub(super) status_path: String,
    pub(super) started_at: Instant,
    pub(super) pid: Option<i32>,
    pub(super) pgrp: Option<i32>,
    //（1）按用户决策：交互输入也记录并回传给 AI（含敏感信息）。
    pub(super) input_log: String,
    //（1）用户在终端里主动生成的快照（Alt+↑）。用于暂存到输入框；旧链路 final 回传会优先用它。
    pub(super) last_user_snapshot: Option<String>,
    pub(super) screen_lines: Vec<String>,
    pub(super) dirty: bool,
}

pub(super) struct PtyUiStateNewArgs {
    pub(super) ctrl_tx: mpsc::Sender<PtyControl>,
    pub(super) cols: u16,
    pub(super) rows: u16,
    pub(super) owner: MindKind,
    pub(super) user_initiated: bool,
    pub(super) job_id: u64,
    pub(super) cmd: String,
    pub(super) saved_path: String,
    pub(super) status_path: String,
}

impl PtyUiState {
    pub(super) fn new(args: PtyUiStateNewArgs) -> Self {
	    	        let PtyUiStateNewArgs {
            ctrl_tx,
            cols,
            rows,
            owner,
            user_initiated,
            job_id,
            cmd,
            saved_path,
            status_path,
        } = args;

        //（1）scrollback 给一个较大的默认值，保证交互期可回看。
        let parser = vt100::Parser::new(rows, cols, PTY_SCROLLBACK_MAX as usize);
        Self {
            ctrl_tx,
            parser,
            cols,
            rows,
            pending_cols: cols,
            pending_rows: rows,
            pending_since: None,
            scroll: 0,
            scroll_applied: 0,
            owner,
            user_initiated,
            job_id,
            cmd,
            saved_path,
            status_path,
            started_at: Instant::now(),
            pid: None,
            pgrp: None,
            input_log: String::new(),
            last_user_snapshot: None,
            screen_lines: Vec::new(),
            dirty: true,
        }
    }

    pub(super) fn process_output(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        self.parser.process(bytes);
        //（1）终端模拟（最小集）：响应 Cursor Position Report (CPR) 查询 `ESC[6n`。
        //（2）一些 TUI（例如 codex/crossterm）会向 stdout 写入 `ESC[6n`，
        //（3）并期待终端回写 `ESC[{row};{col}R` 到 stdin。
        //（4）在我们的“PTY + vt100 parser”架构中，若不响应，应用会报类似：
        //（5）"Error: The cursor position could not be read..."
        const CPR: [u8; 4] = [0x1B, b'[', b'6', b'n'];
        const SR: [u8; 4] = [0x1B, b'[', b'5', b'n']; // Status Report
        const DA1: [u8; 2] = [0x1B, b'c']; // Device Attributes
        const DA2: [u8; 4] = [0x1B, b'[', b'0', b'c']; // Device Attributes (explicit 0)

        if bytes.windows(SR.len()).any(|w| w == SR) {
            let _ = self.ctrl_tx.send(PtyControl::Input(b"\x1b[0n".to_vec()));
        }
        if bytes.windows(DA1.len()).any(|w| w == DA1) || bytes.windows(DA2.len()).any(|w| w == DA2)
        {
            let _ = self.ctrl_tx.send(PtyControl::Input(b"\x1b[?1;0c".to_vec()));
        }
        if bytes.windows(CPR.len()).any(|w| w == CPR) {
            let (row0, col0) = self.parser.screen().cursor_position();
            let row = row0.saturating_add(1);
            let col = col0.saturating_add(1);
            let resp = format!("\x1b[{row};{col}R");
            let _ = self.ctrl_tx.send(PtyControl::Input(resp.into_bytes()));
        }
        self.dirty = true;
    }

    pub(super) fn record_input_bytes(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        //（1）只做最小可读化记录：可打印字符保留，其它控制字符用占位。
        for &b in bytes {
            match b {
                b'\r' | b'\n' => self.input_log.push('\n'),
                0x20..=0x7E => self.input_log.push(b as char),
                0x1B => self.input_log.push_str("<ESC>"),
                0x7F => self.input_log.push_str("<BS>"),
                0x03 => self.input_log.push_str("<C-C>"),
                _ => self.input_log.push_str("<CTRL>"),
            }
        }
    }

    pub(super) fn ensure_size(&mut self, cols: u16, rows: u16) {
        if cols == 0 || rows == 0 {
            return;
        }
        if self.cols == cols && self.rows == rows && self.pending_since.is_none() {
            return;
        }
        if self.pending_cols != cols || self.pending_rows != rows {
            self.pending_cols = cols;
            self.pending_rows = rows;
            self.pending_since = Some(Instant::now());
        }
        //（1）约束：光标必须稳定显示在终端尾部；键盘弹出/收回会导致尺寸变化，
        //（2）若 debounce 过长会造成“光标漂移/错位”观感。因此这里选择立即 resize。
        self.cols = self.pending_cols;
        self.rows = self.pending_rows;
        self.pending_since = None;
        self.parser.set_size(self.rows, self.cols);
        let _ = self.ctrl_tx.send(PtyControl::Resize {
            cols: self.cols,
            rows: self.rows,
        });
        self.dirty = true;
    }

    pub(super) fn rebuild_cache(&mut self) {
        if !self.dirty && self.scroll_applied == self.scroll {
            return;
        }
        self.parser.set_scrollback(self.scroll as usize);
        let applied = self.parser.screen().scrollback().min(u16::MAX as usize) as u16;
        self.scroll = applied;
        self.scroll_applied = applied;
        let text = self.parser.screen().contents().to_string();
        //（1）vt100 的 contents 可能带末尾 '\n'，split('\n') 会多出一个空行导致行数漂移。
        //（2）这里统一用 split_terminator 并严格 pad/truncate 到 rows，
        //（3）保证光标坐标与渲染一致。
        let mut lines: Vec<String> = text.split_terminator('\n').map(|s| s.to_string()).collect();
        let want = self.rows.max(1) as usize;
        if lines.len() < want {
            lines.extend(std::iter::repeat_n(String::new(), want - lines.len()));
        } else if lines.len() > want {
            lines.truncate(want);
        }
        self.screen_lines = lines;
        self.dirty = false;
    }

    pub(super) fn snapshot_plain(&mut self) -> String {
        self.rebuild_cache();
        self.screen_lines.join("\n").trim_end().to_string()
    }
}

pub(super) fn key_to_pty_bytes(code: KeyCode, mods: KeyModifiers) -> Option<Vec<u8>> {
    let ctrl = mods.contains(KeyModifiers::CONTROL);
    let alt = mods.contains(KeyModifiers::ALT);

    let mut out: Vec<u8> = Vec::new();
    if alt && !matches!(code, KeyCode::Esc) {
        out.push(0x1B);
    }
    match code {
        KeyCode::Enter => out.push(b'\r'),
        KeyCode::Tab => out.push(b'\t'),
        KeyCode::Backspace => out.push(0x7F),
        KeyCode::Esc => out.push(0x1B),
        KeyCode::Char(ch) => {
            if ctrl {
                //（1）按常规定义：Ctrl + ASCII('@'..='_') => codepoint & 0x1F
                //（2）这也覆盖了常用的 Ctrl+[ == ESC (0x1B) 等组合。
                if ch.is_ascii() {
                    let c = ch.to_ascii_uppercase() as u8;
                    if (b'@'..=b'_').contains(&c) {
                        out.push(c & 0x1F);
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                let mut buf = [0u8; 4];
                let s = ch.encode_utf8(&mut buf);
                out.extend_from_slice(s.as_bytes());
            }
        }
        KeyCode::Up => out.extend_from_slice(b"\x1b[A"),
        KeyCode::Down => out.extend_from_slice(b"\x1b[B"),
        KeyCode::Right => out.extend_from_slice(b"\x1b[C"),
        KeyCode::Left => out.extend_from_slice(b"\x1b[D"),
        KeyCode::Home => out.extend_from_slice(b"\x1b[H"),
        KeyCode::End => out.extend_from_slice(b"\x1b[F"),
        KeyCode::Delete => out.extend_from_slice(b"\x1b[3~"),
        KeyCode::Insert => out.extend_from_slice(b"\x1b[2~"),
        KeyCode::PageUp => out.extend_from_slice(b"\x1b[5~"),
        KeyCode::PageDown => out.extend_from_slice(b"\x1b[6~"),
        _ => return None,
    }
    Some(out)
}

#[derive(Debug, Clone)]
pub(super) struct PtyAuditTarget {
    pub(super) owner: MindKind,
}

pub(super) fn current_pty_audit_target(
    pty_tabs: &[PtyUiState],
    pty_active_idx: usize,
) -> Option<PtyAuditTarget> {
    if pty_tabs.is_empty() {
        return None;
    }
    let idx = pty_active_idx.min(pty_tabs.len().saturating_sub(1));
    let state = &pty_tabs[idx];
    Some(PtyAuditTarget { owner: state.owner })
}

pub(super) fn running_owner_count(pty_tabs: &[PtyUiState], owner: MindKind) -> usize {
    pty_tabs.iter().filter(|s| s.owner == owner).count()
}

pub(super) fn is_pty_panel_active(screen: Screen, pty_view: bool, pty_tabs: &[PtyUiState]) -> bool {
    matches!(screen, Screen::Chat) && pty_view && !pty_tabs.is_empty()
}

pub(super) fn pty_bottom_height(avail: u16, input_h: u16, pty_panel_active: bool) -> u16 {
    let max_input_h = avail.saturating_sub(1).max(1);
    let pty_h = (avail / 2).max(6).min(max_input_h);
    if pty_panel_active { pty_h } else { input_h }
}

pub(super) fn pty_status_line_override(
    screen: Screen,
    pty_tabs: &[PtyUiState],
    pty_active_idx: usize,
    pty_view: bool,
) -> Option<String> {
    if !matches!(screen, Screen::Chat) || pty_tabs.is_empty() {
        return None;
    }
    let total = pty_tabs.len();
    let active = pty_active_idx
        .min(total.saturating_sub(1))
        .saturating_add(1);
    Some(if pty_view {
        format!("Terminal / 运行中 · {active}/{total}")
    } else {
        format!("Terminal / 运行中 · {total} 个任务")
    })
}

#[derive(Debug, Clone, Copy)]
pub(super) enum PtyStatusPhase {
    Starting,
    Running,
    Done,
    Timeout,
    UserExit,
}

#[derive(Debug, Clone)]
pub(super) struct PtyDoneJob {
    pub(super) job_id: u64,
    pub(super) cmd: String,
    pub(super) user_initiated: bool,
    pub(super) saved_path: String,
    pub(super) status_path: String,
    pub(super) status: String,
    pub(super) exit_code: i32,
    pub(super) elapsed_ms: u128,
    pub(super) bytes: usize,
    pub(super) lines: usize,
    pub(super) tail: String,
}

#[derive(Debug, Default, Clone)]
pub(super) struct PtyDoneBatch {
    pub(super) total_started: usize,
    pub(super) completed: usize,
    pub(super) jobs: Vec<PtyDoneJob>,
}

#[derive(Debug, Default)]
pub(super) struct PtyDoneBatches {
    pub(super) main: PtyDoneBatch,
    pub(super) sub: PtyDoneBatch,
    pub(super) memory: PtyDoneBatch,
    //（1）pty.kill 会导致随后到达的 PtyJobDone 与工具回执重复/冲突。
    //（2）这里记录“应当抑制 DONE 聚合回执”的 job_id：被 kill 的任务只由 pty.kill
    //（3）工具回执汇报。
    pub(super) suppress_main: BTreeSet<u64>,
    pub(super) suppress_sub: BTreeSet<u64>,
    pub(super) suppress_memory: BTreeSet<u64>,
}

impl PtyDoneBatches {
    pub(super) fn batch_mut(&mut self, owner: MindKind) -> &mut PtyDoneBatch {
        match owner {
            MindKind::Main => &mut self.main,
            MindKind::Sub => &mut self.sub,
            MindKind::Memory => &mut self.memory,
        }
    }

    fn suppress_set_mut(&mut self, owner: MindKind) -> &mut BTreeSet<u64> {
        match owner {
            MindKind::Main => &mut self.suppress_main,
            MindKind::Sub => &mut self.suppress_sub,
            MindKind::Memory => &mut self.suppress_memory,
        }
    }

    pub(super) fn suppress_done_for_killed_job(&mut self, owner: MindKind, job_id: u64) -> bool {
        if job_id == 0 {
            return false;
        }
        let set = self.suppress_set_mut(owner);
        if set.insert(job_id) {
            //（1）从“本批总 started”里剔除：否则会出现 completed 永远达不到 total_started，
            //（2）导致剩余任务无法触发 DONE 汇总。
            let b = self.batch_mut(owner);
            b.total_started = b.total_started.saturating_sub(1);
            return true;
        }
        false
    }

    pub(super) fn take_suppressed_done(&mut self, owner: MindKind, job_id: u64) -> bool {
        self.suppress_set_mut(owner).remove(&job_id)
    }

    pub(super) fn reset(&mut self, owner: MindKind) {
        let b = self.batch_mut(owner);
        *b = PtyDoneBatch::default();
    }
}

#[derive(Debug, Clone)]
pub(super) struct PtyDoneFollowup {
    pub(super) owner: MindKind,
    pub(super) jobs: usize,
    pub(super) tool_text: String,
    pub(super) pushed: bool,
}

pub(super) fn format_pty_done_batch_tool_text_unlimited(
    owner: MindKind,
    jobs: &[PtyDoneJob],
    batch_saved: Option<&ExportedBatchMeta>,
) -> String {
    let mut out = String::new();
    let input_line = if jobs.len() == 1 {
        let j = &jobs[0];
        let cmd_preview = truncate_with_suffix(j.cmd.trim(), 140);
        format!(
            "job_id={} exit={} 耗时={}ms cmd={}",
            j.job_id, j.exit_code, j.elapsed_ms, cmd_preview
        )
    } else {
        let last_id = jobs.last().map(|j| j.job_id).unwrap_or(0);
        format!("jobs={} last_job_id={}", jobs.len(), last_id)
    };
    out.push_str("操作: PTY Done\n");
    out.push_str("explain: 终端任务结束\n");
    out.push_str(&format!("input: {input_line}\n"));
    out.push_str("output:\n```text\n");
    if let Some(meta) = batch_saved {
        let path = meta.path.trim();
        if !path.is_empty() {
            out.push_str(&format!("batch_saved:{path}\n"));
            out.push_str(&format!(
                "batch_saved_size:{} | bytes:{} | lines:{}\n\n",
                format_bytes_short(meta.bytes as usize),
                meta.bytes,
                meta.lines
            ));
        }
    }
    for (idx, j) in jobs.iter().enumerate() {
        let source = if j.user_initiated { " | 来源:用户" } else { "" };
        out.push_str(&format!("#{}\n", idx + 1));
        out.push_str(&format!(
            "job_id:{} | 状态:{} | exit:{} | 耗时:{}ms | size:{} | lines:{}{}\n",
            j.job_id,
            j.status,
            j.exit_code,
            j.elapsed_ms,
            format_bytes_short(j.bytes),
            j.lines,
            source
        ));
        let cmd_preview = truncate_with_suffix(j.cmd.trim(), 320);
        out.push_str("cmd: ");
        out.push_str(cmd_preview.trim_end());
        out.push('\n');
        out.push_str(&format!("log:{}\n", j.saved_path));
        out.push_str(&format!("status:{}\n", j.status_path));
        if !j.tail.trim().is_empty() {
            out.push_str("tail:\n");
            out.push_str(j.tail.trim_end());
            out.push('\n');
        }
        out.push('\n');
    }
    out.push_str("```\nmeta:\n```text\n");
    out.push_str(&format!("owner:{owner:?} | jobs:{}\n", jobs.len()));
    out.push_str("```\n");
    out.trim_end().to_string()
}

pub(super) fn format_pty_done_batch_tool_text(
    owner: MindKind,
    jobs: &[PtyDoneJob],
    batch_saved: Option<&ExportedBatchMeta>,
) -> String {
    let mut out = String::new();
    let input_line = if jobs.len() == 1 {
        let j = &jobs[0];
        let cmd_preview = truncate_with_suffix(j.cmd.trim(), 140);
        format!(
            "job_id={} exit={} 耗时={}ms cmd={}",
            j.job_id, j.exit_code, j.elapsed_ms, cmd_preview
        )
    } else {
        let last_id = jobs.last().map(|j| j.job_id).unwrap_or(0);
        format!("jobs={} last_job_id={}", jobs.len(), last_id)
    };
    out.push_str("操作: PTY Done\n");
    out.push_str("explain: 终端任务结束\n");
    out.push_str(&format!("input: {input_line}\n"));
    out.push_str("output:\n```text\n");
    if let Some(meta) = batch_saved {
        let path = meta.path.trim();
        if !path.is_empty() {
            out.push_str(&format!("batch_saved:{path}\n"));
            out.push_str(&format!(
                "batch_saved_size:{} | bytes:{} | lines:{}\n\n",
                format_bytes_short(meta.bytes as usize),
                meta.bytes,
                meta.lines
            ));
        }
    }
    let mut shown = 0usize;
    let mut truncated = false;
    for (idx, j) in jobs.iter().enumerate() {
        let source = if j.user_initiated { " | 来源:用户" } else { "" };
        out.push_str(&format!("#{}\n", idx + 1));
        out.push_str(&format!(
            "job_id:{} | 状态:{} | exit:{} | 耗时:{}ms | size:{} | lines:{}{}\n",
            j.job_id,
            j.status,
            j.exit_code,
            j.elapsed_ms,
            format_bytes_short(j.bytes),
            j.lines,
            source
        ));
        let cmd_preview = truncate_with_suffix(j.cmd.trim(), 320);
        out.push_str("cmd: ");
        out.push_str(cmd_preview.trim_end());
        out.push('\n');
        out.push_str(&format!("log:{}\n", j.saved_path));
        out.push_str(&format!("status:{}\n", j.status_path));
        if !j.tail.trim().is_empty() {
            out.push_str("tail:\n");
            out.push_str(j.tail.trim_end());
            out.push('\n');
        }
        out.push('\n');
        shown = shown.saturating_add(1);
        if out.chars().count() >= PTY_DONE_BATCH_TOOL_MAX_CHARS {
            truncated = true;
            break;
        }
    }
    out = truncate_with_suffix(out.trim_end(), PTY_DONE_BATCH_TOOL_MAX_CHARS);
    out.push_str("\n```\nmeta:\n```text\n");
    out.push_str(&format!(
        "owner:{owner:?} | jobs:{} | shown:{} | truncated:{}\n",
        jobs.len(),
        shown,
        truncated
    ));
    out.push_str("```\n");
    out
}

#[derive(Debug, Clone)]
pub(super) struct ExportedBatchMeta {
    pub(super) path: String,
    pub(super) bytes: u64,
    pub(super) lines: usize,
}

pub(super) fn export_pty_done_batch_summary(
    owner: MindKind,
    tool_text: &str,
) -> Option<ExportedBatchMeta> {
    if tool_text.trim().is_empty() {
        return None;
    }
    let max = PTY_DONE_BATCH_TOOL_MAX_CHARS.max(1);
    if tool_text.chars().count() <= max {
        return None;
    }
    let dir = std::path::Path::new("log/pty-cache");
    let _ = std::fs::create_dir_all(dir);
    let stamp = chrono::Local::now().format("%Y%m%d_%H%M%S").to_string();
    let id = next_pty_done_export_id();
    let file = format!("pty_done_batch_{owner:?}_{stamp}_{id}.txt");
    let path = dir.join(file);
    if std::fs::write(&path, tool_text).is_ok() {
        let bytes = std::fs::metadata(&path)
            .ok()
            .map(|m| m.len())
            .unwrap_or(tool_text.len() as u64);
        let lines = tool_text.lines().count();
        return Some(ExportedBatchMeta {
            path: path.to_string_lossy().to_string(),
            bytes,
            lines,
        });
    }
    None
}

fn format_bytes_short(bytes: usize) -> String {
    const UNITS: [&str; 6] = ["B", "KB", "MB", "GB", "TB", "PB"];
    let mut value = bytes as f64;
    let mut idx = 0usize;
    while value >= 1024.0 && idx + 1 < UNITS.len() {
        value /= 1024.0;
        idx += 1;
    }
    if idx == 0 {
        format!("{bytes} B")
    } else {
        format!("{value:.1} {}", UNITS[idx])
    }
}

pub(super) struct PtyStatusFileArgs<'a> {
    pub(super) path: &'a str,
    pub(super) phase: PtyStatusPhase,
    pub(super) owner: MindKind,
    pub(super) job_id: u64,
    pub(super) cmd: &'a str,
    pub(super) saved_path: &'a str,
    pub(super) bytes: usize,
    pub(super) lines: usize,
    pub(super) exit_code: Option<i32>,
}

pub(super) fn write_pty_status_file(args: PtyStatusFileArgs<'_>) {
    let PtyStatusFileArgs {
        path,
        phase,
        owner,
        job_id,
        cmd,
        saved_path,
        bytes,
        lines,
        exit_code,
    } = args;

    let phase_label = match phase {
        PtyStatusPhase::Starting => "starting",
        PtyStatusPhase::Running => "running",
        PtyStatusPhase::Done => "done",
        PtyStatusPhase::Timeout => "timeout",
        PtyStatusPhase::UserExit => "user_exit",
    };
    let now = chrono::Local::now().format("%Y-%m-%d %H:%M:%S");
    let exit = exit_code
        .map(|v| v.to_string())
        .unwrap_or_else(|| "(none)".to_string());
    let body = format!(
        "phase:{phase_label}\nowner:{owner:?}\njob_id:{job_id}\nupdated_at:{now}\nsaved:{saved_path}\nbytes:{bytes}\nlines:{lines}\nexit:{exit}\ncmd:{cmd}\n"
    );
    let _ = fs::write(path, body.as_bytes());
}

pub(super) fn defer_pty_audit(next_at: &mut Instant, now: Instant) {
    let defer_until = now + Duration::from_secs(PTY_AUDIT_DEFER_SECS);
    if defer_until > *next_at {
        *next_at = defer_until;
    }
}

const DEFAULT_PTY_HELP_PROMPT: &str = "Terminal（PTY）已启动：\n- Home：打开/隐藏 Terminal 视图（后台继续运行）\n- Esc：发送给终端内程序（例如退出 vim/less）\n- （兼容）{DOUBLE_ESC_MS}ms 内快速连按两次 Esc：结束当前终端任务（终止 PTY）\n- PgUp/PgDn：切换不同终端任务（多 PTY tab）\n- Alt+↑：复制当前终端快照到聊天输入框（由用户决定是否发送给 AI）\n";

pub(super) fn load_pty_help_prompt(sys_cfg: &SystemConfig) -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path(&sys_cfg.pty_help_prompt_path, true);
    let (text, err) = load_prompt_file_with_default(&path, DEFAULT_PTY_HELP_PROMPT, "PTY-HELP");
    (text, err, path)
}

pub(super) fn render_pty_help_prompt(template: &str, double_esc_ms: u64) -> String {
    let t = template.trim();
    let tpl = if t.is_empty() {
        DEFAULT_PTY_HELP_PROMPT
    } else {
        t
    };
    tpl.replace("{DOUBLE_ESC_MS}", &double_esc_ms.to_string())
}

//（1）PTY 自动超时：避免后台会话永久占用资源。
const PTY_AUTO_TIMEOUT_SECS: u64 = 30 * 60;

const DEFAULT_PTY_STARTED_NOTICE_PROMPT: &str = "PTY 已启动（后台运行）。\njob_id:{JOB_ID} | owner:{OWNER}\ncwd:{CWD}\ncmd:\n```sh\n{CMD}\n```\nlog:{LOG_PATH}\nstatus:{STATUS_PATH}\n\n说明：终端在后台运行，期间可继续对话。\n如需提前结束：pty.kill(job_id)。\n自动超时：30 分钟（到点自动结束）。\n\n{INITIAL_PREVIEW_BLOCK}\n";

pub(super) fn load_pty_started_notice_prompt() -> String {
    DEFAULT_PTY_STARTED_NOTICE_PROMPT.to_string()
}

pub(super) struct PtyStartedNotice<'a> {
    pub(super) owner: MindKind,
    pub(super) job_id: u64,
    pub(super) cmd: &'a str,
    pub(super) saved_path: &'a str,
    pub(super) status_path: &'a str,
    pub(super) cwd_display: &'a str,
    pub(super) initial_preview: &'a str,
}

fn render_pty_started_notice_prompt(args: PtyStartedNotice<'_>, template: &str) -> String {
    let t = template.trim();
    let tpl = if t.is_empty() {
        DEFAULT_PTY_STARTED_NOTICE_PROMPT
    } else {
        t
    };
    let owner_label = if matches!(args.owner, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    let cmd = args.cmd.trim();
    let cmd = if cmd.is_empty() { "(empty)" } else { cmd };
    let initial_preview_block = if args.initial_preview.trim().is_empty() {
        "".to_string()
    } else {
        format!(
            "[初步输出]\n```text\n{}\n```",
            args.initial_preview.trim_end()
        )
    };
    tpl.replace("{OWNER}", owner_label)
        .replace("{JOB_ID}", &args.job_id.to_string())
        .replace("{CWD}", args.cwd_display)
        .replace("{CMD}", &truncate_with_suffix(cmd, 2000))
        .replace("{LOG_PATH}", args.saved_path)
        .replace("{STATUS_PATH}", args.status_path)
        .replace("{INITIAL_PREVIEW_BLOCK}", &initial_preview_block)
        .replace("{DOUBLE_ESC_MS}", &PTY_DOUBLE_ESC_MS.to_string())
        .trim_end()
        .to_string()
}

pub(super) fn build_pty_started_notice(args: PtyStartedNotice<'_>, template: &str) -> String {
    render_pty_started_notice_prompt(args, template)
}

pub(super) fn render_pty_audit_prompt(
    template: &str,
    owner: MindKind,
    running_total: usize,
    running_owner: usize,
) -> String {
    let t = template.trim();
    if t.is_empty() {
        return String::new();
    }
    let tpl = t;
    let owner_label = if matches!(owner, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    tpl.replace("{OWNER}", owner_label)
        .replace("{RUNNING_TOTAL}", &running_total.to_string())
        .replace("{RUNNING_OWNER}", &running_owner.to_string())
}

#[derive(Debug, Clone)]
pub(super) struct PendingPtySnapshot {
    pub(super) job_id: u64,
    pub(super) placeholder: String,
    pub(super) content: String,
}

pub(super) fn prune_pending_pty_snapshot(input: &str, pending: &mut Option<PendingPtySnapshot>) {
    if let Some(p) = pending.as_ref()
        && !input.contains(p.placeholder.as_str())
    {
        *pending = None;
    }
}

pub(super) fn snap_cursor_out_of_pty_snapshot_placeholder(
    input: &str,
    pending: &Option<PendingPtySnapshot>,
    cursor: usize,
) -> usize {
    let cur = cursor.min(input.len());
    let Some(p) = pending.as_ref() else {
        return cur;
    };
    let ph = p.placeholder.as_str();
    if ph.trim().is_empty() || input.is_empty() {
        return cur;
    }
    let mut search = 0usize;
    while let Some(pos) = input.get(search..).and_then(|s| s.find(ph)) {
        let start = search + pos;
        let end = start + ph.len();
        if cur > start && cur < end {
            return end;
        }
        search = end;
        if search >= input.len() {
            break;
        }
    }
    cur
}

pub(super) fn try_remove_pty_snapshot_placeholder_at_cursor(
    input: &mut String,
    cursor: &mut usize,
    input_chars: &mut usize,
    pending: &mut Option<PendingPtySnapshot>,
    how: PlaceholderRemove,
) -> bool {
    let Some(p) = pending.as_ref() else {
        return false;
    };
    let ph = p.placeholder.as_str();
    if ph.trim().is_empty() {
        return false;
    }
    if !input.contains(ph) {
        *pending = None;
        return false;
    }

    let cur = (*cursor).min(input.len());
    let ph_len = ph.len();
    let ph_chars = count_chars(ph);

    let mut remove_at = None;
    let mut search = 0usize;
    while let Some(pos) = input.get(search..).and_then(|s| s.find(ph)) {
        let start = search + pos;
        let end = start + ph_len;
        if cur > start && cur < end {
            remove_at = Some((start, end));
            break;
        }
        search = end;
        if search >= input.len() {
            break;
        }
    }

    if remove_at.is_none() {
        let before = cur.checked_sub(ph_len).map(|start| (start, cur));
        let after = if cur + ph_len <= input.len() {
            Some((cur, cur + ph_len))
        } else {
            None
        };
        let candidates: [Option<(usize, usize)>; 2] = match how {
            PlaceholderRemove::Backspace => [before, after],
            PlaceholderRemove::Delete => [after, before],
        };
        for cand in candidates.into_iter().flatten() {
            let (start, end) = cand;
            if input.get(start..end) == Some(ph) {
                remove_at = Some((start, end));
                break;
            }
        }
    }

    let Some((start, end)) = remove_at else {
        return false;
    };
    input.drain(start..end);
    *cursor = start;
    *input_chars = input_chars.saturating_sub(ph_chars);
    *pending = None;
    true
}
pub(super) fn spawn_interactive_bash_execution(
    call: ToolCall,
    owner: MindKind,
    tx: mpsc::Sender<AsyncEvent>,
    pty_started_notice_prompt_text: String,
    pty_started_model_note_template: String,
    send_started_receipt: bool,
    user_initiated: bool,
) -> PtySpawnInfo {
    //（1）cwd 展示：
    //（2）在 $HOME 下：显示 `~` 或 `~/<rel>`
    //（3）不在 $HOME 下：显示真实绝对路径
    fn short_cwd_display(raw: &str) -> String {
        let path = raw.trim();
        if path.is_empty() {
            return String::new();
        }
        let p = std::fs::canonicalize(path)
            .ok()
            .and_then(|x| x.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| path.to_string());

        let home = std::env::var("HOME").unwrap_or_default();
        let home = home.trim_end_matches('/').to_string();
        if !home.is_empty() {
            let home_prefix = format!("{home}/");
            if p == home {
                return "~".to_string();
            }
            if p.starts_with(&home_prefix) {
                let rest = p[home_prefix.len()..].trim_start_matches('/');
                if rest.is_empty() {
                    return "~".to_string();
                }
                return format!("~/{rest}");
            }
        }
        p
    }

    let job_id = next_pty_job_id();
    let (saved_path, status_path) = {
        let _ = fs::create_dir_all("log/bash-cache");
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let saved_path = format!("log/bash-cache/pty_{job_id}_{owner:?}_{ts}_{pid}.log");
        let status_path = saved_path.trim_end_matches(".log").to_string() + ".status";
        (saved_path, status_path)
    };
    let spawn_info = PtySpawnInfo {
        job_id,
        saved_path: saved_path.clone(),
        status_path: status_path.clone(),
    };
    thread::spawn(move || {
        let cwd_now = std::env::current_dir()
            .ok()
            .and_then(|p| p.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| "(unknown)".to_string());
        let _cwd_now_display = short_cwd_display(&cwd_now);
        fn expand_home_alias(raw: &str) -> String {
            let s = raw.trim();
            if s.is_empty() {
                return String::new();
            }
            let home = std::env::var("HOME").unwrap_or_default();
            if home.trim().is_empty() {
                return s.to_string();
            }
            let home = home.trim_end_matches('/').to_string();
            let project = format!("{home}/AItermux");
            if s == "." {
                return project;
            }
            if let Some(rest) = s.strip_prefix("./") {
                if rest.is_empty() {
                    return project;
                }
                return format!("{project}/{rest}");
            }
            if s == "~" {
                return project;
            }
            if let Some(rest) = s.strip_prefix("~/") {
                return format!("{project}/{rest}");
            }
            //（1）工程根别名：home -> ~/AItermux
            if s == "home" {
                return project;
            }
            if let Some(rest) = s.strip_prefix("home/") {
                return format!("{project}/{rest}");
            }
            s.to_string()
        }

        let (cwd_exec, cwd_display) = match call
            .cwd
            .as_deref()
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
        {
            //（1）与 bash/adb/termux_api 对齐：默认 cwd 指向工程目录（~/AItermux/system）
            //（2），避免从其它目录启动时相对路径失效。
            None => {
                let abs = crate::mcp::default_workdir();
                let disp = short_cwd_display(&abs);
                (Some(abs), disp)
            }
            Some(raw) => {
                let raw = expand_home_alias(raw);
                let p = Path::new(raw.as_str());
                let abs = if p.is_absolute() {
                    raw.to_string()
                } else if let Ok(base) = std::env::current_dir() {
                    base.join(p).to_string_lossy().to_string()
                } else {
                    raw.to_string()
                };
                let disp = short_cwd_display(&abs);
                (Some(abs.clone()), disp)
            }
        };
        let cmd = call.input.trim().to_string();
        if cmd.is_empty() {
            let outcome = ToolOutcome {
                user_message: "No bash input.".to_string(),
                log_lines: vec![format!("状态:无 | 耗时:0ms | cwd:{cwd_display}")],
            };
            let _ = tx.send(AsyncEvent::ToolStreamEnd {
                outcome,
                sys_msg: Some("工具执行失败".to_string()),
            });
            return;
        }

        let started = Instant::now();
        let started_at = chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
        let timeout_secs = call
            .timeout_secs
            .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
            .unwrap_or(PTY_AUTO_TIMEOUT_SECS);
        write_pty_status_file(PtyStatusFileArgs {
            path: &status_path,
            phase: PtyStatusPhase::Starting,
            owner,
            job_id,
            cmd: &cmd,
            saved_path: &saved_path,
            bytes: 0,
            lines: 0,
            exit_code: None,
        });

        let (ctrl_tx, ctrl_rx) = mpsc::channel::<PtyControl>();
        //（1）初始尺寸：尽量贴近当前 TUI 的 PTY 面板内层（inner rect），避免首屏解析/渲染错位。
        //（2）后续 UI 每帧会用 ensure_size 精确同步，但首屏错位会在“键盘弹出/收回”时更明显。
        let (term_cols, term_rows) = crossterm::terminal::size().unwrap_or((80, 24));
        let init_cols = term_cols.saturating_sub(2).max(20); // 面板左右边框
        let fixed = 6u16; // header + header-sep + spacer + status + sep + ctx（与 core.rs 一致）
        let avail = term_rows.saturating_sub(fixed).max(2);
        let max_input_h = avail.saturating_sub(1).max(1); // chat 至少 1 行
        let pty_h = (avail / 2).max(6).min(max_input_h);
        let init_rows = pty_h.saturating_sub(2).max(4); // 面板上下边框；至少 4 行避免极小尺寸
	        let _ = tx.send(AsyncEvent::PtyReady {
	            ctrl_tx: ctrl_tx.clone(),
	            cols: init_cols,
	            rows: init_rows,
	            owner,
	            user_initiated,
	            job_id,
	            cmd: cmd.clone(),
	            saved_path: saved_path.clone(),
	            status_path: status_path.clone(),
	        });

        let mut file = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&saved_path)
            .ok();

        let emit_fail_done = |outcome: ToolOutcome, exit_code: i32| {
            if send_started_receipt {
                let _ = tx.send(AsyncEvent::ToolStreamEnd {
                    outcome: outcome.clone(),
                    sys_msg: Some("工具执行失败".to_string()),
                });
            }
            let elapsed_ms = started.elapsed().as_millis();
            write_pty_status_file(PtyStatusFileArgs {
                path: &status_path,
                phase: PtyStatusPhase::Done,
                owner,
                job_id,
                cmd: &cmd,
                saved_path: &saved_path,
                bytes: 0,
                lines: 0,
                exit_code: Some(exit_code),
            });
	            let _ = tx.send(AsyncEvent::PtyJobDone {
	                owner,
	                user_initiated,
	                job_id,
	                cmd: cmd.clone(),
	                saved_path: saved_path.clone(),
	                status_path: status_path.clone(),
	                exit_code,
	                timed_out: false,
	                user_exit: false,
	                elapsed_ms,
	                bytes: 0,
	                lines: 0,
	            });
	        };

        let pty_system = native_pty_system();
        let pair = match pty_system.openpty(PtySize {
            rows: init_rows,
            cols: init_cols,
            pixel_width: 0,
            pixel_height: 0,
        }) {
            Ok(p) => p,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY open failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_open_failed | cwd:{cwd_display}")],
                };
                emit_fail_done(outcome, -1);
                return;
            }
        };

        //（1）默认关闭 echoctl，避免用户在终端里误触 Esc/控制键时输出出现 `^[` 等“噪音”。
        //（2）这不影响程序收到的按键，只影响控制字符的回显方式。
        let mut cmd_exec = "stty -echoctl 2>/dev/null || true\n".to_string();
        if let Some(cwd_exec) = cwd_exec.as_deref() {
            cmd_exec.push_str(&format!(
                "cd -- {} || exit 1\n",
                bash_single_quote(cwd_exec)
            ));
        }
        cmd_exec.push_str(cmd.as_str());
        if !cmd_exec.ends_with('\n') {
            cmd_exec.push('\n');
        }
        let mut cmd_builder = CommandBuilder::new("/data/data/com.termux/files/usr/bin/bash");
        cmd_builder.args(["-lc", &cmd_exec]);
        cmd_builder.env("TERM", "xterm-256color");
        cmd_builder.env("COLORTERM", "truecolor");
        cmd_builder.env("LANG", "C.UTF-8");
        cmd_builder.env("LC_ALL", "C.UTF-8");
        cmd_builder.env("TERM_PROGRAM", "AItermux");
        let init_cols_env = init_cols.to_string();
        let init_rows_env = init_rows.to_string();
        cmd_builder.env("COLUMNS", init_cols_env.as_str());
        cmd_builder.env("LINES", init_rows_env.as_str());
        let mut child = match pair.slave.spawn_command(cmd_builder) {
            Ok(c) => c,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY spawn failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_spawn_failed | cwd:{cwd_display}")],
                };
                emit_fail_done(outcome, -1);
                return;
            }
        };
        let _pty_guard = ActivePtyGuard::new();
        let pid = child.process_id().map(|v| v as i32);

        //（1）master：拆出 reader/writer，并保留 master 以处理 resize。
        let master = pair.master;
        let pgrp = master.process_group_leader();
        let _ = tx.send(AsyncEvent::PtySpawned { job_id, pid, pgrp });

        //（1）为了让 “Kill / 超时” 能可靠打断 reader.read()，
        //（2）把 master fd 设为 nonblocking。
        //（3）portable-pty 的 try_clone_reader() 通常基于 dup；
        //（4）O_NONBLOCK 属于 open-file-description，
        //（5）对 dup 后的 reader 同样生效。
        #[cfg(unix)]
        if let Some(fd) = master.as_raw_fd() {
            set_fd_nonblocking(fd, true);
        }

        let mut reader = match master.try_clone_reader() {
            Ok(r) => r,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY reader failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_reader_failed | cwd:{cwd_display}")],
                };
                #[cfg(unix)]
                if let Some(pgrp) = master.process_group_leader() {
                    unsafe { libc::kill(-pgrp, libc::SIGKILL) };
                }
                let _ = child.kill();
                emit_fail_done(outcome, -1);
                return;
            }
        };
        let mut writer = match master.take_writer() {
            Ok(w) => w,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY writer failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_writer_failed | cwd:{cwd_display}")],
                };
                #[cfg(unix)]
                if let Some(pgrp) = master.process_group_leader() {
                    unsafe { libc::kill(-pgrp, libc::SIGKILL) };
                }
                let _ = child.kill();
                emit_fail_done(outcome, -1);
                return;
            }
        };

        //（1）子进程与控制线程共享：支持 Kill / Wait。
        let child: Arc<Mutex<Option<Box<dyn portable_pty::Child + Send + Sync>>>> =
            Arc::new(Mutex::new(Some(child)));
        let done = Arc::new(AtomicBool::new(false));
        let done2 = done.clone();
        let child2 = child.clone();
        let timed_out = Arc::new(AtomicBool::new(false));
        let user_aborted = Arc::new(AtomicBool::new(false));
        let user_aborted2 = user_aborted.clone();
        //（1）注意：portable_pty::Child::try_wait() 可能会“回收”子进程；若后续再 wait()
        //（2）会得到 ECHILD。
        //（3）因此一旦任何线程通过 try_wait() 观察到退出，就把 exit_code 记录下来，
        //（4）finalize 阶段优先使用它。
        let exit_code_seen: Arc<Mutex<Option<i32>>> = Arc::new(Mutex::new(None));

        //（1）ctrl 线程：stdin / resize / kill
        let ctrl_handle = thread::spawn(move || {
            'ctrl: while !done2.load(Ordering::Relaxed) {
                let msg = match ctrl_rx.recv_timeout(Duration::from_millis(120)) {
                    Ok(m) => m,
                    Err(mpsc::RecvTimeoutError::Timeout) => continue,
                    Err(_) => break,
                };
                match msg {
                    PtyControl::Input(bytes) => {
                        let _ = writer.write_all(&bytes);
                        let _ = writer.flush();
                    }
                    PtyControl::Resize { cols, rows } => {
                        let _ = master.resize(PtySize {
                            rows,
                            cols,
                            pixel_width: 0,
                            pixel_height: 0,
                        });
                    }
                    PtyControl::Kill => {
                        user_aborted2.store(true, Ordering::Relaxed);
                        if let Ok(mut guard) = child2.lock()
                            && let Some(ch) = guard.as_mut()
                        {
                            //（1）尽力“杀整棵树”：避免只杀 bash 父进程导致子进程（如 codex/python）残留，
                            //（2）从而读端不 EOF、界面看起来一直“还在跑”。
                            #[cfg(unix)]
                            if let Some(pgrp) = master.process_group_leader() {
                                //（1）kill process group
                                unsafe { libc::kill(-pgrp, libc::SIGKILL) };
                            }
                            if let Some(pid) = ch.process_id().map(|v| v as i32) {
                                kill_process_tree(pid, libc::SIGKILL, 0);
                            }
                            let _ = ch.kill();
                        }
                        //（1）重要：立即退出 ctrl 线程，drop writer/master，给 slave 侧制造 EOF/HUP，
                        //（2）以提升“退出 PTY 后工具能快速收尾”的确定性。
                        break 'ctrl;
                    }
                }
            }
        });

        //（1）PTY 自动超时：30min 到点 kill（仍允许用户双击 Esc 提前结束）。
        if timeout_secs > 0 {
            let tx_kill = ctrl_tx.clone();
            let timed_out2 = timed_out.clone();
            let done_timeout = done.clone();
            let child_timeout = child.clone();
            let exit_code_timeout = exit_code_seen.clone();
            thread::spawn(move || {
                thread::sleep(Duration::from_secs(timeout_secs));
                if done_timeout.load(Ordering::Relaxed) {
                    return;
                }
                let exited = if let Ok(mut guard) = child_timeout.lock() {
                    if let Some(ch) = guard.as_mut() {
                        match ch.try_wait() {
                            Ok(Some(status)) => {
                                if let Ok(mut g) = exit_code_timeout.lock() {
                                    *g = Some(status.exit_code() as i32);
                                }
                                true
                            }
                            Ok(None) => false,
                            Err(_) => false,
                        }
                    } else {
                        true
                    }
                } else {
                    false
                };
                if exited {
                    return;
                }
                timed_out2.store(true, Ordering::Relaxed);
                let _ = tx_kill.send(PtyControl::Kill);
            });
        }

        //（1）reader：持续刷出增量（同时落盘到 bash-cache）
        let mut buf = [0u8; 8192];
        let mut bytes_written: usize = 0;
        let mut lines_written: usize = 0;
        let mut last_status_flush_at = Instant::now();
        let mut last_try_wait_at = Instant::now();
        let mut initial_preview: Vec<u8> = Vec::new();
        let initial_preview_deadline = Instant::now() + Duration::from_millis(260);
        let mut started_notice_sent = !send_started_receipt;
        let mut kill_grace_started_at: Option<Instant> = None;
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    let chunk = buf[..n].to_vec();
                    bytes_written = bytes_written.saturating_add(n);
                    lines_written =
                        lines_written.saturating_add(chunk.iter().filter(|&&b| b == b'\n').count());
                    if Instant::now() <= initial_preview_deadline
                        && initial_preview.len() < 8192
                        && !chunk.is_empty()
                    {
                        let cap = 8192usize.saturating_sub(initial_preview.len());
                        initial_preview.extend_from_slice(&chunk[..chunk.len().min(cap)]);
                    }
                    if let Some(f) = file.as_mut() {
                        let _ = f.write_all(&chunk);
                        let _ = f.flush();
                    }
                    let _ = tx.send(AsyncEvent::PtyOutput {
                        job_id,
                        bytes: chunk,
                    });
                    if Instant::now().saturating_duration_since(last_status_flush_at)
                        >= Duration::from_secs(2)
                    {
                        last_status_flush_at = Instant::now();
                        write_pty_status_file(PtyStatusFileArgs {
                            path: &status_path,
                            phase: PtyStatusPhase::Running,
                            owner,
                            job_id,
                            cmd: &cmd,
                            saved_path: &saved_path,
                            bytes: bytes_written,
                            lines: lines_written,
                            exit_code: None,
                        });
                    }
                    if send_started_receipt
                        && !started_notice_sent
                        && Instant::now() >= initial_preview_deadline
                    {
                        started_notice_sent = true;
                        let initial_preview_text = {
                            let mut s = String::from_utf8_lossy(&initial_preview).to_string();
                            s.retain(|ch| ch != '\u{0}');
                            truncate_with_suffix(s.trim(), 2200)
                        };
	                        let started_notice = build_pty_started_notice(
	                            PtyStartedNotice {
	                                owner,
	                                job_id,
	                                cmd: &cmd,
	                                saved_path: &saved_path,
	                                status_path: &status_path,
	                                cwd_display: &cwd_display,
	                                initial_preview: &initial_preview_text,
	                            },
                            &pty_started_notice_prompt_text,
                        );
                        let started_outcome = ToolOutcome {
                            user_message: started_notice,
                            log_lines: {
                                let mut lines = vec![
                                    format!(
                                        "状态:running | phase:pty_started | job_id:{job_id} | cwd:{cwd_display} | saved:{saved_path} | status:{status_path}"
                                    ),
                                    format!("cmd_len:{}", cmd.chars().count()),
                                ];
                                let tpl = pty_started_model_note_template.trim();
                                if !tpl.is_empty() {
                                    let note = crate::messages::render_template(
                                        tpl,
                                        &[
                                            ("JOB_ID", &job_id.to_string()),
                                            ("OWNER", &format!("{owner:?}")),
                                            ("PTY_COUNT", &active_pty_count().to_string()),
                                            ("STARTED_AT", &started_at),
                                            ("CWD", &cwd_display),
                                            ("CMD", &cmd),
                                            ("LOG_PATH", &saved_path),
                                            ("STATUS_PATH", &status_path),
                                        ],
                                    );
                                    let note = note.trim();
                                    if !note.is_empty() {
                                        lines.push(format!("{MODEL_NOTE_PREFIX}{note}"));
                                    }
                                }
                                lines
                            },
                        };
                        let _ = tx.send(AsyncEvent::ToolStreamEnd {
                            outcome: started_outcome,
                            sys_msg: Some("PTY 已启动（后台运行）".to_string()),
                        });
                    }
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    if send_started_receipt
                        && !started_notice_sent
                        && Instant::now() >= initial_preview_deadline
                    {
                        started_notice_sent = true;
                        let initial_preview_text = {
                            let mut s = String::from_utf8_lossy(&initial_preview).to_string();
                            s.retain(|ch| ch != '\u{0}');
                            truncate_with_suffix(s.trim(), 2200)
                        };
	                        let started_notice = build_pty_started_notice(
	                            PtyStartedNotice {
	                                owner,
	                                job_id,
	                                cmd: &cmd,
	                                saved_path: &saved_path,
	                                status_path: &status_path,
	                                cwd_display: &cwd_display,
	                                initial_preview: &initial_preview_text,
	                            },
                            &pty_started_notice_prompt_text,
                        );
                        let started_outcome = ToolOutcome {
                            user_message: started_notice,
                            log_lines: {
                                let mut lines = vec![
                                    format!(
                                        "状态:running | phase:pty_started | job_id:{job_id} | cwd:{cwd_display} | saved:{saved_path} | status:{status_path}"
                                    ),
                                    format!("cmd_len:{}", cmd.chars().count()),
                                ];
                                let tpl = pty_started_model_note_template.trim();
                                if !tpl.is_empty() {
                                    let note = crate::messages::render_template(
                                        tpl,
                                        &[
                                            ("JOB_ID", &job_id.to_string()),
                                            ("OWNER", &format!("{owner:?}")),
                                            ("PTY_COUNT", &active_pty_count().to_string()),
                                            ("STARTED_AT", &started_at),
                                            ("CWD", &cwd_display),
                                            ("CMD", &cmd),
                                            ("LOG_PATH", &saved_path),
                                            ("STATUS_PATH", &status_path),
                                        ],
                                    );
                                    let note = note.trim();
                                    if !note.is_empty() {
                                        lines.push(format!("{MODEL_NOTE_PREFIX}{note}"));
                                    }
                                }
                                lines
                            },
                        };
                        let _ = tx.send(AsyncEvent::ToolStreamEnd {
                            outcome: started_outcome,
                            sys_msg: Some("PTY 已启动（后台运行）".to_string()),
                        });
                    }
                    if Instant::now().saturating_duration_since(last_status_flush_at)
                        >= Duration::from_secs(2)
                    {
                        last_status_flush_at = Instant::now();
                        write_pty_status_file(PtyStatusFileArgs {
                            path: &status_path,
                            phase: PtyStatusPhase::Running,
                            owner,
                            job_id,
                            cmd: &cmd,
                            saved_path: &saved_path,
                            bytes: bytes_written,
                            lines: lines_written,
                            exit_code: None,
                        });
                    }
                    //（1）重要：某些平台/实现下，子进程退出后 reader.read() 可能长期返回 WouldBlock，
                    //（2）导致 PTY “看起来还在跑”。这里定期 try_wait()，一旦观察到退出就收敛退出。
                    if Instant::now().saturating_duration_since(last_try_wait_at)
                        >= Duration::from_millis(240)
                    {
                        last_try_wait_at = Instant::now();
                        let already_seen = exit_code_seen.lock().ok().and_then(|g| *g).is_some();
                        if already_seen {
                            break;
                        }
                        if let Ok(mut guard) = child.lock()
                            && let Some(ch) = guard.as_mut()
                            && let Ok(Some(status)) = ch.try_wait()
                        {
                            if let Ok(mut g) = exit_code_seen.lock() {
                                *g = Some(status.exit_code() as i32);
                            }
                            break;
                        }
                    }
                    //（1）nonblocking：没有数据时让出 CPU，并在 kill/timeout 后做 bounded 收尾。
                    if user_aborted.load(Ordering::Relaxed) || timed_out.load(Ordering::Relaxed) {
                        if kill_grace_started_at.is_none() {
                            kill_grace_started_at = Some(Instant::now());
                        }
                        //（1）若子进程已退出（try_wait），或 kill grace 超时，则退出 reader loop。
                        let exited = if let Ok(mut guard) = child.lock() {
                            if let Some(ch) = guard.as_mut() {
                                ch.try_wait().ok().flatten().is_some()
                            } else {
                                true
                            }
                        } else {
                            false
                        };
                        if exited {
                            break;
                        }
                        if let Some(t0) = kill_grace_started_at
                            && Instant::now().saturating_duration_since(t0)
                                >= Duration::from_millis(1200)
                        {
                            break;
                        }
                    }
                    thread::sleep(Duration::from_millis(16));
                }
                Err(_) => break,
            }
        }
        if send_started_receipt && !started_notice_sent {
            let initial_preview_text = {
                let mut s = String::from_utf8_lossy(&initial_preview).to_string();
                s.retain(|ch| ch != '\u{0}');
                truncate_with_suffix(s.trim(), 2200)
            };
	            let started_notice = build_pty_started_notice(
	                PtyStartedNotice {
	                    owner,
	                    job_id,
	                    cmd: &cmd,
	                    saved_path: &saved_path,
	                    status_path: &status_path,
	                    cwd_display: &cwd_display,
	                    initial_preview: &initial_preview_text,
	                },
                &pty_started_notice_prompt_text,
            );
            let started_outcome = ToolOutcome {
                user_message: started_notice,
                log_lines: {
                    let mut lines = vec![
                        format!(
                            "状态:running | phase:pty_started | job_id:{job_id} | cwd:{cwd_display} | saved:{saved_path} | status:{status_path}"
                        ),
                        format!("cmd_len:{}", cmd.chars().count()),
                    ];
                    let tpl = pty_started_model_note_template.trim();
                    if !tpl.is_empty() {
                        let note = crate::messages::render_template(
                            tpl,
                            &[
                                ("JOB_ID", &job_id.to_string()),
                                ("OWNER", &format!("{owner:?}")),
                                ("PTY_COUNT", &active_pty_count().to_string()),
                                ("STARTED_AT", &started_at),
                                ("CWD", &cwd_display),
                                ("CMD", &cmd),
                                ("LOG_PATH", &saved_path),
                                ("STATUS_PATH", &status_path),
                            ],
                        );
                        let note = note.trim();
                        if !note.is_empty() {
                            lines.push(format!("{MODEL_NOTE_PREFIX}{note}"));
                        }
                    }
                    lines
                },
            };
            let _ = tx.send(AsyncEvent::ToolStreamEnd {
                outcome: started_outcome,
                sys_msg: Some("PTY 已启动（后台运行）".to_string()),
            });
        }

        //（1）wait + 收尾
        let code_from_try_wait = exit_code_seen.lock().ok().and_then(|g| *g);
        let code = if let Some(code) = code_from_try_wait {
            if let Ok(mut guard) = child.lock() {
                let _ = guard.take();
            }
            code
        } else if let Ok(mut guard) = child.lock() {
            if let Some(mut ch) = guard.take() {
                //（1）若已触发 kill/timeout：bounded wait，避免卡死在 .wait()
                if user_aborted.load(Ordering::Relaxed) || timed_out.load(Ordering::Relaxed) {
                    let started_wait = Instant::now();
                    let mut status = None;
                    while Instant::now().saturating_duration_since(started_wait)
                        < Duration::from_millis(1800)
                    {
                        if let Ok(Some(s)) = ch.try_wait() {
                            status = Some(s);
                            break;
                        }
                        thread::sleep(Duration::from_millis(30));
                    }
                    status.map(|s| s.exit_code() as i32).unwrap_or(-1)
                } else {
                    let status = ch.wait().ok();
                    status.map(|s| s.exit_code() as i32).unwrap_or(-1)
                }
            } else {
                -1
            }
        } else {
            -1
        };
        done.store(true, Ordering::Relaxed);
        let _ = ctrl_handle.join();

        let elapsed_ms = started.elapsed().as_millis();
        let user_exit = user_aborted.load(Ordering::Relaxed);
        write_pty_status_file(PtyStatusFileArgs {
            path: &status_path,
            phase: if timed_out.load(Ordering::Relaxed) {
                PtyStatusPhase::Timeout
            } else if user_exit {
                PtyStatusPhase::UserExit
            } else {
                PtyStatusPhase::Done
            },
            owner,
            job_id,
            cmd: &cmd,
            saved_path: &saved_path,
            bytes: bytes_written,
            lines: lines_written,
            exit_code: Some(code),
        });
	    	let _ = tx.send(AsyncEvent::PtyJobDone {
	    	    owner,
	    	    user_initiated,
	    	    job_id,
	    	    cmd,
	    	    saved_path,
	    	    status_path,
	    	    exit_code: code,
	    	    timed_out: timed_out.load(Ordering::Relaxed),
	    	    user_exit,
	    	    elapsed_ms,
	    	    bytes: bytes_written,
	    	    lines: lines_written,
	    	});
    });
    spawn_info
}

pub(super) struct HandleAsyncEventPtyReadyArgs<'a> {
    pub(super) core: &'a mut Core,
    pub(super) render_cache: &'a mut ui::ChatRenderCache,
    pub(super) pty_tabs: &'a mut Vec<PtyUiState>,
    pub(super) pty_active_idx: &'a mut usize,
    pub(super) pty_handles: &'a mut HashMap<u64, mpsc::Sender<PtyControl>>,
    pub(super) pty_view: &'a mut bool,
    pub(super) pty_done_batches: &'a mut PtyDoneBatches,
    pub(super) pty_help_prompt_text: &'a str,
    pub(super) ctrl_tx: mpsc::Sender<PtyControl>,
    pub(super) cols: u16,
    pub(super) rows: u16,
    pub(super) owner: MindKind,
    pub(super) user_initiated: bool,
    pub(super) job_id: u64,
    pub(super) cmd: String,
    pub(super) saved_path: String,
    pub(super) status_path: String,
}

pub(super) fn handle_async_event_pty_ready(args: HandleAsyncEventPtyReadyArgs<'_>) {
    let HandleAsyncEventPtyReadyArgs {
        core,
        render_cache,
        pty_tabs,
        pty_active_idx,
        pty_handles,
        pty_view,
        pty_done_batches,
        pty_help_prompt_text,
        ctrl_tx,
        cols,
        rows,
        owner,
        user_initiated,
        job_id,
        cmd,
        saved_path,
        status_path,
    } = args;
    runlog_event(
        "INFO",
        "pty.ready",
        json!({
            "owner": mind_label(owner),
            "job_id": job_id,
            "cmd": crate::truncate_with_suffix(cmd.as_str(), 300),
            "log": saved_path,
            "status": status_path,
        }),
    );
    let cols = cols.max(1);
    let rows = rows.max(1);
    let was_empty = pty_tabs.is_empty();
    pty_handles.insert(job_id, ctrl_tx.clone());
    pty_tabs.push(PtyUiState::new(PtyUiStateNewArgs {
        ctrl_tx,
        cols,
        rows,
        owner,
        user_initiated,
        job_id,
        cmd,
        saved_path,
        status_path,
    }));
    *pty_active_idx = pty_tabs.len().saturating_sub(1);
    //（1）将 PTY 快捷操作提示放进聊天界面（system role）：
    //（2）简约态：严格一行（避免刷屏）
    //（3）详情态：按 “工具详情展开”(Alt+Tab) 才显示完整键位说明（UI 折叠负责）
    let header = "♡ · 系统信息 · Terminal（PTY）已启动";
    let details = if was_empty {
        render_pty_help_prompt(pty_help_prompt_text, PTY_DOUBLE_ESC_MS)
    } else {
        "已启动新的终端任务。".to_string()
    };
    let help = if details.trim().is_empty() {
        header.to_string()
    } else {
        format!("{header}\n\n{details}")
    };
    core.push_system(&help);
    render_cache.invalidate(core.history.len().saturating_sub(1));
    //（1）AI 启动 PTY 时自动弹出 Terminal 视图；用户随时可按 Home 返回聊天。
    *pty_view = true;
    let batch = pty_done_batches.batch_mut(owner);
    batch.total_started = batch.total_started.saturating_add(1);
    //（1）不向模型注入 PTY 索引：模型仅通过 PTY Done / PTY 审计获知后台状态。
}

pub(super) fn handle_async_event_pty_output(
    pty_tabs: &mut [PtyUiState],
    job_id: u64,
    bytes: Vec<u8>,
) {
    if let Some(state) = pty_tabs.iter_mut().find(|s| s.job_id == job_id) {
        state.process_output(&bytes);
    }
}

pub(super) fn handle_async_event_pty_spawned(
    pty_tabs: &mut [PtyUiState],
    job_id: u64,
    pid: Option<i32>,
    pgrp: Option<i32>,
) {
    if let Some(state) = pty_tabs.iter_mut().find(|s| s.job_id == job_id) {
        state.pid = pid;
        state.pgrp = pgrp;
    }
}

pub(super) struct HandleAsyncEventPtyJobDoneArgs<'a> {
    pub(super) core: &'a mut Core,
    pub(super) render_cache: &'a mut ui::ChatRenderCache,
    pub(super) pty_tabs: &'a mut Vec<PtyUiState>,
    pub(super) pty_active_idx: &'a mut usize,
    pub(super) pty_handles: &'a mut HashMap<u64, mpsc::Sender<PtyControl>>,
    pub(super) pty_view: &'a mut bool,
    pub(super) pty_done_batches: &'a mut PtyDoneBatches,
    pub(super) pty_done_followups: &'a mut VecDeque<PtyDoneFollowup>,
    pub(super) pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
    pub(super) input: &'a mut String,
    pub(super) cursor: &'a mut usize,
    pub(super) input_chars: &'a mut usize,
    pub(super) last_input_at: &'a mut Option<Instant>,
    pub(super) sys_log: &'a mut VecDeque<String>,
    pub(super) sys_log_limit: usize,
    pub(super) owner: MindKind,
    pub(super) user_initiated: bool,
    pub(super) job_id: u64,
    pub(super) cmd: String,
    pub(super) saved_path: String,
    pub(super) status_path: String,
    pub(super) exit_code: i32,
    pub(super) timed_out: bool,
    pub(super) user_exit: bool,
    pub(super) elapsed_ms: u128,
    pub(super) bytes: usize,
    pub(super) lines: usize,
}

pub(super) fn handle_async_event_pty_job_done(args: HandleAsyncEventPtyJobDoneArgs<'_>) {
    let HandleAsyncEventPtyJobDoneArgs {
        core,
        render_cache,
        pty_tabs,
        pty_active_idx,
        pty_handles,
        pty_view,
        pty_done_batches,
        pty_done_followups,
        pending_pty_snapshot,
        input,
        cursor,
        input_chars,
        last_input_at,
        sys_log,
        sys_log_limit,
        owner,
        user_initiated,
        job_id,
        cmd,
        saved_path,
        status_path,
        exit_code,
        timed_out,
        user_exit,
        elapsed_ms,
        bytes,
        lines,
    } = args;
    runlog_event(
        "INFO",
        "pty.done",
        json!({
            "owner": mind_label(owner),
            "job_id": job_id,
            "exit_code": exit_code,
            "timed_out": timed_out,
            "user_exit": user_exit,
            "elapsed_ms": elapsed_ms,
            "bytes": bytes,
            "lines": lines,
            "cmd": crate::truncate_with_suffix(cmd.as_str(), 300),
            "log": saved_path,
            "status": status_path,
        }),
    );
    pty_handles.remove(&job_id);
    if let Some(snap) = pending_pty_snapshot.take() {
        if snap.job_id == job_id {
            if let Some(pos) = input.find(snap.placeholder.as_str()) {
                input.replace_range(pos..pos + snap.placeholder.len(), "");
                *cursor = (*cursor).min(input.len());
                *input_chars = count_chars(input.as_str());
                if input.is_empty() {
                    *last_input_at = None;
                }
            }
        } else {
            *pending_pty_snapshot = Some(snap);
        }
    }
    //（1）pty.kill 的任务：仅通过 kill 工具回执汇报，避免和 PTY Done 聚合回执冲突。
    //（2）注意：kill 时已经从 total_started 中剔除，因此这里不再参与 completed/汇总计算。
    if pty_done_batches.take_suppressed_done(owner, job_id) {
        //（1）收尾：移除对应 tab；若不存在则忽略（可能已被 kill handler 先移除）。
        if let Some(pos) = pty_tabs.iter().position(|s| s.job_id == job_id) {
            pty_tabs.remove(pos);
            if pty_tabs.is_empty() {
                *pty_view = false;
                *pty_active_idx = 0;
            } else {
                *pty_active_idx = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            }
        }
        return;
    }
    //（1）DONE 先“缓存聚合”，避免多 tab 结束时连发多条 Tool result。
    let status = if timed_out {
        //（1）timeout 属于“结束方式”，不一定是错误：用 ok_ 前缀避免 UI 显示“执行失败：timeout”。
        "ok_timeout".to_string()
    } else if user_exit {
        "user_exit".to_string()
    } else {
        exit_code.to_string()
    };
    let tail = read_tail_text(&saved_path, 48_000).unwrap_or_default();
    let tail = truncate_with_suffix(tail.trim_end(), PTY_DONE_BATCH_TAIL_MAX_CHARS);
    let batch = pty_done_batches.batch_mut(owner);
    if batch.total_started == 0 {
        batch.total_started = 1;
    }
    batch.completed = batch.completed.saturating_add(1);
    batch.jobs.push(PtyDoneJob {
        job_id,
        cmd: cmd.clone(),
        user_initiated,
        saved_path: saved_path.clone(),
        status_path: status_path.clone(),
        status: status.clone(),
        exit_code,
        elapsed_ms,
        bytes,
        lines,
        tail,
    });
    let total = batch.total_started.max(batch.completed).max(1);
    let completed = batch.completed;
    push_sys_log(
        sys_log,
        sys_log_limit,
        format!("Terminal: finished (job_id:{job_id})"),
    );
    if completed < total {
        core.push_system(&format!("PTY {completed}/{total} DONE"));
        render_cache.invalidate(core.history.len().saturating_sub(1));
    } else {
        let jobs = batch.jobs.clone();
        let full = format_pty_done_batch_tool_text_unlimited(owner, &jobs, None);
        let batch_saved = export_pty_done_batch_summary(owner, &full);
        let tool_text = format_pty_done_batch_tool_text(owner, &jobs, batch_saved.as_ref());
        pty_done_followups.push_back(PtyDoneFollowup {
            owner,
            jobs: jobs.len(),
            tool_text,
            pushed: false,
        });
        pty_done_batches.reset(owner);
        core.push_system(&format!("PTY {completed}/{total} DONE"));
        render_cache.invalidate(core.history.len().saturating_sub(1));
    }

    //（1）收尾：移除对应 tab；若不存在则忽略（可能已被用户提前 Kill）。
    if let Some(pos) = pty_tabs.iter().position(|s| s.job_id == job_id) {
        pty_tabs.remove(pos);
        if pty_tabs.is_empty() {
            *pty_view = false;
            *pty_active_idx = 0;
        } else {
            *pty_active_idx = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
        }
    }
    //（1）不向模型注入 PTY 索引：模型仅通过 PTY Done / PTY 审计获知后台状态。
}

pub(super) struct HandleAsyncEventPtyToolRequestArgs<'a> {
    pub(super) pty_tabs: &'a mut Vec<PtyUiState>,
    pub(super) pty_active_idx: &'a mut usize,
    pub(super) pty_handles: &'a mut HashMap<u64, mpsc::Sender<PtyControl>>,
    pub(super) pty_view: &'a mut bool,
    pub(super) pty_done_batches: &'a mut PtyDoneBatches,
    pub(super) tx: &'a mpsc::Sender<AsyncEvent>,
    pub(super) pty_started_notice_prompt_text: &'a str,
    pub(super) pty_messages: &'a PtyMessages,
    pub(super) owner: MindKind,
    pub(super) call: Box<ToolCall>,
}

pub(super) fn handle_async_event_pty_tool_request(args: HandleAsyncEventPtyToolRequestArgs<'_>) {
    let HandleAsyncEventPtyToolRequestArgs {
        pty_tabs,
        pty_active_idx,
        pty_handles,
        pty_view,
        pty_done_batches,
        tx,
        pty_started_notice_prompt_text,
        pty_messages,
        owner,
        call,
    } = args;
    let started_at = Instant::now();
    let op = call.op.as_deref().unwrap_or("").trim().to_ascii_lowercase();
    let mut outcome = ToolOutcome {
        user_message: String::new(),
        log_lines: vec!["状态:0".to_string()],
    };
    let mut sys_msg = Some("工具执行完成".to_string());
    match op.as_str() {
        "list" => {
            let now = Instant::now();
            let mut items: Vec<&PtyUiState> =
                pty_tabs.iter().filter(|s| s.owner == owner).collect();
            items.sort_by_key(|s| s.job_id);
            let mut lines_out: Vec<String> = Vec::new();
            lines_out.push(format!("pty.list running={}", items.len()));
	            for s in items {
	                let elapsed = now.saturating_duration_since(s.started_at).as_secs();
	                let src = if s.user_initiated { "user" } else { "ai" };
	                let os_pid = s
	                    .pid
	                    .map(|v| v.to_string())
	                    .unwrap_or_else(|| "(pending)".to_string());
                let pgrp = s
                    .pgrp
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "(pending)".to_string());
                let mut cmd = s.cmd.trim().replace('\n', " ⏎ ").replace('\t', " ");
                cmd = truncate_with_suffix(&cmd, 72);
	                lines_out.push(format!(
	                    "- pid:{} os_pid:{} pgrp:{} src:{} {}s cmd:{}\n  log:{}\n  status:{}",
	                    s.job_id, os_pid, pgrp, src, elapsed, cmd, s.saved_path, s.status_path
	                ));
	            }
            outcome.user_message = lines_out.join("\n");
            let elapsed_ms = started_at.elapsed().as_millis();
            outcome.log_lines = vec![format!(
                "ok:true | result:0 | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"
            )];
        }
        "kill" => {
            //（1）kill 回执：对模型侧要“可重试/可验证”，对 UI 侧要“可观察且不噪音”。
            //（2）约定：
            //（3）成功：返回 kill 状态 + 对应任务的日志（尽量 inline；
            //（4）过大则导出并返回 saved/size/lines）。
            //（5）失败：只返回失败原因（例如 job_id 不存在）。
            const KILL_INLINE_LOG_MAX_BYTES: u64 = 120_000;
            const KILL_INLINE_LOG_MAX_CHARS: usize = 6000;
            const KILL_EXPORT_THRESHOLD_BYTES: usize = crate::mcp::EXPORT_SAVE_THRESHOLD_BYTES;
            const PTY_CACHE_DIR: &str = "log/pty-cache";

            fn read_log_inline(path: &str) -> Option<(String, bool, usize)> {
                let p = Path::new(path);
                let bytes = fs::metadata(p).ok().map(|m| m.len()).unwrap_or(0);
                let (mut text, truncated) = if bytes > KILL_INLINE_LOG_MAX_BYTES {
                    (
                        read_tail_text(path, KILL_INLINE_LOG_MAX_BYTES).unwrap_or_default(),
                        true,
                    )
                } else {
                    let buf = fs::read(p).ok()?;
                    let mut s = String::from_utf8_lossy(&buf).to_string();
                    s.retain(|ch| ch != '\u{0}');
                    (s, false)
                };
                text = text.replace('\r', "");
                let total = text.chars().count();
                let clipped = if total > KILL_INLINE_LOG_MAX_CHARS {
                    (truncate_tail(&text, KILL_INLINE_LOG_MAX_CHARS), true)
                } else {
                    (text, truncated)
                };
                let lines = clipped.0.lines().count();
                Some((clipped.0, clipped.1, lines))
            }

            fn export_if_needed(text: &str) -> Option<crate::mcp::ExportedOutputMeta> {
                let bytes = text.len();
                let lines = text.lines().count();
                let chars = text.chars().count();
                if bytes <= KILL_EXPORT_THRESHOLD_BYTES
                    && lines <= crate::mcp::OUTPUT_MAX_LINES
                    && chars <= crate::mcp::OUTPUT_MAX_CHARS
                {
                    return None;
                }
                let _ = fs::create_dir_all(PTY_CACHE_DIR);
                let now_ms = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .ok()
                    .map(|d| d.as_millis() as u64)
                    .unwrap_or(0);
                let id = next_pty_done_export_id();
                let path = format!("{PTY_CACHE_DIR}/pty_kill_{now_ms}_{id}.txt");
                if fs::write(&path, text).is_err() {
                    return None;
                }
                let bytes = fs::metadata(&path)
                    .ok()
                    .map(|m| m.len() as usize)
                    .unwrap_or(bytes);
                Some(crate::mcp::exported_meta(path, bytes, lines))
            }

            let now = Instant::now();
            let mut job_ids: Vec<u64> = Vec::new();
            if let Some(id) = call.job_id.filter(|v| *v > 0) {
                job_ids.push(id);
            }
            if let Some(id) = call.pid.filter(|v| *v > 0) {
                job_ids.push(id);
            }
            if let Some(ids) = call.job_ids.as_ref() {
                for id in ids {
                    if *id > 0 {
                        job_ids.push(*id);
                    }
                }
            }
            if let Some(ids) = call.pids.as_ref() {
                for id in ids {
                    if *id > 0 {
                        job_ids.push(*id);
                    }
                }
            }
            //（1）兼容模型把 pid/job_id 放进 input 字符串（例如 input:"pid=7" / "7"）。
            if job_ids.is_empty() {
                for part in call.input.split(|c: char| !c.is_ascii_digit()) {
                    let t = part.trim();
                    if t.is_empty() {
                        continue;
                    }
                    if let Ok(n) = t.parse::<u64>()
                        && n > 0
                    {
                        job_ids.push(n);
                    }
                }
            }
            job_ids.sort_unstable();
            job_ids.dedup();
            if job_ids.is_empty() {
                outcome.user_message = "pty.kill 缺少 pid/pids（或 job_id/job_ids）".to_string();
                let elapsed_ms = started_at.elapsed().as_millis();
                outcome.log_lines = vec![format!(
                    "ok:false | result:missing_pid | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
                let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
                return;
            };
            let mut found = 0usize;
            let mut failed = 0usize;
            let mut lines_out: Vec<String> = Vec::new();
            lines_out.push(format!("pty.kill jobs={}", job_ids.len()));
            for job_id in &job_ids {
                let mut hit = false;
                //（1）捕获任务信息（在移除前），用于 kill 回执里返回“被 kill 的终端任务是谁”。
                let info = pty_tabs.iter().find(|s| &s.job_id == job_id).map(|s| {
                    let elapsed = now.saturating_duration_since(s.started_at).as_secs();
                    let os_pid = s
                        .pid
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "(pending)".to_string());
                    let pgrp = s
                        .pgrp
                        .map(|v| v.to_string())
                        .unwrap_or_else(|| "(pending)".to_string());
                    let mut cmd = s.cmd.trim().replace('\n', " ⏎ ").replace('\t', " ");
                    cmd = truncate_with_suffix(&cmd, 96);
                    (elapsed, os_pid, pgrp, cmd, s.saved_path.clone())
                });
                if let Some(ctrl) = pty_handles.get(job_id) {
                    let _ = ctrl.send(PtyControl::Kill);
                    hit = true;
                } else if let Some(state) = pty_tabs.iter().find(|s| &s.job_id == job_id) {
                    let _ = state.ctrl_tx.send(PtyControl::Kill);
                    hit = true;
                }
                if hit {
                    found = found.saturating_add(1);
                    //（1）重要：抑制随后到来的 PtyJobDone 的 DONE 聚合回执（避免与 kill 回执冲突）。
                    //（2）同时从 total_started 中剔除，保证剩余任务还能正确触发 DONE 汇总。
                    pty_done_batches.suppress_done_for_killed_job(owner, *job_id);
                }
                if let Some((elapsed, os_pid, pgrp, cmd, log_path)) = info {
                    if hit {
                        let (log_text, log_trunc, log_lines) = read_log_inline(&log_path)
                            .unwrap_or_else(|| ("(log unavailable)".to_string(), false, 0usize));
                        lines_out.push(format!(
                            "# job_id:{job_id} status:killed elapsed:{elapsed}s os_pid:{os_pid} pgrp:{pgrp}\ncmd: {cmd}\nlog:\n{log_text}\nlog_truncated:{} | log_lines:{}",
                            if log_trunc { 1 } else { 0 },
                            log_lines
                        ));
                    } else {
                        failed = failed.saturating_add(1);
                        lines_out.push(format!("# job_id:{job_id} status:fail reason:not_found"));
                    }
                } else if hit {
                    lines_out.push(format!("# job_id:{job_id} status:killed (info missing)"));
                } else {
                    failed = failed.saturating_add(1);
                    lines_out.push(format!("# job_id:{job_id} status:fail reason:not_found"));
                }
                pty_handles.remove(job_id);
                if let Some(pos) = pty_tabs.iter().position(|s| &s.job_id == job_id) {
                    pty_tabs.remove(pos);
                }
            }
            if pty_tabs.is_empty() {
                *pty_view = false;
                *pty_active_idx = 0;
            } else {
                *pty_active_idx = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            }
            //（1）不向模型注入 PTY 索引：模型仅通过 PTY Done / PTY 审计获知后台状态。
            lines_out.push(format!(
                "summary: killed={found} failed={failed} total={}",
                job_ids.len()
            ));
            let joined = lines_out.join("\n").trim_end().to_string();
            if let Some(meta) = export_if_needed(&joined) {
                let preview = crate::mcp::truncate_export_preview(&joined);
                let mut body = String::new();
                body.push_str(&crate::mcp::format_exported_notice(&meta));
                if !preview.trim().is_empty() {
                    body.push_str("\n\n");
                    body.push_str(preview.trim_end());
                }
                outcome.user_message = body;
            } else {
                outcome.user_message = joined;
            }
            let elapsed_ms = started_at.elapsed().as_millis();
            if found == 0 {
                outcome.log_lines = vec![format!(
                    "ok:false | result:not_found | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
            } else if failed > 0 {
                outcome.log_lines.insert(
                    0,
                    format!("ok:true | result:partial | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"),
                );
            } else {
                outcome.log_lines = vec![format!(
                    "ok:true | result:0 | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"
                )];
            }
        }
        "input" => {
            //（1）向指定 PTY 任务写入 stdin（用于继续自动化：确认/下一条命令等）。
            //
            //（1）约定：
            //（2）需要显式 pid/pids（或 job_id/job_ids），避免误发到错误终端。
            //（3）payload 优先取 commands（逐条追加换行）；其次 content；最后 input。
            //（4）默认不强制切换到 Terminal 视图（由用户决定是否打开观察）。
            const SEND_LOG_TAIL_BYTES: u64 = 48_000;
            const SEND_INLINE_LOG_MAX_CHARS: usize = 2400;
            const SEND_EXPORT_THRESHOLD_BYTES: usize = crate::mcp::EXPORT_SAVE_THRESHOLD_BYTES;
            const PTY_CACHE_DIR: &str = "log/pty-cache";

            fn read_tail_for_preview(path: &str) -> String {
                let raw = read_tail_text(path, SEND_LOG_TAIL_BYTES).unwrap_or_default();
                let mut s = raw.replace('\r', "");
                s.retain(|ch| ch != '\u{0}');
                let s = s.trim_end().to_string();
                if s.chars().count() > SEND_INLINE_LOG_MAX_CHARS {
                    truncate_tail(&s, SEND_INLINE_LOG_MAX_CHARS)
                } else {
                    s
                }
            }

            fn export_if_needed(text: &str) -> Option<crate::mcp::ExportedOutputMeta> {
                let bytes = text.len();
                let lines = text.lines().count();
                let chars = text.chars().count();
                if bytes <= SEND_EXPORT_THRESHOLD_BYTES
                    && lines <= crate::mcp::OUTPUT_MAX_LINES
                    && chars <= crate::mcp::OUTPUT_MAX_CHARS
                {
                    return None;
                }
                let _ = fs::create_dir_all(PTY_CACHE_DIR);
                let now_ms = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .ok()
                    .map(|d| d.as_millis() as u64)
                    .unwrap_or(0);
                let id = next_pty_done_export_id();
                let path = format!("{PTY_CACHE_DIR}/pty_input_{now_ms}_{id}.txt");
                if fs::write(&path, text).is_err() {
                    return None;
                }
                let bytes = fs::metadata(&path)
                    .ok()
                    .map(|m| m.len() as usize)
                    .unwrap_or(bytes);
                Some(crate::mcp::exported_meta(path, bytes, lines))
            }

            let mut job_ids: Vec<u64> = Vec::new();
            if let Some(id) = call.job_id.filter(|v| *v > 0) {
                job_ids.push(id);
            }
            if let Some(id) = call.pid.filter(|v| *v > 0) {
                job_ids.push(id);
            }
            if let Some(ids) = call.job_ids.as_ref() {
                for id in ids {
                    if *id > 0 {
                        job_ids.push(*id);
                    }
                }
            }
            if let Some(ids) = call.pids.as_ref() {
                for id in ids {
                    if *id > 0 {
                        job_ids.push(*id);
                    }
                }
            }
            //（1）兼容模型把 pid/job_id 放进 input 字符串（例如 input:"pid=7" / "7"）。
            if job_ids.is_empty() {
                for part in call.input.split(|c: char| !c.is_ascii_digit()) {
                    let t = part.trim();
                    if t.is_empty() {
                        continue;
                    }
                    if let Ok(n) = t.parse::<u64>()
                        && n > 0
                    {
                        job_ids.push(n);
                    }
                }
            }
            job_ids.sort_unstable();
            job_ids.dedup();
            if job_ids.is_empty() {
                outcome.user_message = "pty.input 缺少 pid/pids（或 job_id/job_ids）".to_string();
                let elapsed_ms = started_at.elapsed().as_millis();
                outcome.log_lines = vec![format!(
                    "ok:false | result:missing_pid | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
                let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
                return;
            }

            let payload = if let Some(cmds) = call.commands.as_ref().filter(|v| !v.is_empty()) {
                //（1）commands：逐条发送，默认追加换行，方便“像输入命令一样”执行。
                let mut joined = cmds.join("\n");
                if !joined.ends_with('\n') {
                    joined.push('\n');
                }
                joined
            } else if let Some(c) = call
                .content
                .as_deref()
                .map(str::trim)
                .filter(|s| !s.is_empty())
            {
                c.to_string()
            } else {
                call.input.clone()
            };
            if payload.is_empty() {
                outcome.user_message = "pty.input 缺少 commands/content/input".to_string();
                let elapsed_ms = started_at.elapsed().as_millis();
                outcome.log_lines = vec![format!(
                    "ok:false | result:missing_input | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
                let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
                return;
            }
            let payload_bytes = payload.as_bytes().to_vec();
            let bytes_len = payload_bytes.len();

            let now = Instant::now();
            let mut sent = 0usize;
            let mut failed = 0usize;
            let mut lines_out: Vec<String> = Vec::new();
            lines_out.push(format!(
                "pty.input jobs={} bytes={}",
                job_ids.len(),
                bytes_len
            ));
            for job_id in &job_ids {
                let info = pty_tabs.iter().find(|s| &s.job_id == job_id).map(|s| {
                    let elapsed = now.saturating_duration_since(s.started_at).as_secs();
                    let mut cmd = s.cmd.trim().replace('\n', " ⏎ ").replace('\t', " ");
                    cmd = truncate_with_suffix(&cmd, 96);
                    (elapsed, cmd, s.saved_path.clone(), s.status_path.clone())
                });

                let mut ok = false;
                if let Some(ctrl) = pty_handles.get(job_id) {
                    let _ = ctrl.send(PtyControl::Input(payload_bytes.clone()));
                    ok = true;
                } else if let Some(state) = pty_tabs.iter().find(|s| &s.job_id == job_id) {
                    let _ = state.ctrl_tx.send(PtyControl::Input(payload_bytes.clone()));
                    ok = true;
                }
                if ok {
                    sent = sent.saturating_add(1);
                } else {
                    failed = failed.saturating_add(1);
                }

                if let Some((elapsed, cmd, log_path, status_path)) = info {
                    if ok {
                        let log_tail = read_tail_for_preview(&log_path);
                        lines_out.push(format!(
                            "# job_id:{job_id} status:sent elapsed:{elapsed}s\ncmd: {cmd}\nlog:{log_path}\nstatus:{status_path}\nlog_tail:\n{log_tail}"
                        ));
                    } else {
                        lines_out.push(format!("# job_id:{job_id} status:fail reason:not_found"));
                    }
                } else if ok {
                    lines_out.push(format!("# job_id:{job_id} status:sent (info missing)"));
                } else {
                    lines_out.push(format!("# job_id:{job_id} status:fail reason:not_found"));
                }
            }
            lines_out.push(format!(
                "summary: sent={sent} failed={failed} total={}",
                job_ids.len()
            ));

            let joined = lines_out.join("\n").trim_end().to_string();
            if let Some(meta) = export_if_needed(&joined) {
                let preview = crate::mcp::truncate_export_preview(&joined);
                let mut body = String::new();
                body.push_str(&crate::mcp::format_exported_notice(&meta));
                if !preview.trim().is_empty() {
                    body.push_str("\n\n");
                    body.push_str(preview.trim_end());
                }
                outcome.user_message = body;
            } else {
                outcome.user_message = joined;
            }

            let elapsed_ms = started_at.elapsed().as_millis();
            if sent == 0 {
                outcome.log_lines = vec![format!(
                    "ok:false | result:not_found | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
            } else if failed > 0 {
                outcome.log_lines = vec![format!(
                    "ok:true | result:partial | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"
                )];
            } else {
                outcome.log_lines = vec![format!(
                    "ok:true | result:0 | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"
                )];
            }
        }
        "run" => {
            //（1）统一策略：PTY 任务默认保持会话（不自动退出），避免“跑完就关”导致输出来不及观察/复用环境。
            //（2）若用户需要结束任务：用 pty.kill(pid) 或在 Terminal 视图 350ms 内双击 Esc。
            let keep_session = true;
            //（1）注意：此处是在事件循环中“快速回执”，新 PTY 的 PtyReady 事件尚未被消费，
            //（2）pty_tabs 里的 running 数量可能还未包含本次启动的任务；因此这里用估算值。
            let running_before = pty_tabs.iter().filter(|s| s.owner == owner).count();
            let mut cmds: Vec<String> = Vec::new();
            if let Some(list) = call.commands.clone() {
                for c in list {
                    let t = c.trim();
                    if !t.is_empty() {
                        cmds.push(t.to_string());
                    }
                }
            } else {
                for line in call.input.lines() {
                    let t = line.trim();
                    if !t.is_empty() {
                        cmds.push(t.to_string());
                    }
                }
            }
            if cmds.is_empty() {
                outcome.user_message = "pty.run 缺少 commands/input".to_string();
                let elapsed_ms = started_at.elapsed().as_millis();
                outcome.log_lines = vec![format!(
                    "ok:false | result:missing_commands | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                sys_msg = Some("工具执行失败".to_string());
                let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
                return;
            }
            //（1）单次 pty.run 最多接收 3 条命令：避免模型一次性启动过多后台任务。
            let max = 3usize;
            let total = cmds.len();
            cmds.truncate(max);
            //（1）背景 PTY 任务上限：避免 UI/模型陷入“多任务风暴”。
            //（2）运行时允许更多（用于意外情况/用户手动启动多个），但提示词仍要求模型最多使用 3 个。
            const PTY_TASKS_MAX: usize = 10;
            let running_now = pty_tabs.len();
            if running_now + cmds.len() > PTY_TASKS_MAX {
                let running_owner = running_before;
                outcome.user_message = format!(
                    "pty.run status=fail reason=too_many_running running_total={} running_owner={} add={} limit={PTY_TASKS_MAX}",
                    running_now,
                    running_owner,
                    cmds.len()
                );
                let elapsed_ms = started_at.elapsed().as_millis();
                outcome.log_lines = vec![format!(
                    "ok:false | result:limit_exceeded | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
                )];
                outcome.log_lines.push(format!(
                    "{MODEL_NOTE_PREFIX}[sys:PTY启动]启动失败：任务过多（running_total={running_now} running_owner={running_owner} add={} limit={PTY_TASKS_MAX}）。请向用户汇报；先结束一个 PTY（pty.kill(pid) 或 Terminal 视图双击 Esc）再重试（最多 1 次）；不要立刻连发重试。",
                    cmds.len()
                ));
                sys_msg = Some("工具执行失败".to_string());
                let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
                return;
            }
            let mut spawns: Vec<PtySpawnInfo> = Vec::new();
            for cmd in &cmds {
                let cmd = if keep_session {
                    format!("{cmd}\nexec bash -li")
                } else {
                    cmd.to_string()
                };
                let job_call = ToolCall {
                    tool: "bash".to_string(),
                    input: cmd,
                    brief: Some("pty.run".to_string()),
                    interactive: Some(true),
                    cwd: call.cwd.clone(),
                    //（1）统一策略：PTY 默认 30min 自动超时；避免后台会话永久占用。
                    //（2）结束任务也可用 Terminal 视图双击 Esc / pty.kill(pid)。
                    timeout_secs: Some(PTY_AUTO_TIMEOUT_SECS),
                    timeout_ms: None,
                    ..ToolCall::default()
                };
	                let spawn = spawn_interactive_bash_execution(
	                    job_call,
	                    owner,
	                    (*tx).clone(),
	                    pty_started_notice_prompt_text.to_string(),
	                    pty_messages.pty_started_model_note.clone(),
	                    false,
	                    false,
	                );
                spawns.push(spawn);
            }
            //（1）延迟约 2s：给后台任务一点时间输出头部日志，便于模型看到“已启动并在跑”的证据。
            //（2）不能阻塞 UI 线程：用后台线程在 2s 后发送 ToolStreamEnd。
            const PTY_RUN_SNAPSHOT_DELAY_MS: u64 = 2000;
            const PTY_RUN_LOG_TAIL_BYTES: u64 = 48_000;
            const PTY_RUN_INLINE_LOG_MAX_CHARS: usize = 3200;
            const PTY_RUN_EXPORT_THRESHOLD_BYTES: usize = crate::mcp::EXPORT_SAVE_THRESHOLD_BYTES;
            const PTY_CACHE_DIR: &str = "log/pty-cache";

            fn read_tail_for_preview(path: &str) -> String {
                let raw = read_tail_text(path, PTY_RUN_LOG_TAIL_BYTES).unwrap_or_default();
                let mut s = raw.replace('\r', "");
                s.retain(|ch| ch != '\u{0}');
                if s.chars().count() > PTY_RUN_INLINE_LOG_MAX_CHARS {
                    truncate_tail(&s, PTY_RUN_INLINE_LOG_MAX_CHARS)
                } else {
                    s.trim_end().to_string()
                }
            }

            fn export_if_needed(text: &str) -> Option<crate::mcp::ExportedOutputMeta> {
                let bytes = text.len();
                let lines = text.lines().count();
                let chars = text.chars().count();
                if bytes <= PTY_RUN_EXPORT_THRESHOLD_BYTES
                    && lines <= crate::mcp::OUTPUT_MAX_LINES
                    && chars <= crate::mcp::OUTPUT_MAX_CHARS
                {
                    return None;
                }
                let _ = fs::create_dir_all(PTY_CACHE_DIR);
                let now_ms = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .ok()
                    .map(|d| d.as_millis() as u64)
                    .unwrap_or(0);
                let id = next_pty_done_export_id();
                let path = format!("{PTY_CACHE_DIR}/pty_run_{now_ms}_{id}.txt");
                if fs::write(&path, text).is_err() {
                    return None;
                }
                let bytes = fs::metadata(&path)
                    .ok()
                    .map(|m| m.len() as usize)
                    .unwrap_or(bytes);
                Some(crate::mcp::exported_meta(path, bytes, lines))
            }

            let tx2 = (*tx).clone();
            let cmds2 = cmds.clone();
            let spawns2 = spawns.clone();
            thread::spawn(move || {
                let started = Instant::now();
                thread::sleep(Duration::from_millis(PTY_RUN_SNAPSHOT_DELAY_MS));

                let mut out: Vec<String> = Vec::new();
                out.push(format!("pty.run started={}", spawns2.len()));
                for (idx, (cmd, sp)) in cmds2.iter().zip(spawns2.iter()).enumerate() {
                    let mut cmd_prev = cmd.replace('\n', " ⏎ ").replace('\t', " ");
                    cmd_prev = truncate_with_suffix(cmd_prev.trim(), 96);
                    let log_prev = read_tail_for_preview(&sp.saved_path);
                    out.push(format!(
                        "- {} pid:{}\n  cmd:{}\n  log:{}\n  status:{}\n  log_tail:\n{}",
                        idx + 1,
                        sp.job_id,
                        cmd_prev,
                        sp.saved_path,
                        sp.status_path,
                        if log_prev.trim().is_empty() {
                            "(no output yet)".to_string()
                        } else {
                            log_prev
                        }
                    ));
                }
                if total > max {
                    out.push(format!("- … +{}", total - max));
                }
                out.push("hint(ai): pty启动后，用户会进行交互或观察，你可调用 pty.list 或通过日志查看运行状态，或直接汇报结果并等待。".to_string());

                let joined = out.join("\n").trim_end().to_string();
                let elapsed_ms = started.elapsed().as_millis();
                let mut outcome = ToolOutcome {
                    user_message: joined.clone(),
                    log_lines: vec![format!(
                        "ok:true | result:0 | exit:0 | elapsed_ms:{elapsed_ms} | 状态:0"
                    )],
                };
                if let Some(meta) = export_if_needed(&joined) {
                    let preview = crate::mcp::truncate_export_preview(&joined);
                    let mut body = String::new();
                    body.push_str(&crate::mcp::format_exported_notice(&meta));
                    if !preview.trim().is_empty() {
                        body.push_str("\n\n");
                        body.push_str(preview.trim_end());
                    }
                    outcome.user_message = body;
                }
                let _ = tx2.send(AsyncEvent::ToolStreamEnd {
                    outcome,
                    sys_msg: Some("PTY 已启动（快照）".to_string()),
                });
            });
            return;
        }
        _ => {
            outcome.user_message = "pty 工具 op 仅支持 run/list/kill/input".to_string();
            let elapsed_ms = started_at.elapsed().as_millis();
            outcome.log_lines = vec![format!(
                "ok:false | result:unsupported_op | exit:0 | elapsed_ms:{elapsed_ms} | 状态:fail"
            )];
            sys_msg = Some("工具执行失败".to_string());
        }
    }
    let _ = tx.send(AsyncEvent::ToolStreamEnd { outcome, sys_msg });
}

pub(super) struct DrawPtyPanelArgs<'a> {
    pub(super) theme: &'a ui::Theme,
    pub(super) area: ratatui::layout::Rect,
    pub(super) pty_tabs: &'a mut Vec<PtyUiState>,
    pub(super) pty_active_idx: usize,
    pub(super) pty_focus: PtyFocus,
}

pub(super) fn draw_pty_panel(f: &mut ratatui::Frame, args: DrawPtyPanelArgs<'_>) {
    let DrawPtyPanelArgs {
        theme,
        area,
        pty_tabs,
        pty_active_idx,
        pty_focus,
    } = args;
    if pty_tabs.is_empty() {
        return;
    }
    let pty_focused = matches!(pty_focus, PtyFocus::Terminal);
    let border = if pty_focused {
        theme.border_active
    } else {
        theme.border_idle
    };
    let total = pty_tabs.len().max(1);
    let active0 = pty_active_idx.min(total.saturating_sub(1));
    let title = {
        let s = &pty_tabs[active0];
        let cmd_clean = s.cmd.trim().replace('\n', " ⏎ ").replace('\t', " ");
        let cmd = truncate_with_ellipsis(&cmd_clean, 36);
        format!(
            "Terminal {}/{} · job:{} · {}",
            active0.saturating_add(1),
            total,
            s.job_id,
            cmd
        )
    };
    let block = Block::default()
        .borders(Borders::ALL)
        .title(title)
        .border_style(Style::default().fg(border));
    let inner = block.inner(area);
    let cols = inner.width.max(1);
    let rows = inner.height.max(1);
    let mut lines: Vec<Line<'static>> = Vec::new();
    for s in pty_tabs.iter_mut() {
        s.ensure_size(cols, rows);
    }
    let active = pty_active_idx.min(pty_tabs.len().saturating_sub(1));
    if let Some(state) = pty_tabs.get_mut(active) {
        state.rebuild_cache();
        let base = Style::default().fg(theme.fg).bg(theme.bg);
        for l in &state.screen_lines {
            lines.push(Line::from(vec![Span::styled(l.clone(), base)]));
        }
        f.render_widget(
            Paragraph::new(lines)
                .style(base)
                .block(block)
                .scroll((0, 0)),
            area,
        );
        if pty_focused && state.scroll == 0 && !state.parser.screen().hide_cursor() {
            let (cur_row, cur_col) = state.parser.screen().cursor_position();
            let max_col = cols.saturating_sub(1);
            let max_row = rows.saturating_sub(1);
            let cx = inner.x.saturating_add(cur_col.min(max_col));
            let cy = inner.y.saturating_add(cur_row.min(max_row));
            f.set_cursor_position((cx, cy));
        }
    }
}

pub(super) fn apply_mouse_wheel_to_pty_view(
    pty_view: bool,
    pty_tabs: &mut [PtyUiState],
    pty_active_idx: usize,
    up: bool,
    delta: u16,
) -> bool {
    if !pty_view || pty_tabs.is_empty() {
        return false;
    }
    let active = pty_active_idx.min(pty_tabs.len().saturating_sub(1));
    if let Some(state) = pty_tabs.get_mut(active) {
        if up {
            state.scroll = state.scroll.saturating_add(delta).min(PTY_SCROLLBACK_MAX);
        } else {
            state.scroll = state.scroll.saturating_sub(delta);
        }
        state.dirty = true;
        return true;
    }
    false
}

pub(super) fn apply_touch_drag_to_pty_view(
    pty_view: bool,
    pty_tabs: &mut [PtyUiState],
    pty_active_idx: usize,
    dy: i32,
    delta: u16,
) -> bool {
    if !pty_view || pty_tabs.is_empty() {
        return false;
    }
    let active = pty_active_idx.min(pty_tabs.len().saturating_sub(1));
    if let Some(state) = pty_tabs.get_mut(active) {
        if dy > 0 {
            state.scroll = state.scroll.saturating_add(delta).min(PTY_SCROLLBACK_MAX);
        } else {
            state.scroll = state.scroll.saturating_sub(delta);
        }
        state.dirty = true;
        return true;
    }
    false
}

pub(super) struct TryShowPtyViewOnHomeArgs<'a> {
    pub(super) key: crossterm::event::KeyEvent,
    pub(super) ctrl: bool,
    pub(super) alt: bool,
    pub(super) pty_view: &'a mut bool,
    pub(super) pty_tabs: &'a [PtyUiState],
    pub(super) pty_focus: &'a mut PtyFocus,
    pub(super) selected_msg_idx: Option<usize>,
    pub(super) send_queue_closed: bool,
    pub(super) menu_open: bool,
}

pub(super) fn try_show_pty_view_on_home(args: TryShowPtyViewOnHomeArgs<'_>) -> bool {
    let TryShowPtyViewOnHomeArgs {
        key,
        ctrl,
        alt,
        pty_view,
        pty_tabs,
        pty_focus,
        selected_msg_idx,
        send_queue_closed,
        menu_open,
    } = args;

    if *pty_view
        || pty_tabs.is_empty()
        || !matches!(key.code, KeyCode::Home)
        || ctrl
        || alt
        || selected_msg_idx.is_some()
        || !send_queue_closed
        || menu_open
    {
        return false;
    }
    *pty_view = true;
    *pty_focus = PtyFocus::Terminal;
    true
}

pub(super) struct HandlePtyViewKeyArgs<'a> {
    pub(super) now: Instant,
    pub(super) key: crossterm::event::KeyEvent,
    pub(super) ctrl: bool,
    pub(super) alt: bool,
    pub(super) pty_tabs: &'a mut Vec<PtyUiState>,
    pub(super) pty_active_idx: &'a mut usize,
    pub(super) pty_focus: &'a mut PtyFocus,
    pub(super) pty_handles: &'a mut HashMap<u64, mpsc::Sender<PtyControl>>,
    pub(super) pty_view: &'a mut bool,
    pub(super) pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
    pub(super) input: &'a mut String,
    pub(super) cursor: &'a mut usize,
    pub(super) input_chars: &'a mut usize,
    pub(super) last_input_at: &'a mut Option<Instant>,
    pub(super) last_esc_at: &'a mut Option<Instant>,
    pub(super) sys_log: &'a mut VecDeque<String>,
    pub(super) sys_log_limit: usize,
    //（1）状态栏右侧动作提示（短暂显示）：仅用于人类观测。
    pub(super) action_hint: &'a mut Option<(Instant, String)>,
}

pub(super) fn handle_pty_view_key(args: HandlePtyViewKeyArgs<'_>) -> bool {
    let HandlePtyViewKeyArgs {
        now,
        key,
        ctrl,
        alt,
        pty_tabs,
        pty_active_idx,
        pty_focus,
        pty_handles,
        pty_view,
        pending_pty_snapshot,
        input,
        cursor,
        input_chars,
        last_input_at,
        last_esc_at,
        sys_log,
        sys_log_limit,
        action_hint,
    } = args;
    if !*pty_view || pty_tabs.is_empty() {
        return false;
    }

    if !matches!(key.code, KeyCode::Esc) {
        *last_esc_at = None;
    }
    //（1）PgUp/PgDn：仅在“PTY 焦点”下切换不同终端任务。
    //（2）焦点切换必须由用户手动点击（避免 PgUp/PgDn 抢走聊天区的“选消息”语义）。
    if !ctrl
        && !alt
        && (matches!(key.code, KeyCode::PageUp) || matches!(key.code, KeyCode::PageDown))
        && matches!(*pty_focus, PtyFocus::Terminal)
    {
        let n = pty_tabs.len().max(1);
        let mut idx = (*pty_active_idx).min(n.saturating_sub(1));
        if matches!(key.code, KeyCode::PageDown) {
            idx = (idx + 1) % n;
        } else {
            idx = (idx + n - 1) % n;
        }
        *pty_active_idx = idx;
        *action_hint = Some((now, format!("PTY: Tab {}/{}", idx.saturating_add(1), n)));
        *last_esc_at = None;
        runlog_event(
            "INFO",
            "pty.ui.tab",
            json!({"active_idx":pty_active_idx,"tabs":n}),
        );
        return true;
        //（1）焦点在聊天区：不拦截，让 core.rs 的 PgUp/PgDn 消息选择逻辑生效。
    }
    //（1）终端内程序常用 Esc（vim/less/cancel 等），因此默认透传 Esc；
    //（2）若用户快速连按两次 Esc，则将第二次视为“结束当前终端任务”（终止 PTY）。
    if matches!(*pty_focus, PtyFocus::Terminal) && !ctrl && !alt && matches!(key.code, KeyCode::Esc)
    {
        if let Some(t0) = *last_esc_at
            && now.saturating_duration_since(t0) <= Duration::from_millis(PTY_DOUBLE_ESC_MS)
        {
            let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            let killed_job_id = pty_tabs.get(active).map(|s| s.job_id);
            if let Some(job_id) = killed_job_id
                && let Some(tx_kill) = pty_handles.get(&job_id)
            {
                let _ = tx_kill.send(PtyControl::Kill);
            }
            if !pty_tabs.is_empty() {
                pty_tabs.remove(active);
            }
            if pty_tabs.is_empty() {
                *pty_view = false;
                *pty_active_idx = 0;
            } else {
                *pty_active_idx = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            }
            //（1）不向模型注入 PTY 索引：避免“启动/终止 PTY”造成额外上下文噪声。
            push_sys_log(
                sys_log,
                sys_log_limit,
                format!(
                    "Terminal: double-esc kill (job_id:{})",
                    killed_job_id.unwrap_or(0)
                ),
            );
            *action_hint = Some((now, "Esc×2: 结束终端".to_string()));
            runlog_event(
                "INFO",
                "pty.ui.kill",
                json!({"via":"double_esc","job_id":killed_job_id.unwrap_or(0)}),
            );
            if let Some(snap) = pending_pty_snapshot.take() {
                if Some(snap.job_id) == killed_job_id {
                    if let Some(pos) = input.find(snap.placeholder.as_str()) {
                        input.replace_range(pos..pos + snap.placeholder.len(), "");
                        *cursor = (*cursor).min(input.len());
                        *input_chars = count_chars(input);
                        if input.is_empty() {
                            *last_input_at = None;
                        }
                    }
                } else {
                    *pending_pty_snapshot = Some(snap);
                }
            }
            *last_esc_at = None;
            return true;
        }
        *last_esc_at = Some(now);
    }
    //（1）Home：隐藏终端视图（后台继续运行）。
    if !ctrl && !alt && matches!(key.code, KeyCode::Home) {
        *pty_view = false;
        *last_esc_at = None;
        *action_hint = Some((now, "Home: 隐藏终端".to_string()));
        runlog_event("INFO", "pty.ui.home", json!({"action":"hide"}));
        return true;
    }
    //（1）End/Ins：仅在“终端焦点”时用于快速滚动。
    //（2）End：回到底部（实时输出）
    //（3）Ins：跳到最前（最大回看）
    if matches!(*pty_focus, PtyFocus::Terminal) && !ctrl && !alt {
        if matches!(key.code, KeyCode::End) {
            let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            if let Some(state) = pty_tabs.get_mut(active) {
                state.scroll = 0;
                state.dirty = true;
            }
            *action_hint = Some((now, "End: 终端置底".to_string()));
            return true;
        }
        if matches!(key.code, KeyCode::Insert) {
            let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
            if let Some(state) = pty_tabs.get_mut(active) {
                state.scroll = PTY_SCROLLBACK_MAX;
                state.dirty = true;
            }
            *action_hint = Some((now, "Ins: 终端回看".to_string()));
            return true;
        }
    }
    if matches!(*pty_focus, PtyFocus::Terminal) && alt && !ctrl && matches!(key.code, KeyCode::Up) {
        let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
        if let Some(state) = pty_tabs.get_mut(active) {
            let snap_plain = state.snapshot_plain();
            let snap = if snap_plain.trim().is_empty() {
                "(empty)".to_string()
            } else {
                truncate_with_suffix(&snap_plain, PTY_SNAPSHOT_MAX_CHARS)
            };
            state.last_user_snapshot = Some(snap.clone());

            //（1）Alt+↑：把快照“暂存到聊天输入框”，由用户决定是否发送给 AI。
            //（2）若上一次快照仍未发送且仍在输入框中，则先移除旧快照块，避免堆叠。
            if let Some(old) = pending_pty_snapshot.take()
                && let Some(pos) = input.find(old.placeholder.as_str())
            {
                input.replace_range(pos..pos + old.placeholder.len(), "");
            }

            *pending_pty_snapshot = Some(PendingPtySnapshot {
                job_id: state.job_id,
                placeholder: USER_PTY_SNAPSHOT_PLACEHOLDER.to_string(),
                content: snap.clone(),
            });
            if !input.is_empty() && input.as_bytes().last().copied() != Some(10) {
                input.push(10 as char);
            }
            input.push_str(USER_PTY_SNAPSHOT_PLACEHOLDER);
            input.push(10 as char);
            *cursor = input.len();
            *input_chars = count_chars(input);
            *last_input_at = Some(now);
            push_sys_log(
                sys_log,
                sys_log_limit,
                "Terminal: snapshot staged (in input)",
            );
            *action_hint = Some((now, "Alt+↑: 快照暂存".to_string()));
        }
        return true;
    }
    //（1）Chat 焦点时：不拦截其它按键，让聊天滚动/其它快捷键继续工作。
    if matches!(*pty_focus, PtyFocus::Chat) {
        //（1）终端面板展开时，输入框仍可见：允许用户在输入框里编辑文本，
        //（2）并由 core.rs 决定“Enter 发送到 AI 还是发送到 Terminal”。
        //（3）因此这里不再吞掉会修改 input 的按键。
        return false;
    }

    let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
    if let Some(state) = pty_tabs.get_mut(active)
        && let Some(bytes) = key_to_pty_bytes(key.code, key.modifiers)
    {
        state.record_input_bytes(&bytes);
        if let Some(tx_in) = pty_handles.get(&state.job_id) {
            let _ = tx_in.send(PtyControl::Input(bytes));
        }
    }
    true
}
