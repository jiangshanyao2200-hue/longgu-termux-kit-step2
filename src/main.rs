use std::collections::{BTreeSet, VecDeque};
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::Context;
use crossterm::event::{
    DisableBracketedPaste, EnableBracketedPaste, Event, KeyCode, KeyModifiers, MouseEventKind,
};
use crossterm::execute;
use crossterm::terminal::{
    Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph};
use portable_pty::{CommandBuilder, PtySize, native_pty_system};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::json;
use signal_hook::consts::signal::{SIGHUP, SIGINT, SIGQUIT, SIGTERM};
use signal_hook::iterator::Signals;
use unicode_width::UnicodeWidthChar;

mod mcp;
mod memorydb;
mod ui;

use crate::commands::filter_commands_for_input;
use crate::config::{AppConfig, ContextPromptConfig, DogApiConfig, MainApiConfig, SystemConfig};
use crate::input::{
    PasteCapture, PlaceholderRemove, can_accept_more, count_chars, is_paste_like_activity,
    materialize_pastes, maybe_begin_paste_capture, maybe_finalize_paste_capture,
    next_char_boundary, prev_char_boundary, prune_pending_pastes, snap_cursor_out_of_placeholder,
    try_insert_char_limited, try_insert_str_limited, try_remove_paste_placeholder_at_cursor,
    update_paste_burst,
};
use crate::mcp::{
    ToolCall, ToolOutcome, extract_tool_calls, format_tool_message_raw,
    handle_tool_call_with_retry, tool_requires_confirmation,
};
use crate::memorydb::{MemoDb, MemoKind, build_memo_entry};

pub use crate::commands::CommandSpec;
use crate::types::THINKING_MARKER;
pub use crate::types::{
    ContextLine, Core, Message, MindKind, Mode, PulseDir, Role, Screen, SettingsFocus,
};

const TOOL_STREAM_PREVIEW_MAX: usize = 8000;
const HEARTBEAT_FADE_FRAMES: u64 = 4;
const HEARTBEAT_BLINK_FRAMES: u64 = 4;
const HEARTBEAT_CYCLE_FRAMES: u64 = HEARTBEAT_FADE_FRAMES * 3 + HEARTBEAT_BLINK_FRAMES;
const HEARTBEAT_CYCLES: u64 = 2;
const HEARTBEAT_BANNER: &str = "♡";
const PTY_SCROLLBACK_MAX: u16 = 3000;
const PTY_SNAPSHOT_MAX_CHARS: usize = 8000;
// 交互 PTY：结束时回传给模型的“可读输出”预算（防止撑爆上下文）。
const PTY_RETURN_SCREEN_MAX_CHARS: usize = 12_000;
const PTY_RETURN_RAW_TAIL_BYTES: u64 = 64 * 1024;
const PTY_RETURN_RAW_TAIL_MAX_CHARS: usize = 6_000;
const PTY_RETURN_STDIN_MAX_CHARS: usize = 2_000;
// DeepSeek chat/completions 没有 tool role，且部分提供方对“最后一条消息 role”很敏感：
// - 若最后是 assistant（或 system），可能报 400（如 Invalid consecutive assistant message）
// 解决：仅在“发起请求快照”时，必要时追加一条“伪 user 占位”。
// 这条文本必须清晰声明“非用户输入”，避免被模型当成用户意图。
const DEEPSEEK_TOOL_LOOP_TICK_USER: &str =
    "[AITERMUX_INTERNAL_TOOL_LOOP] 系统注入：非用户输入。请基于上一条 Tool result 继续当前任务。";

#[cfg(unix)]
fn set_fd_nonblocking(fd: i32, enabled: bool) {
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags < 0 {
            return;
        }
        let new_flags = if enabled {
            flags | libc::O_NONBLOCK
        } else {
            flags & !libc::O_NONBLOCK
        };
        let _ = libc::fcntl(fd, libc::F_SETFL, new_flags);
    }
}

struct PulseNotice {
    started_at: Instant,
    msg_idx: usize,
    done: bool,
}

#[derive(Debug, Clone)]
enum PtyControl {
    Input(Vec<u8>),
    Resize { cols: u16, rows: u16 },
    Kill,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PtyFocus {
    Terminal,
    ChatInput,
}

struct PtyUiState {
    ctrl_tx: mpsc::Sender<PtyControl>,
    parser: vt100::Parser,
    cols: u16,
    rows: u16,
    pending_cols: u16,
    pending_rows: u16,
    pending_since: Option<Instant>,
    scroll: u16,
    scroll_applied: u16,
    saved_path: String,
    // 按用户决策：交互输入也记录并回传给 AI（含敏感信息）。
    input_log: String,
    // 用户主动同步的快照（Alt+↑）。若存在，工具结束时优先回传该快照。
    last_user_snapshot: Option<String>,
    screen_lines: Vec<String>,
    dirty: bool,
}

impl PtyUiState {
    fn new(ctrl_tx: mpsc::Sender<PtyControl>, cols: u16, rows: u16, saved_path: String) -> Self {
        // scrollback 给一个较大的默认值，保证交互期可回看。
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
            saved_path,
            input_log: String::new(),
            last_user_snapshot: None,
            screen_lines: Vec::new(),
            dirty: true,
        }
    }

    fn process_output(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        self.parser.process(bytes);
        // 终端模拟（最小集）：响应 Cursor Position Report (CPR) 查询 `ESC[6n`。
        // 一些 TUI（例如 codex/crossterm）会向 stdout 写入 `ESC[6n`，并期待终端回写 `ESC[{row};{col}R` 到 stdin。
        // 在我们的“PTY + vt100 parser”架构中，若不响应，应用会报类似：
        // "Error: The cursor position could not be read..."
        const CPR: [u8; 4] = [0x1B, b'[', b'6', b'n'];
        const SR: [u8; 4] = [0x1B, b'[', b'5', b'n']; // Status Report
        const DA1: [u8; 2] = [0x1B, b'c']; // Device Attributes
        const DA2: [u8; 4] = [0x1B, b'[', b'0', b'c']; // Device Attributes (explicit 0)

        if bytes.windows(SR.len()).any(|w| w == SR) {
            let _ = self.ctrl_tx.send(PtyControl::Input(b"\x1b[0n".to_vec()));
        }
        if bytes.windows(DA1.len()).any(|w| w == DA1) || bytes.windows(DA2.len()).any(|w| w == DA2)
        {
            let _ = self
                .ctrl_tx
                .send(PtyControl::Input(b"\x1b[?1;0c".to_vec()));
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

    fn record_input_bytes(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }
        // 只做最小可读化记录：可打印字符保留，其它控制字符用占位。
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

    fn ensure_size(&mut self, cols: u16, rows: u16) {
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
        // Termux 软键盘弹出/收回会造成尺寸快速抖动：等尺寸稳定一小段时间再 resize，
        // 避免 vt100 parser 频繁 set_size 导致“屏幕跳动/错位”。
        const RESIZE_DEBOUNCE_MS: u64 = 160;
        if let Some(since) = self.pending_since
            && since.elapsed() < Duration::from_millis(RESIZE_DEBOUNCE_MS)
        {
            return;
        }
        self.cols = self.pending_cols;
        self.rows = self.pending_rows;
        self.pending_since = None;
        self.parser.set_size(self.rows, self.cols);
        let _ = self
            .ctrl_tx
            .send(PtyControl::Resize { cols: self.cols, rows: self.rows });
        self.dirty = true;
    }

    fn rebuild_cache(&mut self) {
        if !self.dirty && self.scroll_applied == self.scroll {
            return;
        }
        self.parser.set_scrollback(self.scroll as usize);
        let applied = self.parser.screen().scrollback().min(u16::MAX as usize) as u16;
        self.scroll = applied;
        self.scroll_applied = applied;
        let text = self.parser.screen().contents().to_string();
        self.screen_lines = text.split('\n').map(|s| s.to_string()).collect();
        self.dirty = false;
    }

    fn snapshot_plain(&mut self) -> String {
        self.rebuild_cache();
        self.screen_lines.join("\n").trim_end().to_string()
    }
}

fn truncate_to_max_bytes(s: &mut String, max_bytes: usize) {
    if s.len() <= max_bytes {
        return;
    }
    let mut end = max_bytes.min(s.len());
    while end > 0 && !s.is_char_boundary(end) {
        end = end.saturating_sub(1);
    }
    s.truncate(end);
}

fn safe_prefix(text: &str, end: usize) -> String {
    let mut idx = end.min(text.len());
    while idx > 0 && !text.is_char_boundary(idx) {
        idx = idx.saturating_sub(1);
    }
    text[..idx].to_string()
}

fn key_to_pty_bytes(code: KeyCode, mods: KeyModifiers) -> Option<Vec<u8>> {
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
                // 按常规定义：Ctrl + ASCII('@'..='_') => codepoint & 0x1F
                // 这也覆盖了常用的 Ctrl+[ == ESC (0x1B) 等组合。
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

fn read_proc_children_pids(pid: i32) -> Vec<i32> {
    if pid <= 0 {
        return Vec::new();
    }
    let path = format!("/proc/{pid}/task/{pid}/children");
    let Ok(text) = fs::read_to_string(&path) else {
        return Vec::new();
    };
    text.split_whitespace()
        .filter_map(|s| s.trim().parse::<i32>().ok())
        .filter(|p| *p > 0 && *p != pid)
        .collect()
}

fn kill_process_tree(pid: i32, sig: i32, depth: usize) {
    if pid <= 0 || depth > 64 {
        return;
    }
    for child in read_proc_children_pids(pid) {
        kill_process_tree(child, sig, depth.saturating_add(1));
    }
    unsafe {
        libc::kill(pid, sig);
    }
}

fn find_tool_start(text: &str) -> Option<usize> {
    if let Some(pos) = text.find("<tool>") {
        return Some(pos);
    }
    if let Some(pos) = text.find("\"tool\"") {
        if let Some(brace) = text[..pos].rfind('{') {
            return Some(brace);
        }
        return Some(pos);
    }
    None
}

fn extract_tool_name_hint(text: &str) -> Option<String> {
    let pos = text.find("\"tool\"")?;
    let rest = &text[pos + "\"tool\"".len()..];
    let mut iter = rest.chars();
    // skip whitespace until ':'
    for ch in iter.by_ref() {
        if ch == ':' {
            break;
        }
        if !ch.is_whitespace() {
            return None;
        }
    }
    let rest: String = iter.collect();
    let trimmed = rest.trim_start();
    let quoted = trimmed.strip_prefix('"')?;
    let end = quoted.find('"')?;
    Some(quoted[..end].to_string())
}

fn mind_label(kind: MindKind) -> &'static str {
    match kind {
        MindKind::Main => "MAIN",
        MindKind::Sub => "DOG",
    }
}

fn pulse_dir(from: MindKind, to: MindKind) -> PulseDir {
    match (from, to) {
        (MindKind::Main, MindKind::Sub) => PulseDir::MainToDog,
        (MindKind::Sub, MindKind::Main) => PulseDir::DogToMain,
        _ => PulseDir::MainToDog,
    }
}

#[derive(Debug, Clone, Copy)]
struct MindPulse {
    dir: PulseDir,
    until: Instant,
}

fn normalize_mind_target_kind(raw: &str) -> Option<MindKind> {
    let t = raw.trim().to_ascii_lowercase();
    match t.as_str() {
        "dog" | "sub" | "todog" | "to_dog" | "to-dog" | "潜意识" => Some(MindKind::Sub),
        "main" | "tomain" | "to_main" | "to-main" | "主意识" | "萤" => Some(MindKind::Main),
        _ => None,
    }
}

fn resolve_mind_target_kind(call: &ToolCall) -> Option<MindKind> {
    call.target
        .as_deref()
        .and_then(normalize_mind_target_kind)
        .or_else(|| normalize_mind_target_kind(call.input.trim()))
}

fn resolve_mind_message_text(call: &ToolCall) -> String {
    if let Some(content) = call
        .content
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        return content.to_string();
    }
    let input = call.input.trim();
    if normalize_mind_target_kind(input).is_some() {
        return String::new();
    }
    input.to_string()
}

fn strip_thinking_stream(raw: &str) -> String {
    let mut out = String::new();
    let mut rest = raw;
    loop {
        let Some(start) = rest.find("<thinking") else {
            out.push_str(rest);
            break;
        };
        out.push_str(&rest[..start]);
        let after = &rest[start..];
        if let Some(close_idx) = after.find("</thinking>") {
            let tail = &after[close_idx + "</thinking>".len()..];
            rest = tail;
            continue;
        }
        break;
    }
    out
}

fn open_thinking_tail(raw: &str) -> Option<String> {
    let tag = "<thinking>";
    let open = raw.rfind(tag)?;
    let after_open = &raw[open + tag.len()..];
    if after_open.contains("</thinking>") {
        None
    } else {
        Some(after_open.to_string())
    }
}

fn reset_paste_burst(
    burst_count: &mut usize,
    burst_last_at: &mut Option<Instant>,
    burst_started_at: &mut Option<Instant>,
    burst_start_cursor: &mut Option<usize>,
) {
    *burst_count = 0;
    *burst_last_at = None;
    *burst_started_at = None;
    *burst_start_cursor = None;
}

fn arm_paste_block(
    now: Instant,
    config: &AppConfig,
    paste_drop_until: &mut Option<Instant>,
    paste_guard_until: &mut Option<Instant>,
) {
    *paste_drop_until = Some(now + Duration::from_millis(config.paste_drop_cooldown_ms));
    *paste_guard_until = Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
}

fn set_input_limit_toast(
    now: Instant,
    max_input_chars: usize,
    toast: &mut Option<(Instant, String)>,
    truncated: bool,
) {
    let (ttl_ms, msg) = if truncated {
        (
            1800,
            format!("超出输入上限：{max_input_chars} 字符（已截断）"),
        )
    } else {
        (1200, format!("输入已达上限：{max_input_chars} 字符"))
    };
    *toast = Some((now + Duration::from_millis(ttl_ms), msg));
}

struct ActiveStatusArgs<'a> {
    now: Instant,
    retry_status: Option<&'a str>,
    sending_until: Option<Instant>,
    mode: Mode,
    reveal_idx: Option<usize>,
    streaming_has_content: bool,
    tool_preview_any: bool,
    tool_preview: &'a str,
    pending_tool_confirm: Option<&'a ToolCall>,
    active_tool_stream: Option<&'a ToolStreamState>,
    ctx_compact_main: bool,
    ctx_compact_dog: bool,
    mind_pulse: Option<PulseDir>,
    input_active: bool,
    diary_active: bool,
}

fn build_active_status(args: ActiveStatusArgs<'_>) -> Option<String> {
    let ActiveStatusArgs {
        now,
        retry_status,
        sending_until,
        mode,
        reveal_idx,
        streaming_has_content,
        tool_preview_any,
        tool_preview,
        pending_tool_confirm,
        active_tool_stream,
        ctx_compact_main,
        ctx_compact_dog,
        mind_pulse,
        input_active,
        diary_active,
    } = args;
    fn mind_name(kind: MindKind) -> &'static str {
        match kind {
            MindKind::Main => "Main",
            MindKind::Sub => "Memory",
        }
    }
    fn tool_name(tool: &str) -> String {
        match tool.trim() {
            "adb" => "ADB".to_string(),
            "bash" => "BASH".to_string(),
            "termux_api" => "Termux API".to_string(),
            "write_file" => "Write".to_string(),
            "apply_patch" => "Patch".to_string(),
            "read_file" => "Read".to_string(),
            "search" => "Search".to_string(),
            "list_dir" => "List".to_string(),
            "stat_file" => "Info".to_string(),
            "memory_add" => "MAdd".to_string(),
            "memory_check" => "MFind".to_string(),
            "memory_read" => "MRead".to_string(),
            "memory_edit" => "MEdit".to_string(),
            "mind_msg" => "Mind".to_string(),
            "context_compact" => "CtxCompact".to_string(),
            other => other.to_string(),
        }
    }

    if diary_active {
        return Some("Updating diary".to_string());
    }
    if let Some(status) = retry_status {
        return Some(status.to_string());
    }
    if sending_until.is_some_and(|t| now < t) {
        return Some("Sending…".to_string());
    }
    if let Some(dir) = mind_pulse {
        let s = match dir {
            PulseDir::MainToDog => "Linking Main → Memory",
            PulseDir::DogToMain => "Linking Memory → Main",
        };
        return Some(s.to_string());
    }
    if ctx_compact_main || ctx_compact_dog {
        return Some("Compacting context".to_string());
    }
    if let Some(call) = pending_tool_confirm {
        return Some(format!("Approve tool: {}", tool_name(&call.tool)));
    }
    if let Some(state) = active_tool_stream {
        let who = mind_name(state.owner);
        return Some(format!("{who} tool: {}", tool_name(&state.call.tool)));
    }
    if tool_preview_any {
        if let Some(name) = extract_tool_name_hint(tool_preview) {
            return Some(format!("Tool: {}", tool_name(&name)));
        }
        return Some("Tool…".to_string());
    }
    if streaming_has_content || reveal_idx.is_some() {
        return Some("Streaming…".to_string());
    }
    match mode {
        Mode::Generating => Some("Thinking…".to_string()),
        Mode::ExecutingTool => Some("Running tool…".to_string()),
        Mode::ApprovingTool => Some("Approve tool".to_string()),
        _ => {
            if input_active {
                Some("Typing…".to_string())
            } else {
                None
            }
        }
    }
}

fn settings_field_hint(section: SettingsSection, kind: SettingsFieldKind) -> &'static str {
    match (section, kind) {
        (SettingsSection::PromptCenter, SettingsFieldKind::DogPrompt) => "Edit DOG prompt",
        (SettingsSection::PromptCenter, SettingsFieldKind::MainPrompt) => "Edit MAIN prompt",
        (SettingsSection::PromptCenter, SettingsFieldKind::ContextMainPrompt) => {
            "Edit MAIN context prompt"
        }
        (SettingsSection::PromptCenter, SettingsFieldKind::ContextCompactPrompt) => {
            "Edit context compact prompt"
        }
        _ => match kind {
            SettingsFieldKind::Provider => "Select provider",
            SettingsFieldKind::BaseUrl => "API base URL",
            SettingsFieldKind::ApiKey => "API key",
            SettingsFieldKind::Model => "Model",
            SettingsFieldKind::Temperature => "Temperature (0~2)",
            SettingsFieldKind::MaxTokens => "Max tokens",
            SettingsFieldKind::ContextK => "Context compaction threshold (k)",
            SettingsFieldKind::HeartbeatMinutes => "Heartbeat interval (min, 0=off)",
            SettingsFieldKind::SseEnabled => "Toggle SSE",
            SettingsFieldKind::ContextMaxTokens => "Dynamic context threshold (tokens)",
            SettingsFieldKind::ContextPoolMaxItems => "Summary pool size",
            SettingsFieldKind::ChatTarget => "Switch model (Main/Memory)",
            SettingsFieldKind::ExecPermission => "Tool execution permission (safe/full)",
            SettingsFieldKind::DogPrompt => "Edit DOG prompt",
            SettingsFieldKind::MainPrompt => "Edit MAIN prompt",
            SettingsFieldKind::ContextMainPrompt => "Edit context prompt",
            SettingsFieldKind::ContextCompactPrompt => "Edit context compact prompt",
        },
    }
}

struct RejectPasteLikeInputArgs<'a> {
    now: Instant,
    max_input_chars: usize,
    toast: &'a mut Option<(Instant, String)>,
    config: &'a AppConfig,
    paste_drop_until: &'a mut Option<Instant>,
    paste_guard_until: &'a mut Option<Instant>,
    burst_count: &'a mut usize,
    burst_last_at: &'a mut Option<Instant>,
    burst_started_at: &'a mut Option<Instant>,
    burst_start_cursor: &'a mut Option<usize>,
}

fn reject_paste_like_input(args: RejectPasteLikeInputArgs<'_>) {
    let RejectPasteLikeInputArgs {
        now,
        max_input_chars,
        toast,
        config,
        paste_drop_until,
        paste_guard_until,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
    } = args;
    set_input_limit_toast(now, max_input_chars, toast, true);
    arm_paste_block(now, config, paste_drop_until, paste_guard_until);
    reset_paste_burst(
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
    );
}

struct FinalizePasteCaptureArgs<'a> {
    force: bool,
    now: Instant,
    config: &'a AppConfig,
    paste_capture: &'a mut Option<PasteCapture>,
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    pending_pastes: &'a mut Vec<(String, String)>,
    toast: &'a mut Option<(Instant, String)>,
    max_input_chars: usize,
    burst_count: &'a mut usize,
    burst_last_at: &'a mut Option<Instant>,
    burst_started_at: &'a mut Option<Instant>,
    burst_start_cursor: &'a mut Option<usize>,
    paste_drop_until: &'a mut Option<Instant>,
    paste_guard_until: &'a mut Option<Instant>,
}

fn finalize_paste_capture_and_handle(
    args: FinalizePasteCaptureArgs<'_>,
) -> input::PasteFinalizeResult {
    let FinalizePasteCaptureArgs {
        force,
        now,
        config,
        paste_capture,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
        paste_drop_until,
        paste_guard_until,
    } = args;
    let finalize = maybe_finalize_paste_capture(input::MaybeFinalizePasteCaptureArgs {
        force,
        now,
        capture: paste_capture,
        input,
        cursor,
        input_chars,
        pending: pending_pastes,
        toast,
        per_paste_line_threshold: config.paste_placeholder_line_threshold,
        per_paste_char_threshold: config.paste_placeholder_char_threshold,
        max_input_chars,
        flush_gap_ms: config.paste_capture_flush_gap_ms,
    });
    if finalize.flushed {
        reset_paste_burst(
            burst_count,
            burst_last_at,
            burst_started_at,
            burst_start_cursor,
        );
    }
    if finalize.rejected {
        arm_paste_block(now, config, paste_drop_until, paste_guard_until);
    }
    finalize
}

struct FlushPasteCaptureOverflowArgs<'a> {
    now: Instant,
    config: &'a AppConfig,
    paste_capture: &'a mut Option<PasteCapture>,
    paste_capture_max_bytes: usize,
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    pending_pastes: &'a mut Vec<(String, String)>,
    toast: &'a mut Option<(Instant, String)>,
    max_input_chars: usize,
    burst_count: &'a mut usize,
    burst_last_at: &'a mut Option<Instant>,
    burst_started_at: &'a mut Option<Instant>,
    burst_start_cursor: &'a mut Option<usize>,
    paste_drop_until: &'a mut Option<Instant>,
    paste_guard_until: &'a mut Option<Instant>,
}

fn flush_paste_capture_if_overflow(args: FlushPasteCaptureOverflowArgs<'_>) -> bool {
    let FlushPasteCaptureOverflowArgs {
        now,
        config,
        paste_capture,
        paste_capture_max_bytes,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
        paste_drop_until,
        paste_guard_until,
    } = args;
    if paste_capture
        .as_ref()
        .is_none_or(|c| c.buf.len() <= paste_capture_max_bytes)
    {
        return false;
    }
    if let Some(c) = paste_capture.as_mut() {
        truncate_to_max_bytes(&mut c.buf, paste_capture_max_bytes);
    }
    finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
        force: true,
        now,
        config,
        paste_capture,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
        paste_drop_until,
        paste_guard_until,
    });
    arm_paste_block(now, config, paste_drop_until, paste_guard_until);
    true
}

struct InlineInsertArgs<'a> {
    now: Instant,
    config: &'a AppConfig,
    paste_capture_max_bytes: usize,
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    pending_pastes: &'a mut Vec<(String, String)>,
    toast: &'a mut Option<(Instant, String)>,
    max_input_chars: usize,
    burst_count: &'a mut usize,
    burst_last_at: &'a mut Option<Instant>,
    burst_started_at: &'a mut Option<Instant>,
    burst_start_cursor: &'a mut Option<usize>,
    paste_capture: &'a mut Option<PasteCapture>,
    paste_drop_until: &'a mut Option<Instant>,
    paste_guard_until: &'a mut Option<Instant>,
    command_menu_suppress: &'a mut bool,
}

fn handle_inline_insert<F>(args: InlineInsertArgs<'_>, insert_fn: F)
where
    F: FnOnce(&mut String, &mut usize, &mut usize, usize) -> bool,
{
    let InlineInsertArgs {
        now,
        config,
        paste_capture_max_bytes,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
        paste_capture,
        paste_drop_until,
        paste_guard_until,
        command_menu_suppress,
    } = args;
    *cursor = snap_cursor_out_of_placeholder(input, pending_pastes, *cursor);
    update_paste_burst(
        now,
        burst_last_at,
        burst_started_at,
        burst_count,
        burst_start_cursor,
        *cursor,
    );
    let fast_burst = *burst_count >= 2;
    let paste_like =
        is_paste_like_activity(now, *burst_last_at, *burst_started_at, *burst_count) || fast_burst;
    if paste_like {
        *paste_guard_until = Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
    }

    if !can_accept_more(input, pending_pastes, *input_chars, 1, max_input_chars) {
        if paste_like {
            reject_paste_like_input(RejectPasteLikeInputArgs {
                now,
                max_input_chars,
                toast,
                config,
                paste_drop_until,
                paste_guard_until,
                burst_count,
                burst_last_at,
                burst_started_at,
                burst_start_cursor,
            });
        } else {
            set_input_limit_toast(now, max_input_chars, toast, false);
        }
        return;
    }
    if !insert_fn(input, cursor, input_chars, max_input_chars) {
        if paste_like {
            reject_paste_like_input(RejectPasteLikeInputArgs {
                now,
                max_input_chars,
                toast,
                config,
                paste_drop_until,
                paste_guard_until,
                burst_count,
                burst_last_at,
                burst_started_at,
                burst_start_cursor,
            });
        } else {
            set_input_limit_toast(now, max_input_chars, toast, false);
        }
    } else {
        *command_menu_suppress = false;
    }

    maybe_begin_paste_capture(input::MaybeBeginPasteCaptureArgs {
        now,
        capture: paste_capture,
        input,
        cursor,
        input_chars,
        toast,
        burst_count: *burst_count,
        burst_started_at: *burst_started_at,
        burst_start_cursor: *burst_start_cursor,
    });
    if flush_paste_capture_if_overflow(FlushPasteCaptureOverflowArgs {
        now,
        config,
        paste_capture,
        paste_capture_max_bytes,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        burst_count,
        burst_last_at,
        burst_started_at,
        burst_start_cursor,
        paste_drop_until,
        paste_guard_until,
    }) {
        return;
    }
    if paste_capture.is_some() {
        reset_paste_burst(
            burst_count,
            burst_last_at,
            burst_started_at,
            burst_start_cursor,
        );
    }
    prune_pending_pastes(input, pending_pastes);
}

struct InsertNewlineArgs<'a> {
    now: Instant,
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    pending_pastes: &'a mut Vec<(String, String)>,
    toast: &'a mut Option<(Instant, String)>,
    max_input_chars: usize,
    command_menu_suppress: Option<&'a mut bool>,
}

fn insert_newline_limited(args: InsertNewlineArgs<'_>) -> bool {
    let InsertNewlineArgs {
        now,
        input,
        cursor,
        input_chars,
        pending_pastes,
        toast,
        max_input_chars,
        command_menu_suppress,
    } = args;
    *cursor = snap_cursor_out_of_placeholder(input, pending_pastes, *cursor);
    if !can_accept_more(input, pending_pastes, *input_chars, 1, max_input_chars) {
        set_input_limit_toast(now, max_input_chars, toast, false);
        return false;
    }
    if !try_insert_str_limited(input, cursor, "\n", input_chars, max_input_chars) {
        set_input_limit_toast(now, max_input_chars, toast, false);
    } else if let Some(menu) = command_menu_suppress {
        *menu = false;
    }
    prune_pending_pastes(input, pending_pastes);
    true
}

fn estimate_tokens(text: &str) -> usize {
    let bytes = text.len();
    (bytes.saturating_add(3)) / 4
}

fn calc_pct(used: usize, limit: usize) -> u8 {
    if limit == 0 {
        return 0;
    }
    let pct = used.saturating_mul(100) / limit;
    pct.min(100) as u8
}

fn build_cursor_map(text: &str, width: usize) -> Vec<(usize, usize, usize)> {
    let width = width.max(1);
    let mut map = Vec::with_capacity(text.chars().count().saturating_add(1));
    let mut x: usize = 0;
    let mut y: usize = 0;
    map.push((0, 0, 0));
    for (idx, ch) in text.char_indices() {
        if ch == '\n' {
            y = y.saturating_add(1);
            x = 0;
            map.push((idx.saturating_add(ch.len_utf8()), x, y));
            continue;
        }
        let w = UnicodeWidthChar::width(ch).unwrap_or(1);
        if x.saturating_add(w) > width {
            y = y.saturating_add(1);
            x = 0;
        }
        x = x.saturating_add(w);
        if x >= width {
            y = y.saturating_add(1);
            x = 0;
        }
        map.push((idx.saturating_add(ch.len_utf8()), x, y));
    }
    map
}

fn cursor_xy_from_map(map: &[(usize, usize, usize)], cursor: usize) -> (usize, usize) {
    for (idx, x, y) in map {
        if *idx == cursor {
            return (*x, *y);
        }
    }
    map.last().map(|(_, x, y)| (*x, *y)).unwrap_or((0, 0))
}

fn cursor_index_for_xy(map: &[(usize, usize, usize)], target_x: usize, target_y: usize) -> usize {
    let mut candidate: Option<usize> = None;
    for (idx, x, y) in map {
        if *y < target_y {
            continue;
        }
        if *y > target_y {
            break;
        }
        if *x <= target_x {
            candidate = Some(*idx);
            continue;
        }
        return candidate.unwrap_or(*idx);
    }
    candidate
        .or_else(|| map.last().map(|(idx, _, _)| *idx))
        .unwrap_or(0)
}

fn move_prompt_cursor_vertical(text: &str, width: usize, cursor: usize, delta: i32) -> usize {
    if width == 0 || delta == 0 {
        return cursor;
    }
    let cursor = cursor.min(text.len());
    let map = build_cursor_map(text, width);
    let (cx, cy) = cursor_xy_from_map(&map, cursor);
    let target_y = if delta.is_negative() {
        cy.saturating_sub(delta.unsigned_abs() as usize)
    } else {
        cy.saturating_add(delta as usize)
    };
    cursor_index_for_xy(&map, cx, target_y)
}

fn build_api_url(base: &str, path: &str) -> String {
    let base = base.trim_end_matches('/');
    let path = path.trim_start_matches('/');
    format!("{base}/{path}")
}

fn compact_ws_inline(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn extract_internal_tool_echo_blocks(text: &str) -> (String, Vec<String>) {
    fn is_header(line: &str) -> bool {
        let l = line.trim_start();
        (l.starts_with("● Ran CMD") || l.starts_with("○ Ran CMD"))
            || (l.starts_with("● Tool result") || l.starts_with("○ Tool result"))
    }

    fn is_continuation(line: &str) -> bool {
        let l = line.trim_start();
        l.starts_with('↳')
            || l.starts_with('┆')
            || l.starts_with("meta:")
            || l.starts_with("output:")
            || l.starts_with("input:")
            || l.starts_with("explain:")
            || l.starts_with("状态:")
            || l.starts_with("Tool result:")
    }

    let mut assistant_lines: Vec<String> = Vec::new();
    let mut blocks: Vec<String> = Vec::new();
    let mut cur_block: Vec<String> = Vec::new();
    let mut in_block = false;

    let flush_block = |blocks: &mut Vec<String>, cur: &mut Vec<String>| {
        if cur.is_empty() {
            return;
        }
        while cur.last().is_some_and(|l| l.trim().is_empty()) {
            cur.pop();
        }
        if !cur.is_empty() {
            blocks.push(cur.join("\n").trim_end().to_string());
        }
        cur.clear();
    };

    for raw in text.lines() {
        let line = raw.trim_end();
        if !in_block {
            if is_header(line) {
                in_block = true;
                cur_block.push(line.to_string());
            } else {
                assistant_lines.push(line.to_string());
            }
            continue;
        }

        if line.trim().is_empty() {
            flush_block(&mut blocks, &mut cur_block);
            in_block = false;
            continue;
        }
        if is_header(line) {
            flush_block(&mut blocks, &mut cur_block);
            cur_block.push(line.to_string());
            in_block = true;
            continue;
        }
        if is_continuation(line) {
            cur_block.push(line.to_string());
            continue;
        }
        flush_block(&mut blocks, &mut cur_block);
        in_block = false;
        assistant_lines.push(line.to_string());
    }
    if in_block {
        flush_block(&mut blocks, &mut cur_block);
    }

    while assistant_lines.last().is_some_and(|l| l.trim().is_empty()) {
        assistant_lines.pop();
    }
    (assistant_lines.join("\n").trim().to_string(), blocks)
}

fn wrap_tool_echo_block_as_tool_message(block: &str) -> String {
    let mut cleaned_lines: Vec<String> = Vec::new();
    let mut status: Option<String> = None;
    for raw in block.lines() {
        let line = raw.trim_end();
        let trimmed = line.trim_start();
        let stripped = if let Some(rest) = trimmed.strip_prefix("● ") {
            rest
        } else if let Some(rest) = trimmed.strip_prefix("○ ") {
            rest
        } else {
            line
        };
        if status.is_none() {
            if let Some(pos) = stripped.find("状态:") {
                let rest = stripped[pos + "状态:".len()..].trim();
                let tok = rest
                    .split(|c: char| c.is_whitespace() || c == '|' || c == ',')
                    .next()
                    .unwrap_or("")
                    .trim();
                if !tok.is_empty() {
                    status = Some(tok.to_string());
                }
            }
        }
        cleaned_lines.push(stripped.to_string());
    }
    let mut cleaned = cleaned_lines.join("\n").trim_end().to_string();
    truncate_to_max_bytes(&mut cleaned, 24_000);
    let meta = status
        .map(|s| format!("状态:{s}"))
        .unwrap_or_else(|| "状态:unknown".to_string());
    format!(
        "操作: TOOL_ECHO\nexplain: 解析到内部工具回显（已从正文剥离）\noutput:\n```text\n{cleaned}\n```\nmeta:\n```text\n{meta}\n```\n"
    )
}

fn load_prompt(path: &Path) -> anyhow::Result<String> {
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("读取提示词失败: {}", path.display()))?;
    Ok(text.trim().to_string())
}

fn truncate_chars_safe(text: &str, max_chars: usize) -> String {
    if max_chars == 0 {
        return String::new();
    }
    let mut end = text.len();
    let mut count = 0usize;
    for (idx, _) in text.char_indices() {
        if count >= max_chars {
            end = idx;
            break;
        }
        count = count.saturating_add(1);
    }
    if end >= text.len() {
        return text.to_string();
    }
    let mut out = text[..end].to_string();
    out.push('…');
    out
}

fn read_fastmemo_for_context() -> String {
    // fastmemo 文件由记忆工具维护；这里仅做只读注入。
    let path = Path::new("memory/fastmemo.jsonl");
    // 结构自愈：避免 fastmemo v1/脏结构进入上下文，导致后续 memory_add/edit 逻辑出现偏差。
    let _ = crate::mcp::ensure_memory_file("fastmemo", FASTMEMO_PATH);
    let text = std::fs::read_to_string(path).unwrap_or_default();
    truncate_chars_safe(text.trim(), 1800)
}

const FASTMEMO_PATH: &str = "memory/fastmemo.jsonl";

fn fastmemo_dynamic_count_and_any_ge10(text: &str) -> (usize, bool) {
    let mut current: Option<&str> = None;
    let mut self_n = 0usize;
    let mut user_n = 0usize;
    let mut env_n = 0usize;
    let mut hist_n = 0usize;
    let mut dyn_n = 0usize;
    for line in text.lines() {
        let t = line.trim();
        if t.starts_with('[') && t.contains(']') {
            current = match t {
                "[自我感知]" => Some("自我感知"),
                "[用户感知]" => Some("用户感知"),
                "[环境感知]" => Some("环境感知"),
                "[历史感知]" => Some("历史感知"),
                "[动态context池]" => Some("动态context池"),
                _ => None,
            };
            continue;
        }
        if !t.starts_with("- ") {
            continue;
        }
        match current {
            Some("自我感知") => self_n += 1,
            Some("用户感知") => user_n += 1,
            Some("环境感知") => env_n += 1,
            Some("历史感知") => hist_n += 1,
            Some("动态context池") => dyn_n += 1,
            _ => {}
        }
    }
    let any_ge10 = self_n >= 10 || user_n >= 10 || env_n >= 10 || hist_n >= 10 || dyn_n >= 10;
    (dyn_n, any_ge10)
}

fn append_fastmemo_dynamic_pool_item(raw: &str) -> anyhow::Result<(usize, bool)> {
    crate::mcp::ensure_memory_file("fastmemo", FASTMEMO_PATH)?;
    let mut lines: Vec<String> = std::fs::read_to_string(FASTMEMO_PATH)
        .unwrap_or_default()
        .lines()
        .map(|s| s.to_string())
        .collect();
    if lines.is_empty() {
        // 兜底：若文件异常为空，至少补全 v2 section 结构，避免后续写入失败。
        lines.push("fastmemo v2 | max_chars: 1800".to_string());
        lines.push(String::new());
        for sec in [
            "自我感知",
            "用户感知",
            "环境感知",
            "历史感知",
            "动态context池",
        ] {
            lines.push(format!("[{sec}]"));
            lines.push(String::new());
        }
    }

    let header = "[动态context池]";
    let header_idx = if let Some(pos) = lines.iter().position(|l| l.trim() == header) {
        pos
    } else {
        if !lines.last().is_some_and(|l| l.trim().is_empty()) {
            lines.push(String::new());
        }
        lines.push(header.to_string());
        lines.push(String::new());
        lines.len().saturating_sub(2)
    };
    let mut next_header_idx = None;
    for (idx, line) in lines.iter().enumerate().skip(header_idx + 1) {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.contains(']') {
            next_header_idx = Some(idx);
            break;
        }
    }
    let end_idx = next_header_idx.unwrap_or(lines.len());
    let mut insert_idx = end_idx;
    while insert_idx > header_idx + 1 && lines[insert_idx - 1].trim().is_empty() {
        insert_idx = insert_idx.saturating_sub(1);
    }
    let merged = compact_ws_inline(raw.trim());
    if merged.is_empty() {
        let text = std::fs::read_to_string(FASTMEMO_PATH).unwrap_or_default();
        return Ok(fastmemo_dynamic_count_and_any_ge10(&text));
    }
    let mut bullet = merged;
    if bullet.chars().count() > 420 {
        bullet = bullet.chars().take(420).collect::<String>();
        bullet.push('…');
    }
    let mut injected: Vec<String> = vec![format!("- {bullet}")];
    if insert_idx < lines.len() && !lines[insert_idx].trim().is_empty() {
        injected.push(String::new());
    }
    lines.splice(insert_idx..insert_idx, injected);
    let mut out = lines.join("\n");
    if !out.ends_with('\n') {
        out.push('\n');
    }
    std::fs::write(FASTMEMO_PATH, &out).context("写入 fastmemo 失败")?;
    Ok(fastmemo_dynamic_count_and_any_ge10(&out))
}

#[derive(Debug, Clone, Serialize)]
struct ApiMessage {
    role: String,
    content: String,
}

#[derive(Debug, Serialize)]
struct DeepseekRequest<'a> {
    model: &'a str,
    messages: &'a [ApiMessage],
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f32>,
    stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    stream_options: Option<StreamOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
}

#[derive(Debug, Serialize)]
struct StreamOptions {
    include_usage: bool,
}

#[derive(Debug, Clone)]
struct DogState {
    messages: Vec<ApiMessage>,
    prompt: String,
    prompt_reinject_pct: u8,
    last_prompt_inject_user: usize,
    user_count: usize,
    used_tokens_est: usize,
    last_usage_total_tokens: u64,
    fastmemo_idx: Option<usize>,
    ctx_compact_pending: bool,
    ctx_compact_inflight: bool,
    fastmemo_compact_pending: bool,
    include_tool_context: bool,
}

impl DogState {
    fn new(prompt: String, prompt_reinject_pct: u8) -> Self {
        let mut s = Self {
            messages: Vec::new(),
            prompt,
            prompt_reinject_pct,
            last_prompt_inject_user: 0,
            user_count: 0,
            used_tokens_est: 0,
            last_usage_total_tokens: 0,
            fastmemo_idx: None,
            ctx_compact_pending: false,
            ctx_compact_inflight: false,
            fastmemo_compact_pending: false,
            include_tool_context: true,
        };
        s.inject_prompt();
        s
    }

    fn inject_prompt(&mut self) {
        let prompt = self.prompt.trim();
        if prompt.is_empty() {
            return;
        }
        self.push_message("system", prompt.to_string());
    }

    fn push_message(&mut self, role: &str, content: String) {
        let clean = content.trim();
        if clean.is_empty() {
            return;
        }
        // DeepSeek 会拒绝连续 assistant 消息（400: Invalid consecutive assistant message）。
        // 工具链/自动续写等场景可能造成 assistant 连续出现；这里做最小合并，保持请求合法。
        if role == "assistant"
            && let Some(last) = self.messages.last_mut()
            && last.role == "assistant"
        {
            let old = estimate_tokens(&last.content);
            if !last.content.trim_end().is_empty() {
                last.content.push_str("\n\n");
            }
            last.content.push_str(clean);
            let new = estimate_tokens(&last.content);
            if new >= old {
                self.used_tokens_est = self.used_tokens_est.saturating_add(new - old);
            } else {
                self.used_tokens_est = self.used_tokens_est.saturating_sub(old - new);
            }
            return;
        }

        let clean = clean.to_string();
        self.used_tokens_est = self.used_tokens_est.saturating_add(estimate_tokens(&clean));
        self.messages.push(ApiMessage {
            role: role.to_string(),
            content: clean,
        });
    }

    fn push_user(&mut self, text: &str, limit: usize) -> bool {
        self.user_count = self.user_count.saturating_add(1);
        let added = estimate_tokens(text);
        let projected = self.used_tokens_est.saturating_add(added);
        let reinject_now = !self.prompt.trim().is_empty()
            && calc_pct(projected, limit) >= self.prompt_reinject_pct
            && self.last_prompt_inject_user != self.user_count;
        if reinject_now {
            self.inject_prompt();
            self.last_prompt_inject_user = self.user_count;
        }
        self.push_message("user", text.trim().to_string());
        reinject_now
    }

    fn push_assistant(&mut self, text: &str) {
        self.push_message("assistant", text.trim().to_string());
    }

    fn push_system(&mut self, text: &str) {
        self.push_message("system", text.trim().to_string());
    }

    fn upsert_system_message(&mut self, idx: &mut Option<usize>, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            return;
        }
        if let Some(pos) = *idx {
            if let Some(entry) = self.messages.get_mut(pos) {
                let old = estimate_tokens(&entry.content);
                entry.content = clean.to_string();
                let new = estimate_tokens(&entry.content);
                if new >= old {
                    self.used_tokens_est = self.used_tokens_est.saturating_add(new - old);
                } else {
                    self.used_tokens_est = self.used_tokens_est.saturating_sub(old - new);
                }
                return;
            }
            *idx = None;
        }
        let pos = self.messages.len();
        self.push_system(clean);
        *idx = Some(pos);
    }

	    fn push_tool(&mut self, text: &str) {
	        if !self.include_tool_context {
	            return;
	        }
	        let clean = text.trim();
	        if clean.is_empty() {
	            return;
	        }
	        // DeepSeek chat/completions 没有 tool role：用 assistant 承载 tool 结果。
	        // “伪 user 占位”只在 message_snapshot() 里按需追加，避免污染历史与 token 预算。
	        self.push_message("assistant", format!("Tool result:\n{clean}"));
	    }

    fn set_last_usage_total(&mut self, tokens: u64) {
        if tokens > 0 {
            self.last_usage_total_tokens = tokens;
        }
    }

    fn reset_context(&mut self) {
        self.messages.clear();
        self.used_tokens_est = 0;
        self.user_count = 0;
        self.last_prompt_inject_user = 0;
        self.last_usage_total_tokens = 0;
        self.fastmemo_idx = None;
        self.ctx_compact_pending = false;
        self.ctx_compact_inflight = false;
        self.fastmemo_compact_pending = false;
        self.inject_prompt();
    }

    fn push_ctx_pool_item(&mut self, _sys_cfg: &SystemConfig, item: &str) {
        if let Ok((_dyn_n, pending)) = append_fastmemo_dynamic_pool_item(item) {
            if pending {
                self.fastmemo_compact_pending = true;
            }
        }
    }

	    fn message_snapshot(&self, extra_system: Option<&str>) -> Vec<ApiMessage> {
	        let mut out = self.messages.clone();
	        if let Some(extra) = extra_system {
	            let clean = extra.trim();
	            if !clean.is_empty() {
                // DeepSeek 对“最后一条消息的 role”很敏感：若最后是 assistant（或 system），
                // 可能报 400（Invalid consecutive assistant message）。这里将附加指令以 user 形式注入，
                // 且文本本身以“系统：...”开头，保留“系统指令”语义。
	                out.push(ApiMessage {
	                    role: "user".to_string(),
	                    content: clean.to_string(),
	                });
	            }
	        }
	        let mut out = normalize_messages_for_deepseek(&out);
	        let need_tick = match out.last() {
	            None => true,
	            Some(m) => m.role.trim() != "user" || m.content.trim().is_empty(),
	        };
	        if need_tick {
	            out.push(ApiMessage {
	                role: "user".to_string(),
	                content: DEEPSEEK_TOOL_LOOP_TICK_USER.to_string(),
	            });
	        }
	        out
	    }

    fn set_include_tool_context(&mut self, enabled: bool) {
        self.include_tool_context = enabled;
    }

    fn estimate_total_tokens(&self) -> usize {
        self.messages
            .iter()
            .map(|m| estimate_tokens(&m.content))
            .sum()
    }

    fn estimate_chat_tokens(&self) -> usize {
        self.messages
            .iter()
            .filter(|m| m.role == "user" || m.role == "assistant")
            .map(|m| estimate_tokens(&m.content))
            .sum()
    }

    fn recent_chat_window(&self, rounds: usize) -> Vec<ApiMessage> {
        if rounds == 0 {
            return Vec::new();
        }
        let keep = rounds.saturating_mul(2).max(1);
        let chat: Vec<ApiMessage> = self
            .messages
            .iter()
            .filter(|m| (m.role == "user" || m.role == "assistant") && !m.content.trim().is_empty())
            .cloned()
            .collect();
        let start = chat.len().saturating_sub(keep);
        chat[start..].to_vec()
    }

    fn recalc_used_tokens_est(&mut self) {
        self.used_tokens_est = self.estimate_total_tokens();
    }

    fn refresh_fastmemo_system(&mut self, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            self.fastmemo_idx = None;
            return;
        }
        let body = format!("【表层记忆 fastmemo】\n{clean}");
        let mut idx = self.fastmemo_idx;
        self.upsert_system_message(&mut idx, &body);
        self.fastmemo_idx = idx;
    }

    fn arm_context_compact_if_needed(&mut self, sys_cfg: &SystemConfig) -> bool {
        if self.ctx_compact_pending || self.ctx_compact_inflight {
            return false;
        }
        let tokens = self.estimate_chat_tokens();
        if tokens >= sys_cfg.ctx_recent_max_tokens {
            let chat_total = self
                .messages
                .iter()
                .filter(|m| m.role == "user" || m.role == "assistant")
                .count();
            if chat_total <= CTX_KEEP_RECENT_ROUNDS.saturating_mul(2) {
                return false;
            }
            self.ctx_compact_pending = true;
            return true;
        }
        false
    }

    fn begin_context_compact(&mut self) -> bool {
        if self.ctx_compact_pending && !self.ctx_compact_inflight {
            self.ctx_compact_pending = false;
            self.ctx_compact_inflight = true;
            return true;
        }
        false
    }

    fn abort_context_compact(&mut self) {
        if self.ctx_compact_inflight {
            self.ctx_compact_inflight = false;
            self.ctx_compact_pending = true;
        }
    }

    fn apply_context_compact(&mut self, sys_cfg: &SystemConfig, summary: &str) -> (usize, usize) {
        let clean = summary.trim();
        let chat_tokens_before = self.estimate_chat_tokens();
        // 清空可压缩聊天记录：保留最近 3 轮对话 + 所有 system 头部（保持连贯性，避免彻底“断片”）。
        let kept_recent = self.recent_chat_window(CTX_KEEP_RECENT_ROUNDS);
        let mut kept: Vec<ApiMessage> = self
            .messages
            .iter()
            .filter(|m| m.role == "system")
            .cloned()
            .collect();
        if !kept_recent.is_empty() {
            kept.extend(kept_recent);
        }
        self.messages = kept;
        self.ctx_compact_inflight = false;
        self.recalc_used_tokens_est();
        let chat_tokens_after = self.estimate_chat_tokens();
        let cleared = chat_tokens_before.saturating_sub(chat_tokens_after);
        let mut pool_len = 0usize;
        if !clean.is_empty() {
            if let Ok((dyn_n, pending)) = append_fastmemo_dynamic_pool_item(clean) {
                pool_len = dyn_n;
                if pending {
                    self.fastmemo_compact_pending = true;
                }
            }
        }
        let _ = sys_cfg;
        (cleared, pool_len)
    }
}

fn normalize_messages_for_deepseek(messages: &[ApiMessage]) -> Vec<ApiMessage> {
    let mut out: Vec<ApiMessage> = Vec::with_capacity(messages.len());
    for msg in messages {
        let role = msg.role.trim();
        let content = msg.content.trim();
        if role.is_empty() || content.is_empty() {
            continue;
        }
        if role == "assistant"
            && let Some(last) = out.last_mut()
            && last.role == "assistant"
        {
            if !last.content.trim_end().is_empty() {
                last.content.push_str("\n\n");
            }
            last.content.push_str(content);
            continue;
        }
        out.push(ApiMessage {
            role: role.to_string(),
            content: content.to_string(),
        });
    }
    out
}

#[derive(Debug, Clone)]
struct DogClient {
    http: reqwest::blocking::Client,
    cfg: DogApiConfig,
}

impl DogClient {
    fn new(cfg: DogApiConfig) -> anyhow::Result<Self> {
        let key = cfg.api_key.as_deref().unwrap_or("").trim().to_string();
        if key.is_empty() {
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        let http = reqwest::blocking::Client::builder()
            .build()
            .context("创建 HTTP 客户端失败")?;
        Ok(Self { http, cfg })
    }

    fn request_timeout_secs(&self, stream: bool) -> u64 {
        let base = self.cfg.timeout_secs.max(30);
        if stream {
            base.max(120)
        } else {
            base.max(120).saturating_add(120)
        }
    }

    fn post_deepseek(
        &self,
        req: &DeepseekRequest<'_>,
        key: &str,
        timeout_secs: u64,
    ) -> anyhow::Result<reqwest::blocking::Response> {
        let url = build_api_url(&self.cfg.base_url, "chat/completions");
        self.http
            .post(&url)
            .bearer_auth(key)
            .timeout(Duration::from_secs(timeout_secs))
            .json(req)
            .send()
            .context("请求 DeepSeek 失败")
            .and_then(|r| {
                let status = r.status();
                if !status.is_success() {
                    let body = r.text().unwrap_or_default();
                    let body = body.trim();
                    if body.is_empty() {
                        return Err(anyhow::anyhow!("DeepSeek 返回异常状态: {status}"));
                    }
                    return Err(anyhow::anyhow!(
                        "DeepSeek 返回异常状态: {status} | {}",
                        body
                    ));
                }
                Ok(r)
            })
    }

    fn send_chat_stream(
        &self,
        messages: Vec<ApiMessage>,
        tx: mpsc::Sender<AsyncEvent>,
        kind: MindKind,
        request_id: u64,
        cancel: Arc<AtomicBool>,
    ) -> anyhow::Result<()> {
        runlog_event(
            "INFO",
            "model.request.start",
            json!({
                "mind": mind_label(kind),
                "request_id": request_id,
                "stream": true,
                "provider": &self.cfg.provider,
                "base_url": &self.cfg.base_url,
                "model": &self.cfg.model,
                "temperature": self.cfg.temperature,
                "max_tokens": self.cfg.max_tokens,
                "timeout_secs": self.request_timeout_secs(true),
                "messages_len": messages.len(),
                "estimated_in_tokens": estimate_messages_in_tokens(&messages),
                "messages": &messages,
            }),
        );
        let key = self.cfg.api_key.as_deref().unwrap_or("").trim();
        if key.is_empty() {
            runlog_event(
                "ERROR",
                "model.request.aborted",
                json!({"mind": mind_label(kind), "request_id": request_id, "reason": "missing_api_key"}),
            );
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        let req = DeepseekRequest {
            model: &self.cfg.model,
            messages: &messages,
            temperature: self.cfg.temperature,
            stream: true,
            stream_options: Some(StreamOptions {
                include_usage: true,
            }),
            max_tokens: self.cfg.max_tokens,
        };
        let max_retries = 3usize;
        let mut retries_done = 0usize;
        let mut started = false;
        let mut last_err: Option<anyhow::Error> = None;

        loop {
            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            let resp = match self.post_deepseek(&req, key, self.request_timeout_secs(true)) {
                Ok(r) => r,
                Err(e) => {
                    runlog_event(
                        "WARN",
                        "model.request.retryable_error",
                        json!({
                            "mind": mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            if !started {
                let _ = tx.send(AsyncEvent::ModelStreamStart { kind, request_id });
                started = true;
            }

            let mut resp = resp;
            let mut reader = io::BufReader::new(&mut resp);
            let mut line = String::new();
            let mut usage = 0u64;
            let mut emitted = false;
            let stream_result: anyhow::Result<()> = (|| {
                loop {
                    if cancel.load(Ordering::SeqCst) {
                        return Ok(());
                    }
                    line.clear();
                    let bytes = reader.read_line(&mut line).context("读取流式响应失败")?;
                    if bytes == 0 {
                        break;
                    }
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    if !trimmed.starts_with("data:") {
                        continue;
                    }
                    let payload = trimmed.trim_start_matches("data:").trim();
                    if payload == "[DONE]" {
                        break;
                    }
                    let value: serde_json::Value = match serde_json::from_str(payload) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if let Some(total) = value
                        .get("usage")
                        .and_then(|u| u.get("total_tokens"))
                        .and_then(|v| v.as_u64())
                    {
                        usage = total;
                    }
                    if let Some(delta) = value
                        .get("choices")
                        .and_then(|c| c.get(0))
                        .and_then(|c| c.get("delta"))
                    {
                        let content = delta
                            .get("content")
                            .and_then(|v| v.as_str())
                            .unwrap_or("")
                            .to_string();
                        let reasoning = delta
                            .get("reasoning_content")
                            .and_then(|v| v.as_str())
                            .unwrap_or("")
                            .to_string();
                        if RUN_LOGGER.get().map(|l| l.stream_chunks).unwrap_or(false)
                            && (!content.is_empty() || !reasoning.is_empty())
                        {
                            runlog_event(
                                "DEBUG",
                                "model.stream.chunk",
                                json!({
                                    "mind": mind_label(kind),
                                    "request_id": request_id,
                                    "content": &content,
                                    "reasoning": &reasoning,
                                }),
                            );
                        }
                        if !content.is_empty() || !reasoning.is_empty() {
                            if cancel.load(Ordering::SeqCst) {
                                return Ok(());
                            }
                            emitted = true;
                            let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                content,
                                reasoning,
                                request_id,
                            });
                        }
                    }
                }
                Ok(())
            })();

            match stream_result {
                Ok(()) => {
                    if !cancel.load(Ordering::SeqCst) {
                        let _ = tx.send(AsyncEvent::ModelStreamEnd {
                            kind,
                            usage,
                            error: None,
                            request_id,
                        });
                    }
                    runlog_event(
                        "INFO",
                        "model.stream.end",
                        json!({
                            "mind": mind_label(kind),
                            "request_id": request_id,
                            "usage_total_tokens": usage,
                            "emitted_any_chunk": emitted,
                        }),
                    );
                    return Ok(());
                }
                Err(e) => {
                    runlog_event(
                        "ERROR",
                        "model.stream.read_error",
                        json!({
                            "mind": mind_label(kind),
                            "request_id": request_id,
                            "error": format!("{e:#}"),
                            "emitted_any_chunk": emitted,
                        }),
                    );
                    let _ = last_err.replace(e);
                    if !emitted && retries_done < max_retries {
                        retries_done = retries_done.saturating_add(1);
                        let _ = tx.send(AsyncEvent::ErrorRetry {
                            attempt: retries_done,
                            max: max_retries,
                            request_id,
                        });
                        std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                        continue;
                    }
                    break;
                }
            }
        }

        if cancel.load(Ordering::SeqCst) {
            return Ok(());
        }
        let msg = last_err
            .map(|e| format!("{e:#}"))
            .unwrap_or_else(|| "DeepSeek 请求失败".to_string());
        let _ = tx.send(AsyncEvent::ModelStreamEnd {
            kind,
            usage: 0,
            error: Some(msg),
            request_id,
        });
        Ok(())
    }

    fn send_chat(
        &self,
        messages: Vec<ApiMessage>,
        tx: mpsc::Sender<AsyncEvent>,
        kind: MindKind,
        request_id: u64,
        cancel: Arc<AtomicBool>,
    ) -> anyhow::Result<()> {
        runlog_event(
            "INFO",
            "model.request.start",
            json!({
                "mind": mind_label(kind),
                "request_id": request_id,
                "stream": false,
                "provider": &self.cfg.provider,
                "base_url": &self.cfg.base_url,
                "model": &self.cfg.model,
                "temperature": self.cfg.temperature,
                "max_tokens": self.cfg.max_tokens,
                "timeout_secs": self.request_timeout_secs(false),
                "messages_len": messages.len(),
                "estimated_in_tokens": estimate_messages_in_tokens(&messages),
                "messages": &messages,
            }),
        );
        let key = self.cfg.api_key.as_deref().unwrap_or("").trim();
        if key.is_empty() {
            runlog_event(
                "ERROR",
                "model.request.aborted",
                json!({"mind": mind_label(kind), "request_id": request_id, "reason": "missing_api_key"}),
            );
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        let req = DeepseekRequest {
            model: &self.cfg.model,
            messages: &messages,
            temperature: self.cfg.temperature,
            stream: false,
            stream_options: None,
            max_tokens: self.cfg.max_tokens,
        };
        let max_retries = 3usize;
        let mut retries_done = 0usize;
        let mut last_err: Option<anyhow::Error> = None;

        loop {
            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            let resp = match self.post_deepseek(&req, key, self.request_timeout_secs(false)) {
                Ok(r) => r,
                Err(e) => {
                    runlog_event(
                        "WARN",
                        "model.request.retryable_error",
                        json!({
                            "mind": mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            let value: serde_json::Value = match resp.json().context("解析 DeepSeek 响应失败")
            {
                Ok(v) => v,
                Err(e) => {
                    runlog_event(
                        "WARN",
                        "model.response.parse_error",
                        json!({
                            "mind": mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            let usage = value
                .get("usage")
                .and_then(|u| u.get("total_tokens"))
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            let message = value
                .get("choices")
                .and_then(|c| c.get(0))
                .and_then(|c| c.get("message"))
                .cloned()
                .unwrap_or(serde_json::Value::Null);
            let content = message
                .get("content")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            let reasoning = message
                .get("reasoning_content")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();

            runlog_event(
                "INFO",
                "model.response.json",
                json!({
                    "mind": mind_label(kind),
                    "request_id": request_id,
                    "usage_total_tokens": usage,
                    "raw": value,
                    "parsed": { "content": &content, "reasoning": &reasoning },
                }),
            );

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            let _ = tx.send(AsyncEvent::ModelStreamStart { kind, request_id });
            if !reasoning.is_empty() {
                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                    content: String::new(),
                    reasoning,
                    request_id,
                });
                std::thread::sleep(Duration::from_millis(120));
            }
            if !content.is_empty() {
                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                    content,
                    reasoning: String::new(),
                    request_id,
                });
            }
            if !cancel.load(Ordering::SeqCst) {
                let _ = tx.send(AsyncEvent::ModelStreamEnd {
                    kind,
                    usage,
                    error: None,
                    request_id,
                });
            }
            return Ok(());
        }

        if cancel.load(Ordering::SeqCst) {
            return Ok(());
        }
        let msg = last_err
            .map(|e| format!("{e:#}"))
            .unwrap_or_else(|| "DeepSeek 请求失败".to_string());
        let _ = tx.send(AsyncEvent::ModelStreamEnd {
            kind,
            usage: 0,
            error: Some(msg),
            request_id,
        });
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct TokenTotals {
    total_tokens: u64,
    #[serde(default)]
    context_tokens: u64,
    #[serde(default)]
    total_in_tokens: u64,
    #[serde(default)]
    total_out_tokens: u64,
    #[serde(default)]
    total_heartbeat_count: u64,
    #[serde(default)]
    total_heartbeat_responses: u64,
}

fn load_token_totals(path: &Path) -> TokenTotals {
    let data = std::fs::read_to_string(path).ok();
    data.and_then(|s| serde_json::from_str(&s).ok())
        .unwrap_or_default()
}

fn store_token_totals(path: &Path, totals: &TokenTotals) -> anyhow::Result<()> {
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir).ok();
    }
    let text = serde_json::to_string(totals).context("序列化 token 统计失败")?;
    std::fs::write(path, text).context("写入 token 统计失败")?;
    Ok(())
}

fn heartbeat_reply_text(mut content: String, thinking_full_text: &str) -> String {
    if !content.trim().is_empty() {
        return content;
    }
    let fallback = thinking_full_text.trim();
    if fallback.is_empty() {
        return content;
    }
    content.clear();
    content.push_str(fallback);
    content
}

fn backfill_heartbeat_totals_from_memo_db(db_path: &str) -> Option<(u64, u64)> {
    let conn = rusqlite::Connection::open(db_path).ok()?;
    let mut stmt = conn
        .prepare("SELECT speaker, content FROM metamemo ORDER BY id")
        .ok()?;
    let mut rows = stmt.query([]).ok()?;

    let mut heartbeats: u64 = 0;
    let mut responses: u64 = 0;
    let mut awaiting = false;
    while let Some(row) = rows.next().ok()? {
        let speaker: Option<String> = row.get(0).ok();
        let content: String = row.get(1).ok().unwrap_or_default();

        let speaker = speaker.unwrap_or_default();
        let is_user = speaker == "user" || speaker.starts_with("user:");
        let is_assistant_main = speaker == "assistant:main";
        let is_heartbeat = speaker == "system:main" && content.contains("心跳：");

        if is_heartbeat {
            heartbeats = heartbeats.saturating_add(1);
            awaiting = true;
            continue;
        }
        if !awaiting {
            continue;
        }
        if is_user {
            awaiting = false;
            continue;
        }
        if is_assistant_main {
            if !is_pass_message(&content) {
                responses = responses.saturating_add(1);
            }
            awaiting = false;
        }
    }
    Some((heartbeats, responses))
}

#[cfg(test)]
mod heartbeat_backfill_tests {
    use super::*;
    use std::fs;

    #[test]
    fn backfill_counts_heartbeats_and_non_pass_responses() {
        let path = std::env::temp_dir().join(format!(
            "ying_hb_backfill_test_{}_{}.db",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(1)
        ));
        let _ = fs::remove_file(&path);

        {
            let conn = rusqlite::Connection::open(&path).unwrap();
            conn.execute(
                "CREATE TABLE metamemo (\
                    id INTEGER PRIMARY KEY AUTOINCREMENT,\
                    ts TEXT NOT NULL,\
                    date TEXT NOT NULL,\
                    speaker TEXT,\
                    content TEXT NOT NULL\
                )",
                [],
            )
            .unwrap();
            let mut insert = conn
                .prepare(
                    "INSERT INTO metamemo (ts, date, speaker, content) VALUES (?1, ?2, ?3, ?4)",
                )
                .unwrap();

            insert
                .execute((
                    "2026-01-01 00:00:00",
                    "2026-01-01",
                    "system:main",
                    "心跳：t1 | idle",
                ))
                .unwrap();
            insert
                .execute(("2026-01-01 00:00:01", "2026-01-01", "assistant:main", "hi"))
                .unwrap();
            insert
                .execute((
                    "2026-01-01 00:00:02",
                    "2026-01-01",
                    "system:main",
                    "心跳：t2 | idle",
                ))
                .unwrap();
            insert
                .execute((
                    "2026-01-01 00:00:03",
                    "2026-01-01",
                    "assistant:main",
                    "[mainpass]",
                ))
                .unwrap();
            insert
                .execute((
                    "2026-01-01 00:00:04",
                    "2026-01-01",
                    "system:main",
                    "心跳：t3 | idle",
                ))
                .unwrap();
            insert
                .execute(("2026-01-01 00:00:05", "2026-01-01", "user", "hey"))
                .unwrap();
            insert
                .execute((
                    "2026-01-01 00:00:06",
                    "2026-01-01",
                    "assistant:main",
                    "later",
                ))
                .unwrap();
        }

        let (heartbeats, responses) =
            backfill_heartbeat_totals_from_memo_db(path.to_str().unwrap()).unwrap();
        assert_eq!((heartbeats, responses), (3, 1));

        let _ = fs::remove_file(&path);
    }
}

#[cfg(test)]
mod heartbeat_reply_tests {
    use super::*;

    #[test]
    fn uses_thinking_when_content_empty() {
        let out = heartbeat_reply_text(String::new(), "[mainpass]");
        assert_eq!(out, "[mainpass]");
    }

    #[test]
    fn prefers_content_when_present() {
        let out = heartbeat_reply_text("hi".to_string(), "reasoning");
        assert_eq!(out, "hi");
    }
}

#[cfg(test)]
mod message_normalize_tests {
    use super::*;

    #[test]
    fn merges_consecutive_assistant_messages_for_deepseek() {
        let mut state = DogState::new(String::new(), 80);
        state.messages.clear();
        state.used_tokens_est = 0;

        state.push_assistant("a");
        state.push_assistant("b");
        let assistants: Vec<&ApiMessage> = state
            .messages
            .iter()
            .filter(|m| m.role == "assistant")
            .collect();
        assert_eq!(assistants.len(), 1);
        assert!(assistants[0].content.contains("a"));
        assert!(assistants[0].content.contains("b"));
    }

    #[test]
    fn message_snapshot_normalizes_existing_consecutive_assistants() {
        let mut state = DogState::new(String::new(), 80);
        state.messages.clear();
        state.used_tokens_est = 0;
        state.messages.push(ApiMessage {
            role: "assistant".to_string(),
            content: "a".to_string(),
        });
        state.messages.push(ApiMessage {
            role: "assistant".to_string(),
            content: "b".to_string(),
        });
        let snap = state.message_snapshot(None);
        let assistants: Vec<&ApiMessage> = snap.iter().filter(|m| m.role == "assistant").collect();
        assert_eq!(assistants.len(), 1);
        assert!(assistants[0].content.contains("a"));
        assert!(assistants[0].content.contains("b"));
    }

	    #[test]
	    fn tool_context_user_placeholder_is_injected_at_snapshot() {
	        let mut state = DogState::new(String::new(), 80);
	        state.messages.clear();
	        state.used_tokens_est = 0;

	        state.push_tool("ok");
	        assert_eq!(state.messages.len(), 1);
	        assert_eq!(state.messages[0].role, "assistant");
	        assert!(state.messages[0].content.contains("Tool result:"));

	        let snap = state.message_snapshot(None);
	        assert_eq!(snap.len(), 2);
	        assert_eq!(snap[0].role, "assistant");
	        assert!(snap[0].content.contains("Tool result:"));
	        assert_eq!(snap[1].role, "user");
	        assert_eq!(snap[1].content, DEEPSEEK_TOOL_LOOP_TICK_USER);
	    }
	}

#[cfg(test)]
mod event_helpers_tests {
    use super::*;

    #[test]
    fn event_request_id_extracts_only_model_events() {
        assert_eq!(
            event_request_id(&AsyncEvent::ModelStreamStart {
                kind: MindKind::Main,
                request_id: 1
            }),
            Some(1)
        );
        assert_eq!(
            event_request_id(&AsyncEvent::ModelStreamChunk {
                content: String::new(),
                reasoning: String::new(),
                request_id: 2
            }),
            Some(2)
        );
        assert_eq!(
            event_request_id(&AsyncEvent::ModelStreamEnd {
                kind: MindKind::Main,
                usage: 0,
                error: None,
                request_id: 3
            }),
            Some(3)
        );
        assert_eq!(
            event_request_id(&AsyncEvent::ErrorRetry {
                attempt: 1,
                max: 2,
                request_id: 4
            }),
            Some(4)
        );
        assert_eq!(
            event_request_id(&AsyncEvent::ToolStreamEnd {
                outcome: ToolOutcome {
                    user_message: "noop".to_string(),
                    log_lines: vec![],
                },
                sys_msg: None,
            }),
            None
        );
    }

    #[test]
    fn should_ignore_request_requires_exact_match() {
        assert!(should_ignore_request(&None, 1));
        assert!(should_ignore_request(&Some(2), 1));
        assert!(!should_ignore_request(&Some(1), 1));
    }

    #[test]
    fn pulse_dir_maps_expected_pairs() {
        assert!(matches!(
            pulse_dir(MindKind::Main, MindKind::Sub),
            PulseDir::MainToDog
        ));
        assert!(matches!(
            pulse_dir(MindKind::Sub, MindKind::Main),
            PulseDir::DogToMain
        ));
    }
}

fn store_config_file<T: Serialize>(path: &Path, cfg: &T) -> anyhow::Result<()> {
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir).ok();
    }
    let text = serde_json::to_string_pretty(cfg).context("序列化配置失败")?;
    std::fs::write(path, text).context("写入配置失败")?;
    Ok(())
}

fn resolve_config_path_from_env(env_key: &str, default_path: &str) -> PathBuf {
    let raw = std::env::var(env_key)
        .ok()
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| default_path.to_string());
    let prefer_project = !PathBuf::from(&raw).is_absolute();
    resolve_config_path(&raw, prefer_project)
}

fn load_json_config<T>(path: &Path, label: &str) -> (T, Option<String>)
where
    T: Serialize + DeserializeOwned + Default,
{
    let mut err: Option<String> = None;
    let mut cfg = T::default();
    match fs::read_to_string(path) {
        Ok(text) => match serde_json::from_str::<T>(&text) {
            Ok(parsed) => cfg = parsed,
            Err(e) => {
                err = Some(format!("{label}解析失败: {e}"));
            }
        },
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                err = Some(format!("{label}不存在，已生成默认"));
            } else {
                err = Some(format!("{label}读取失败: {e}"));
            }
            let _ = store_config_file(path, &cfg);
        }
    }
    (cfg, err)
}

fn load_dog_api_config() -> (DogApiConfig, Option<String>, PathBuf) {
    let path_buf = resolve_config_path_from_env("YING_DOG_CONFIG", "config/dog_api.json");
    let (mut cfg, err) = load_json_config::<DogApiConfig>(&path_buf, "DOG 配置");
    if let Ok(key) =
        std::env::var("DEEPSEEK_API_KEY").or_else(|_| std::env::var("YING_DOG_API_KEY"))
        && !key.trim().is_empty()
    {
        cfg.api_key = Some(key);
    }
    normalize_common_api_fields(
        &mut cfg.provider,
        &mut cfg.temperature,
        &mut cfg.timeout_secs,
        &mut cfg.max_tokens,
    );
    if cfg.prompt_reinject_pct == 0 || cfg.prompt_reinject_pct > 100 {
        cfg.prompt_reinject_pct = 80;
    }
    cfg = normalize_dog_config_paths(cfg, &path_buf);
    (cfg, err, path_buf)
}

fn load_main_api_config() -> (MainApiConfig, Option<String>, PathBuf) {
    let path_buf = resolve_config_path_from_env("YING_MAIN_CONFIG", "config/main_api.json");
    let (mut cfg, err) = load_json_config::<MainApiConfig>(&path_buf, "MAIN 配置");
    if let Ok(key) =
        std::env::var("DEEPSEEK_API_KEY").or_else(|_| std::env::var("YING_MAIN_API_KEY"))
        && !key.trim().is_empty()
    {
        cfg.api_key = Some(key);
    }
    normalize_common_api_fields(
        &mut cfg.provider,
        &mut cfg.temperature,
        &mut cfg.timeout_secs,
        &mut cfg.max_tokens,
    );
    cfg = normalize_main_config_paths(cfg, &path_buf);
    (cfg, err, path_buf)
}

fn load_system_config() -> (SystemConfig, Option<String>, PathBuf) {
    let path_buf = resolve_config_path_from_env("YING_SYSTEM_CONFIG", "config/system.json");
    let (mut cfg, err) = load_json_config::<SystemConfig>(&path_buf, "系统配置");
    if cfg.context_k < 8 || cfg.context_k > 1024 {
        cfg.context_k = 128;
    }
    cfg.heartbeat_minutes = normalize_heartbeat_minutes(cfg.heartbeat_minutes);
    if cfg.ctx_recent_max_tokens < 500 || cfg.ctx_recent_max_tokens > 200_000 {
        cfg.ctx_recent_max_tokens = 3000;
    }
    if cfg.ctx_pool_max_items == 0 || cfg.ctx_pool_max_items > 50 {
        cfg.ctx_pool_max_items = 10;
    }
    if cfg.context_compact_prompt_path.trim().is_empty() {
        cfg.context_compact_prompt_path = "prompts/context_compact.txt".to_string();
    }
    cfg.chat_target = normalize_chat_target_value(&cfg.chat_target);
    (cfg, err, path_buf)
}

fn normalize_common_api_fields(
    provider: &mut String,
    temperature: &mut Option<f32>,
    timeout_secs: &mut u64,
    max_tokens: &mut Option<u32>,
) {
    if let Some(temp) = *temperature {
        if !(0.0..=1.5).contains(&temp) {
            *temperature = None;
        }
    } else {
        *temperature = Some(0.6);
    }
    if provider.trim().is_empty() {
        *provider = "deepseek".to_string();
    }
    if *timeout_secs < 5 {
        *timeout_secs = 60;
    }
    if let Some(tokens) = *max_tokens {
        if tokens == 0 {
            *max_tokens = Some(5_000);
        }
    } else {
        *max_tokens = Some(5_000);
    }
}

fn normalize_chat_target_value(raw: &str) -> String {
    let lower = raw.trim().to_ascii_lowercase();
    match lower.as_str() {
        "main" | "m" => "main".to_string(),
        "dog" | "sub" | "s" => "dog".to_string(),
        _ => "dog".to_string(),
    }
}

fn normalize_heartbeat_minutes(value: usize) -> usize {
    match value {
        0 | 5 | 10 | 30 | 60 => value,
        _ => 5,
    }
}

fn parse_chat_target(raw: &str) -> MindKind {
    if normalize_chat_target_value(raw) == "main" {
        MindKind::Main
    } else {
        MindKind::Sub
    }
}

fn reset_state_for_target(
    target: MindKind,
    dog_state: &mut DogState,
    main_state: &mut DogState,
    mind_ctx_idx_main: &mut Option<usize>,
    mind_ctx_idx_dog: &mut Option<usize>,
) {
    match target {
        MindKind::Main => {
            main_state.reset_context();
            *mind_ctx_idx_main = None;
        }
        MindKind::Sub => {
            dog_state.reset_context();
            *mind_ctx_idx_dog = None;
        }
    }
}

const HEARTBEAT_DEFER_SECS: u64 = 600;
// 关闭心跳时，把 next_heartbeat_at 推到足够远，避免极端情况下被其它逻辑误触发。
const HEARTBEAT_DISABLED_SECS: u64 = 60 * 60 * 24 * 365 * 10;

fn heartbeat_interval(minutes: usize) -> Duration {
    Duration::from_secs(minutes as u64 * 60)
}

fn defer_heartbeat(next_at: &mut Instant, now: Instant) {
    let defer_until = now + Duration::from_secs(HEARTBEAT_DEFER_SECS);
    if defer_until > *next_at {
        *next_at = defer_until;
    }
}

fn can_inject_heartbeat(
    mode: Mode,
    active_request_id: &Option<u64>,
    pending_tools: &VecDeque<ToolCall>,
    pending_tool_confirm: &Option<(ToolCall, String, usize)>,
    active_tool_stream: &Option<ToolStreamState>,
) -> bool {
    matches!(mode, Mode::Idle)
        && active_request_id.is_none()
        && pending_tools.is_empty()
        && pending_tool_confirm.is_none()
        && active_tool_stream.is_none()
}

fn try_termux_wake_lock(enable: bool) -> anyhow::Result<()> {
    let cmd = if enable {
        "termux-wake-lock"
    } else {
        "termux-wake-unlock"
    };
    let status = Command::new(cmd).status()?;
    if status.success() {
        Ok(())
    } else {
        Err(anyhow::anyhow!("{cmd} failed"))
    }
}

fn pulse_anim_state(
    now: Instant,
    started_at: Instant,
    frame_ms: u64,
) -> (ui::HeartbeatStyle, bool) {
    let frame_ms = frame_ms.max(16);
    let elapsed_ms = now.saturating_duration_since(started_at).as_millis() as u64;
    let frame = elapsed_ms / frame_ms;
    let total_frames = HEARTBEAT_CYCLE_FRAMES.saturating_mul(HEARTBEAT_CYCLES);
    if frame >= total_frames {
        return (
            ui::HeartbeatStyle {
                intensity: 1.0,
                visible: true,
            },
            true,
        );
    }
    let cycle_frame = frame % HEARTBEAT_CYCLE_FRAMES;
    let mut visible = true;
    let intensity;
    if cycle_frame < HEARTBEAT_FADE_FRAMES {
        let idx = cycle_frame + 1;
        intensity = 0.2 + 0.8 * (idx as f32 / HEARTBEAT_FADE_FRAMES as f32);
    } else if cycle_frame < HEARTBEAT_FADE_FRAMES + HEARTBEAT_BLINK_FRAMES {
        let idx = cycle_frame - HEARTBEAT_FADE_FRAMES;
        visible = idx.is_multiple_of(2);
        intensity = 1.0;
    } else if cycle_frame < HEARTBEAT_FADE_FRAMES + HEARTBEAT_BLINK_FRAMES + HEARTBEAT_FADE_FRAMES {
        let idx = cycle_frame - (HEARTBEAT_FADE_FRAMES + HEARTBEAT_BLINK_FRAMES) + 1;
        intensity = 1.0 - 0.8 * (idx as f32 / HEARTBEAT_FADE_FRAMES as f32);
    } else {
        let idx = cycle_frame
            - (HEARTBEAT_FADE_FRAMES + HEARTBEAT_BLINK_FRAMES + HEARTBEAT_FADE_FRAMES)
            + 1;
        intensity = 0.2 + 0.8 * (idx as f32 / HEARTBEAT_FADE_FRAMES as f32);
    }
    (
        ui::HeartbeatStyle {
            intensity: intensity.clamp(0.2, 1.0),
            visible,
        },
        false,
    )
}

fn is_pass_message(text: &str) -> bool {
    matches!(text.trim(), "[mainpass]" | "[dogpass]")
}

fn info_label_for_tool(tool: &str) -> &'static str {
    match tool {
        "memory_add" | "write_file" => "日记",
        "memory_check" | "memory_read" => "回忆",
        "memory_edit" => "记录",
        "bash" => "命令",
        "adb" | "termux_api" => "调试",
        _ => "工具",
    }
}

fn info_label_for_tool_preview(preview: &str) -> String {
    extract_tool_name_hint(preview)
        .map(|tool| info_label_for_tool(&tool).to_string())
        .unwrap_or_else(|| "工具".to_string())
}

fn format_tool_call_preview(call: &ToolCall) -> String {
    let mut out = Vec::new();
    out.push(format!("tool: {}", call.tool.trim()));
    if let Some(brief) = call
        .brief
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        out.push(format!("brief: {brief}"));
    }
    if !call.input.trim().is_empty() {
        out.push(format!("input: {}", call.input.trim()));
    }
    if let Some(path) = call
        .path
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        out.push(format!("path: {path}"));
    }
    if let Some(pattern) = call
        .pattern
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        out.push(format!("pattern: {pattern}"));
    }
    if let Some(root) = call
        .root
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        out.push(format!("root: {root}"));
    }
    if let Some(target) = call
        .target
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        out.push(format!("target: {target}"));
    }
    out.join("\n")
}

fn estimate_messages_in_tokens(messages: &[ApiMessage]) -> u64 {
    messages
        .iter()
        .map(|m| estimate_tokens(&m.content) as u64)
        .sum()
}

fn record_request_in_tokens(
    core: &mut Core,
    token_totals: &mut TokenTotals,
    token_total_path: &Path,
    context_usage: &ContextUsage,
    active_request_in_tokens: &mut Option<u64>,
    in_tokens: u64,
) {
    *active_request_in_tokens = Some(in_tokens);
    core.add_run_in_tokens(in_tokens);
    token_totals.total_in_tokens = token_totals.total_in_tokens.saturating_add(in_tokens);
    token_totals.total_tokens = token_totals
        .total_in_tokens
        .saturating_add(token_totals.total_out_tokens);
    token_totals.context_tokens = context_usage.tokens() as u64;
    let _ = store_token_totals(token_total_path, token_totals);
}

fn format_chat_target(raw: &str) -> String {
    if normalize_chat_target_value(raw) == "main" {
        "MAIN".to_string()
    } else {
        "MEMORY".to_string()
    }
}

fn store_prompt_file(path: &Path, text: &str) -> anyhow::Result<()> {
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建目录失败：{}", parent.display()))?;
    }
    fs::write(path, text).with_context(|| format!("写入提示词失败：{}", path.display()))?;
    Ok(())
}

fn load_prompt_file_with_default(
    path: &Path,
    default_text: &str,
    label: &str,
) -> (String, Option<String>) {
    match fs::read_to_string(path) {
        Ok(text) => (text.trim().to_string(), None),
        Err(e) => {
            let msg = if e.kind() == std::io::ErrorKind::NotFound {
                format!("{label} 提示词不存在，已生成默认")
            } else {
                format!("{label} 提示词读取失败: {e}")
            };
            let _ = store_prompt_file(path, default_text);
            (default_text.to_string(), Some(msg))
        }
    }
}

fn load_context_prompts() -> (ContextPromptConfig, Vec<String>, PathBuf) {
    let defaults = ContextPromptConfig::default();
    let main_path =
        resolve_config_path_from_env("YING_CTX_MAIN_PROMPT_PATH", "prompts/ctxmainprompt.txt");
    let mut errors = Vec::new();
    let (main_prompt, err_main) =
        load_prompt_file_with_default(&main_path, &defaults.main_prompt, "CONTEXT-MAIN");
    if let Some(err) = err_main {
        errors.push(err);
    }
    (ContextPromptConfig { main_prompt }, errors, main_path)
}

const DEFAULT_CONTEXT_COMPACT_PROMPT: &str = "系统：聊天窗口已达到动态压缩阈值（请严格按要求执行）。\n\n你将看到：提示词、fastmemo、动态摘要池、以及最近的聊天记录。\n\n任务：\n- 只压缩“本条用户消息之前”的聊天记录（不包含提示词/fastmemo/动态摘要池，也不包含本条用户消息）。\n- 仅压缩较旧聊天记录：保留最近 3 轮对话不写入压缩产物（用于保持连贯性）。\n- 压缩产物只需一段纯文本：简短描述聊了什么、记录关键事实/约束/进度/待办，避免失忆。\n- 压缩要与动态摘要池形成连续记忆：避免机械重复旧摘要，能增量更新就增量更新。\n\n输出格式：\n- 本轮只允许输出 1 条工具 JSON（不要附加说明/不要输出正文/不要调用其他工具）。\n- 工具名固定为 context_compact。\n\n工具 JSON 示例：\n<tool>{\"tool\":\"context_compact\",\"target\":\"{TARGET}\",\"content\":\"(你的压缩文本)\",\"brief\":\"↻ context summary\"}</tool>\n\n注意：target 必须是 {TARGET}（main 或 dog）。\n用户最新消息（不要写入压缩内容）：\n{USER}\n";

const CTX_KEEP_RECENT_ROUNDS: usize = 3;

fn load_context_compact_prompt(sys_cfg: &SystemConfig) -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path(&sys_cfg.context_compact_prompt_path, true);
    let (text, err) =
        load_prompt_file_with_default(&path, DEFAULT_CONTEXT_COMPACT_PROMPT, "CONTEXT-COMPACT");
    (text, err, path)
}

const DEFAULT_FASTMEMO_COMPACT_PROMPT: &str = "系统：fastmemo 已达到阈值，需要进行压缩（由 DOG 执行，静默维护）。\n\n任务：\n- 读取 fastmemo（优先使用 memory_read 读取 fastmemo 原文；若上下文已包含 fastmemo，也可直接使用）。\n- fastmemo 包含 5 个固定区域：[自我感知] [用户感知] [环境感知] [历史感知] [动态context池]\n- 你需要把每个区域的内容“迭代压缩”为 1 条 bullet（只保留长期稳定、可复用、关键的信息；不确定就丢弃或标注待确认）。\n- 禁止把工具输出原文/日志/大段代码写回 fastmemo；若需要引用，只写摘要+指针。\n\n输出格式（强制）：\n- 只允许输出工具 JSON（不要输出正文）。\n- 推荐流程：先 1 次 memory_read，然后依次对 5 个区域使用 memory_edit(section=..., content=...) 覆盖为 1 条 bullet。\n\n示例：\n<tool>{\"tool\":\"memory_read\",\"path\":\"fastmemo\",\"brief\":\"读取 fastmemo 原文\"}</tool>\n<tool>{\"tool\":\"memory_edit\",\"path\":\"fastmemo\",\"section\":\"自我感知\",\"content\":\"- ...\",\"brief\":\"压缩自我感知\"}</tool>\n\n注意：完成压缩后，不要再输出任何解释性文字。";

fn load_fastmemo_compact_prompt() -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path_from_env(
        "YING_FASTMEMO_COMPACT_PROMPT_PATH",
        "prompts/fastmemo_compact.txt",
    );
    let (text, err) =
        load_prompt_file_with_default(&path, DEFAULT_FASTMEMO_COMPACT_PROMPT, "FASTMEMO-COMPACT");
    (text, err, path)
}

fn render_context_compact_prompt(
    template: &str,
    target: MindKind,
    sys_cfg: &SystemConfig,
    latest_user: &str,
) -> String {
    let t = template.trim();
    let tpl = if t.is_empty() {
        DEFAULT_CONTEXT_COMPACT_PROMPT
    } else {
        t
    };
    let target_label = if matches!(target, MindKind::Main) {
        "main"
    } else {
        "dog"
    };
    tpl.replace("{TARGET}", target_label)
        // 兼容旧模板：ROUNDS 已弃用（改为 token 触发）。
        .replace("{ROUNDS}", "")
        .replace("{TOKENS}", &sys_cfg.ctx_recent_max_tokens.to_string())
        .replace("{USER}", latest_user.trim())
}

fn resolve_config_path(path: &str, prefer_project: bool) -> PathBuf {
    let raw = PathBuf::from(path);
    if raw.is_absolute() {
        return raw;
    }
    if prefer_project && let Some(candidate) = resolve_relative_to_exe(&raw) {
        return candidate;
    }
    if raw.exists() {
        return raw;
    }
    if !prefer_project && let Some(candidate) = resolve_relative_to_exe(&raw) {
        return candidate;
    }
    raw
}

fn resolve_relative_to_exe(raw: &Path) -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let mut dir = exe.parent().map(|p| p.to_path_buf());
    for _ in 0..5 {
        let current = dir.clone()?;
        let candidate = current.join(raw);
        if candidate.exists() {
            return Some(candidate);
        }
        dir = current.parent().map(|p| p.to_path_buf());
    }
    None
}

fn resolve_relative_path(path: &str, base: &Path) -> String {
    let p = PathBuf::from(path);
    if p.is_absolute() {
        return p.to_string_lossy().to_string();
    }
    base.join(p).to_string_lossy().to_string()
}

fn config_root_dir(cfg_path: &Path) -> Option<PathBuf> {
    let dir = cfg_path.parent()?;
    if let Some(name) = dir.file_name()
        && name == "config"
    {
        return dir.parent().map(|p| p.to_path_buf());
    }
    Some(dir.to_path_buf())
}

fn normalize_dog_config_paths(mut cfg: DogApiConfig, cfg_path: &Path) -> DogApiConfig {
    if let Some(base) = config_root_dir(cfg_path) {
        cfg.prompt_path = resolve_relative_path(&cfg.prompt_path, &base);
        cfg.token_total_path = resolve_relative_path(&cfg.token_total_path, &base);
    }
    cfg
}

fn normalize_main_config_paths(mut cfg: MainApiConfig, cfg_path: &Path) -> MainApiConfig {
    if let Some(base) = config_root_dir(cfg_path) {
        cfg.prompt_path = resolve_relative_path(&cfg.prompt_path, &base);
    }
    cfg
}

fn build_main_client(cfg: &MainApiConfig) -> anyhow::Result<DogClient> {
    let dog_cfg = DogApiConfig {
        api_key: cfg.api_key.clone(),
        provider: cfg.provider.clone(),
        base_url: cfg.base_url.clone(),
        model: cfg.model.clone(),
        temperature: cfg.temperature,
        timeout_secs: cfg.timeout_secs,
        max_tokens: cfg.max_tokens,
        ..Default::default()
    };
    DogClient::new(dog_cfg)
}

#[derive(Debug, Clone)]
struct MetaMemoSig {
    role: String,
    agent: Option<String>,
    hash: u64,
}

#[derive(Debug)]
struct MetaMemo {
    db: MemoDb,
    last_sig: Option<MetaMemoSig>,
}

impl MetaMemo {
    fn open(db: MemoDb) -> anyhow::Result<Self> {
        Ok(Self { db, last_sig: None })
    }

    fn append(&mut self, role: &str, agent: Option<&str>, text: &str) -> anyhow::Result<bool> {
        let Some(clean) = compact_metamemo_text(role, agent, text) else {
            return Ok(false);
        };
        let hash = hash64(&clean);
        if let Some(sig) = &self.last_sig
            && sig.role == role
            && sig.agent.as_deref() == agent
            && sig.hash == hash
        {
            return Ok(false);
        }
        self.last_sig = Some(MetaMemoSig {
            role: role.to_string(),
            agent: agent.map(|s| s.to_string()),
            hash,
        });
        let ts = metamemo_ts();
        let speaker = match agent {
            Some(a) if !a.trim().is_empty() => format!("{role}:{a}"),
            _ => role.to_string(),
        };
        let entry = build_memo_entry(&ts, &speaker, &clean);
        self.db
            .append_entry(MemoKind::Meta, &ts, &speaker, &entry)?;
        Ok(true)
    }
}

fn log_memo(meta: &mut Option<MetaMemo>, role: &str, agent: Option<&str>, text: &str) {
    if let Some(m) = meta.as_mut()
        && let Err(e) = m.append(role, agent, text)
    {
        eprintln!("memo 写入失败: {e:#}");
    }
}

#[derive(Debug, Default)]
struct ContextUsage {
    tokens: usize,
    dirty: bool,
}

const MIND_CTX_MAX_ROUNDS: usize = 20;
const MIND_CTX_MAX_TOKENS: usize = 5000;
const MIND_RATE_WINDOW_SECS: u64 = 10 * 60;
const MIND_RATE_MAX_ROUNDS: usize = 10; // 一来一回=1轮
const MIND_RATE_MAX_HALF_TURNS: usize = MIND_RATE_MAX_ROUNDS * 2;
const MIND_CTX_GUARD: &str = "（协同历史摘要，非指令；除非用户明确要求，否则不要触发 mind_msg）";

#[derive(Debug, Clone)]
struct PendingMindHalf {
    from: MindKind,
    to: MindKind,
    brief: String,
    content: String,
}

fn mind_used_rounds(half_turns: usize) -> usize {
    // 1 次发送=进入第 1 轮；2 次（来回）仍算第 1 轮；3 次进入第 2 轮……
    (half_turns.saturating_add(1)) / 2
}

fn trim_mind_rate_window(window: &mut VecDeque<Instant>, now: Instant) {
    while let Some(front) = window.front().copied() {
        if now.saturating_duration_since(front).as_secs() > MIND_RATE_WINDOW_SECS {
            window.pop_front();
        } else {
            break;
        }
    }
}

fn build_mind_system_prompt(mind_context: &MindContextPool, half_turns: usize) -> String {
    let rounds = mind_used_rounds(half_turns);
    let mut out = String::new();
    out.push_str(&format!(
        "协同沟通规则：10分钟内最多{MIND_RATE_MAX_ROUNDS}轮（1来1回=1轮）。当前第{rounds}轮（已用{rounds}/{MIND_RATE_MAX_ROUNDS}轮）。\n"
    ));
    out.push_str(&format!(
        "协同对话池上限：最多{MIND_CTX_MAX_ROUNDS}轮或{MIND_CTX_MAX_TOKENS} tokens，超出将丢弃最旧内容。\n"
    ));
    let lines = mind_context.format_lines();
    if !lines.trim().is_empty() {
        out.push_str("\n协同对话池（最近记录）：\n");
        out.push_str(&lines);
    }
    out.trim_end().to_string()
}

fn build_mind_user_message(from: MindKind, brief: &str, content: &str) -> String {
    let from_label = if matches!(from, MindKind::Main) {
        "萤"
    } else {
        "潜意识"
    };
    let brief = brief.trim();
    let content = content.trim();
    if brief.is_empty() || brief.eq_ignore_ascii_case("mind_msg") {
        format!("来自{from_label}的消息：{content}")
    } else {
        format!("来自{from_label}的消息（{brief}）：{content}")
    }
}

fn format_mind_half_line(from: MindKind, to: MindKind, brief: &str, content: &str) -> String {
    let from_label = if matches!(from, MindKind::Main) {
        "萤"
    } else {
        "潜意识"
    };
    let to_label = if matches!(to, MindKind::Main) {
        "萤"
    } else {
        "潜意识"
    };
    let brief = brief.trim();
    let content = content.trim();
    if brief.is_empty() || brief.eq_ignore_ascii_case("mind_msg") {
        format!("{from_label}→{to_label}：{content}")
    } else {
        format!("{from_label}→{to_label}（{brief}）：{content}")
    }
}

struct MindContextEntry {
    from: MindKind,
    to: MindKind,
    text: String,
    tokens: usize,
}

struct MindContextPool {
    entries: VecDeque<MindContextEntry>,
    total_tokens: usize,
    max_messages: usize,
    max_tokens: usize,
}

impl MindContextPool {
    fn new(max_rounds: usize, max_tokens: usize) -> Self {
        Self {
            entries: VecDeque::new(),
            total_tokens: 0,
            max_messages: max_rounds.saturating_mul(2).max(2),
            max_tokens: max_tokens.max(1),
        }
    }

    fn push(&mut self, from: MindKind, to: MindKind, text: &str) {
        let clean = compact_ws_inline(text);
        if clean.is_empty() {
            return;
        }
        let mut content = clean;
        let mut tokens = estimate_tokens(&content);
        let entry_limit = self.max_tokens.saturating_div(2).max(1);
        if tokens > entry_limit {
            content = truncate_to_token_budget(&content, entry_limit);
            tokens = estimate_tokens(&content);
        }
        self.entries.push_back(MindContextEntry {
            from,
            to,
            text: content,
            tokens,
        });
        self.total_tokens = self.total_tokens.saturating_add(tokens);
        self.trim();
    }

    fn trim(&mut self) {
        while self.entries.len() > self.max_messages || self.total_tokens > self.max_tokens {
            if let Some(entry) = self.entries.pop_front() {
                self.total_tokens = self.total_tokens.saturating_sub(entry.tokens);
            } else {
                break;
            }
        }
    }

    fn format_lines(&self) -> String {
        let mut out = String::new();
        for entry in &self.entries {
            let label = match (entry.from, entry.to) {
                (MindKind::Main, MindKind::Sub) => "萤→潜意识",
                (MindKind::Sub, MindKind::Main) => "潜意识→萤",
                (MindKind::Main, MindKind::Main) => "萤→萤",
                (MindKind::Sub, MindKind::Sub) => "潜意识→潜意识",
            };
            out.push_str(&format!("- {label}: {}\n", entry.text));
        }
        out.trim_end().to_string()
    }
}

fn mind_context_speaker(from: MindKind, to: MindKind) -> &'static str {
    match (from, to) {
        (MindKind::Main, MindKind::Sub) => "main->dog",
        (MindKind::Sub, MindKind::Main) => "dog->main",
        (MindKind::Main, MindKind::Main) => "main->main",
        (MindKind::Sub, MindKind::Sub) => "dog->dog",
    }
}

fn truncate_to_token_budget(text: &str, max_tokens: usize) -> String {
    let max_bytes = max_tokens.saturating_mul(4).max(1);
    let mut out = text.to_string();
    truncate_to_max_bytes(&mut out, max_bytes);
    out
}

impl ContextUsage {
    fn add_text(&mut self, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            return;
        }
        self.tokens = self.tokens.saturating_add(estimate_tokens(clean));
        self.dirty = true;
    }

    fn reset(&mut self) {
        self.tokens = 0;
        self.dirty = true;
    }

    fn load_tokens(&mut self, tokens: usize) {
        self.tokens = tokens;
        self.dirty = false;
    }

    fn mark_clean(&mut self) {
        self.dirty = false;
    }

    fn dirty(&self) -> bool {
        self.dirty
    }

    fn tokens(&self) -> usize {
        self.tokens
    }
}

fn should_count_usage(role: &str, _agent: Option<&str>) -> bool {
    let _ = role;
    false
}

fn is_datememo_add(call: &ToolCall) -> bool {
    if call.tool != "memory_add" {
        return false;
    }
    let raw = call
        .path
        .as_deref()
        .unwrap_or(call.input.trim())
        .to_ascii_lowercase();
    raw.contains("datememo")
}

fn log_memos(
    meta: &mut Option<MetaMemo>,
    usage: &mut ContextUsage,
    role: &str,
    agent: Option<&str>,
    text: &str,
) {
    log_memo(meta, role, agent, text);
    if should_count_usage(role, agent) {
        usage.add_text(text);
    }
}

fn push_system_and_log(
    core: &mut Core,
    meta: &mut Option<MetaMemo>,
    usage: &mut ContextUsage,
    agent: Option<&str>,
    text: &str,
) {
    core.push_system(text);
    log_memos(meta, usage, "system", agent, text);
}

fn update_history_text_at(
    core: &mut Core,
    render_cache: &mut ui::ChatRenderCache,
    idx: usize,
    text: &str,
) {
    if let Some(entry) = core.history.get_mut(idx) {
        entry.text.clear();
        entry.text.push_str(text);
        render_cache.invalidate(idx);
    }
}

fn update_ctx_compact_notice(
    core: &mut Core,
    render_cache: &mut ui::ChatRenderCache,
    ctx_compact_notice_idx: &mut Option<usize>,
    text: &str,
) {
    if let Some(idx) = *ctx_compact_notice_idx {
        update_history_text_at(core, render_cache, idx, text);
        *ctx_compact_notice_idx = None;
    }
}

fn update_ctx_compact_notice_if_system(
    core: &mut Core,
    render_cache: &mut ui::ChatRenderCache,
    ctx_compact_notice_idx: &mut Option<usize>,
    text: &str,
) {
    if let Some(idx) = *ctx_compact_notice_idx
        && core
            .history
            .get(idx)
            .is_some_and(|m| m.role == Role::System)
    {
        update_ctx_compact_notice(core, render_cache, ctx_compact_notice_idx, text);
    }
}

fn hash64(text: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

fn metamemo_ts() -> String {
    chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

#[derive(Debug)]
struct RunLogger {
    writer: Mutex<io::BufWriter<fs::File>>,
    run_id: String,
    seq: AtomicU64,
    stream_chunks: bool,
}

static RUN_LOGGER: OnceLock<RunLogger> = OnceLock::new();

fn init_run_logger(path: &str) {
    if path.trim().is_empty() {
        return;
    }
    if RUN_LOGGER.get().is_some() {
        return;
    }
    let p = Path::new(path);
    if let Some(parent) = p.parent()
        && !parent.as_os_str().is_empty()
    {
        let _ = fs::create_dir_all(parent);
    }
    let Ok(file) = fs::OpenOptions::new().create(true).append(true).open(p) else {
        return;
    };
    let stream_chunks = std::env::var("YING_RUN_LOG_STREAM_CHUNKS")
        .ok()
        .is_some_and(|v| v.trim() == "1" || v.trim().eq_ignore_ascii_case("true"));
    let run_id = format!(
        "{}:{}:{}",
        std::process::id(),
        metamemo_ts(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(1)
    );
    let _ = RUN_LOGGER.set(RunLogger {
        writer: Mutex::new(io::BufWriter::new(file)),
        run_id,
        seq: AtomicU64::new(0),
        stream_chunks,
    });
}

fn redact_log_value(value: serde_json::Value) -> serde_json::Value {
    fn is_sensitive_key(key: &str) -> bool {
        let k = key.trim().to_ascii_lowercase();
        matches!(
            k.as_str(),
            "api_key"
                | "apikey"
                | "authorization"
                | "auth"
                | "bearer"
                | "token"
                | "secret"
                | "password"
                | "pass"
        )
    }
    fn walk(v: &mut serde_json::Value) {
        match v {
            serde_json::Value::Object(map) => {
                for (k, child) in map.iter_mut() {
                    if is_sensitive_key(k) {
                        *child = serde_json::Value::String("<redacted>".to_string());
                    } else {
                        walk(child);
                    }
                }
            }
            serde_json::Value::Array(items) => {
                for item in items.iter_mut() {
                    walk(item);
                }
            }
            _ => {}
        }
    }
    let mut out = value;
    walk(&mut out);
    out
}

fn runlog_event(level: &str, event: &str, data: serde_json::Value) {
    let Some(logger) = RUN_LOGGER.get() else {
        return;
    };
    let seq = logger.seq.fetch_add(1, Ordering::Relaxed).saturating_add(1);
    let record = json!({
        "ts": metamemo_ts(),
        "run_id": logger.run_id,
        "seq": seq,
        "level": level,
        "event": event,
        "thread": format!("{:?}", thread::current().id()),
        "data": redact_log_value(data),
    });
    let Ok(line) = serde_json::to_string(&record) else {
        return;
    };
    if let Ok(mut w) = logger.writer.lock() {
        let _ = w.write_all(line.as_bytes());
        let _ = w.write_all(b"\n");
        if level == "ERROR" || level == "WARN" {
            let _ = w.flush();
        }
    }
}

fn tool_call_log_fields(call: &ToolCall) -> serde_json::Value {
    json!({
        "tool": &call.tool,
        "brief": call.brief.as_deref(),
        "input": &call.input,
        "path": call.path.as_deref(),
        "content": call.content.as_deref(),
        "pattern": call.pattern.as_deref(),
        "root": call.root.as_deref(),
        "patch": call.patch.as_deref(),
        "find": call.find.as_deref(),
        "replace": call.replace.as_deref(),
        "count": call.count,
        "start_line": call.start_line,
        "max_lines": call.max_lines,
        "head": call.head,
        "tail": call.tail,
        "file": call.file,
        "strict": call.strict,
        "time": call.time.as_deref(),
        "keywords": call.keywords.as_deref(),
        "diary": call.diary.as_deref(),
        "category": call.category.as_deref(),
        "section": call.section.as_deref(),
        "target": call.target.as_deref(),
        "date_start": call.date_start.as_deref(),
        "date_end": call.date_end.as_deref(),
        "heartbeat_minutes": call.heartbeat_minutes,
    })
}

fn append_run_log(path: &str, level: &str, msg: &str) {
    init_run_logger(path);
    runlog_event(level, "log", json!({ "msg": msg.trim() }));
}

fn contextmemo_ts() -> String {
    metamemo_ts()
}

fn load_contextmemo_tokens(path: &str) -> usize {
    let file = match fs::File::open(path) {
        Ok(f) => f,
        Err(_) => return 0,
    };
    let reader = io::BufReader::new(file);
    let mut tokens = 0usize;
    for line in reader.lines().map_while(Result::ok) {
        let clean = line.trim();
        if clean.is_empty() {
            continue;
        }
        tokens = tokens.saturating_add(estimate_tokens(clean));
    }
    tokens
}

fn append_contextmemo(
    path: &str,
    usage: &mut ContextUsage,
    speaker: &str,
    text: &str,
) -> anyhow::Result<()> {
    let clean = text.trim();
    if clean.is_empty() {
        return Ok(());
    }
    let entry = json!({
        "time": contextmemo_ts(),
        "speaker": speaker,
        "content": clean,
    });
    let line = serde_json::to_string(&entry)?;
    let p = Path::new(path);
    if let Some(parent) = p.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建目录失败：{}", parent.display()))?;
    }
    let mut f = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(p)
        .with_context(|| format!("写入失败：{}", p.display()))?;
    writeln!(f, "{line}").ok();
    usage.add_text(&line);
    Ok(())
}

fn read_contextmemo_text(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_default()
}

fn log_contextmemo(path: &str, usage: &mut ContextUsage, speaker: &str, text: &str) {
    if let Err(e) = append_contextmemo(path, usage, speaker, text) {
        eprintln!("contextmemo 写入失败: {e:#}");
    }
}

fn clear_contextmemo(path: &str) {
    if let Err(e) = fs::write(path, "") {
        eprintln!("contextmemo 清理失败: {e:#}");
    }
}

fn compact_metamemo_text(role: &str, agent: Option<&str>, text: &str) -> Option<String> {
    let clean = text.trim();
    if clean.is_empty() {
        return None;
    }
    // MetaMemo 只保留“真正聊天记录”：user/assistant（以及极简的 tool 摘要）。
    // - system（含提示词/上下文注入/状态通知）一律不写入，避免撑爆 db。
    // - tool 只写入操作摘要，不写 output/meta 内容。
    if role == "system" {
        return None;
    }
    if role == "tool" {
        return summarize_tool_for_metamemo(clean, agent);
    }
    if role != "user" && role != "assistant" {
        return None;
    }
    if role == "assistant" && is_pass_message(clean) {
        return None;
    }
    if role == "user" {
        let cmd = clean.trim();
        let lower = cmd.to_ascii_lowercase();
        // 明确的 UI 指令不属于“聊天内容”，不进 MetaMemo。
        if lower == "/quit"
            || lower == "/q"
            || lower == "/cmd"
            || lower == "/settings"
            || lower.starts_with("/turn ")
            || lower.starts_with("/trun ")
            || lower.starts_with("/switch ")
            || lower.starts_with("/sse ")
        {
            return None;
        }
    }
    let compact = collapse_blank_lines(clean);
    Some(truncate_with_suffix(&compact, 2000))
}

fn summarize_tool_for_metamemo(text: &str, agent: Option<&str>) -> Option<String> {
    let clean = text.trim();
    if clean.is_empty() {
        return None;
    }
    // 仅保留 tool 头部信息：操作/说明/input/mind，并在 output/meta 前截止。
    let mut parts: Vec<String> = Vec::new();
    let mut tool_name: Option<String> = None;
    let agent = agent.unwrap_or("");
    if !agent.trim().is_empty() {
        parts.push(format!("agent:{agent}"));
    }
    for line in clean.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let lower = trimmed.to_ascii_lowercase();
        if lower.starts_with("output:") || lower.starts_with("meta:") {
            break;
        }
        if trimmed == "```text" || trimmed == "```" {
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("操作:") {
            let v = rest.trim();
            if !v.is_empty() {
                tool_name = Some(v.to_string());
                parts.push(format!("工具: {}", truncate_with_suffix(v, 140)));
            }
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("explain:") {
            let v = rest.trim();
            if !v.is_empty() {
                parts.push(format!("说明: {}", truncate_with_suffix(v, 160)));
            }
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("input:") {
            let v = rest.trim();
            if !v.is_empty() {
                parts.push(format!("输入: {}", truncate_with_suffix(v, 160)));
            }
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("mind:") {
            let v = rest.trim();
            if !v.is_empty() {
                parts.push(format!("mind:{v}"));
            }
            continue;
        }
    }
    if parts.is_empty() {
        return None;
    }
    if tool_name.is_some_and(|v| v.eq_ignore_ascii_case("think")) {
        return None;
    }
    let joined = parts.join(" | ");
    Some(truncate_with_suffix(&joined, 420))
}

fn collapse_blank_lines(text: &str) -> String {
    let mut out = String::new();
    let mut last_blank = false;
    for line in text.lines() {
        let blank = line.trim().is_empty();
        if blank {
            if last_blank {
                continue;
            }
            last_blank = true;
            out.push('\n');
            continue;
        }
        last_blank = false;
        out.push_str(line.trim_end());
        out.push('\n');
    }
    out.trim_end().to_string()
}

fn truncate_with_suffix(text: &str, max_chars: usize) -> String {
    let total = text.chars().count();
    if total <= max_chars {
        return text.to_string();
    }
    let mut out = String::new();
    for (count, ch) in text.chars().enumerate() {
        if count >= max_chars {
            break;
        }
        out.push(ch);
    }
    out.push_str(&format!("... [截断 {}/{} chars]", max_chars, total));
    out
}

fn truncate_tail(text: &str, max_chars: usize) -> String {
    let total = text.chars().count();
    if total <= max_chars {
        return text.to_string();
    }
    let mut out: Vec<char> = text.chars().rev().take(max_chars).collect();
    out.reverse();
    let kept: String = out.into_iter().collect();
    format!("... [截断(保留末尾) {}/{} chars]\n{kept}", max_chars, total)
}

fn read_tail_text(path: &str, max_bytes: u64) -> Option<String> {
    let p = Path::new(path);
    let mut f = fs::File::open(p).ok()?;
    let len = f.metadata().ok()?.len();
    let start = len.saturating_sub(max_bytes);
    if f.seek(SeekFrom::Start(start)).is_err() {
        return None;
    }
    let mut buf = Vec::new();
    let _ = f.read_to_end(&mut buf).ok()?;
    // raw PTY bytes 可能含 ANSI/不可见控制符：这里不强行剥离，只做 lossy 解码与 NUL 清理。
    let mut s = String::from_utf8_lossy(&buf).to_string();
    s.retain(|ch| ch != '\u{0}');
    Some(s)
}

fn summarize_api_error_for_user(raw: &str) -> String {
    let clean = raw.trim();
    if clean.is_empty() {
        return "请求失败（无错误信息）".to_string();
    }
    let lower = clean.to_ascii_lowercase();
    let reason = if lower.contains("timeout")
        || lower.contains("timed out")
        || lower.contains("deadline exceeded")
    {
        "请求超时"
    } else if lower.contains("missing_api_key")
        || (lower.contains("api key")
            && (lower.contains("missing")
                || lower.contains("not set")
                || lower.contains("unset")
                || lower.contains("empty")
                || lower.contains("未配置")
                || lower.contains("未设置")
                || lower.contains("为空")))
    {
        "未配置 API Key"
    } else if lower.contains("429")
        || lower.contains("rate limit")
        || lower.contains("too many requests")
    {
        "请求过于频繁（限流）"
    } else if lower.contains("401")
        || lower.contains("unauthorized")
        || lower.contains("invalid api key")
        || lower.contains("api key") && lower.contains("invalid")
    {
        "认证失败（检查 API Key）"
    } else if lower.contains("403") || lower.contains("forbidden") {
        "权限不足（403）"
    } else if lower.contains("400") || lower.contains("bad request") {
        "请求格式错误（400）"
    } else if lower.contains("404") || lower.contains("not found") {
        "资源不存在（404）"
    } else if lower.contains("500")
        || lower.contains("502")
        || lower.contains("503")
        || lower.contains("504")
        || lower.contains("server error")
        || lower.contains("bad gateway")
        || lower.contains("service unavailable")
    {
        "服务端异常（5xx）"
    } else if lower.contains("dns")
        || lower.contains("could not resolve")
        || lower.contains("name or service not known")
        || lower.contains("nodename nor servname provided")
    {
        "网络解析失败（DNS）"
    } else if lower.contains("connection refused")
        || lower.contains("connection reset")
        || lower.contains("broken pipe")
        || lower.contains("tcp")
        || lower.contains("connect")
    {
        "网络连接失败"
    } else if lower.contains("model") && (lower.contains("not found") || lower.contains("invalid"))
    {
        "模型配置错误"
    } else {
        "请求失败"
    };
    let id = format!("{:08x}", hash64(clean));
    format!("{reason}（ID:{id}）")
}

struct ToolStreamState {
    idx: usize,
    owner: MindKind,
    call: Box<ToolCall>,
    output: String,
    meta: Vec<String>,
    placeholder: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SettingsSection {
    MainApi,
    DogApi,
    System,
    PromptCenter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SettingsFieldKind {
    Provider,
    BaseUrl,
    ApiKey,
    Model,
    Temperature,
    MaxTokens,
    ContextK,
    HeartbeatMinutes,
    SseEnabled,
    ContextMaxTokens,
    ContextPoolMaxItems,
    ChatTarget,
    ExecPermission,
    DogPrompt,
    MainPrompt,
    ContextMainPrompt,
    ContextCompactPrompt,
}

struct SettingsFieldSpec {
    label: &'static str,
    value: String,
    kind: SettingsFieldKind,
}

struct SettingsState {
    menu_index: usize,
    field_index: usize,
    focus: SettingsFocus,
    edit_kind: Option<SettingsFieldKind>,
    edit_buffer: String,
    edit_cursor: usize,
    notice: Option<(Instant, String)>,
}

impl SettingsState {
    fn new() -> Self {
        Self {
            menu_index: 0,
            field_index: 0,
            focus: SettingsFocus::Tabs,
            edit_kind: None,
            edit_buffer: String::new(),
            edit_cursor: 0,
            notice: None,
        }
    }
}

fn set_settings_notice(settings: &mut SettingsState, now: Instant, msg: String) {
    settings.notice = Some((now + Duration::from_millis(1600), msg));
}

fn reset_settings_edit(settings: &mut SettingsState) {
    settings.edit_kind = None;
    settings.edit_buffer.clear();
    settings.edit_cursor = 0;
}

fn reset_settings_to_tabs(settings: &mut SettingsState) {
    settings.focus = SettingsFocus::Tabs;
    reset_settings_edit(settings);
}

fn handle_settings_tab_nav(code: KeyCode, settings: &mut SettingsState) -> bool {
    if matches!(code, KeyCode::PageUp | KeyCode::PageDown | KeyCode::Tab) {
        let max = settings_menu_items().len().saturating_sub(1);
        if matches!(code, KeyCode::PageUp) {
            settings.menu_index = settings.menu_index.saturating_sub(1);
        } else {
            settings.menu_index = (settings.menu_index + 1).min(max);
        }
        reset_settings_to_tabs(settings);
        return true;
    }
    false
}

fn reset_input_buffer(
    input: &mut String,
    cursor: &mut usize,
    input_chars: &mut usize,
    last_input_at: &mut Option<Instant>,
) {
    input.clear();
    *cursor = 0;
    *input_chars = 0;
    *last_input_at = None;
}

fn edit_buffer_backspace(buffer: &mut String, cursor: &mut usize) {
    if *cursor > 0 {
        let prev = prev_char_boundary(buffer, *cursor);
        buffer.drain(prev..*cursor);
        *cursor = prev;
    }
}

fn edit_buffer_delete(buffer: &mut String, cursor: &mut usize) {
    if *cursor < buffer.len() {
        let next = next_char_boundary(buffer, *cursor);
        buffer.drain(*cursor..next);
    }
}

fn edit_buffer_left(buffer: &str, cursor: &mut usize) {
    if *cursor > 0 {
        *cursor = prev_char_boundary(buffer, *cursor);
    }
}

fn edit_buffer_right(buffer: &str, cursor: &mut usize) {
    if *cursor < buffer.len() {
        *cursor = next_char_boundary(buffer, *cursor);
    }
}

fn edit_buffer_home(cursor: &mut usize) {
    *cursor = 0;
}

fn edit_buffer_end(buffer: &str, cursor: &mut usize) {
    *cursor = buffer.len();
}

fn edit_buffer_insert_char(buffer: &mut String, cursor: &mut usize, ch: char) {
    buffer.insert(*cursor, ch);
    *cursor = cursor.saturating_add(ch.len_utf8());
}

fn drain_input_for_send(
    input: &mut String,
    cursor: &mut usize,
    input_chars: &mut usize,
    last_input_at: &mut Option<Instant>,
    pending_pastes: &mut Vec<(String, String)>,
    paste_capture: &mut Option<PasteCapture>,
    command_menu_suppress: &mut bool,
) -> String {
    let send = materialize_pastes(input, pending_pastes);
    reset_input_buffer(input, cursor, input_chars, last_input_at);
    pending_pastes.clear();
    *paste_capture = None;
    *command_menu_suppress = false;
    send
}

fn sync_system_toggles(
    sys_cfg: &SystemConfig,
    sse_enabled: &mut bool,
    chat_target: &mut MindKind,
) {
    *sse_enabled = sys_cfg.sse_enabled;
    *chat_target = parse_chat_target(&sys_cfg.chat_target);
}

struct CommitSettingsInputArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    buffer: &'a str,
    settings: &'a mut SettingsState,
    now: Instant,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    context_compact_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
    context_compact_prompt_path: &'a Path,
    sse_enabled: &'a mut bool,
    chat_target: &'a mut MindKind,
}

fn commit_settings_input(args: CommitSettingsInputArgs<'_>) {
    let CommitSettingsInputArgs {
        kind,
        section,
        buffer,
        settings,
        now,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        main_prompt_text,
        context_compact_prompt_text,
        dog_client,
        main_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        context_compact_prompt_path,
        sse_enabled,
        chat_target,
    } = args;

    apply_settings_with_notice(ApplySettingsWithNoticeArgs {
        kind,
        section,
        value: buffer,
        settings,
        now,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        main_prompt_text,
        context_compact_prompt_text,
        dog_client,
        main_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        context_compact_prompt_path,
    });
    sync_system_toggles(sys_cfg, sse_enabled, chat_target);
    settings.focus = SettingsFocus::Fields;
    reset_settings_edit(settings);
}

fn settings_menu_items() -> Vec<String> {
    vec![
        "1 主API配置".to_string(),
        "2 辅API配置".to_string(),
        "3 系统配置".to_string(),
        "4 提示词管理".to_string(),
    ]
}

fn settings_section_for_menu(idx: usize) -> SettingsSection {
    match idx {
        0 => SettingsSection::MainApi,
        1 => SettingsSection::DogApi,
        2 => SettingsSection::System,
        3 => SettingsSection::PromptCenter,
        _ => SettingsSection::MainApi,
    }
}

fn settings_section_title(section: SettingsSection) -> &'static str {
    match section {
        SettingsSection::MainApi => "主API配置",
        SettingsSection::DogApi => "辅API配置",
        SettingsSection::System => "系统配置",
        SettingsSection::PromptCenter => "提示词管理",
    }
}

fn is_prompt_kind(kind: SettingsFieldKind) -> bool {
    matches!(
        kind,
        SettingsFieldKind::DogPrompt
            | SettingsFieldKind::MainPrompt
            | SettingsFieldKind::ContextMainPrompt
            | SettingsFieldKind::ContextCompactPrompt
    )
}

fn mask_api_key(key: Option<&str>) -> String {
    let Some(raw) = key else {
        return "未设置".to_string();
    };
    let raw = raw.trim();
    if raw.is_empty() {
        return "未设置".to_string();
    }
    if raw.len() <= 6 {
        return "••••".to_string();
    }
    let tail = raw.chars().rev().take(4).collect::<String>();
    format!("sk-••••{tail}")
}

fn provider_label(value: &str) -> &'static str {
    if value.trim().eq_ignore_ascii_case("deepseek") {
        "DeepSeek"
    } else {
        "开发中"
    }
}

fn normalize_provider(value: &str) -> &str {
    if value.trim().eq_ignore_ascii_case("deepseek") {
        "deepseek"
    } else {
        "dev"
    }
}

fn model_label(value: &str) -> &'static str {
    let v = value.trim().to_ascii_lowercase();
    if v.contains("reasoner") {
        "DeepSeek Reasoner"
    } else if v.contains("chat") {
        "DeepSeek Chat"
    } else {
        "自定义"
    }
}

fn format_temp(temp: Option<f32>) -> String {
    match temp {
        Some(t) if t > 0.0 => format!("{t:.2}"),
        _ => "Default".to_string(),
    }
}

fn format_max_tokens(max_tokens: Option<u32>) -> String {
    max_tokens
        .filter(|v| *v > 0)
        .map(|v| v.to_string())
        .unwrap_or_else(|| "Default".to_string())
}

fn format_toggle(value: bool) -> String {
    if value {
        "On".to_string()
    } else {
        "Off".to_string()
    }
}

fn parse_toggle_input(value: &str) -> Option<bool> {
    let t = value.trim().to_ascii_lowercase();
    if t.is_empty() {
        return None;
    }
    match t.as_str() {
        "1" | "true" | "on" | "open" | "enable" | "enabled" | "开启" | "开" | "是" | "yes" => {
            Some(true)
        }
        "0" | "false" | "off" | "close" | "disable" | "disabled" | "关闭" | "关" | "否" | "no" => {
            Some(false)
        }
        _ => None,
    }
}

fn summarize_prompt(text: &str) -> String {
    let lines = text.lines().count().max(1);
    let chars = text.chars().count();
    format!("{lines} lines / {chars} chars")
}

struct BuildSettingsFieldsArgs<'a> {
    section: SettingsSection,
    dog_cfg: &'a DogApiConfig,
    main_cfg: &'a MainApiConfig,
    sys_cfg: &'a SystemConfig,
    dog_prompt_text: &'a str,
    main_prompt_text: &'a str,
    context_prompts: &'a ContextPromptConfig,
    context_compact_prompt_text: &'a str,
}

fn build_settings_fields(args: BuildSettingsFieldsArgs<'_>) -> Vec<SettingsFieldSpec> {
    let BuildSettingsFieldsArgs {
        section,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_prompt_text,
        main_prompt_text,
        context_prompts,
        context_compact_prompt_text,
    } = args;
    match section {
        SettingsSection::MainApi => vec![
            SettingsFieldSpec {
                label: "Provider",
                value: provider_label(&main_cfg.provider).to_string(),
                kind: SettingsFieldKind::Provider,
            },
            SettingsFieldSpec {
                label: "Base URL",
                value: main_cfg.base_url.clone(),
                kind: SettingsFieldKind::BaseUrl,
            },
            SettingsFieldSpec {
                label: "Key",
                value: mask_api_key(main_cfg.api_key.as_deref()),
                kind: SettingsFieldKind::ApiKey,
            },
            SettingsFieldSpec {
                label: "Model",
                value: model_label(&main_cfg.model).to_string(),
                kind: SettingsFieldKind::Model,
            },
            SettingsFieldSpec {
                label: "Temp",
                value: format_temp(main_cfg.temperature),
                kind: SettingsFieldKind::Temperature,
            },
            SettingsFieldSpec {
                label: "MaxToken",
                value: format_max_tokens(main_cfg.max_tokens),
                kind: SettingsFieldKind::MaxTokens,
            },
        ],
        SettingsSection::DogApi => vec![
            SettingsFieldSpec {
                label: "Provider",
                value: provider_label(&dog_cfg.provider).to_string(),
                kind: SettingsFieldKind::Provider,
            },
            SettingsFieldSpec {
                label: "Base URL",
                value: dog_cfg.base_url.clone(),
                kind: SettingsFieldKind::BaseUrl,
            },
            SettingsFieldSpec {
                label: "Key",
                value: mask_api_key(dog_cfg.api_key.as_deref()),
                kind: SettingsFieldKind::ApiKey,
            },
            SettingsFieldSpec {
                label: "Model",
                value: model_label(&dog_cfg.model).to_string(),
                kind: SettingsFieldKind::Model,
            },
            SettingsFieldSpec {
                label: "Temp",
                value: format_temp(dog_cfg.temperature),
                kind: SettingsFieldKind::Temperature,
            },
            SettingsFieldSpec {
                label: "MaxToken",
                value: format_max_tokens(dog_cfg.max_tokens),
                kind: SettingsFieldKind::MaxTokens,
            },
        ],
        SettingsSection::System => vec![
            SettingsFieldSpec {
                label: "Ctx compact (k)",
                value: format!("{}k", sys_cfg.context_k),
                kind: SettingsFieldKind::ContextK,
            },
            SettingsFieldSpec {
                label: "Heartbeat",
                value: if sys_cfg.heartbeat_minutes == 0 {
                    "Off".to_string()
                } else {
                    format!("{}m", sys_cfg.heartbeat_minutes)
                },
                kind: SettingsFieldKind::HeartbeatMinutes,
            },
            SettingsFieldSpec {
                label: "Dyn ctx (tok)",
                value: format!("{}", sys_cfg.ctx_recent_max_tokens),
                kind: SettingsFieldKind::ContextMaxTokens,
            },
            SettingsFieldSpec {
                label: "Summary pool",
                value: format!("{} items", sys_cfg.ctx_pool_max_items),
                kind: SettingsFieldKind::ContextPoolMaxItems,
            },
            SettingsFieldSpec {
                label: "Chat target",
                value: format_chat_target(&sys_cfg.chat_target),
                kind: SettingsFieldKind::ChatTarget,
            },
            SettingsFieldSpec {
                label: "Exec perm",
                value: if sys_cfg.tool_full_access {
                    "Full".to_string()
                } else {
                    "Safe".to_string()
                },
                kind: SettingsFieldKind::ExecPermission,
            },
            SettingsFieldSpec {
                label: "SSE",
                value: format_toggle(sys_cfg.sse_enabled),
                kind: SettingsFieldKind::SseEnabled,
            },
        ],
        SettingsSection::PromptCenter => vec![
            SettingsFieldSpec {
                label: "1 DOG prompt",
                value: summarize_prompt(dog_prompt_text),
                kind: SettingsFieldKind::DogPrompt,
            },
            SettingsFieldSpec {
                label: "2 MAIN prompt",
                value: summarize_prompt(main_prompt_text),
                kind: SettingsFieldKind::MainPrompt,
            },
            SettingsFieldSpec {
                label: "3 MAIN Context",
                value: summarize_prompt(&context_prompts.main_prompt),
                kind: SettingsFieldKind::ContextMainPrompt,
            },
            SettingsFieldSpec {
                label: "4 Context compact",
                value: summarize_prompt(context_compact_prompt_text),
                kind: SettingsFieldKind::ContextCompactPrompt,
            },
        ],
    }
}

struct FieldRawValueArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    dog_cfg: &'a DogApiConfig,
    main_cfg: &'a MainApiConfig,
    sys_cfg: &'a SystemConfig,
    dog_prompt_text: &'a str,
    main_prompt_text: &'a str,
    context_prompts: &'a ContextPromptConfig,
    context_compact_prompt_text: &'a str,
}

fn field_raw_value(args: FieldRawValueArgs<'_>) -> String {
    let FieldRawValueArgs {
        kind,
        section,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_prompt_text,
        main_prompt_text,
        context_prompts,
        context_compact_prompt_text,
    } = args;
    match kind {
        SettingsFieldKind::Provider => match section {
            SettingsSection::MainApi => main_cfg.provider.clone(),
            SettingsSection::DogApi => dog_cfg.provider.clone(),
            _ => "deepseek".to_string(),
        },
        SettingsFieldKind::BaseUrl => match section {
            SettingsSection::MainApi => main_cfg.base_url.clone(),
            SettingsSection::DogApi => dog_cfg.base_url.clone(),
            _ => String::new(),
        },
        SettingsFieldKind::ApiKey => match section {
            SettingsSection::MainApi => main_cfg.api_key.clone().unwrap_or_default(),
            SettingsSection::DogApi => dog_cfg.api_key.clone().unwrap_or_default(),
            _ => String::new(),
        },
        SettingsFieldKind::Model => match section {
            SettingsSection::MainApi => main_cfg.model.clone(),
            SettingsSection::DogApi => dog_cfg.model.clone(),
            _ => String::new(),
        },
        SettingsFieldKind::Temperature => match section {
            SettingsSection::MainApi => main_cfg
                .temperature
                .map(|t| t.to_string())
                .unwrap_or_default(),
            SettingsSection::DogApi => dog_cfg
                .temperature
                .map(|t| t.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        },
        SettingsFieldKind::DogPrompt => dog_prompt_text.to_string(),
        SettingsFieldKind::MainPrompt => main_prompt_text.to_string(),
        SettingsFieldKind::MaxTokens => match section {
            SettingsSection::MainApi => main_cfg
                .max_tokens
                .map(|v| v.to_string())
                .unwrap_or_default(),
            SettingsSection::DogApi => dog_cfg
                .max_tokens
                .map(|v| v.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        },
        SettingsFieldKind::ContextK => sys_cfg.context_k.to_string(),
        SettingsFieldKind::HeartbeatMinutes => sys_cfg.heartbeat_minutes.to_string(),
        SettingsFieldKind::ContextMaxTokens => sys_cfg.ctx_recent_max_tokens.to_string(),
        SettingsFieldKind::ContextPoolMaxItems => sys_cfg.ctx_pool_max_items.to_string(),
        SettingsFieldKind::SseEnabled => {
            if sys_cfg.sse_enabled {
                "on".to_string()
            } else {
                "off".to_string()
            }
        }
        SettingsFieldKind::ChatTarget => sys_cfg.chat_target.clone(),
        SettingsFieldKind::ExecPermission => {
            if sys_cfg.tool_full_access {
                "full".to_string()
            } else {
                "safe".to_string()
            }
        }
        SettingsFieldKind::ContextMainPrompt => context_prompts.main_prompt.to_string(),
        SettingsFieldKind::ContextCompactPrompt => context_compact_prompt_text.to_string(),
    }
}

fn selected_provider(
    section: SettingsSection,
    dog_cfg: &DogApiConfig,
    main_cfg: &MainApiConfig,
) -> String {
    match section {
        SettingsSection::DogApi => normalize_provider(&dog_cfg.provider).to_string(),
        SettingsSection::MainApi => normalize_provider(&main_cfg.provider).to_string(),
        _ => "deepseek".to_string(),
    }
}

fn selected_model(
    section: SettingsSection,
    dog_cfg: &DogApiConfig,
    main_cfg: &MainApiConfig,
) -> String {
    match section {
        SettingsSection::DogApi => dog_cfg.model.trim().to_string(),
        SettingsSection::MainApi => main_cfg.model.trim().to_string(),
        _ => "deepseek-chat".to_string(),
    }
}

fn next_provider(current: &str) -> String {
    if current.eq_ignore_ascii_case("deepseek") {
        "dev".to_string()
    } else {
        "deepseek".to_string()
    }
}

fn next_model(current: &str, section: SettingsSection) -> String {
    let cur = current.to_ascii_lowercase();
    if cur.contains("reasoner") {
        "deepseek-chat".to_string()
    } else if cur.contains("chat") || matches!(section, SettingsSection::DogApi) {
        "deepseek-reasoner".to_string()
    } else {
        "deepseek-chat".to_string()
    }
}

struct ApplySettingsEditArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    value: &'a str,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    context_compact_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
    context_compact_prompt_path: &'a Path,
}

fn apply_settings_edit(args: ApplySettingsEditArgs<'_>) -> Result<String, String> {
    let ApplySettingsEditArgs {
        kind,
        section,
        value,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        main_prompt_text,
        context_compact_prompt_text,
        dog_client,
        main_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        context_compact_prompt_path,
    } = args;

    let trimmed = value.trim();
    match kind {
        SettingsFieldKind::Provider => {
            let next = if trimmed.is_empty() {
                "deepseek".to_string()
            } else {
                trimmed.to_string()
            };
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.provider = next;
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.provider = next;
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::BaseUrl => {
            if trimmed.is_empty() {
                return Err("域名不能为空".to_string());
            }
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.base_url = trimmed.to_string();
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.base_url = trimmed.to_string();
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::ApiKey => {
            let next = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.api_key = next;
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.api_key = next;
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::Model => {
            if trimmed.is_empty() {
                return Err("模型不能为空".to_string());
            }
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.model = trimmed.to_string();
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.model = trimmed.to_string();
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::Temperature => {
            let next = if trimmed.is_empty() {
                None
            } else {
                let t: f32 = trimmed.parse().map_err(|_| "温度需为数字".to_string())?;
                if !(0.0..=1.5).contains(&t) {
                    return Err("温度范围 0~1.5".to_string());
                } else {
                    Some(t)
                }
            };
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.temperature = next;
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.temperature = next;
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::MaxTokens => {
            if trimmed.is_empty() {
                return Err("MaxToken 不能为空".to_string());
            }
            let v: u32 = trimmed
                .parse()
                .map_err(|_| "MaxToken 需为整数".to_string())?;
            if v == 0 {
                return Err("MaxToken 必须大于 0".to_string());
            }
            match section {
                SettingsSection::DogApi => {
                    dog_cfg.max_tokens = Some(v);
                    store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                }
                SettingsSection::MainApi => {
                    main_cfg.max_tokens = Some(v);
                    store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                }
                _ => {}
            }
        }
        SettingsFieldKind::ContextK => {
            if trimmed.is_empty() {
                return Err("上下文压缩阈值不能为空".to_string());
            }
            let raw = trimmed.trim_end_matches(['k', 'K']);
            let v: usize = raw
                .parse()
                .map_err(|_| "上下文压缩阈值需为数字".to_string())?;
            if !(8..=1024).contains(&v) {
                return Err("上下文压缩阈值范围 8~1024k".to_string());
            }
            sys_cfg.context_k = v;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::HeartbeatMinutes => {
            if trimmed.is_empty() {
                return Err("心跳不能为空".to_string());
            }
            let raw = trimmed.trim_end_matches(['m', 'M']);
            let v: usize = raw.parse().map_err(|_| "心跳需为数字".to_string())?;
            if !matches!(v, 0 | 5 | 10 | 30 | 60) {
                return Err("心跳仅支持 0/5/10/30/60 分钟（0=关闭）".to_string());
            }
            sys_cfg.heartbeat_minutes = v;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::ContextMaxTokens => {
            if trimmed.is_empty() {
                return Err("动态上下文阈值不能为空".to_string());
            }
            let v: usize = trimmed
                .parse()
                .map_err(|_| "动态上下文阈值需为数字".to_string())?;
            if !(500..=200_000).contains(&v) {
                return Err("动态上下文阈值范围 500~200000".to_string());
            }
            sys_cfg.ctx_recent_max_tokens = v;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::ContextPoolMaxItems => {
            if trimmed.is_empty() {
                return Err("摘要池不能为空".to_string());
            }
            let raw = trimmed.trim_end_matches(['条']);
            let v: usize = raw.parse().map_err(|_| "摘要池需为数字".to_string())?;
            if v == 0 || v > 30 {
                return Err("摘要池范围 1~30".to_string());
            }
            sys_cfg.ctx_pool_max_items = v;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::SseEnabled => {
            let next = if trimmed.is_empty() {
                !sys_cfg.sse_enabled
            } else {
                parse_toggle_input(trimmed).ok_or_else(|| "SSE 需为 on/off".to_string())?
            };
            sys_cfg.sse_enabled = next;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::ExecPermission => {
            let next = if trimmed.is_empty() {
                !sys_cfg.tool_full_access
            } else {
                match trimmed.to_ascii_lowercase().as_str() {
                    "safe" | "安全" => false,
                    "full" | "all" | "全权" => true,
                    other => {
                        return Err(format!(
                            "执行权限仅支持 safe/full（当前：{other}）"
                        ));
                    }
                }
            };
            sys_cfg.tool_full_access = next;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::ChatTarget => {
            let next = if trimmed.is_empty() {
                if normalize_chat_target_value(&sys_cfg.chat_target) == "main" {
                    "dog".to_string()
                } else {
                    "main".to_string()
                }
            } else {
                normalize_chat_target_value(trimmed)
            };
            sys_cfg.chat_target = next;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
            let target = parse_chat_target(&sys_cfg.chat_target);
            reset_state_for_target(
                target,
                dog_state,
                main_state,
                mind_ctx_idx_main,
                mind_ctx_idx_dog,
            );
        }
        SettingsFieldKind::DogPrompt => {
            let text = value.to_string();
            store_prompt_file(Path::new(&dog_cfg.prompt_path), &text).map_err(|e| e.to_string())?;
            dog_state.prompt = text;
            dog_state.reset_context();
        }
        SettingsFieldKind::MainPrompt => {
            let text = value.to_string();
            store_prompt_file(Path::new(&main_cfg.prompt_path), &text)
                .map_err(|e| e.to_string())?;
            *main_prompt_text = text;
            main_state.prompt = main_prompt_text.clone();
            main_state.reset_context();
        }
        SettingsFieldKind::ContextMainPrompt => {
            context_prompts.main_prompt = value.to_string();
            store_prompt_file(context_prompt_path, &context_prompts.main_prompt)
                .map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::ContextCompactPrompt => {
            let text = value.to_string();
            store_prompt_file(context_compact_prompt_path, &text).map_err(|e| e.to_string())?;
            *context_compact_prompt_text = text;
        }
    }

    if matches!(section, SettingsSection::DogApi)
        && matches!(
            kind,
            SettingsFieldKind::Provider
                | SettingsFieldKind::BaseUrl
                | SettingsFieldKind::ApiKey
                | SettingsFieldKind::Model
                | SettingsFieldKind::Temperature
                | SettingsFieldKind::MaxTokens
        )
    {
        match DogClient::new(dog_cfg.clone()) {
            Ok(client) => {
                *dog_client = Some(client);
                push_sys_log(sys_log, sys_log_limit, "DOG: 配置已更新");
            }
            Err(e) => {
                *dog_client = None;
                push_sys_log(sys_log, sys_log_limit, format!("DOG: 配置无效 {e}"));
            }
        }
    }
    if matches!(section, SettingsSection::MainApi)
        && matches!(
            kind,
            SettingsFieldKind::Provider
                | SettingsFieldKind::BaseUrl
                | SettingsFieldKind::ApiKey
                | SettingsFieldKind::Model
                | SettingsFieldKind::Temperature
                | SettingsFieldKind::MaxTokens
        )
    {
        match build_main_client(main_cfg) {
            Ok(client) => {
                *main_client = Some(client);
                push_sys_log(sys_log, sys_log_limit, "MAIN: 配置已更新");
            }
            Err(e) => {
                *main_client = None;
                push_sys_log(sys_log, sys_log_limit, format!("MAIN: 配置无效 {e}"));
            }
        }
    }

    Ok("已保存".to_string())
}

struct ApplySettingsWithNoticeArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    value: &'a str,
    settings: &'a mut SettingsState,
    now: Instant,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    context_compact_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
    context_compact_prompt_path: &'a Path,
}

fn apply_settings_with_notice(args: ApplySettingsWithNoticeArgs<'_>) {
    let ApplySettingsWithNoticeArgs {
        kind,
        section,
        value,
        settings,
        now,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        main_prompt_text,
        context_compact_prompt_text,
        dog_client,
        main_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        context_compact_prompt_path,
    } = args;

    match apply_settings_edit(ApplySettingsEditArgs {
        kind,
        section,
        value,
        dog_cfg,
        main_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        main_prompt_text,
        context_compact_prompt_text,
        dog_client,
        main_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        context_compact_prompt_path,
    }) {
        Ok(msg) => set_settings_notice(settings, now, msg),
        Err(e) => set_settings_notice(settings, now, e),
    }
}

fn tool_confirm_prompt(reason: &str, call: &ToolCall) -> String {
    let label = crate::mcp::tool_display_label(&call.tool);
    let cmd_raw = call.input.trim();
    let cmd = if cmd_raw.is_empty() {
        "(empty)".to_string()
    } else {
        truncate_with_suffix(cmd_raw, 2000)
    };
    format!(
        "危险指令需要确认：{reason}\n[{label}]\ncmd:\n```sh\n{cmd}\n```\n输入 yes 执行 / no 拒绝"
    )
}

fn spawn_tool_execution(call: ToolCall, owner: MindKind, tx: mpsc::Sender<AsyncEvent>) {
    let _ = tx.send(AsyncEvent::ToolStreamStart {
        owner,
        call: Box::new(call.clone()),
        sys_msg: Some("工具执行中".to_string()),
    });
    if call.tool == "context_compact" {
        let outcome = ToolOutcome {
            user_message: "context_compact: ok".to_string(),
            log_lines: vec![],
        };
        let _ = tx.send(AsyncEvent::ToolStreamEnd {
            outcome,
            sys_msg: Some("上下文压缩完成".to_string()),
        });
        return;
    }
    if call.tool == "bash" && call.interactive.unwrap_or(false) {
        spawn_interactive_bash_execution(call, owner, tx);
        return;
    }
    thread::spawn(move || {
        let outcome = handle_tool_call_with_retry(&call, 3);
        let _ = tx.send(AsyncEvent::ToolStreamEnd {
            outcome,
            sys_msg: Some("工具执行完成".to_string()),
        });
    });
}

fn spawn_interactive_bash_execution(call: ToolCall, owner: MindKind, tx: mpsc::Sender<AsyncEvent>) {
    thread::spawn(move || {
        let cwd_display = std::env::current_dir()
            .ok()
            .and_then(|p| p.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| "(unknown)".to_string());
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
        let timeout_secs = call
            .timeout_secs
            .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
            .unwrap_or(0);

        let saved_path = {
            let _ = fs::create_dir_all("log/bash-cache");
            let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
            let pid = unsafe { libc::getpid() };
            format!("log/bash-cache/pty_{owner:?}_{ts}_{pid}.log")
        };

        let (ctrl_tx, ctrl_rx) = mpsc::channel::<PtyControl>();
        let _ = tx.send(AsyncEvent::PtyReady {
            ctrl_tx: ctrl_tx.clone(),
            cols: 80,
            rows: 24,
            saved_path: saved_path.clone(),
        });

        let mut file = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&saved_path)
            .ok();

        let pty_system = native_pty_system();
        let pair = match pty_system.openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        }) {
            Ok(p) => p,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY open failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_open_failed | cwd:{cwd_display}")],
                };
                let _ = tx.send(AsyncEvent::ToolStreamEnd {
                    outcome,
                    sys_msg: Some("工具执行失败".to_string()),
                });
                return;
            }
        };

        let mut cmd_builder = CommandBuilder::new("/data/data/com.termux/files/usr/bin/bash");
        cmd_builder.args(["-lc", &cmd]);
        cmd_builder.env("TERM", "xterm-256color");
        cmd_builder.env("COLORTERM", "truecolor");
        cmd_builder.env("LANG", "C.UTF-8");
        cmd_builder.env("LC_ALL", "C.UTF-8");
        cmd_builder.env("TERM_PROGRAM", "AItermux");
        cmd_builder.env("COLUMNS", "80");
        cmd_builder.env("LINES", "24");
        let child = match pair.slave.spawn_command(cmd_builder) {
            Ok(c) => c,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY spawn failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_spawn_failed | cwd:{cwd_display}")],
                };
                let _ = tx.send(AsyncEvent::ToolStreamEnd {
                    outcome,
                    sys_msg: Some("工具执行失败".to_string()),
                });
                return;
            }
        };

        // master：拆出 reader/writer，并保留 master 以处理 resize。
        let master = pair.master;

        // 为了让 “Kill / 超时” 能可靠打断 reader.read()，把 master fd 设为 nonblocking。
        // portable-pty 的 try_clone_reader() 通常基于 dup；O_NONBLOCK 属于 open-file-description，
        // 对 dup 后的 reader 同样生效。
        #[cfg(unix)]
        if let Some(fd) = master.as_raw_fd().map(|v| v as i32) {
            set_fd_nonblocking(fd, true);
        }

        let mut reader = match master.try_clone_reader() {
            Ok(r) => r,
            Err(e) => {
                let outcome = ToolOutcome {
                    user_message: format!("PTY reader failed: {e:#}"),
                    log_lines: vec![format!("状态:pty_reader_failed | cwd:{cwd_display}")],
                };
                let _ = tx.send(AsyncEvent::ToolStreamEnd {
                    outcome,
                    sys_msg: Some("工具执行失败".to_string()),
                });
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
                let _ = tx.send(AsyncEvent::ToolStreamEnd {
                    outcome,
                    sys_msg: Some("工具执行失败".to_string()),
                });
                return;
            }
        };

        // 子进程与控制线程共享：支持 Kill / Wait。
        let child: Arc<Mutex<Option<Box<dyn portable_pty::Child + Send + Sync>>>> =
            Arc::new(Mutex::new(Some(child)));
	        let done = Arc::new(AtomicBool::new(false));
	        let done2 = done.clone();
	        let child2 = child.clone();
	        let timed_out = Arc::new(AtomicBool::new(false));
	        let user_aborted = Arc::new(AtomicBool::new(false));
	        let user_aborted2 = user_aborted.clone();

        // ctrl 线程：stdin / resize / kill
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
	                        if let Ok(mut guard) = child2.lock() {
	                            if let Some(ch) = guard.as_mut() {
	                                // 尽力“杀整棵树”：避免只杀 bash 父进程导致子进程（如 codex/python）残留，
	                                // 从而读端不 EOF、界面看起来一直“还在跑”。
                                        #[cfg(unix)]
	                                if let Some(pgrp) =
	                                    master.process_group_leader().map(|v| v as i32)
	                                {
	                                    // kill process group
	                                    unsafe { libc::kill(-pgrp, libc::SIGKILL) };
	                                }
	                                if let Some(pid) = ch.process_id().map(|v| v as i32) {
	                                    kill_process_tree(pid, libc::SIGKILL, 0);
	                                }
	                                let _ = ch.kill();
	                            }
	                        }
                            // 重要：立即退出 ctrl 线程，drop writer/master，给 slave 侧制造 EOF/HUP，
                            // 以提升“退出 PTY 后工具能快速收尾”的确定性。
                            break 'ctrl;
	                    }
	                }
	            }
	        });

        // 超时：交互态默认不自动终止（timeout=0）。若显式给出，则到点触发 kill。
        if timeout_secs > 0 {
            let tx_kill = ctrl_tx.clone();
            let timed_out2 = timed_out.clone();
            thread::spawn(move || {
                thread::sleep(Duration::from_secs(timeout_secs));
                timed_out2.store(true, Ordering::Relaxed);
                let _ = tx_kill.send(PtyControl::Kill);
            });
        }

        // reader：持续刷出增量（同时落盘到 bash-cache）
        let mut buf = [0u8; 8192];
        let mut kill_grace_started_at: Option<Instant> = None;
        loop {
            match reader.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => {
                    let chunk = buf[..n].to_vec();
                    if let Some(f) = file.as_mut() {
                        let _ = f.write_all(&chunk);
                        let _ = f.flush();
                    }
                    let _ = tx.send(AsyncEvent::PtyOutput { bytes: chunk });
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    // nonblocking：没有数据时让出 CPU，并在 kill/timeout 后做 bounded 收尾。
                    if user_aborted.load(Ordering::Relaxed) || timed_out.load(Ordering::Relaxed) {
                        if kill_grace_started_at.is_none() {
                            kill_grace_started_at = Some(Instant::now());
                        }
                        // 若子进程已退出（try_wait），或 kill grace 超时，则退出 reader loop。
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

        // wait + 收尾
        let code = if let Ok(mut guard) = child.lock() {
            if let Some(mut ch) = guard.take() {
                // 若已触发 kill/timeout：bounded wait，避免卡死在 .wait()
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
	        let status = if timed_out.load(Ordering::Relaxed) {
	            "timeout".to_string()
	        } else if user_exit {
	            // 用户按 Esc/Ctrl+[ 主动结束交互：不视为失败（避免工具行显示“（失败）”）。
	            "done".to_string()
	        } else {
	            code.to_string()
	        };
	        let mut log_lines = vec![format!(
	            "状态:{status} | exit:{code} | 耗时:{elapsed_ms}ms | cwd:{cwd_display} | saved:{saved_path}"
	        )];
	        if user_exit {
	            log_lines.push("note:user_exit".to_string());
	        }
	        let outcome = ToolOutcome {
	            user_message: String::new(),
	            log_lines,
	        };
	        let _ = tx.send(AsyncEvent::ToolStreamEnd {
	            outcome,
	            sys_msg: Some(if timed_out.load(Ordering::Relaxed) {
	                format!("交互终端结束（timeout，exit:{code}）")
	            } else if user_exit {
	                "交互终端结束（用户退出）".to_string()
	            } else {
	                format!("交互终端结束（exit:{code}）")
	            }),
	        });
    });
}

struct TryStartNextToolArgs<'a> {
    pending_tools: &'a mut VecDeque<ToolCall>,
    pending_tool_confirm: &'a mut Option<(ToolCall, String, usize)>,
    tx: mpsc::Sender<AsyncEvent>,
    core: &'a mut Core,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    mode: &'a mut Mode,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    sys_cfg: &'a SystemConfig,
    owner: MindKind,
}

fn try_start_next_tool(args: TryStartNextToolArgs<'_>) -> bool {
    let TryStartNextToolArgs {
        pending_tools,
        pending_tool_confirm,
        tx,
        core,
        meta,
        context_usage,
        mode,
        sys_log,
        sys_log_limit,
        sys_cfg,
        owner,
    } = args;
    if matches!(*mode, Mode::ExecutingTool | Mode::ApprovingTool) {
        return true;
    }
    if pending_tool_confirm.is_some() {
        return true;
    }
    let Some(call) = pending_tools.pop_front() else {
        return false;
    };
    if !sys_cfg.tool_full_access && let Some(reason) = tool_requires_confirmation(&call) {
        let prompt = tool_confirm_prompt(&reason, &call);
        let msg_idx = core.history.len();
        push_system_and_log(core, meta, context_usage, Some("tool"), &prompt);
        *pending_tool_confirm = Some((call, reason, msg_idx));
        *mode = Mode::ApprovingTool;
        push_sys_log(sys_log, sys_log_limit, "等待工具确认");
        return true;
    }
    spawn_tool_execution(call, owner, tx);
    true
}

fn normalize_confirm_input(s: &str) -> Option<bool> {
    let t = s.trim().to_ascii_lowercase();
    if t == "y" || t == "yes" || t == "确认" || t == "是" {
        Some(true)
    } else if t == "n" || t == "no" || t == "取消" || t == "否" {
        Some(false)
    } else {
        None
    }
}

fn build_tool_stream_message(state: &ToolStreamState) -> String {
    // 工具消息在聊天区“可观察/可收敛”：
    // - 执行中：展示可读摘要（包含 explain/input），output 用占位符
    // - 执行完：展示截断后的 output/meta（由 UI 决定是否展开/收敛）
    let mut meta = state.meta.clone();
    if meta.is_empty() {
        meta.push("状态:running".to_string());
    }
    let outcome = crate::mcp::ToolOutcome {
        user_message: state.output.clone(),
        log_lines: meta,
    };
    crate::mcp::format_tool_message_raw(&state.call, &outcome)
}

fn is_chat_message_selectable(
    msg_idx: usize,
    msg: &Message,
    thinking_idx: Option<usize>,
    expanded_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> bool {
    if msg.text.trim().is_empty() {
        return false;
    }
    if msg.role == Role::Assistant && msg.text.trim().is_empty() {
        return false;
    }
    if msg.role == Role::System && msg.text.starts_with(THINKING_MARKER) {
        return false;
    }
    if msg.role == Role::Tool {
        let head = msg.text.trim_start();
        let lower = head.to_ascii_lowercase();
        let is_think = lower.starts_with("操作: think") || lower.starts_with("[tool:think]");
        if is_think
            && !details_mode
            && !expanded_thinking_idxs.contains(&msg_idx)
            && thinking_idx != Some(msg_idx)
        {
            return false;
        }
    }
    true
}

fn is_think_tool_message(msg: &Message) -> bool {
    if msg.role != Role::Tool {
        return false;
    }
    let lower = msg.text.trim_start().to_ascii_lowercase();
    lower.starts_with("操作: think") || lower.starts_with("[tool:think]")
}

fn find_think_message_for_selection(core: &Core, selected_idx: usize) -> Option<usize> {
    if core
        .history
        .get(selected_idx)
        .is_some_and(|m| is_think_tool_message(m))
    {
        return Some(selected_idx);
    }
    for idx in (0..selected_idx).rev() {
        let msg = core.history.get(idx)?;
        if is_think_tool_message(msg) {
            return Some(idx);
        }
        if matches!(msg.role, Role::User | Role::Assistant) {
            break;
        }
    }
    None
}

fn select_last_chat_message(
    core: &Core,
    thinking_idx: Option<usize>,
    expanded_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    for (idx, msg) in core.history.iter().enumerate().rev() {
        if is_chat_message_selectable(idx, msg, thinking_idx, expanded_thinking_idxs, details_mode) {
            return Some(idx);
        }
    }
    None
}

fn select_prev_chat_message(
    core: &Core,
    cur: Option<usize>,
    thinking_idx: Option<usize>,
    expanded_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    let start = cur.unwrap_or(core.history.len());
    for idx in (0..start).rev() {
        if let Some(msg) = core.history.get(idx)
            && is_chat_message_selectable(idx, msg, thinking_idx, expanded_thinking_idxs, details_mode)
        {
            return Some(idx);
        }
    }
    cur.or_else(|| select_last_chat_message(core, thinking_idx, expanded_thinking_idxs, details_mode))
}

fn select_next_chat_message(
    core: &Core,
    cur: Option<usize>,
    thinking_idx: Option<usize>,
    expanded_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    if core.history.is_empty() {
        return None;
    }
    let start = cur.map(|v| v.saturating_add(1)).unwrap_or(0);
    for idx in start..core.history.len() {
        if let Some(msg) = core.history.get(idx)
            && is_chat_message_selectable(idx, msg, thinking_idx, expanded_thinking_idxs, details_mode)
        {
            return Some(idx);
        }
    }
    cur.or_else(|| select_last_chat_message(core, thinking_idx, expanded_thinking_idxs, details_mode))
}

fn ensure_selected_visible(
    scroll: &mut u16,
    follow_bottom: &mut bool,
    selected_msg_idx: Option<usize>,
    msg_line_ranges: &[Option<(usize, usize)>],
    chat_height: usize,
    max_scroll_cache: usize,
) {
    let Some(sel) = selected_msg_idx else {
        return;
    };
    let Some((start, end)) = msg_line_ranges.get(sel).copied().flatten() else {
        return;
    };
    let h = chat_height.max(1);
    let view_start = (*scroll as usize).min(max_scroll_cache);
    let view_end = view_start.saturating_add(h);
    if start < view_start {
        *scroll = start.min(max_scroll_cache) as u16;
        *follow_bottom = false;
        return;
    }
    if end > view_end {
        let next = end.saturating_sub(h).min(max_scroll_cache);
        *scroll = next as u16;
        *follow_bottom = false;
    }
}

fn try_termux_clipboard_set(text: &str) -> bool {
    let Ok(mut child) = Command::new("termux-clipboard-set")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
    else {
        return false;
    };
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(text.as_bytes());
    }
    child.wait().is_ok()
}

#[derive(Debug, Default)]
struct StreamingState {
    idx: Option<usize>,
    raw_text: String,
    text: String,
    has_content: bool,
    tool_start: Option<usize>,
}

impl StreamingState {
    fn reset(&mut self) {
        self.idx = None;
        self.raw_text.clear();
        self.text.clear();
        self.has_content = false;
        self.tool_start = None;
    }

    fn start(&mut self, idx: usize) {
        self.idx = Some(idx);
        self.raw_text.clear();
        self.text.clear();
        self.has_content = false;
        self.tool_start = None;
    }

    fn append_content(&mut self, chunk: &str) -> bool {
        if chunk.is_empty() {
            return false;
        }
        let was_empty = !self.has_content;
        self.raw_text.push_str(chunk);
        self.has_content = true;
        was_empty
    }

    fn adjust_index(&mut self, removed: usize) {
        if let Some(value) = self.idx {
            if value < removed {
                self.idx = None;
            } else {
                self.idx = Some(value.saturating_sub(removed));
            }
        }
    }
}

fn push_tool_stream_chunk(state: &mut ToolStreamState, chunk: &str) {
    if chunk.is_empty() {
        return;
    }
    if state.placeholder {
        state.output.clear();
        state.output.push_str(chunk);
        state.placeholder = false;
    } else {
        state.output.push_str(chunk);
    }
}

const THINK_OUTPUT_MAX_LINES: usize = 240;
const THINK_OUTPUT_MAX_CHARS: usize = 12_000;

fn truncate_thinking_payload(text: &str, max_lines: usize, max_chars: usize) -> String {
    if text.is_empty() {
        return String::new();
    }
    let lines: Vec<&str> = text.split('\n').collect();
    let mut truncated = false;
    let mut body = if lines.len() > max_lines {
        truncated = true;
        let head = max_lines / 2;
        let tail = max_lines.saturating_sub(head).max(1);
        let mut out = String::new();
        if head > 0 {
            out.push_str(&lines[..head].join("\n"));
            out.push('\n');
        }
        out.push_str("...\n");
        out.push_str(&lines[lines.len().saturating_sub(tail)..].join("\n"));
        out
    } else {
        text.to_string()
    };

    if body.chars().count() > max_chars {
        truncated = true;
        let head = max_chars / 2;
        let tail = max_chars.saturating_sub(head).max(1);
        let head_str: String = body.chars().take(head).collect();
        let tail_str: String = body
            .chars()
            .rev()
            .take(tail)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        body = format!("{head_str}\n...\n{tail_str}");
    }

    if truncated {
        body.push_str(&format!(
            "\n\n[输出已截断：保留头尾，最多 {max_lines} 行 / {max_chars} 字符]"
        ));
    }
    body
}

fn build_thinking_summary(kind: MindKind, secs: u64) -> String {
    match kind {
        MindKind::Main => String::new(),
        MindKind::Sub => pick_thinking_word_dog(secs),
    }
}

fn build_thinking_tool_message_stub(kind: MindKind, status: &str, input: &str) -> String {
    let mut msg = String::new();
    msg.push_str("操作: Think\n");
    msg.push_str(&format!("input: {}\n", input.trim()));
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    msg.push_str(&format!("mind: {mind}\n"));
    msg.push_str("meta:\n```text\n");
    msg.push_str(&format!("状态:{status}"));
    msg.push_str("\n```\n");
    msg
}

fn next_lcg(seed: &AtomicU64) -> u64 {
    let mut value = seed.load(Ordering::Relaxed);
    if value == 0 {
        value = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(1)
            .max(1);
    }
    value = value.wrapping_mul(6364136223846793005).wrapping_add(1);
    seed.store(value, Ordering::Relaxed);
    value
}

fn pick_thinking_word_dog(secs: u64) -> String {
    let secs = secs.max(1);
    let templates: [&str; 10] = [
        "Thought for {secs}s.",
        "Done in {secs}s.",
        "Finished in {secs}s.",
        "Reasoned for {secs}s.",
        "Thinking took {secs}s.",
        "Solved in {secs}s.",
        "Wrapped in {secs}s.",
        "Processed in {secs}s.",
        "Sorted in {secs}s.",
        "Ready in {secs}s.",
    ];
    static SEED: AtomicU64 = AtomicU64::new(0);
    let idx = (next_lcg(&SEED) as usize) % templates.len();
    templates[idx].replace("{secs}", &secs.to_string())
}

fn build_interrupt_prompt() -> String {
    "请求已被取消".to_string()
}

fn build_switch_prompt(target: MindKind) -> String {
    match target {
        MindKind::Main => "··· Switch to 萤·主意识".to_string(),
        MindKind::Sub => "··· Switch to 萤·潜意识".to_string(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiaryStage {
    Idle,
    WaitingMain,
}

#[derive(Debug)]
struct DiaryState {
    stage: DiaryStage,
}

impl DiaryState {
    fn new() -> Self {
        Self {
            stage: DiaryStage::Idle,
        }
    }

    fn active(&self) -> bool {
        !matches!(self.stage, DiaryStage::Idle)
    }
}

fn build_main_diary_prompt(
    prompt_override: &str,
    system_prompt: &str,
    last_diary: &str,
    context_text: &str,
) -> String {
    let default_prompt = "系统：context 已满，需要进行上下文压缩。\
请你根据当前记忆生成日记，并直接调用 memory_add 写入 datememo。\
系统会附带上一条日记用于对比，请避免重复。仅输出工具 JSON，不要附加说明。\
格式：<tool>{\"tool\":\"memory_add\",\"path\":\"datememo\",\"content\":\"YYYY-MM-DD HH:MM:SS | main | 关键词: ... | 日记: ...\",\"brief\":\"我来更新日记。\"}</tool>";
    let mut out = if prompt_override.trim().is_empty() {
        default_prompt.to_string()
    } else {
        prompt_override.trim().to_string()
    };
    if !system_prompt.trim().is_empty() {
        out.push_str("\n\n[当前系统提示词]\n");
        out.push_str(system_prompt.trim());
        out.push('\n');
    }
    if !context_text.trim().is_empty() {
        out.push_str("\n[上下文记录]\n");
        out.push_str(context_text.trim());
        out.push('\n');
    }
    if !last_diary.trim().is_empty() {
        if !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str("\n[上一条日记]\n");
        out.push_str(last_diary.trim());
        out.push('\n');
    }
    out
}

fn extract_thinking_tags(text: &str) -> (String, String) {
    let mut thinking = String::new();
    let mut cleaned = String::new();
    let mut rest = text;
    loop {
        let Some(start) = rest.find("<thinking>") else {
            cleaned.push_str(rest);
            break;
        };
        let (before, after) = rest.split_at(start);
        cleaned.push_str(before);
        let after = &after["<thinking>".len()..];
        let Some(end) = after.find("</thinking>") else {
            cleaned.push_str("<thinking>");
            cleaned.push_str(after);
            break;
        };
        let (block, tail) = after.split_at(end);
        let chunk = block.trim();
        if !chunk.is_empty() {
            if !thinking.is_empty() {
                thinking.push('\n');
            }
            thinking.push_str(chunk);
        }
        rest = &tail["</thinking>".len()..];
    }
    (thinking, cleaned)
}

fn read_last_datememo_entry(memo_db: &Option<MemoDb>) -> String {
    memo_db
        .as_ref()
        .and_then(|db| db.read_last_entry(MemoKind::Date).ok().flatten())
        .unwrap_or_default()
}

fn build_thinking_tool_message(
    summary: &str,
    full_text: &str,
    secs: u64,
    chars: usize,
    kind: MindKind,
) -> String {
    let clean = full_text.trim();
    if clean.is_empty() {
        return String::new();
    }
    let mut msg = String::new();
    msg.push_str("操作: Think\n");
    msg.push_str(&format!("input: {}\n", summary.trim()));
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    msg.push_str(&format!("mind: {mind}\n"));
    msg.push_str("output:\n```text\n");
    let body = truncate_thinking_payload(clean, THINK_OUTPUT_MAX_LINES, THINK_OUTPUT_MAX_CHARS);
    msg.push_str(&body);
    msg.push_str("\n```\n");
    msg.push_str("meta:\n```text\n");
    msg.push_str(&format!("状态:0 | 耗时:{secs}s | chars:{chars}"));
    msg.push_str("\n```\n");
    msg
}

fn build_thinking_tool_message_running(kind: MindKind, full_text: &str) -> String {
    let clean = full_text.trim();
    if clean.is_empty() {
        return build_thinking_tool_message_stub(kind, "running", "");
    }
    let mut msg = String::new();
    msg.push_str("操作: Think\n");
    msg.push_str("input:\n");
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    msg.push_str(&format!("mind: {mind}\n"));
    msg.push_str("output:\n```text\n");
    let body = truncate_thinking_payload(clean, THINK_OUTPUT_MAX_LINES, THINK_OUTPUT_MAX_CHARS);
    msg.push_str(&body);
    msg.push_str("\n```\n");
    msg.push_str("meta:\n```text\n");
    msg.push_str("状态:running");
    msg.push_str("\n```\n");
    msg
}

struct RemoveMessageAtArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    idx: usize,
    reveal_idx: &'a mut Option<usize>,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    thinking_idx: &'a mut Option<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview_chat_idx: &'a mut Option<usize>,
    streaming_state: &'a mut StreamingState,
    active_tool_stream: &'a mut Option<ToolStreamState>,
}

fn remove_message_at(args: RemoveMessageAtArgs<'_>) {
    let RemoveMessageAtArgs {
        core,
        render_cache,
        idx,
        reveal_idx,
        expanded_tool_idxs,
        thinking_idx,
        expanded_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
        streaming_state,
        active_tool_stream,
    } = args;
    fn pull_index(idx_opt: &mut Option<usize>, removed_at: usize) {
        if let Some(value) = *idx_opt {
            if value == removed_at {
                *idx_opt = None;
            } else if value > removed_at {
                *idx_opt = Some(value.saturating_sub(1));
            }
        }
    }
    fn pull_index_set(set: &mut BTreeSet<usize>, removed_at: usize) {
        if set.is_empty() {
            return;
        }
        let mut next: BTreeSet<usize> = BTreeSet::new();
        for v in set.iter().copied() {
            if v == removed_at {
                continue;
            }
            if v > removed_at {
                next.insert(v.saturating_sub(1));
            } else {
                next.insert(v);
            }
        }
        *set = next;
    }
    if idx >= core.history.len() {
        return;
    }
    core.history.remove(idx);
    *render_cache = ui::ChatRenderCache::new();
    pull_index(reveal_idx, idx);
    pull_index_set(expanded_tool_idxs, idx);
    pull_index(thinking_idx, idx);
    pull_index_set(expanded_thinking_idxs, idx);
    pull_index(selected_msg_idx, idx);
    pull_index(tool_preview_chat_idx, idx);
    if let Some(v) = streaming_state.idx {
        if v == idx {
            streaming_state.idx = None;
        } else if v > idx {
            streaming_state.idx = Some(v.saturating_sub(1));
        }
    }
    if let Some(state) = active_tool_stream.as_mut() {
        if state.idx == idx {
            *active_tool_stream = None;
        } else if state.idx > idx {
            state.idx = state.idx.saturating_sub(1);
        }
    }
}

struct ClearThinkingStateArgs<'a> {
    thinking_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_scroll: &'a mut usize,
    thinking_scroll_cap: &'a mut usize,
    thinking_full_text: &'a mut String,
    thinking_started_at: &'a mut Option<Instant>,
}

fn clear_thinking_state(args: ClearThinkingStateArgs<'_>) {
    let ClearThinkingStateArgs {
        thinking_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_scroll,
        thinking_scroll_cap,
        thinking_full_text,
        thinking_started_at,
    } = args;
    thinking_text.clear();
    thinking_full_text.clear();
    *thinking_started_at = None;
    *thinking_idx = None;
    *thinking_in_progress = false;
    *thinking_pending_idle = false;
    *thinking_scroll = 0;
    *thinking_scroll_cap = 0;
}

fn clear_tool_preview_state(
    tool_preview: &mut String,
    tool_preview_active: &mut bool,
    tool_preview_pending_idle: &mut bool,
) {
    tool_preview.clear();
    *tool_preview_active = false;
    *tool_preview_pending_idle = false;
}

struct ResetAfterCommandArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    config: &'a AppConfig,
    reveal_idx: &'a mut Option<usize>,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    ctx_compact_notice_idx: &'a mut Option<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview_chat_idx: &'a mut Option<usize>,
    thinking_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_scroll: &'a mut usize,
    thinking_scroll_cap: &'a mut usize,
    thinking_full_text: &'a mut String,
    thinking_started_at: &'a mut Option<Instant>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    streaming_state: &'a mut StreamingState,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    screen: Screen,
    settings: &'a mut SettingsState,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
}

fn reset_after_command(args: ResetAfterCommandArgs<'_>) {
    let ResetAfterCommandArgs {
        core,
        render_cache,
        config,
        reveal_idx,
        expanded_tool_idxs,
        ctx_compact_notice_idx,
        expanded_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
        thinking_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_scroll,
        thinking_scroll_cap,
        thinking_full_text,
        thinking_started_at,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        streaming_state,
        active_tool_stream,
        screen,
        settings,
        scroll,
        follow_bottom,
    } = args;
    expanded_tool_idxs.clear();
    expanded_thinking_idxs.clear();
    clear_thinking_state(ClearThinkingStateArgs {
        thinking_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_scroll,
        thinking_scroll_cap,
        thinking_full_text,
        thinking_started_at,
    });
    clear_tool_preview_state(tool_preview, tool_preview_active, tool_preview_pending_idle);
    if matches!(screen, Screen::Settings) {
        reset_settings_to_tabs(settings);
    }
    jump_to_bottom(scroll, follow_bottom);
    prune_history_if_needed(PruneHistoryArgs {
        core,
        render_cache,
        config,
        reveal_idx,
        expanded_tool_idxs,
        thinking_idx,
        expanded_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
        ctx_compact_notice_idx,
        streaming_state,
        active_tool_stream,
    });
}

fn jump_to_bottom(scroll: &mut u16, follow_bottom: &mut bool) {
    *scroll = u16::MAX;
    *follow_bottom = true;
}

fn event_request_id(ev: &AsyncEvent) -> Option<u64> {
    match ev {
        AsyncEvent::ModelStreamStart { request_id, .. } => Some(*request_id),
        AsyncEvent::ModelStreamChunk { request_id, .. } => Some(*request_id),
        AsyncEvent::ModelStreamEnd { request_id, .. } => Some(*request_id),
        AsyncEvent::ErrorRetry { request_id, .. } => Some(*request_id),
        _ => None,
    }
}

#[derive(Debug)]
enum AsyncEvent {
    ModelStreamStart {
        kind: MindKind,
        request_id: u64,
    },
    ModelStreamChunk {
        content: String,
        reasoning: String,
        request_id: u64,
    },
    ModelStreamEnd {
        kind: MindKind,
        usage: u64,
        error: Option<String>,
        request_id: u64,
    },
    ErrorRetry {
        attempt: usize,
        max: usize,
        request_id: u64,
    },
    ToolStreamStart {
        owner: MindKind,
        call: Box<ToolCall>,
        sys_msg: Option<String>,
    },
    ToolStreamEnd {
        outcome: ToolOutcome,
        sys_msg: Option<String>,
    },
    PtyReady {
        ctrl_tx: mpsc::Sender<PtyControl>,
        cols: u16,
        rows: u16,
        saved_path: String,
    },
    PtyOutput {
        bytes: Vec<u8>,
    },
}

fn arm_parent_death_signal() {
    // Android/Termux 同样是 Linux 内核，但 Rust 的 target_os 为 "android"。
    // 若不设置 PDEATHSIG，父进程（zsh/login）退出后，TUI 可能变成孤儿进程并继续烧 CPU。
    #[cfg(any(target_os = "linux", target_os = "android"))]
    unsafe {
        libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGTERM);
    }
    // 竞态兜底：如果父进程已在 prctl 前退出，PDEATHSIG 不会再触发，这里主动退出。
    #[cfg(any(target_os = "linux", target_os = "android"))]
    {
        if unsafe { libc::getppid() } == 1 {
            std::process::exit(0);
        }
    }
}

fn setup_exit_flag() -> anyhow::Result<Arc<AtomicBool>> {
    let flag = Arc::new(AtomicBool::new(false));
    let mut signals = Signals::new([SIGINT, SIGTERM, SIGHUP, SIGQUIT])?;
    let flag_clone = flag.clone();
    thread::spawn(move || {
        for _ in signals.forever() {
            flag_clone.store(true, Ordering::Relaxed);
        }
    });
    Ok(flag)
}

fn tty_is_alive() -> bool {
    if std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/tty")
        .is_ok()
    {
        return true;
    }
    // 兜底：某些环境 /dev/tty 可能不可用，但至少要确保 stdio 是终端。
    use std::io::IsTerminal;
    io::stdin().is_terminal() && io::stdout().is_terminal()
}

fn write_termux_cmd_banner() {
    let mut out = io::stdout();
    let (cols, _) = crossterm::terminal::size().unwrap_or((80, 24));
    let _ = execute!(out, Clear(ClearType::All));

    let art: [&str; 6] = [
        "████████╗███████╗██████╗ ███╗   ███╗██╗   ██╗██╗  ██╗",
        "╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║   ██║╚██╗██╔╝",
        "   ██║   █████╗  ██████╔╝██╔████╔██║██║   ██║ ╚███╔╝ ",
        "   ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║   ██║ ██╔██╗ ",
        "   ██║   ███████╗██║  ██║██║ ╚═╝ ██║╚██████╔╝██╔╝ ██╗",
        "   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝",
    ];
    let art_w = art.iter().map(|s| s.chars().count()).max().unwrap_or(0);
    let cols_usize = cols.max(1) as usize;
    let left_pad = cols_usize.saturating_sub(art_w) / 2;

    let _ = writeln!(out);
    for line in art {
        let _ = writeln!(out, "{}{}", " ".repeat(left_pad), line);
    }
    let tip = "tips：输入 /AI 回到主面板";
    let _ = writeln!(out, "\n{}{}", " ".repeat(left_pad), tip);
    let _ = writeln!(out);
    let _ = out.flush();
}

fn run_native_shell(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> anyhow::Result<()> {
    disable_raw_mode().ok();
    execute!(
        terminal.backend_mut(),
        DisableBracketedPaste,
        LeaveAlternateScreen,
        crossterm::cursor::Show
    )
    .ok();

    write_termux_cmd_banner();

    let shell = std::env::var("SHELL")
        .unwrap_or_else(|_| "/data/data/com.termux/files/usr/bin/zsh".to_string());
    let home =
        std::env::var("HOME").unwrap_or_else(|_| "/data/data/com.termux/files/home".to_string());
    let status = Command::new(shell)
        .arg("-il")
        .current_dir(&home)
        .env("AITERMUX_DISABLE", "1")
        .env("AITERMUX_CMD_SHELL", "1")
        .env("AITERMUX_STARTED", "1")
        .status();

    enable_raw_mode().context("enable_raw_mode failed")?;
    execute!(
        terminal.backend_mut(),
        EnterAlternateScreen,
        EnableBracketedPaste
    )
    .ok();
    terminal.clear().ok();

    match status {
        Ok(_) => Ok(()),
        Err(e) => Err(anyhow::anyhow!("启动 shell 失败：{e}")),
    }
}

pub fn run() -> anyhow::Result<i32> {
    arm_parent_death_signal();
    let exit_flag = setup_exit_flag()?;
    if !tty_is_alive() {
        return Ok(0);
    }
    let mut stdout = io::stdout();
    enable_raw_mode().context("enable_raw_mode failed")?;
    execute!(stdout, EnterAlternateScreen, EnableBracketedPaste)?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    execute!(terminal.backend_mut(), Clear(ClearType::All)).ok();
    terminal.clear().ok();

    let res = run_loop(&mut terminal, exit_flag);

    disable_raw_mode().ok();
    execute!(
        terminal.backend_mut(),
        DisableBracketedPaste,
        LeaveAlternateScreen
    )
    .ok();
    execute!(io::stdout(), crossterm::cursor::Show).ok();

    res
}

struct PollTimeoutArgs<'a> {
    now: Instant,
    config: &'a AppConfig,
    anim_enabled: bool,
    last_anim_at: Instant,
    toast: &'a Option<(Instant, String)>,
    paste_capture: &'a Option<PasteCapture>,
    header_sys_line: &'a str,
    sys_scroll_until: Option<Instant>,
    last_sys_scroll: Instant,
    reveal_idx: &'a Option<usize>,
    last_reveal_at: Instant,
}

fn compute_poll_timeout(args: PollTimeoutArgs<'_>) -> Duration {
    let PollTimeoutArgs {
        now,
        config,
        anim_enabled,
        last_anim_at,
        toast,
        paste_capture,
        header_sys_line,
        sys_scroll_until,
        last_sys_scroll,
        reveal_idx,
        last_reveal_at,
    } = args;

    let mut poll_timeout = Duration::from_secs(3600);
    if anim_enabled {
        let elapsed = now.saturating_duration_since(last_anim_at);
        let frame = Duration::from_millis(config.active_frame_ms);
        poll_timeout = poll_timeout.min(frame.saturating_sub(elapsed));
    }
    if let Some((until, _)) = toast.as_ref() {
        poll_timeout = poll_timeout.min(until.saturating_duration_since(now));
    }
    if let Some(c) = paste_capture.as_ref() {
        let due = c.last_at + Duration::from_millis(config.paste_capture_flush_gap_ms);
        poll_timeout = poll_timeout.min(due.saturating_duration_since(now));
    }
    if anim_enabled
        && !header_sys_line.trim().is_empty()
        && sys_scroll_until.is_some_and(|t| now < t)
    {
        let due = last_sys_scroll + Duration::from_millis(config.sys_scroll_ms);
        poll_timeout = poll_timeout.min(due.saturating_duration_since(now));
    }
    if anim_enabled && reveal_idx.is_some() {
        let due = last_reveal_at + Duration::from_millis(config.reveal_frame_ms);
        poll_timeout = poll_timeout.min(due.saturating_duration_since(now));
    }
    poll_timeout.min(Duration::from_millis(config.exit_poll_ms))
}

fn should_ignore_request(active_request_id: &Option<u64>, request_id: u64) -> bool {
    !active_request_id.is_some_and(|v| v == request_id)
}

struct ModelStreamStartArgs<'a> {
    core: &'a mut Core,
    kind: MindKind,
    request_id: u64,
    heartbeat_request_id: &'a Option<u64>,
    sse_enabled: bool,
    retry_status: &'a mut Option<String>,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    active_kind: &'a mut MindKind,
    thinking_idx: &'a mut Option<usize>,
    streaming_state: &'a mut StreamingState,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_started_at: &'a mut Option<Instant>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    mode: &'a mut Mode,
    reveal_idx: &'a mut Option<usize>,
    reveal_len: &'a mut usize,
}

fn handle_model_stream_start(args: ModelStreamStartArgs<'_>) {
    let ModelStreamStartArgs {
        core,
        kind,
        request_id,
        heartbeat_request_id,
        sse_enabled,
        retry_status,
        active_tool_stream,
        active_kind,
        thinking_idx,
        streaming_state,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        thinking_text,
        thinking_full_text,
        thinking_started_at,
        thinking_in_progress,
        thinking_pending_idle,
        mode,
        reveal_idx,
        reveal_len,
    } = args;

    let is_heartbeat = heartbeat_request_id
        .as_ref()
        .is_some_and(|v| *v == request_id);
    runlog_event(
        "INFO",
        "model.stream.start",
        json!({
            "mind": mind_label(kind),
            "request_id": request_id,
            "is_heartbeat": is_heartbeat,
            "sse_enabled": sse_enabled,
        }),
    );
    *retry_status = None;
    *active_tool_stream = None;
    // 在请求启动前由发起方设置（mind 协同/普通对话/心跳/写日记）；这里不改写标志。
    // 以实际开始流式输出的 mind 为准，避免顶栏“亮错边”（例如主意识发起的工具/请求）。
    *active_kind = kind;
    // 思考占位：仅在 SSE 流式时需要（用于首包前的 ● 闪烁）。
    // 非流式：不展示思考，思考只在详情模式回看（由 stream_end 推入 Tool 消息）。
    if sse_enabled && !is_heartbeat && thinking_idx.is_none() {
        let stub = build_thinking_tool_message_stub(kind, "running", "Thinking");
        core.push_tool(stub);
        *thinking_idx = Some(core.history.len().saturating_sub(1));
    }
    let assistant_idx = core.history.len();
    streaming_state.start(assistant_idx);
    core.history.push(Message {
        role: Role::Assistant,
        text: String::new(),
        mind: Some(kind),
    });
    tool_preview.clear();
    *tool_preview_active = false;
    *tool_preview_pending_idle = false;
    *tool_preview_chat_idx = None;
    thinking_text.clear();
    thinking_full_text.clear();
    *thinking_started_at = None;
    *thinking_in_progress = sse_enabled;
    *thinking_pending_idle = false;
    *mode = Mode::Generating;
    *reveal_idx = None;
    *reveal_len = 0;
}

struct ToolStreamStartArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    tool_preview_chat_idx: &'a mut Option<usize>,
    mind_pulse: &'a mut Option<MindPulse>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_idx: &'a Option<usize>,
    mode: &'a mut Mode,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
    owner: MindKind,
    call: Box<ToolCall>,
    sys_msg: Option<String>,
}

fn handle_tool_stream_start(args: ToolStreamStartArgs<'_>) {
    let ToolStreamStartArgs {
        core,
        render_cache,
        expanded_tool_idxs,
        active_tool_stream,
        tool_preview_chat_idx,
        mind_pulse,
        sys_log,
        sys_log_limit,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_idx,
        mode,
        scroll,
        follow_bottom,
        owner,
        call,
        sys_msg,
    } = args;

    runlog_event(
        "INFO",
        "tool.stream.start",
        json!({
            "owner": mind_label(owner),
            "call": tool_call_log_fields(&call),
            "sys_msg": sys_msg.as_deref(),
        }),
    );
    if let Some(msg) = sys_msg {
        push_sys_log(sys_log, sys_log_limit, msg);
    }
    if call.tool == "mind_msg"
        && let Some(target) = resolve_mind_target_kind(&call)
        && owner != target
    {
        let dir = pulse_dir(owner, target);
        *mind_pulse = Some(MindPulse {
            dir,
            // 迅速的一次性“乱码脉冲”，避免长时间持续动画造成干扰。
            until: Instant::now() + Duration::from_millis(240),
        });
    }
    let mut state = ToolStreamState {
        idx: 0,
        owner,
        call,
        output: "...".to_string(),
        meta: vec![],
        placeholder: true,
    };
    let tool_text = build_tool_stream_message(&state);
    if let Some(stub_idx) = tool_preview_chat_idx.take()
        && core
            .history
            .get(stub_idx)
            .is_some_and(|m| m.role == Role::Tool)
    {
        if let Some(entry) = core.history.get_mut(stub_idx) {
            entry.text.clone_from(&tool_text);
            render_cache.invalidate(stub_idx);
        }
        state.idx = stub_idx;
    } else {
        core.push_tool(tool_text.clone());
        state.idx = core.history.len().saturating_sub(1);
    }
    // 默认只展示 brief：工具详情由用户手动展开（→）。
    expanded_tool_idxs.remove(&state.idx);
    *active_tool_stream = Some(state);
    *thinking_in_progress = false;
    *thinking_pending_idle = thinking_idx.is_some();
    *mode = Mode::ExecutingTool;
    jump_to_bottom(scroll, follow_bottom);
}

struct ContextCompactToolArgs<'a> {
    state: &'a ToolStreamState,
    outcome: &'a mut ToolOutcome,
    main_state: &'a mut DogState,
    dog_state: &'a mut DogState,
    sys_cfg: &'a mut SystemConfig,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    ctx_compact_notice_idx: &'a mut Option<usize>,
}

fn handle_context_compact_tool(args: ContextCompactToolArgs<'_>) {
    let ContextCompactToolArgs {
        state,
        outcome,
        main_state,
        dog_state,
        sys_cfg,
        sys_log,
        sys_log_limit,
        core,
        render_cache,
        ctx_compact_notice_idx,
    } = args;

    let target = state
        .call
        .target
        .as_deref()
        .map(|s| s.trim().to_ascii_lowercase())
        .unwrap_or_else(|| {
            if matches!(state.owner, MindKind::Main) {
                "main".to_string()
            } else {
                "dog".to_string()
            }
        });
    let kind = if target == "main" {
        MindKind::Main
    } else {
        MindKind::Sub
    };
    let summary = state
        .call
        .content
        .as_deref()
        .unwrap_or(state.call.input.trim())
        .trim()
        .to_string();
    if summary.is_empty() {
        match kind {
            MindKind::Main => main_state.abort_context_compact(),
            MindKind::Sub => dog_state.abort_context_compact(),
        }
        outcome.user_message = "context_compact 失败：缺少 content".to_string();
        outcome
            .log_lines
            .push("状态:fail | reason:missing_content".to_string());
        update_ctx_compact_notice(
            core,
            render_cache,
            ctx_compact_notice_idx,
            "↻ CONTEXT SUMMARY 执行已中断",
        );
        return;
    }

    let (cleared_chat_tokens, pool_len) = match kind {
        MindKind::Main => main_state.apply_context_compact(sys_cfg, &summary),
        MindKind::Sub => dog_state.apply_context_compact(sys_cfg, &summary),
    };
    outcome.user_message = format!(
        "↻ context summary 已写入（池 {pool_len} 条），已清空 {cleared_chat_tokens} 聊天tokens"
    );
    outcome.log_lines.push(format!(
        "状态:0 | target:{} | pool:{} | cleared_chat_tokens:{}",
        if matches!(kind, MindKind::Main) {
            "main"
        } else {
            "dog"
        },
        pool_len,
        cleared_chat_tokens
    ));
    push_sys_log(sys_log, sys_log_limit, "↻ Context Summary 已更新");
    update_ctx_compact_notice(
        core,
        render_cache,
        ctx_compact_notice_idx,
        "↻ CONTEXT SUMMARY 已更新",
    );
}

struct MindMsgToolArgs<'a> {
    state: &'a ToolStreamState,
    pending_mind_half: &'a mut Option<PendingMindHalf>,
    main_state: &'a mut DogState,
    dog_state: &'a mut DogState,
    sys_cfg: &'a SystemConfig,
    config: &'a AppConfig,
    context_usage: &'a mut ContextUsage,
}

fn handle_mind_msg_tool(args: MindMsgToolArgs<'_>) -> Option<(MindKind, MindKind, String, String)> {
    let MindMsgToolArgs {
        state,
        pending_mind_half,
        main_state,
        dog_state,
        sys_cfg,
        config,
        context_usage,
    } = args;
    let target = resolve_mind_target_kind(&state.call)?;
    let content = resolve_mind_message_text(&state.call);
    if content.trim().is_empty() {
        return None;
    }
    let owner = state.owner;
    if owner == target {
        return None;
    }

    let brief = state
        .call
        .brief
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .unwrap_or("mind_msg")
        .to_string();
    let content_clean = content.trim().to_string();
    let brief_clean = brief.trim().to_string();

    if let Some(prev) = pending_mind_half.take() {
        let paired = prev.from == target && prev.to == owner;
        if paired {
            let item = format!(
                "{MIND_CTX_GUARD}\n协同记录（1轮）：\n- {}\n- {}\n",
                format_mind_half_line(prev.from, prev.to, &prev.brief, &prev.content),
                format_mind_half_line(owner, target, &brief_clean, &content_clean)
            );
            main_state.push_ctx_pool_item(sys_cfg, &item);
            dog_state.push_ctx_pool_item(sys_cfg, &item);
        } else {
            let item = format!(
                "{MIND_CTX_GUARD}\n协同记录（未完成一轮）：{}",
                format_mind_half_line(prev.from, prev.to, &prev.brief, &prev.content)
            );
            main_state.push_ctx_pool_item(sys_cfg, &item);
            dog_state.push_ctx_pool_item(sys_cfg, &item);
            *pending_mind_half = Some(PendingMindHalf {
                from: owner,
                to: target,
                brief: brief_clean.clone(),
                content: content_clean.clone(),
            });
        }
    } else {
        *pending_mind_half = Some(PendingMindHalf {
            from: owner,
            to: target,
            brief: brief_clean.clone(),
            content: content_clean.clone(),
        });
    }

    log_contextmemo(
        &config.contextmemo_path,
        context_usage,
        mind_context_speaker(owner, target),
        &content_clean,
    );

    Some((owner, target, brief_clean, content_clean))
}

fn handle_error_retry(retry_status: &mut Option<String>, attempt: usize, max: usize) {
    *retry_status = Some(format!("♡ 现在[萤]第{attempt}/{max}次重试中…"));
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    exit_flag: Arc<AtomicBool>,
) -> anyhow::Result<i32> {
    let mut core = Core::new();
    let mut config = AppConfig::from_env();
    let (mut dog_cfg, dog_cfg_err, dog_cfg_path) = load_dog_api_config();
    let (mut main_cfg, main_cfg_err, main_cfg_path) = load_main_api_config();
    let (mut sys_cfg, sys_cfg_err, sys_cfg_path) = load_system_config();
    let (mut context_prompts, context_prompt_errs, context_prompt_path) = load_context_prompts();
    if let Some(root) = config_root_dir(&dog_cfg_path) {
        config.metamemo_path = resolve_relative_path(&config.metamemo_path, &root);
        config.datememo_path = resolve_relative_path(&config.datememo_path, &root);
        config.contextmemo_path = resolve_relative_path(&config.contextmemo_path, &root);
        config.run_log_path = resolve_relative_path(&config.run_log_path, &root);
        config.memo_db_path = resolve_relative_path(&config.memo_db_path, &root);
        let _ = std::env::set_current_dir(&root);
    }
    init_run_logger(&config.run_log_path);
    runlog_event(
        "INFO",
        "app.start",
        json!({
            "cwd": std::env::current_dir().ok().map(|p| p.to_string_lossy().to_string()),
            "run_log_path": config.run_log_path,
            "metamemo_path": config.metamemo_path,
            "datememo_path": config.datememo_path,
            "contextmemo_path": config.contextmemo_path,
            "memo_db_path": config.memo_db_path,
            "version": env!("CARGO_PKG_VERSION"),
        }),
    );
    // set_var 在当前 toolchain 中为 unsafe，集中处理并降低散落风险。
    unsafe {
        std::env::set_var("YING_MEMO_DB_PATH", &config.memo_db_path);
        std::env::set_var("YING_METAMEMO_PATH", &config.metamemo_path);
        std::env::set_var("YING_DATEMEMO_PATH", &config.datememo_path);
    }
    let mut memo_db: Option<MemoDb> = None;
    let mut memo_db_err: Option<String> = None;
    match MemoDb::open(
        PathBuf::from(&config.memo_db_path),
        PathBuf::from(&config.metamemo_path),
        PathBuf::from(&config.datememo_path),
    ) {
        Ok(db) => memo_db = Some(db),
        Err(e) => memo_db_err = Some(format!("{e:#}")),
    }
    let mut metamemo: Option<MetaMemo> = None;
    let mut metamemo_err = memo_db_err.clone();
    if let Some(db) = memo_db.clone() {
        match MetaMemo::open(db) {
            Ok(m) => {
                metamemo = Some(m);
                metamemo_err = None;
            }
            Err(e) => metamemo_err = Some(format!("{e:#}")),
        }
    }
    let mut datememo_err = memo_db_err.clone();
    let mut dog_prompt_err: Option<String> = None;
    let dog_prompt = match load_prompt(Path::new(&dog_cfg.prompt_path)) {
        Ok(text) => text,
        Err(e) => {
            dog_prompt_err = Some(format!("{e:#}"));
            String::new()
        }
    };
    let mut main_prompt_err: Option<String> = None;
    let main_prompt = match load_prompt(Path::new(&main_cfg.prompt_path)) {
        Ok(text) => text,
        Err(e) => {
            main_prompt_err = Some(format!("{e:#}"));
            String::new()
        }
    };
    let (mut context_compact_prompt_text, context_compact_prompt_err, context_compact_prompt_path) =
        load_context_compact_prompt(&sys_cfg);
    let (fastmemo_compact_prompt_text, fastmemo_compact_prompt_err, _fastmemo_compact_prompt_path) =
        load_fastmemo_compact_prompt();
    let mut dog_state = DogState::new(dog_prompt, dog_cfg.prompt_reinject_pct);
    let mut main_prompt_text = main_prompt;
    let mut main_state = DogState::new(main_prompt_text.clone(), 80);
    // MAIN 也可能发起工具调用（记忆/系统配置等）。若不回注工具结果，模型会反复重试同一工具。
    main_state.set_include_tool_context(true);
    dog_state.reset_context();
    main_state.reset_context();
    let mut mind_context = MindContextPool::new(MIND_CTX_MAX_ROUNDS, MIND_CTX_MAX_TOKENS);
    let mut mind_ctx_idx_main: Option<usize> = None;
    let mut mind_ctx_idx_dog: Option<usize> = None;
    let mut mind_rate_window: VecDeque<Instant> = VecDeque::new();
    let mut active_request_is_mind: bool = false;
    let mut active_request_is_fastmemo_compact: bool = false;
    let mut auto_fastmemo_compact: bool = false;
    let mut fastmemo_compact_inflight: bool = false;
    let mut fastmemo_compact_edit_mask: u8 = 0;
    let mut fastmemo_compact_retry_at: Option<Instant> = None;
    let mut pending_mind_half: Option<PendingMindHalf> = None;
    let token_total_path = PathBuf::from(&dog_cfg.token_total_path);
    let mut token_totals = load_token_totals(&token_total_path);
    if token_totals.total_heartbeat_count == 0
        && token_totals.total_heartbeat_responses == 0
        && let Some((heartbeats, responses)) =
            backfill_heartbeat_totals_from_memo_db(&config.memo_db_path)
        && heartbeats > 0
    {
        token_totals.total_heartbeat_count = heartbeats;
        token_totals.total_heartbeat_responses = responses;
        token_totals.total_tokens = token_totals
            .total_in_tokens
            .saturating_add(token_totals.total_out_tokens);
        let _ = store_token_totals(&token_total_path, &token_totals);
    }
    let dog_client_result = DogClient::new(dog_cfg.clone());
    let mut dog_client = dog_client_result.as_ref().ok().cloned();
    let dog_client_err = dog_client_result.err().map(|e| e.to_string());
    let main_client_result = build_main_client(&main_cfg);
    let mut main_client = main_client_result.as_ref().ok().cloned();
    let main_client_err = main_client_result.err().map(|e| e.to_string());
    let theme = ui::Theme::from_env();
    let mut render_cache = ui::ChatRenderCache::new();
    let mut input = String::new();
    let mut cursor: usize = 0;
    let mut input_chars: usize = 0;
    let mut scroll: u16 = 0;
    let mut follow_bottom = true;
    let mut max_scroll_cache: usize = 0;
    let mut chat_width_cache: usize = 0;
    let status_width_cache: usize = 0;
    let mut settings_editor_width_cache: usize = 0;
    let mut input_width_cache: usize = 0;
    let mut mode = Mode::Idle;
    let mut sse_enabled = sys_cfg.sse_enabled;
    let mut request_seq: u64 = 0;
    let mut active_request_id: Option<u64> = None;
    let mut active_cancel: Option<Arc<AtomicBool>> = None;
    let mut active_request_in_tokens: Option<u64> = None;
    let mut spinner_tick: usize = 0;
    let mut chat_target = parse_chat_target(&sys_cfg.chat_target);
    let mut active_kind = chat_target;
    let run_started_at = Instant::now();
    let mut last_run_secs: u64 = 0;
    let mut heartbeat_minutes_cache = sys_cfg.heartbeat_minutes;
    let now0 = Instant::now();
    let mut next_heartbeat_at = if heartbeat_minutes_cache == 0 {
        now0 + Duration::from_secs(HEARTBEAT_DISABLED_SECS)
    } else {
        now0 + heartbeat_interval(heartbeat_minutes_cache)
    };
    let mut heartbeat_request_id: Option<u64> = None;
    let mut pulse_notice: Option<PulseNotice> = None;
    let mut heartbeat_count: u64 = token_totals.total_heartbeat_count;
    let mut response_count: u64 = token_totals.total_heartbeat_responses;
    let mut hint_last = String::new();
    let mut hint_anim_start_tick: usize = 0;
    let mut hint_anim_seed: u64 = 0;
    let mut pending_cmd_shell = false;
    let mut diary_state = DiaryState::new();
    let mut sys_log: VecDeque<String> = VecDeque::with_capacity(config.sys_log_limit);
    let mut wake_lock_acquired = false;
    let mut context_usage = ContextUsage::default();
    let contextmemo_tokens = load_contextmemo_tokens(&config.contextmemo_path);
    context_usage.load_tokens(contextmemo_tokens);
    if let Some(err) = dog_cfg_err {
        push_sys_log(&mut sys_log, config.sys_log_limit, format!("DOG: {err}"));
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("DOG: 配置 {}", dog_cfg_path.display()),
        );
    }
    if dog_cfg.api_key.as_deref().unwrap_or("").trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "DOG: API Key 为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "DOG: API Key 已配置");
    }
    if let Some(err) = main_cfg_err {
        push_sys_log(&mut sys_log, config.sys_log_limit, format!("MAIN: {err}"));
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("MAIN: 配置 {}", main_cfg_path.display()),
        );
    }
    if let Some(err) = sys_cfg_err {
        push_sys_log(&mut sys_log, config.sys_log_limit, format!("SYS: {err}"));
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("SYS: 配置 {}", sys_cfg_path.display()),
        );
    }
    if main_cfg.api_key.as_deref().unwrap_or("").trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MAIN: API Key 为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MAIN: API Key 已配置");
    }
    if let Some(err) = dog_prompt_err.take() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("DOG: 提示词读取失败 {err}"),
        );
    } else if dog_state.prompt.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "DOG: 提示词为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "DOG: 已载入提示词");
        log_memos(
            &mut metamemo,
            &mut context_usage,
            "system",
            Some("dog"),
            &dog_state.prompt,
        );
    }
    if let Some(err) = main_prompt_err.take() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("MAIN: 提示词读取失败 {err}"),
        );
    } else if main_prompt_text.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MAIN: 提示词为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MAIN: 已载入提示词");
        log_memos(
            &mut metamemo,
            &mut context_usage,
            "system",
            Some("main"),
            &main_prompt_text,
        );
    }
    for err in context_prompt_errs {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("CONTEXT: {err}"),
        );
    }
    if let Some(err) = context_compact_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("CONTEXT-COMPACT: {err}"),
        );
    } else if context_compact_prompt_text.trim().is_empty() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            "CONTEXT-COMPACT: 提示词为空",
        );
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            "CONTEXT-COMPACT: 已载入提示词",
        );
    }
    if let Some(err) = fastmemo_compact_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("FASTMEMO-COMPACT: {err}"),
        );
    } else if fastmemo_compact_prompt_text.trim().is_empty() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            "FASTMEMO-COMPACT: 提示词为空",
        );
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            "FASTMEMO-COMPACT: 已载入提示词",
        );
    }
    if let Some(err) = metamemo_err.take() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("META: 初始化失败 {err}"),
        );
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "META: 已启用");
    }
    if let Some(err) = datememo_err.take() {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("DATEMEMO: 初始化失败 {err}"),
        );
    }
    if let Some(err) = dog_client_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("DOG: API 未就绪 {}", summarize_api_error_for_user(&err)),
        );
    }
    if let Some(err) = main_client_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("MAIN: API 未就绪 {}", summarize_api_error_for_user(&err)),
        );
    }
    if try_termux_wake_lock(true).is_ok() {
        wake_lock_acquired = true;
        push_sys_log(&mut sys_log, config.sys_log_limit, "WAKELOCK: 已启用");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "WAKELOCK: 启用失败");
    }
    let mut sys_scroll: usize = 0;
    let mut last_context_save = Instant::now();
    let mut last_sys_scroll = Instant::now();
    let mut sys_scroll_until: Option<Instant> = None;
    let mut last_sys_line = String::new();
    let mut retry_status: Option<String> = None;
    let mut mind_pulse: Option<MindPulse> = None;
    let mut thinking_text = String::new();
    let mut thinking_full_text = String::new();
    let mut thinking_idx: Option<usize> = None;
    let mut thinking_in_progress = false;
    let mut thinking_pending_idle = false;
    let mut thinking_started_at: Option<Instant> = None;
    let mut thinking_scroll: usize = 0;
    let mut thinking_scroll_cap: usize = 0;
    let mut last_thinking_text = String::new();
    let mut last_status_label = "思考".to_string();
    let mut tool_preview = String::new();
    let mut tool_preview_active = false;
    let mut tool_preview_pending_idle = false;
    let mut tool_preview_chat_idx: Option<usize> = None;
    let mut ctx_compact_notice_idx: Option<usize> = None;
    let mut reveal_idx: Option<usize> = None;
    let mut reveal_len: usize = 0;
    let mut last_reveal_at = Instant::now();
    let mut sending_until: Option<Instant> = None;
    let mut streaming_state = StreamingState::default();
    let mut last_input_at: Option<Instant> = None;
    let mut expanded_tool_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut expanded_thinking_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut selected_msg_idx: Option<usize> = None;
    let mut details_mode = false;
    let mut last_details_toggle_at: Option<Instant> = None;
    let mut msg_line_ranges_cache: Vec<Option<(usize, usize)>> = Vec::new();
    let mut chat_height_cache: usize = 0;
    let mut active_tool_stream: Option<ToolStreamState> = None;
    let mut pending_tools: VecDeque<ToolCall> = VecDeque::new();
    let mut pending_tool_confirm: Option<(ToolCall, String, usize)> = None;
    let mut screen = Screen::Chat;
    let mut settings = SettingsState::new();
    let mut pty: Option<PtyUiState> = None;
    let mut pty_view = false;
    let mut pty_focus = PtyFocus::Terminal;
    // 交互 PTY 是否已自然结束（子进程退出）。结束后保留屏幕供用户确认/回看，
    // 直到用户按 Esc 关闭终端视图。
    let mut pty_done = false;
    // 用户已请求退出 PTY（Esc/Ctrl+[ 或 Ctrl+Q）。此时不允许 Home 复活终端视图，
    // 但保留 pty 状态直到 ToolStreamEnd 以便回传输出。
    let mut pty_exit_requested = false;

    let (tx, rx) = mpsc::channel::<AsyncEvent>();
    let mut paste_guard_until: Option<Instant> = None;
    let mut paste_drop_until: Option<Instant> = None;
    let mut burst_count: usize = 0;
    let mut burst_last_at: Option<Instant> = None;
    let mut burst_started_at: Option<Instant> = None;
    let mut burst_start_cursor: Option<usize> = None;
    let mut paste_capture: Option<PasteCapture> = None;
    let mut toast: Option<(Instant, String)> = None;
    let mut pending_pastes: Vec<(String, String)> = Vec::new();
    let max_input_chars = config.max_input_chars;
    let paste_capture_max_bytes = config.paste_capture_max_bytes;
    let mut command_menu_selected: usize = 0;
    let mut command_menu_suppress = false;
    let mut should_exit = false;
    let mut exit_code: i32 = 0;
    let mut needs_redraw = true;
    let mut last_anim_at = Instant::now();
    let mut last_draw_at = Instant::now();

    macro_rules! drain_events {
        () => {
		            drain_async_events(DrainAsyncEventsArgs {
		                core: &mut core,
		                rx: &rx,
		                render_cache: &mut render_cache,
		                pty: &mut pty,
		                pty_view: &mut pty_view,
		                pty_done: &mut pty_done,
		                pty_exit_requested: &mut pty_exit_requested,
		                mode: &mut mode,
		                scroll: &mut scroll,
		                follow_bottom: &mut follow_bottom,
	                active_kind: &mut active_kind,
	                reveal_idx: &mut reveal_idx,
                reveal_len: &mut reveal_len,
                expanded_tool_idxs: &mut expanded_tool_idxs,
                active_tool_stream: &mut active_tool_stream,
                dog_state: &mut dog_state,
                dog_client: &dog_client,
                main_state: &mut main_state,
                main_client: &main_client,
                mind_context: &mut mind_context,
                _mind_ctx_idx_main: &mut mind_ctx_idx_main,
                _mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                sending_until: &mut sending_until,
                token_totals: &mut token_totals,
                token_total_path: &token_total_path,
                meta: &mut metamemo,
                context_usage: &mut context_usage,
                diary_state: &mut diary_state,
                ctx_compact_notice_idx: &mut ctx_compact_notice_idx,
                mind_rate_window: &mut mind_rate_window,
                pending_mind_half: &mut pending_mind_half,
                thinking_text: &mut thinking_text,
                thinking_full_text: &mut thinking_full_text,
                thinking_idx: &mut thinking_idx,
                thinking_in_progress: &mut thinking_in_progress,
                thinking_pending_idle: &mut thinking_pending_idle,
                thinking_started_at: &mut thinking_started_at,
                expanded_thinking_idxs: &mut expanded_thinking_idxs,
                selected_msg_idx: &mut selected_msg_idx,
                tool_preview: &mut tool_preview,
                tool_preview_active: &mut tool_preview_active,
                tool_preview_pending_idle: &mut tool_preview_pending_idle,
                tool_preview_chat_idx: &mut tool_preview_chat_idx,
                active_request_is_mind: &mut active_request_is_mind,
                active_request_is_fastmemo_compact: &mut active_request_is_fastmemo_compact,
                streaming_state: &mut streaming_state,
                mind_pulse: &mut mind_pulse,
                retry_status: &mut retry_status,
                sys_log: &mut sys_log,
                sys_log_limit: config.sys_log_limit,
                config: &config,
                context_compact_prompt_text: &context_compact_prompt_text,
                fastmemo_compact_prompt_text: &fastmemo_compact_prompt_text,
                auto_fastmemo_compact: &mut auto_fastmemo_compact,
                fastmemo_compact_inflight: &mut fastmemo_compact_inflight,
                fastmemo_compact_edit_mask: &mut fastmemo_compact_edit_mask,
                fastmemo_compact_retry_at: &mut fastmemo_compact_retry_at,
                sys_cfg: &mut sys_cfg,
                heartbeat_minutes_cache: &mut heartbeat_minutes_cache,
                next_heartbeat_at: &mut next_heartbeat_at,
                heartbeat_request_id: &mut heartbeat_request_id,
                response_count: &mut response_count,
                _pulse_notice: &mut pulse_notice,
                tx: &tx,
                pending_tools: &mut pending_tools,
                pending_tool_confirm: &mut pending_tool_confirm,
                request_seq: &mut request_seq,
                active_request_id: &mut active_request_id,
                active_cancel: &mut active_cancel,
                active_request_in_tokens: &mut active_request_in_tokens,
                sse_enabled,
                run_log_path: &config.run_log_path,
            });
        };
    }

    loop {
        if exit_flag.load(Ordering::Relaxed) {
            break;
        }
        let now = Instant::now();
        let run_secs = run_started_at.elapsed().as_secs();
        if run_secs != last_run_secs {
            last_run_secs = run_secs;
            if !tty_is_alive() {
                break;
            }
            // 兜底：若父进程已退出（PPID=1），立刻退出，避免极端情况下变成孤儿进程持续占用 CPU。
            #[cfg(any(target_os = "linux", target_os = "android"))]
            {
                if unsafe { libc::getppid() } == 1 {
                    break;
                }
            }
            needs_redraw = true;
        }
        if sys_cfg.heartbeat_minutes != heartbeat_minutes_cache {
            heartbeat_minutes_cache = sys_cfg.heartbeat_minutes;
            next_heartbeat_at = if heartbeat_minutes_cache == 0 {
                heartbeat_request_id = None;
                push_sys_log(&mut sys_log, config.sys_log_limit, "Heartbeat: Off");
                now + Duration::from_secs(HEARTBEAT_DISABLED_SECS)
            } else {
                push_sys_log(
                    &mut sys_log,
                    config.sys_log_limit,
                    format!("Heartbeat: {}m", heartbeat_minutes_cache),
                );
                now + heartbeat_interval(heartbeat_minutes_cache)
            };
        }
        if heartbeat_minutes_cache > 0
            && now >= next_heartbeat_at
            && can_inject_heartbeat(
                mode,
                &active_request_id,
                &pending_tools,
                &pending_tool_confirm,
                &active_tool_stream,
            )
        {
            let stamp = metamemo_ts();
            let msg = format!("心跳：{stamp} | idle");
            log_memos(
                &mut metamemo,
                &mut context_usage,
                "system",
                Some("main"),
                &msg,
            );
            // DeepSeek 对 system 消息位置/排序比较敏感（容易触发 400: Invalid consecutive assistant message）。
            // 心跳作为一次“用户事件”，用 user 消息注入到对话上下文：assistant -> user(heartbeat) -> assistant(reply)。
            let ctx_limit = sys_cfg.context_k.saturating_mul(1000).max(1);
            let hb_user = format!(
                "心跳信号：{stamp} | idle\n请像正常消息一样回复（可简短）。若无需回应请只回复 [mainpass]。不要调用工具。"
            );
            let _ = main_state.push_user(&hb_user, ctx_limit);
            if let Some(in_tokens) = try_start_main_heartbeat(TryStartMainHeartbeatArgs {
                main_client: &main_client,
                main_state: &main_state,
                tx: &tx,
                config: &config,
                mode: &mut mode,
                active_kind: &mut active_kind,
                sending_until: &mut sending_until,
                sys_log: &mut sys_log,
                sys_log_limit: config.sys_log_limit,
                streaming_state: &mut streaming_state,
                request_seq: &mut request_seq,
                active_request_id: &mut active_request_id,
                active_cancel: &mut active_cancel,
                heartbeat_request_id: &mut heartbeat_request_id,
                sse_enabled,
            }) {
                heartbeat_count = heartbeat_count.saturating_add(1);
                token_totals.total_heartbeat_count = heartbeat_count;
                token_totals.total_heartbeat_responses = response_count;
                record_request_in_tokens(
                    &mut core,
                    &mut token_totals,
                    &token_total_path,
                    &context_usage,
                    &mut active_request_in_tokens,
                    in_tokens,
                );
                core.push_system(HEARTBEAT_BANNER);
                needs_redraw = true;
                next_heartbeat_at = now + heartbeat_interval(heartbeat_minutes_cache);
            } else {
                next_heartbeat_at = now + heartbeat_interval(heartbeat_minutes_cache);
            }
        }
        let menu_items = if screen == Screen::Chat
            && !command_menu_suppress
            && mode == Mode::Idle
            && !(pty_view && pty.is_some())
        {
            filter_commands_for_input(&input, sse_enabled, chat_target)
        } else {
            Vec::new()
        };
        let menu_open = !menu_items.is_empty();
        if !menu_open {
            command_menu_selected = 0;
        } else if command_menu_selected >= menu_items.len() {
            command_menu_selected = menu_items.len().saturating_sub(1);
        }
        if screen == Screen::Chat && input.is_empty() {
            last_input_at = None;
        }
        let input_active = if screen == Screen::Chat {
            last_input_at.is_some_and(|t| {
                now.saturating_duration_since(t) <= Duration::from_millis(config.input_status_ms)
            }) && !input.is_empty()
        } else {
            false
        };
        let mut pulse_style: Option<ui::HeartbeatStyle> = None;
        let mut pulse_idx: Option<usize> = None;
        let mut pulse_animating = false;
        if let Some(state) = pulse_notice.as_mut()
            && state.msg_idx < core.history.len()
        {
            pulse_idx = Some(state.msg_idx);
            if state.done {
                pulse_style = Some(ui::HeartbeatStyle {
                    intensity: 1.0,
                    visible: true,
                });
            } else {
                let (style, done) = pulse_anim_state(now, state.started_at, config.active_frame_ms);
                pulse_style = Some(style);
                pulse_animating = !done;
                if done {
                    state.done = true;
                }
            }
        }
        let status_has_text = if sse_enabled {
            if tool_preview_active || tool_preview_pending_idle {
                !tool_preview.trim().is_empty()
            } else {
                !thinking_text.trim().is_empty()
            }
        } else {
            false
        };
        let thinking_scroll_active = status_has_text
            && (thinking_in_progress
                || tool_preview_active
                || tool_preview_pending_idle
                || thinking_pending_idle);
        // 聊天区相关动效（乱码/思考滚动/心跳脉冲）只在聊天页驱动 tick；
        // 否则在 Settings 页会出现“无操作也在动”的错觉。
        let in_chat = matches!(screen, Screen::Chat);
        let thinking_scroll_active = in_chat && thinking_scroll_active;
        let scramble_animating = in_chat && render_cache.has_scramble_pending();
        let pulse_animating = in_chat && pulse_animating;
        let mind_pulse_animating = mind_pulse.as_ref().is_some_and(|p| now < p.until);
        let mut anim_enabled = matches!(
            mode,
            Mode::Generating | Mode::ExecutingTool | Mode::ApprovingTool
        ) || thinking_scroll_active
            || input_active
            || pulse_animating
            || scramble_animating
            || mind_pulse_animating;
        if in_chat && pty_view && pty.is_some() {
            anim_enabled = true;
        }
        if paste_drop_until.is_some_and(|t| now >= t) {
            paste_drop_until = None;
        }
        if paste_guard_until.is_some_and(|t| now >= t) {
            paste_guard_until = None;
        }
        if sending_until.is_some_and(|t| now >= t) {
            sending_until = None;
        }
        if let Some((until, _)) = toast.as_ref()
            && now >= *until
        {
            toast = None;
            needs_redraw = true;
        }
        if let Some((until, _)) = settings.notice.as_ref()
            && now >= *until
        {
            settings.notice = None;
            needs_redraw = true;
        }
        let tool_preview_any = tool_preview_active || tool_preview_pending_idle;
        let active_status = if screen == Screen::Chat {
            let mind_pulse_dir = mind_pulse.as_ref().filter(|p| now < p.until).map(|p| p.dir);
            build_active_status(ActiveStatusArgs {
                now,
                retry_status: retry_status.as_deref(),
                sending_until,
                mode,
                reveal_idx,
                streaming_has_content: streaming_state.has_content,
                tool_preview_any,
                tool_preview: tool_preview.as_str(),
	                pending_tool_confirm: pending_tool_confirm.as_ref().map(|(call, _, _)| call),
	                active_tool_stream: active_tool_stream.as_ref(),
	                ctx_compact_main: main_state.ctx_compact_inflight,
	                ctx_compact_dog: dog_state.ctx_compact_inflight,
	                mind_pulse: mind_pulse_dir,
	                input_active,
                diary_active: diary_state.active(),
            })
        } else {
            None
        };
        let header_sys_line = if sys_log.is_empty() {
            String::new()
        } else {
            let mut recent: Vec<String> = sys_log
                .iter()
                .rev()
                .take(config.sys_display_limit)
                .cloned()
                .collect();
            recent.reverse();
            recent.join(" · ")
        };
        let settings_section = settings_section_for_menu(settings.menu_index);
        let settings_fields = build_settings_fields(BuildSettingsFieldsArgs {
            section: settings_section,
            dog_cfg: &dog_cfg,
            main_cfg: &main_cfg,
            sys_cfg: &sys_cfg,
            dog_prompt_text: &dog_state.prompt,
            main_prompt_text: &main_prompt_text,
            context_prompts: &context_prompts,
            context_compact_prompt_text: &context_compact_prompt_text,
        });
        if !settings_fields.is_empty() && settings.field_index >= settings_fields.len() {
            settings.field_index = settings_fields.len().saturating_sub(1);
        }
        let selected_label = settings_fields
            .get(settings.field_index)
            .map(|f| f.label)
            .unwrap_or("提示词");
        let settings_line = if let Some((_, msg)) = settings.notice.as_ref() {
            msg.clone()
        } else {
            let selected_kind = settings_fields.get(settings.field_index).map(|f| f.kind);
            match settings.focus {
                SettingsFocus::Tabs => {
                    "Select tab (←→/Tab, Enter, Esc)".to_string()
                }
                SettingsFocus::Fields => {
                    if let Some(kind) = selected_kind {
                        match kind {
                            SettingsFieldKind::Provider | SettingsFieldKind::Model => {
                                format!(
                                    "{} (Enter)",
                                    settings_field_hint(settings_section, kind)
                                )
                            }
                            SettingsFieldKind::SseEnabled
                            | SettingsFieldKind::ChatTarget
                            | SettingsFieldKind::ExecPermission => {
                                format!(
                                    "{} (Enter)",
                                    settings_field_hint(settings_section, kind)
                                )
                            }
                            SettingsFieldKind::DogPrompt
                            | SettingsFieldKind::MainPrompt
                            | SettingsFieldKind::ContextMainPrompt
                            | SettingsFieldKind::ContextCompactPrompt => {
                                "Open prompt editor (Enter)".to_string()
                            }
                            _ => format!(
                                "{} (Enter to edit, Esc)",
                                settings_field_hint(settings_section, kind)
                            ),
                        }
                    } else {
                        settings_section_title(settings_section).to_string()
                    }
                }
                SettingsFocus::Input => {
                    let kind = settings.edit_kind.or(selected_kind);
                    if let Some(kind) = kind {
                        format!(
                            "{} (Enter to save, Esc)",
                            settings_field_hint(settings_section, kind)
                        )
                    } else {
                        format!("Editing: {selected_label}")
                    }
                }
                SettingsFocus::Prompt => "Prompt editor (Ctrl+S save, Esc)".to_string(),
            }
        };
        let mut input_line = if screen == Screen::Chat {
            // 压缩/写日记期间：输入框状态栏强制占用并屏蔽其它提示（toast/菜单/输入等），直到流程结束再解除。
            let locked = if diary_state.active() {
                Some("Updating diary".to_string())
            } else if main_state.ctx_compact_inflight || dog_state.ctx_compact_inflight {
                Some("Compacting context".to_string())
            } else {
                None
            };
            if let Some(line) = locked {
                line
            } else if let Some((_, msg)) = toast.as_ref() {
                msg.clone()
            } else if menu_open {
                menu_items
                    .get(command_menu_selected)
                    .map(|c| c.desc.to_string())
                    .unwrap_or_else(|| "Ready".to_string())
            } else if let Some(active) = active_status {
                active
            } else {
                "Ready".to_string()
            }
        } else {
            settings_line.clone()
        };
        if matches!(screen, Screen::Chat) && pty.is_some() {
            anim_enabled = true;
            let stage = if pty_done {
                "命令执行完毕 ESC to exit"
            } else {
                "正在执行中..."
            };
            let nav = if pty_view { "" } else { " · Home: Terminal" };
            input_line = format!("Terminal OS/ {stage}{nav}");
        }
        // 输入提示行（横杠上方）：文本变化时触发（●闪烁 → 乱码展开）。
        // 需要把它并入 anim_enabled，否则 idle 时 poll_timeout 会很长导致动画不刷新。
        const HINT_ANIM_MAX_FRAMES: usize = 16;
        if input_line != hint_last {
            hint_last = input_line.clone();
            hint_anim_start_tick = spinner_tick;
        // 文本变化时触发一次“●闪烁→乱码收敛”，包括 Ready（更有生命感）。
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_nanos() as u64)
                .unwrap_or(spinner_tick as u64);
            hint_anim_seed = nanos ^ request_seq.wrapping_mul(0xA0761D6478BD642F);
            needs_redraw = true;
        }
        let hint_anim_tick = spinner_tick.wrapping_sub(hint_anim_start_tick);
        let hint_animating = hint_anim_seed != 0 && hint_anim_tick < HINT_ANIM_MAX_FRAMES;
        if hint_animating {
            anim_enabled = true;
        }
        let (status_label, status_snapshot) = if sse_enabled {
            if tool_preview_active || tool_preview_pending_idle {
                (
                    info_label_for_tool_preview(&tool_preview),
                    tool_preview.trim().to_string(),
                )
            } else {
                ("思考".to_string(), thinking_text.trim().to_string())
            }
        } else {
            ("思考".to_string(), String::new())
        };
        let guard_active = paste_guard_until.is_some_and(|t| now < t);
        if !guard_active {
            let finalize = finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                force: false,
                now,
                config: &config,
                paste_capture: &mut paste_capture,
                input: &mut input,
                cursor: &mut cursor,
                input_chars: &mut input_chars,
                pending_pastes: &mut pending_pastes,
                toast: &mut toast,
                max_input_chars,
                burst_count: &mut burst_count,
                burst_last_at: &mut burst_last_at,
                burst_started_at: &mut burst_started_at,
                burst_start_cursor: &mut burst_start_cursor,
                paste_drop_until: &mut paste_drop_until,
                paste_guard_until: &mut paste_guard_until,
            });
            if finalize.flushed {
                needs_redraw = true;
            }
        }
        if context_usage.dirty()
            && now.saturating_duration_since(last_context_save) >= Duration::from_millis(800)
        {
            token_totals.context_tokens = context_usage.tokens() as u64;
            let _ = store_token_totals(&token_total_path, &token_totals);
            context_usage.mark_clean();
            last_context_save = now;
        }
        if mind_pulse.as_ref().is_some_and(|p| now >= p.until) {
            mind_pulse = None;
            needs_redraw = true;
        }

        if header_sys_line != last_sys_line {
            sys_scroll = 0;
            sys_scroll_until = if anim_enabled {
                Some(now + Duration::from_millis(config.sys_scroll_burst_ms))
            } else {
                None
            };
            last_sys_line = header_sys_line.clone();
            needs_redraw = true;
        }
        if status_snapshot != last_thinking_text || status_label != last_status_label {
            let status_active = thinking_in_progress || tool_preview_active;
            last_thinking_text = status_snapshot;
            last_status_label = status_label;
            thinking_scroll_cap = ui::thinking_scroll_limit(
                status_width_cache,
                &last_status_label,
                &last_thinking_text,
            );
            if status_active && !last_thinking_text.is_empty() {
                thinking_scroll = thinking_scroll_cap;
            } else {
                thinking_scroll = 0;
            }
            needs_redraw = true;
        }

        if anim_enabled {
            if !header_sys_line.trim().is_empty()
                && sys_scroll_until.is_some_and(|t| now < t)
                && now.saturating_duration_since(last_sys_scroll)
                    >= Duration::from_millis(config.sys_scroll_ms)
            {
                sys_scroll = sys_scroll.wrapping_add(2);
                last_sys_scroll = now;
                needs_redraw = true;
            }

            if let Some(idx) = reveal_idx
                && now.saturating_duration_since(last_reveal_at)
                    >= Duration::from_millis(config.reveal_frame_ms)
            {
                if let Some(msg) = core.history.get(idx) {
                    let total = msg.text.chars().count();
                    if reveal_len >= total || total == 0 {
                        reveal_idx = None;
                    } else {
                        reveal_len = (reveal_len + config.reveal_step).min(total);
                        last_reveal_at = now;
                        needs_redraw = true;
                    }
                } else {
                    reveal_idx = None;
                }
            }
        } else {
            if sys_scroll_until.take().is_some() {
                sys_scroll = 0;
                needs_redraw = true;
            }
            if reveal_idx.is_some() {
                reveal_idx = None;
                reveal_len = 0;
            }
        }

        if (tool_preview_pending_idle || thinking_pending_idle)
            && thinking_scroll < thinking_scroll_cap
        {
            thinking_scroll = thinking_scroll_cap;
            needs_redraw = true;
        }
        if tool_preview_pending_idle && thinking_scroll >= thinking_scroll_cap {
            tool_preview_pending_idle = false;
            tool_preview_active = false;
            tool_preview.clear();
        } else if thinking_pending_idle && thinking_scroll >= thinking_scroll_cap {
            thinking_pending_idle = false;
            if matches!(mode, Mode::Generating) {
                mode = Mode::Idle;
                needs_redraw = true;
            }
            thinking_text.clear();
        }

        if anim_enabled
            && now.saturating_duration_since(last_anim_at)
                >= Duration::from_millis(config.active_frame_ms)
        {
            spinner_tick = spinner_tick.wrapping_add(1);
            last_anim_at = now;
            needs_redraw = true;
        }

        let paste_busy = paste_capture.is_some()
            || is_paste_like_activity(now, burst_last_at, burst_started_at, burst_count);
        let allow_draw = if paste_busy {
            now.saturating_duration_since(last_draw_at)
                >= Duration::from_millis(config.paste_redraw_throttle_ms)
        } else {
            true
        };

        if needs_redraw && allow_draw {
            let pulse_dir = mind_pulse.as_ref().map(|p| p.dir);
            // 已移除弹窗/窗口栈：思考/工具/正文都在聊天流内以“动态层”呈现。
            let mut settings_draw = ui::SettingsDrawResult::default();
            terminal.draw(|f| {
                let size = f.area();
                ui::fill_background(f, &theme, size);
                let (input_text, input_cursor) = if screen == Screen::Settings
                    && matches!(settings.focus, SettingsFocus::Input)
                {
                    (settings.edit_buffer.as_str(), settings.edit_cursor)
                } else if screen == Screen::Settings {
                    ("", 0usize)
                } else {
                    (input.as_str(), cursor)
                };
                let input_inner_w = size.width.saturating_sub(2).max(1) as usize;
                let input_lines = ui::wrapped_line_count(input_inner_w, input_text);
	                let input_lines = if pty_view && pty.is_some() && matches!(screen, Screen::Chat) {
	                    // PTY 视图下也保留一个“可用的输入框高度”，避免被终端挤压到只剩一行。
	                    4u16
	                } else {
	                    input_lines.clamp(4, 10) as u16
                };
                let desired_input_h = input_lines;
                let max_input_h = size.height.saturating_sub(1 + 1 + 1 + 1 + 1 + 1 + 1); // header + header-sep + chat(min1) + spacer + status + sep + ctx
                let input_h = desired_input_h.min(max_input_h.max(1));
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(1),
                        Constraint::Length(1),
                        Constraint::Min(1),
                        Constraint::Length(1),
                        Constraint::Length(1),
                        Constraint::Length(1),
                        Constraint::Length(input_h),
                        Constraint::Length(1),
                    ])
                    .split(size);

                ui::draw_header(
                    f,
                    ui::DrawHeaderArgs {
                        theme: &theme,
                        area: chunks[0],
                        main_mode: {
                            match mode {
                                Mode::Generating => match active_kind {
                                    MindKind::Main => mode,
                                    MindKind::Sub => Mode::Idle,
                                },
                                Mode::ExecutingTool | Mode::ApprovingTool => {
                                    if active_tool_stream
                                        .as_ref()
                                        .is_some_and(|s| matches!(s.owner, MindKind::Main))
                                    {
                                        mode
                                    } else {
                                        Mode::Idle
                                    }
                                }
                                _ => Mode::Idle,
                            }
                        },
                        dog_mode: {
                            match mode {
                                Mode::Generating => match active_kind {
                                    MindKind::Sub => mode,
                                    MindKind::Main => Mode::Idle,
                                },
                                Mode::ExecutingTool | Mode::ApprovingTool => {
                                    if active_tool_stream
                                        .as_ref()
                                        .is_some_and(|s| matches!(s.owner, MindKind::Sub))
                                    {
                                        mode
                                    } else {
                                        Mode::Idle
                                    }
                                }
                                _ => Mode::Idle,
                            }
                        },
                        user_active: input_active,
                        pulse_dir,
                        tick: spinner_tick,
                        run_secs,
                    },
                );
                // 顶部横杠：用于把“标题栏”和主体隔离开。
                ui::draw_separator(
                    f,
                    ui::DrawSeparatorArgs {
                        theme: &theme,
                        area: chunks[1],
                        mode,
                        active_kind,
                        user_active: input_active,
                        tick: spinner_tick,
                        hint: "",
                    },
                );

	                if screen == Screen::Chat {
	                    // 交互 PTY：聊天区域临时切为“真实终端视图”，Home 可来回切换。
			                    if pty_view && pty.is_some() {
			                        let pty_focused = matches!(pty_focus, PtyFocus::Terminal);
		                        let border = if pty_focused {
		                            theme.border_active
		                        } else {
		                            theme.border_idle
		                        };
		                        let block = Block::default()
		                            .borders(Borders::ALL)
		                            .title("-● Terminal OS ●-+")
		                            .border_style(Style::default().fg(border));
		                        let inner = block.inner(chunks[2]);
		                        let cols = inner.width.max(1);
		                        let rows = inner.height.max(1);
			                        let mut lines: Vec<Line<'static>> = Vec::new();
			                        if let Some(state) = pty.as_mut() {
			                            state.ensure_size(cols, rows);
			                            state.rebuild_cache();
			                            let base = Style::default().fg(theme.fg).bg(theme.bg);
			                            let mut cursor_pos: Option<(u16, u16)> = None;
			                            if pty_focused
			                                && state.scroll == 0
			                                && !state.parser.screen().hide_cursor()
			                            {
			                                cursor_pos = Some(state.parser.screen().cursor_position());
			                            }
				                            let cursor_style =
				                                Style::default().fg(theme.bg).bg(theme.fg);
				                            for (row_idx, l) in state.screen_lines.iter().enumerate() {
				                                let row_u16 = row_idx.min(u16::MAX as usize) as u16;
				                                let target_col = match cursor_pos {
				                                    Some((cur_row, cur_col)) if row_u16 == cur_row => {
				                                        Some(cur_col as usize)
				                                    }
				                                    _ => None,
				                                };
				                                if let Some(target_col) = target_col {
				                                    let mut col = 0usize;
				                                    let mut hit: Option<(usize, char, usize)> = None;
				                                    for (byte_idx, ch) in l.char_indices() {
				                                        let w = ch.width().unwrap_or(0).max(1);
			                                        if col <= target_col && target_col < col + w {
			                                            hit = Some((byte_idx, ch, ch.len_utf8()));
			                                            break;
			                                        }
			                                        col = col.saturating_add(w);
			                                    }
			                                    match hit {
			                                        Some((byte_idx, ch, ch_len)) => {
			                                            let pre = l[..byte_idx].to_string();
			                                            let post_start = (byte_idx + ch_len).min(l.len());
			                                            let post = l[post_start..].to_string();
			                                            lines.push(Line::from(vec![
			                                                Span::styled(pre, base),
			                                                Span::styled(ch.to_string(), cursor_style),
			                                                Span::styled(post, base),
			                                            ]));
			                                        }
			                                        None => {
			                                            // 光标在行尾之后：渲染一个反色空格作“块光标”。
			                                            lines.push(Line::from(vec![
			                                                Span::styled(l.clone(), base),
			                                                Span::styled(" ".to_string(), cursor_style),
			                                            ]));
			                                        }
			                                    }
			                                } else {
			                                    lines.push(Line::from(vec![Span::styled(
			                                        l.clone(),
			                                        base,
			                                    )]));
			                                }
			                            }
			                            f.render_widget(
			                                Paragraph::new(lines)
			                                    .style(base)
			                                    .block(block)
		                                    .scroll((0, 0)),
		                                chunks[2],
		                            );
			                            if pty_focused
			                                && state.scroll == 0
			                                && !state.parser.screen().hide_cursor()
			                            {
			                                let (cur_row, cur_col) = state.parser.screen().cursor_position();
			                                let max_col = cols.saturating_sub(1);
			                                let max_row = rows.saturating_sub(1);
			                                let cx = inner.x.saturating_add(cur_col.min(max_col));
		                                let cy = inner.y.saturating_add(cur_row.min(max_row));
		                                f.set_cursor_position((cx, cy));
		                            }
		                        }
		                        // pty 视图下不更新聊天布局缓存，避免误用选择/自动滚动逻辑。
		                        chat_width_cache = cols as usize;
		                        chat_height_cache = rows as usize;
		                        max_scroll_cache = 0;
	                    } else {
	                        let chat_width = chunks[2].width.max(1) as usize;
	                        let layout = ui::build_chat_layout(ui::BuildChatLinesArgs {
	                            theme: &theme,
	                            core: &core,
	                            render_cache: &mut render_cache,
	                            width: chat_width,
	                            streaming_idx: streaming_state.idx,
	                            reveal_idx,
	                            reveal_len,
	                            tick: spinner_tick,
	                            selected_msg_idx,
	                            expanded_tool_idxs: &expanded_tool_idxs,
	                            thinking_idx,
	                            expanded_thinking_idxs: &expanded_thinking_idxs,
	                            details_mode,
	                            streaming_enabled: sse_enabled,
	                            pulse_idx,
	                            pulse_style,
	                        });
	                        msg_line_ranges_cache = layout.msg_line_ranges;
	                        let chat_lines = layout.lines;
	                        chat_width_cache = chat_width;
	                        let chat_height = chunks[2].height.max(1) as usize;
	                        chat_height_cache = chat_height;
	                        let total_lines = chat_lines.len();
	                        let max_scroll = total_lines.saturating_sub(chat_height) as u16;
	                        max_scroll_cache = max_scroll as usize;
	                        if follow_bottom || scroll > max_scroll {
	                            scroll = max_scroll;
	                        }
	                        ui::draw_chat(f, &theme, chunks[2], chat_lines, scroll);
	                    }

	                    ui::draw_hint_line(
	                        f,
	                        ui::DrawHintLineArgs {
                            theme: &theme,
                            area: chunks[4],
                            hint: &input_line,
                            mode,
                            active_kind,
                            user_active: input_active,
                            tick: spinner_tick,
                            anim_tick: hint_anim_tick,
                            anim_seed: hint_anim_seed,
                        },
                    );
                    //（已移除弹窗/窗口栈：不在此处绘制“思考窗”）

                    //（已移除弹窗/窗口栈：不在此处绘制“工具/正文弹窗”与“思考置顶窗”）
                } else {
                    let menu_items = settings_menu_items();
                    let field_pairs: Vec<(String, String)> = settings_fields
                        .iter()
                        .map(|f| (f.label.to_string(), f.value.clone()))
                        .collect();
                    let prompt_editor = if matches!(settings.focus, SettingsFocus::Prompt) {
                        Some((settings.edit_buffer.as_str(), settings.edit_cursor))
                    } else {
                        None
                    };
                    settings_draw = ui::draw_settings(
                        f,
                        ui::DrawSettingsArgs {
                            theme: &theme,
                            area: chunks[2],
                            title: settings_section_title(settings_section),
                            menu_items: &menu_items,
                            menu_selected: settings.menu_index,
                            fields: &field_pairs,
                            field_selected: settings.field_index,
                            focus: settings.focus,
                            prompt_editor,
                            tick: spinner_tick,
                        },
                    );
                    ui::draw_settings_status(
                        f,
                        &theme,
                        chunks[4],
                        settings.focus,
                        spinner_tick,
                        &settings_line,
                    );
                }

                ui::draw_separator(
                    f,
                    ui::DrawSeparatorArgs {
                        theme: &theme,
                        area: chunks[5],
                        mode,
                        active_kind,
                        user_active: input_active,
                        tick: spinner_tick,
                        hint: "",
                    },
                );
		                ui::draw_input(
		                    f,
		                    ui::DrawInputArgs {
		                        theme: &theme,
		                        area: chunks[6],
		                        input: input_text,
		                        cursor: input_cursor,
		                        focused: matches!(screen, Screen::Chat)
		                            && !(pty_view && pty.is_some() && matches!(pty_focus, PtyFocus::Terminal)),
		                    },
		                );
                if let Some(pos) = settings_draw.cursor {
                    f.set_cursor_position(pos);
                }
                // “/”命令菜单：以横杠为基准向上展开（占用聊天区域底部），避免遮挡提示/解释行。
                if menu_open && matches!(screen, Screen::Chat) {
                    let available = chunks[2].height.max(1) as usize;
                    let menu_h = menu_items.len().min(available).max(1) as u16;
                    let menu_area = ratatui::layout::Rect {
                        x: chunks[2].x,
                        y: chunks[4].y.saturating_sub(menu_h),
                        width: chunks[2].width,
                        height: menu_h,
                    };
                    ui::draw_command_menu(f, &theme, menu_area, &menu_items, command_menu_selected);
                }
                let context_used = context_usage.tokens();
                let ctx_limit = sys_cfg.context_k.saturating_mul(1000).max(1);
                let sc_used = match chat_target {
                    MindKind::Main => main_state.estimate_chat_tokens(),
                    MindKind::Sub => dog_state.estimate_chat_tokens(),
                };
                let sc_limit = sys_cfg.ctx_recent_max_tokens.max(1);
                let context_line = ContextLine {
                    sc_tokens: sc_used,
                    sc_pct: calc_pct(sc_used, sc_limit),
                    ctx_pct: calc_pct(context_used, ctx_limit),
                    run_in_tokens: core.run_in_token_total(),
                    run_out_tokens: core.run_out_token_total(),
                    total_in_tokens: token_totals.total_in_tokens,
                    total_out_tokens: token_totals.total_out_tokens,
                    run_secs,
                    heartbeat_count,
                    response_count,
                };
                ui::draw_context_bar(f, &theme, chunks[7], context_line);
                input_width_cache = chunks[6].width.saturating_sub(2).max(1) as usize;
            })?;
            settings_editor_width_cache = settings_draw
                .editor_rect
                .map(|r| r.width.max(1) as usize)
                .unwrap_or(0);
            needs_redraw = false;
            last_draw_at = now;
        }

        let poll_timeout = compute_poll_timeout(PollTimeoutArgs {
            now,
            config: &config,
            anim_enabled,
            last_anim_at,
            toast: &toast,
            paste_capture: &paste_capture,
            header_sys_line: &header_sys_line,
            sys_scroll_until,
            last_sys_scroll,
            reveal_idx: &reveal_idx,
            last_reveal_at,
        });

        if !crossterm::event::poll(poll_timeout)? {
            drain_events!();
            continue;
        }

	        match crossterm::event::read()? {
	            Event::Mouse(me) => {
	                let mut changed = false;
	                match me.kind {
	                    MouseEventKind::ScrollUp | MouseEventKind::ScrollDown => {
	                        let up = matches!(me.kind, MouseEventKind::ScrollUp);
	                        let delta: u16 = 3;
		                        // 已移除弹窗/窗口栈：鼠标滚轮只滚动聊天区；交互 PTY 视图下则滚动 PTY 回看。
		                        if matches!(screen, Screen::Chat) && pty_view && pty.is_some() {
		                            if let Some(state) = pty.as_mut() {
		                                if up {
		                                    state.scroll = state.scroll.saturating_add(delta).min(PTY_SCROLLBACK_MAX);
		                                } else {
		                                    state.scroll = state.scroll.saturating_sub(delta);
		                                }
		                                state.dirty = true;
		                            }
		                        } else if up {
		                            scroll = scroll.saturating_sub(delta);
		                            follow_bottom = false;
	                        } else {
	                            let max_scroll = max_scroll_cache as u16;
	                            scroll = (scroll + delta).min(max_scroll);
	                            follow_bottom = scroll >= max_scroll;
	                        }
	                        changed = true;
	                    }
	                    _ => {}
	                }
                if changed {
                    needs_redraw = true;
                }
                continue;
            }
	            Event::Key(key) => {
		                needs_redraw = true;
		                let now = Instant::now();
		                let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
		                let alt = key.modifiers.contains(KeyModifiers::ALT);
			                let _shift = key.modifiers.contains(KeyModifiers::SHIFT);
		                if screen == Screen::Settings {
                    let section = settings_section;
                    if ctrl && matches!(key.code, KeyCode::Char('c')) {
                        break;
                    }
                    match settings.focus {
                        SettingsFocus::Tabs => match key.code {
                            KeyCode::PageUp => {
                                settings.menu_index = settings.menu_index.saturating_sub(1);
                            }
                            KeyCode::PageDown | KeyCode::Tab => {
                                let max = settings_menu_items().len().saturating_sub(1);
                                settings.menu_index = (settings.menu_index + 1).min(max);
                            }
                            KeyCode::Left => {
                                settings.menu_index = settings.menu_index.saturating_sub(1);
                            }
                            KeyCode::Right => {
                                let max = settings_menu_items().len().saturating_sub(1);
                                settings.menu_index = (settings.menu_index + 1).min(max);
                            }
                            KeyCode::Up => {
                                screen = Screen::Chat;
                            }
                            KeyCode::Enter | KeyCode::Down => {
                                settings.focus = SettingsFocus::Fields;
                                settings.field_index = 0;
                            }
                            KeyCode::Esc => {
                                screen = Screen::Chat;
                            }
                            _ => {}
                        },
                        SettingsFocus::Fields => match key.code {
                            KeyCode::PageUp => {
                                settings.menu_index = settings.menu_index.saturating_sub(1);
                                settings.focus = SettingsFocus::Tabs;
                            }
                            KeyCode::PageDown | KeyCode::Tab => {
                                let max = settings_menu_items().len().saturating_sub(1);
                                settings.menu_index = (settings.menu_index + 1).min(max);
                                settings.focus = SettingsFocus::Tabs;
                            }
                            KeyCode::Up => {
                                settings.field_index = settings.field_index.saturating_sub(1);
                            }
                            KeyCode::Down => {
                                let max = settings_fields.len().saturating_sub(1);
                                settings.field_index = (settings.field_index + 1).min(max);
                            }
                            KeyCode::Left | KeyCode::Esc => {
                                settings.focus = SettingsFocus::Tabs;
                            }
                            KeyCode::Enter => {
                                if let Some(spec) = settings_fields.get(settings.field_index) {
                                    if matches!(spec.kind, SettingsFieldKind::Provider) {
                                        let current =
                                            selected_provider(section, &dog_cfg, &main_cfg);
                                        let next = next_provider(&current);
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            value: &next,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            main_prompt_text: &mut main_prompt_text,
                                            context_compact_prompt_text:
                                                &mut context_compact_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                            context_compact_prompt_path:
                                                &context_compact_prompt_path,
                                        });
                                        continue;
                                    }
                                    if matches!(spec.kind, SettingsFieldKind::Model) {
                                        let provider =
                                            selected_provider(section, &dog_cfg, &main_cfg);
                                        if provider != "deepseek" {
                                            set_settings_notice(
                                                &mut settings,
                                                now,
                                                "当前供应商暂无模型".to_string(),
                                            );
                                            continue;
                                        }
                                        let current = selected_model(section, &dog_cfg, &main_cfg);
                                        let next = next_model(&current, section);
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            value: &next,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            main_prompt_text: &mut main_prompt_text,
                                            context_compact_prompt_text:
                                                &mut context_compact_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                            context_compact_prompt_path:
                                                &context_compact_prompt_path,
                                        });
                                        continue;
                                    }
                                    if matches!(
                                        spec.kind,
                                        SettingsFieldKind::SseEnabled
                                            | SettingsFieldKind::ChatTarget
                                            | SettingsFieldKind::ExecPermission
                                    ) {
                                        let value =
                                            match spec.kind {
                                                SettingsFieldKind::SseEnabled => {
                                                    if sys_cfg.sse_enabled { "off" } else { "on" }
                                                }
                                                SettingsFieldKind::ChatTarget => {
                                                    if normalize_chat_target_value(
                                                        &sys_cfg.chat_target,
                                                    ) == "main"
                                                    {
                                                        "dog"
                                                    } else {
                                                        "main"
                                                    }
                                                }
                                                SettingsFieldKind::ExecPermission => {
                                                    if sys_cfg.tool_full_access {
                                                        "safe"
                                                    } else {
                                                        "full"
                                                    }
                                                }
                                                _ => "on",
                                            };
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            value,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            main_prompt_text: &mut main_prompt_text,
                                            context_compact_prompt_text:
                                                &mut context_compact_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                            context_compact_prompt_path:
                                                &context_compact_prompt_path,
                                        });
                                        sync_system_toggles(
                                            &sys_cfg,
                                            &mut sse_enabled,
                                            &mut chat_target,
                                        );
                                        if matches!(spec.kind, SettingsFieldKind::ChatTarget)
                                            && matches!(mode, Mode::Idle)
                                        {
                                            active_kind = chat_target;
                                        }
                                        continue;
                                    }
                                    settings.edit_kind = Some(spec.kind);
                                    settings.edit_buffer = field_raw_value(FieldRawValueArgs {
                                        kind: spec.kind,
                                        section,
                                        dog_cfg: &dog_cfg,
                                        main_cfg: &main_cfg,
                                        sys_cfg: &sys_cfg,
                                        dog_prompt_text: &dog_state.prompt,
                                        main_prompt_text: &main_prompt_text,
                                        context_prompts: &context_prompts,
                                        context_compact_prompt_text: &context_compact_prompt_text,
                                    });
                                    settings.edit_cursor = settings.edit_buffer.len();
                                    if is_prompt_kind(spec.kind) {
                                        settings.focus = SettingsFocus::Prompt;
                                    } else {
                                        settings.focus = SettingsFocus::Input;
                                    }
                                }
                            }
                            _ => {}
                        },
                        SettingsFocus::Input => {
                            if handle_settings_tab_nav(key.code, &mut settings) {
                                continue;
                            }
                            let Some(kind) = settings.edit_kind else {
                                settings.focus = SettingsFocus::Fields;
                                continue;
                            };
                            if is_prompt_kind(kind) {
                                settings.focus = SettingsFocus::Fields;
                                continue;
                            }
                            match key.code {
                                KeyCode::Esc => {
                                    settings.focus = SettingsFocus::Fields;
                                    reset_settings_edit(&mut settings);
                                }
                                KeyCode::Char('s')
                                    if key.modifiers.contains(KeyModifiers::CONTROL) =>
                                {
                                    let buffer = settings.edit_buffer.clone();
                                    commit_settings_input(CommitSettingsInputArgs {
                                        kind,
                                        section,
                                        buffer: &buffer,
                                        settings: &mut settings,
                                        now,
                                        dog_cfg: &mut dog_cfg,
                                        main_cfg: &mut main_cfg,
                                        sys_cfg: &mut sys_cfg,
                                        dog_cfg_path: &dog_cfg_path,
                                        main_cfg_path: &main_cfg_path,
                                        sys_cfg_path: &sys_cfg_path,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        main_prompt_text: &mut main_prompt_text,
                                        context_compact_prompt_text:
                                            &mut context_compact_prompt_text,
                                        dog_client: &mut dog_client,
                                        main_client: &mut main_client,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                        context_prompts: &mut context_prompts,
                                        context_prompt_path: &context_prompt_path,
                                        context_compact_prompt_path: &context_compact_prompt_path,
                                        sse_enabled: &mut sse_enabled,
                                        chat_target: &mut chat_target,
                                    });
                                }
                                KeyCode::Enter => {
                                    let buffer = settings.edit_buffer.clone();
                                    commit_settings_input(CommitSettingsInputArgs {
                                        kind,
                                        section,
                                        buffer: &buffer,
                                        settings: &mut settings,
                                        now,
                                        dog_cfg: &mut dog_cfg,
                                        main_cfg: &mut main_cfg,
                                        sys_cfg: &mut sys_cfg,
                                        dog_cfg_path: &dog_cfg_path,
                                        main_cfg_path: &main_cfg_path,
                                        sys_cfg_path: &sys_cfg_path,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        main_prompt_text: &mut main_prompt_text,
                                        context_compact_prompt_text:
                                            &mut context_compact_prompt_text,
                                        dog_client: &mut dog_client,
                                        main_client: &mut main_client,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                        context_prompts: &mut context_prompts,
                                        context_prompt_path: &context_prompt_path,
                                        context_compact_prompt_path: &context_compact_prompt_path,
                                        sse_enabled: &mut sse_enabled,
                                        chat_target: &mut chat_target,
                                    });
                                }
                                KeyCode::Backspace => {
                                    edit_buffer_backspace(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Delete => {
                                    edit_buffer_delete(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Left => {
                                    edit_buffer_left(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Right => {
                                    edit_buffer_right(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Home => {
                                    edit_buffer_home(&mut settings.edit_cursor);
                                }
                                KeyCode::End => {
                                    edit_buffer_end(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Char(ch) => {
                                    edit_buffer_insert_char(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                        ch,
                                    );
                                }
                                _ => {}
                            }
                        }
                        SettingsFocus::Prompt => {
                            if handle_settings_tab_nav(key.code, &mut settings) {
                                continue;
                            }
                            let Some(kind) = settings.edit_kind else {
                                settings.focus = SettingsFocus::Fields;
                                continue;
                            };
                            if !is_prompt_kind(kind) {
                                settings.focus = SettingsFocus::Fields;
                                continue;
                            }
                            match key.code {
                                KeyCode::Esc => {
                                    settings.focus = SettingsFocus::Fields;
                                    reset_settings_edit(&mut settings);
                                }
                                KeyCode::Char('s')
                                    if key.modifiers.contains(KeyModifiers::CONTROL) =>
                                {
                                    let buffer = settings.edit_buffer.clone();
                                    apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                        kind,
                                        section,
                                        value: &buffer,
                                        settings: &mut settings,
                                        now,
                                        dog_cfg: &mut dog_cfg,
                                        main_cfg: &mut main_cfg,
                                        sys_cfg: &mut sys_cfg,
                                        dog_cfg_path: &dog_cfg_path,
                                        main_cfg_path: &main_cfg_path,
                                        sys_cfg_path: &sys_cfg_path,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        main_prompt_text: &mut main_prompt_text,
                                        context_compact_prompt_text:
                                            &mut context_compact_prompt_text,
                                        dog_client: &mut dog_client,
                                        main_client: &mut main_client,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                        context_prompts: &mut context_prompts,
                                        context_prompt_path: &context_prompt_path,
                                        context_compact_prompt_path: &context_compact_prompt_path,
                                    });
                                }
                                KeyCode::Enter => {
                                    edit_buffer_insert_char(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                        '\n',
                                    );
                                }
                                KeyCode::Backspace => {
                                    edit_buffer_backspace(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Delete => {
                                    edit_buffer_delete(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Left => {
                                    edit_buffer_left(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Right => {
                                    edit_buffer_right(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Up => {
                                    let width = settings_editor_width_cache.max(1);
                                    settings.edit_cursor = move_prompt_cursor_vertical(
                                        &settings.edit_buffer,
                                        width,
                                        settings.edit_cursor,
                                        -1,
                                    );
                                }
                                KeyCode::Down => {
                                    let width = settings_editor_width_cache.max(1);
                                    settings.edit_cursor = move_prompt_cursor_vertical(
                                        &settings.edit_buffer,
                                        width,
                                        settings.edit_cursor,
                                        1,
                                    );
                                }
                                KeyCode::Home => {
                                    edit_buffer_home(&mut settings.edit_cursor);
                                }
                                KeyCode::End => {
                                    edit_buffer_end(
                                        &settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                    );
                                }
                                KeyCode::Char(ch) => {
                                    edit_buffer_insert_char(
                                        &mut settings.edit_buffer,
                                        &mut settings.edit_cursor,
                                        ch,
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
	                    continue;
	                }

				                // 交互 PTY 视图：优先处理（避免被命令菜单/选择模式等逻辑吃掉）。
				                if matches!(screen, Screen::Chat) && pty_view && pty.is_some() {
				                    if matches!(key.code, KeyCode::Home) {
				                        pty_view = false;
				                        continue;
				                    }
					                    // Ctrl+Q：紧急退出（Kill）交互进程并离开终端视图。
					                    if ctrl
					                        && matches!(key.code, KeyCode::Char('q') | KeyCode::Char('Q'))
					                        && !alt
					                    {
						                        if let Some(state) = pty.as_mut() {
						                            let _ = state.ctrl_tx.send(PtyControl::Kill);
						                        }
						                        pty_view = false;
						                        pty_focus = PtyFocus::Terminal;
						                        pty_exit_requested = true;
						                        push_sys_log(
						                            &mut sys_log,
						                            config.sys_log_limit,
						                            "Terminal: killed (Ctrl+Q)",
						                        );
					                        continue;
					                    }
				                    if alt && !ctrl && matches!(key.code, KeyCode::Up) {
				                        if let Some(state) = pty.as_mut() {
				                            let snap = state.snapshot_plain();
				                            let snap = if snap.trim().is_empty() {
				                                "(empty)".to_string()
				                            } else {
				                                truncate_with_suffix(&snap, PTY_SNAPSHOT_MAX_CHARS)
				                            };
				                            state.last_user_snapshot = Some(snap.clone());
				                            let tool_text = format!(
				                                "操作: PTY SNAPSHOT\nexplain: 用户触发同步（Alt+↑）\noutput:\n```text\n{snap}\n```\nmeta:\n```text\nscrollback:{} | saved:{} | stdin_log: omitted\n```",
				                                state.scroll,
				                                state.saved_path
				                            );
				                            core.push_tool(tool_text.clone());
				                            match active_tool_stream.as_ref().map(|s| s.owner) {
			                                Some(MindKind::Sub) => dog_state.push_tool(&tool_text),
			                                _ => main_state.push_tool(&tool_text),
			                            }
			                            push_sys_log(
			                                &mut sys_log,
			                                config.sys_log_limit,
			                                "Terminal: snapshot synced",
			                            );
			                        }
				                        continue;
				                    }
				                    let is_plain_esc = matches!(key.code, KeyCode::Esc)
				                        || matches!(key.code, KeyCode::Char('\u{1b}'));
				                    let is_ctrl_lbracket_exit =
				                        ctrl && !alt && matches!(key.code, KeyCode::Char('['));
				                    if (is_plain_esc && !ctrl && !alt) || is_ctrl_lbracket_exit {
				                        if pty_done {
				                            pty_view = false;
				                            pty_focus = PtyFocus::Terminal;
				                            pty_done = false;
				                            pty_exit_requested = false;
				                            pty = None;
				                            push_sys_log(
				                                &mut sys_log,
				                                config.sys_log_limit,
				                                "Terminal: closed",
				                            );
				                        } else {
				                            if let Some(state) = pty.as_mut() {
				                                let _ = state.ctrl_tx.send(PtyControl::Kill);
				                            }
				                            pty_view = false;
				                            pty_focus = PtyFocus::Terminal;
				                            pty_exit_requested = true;
				                            push_sys_log(
				                                &mut sys_log,
				                                config.sys_log_limit,
				                                "Terminal: exited",
				                            );
				                        }
			                        continue;
			                    }
				                    if let Some(state) = pty.as_mut() {
				                        let term_focus = matches!(pty_focus, PtyFocus::Terminal);
				                        // 滚动回看：在“回看模式”（scroll>0）时，↑↓ 用于滚动，
				                        // 避免一按方向键就把 scroll 归零导致“无法翻页”的观感。
				                        // Ctrl+↑/↓：随时滚动（不转发给进程）。
				                        match key.code {
				                            KeyCode::Up if ctrl && term_focus => {
				                                state.scroll =
				                                    state.scroll.saturating_add(1).min(PTY_SCROLLBACK_MAX);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::Down if ctrl && term_focus => {
				                                state.scroll = state.scroll.saturating_sub(1);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::Up if term_focus && state.scroll > 0 => {
				                                state.scroll =
				                                    state.scroll.saturating_add(1).min(PTY_SCROLLBACK_MAX);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::Down if term_focus && state.scroll > 0 => {
				                                state.scroll = state.scroll.saturating_sub(1);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::End if term_focus && state.scroll > 0 => {
				                                state.scroll = 0;
				                                state.dirty = true;
				                                continue;
				                            }
				                            _ => {}
				                        }
				                        // PgUp/PgDn：切换焦点（PTY ↔ 输入框）。
				                        // - PgUp：焦点到 PTY（方向键/回车等直接操作终端）
				                        // - PgDn：焦点到输入框（本地编辑一行，Enter 送入 PTY）
				                        // Ctrl+PgUp/PgDn：本地滚动回看（不转发给进程）。
				                        match key.code {
				                            KeyCode::PageUp if ctrl => {
				                                state.scroll =
				                                    state.scroll.saturating_add(6).min(PTY_SCROLLBACK_MAX);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::PageDown if ctrl => {
				                                state.scroll = state.scroll.saturating_sub(6);
				                                state.dirty = true;
				                                continue;
				                            }
				                            KeyCode::PageUp => {
				                                pty_focus = PtyFocus::Terminal;
				                                continue;
				                            }
				                            KeyCode::PageDown => {
				                                pty_focus = PtyFocus::ChatInput;
				                                continue;
				                            }
				                            _ => {}
				                        }

				                        // 已自然结束：只允许回看/退出，不再转发输入。
				                        if pty_done {
				                            continue;
				                        }

		                        // ChatInput 焦点：本地编辑一行，Enter 送入 PTY。
			                        if matches!(pty_focus, PtyFocus::ChatInput) {
	                            // Ctrl+C：优先发给 PTY（方便中断命令），不需要切回终端焦点。
	                            if ctrl && matches!(key.code, KeyCode::Char('c')) {
	                                if let Some(bytes) = key_to_pty_bytes(key.code, key.modifiers) {
	                                    state.record_input_bytes(&bytes);
	                                    let _ = state.ctrl_tx.send(PtyControl::Input(bytes));
	                                }
	                                continue;
	                            }
		                            match key.code {
		                                KeyCode::Esc => {
		                                    if (ctrl || alt)
		                                        && let Some(bytes) =
		                                            key_to_pty_bytes(KeyCode::Esc, key.modifiers)
		                                    {
		                                        state.record_input_bytes(&bytes);
		                                        let _ = state.ctrl_tx.send(PtyControl::Input(bytes));
		                                    }
		                                    continue;
		                                }
		                                KeyCode::Left => {
		                                    if cursor > 0 {
		                                        cursor = prev_char_boundary(&input, cursor);
	                                    }
	                                }
	                                KeyCode::Right => {
	                                    if cursor < input.len() {
	                                        cursor = next_char_boundary(&input, cursor);
	                                    }
	                                }
	                                KeyCode::Home => {
	                                    cursor = 0;
	                                }
	                                KeyCode::End => {
	                                    cursor = input.len();
	                                }
		                                KeyCode::Backspace => {
		                                    if cursor > 0 {
		                                        let prev = prev_char_boundary(&input, cursor);
		                                        input.drain(prev..cursor);
		                                        cursor = prev;
		                                        input_chars = count_chars(&input);
		                                    }
		                                }
		                                KeyCode::Delete => {
		                                    if cursor < input.len() {
		                                        let next = next_char_boundary(&input, cursor);
		                                        input.drain(cursor..next);
		                                        input_chars = count_chars(&input);
		                                    }
		                                }
		                                KeyCode::Tab => {
		                                    if try_insert_char_limited(
		                                        &mut input,
		                                        &mut cursor,
		                                        '\t',
		                                        &mut input_chars,
		                                        max_input_chars,
		                                    ) {
		                                        last_input_at = Some(now);
		                                    }
		                                    continue;
		                                }
		                                KeyCode::Char(ch) => {
		                                    if try_insert_char_limited(
		                                        &mut input,
		                                        &mut cursor,
		                                        ch,
		                                        &mut input_chars,
		                                        max_input_chars,
		                                    ) {
		                                        last_input_at = Some(now);
		                                    }
		                                    continue;
		                                }
	                                KeyCode::Enter => {
	                                    if !input.is_empty() {
	                                        let mut bytes = input.as_bytes().to_vec();
	                                        bytes.push(b'\r');
	                                        if state.scroll != 0 {
	                                            state.scroll = 0;
	                                            state.dirty = true;
	                                        }
	                                        state.record_input_bytes(&bytes);
	                                        let _ = state.ctrl_tx.send(PtyControl::Input(bytes));
	                                        reset_input_buffer(
	                                            &mut input,
	                                            &mut cursor,
	                                            &mut input_chars,
	                                            &mut last_input_at,
	                                        );
	                                        pty_focus = PtyFocus::Terminal;
	                                    } else if let Some(bytes) =
	                                        key_to_pty_bytes(KeyCode::Enter, key.modifiers)
	                                    {
	                                        if state.scroll != 0 {
	                                            state.scroll = 0;
	                                            state.dirty = true;
	                                        }
	                                        state.record_input_bytes(&bytes);
	                                        let _ = state.ctrl_tx.send(PtyControl::Input(bytes));
	                                    }
	                                    continue;
	                                }
	                                _ => {}
	                            }
	                            last_input_at = Some(now);
	                            continue;
	                        }

	                        // Terminal 焦点：尽量把按键转发给 PTY（包括 Ctrl+C、方向键等）。
	                        if let Some(bytes) = key_to_pty_bytes(key.code, key.modifiers) {
	                            if state.scroll != 0 {
	                                state.scroll = 0;
	                                state.dirty = true;
	                            }
	                            state.record_input_bytes(&bytes);
	                            let _ = state.ctrl_tx.send(PtyControl::Input(bytes));
	                        }
	                    }
	                    continue;
	                }
	                // 交互 PTY 存在但未显示：Home 快速切回终端视图。
		                if matches!(screen, Screen::Chat)
		                    && !pty_view
		                    && pty.is_some()
		                    && !pty_exit_requested
		                    && matches!(key.code, KeyCode::Home)
		                    && input.is_empty()
		                    && selected_msg_idx.is_none()
		                    && !menu_open
		                {
	                    pty_view = true;
	                    pty_focus = PtyFocus::Terminal;
	                    continue;
	                }

	                if paste_drop_until.is_some_and(|t| now < t)
	                    && matches!(key.code, KeyCode::Char(_) | KeyCode::Tab | KeyCode::Enter)
	                {
	                    continue;
	                }
	                // Alt+↓：复制选中消息（避免占用 Enter；兼容用户单手操作）。
	                if alt
	                    && matches!(key.code, KeyCode::Down)
	                    && input.trim().is_empty()
	                    && matches!(mode, Mode::Idle)
	                    && let Some(sel) = selected_msg_idx
	                    && let Some(msg) = core.history.get(sel)
	                {
	                    if try_termux_clipboard_set(msg.text.trim_end()) {
	                        push_sys_log(
	                            &mut sys_log,
	                            config.sys_log_limit,
	                            "Copied selected message",
	                        );
	                    } else {
	                        push_sys_log(
	                            &mut sys_log,
	                            config.sys_log_limit,
	                            "Copy failed: termux-clipboard-set unavailable",
	                        );
	                    }
	                    continue;
	                }
	                if mode == Mode::Idle && menu_open && !ctrl {
	                    let items_now = filter_commands_for_input(&input, sse_enabled, chat_target);
	                    if !items_now.is_empty() && command_menu_selected >= items_now.len() {
	                        command_menu_selected = items_now.len().saturating_sub(1);
                    }
                    match key.code {
                        KeyCode::Up => {
                            command_menu_selected = command_menu_selected.saturating_sub(1);
                            continue;
                        }
                        KeyCode::Down => {
                            if !items_now.is_empty() {
                                command_menu_selected = (command_menu_selected + 1)
                                    .min(items_now.len().saturating_sub(1));
                            }
                            continue;
                        }
                        KeyCode::Enter => {
                            if let Some(sel) = items_now.get(command_menu_selected) {
                                let cmd = sel.cmd;
                                let raw = if cmd.eq_ignore_ascii_case("/sse") {
                                    if sel.desc.eq_ignore_ascii_case("off") {
                                        "/SSE Close".to_string()
                                    } else {
                                        "/SSE Open".to_string()
                                    }
                                } else if cmd.eq_ignore_ascii_case("/switch to") {
                                    format!("/Switch {}", sel.desc)
                                } else {
                                    cmd.to_string()
                                };
                                if cmd.eq_ignore_ascii_case("/quit")
                                    || cmd.eq_ignore_ascii_case("/cmd")
                                    || cmd.eq_ignore_ascii_case("/settings")
                                    || cmd.eq_ignore_ascii_case("/sse")
                                    || cmd.eq_ignore_ascii_case("/switch to")
                                {
                                    reset_input_buffer(
                                        &mut input,
                                        &mut cursor,
                                        &mut input_chars,
                                        &mut last_input_at,
                                    );
                                    command_menu_suppress = true;
                                    if handle_command(HandleCommandArgs {
                                        core: &mut core,
                                        raw: &raw,
                                        meta: &mut metamemo,
                                        context_usage: &mut context_usage,
                                        mode: &mut mode,
                                        active_kind: &mut active_kind,
                                        chat_target: &mut chat_target,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        screen: &mut screen,
                                        should_exit: &mut should_exit,
                                        sse_enabled: &mut sse_enabled,
                                        enter_cmd_shell: &mut pending_cmd_shell,
                                        exit_code: &mut exit_code,
                                        sys_cfg: &mut sys_cfg,
                                        sys_cfg_path: &sys_cfg_path,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                    })? {
	                                        reset_after_command(ResetAfterCommandArgs {
	                                            core: &mut core,
	                                            render_cache: &mut render_cache,
	                                            config: &config,
	                                            reveal_idx: &mut reveal_idx,
	                                            expanded_tool_idxs: &mut expanded_tool_idxs,
	                                            ctx_compact_notice_idx: &mut ctx_compact_notice_idx,
	                                            expanded_thinking_idxs: &mut expanded_thinking_idxs,
	                                            selected_msg_idx: &mut selected_msg_idx,
	                                            tool_preview_chat_idx: &mut tool_preview_chat_idx,
                                            thinking_text: &mut thinking_text,
                                            thinking_idx: &mut thinking_idx,
                                            thinking_in_progress: &mut thinking_in_progress,
                                            thinking_pending_idle: &mut thinking_pending_idle,
                                            thinking_scroll: &mut thinking_scroll,
                                            thinking_scroll_cap: &mut thinking_scroll_cap,
                                            thinking_full_text: &mut thinking_full_text,
                                            thinking_started_at: &mut thinking_started_at,
                                            tool_preview: &mut tool_preview,
                                            tool_preview_active: &mut tool_preview_active,
                                            tool_preview_pending_idle:
                                                &mut tool_preview_pending_idle,
                                            streaming_state: &mut streaming_state,
                                            active_tool_stream: &mut active_tool_stream,
                                            screen,
                                            settings: &mut settings,
                                            scroll: &mut scroll,
                                            follow_bottom: &mut follow_bottom,
                                        });
                                        if pending_cmd_shell && matches!(screen, Screen::Chat) {
                                            pending_cmd_shell = false;
                                            reset_input_buffer(
                                                &mut input,
                                                &mut cursor,
                                                &mut input_chars,
                                                &mut last_input_at,
                                            );
                                            let _ = run_native_shell(terminal);
                                            needs_redraw = true;
                                        }
                                        if should_exit {
                                            break;
                                        }
                                        command_menu_selected = 0;
                                        continue;
                                    }
                                } else {
                                    reset_input_buffer(
                                        &mut input,
                                        &mut cursor,
                                        &mut input_chars,
                                        &mut last_input_at,
                                    );
                                    input.push_str(cmd);
                                    cursor = input.len();
                                    input_chars = count_chars(&input);
                                    last_input_at = Some(now);
                                    command_menu_suppress = true;
                                }
                            }
                            command_menu_selected = 0;
                            continue;
                        }
                        KeyCode::Esc => {
                            command_menu_suppress = true;
                            continue;
                        }
                        _ => {}
                    }
                }

                match key.code {
                    KeyCode::Char('c') if ctrl => {
                        finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                            force: true,
                            now,
                            config: &config,
                            paste_capture: &mut paste_capture,
                            input: &mut input,
                            cursor: &mut cursor,
                            input_chars: &mut input_chars,
                            pending_pastes: &mut pending_pastes,
                            toast: &mut toast,
                            max_input_chars,
                            burst_count: &mut burst_count,
                            burst_last_at: &mut burst_last_at,
                            burst_started_at: &mut burst_started_at,
                            burst_start_cursor: &mut burst_start_cursor,
                            paste_drop_until: &mut paste_drop_until,
                            paste_guard_until: &mut paste_guard_until,
                        });
                        if mode == Mode::Generating {
                            if let Some(cancel) = active_cancel.take() {
                                cancel.store(true, Ordering::SeqCst);
                            }
                            active_request_id = None;
	                            active_request_in_tokens = None;
	                            heartbeat_request_id = None;
	                            mode = Mode::Idle;
	                            diary_state.stage = DiaryStage::Idle;
	                            expanded_tool_idxs.clear();
	                            expanded_thinking_idxs.clear();
	                            clear_thinking_state(ClearThinkingStateArgs {
	                                thinking_text: &mut thinking_text,
	                                thinking_idx: &mut thinking_idx,
	                                thinking_in_progress: &mut thinking_in_progress,
	                                thinking_pending_idle: &mut thinking_pending_idle,
                                thinking_scroll: &mut thinking_scroll,
                                thinking_scroll_cap: &mut thinking_scroll_cap,
                                thinking_full_text: &mut thinking_full_text,
                                thinking_started_at: &mut thinking_started_at,
                            });
                            clear_tool_preview_state(
                                &mut tool_preview,
                                &mut tool_preview_active,
                                &mut tool_preview_pending_idle,
                            );
                            push_sys_log(&mut sys_log, config.sys_log_limit, "请求已被取消");
                            let cancel_msg = "请求已被取消";
                            push_system_and_log(
                                &mut core,
                                &mut metamemo,
                                &mut context_usage,
                                None,
                                cancel_msg,
                            );
                        } else {
                            should_exit = true;
                        }
                    }
                    KeyCode::Esc => {
                        if matches!(mode, Mode::Generating) {
                            finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                force: true,
                                now,
                                config: &config,
                                paste_capture: &mut paste_capture,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                            });
                            if let Some(cancel) = active_cancel.take() {
                                cancel.store(true, Ordering::SeqCst);
                            }
                            active_request_id = None;
	                            active_request_in_tokens = None;
	                            heartbeat_request_id = None;
	                            mode = Mode::Idle;
	                            diary_state.stage = DiaryStage::Idle;
	                            expanded_tool_idxs.clear();
	                            expanded_thinking_idxs.clear();
	                            clear_thinking_state(ClearThinkingStateArgs {
	                                thinking_text: &mut thinking_text,
	                                thinking_idx: &mut thinking_idx,
	                                thinking_in_progress: &mut thinking_in_progress,
                                thinking_pending_idle: &mut thinking_pending_idle,
                                thinking_scroll: &mut thinking_scroll,
                                thinking_scroll_cap: &mut thinking_scroll_cap,
                                thinking_full_text: &mut thinking_full_text,
                                thinking_started_at: &mut thinking_started_at,
                            });
                            clear_tool_preview_state(
                                &mut tool_preview,
                                &mut tool_preview_active,
                                &mut tool_preview_pending_idle,
                            );
                            let prompt = build_interrupt_prompt();
                            push_system_and_log(
                                &mut core,
                                &mut metamemo,
                                &mut context_usage,
                                None,
                                &prompt,
                            );
                            jump_to_bottom(&mut scroll, &mut follow_bottom);
                            continue;
                        }
                        command_menu_suppress = true;
                    }
                    // Tab：全局展开/收起（工具详情 + 思考）。
                    // - 若当前有任意展开：先“全部收起”（解决“部分展开时 Tab 行为不明确”）
                    // - 再按一次：展开最近 N 条（防卡顿）
                    KeyCode::Tab if input.is_empty() && !menu_open => {
                        const EXPAND_TAIL_MAX: usize = 120;
                        if !expanded_tool_idxs.is_empty() || !expanded_thinking_idxs.is_empty() {
                            expanded_tool_idxs.clear();
                            expanded_thinking_idxs.clear();
                            render_cache.invalidate_all();
                            push_sys_log(&mut sys_log, config.sys_log_limit, "已全部收起");
                            follow_bottom = false;
                            continue;
                        }
                        let start = core.history.len().saturating_sub(EXPAND_TAIL_MAX.max(1));
                        for idx in start..core.history.len() {
                            if let Some(msg) = core.history.get(idx) {
                                if msg.role == Role::Tool {
                                    expanded_tool_idxs.insert(idx);
                                    let head = msg.text.trim_start();
                                    let lower = head.to_ascii_lowercase();
                                    let is_think = lower.starts_with("操作: think")
                                        || lower.starts_with("[tool:think]");
                                    if is_think {
                                        expanded_thinking_idxs.insert(idx);
                                    }
                                }
                            }
                        }
                        render_cache.invalidate_all();
                        push_sys_log(
                            &mut sys_log,
                            config.sys_log_limit,
                            format!("已全部展开（最近 {EXPAND_TAIL_MAX} 条）"),
                        );
                        follow_bottom = false;
                    }
                    // Shift+Tab：切换“详情/简约模式”（保留给审计，但避免与 Tab 的“全局展开/收起”冲突）。
                    KeyCode::BackTab if input.is_empty() && !menu_open => {
                        const TOGGLE_DEBOUNCE_MS: u64 = 250;
                        const DETAILS_TAIL_MAX: usize = 120;
                        if last_details_toggle_at.is_some_and(|t| {
                            now.saturating_duration_since(t)
                                < Duration::from_millis(TOGGLE_DEBOUNCE_MS)
                        }) {
                            continue;
                        }
                        last_details_toggle_at = Some(now);
                        details_mode = !details_mode;
                        render_cache.invalidate_all();
                        if details_mode {
                            if core.history.len() > DETAILS_TAIL_MAX {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    format!(
                                        "详情模式：仅深度渲染最近 {DETAILS_TAIL_MAX} 条（防卡顿）"
                                    ),
                                );
                            } else {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "详情模式：已开启",
                                );
                            }
                        } else {
                            push_sys_log(&mut sys_log, config.sys_log_limit, "简约模式：已开启");
                        }
                    }
                    KeyCode::Home if input.is_empty() && selected_msg_idx.is_some() => {
                        selected_msg_idx = None;
                        push_sys_log(&mut sys_log, config.sys_log_limit, "已退出选择模式");
                    }
                    // ↑/↓：不再用于“选择消息”（避免与手机手势/滚动混淆）。
                    // - 输入框有内容：仍用于多行光标上下移动
                    // - 输入框为空：仅做轻量滚动（不进入选择模式）
                    KeyCode::Up => {
                        if !input.is_empty() {
                            let width = input_width_cache.max(1);
                            cursor = move_prompt_cursor_vertical(&input, width, cursor, -1);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        } else {
                            scroll = scroll.saturating_sub(1);
                            follow_bottom = false;
                        }
                    }
                    KeyCode::Down => {
                        if !input.is_empty() {
                            let width = input_width_cache.max(1);
                            cursor = move_prompt_cursor_vertical(&input, width, cursor, 1);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        } else {
                            let max_scroll = max_scroll_cache as u16;
                            scroll = (scroll + 1).min(max_scroll);
                            follow_bottom = scroll >= max_scroll;
                        }
                    }
	                    KeyCode::Left if input.is_empty() && selected_msg_idx.is_some() => {
	                        let sel = selected_msg_idx.unwrap_or(0);
	                        if let Some(ti) = find_think_message_for_selection(&core, sel) {
	                            if expanded_thinking_idxs.contains(&ti) {
	                                expanded_thinking_idxs.remove(&ti);
	                            } else {
	                                expanded_thinking_idxs.insert(ti);
	                            }
	                            follow_bottom = false;
	                        } else {
	                            push_sys_log(
	                                &mut sys_log,
	                                config.sys_log_limit,
                                "提示：选中 AI 正文后按 ← 展开/收起思考",
                            );
                        }
                    }
	                    KeyCode::Right if input.is_empty() && selected_msg_idx.is_some() => {
	                        let sel = selected_msg_idx.unwrap_or(0);
	                        if core.history.get(sel).is_some_and(|m| m.role == Role::Tool) {
	                            if expanded_tool_idxs.contains(&sel) {
	                                expanded_tool_idxs.remove(&sel);
	                            } else {
	                                expanded_tool_idxs.insert(sel);
	                            }
	                            follow_bottom = false;
	                        } else {
	                            push_sys_log(
	                                &mut sys_log,
	                                config.sys_log_limit,
                                "提示：选中工具消息后按 → 展开/收起详情",
                            );
                        }
                    }
                    KeyCode::Left => {
                        if cursor > 0 {
                            cursor = prev_char_boundary(&input, cursor);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    KeyCode::Right => {
                        if cursor < input.len() {
                            cursor = next_char_boundary(&input, cursor);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    // PgUp/PgDn：消息选择（只在输入框为空时生效）
                    // - PgUp：进入/上移选择
                    // - PgDn：下移选择（不主动进入选择，避免误触）
                    // Ctrl+PgUp/PgDn：滚动聊天（保留键盘滚动通道）
                    KeyCode::PageUp if ctrl => {
                        scroll = scroll.saturating_sub(6);
                        follow_bottom = false;
                    }
                    KeyCode::PageDown if ctrl => {
                        let max_scroll = max_scroll_cache as u16;
                        scroll = (scroll + 6).min(max_scroll);
                        follow_bottom = scroll >= max_scroll;
                    }
                    KeyCode::PageUp => {
                        if input.is_empty() {
                            selected_msg_idx = select_prev_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                &expanded_thinking_idxs,
                                details_mode,
                            );
                            follow_bottom = false;
                            ensure_selected_visible(
                                &mut scroll,
                                &mut follow_bottom,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                                chat_height_cache,
                                max_scroll_cache,
                            );
                        }
                    }
                    KeyCode::PageDown => {
                        if input.is_empty() && selected_msg_idx.is_some() {
                            selected_msg_idx = select_next_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                &expanded_thinking_idxs,
                                details_mode,
                            );
                            follow_bottom = false;
                            ensure_selected_visible(
                                &mut scroll,
                                &mut follow_bottom,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                                chat_height_cache,
                                max_scroll_cache,
                            );
                        }
                    }
                    KeyCode::Home => {
                        if !input.is_empty() {
                            cursor = 0;
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    KeyCode::End => {
                        scroll = max_scroll_cache as u16;
                        follow_bottom = true;
                    }
                    KeyCode::Backspace => {
                        last_input_at = Some(now);
                        finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                            force: true,
                            now,
                            config: &config,
                            paste_capture: &mut paste_capture,
                            input: &mut input,
                            cursor: &mut cursor,
                            input_chars: &mut input_chars,
                            pending_pastes: &mut pending_pastes,
                            toast: &mut toast,
                            max_input_chars,
                            burst_count: &mut burst_count,
                            burst_last_at: &mut burst_last_at,
                            burst_started_at: &mut burst_started_at,
                            burst_start_cursor: &mut burst_start_cursor,
                            paste_drop_until: &mut paste_drop_until,
                            paste_guard_until: &mut paste_guard_until,
                        });
                        if try_remove_paste_placeholder_at_cursor(
                            &mut input,
                            &mut cursor,
                            &mut input_chars,
                            &mut pending_pastes,
                            PlaceholderRemove::Backspace,
                        ) {
                            command_menu_suppress = false;
                            continue;
                        }
                        if cursor > 0 {
                            let prev = prev_char_boundary(&input, cursor);
                            let removed_chars =
                                input.get(prev..cursor).map(count_chars).unwrap_or(0);
                            input.drain(prev..cursor);
                            cursor = prev;
                            input_chars = input_chars.saturating_sub(removed_chars);
                            command_menu_suppress = false;
                        }
                        prune_pending_pastes(&input, &mut pending_pastes);
                    }
                    KeyCode::Delete => {
                        last_input_at = Some(now);
                        finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                            force: true,
                            now,
                            config: &config,
                            paste_capture: &mut paste_capture,
                            input: &mut input,
                            cursor: &mut cursor,
                            input_chars: &mut input_chars,
                            pending_pastes: &mut pending_pastes,
                            toast: &mut toast,
                            max_input_chars,
                            burst_count: &mut burst_count,
                            burst_last_at: &mut burst_last_at,
                            burst_started_at: &mut burst_started_at,
                            burst_start_cursor: &mut burst_start_cursor,
                            paste_drop_until: &mut paste_drop_until,
                            paste_guard_until: &mut paste_guard_until,
                        });
                        if try_remove_paste_placeholder_at_cursor(
                            &mut input,
                            &mut cursor,
                            &mut input_chars,
                            &mut pending_pastes,
                            PlaceholderRemove::Delete,
                        ) {
                            command_menu_suppress = false;
                            continue;
                        }
                        if cursor < input.len() {
                            let next = next_char_boundary(&input, cursor);
                            let removed_chars =
                                input.get(cursor..next).map(count_chars).unwrap_or(0);
                            input.drain(cursor..next);
                            input_chars = input_chars.saturating_sub(removed_chars);
                            command_menu_suppress = false;
                        }
                        prune_pending_pastes(&input, &mut pending_pastes);
                    }
                    KeyCode::Enter => {
                        if !ctrl
                            && input.trim().is_empty()
                            && matches!(mode, Mode::Idle)
                            && let Some(sel) = selected_msg_idx
                            && let Some(msg) = core.history.get(sel)
                        {
                            if try_termux_clipboard_set(msg.text.trim_end()) {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "已复制选中消息到剪贴板",
                                );
                            } else {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "复制失败：termux-clipboard-set 不可用",
                                );
                            }
                            continue;
                        }
                        if ctrl {
                            finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                force: true,
                                now,
                                config: &config,
                                paste_capture: &mut paste_capture,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                            });
                            if !insert_newline_limited(InsertNewlineArgs {
                                now,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                command_menu_suppress: Some(&mut command_menu_suppress),
                            }) {
                                continue;
                            }
                        } else if matches!(mode, Mode::Generating | Mode::ExecutingTool) {
                            insert_newline_limited(InsertNewlineArgs {
                                now,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                command_menu_suppress: None,
                            });
                            continue;
                        } else if mode == Mode::ApprovingTool {
                            finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                force: true,
                                now,
                                config: &config,
                                paste_capture: &mut paste_capture,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                            });
                            let send = drain_input_for_send(
                                &mut input,
                                &mut cursor,
                                &mut input_chars,
                                &mut last_input_at,
                                &mut pending_pastes,
                                &mut paste_capture,
                                &mut command_menu_suppress,
                            );

	                            if let Some(decision) = normalize_confirm_input(&send) {
	                                if let Some((call, _reason, msg_idx)) =
	                                    pending_tool_confirm.take()
	                                {
	                                    if decision {
	                                        update_history_text_at(
	                                            &mut core,
	                                            &mut render_cache,
	                                            msg_idx,
	                                            "您已选择：立即执行",
	                                        );
	                                        spawn_tool_execution(call, active_kind, tx.clone());
	                                        mode = Mode::ExecutingTool;
	                                    } else {
	                                        update_history_text_at(
	                                            &mut core,
	                                            &mut render_cache,
	                                            msg_idx,
	                                            "您已选择：拒绝执行",
	                                        );
	                                        match active_kind {
	                                            MindKind::Sub => {
	                                                dog_state.push_tool("工具确认：用户拒绝执行")
	                                            }
	                                            MindKind::Main => {
	                                                main_state.push_tool("工具确认：用户拒绝执行")
	                                            }
	                                        }
	                                        mode = Mode::Idle;
	                                        let has_next = try_start_next_tool(TryStartNextToolArgs {
	                                            pending_tools: &mut pending_tools,
	                                            pending_tool_confirm: &mut pending_tool_confirm,
	                                            tx: tx.clone(),
	                                            core: &mut core,
	                                            meta: &mut metamemo,
	                                            context_usage: &mut context_usage,
	                                            mode: &mut mode,
	                                            sys_log: &mut sys_log,
	                                            sys_log_limit: config.sys_log_limit,
	                                            sys_cfg: &sys_cfg,
	                                            owner: active_kind,
	                                        });
	                                        if !has_next && matches!(active_kind, MindKind::Sub) {
	                                            if let Some(in_tokens) = try_start_dog_generation(
	                                                TryStartDogGenerationArgs {
	                                                    kind: MindKind::Sub,
	                                                    dog_client: &dog_client,
                                                    dog_state: &dog_state,
                                                    extra_system: None,
                                                    tx: &tx,
                                                    config: &config,
                                                    mode: &mut mode,
                                                    active_kind: &mut active_kind,
                                                    sending_until: &mut sending_until,
                                                    sys_log: &mut sys_log,
                                                    sys_log_limit: config.sys_log_limit,
                                                    streaming_state: &mut streaming_state,
                                                    request_seq: &mut request_seq,
                                                    active_request_id: &mut active_request_id,
                                                    active_cancel: &mut active_cancel,
                                                    sse_enabled,
                                                },
                                            ) {
                                                record_request_in_tokens(
                                                    &mut core,
                                                    &mut token_totals,
                                                    &token_total_path,
                                                    &context_usage,
                                                    &mut active_request_in_tokens,
                                                    in_tokens,
                                                );
                                            }
	                                        } else if !has_next
	                                            && matches!(active_kind, MindKind::Main)
	                                            && let Some(in_tokens) = try_start_main_generation(
	                                                TryStartMainGenerationArgs {
	                                                    main_client: &main_client,
	                                                    main_state: &main_state,
	                                                    extra_system: None,
                                                    tx: &tx,
                                                    config: &config,
                                                    mode: &mut mode,
                                                    active_kind: &mut active_kind,
                                                    sending_until: &mut sending_until,
                                                    sys_log: &mut sys_log,
                                                    sys_log_limit: config.sys_log_limit,
                                                    streaming_state: &mut streaming_state,
                                                    request_seq: &mut request_seq,
                                                    active_request_id: &mut active_request_id,
                                                    active_cancel: &mut active_cancel,
                                                    sse_enabled,
                                                },
                                            )
                                        {
                                            record_request_in_tokens(
                                                &mut core,
                                                &mut token_totals,
                                                &token_total_path,
                                                &context_usage,
                                                &mut active_request_in_tokens,
                                                in_tokens,
                                            );
                                        }
	                                    }
	                                } else {
	                                    mode = Mode::Idle;
	                                }
	                            } else {
	                                push_system_and_log(
	                                    &mut core,
	                                    &mut metamemo,
                                    &mut context_usage,
                                    Some("tool"),
                                    "请输入 yes 或 no",
                                );
                            }
                        } else if mode == Mode::Idle {
                            let context_used = context_usage.tokens();
                            let ctx_limit = sys_cfg.context_k.saturating_mul(1000).max(1);
                            let ctx_pct = calc_pct(context_used, ctx_limit);
                            if matches!(diary_state.stage, DiaryStage::Idle) && ctx_pct >= 100 {
                                finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                    force: true,
                                    now,
                                    config: &config,
                                    paste_capture: &mut paste_capture,
                                    input: &mut input,
                                    cursor: &mut cursor,
                                    input_chars: &mut input_chars,
                                    pending_pastes: &mut pending_pastes,
                                    toast: &mut toast,
                                    max_input_chars,
                                    burst_count: &mut burst_count,
                                    burst_last_at: &mut burst_last_at,
                                    burst_started_at: &mut burst_started_at,
                                    burst_start_cursor: &mut burst_start_cursor,
                                    paste_drop_until: &mut paste_drop_until,
                                    paste_guard_until: &mut paste_guard_until,
                                });
                                let last_diary = read_last_datememo_entry(&memo_db);
                                let context_text = read_contextmemo_text(&config.contextmemo_path);
                                if try_start_main_diary(TryStartMainDiaryArgs {
                                    main_client: &main_client,
                                    main_prompt: &context_prompts.main_prompt,
                                    system_prompt: &main_state.prompt,
                                    last_diary: &last_diary,
                                    context_text: &context_text,
                                    tx: &tx,
                                    config: &config,
                                    mode: &mut mode,
                                    active_kind: &mut active_kind,
                                    sending_until: &mut sending_until,
                                    sys_log: &mut sys_log,
                                    sys_log_limit: config.sys_log_limit,
                                    streaming_state: &mut streaming_state,
                                    request_seq: &mut request_seq,
                                    active_request_id: &mut active_request_id,
                                    active_cancel: &mut active_cancel,
                                    sse_enabled,
                                }) {
                                    diary_state.stage = DiaryStage::WaitingMain;
                                    continue;
                                } else {
                                    push_system_and_log(
                                        &mut core,
                                        &mut metamemo,
                                        &mut context_usage,
                                        Some("main"),
                                        "MAIN 未配置 API Key，无法写日记。",
                                    );
                                    continue;
                                }
                            }
                            if let Some(c) = paste_capture.as_mut() {
                                let gap = now.saturating_duration_since(c.last_at);
                                if gap >= Duration::from_millis(config.paste_capture_flush_gap_ms) {
                                    finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                        force: true,
                                        now,
                                        config: &config,
                                        paste_capture: &mut paste_capture,
                                        input: &mut input,
                                        cursor: &mut cursor,
                                        input_chars: &mut input_chars,
                                        pending_pastes: &mut pending_pastes,
                                        toast: &mut toast,
                                        max_input_chars,
                                        burst_count: &mut burst_count,
                                        burst_last_at: &mut burst_last_at,
                                        burst_started_at: &mut burst_started_at,
                                        burst_start_cursor: &mut burst_start_cursor,
                                        paste_drop_until: &mut paste_drop_until,
                                        paste_guard_until: &mut paste_guard_until,
                                    });
                                } else {
                                    c.buf.push('\n');
                                    c.last_at = now;
                                    paste_guard_until = Some(
                                        now + Duration::from_millis(config.paste_send_inhibit_ms),
                                    );
                                    continue;
                                }
                            }

                            update_paste_burst(
                                now,
                                &mut burst_last_at,
                                &mut burst_started_at,
                                &mut burst_count,
                                &mut burst_start_cursor,
                                cursor,
                            );
                            let fast_burst = burst_count >= 2;
                            if fast_burst {
                                paste_guard_until =
                                    Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
                            }
                            let guard_active = paste_guard_until.is_some_and(|t| now < t);
                            let looks_like_paste = is_paste_like_activity(
                                now,
                                burst_last_at,
                                burst_started_at,
                                burst_count,
                            );
                            let multi_line_guard = input.contains('\n')
                                && burst_started_at.is_some_and(|t| {
                                    now.saturating_duration_since(t) <= Duration::from_millis(180)
                                })
                                && burst_count >= 3;
                            let paste_context = paste_capture.is_some()
                                || !pending_pastes.is_empty()
                                || looks_like_paste
                                || multi_line_guard
                                || guard_active;
                            if paste_context {
                                last_input_at = Some(now);
                                paste_guard_until =
                                    Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
                                let inserted = insert_newline_limited(InsertNewlineArgs {
                                    now,
                                    input: &mut input,
                                    cursor: &mut cursor,
                                    input_chars: &mut input_chars,
                                    pending_pastes: &mut pending_pastes,
                                    toast: &mut toast,
                                    max_input_chars,
                                    command_menu_suppress: None,
                                });
                                if inserted {
                                    maybe_begin_paste_capture(input::MaybeBeginPasteCaptureArgs {
                                        now,
                                        capture: &mut paste_capture,
                                        input: &mut input,
                                        cursor: &mut cursor,
                                        input_chars: &mut input_chars,
                                        toast: &mut toast,
                                        burst_count,
                                        burst_started_at,
                                        burst_start_cursor,
                                    });
                                }
                                continue;
                            }
                            finalize_paste_capture_and_handle(FinalizePasteCaptureArgs {
                                force: true,
                                now,
                                config: &config,
                                paste_capture: &mut paste_capture,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                            });
                            let send = drain_input_for_send(
                                &mut input,
                                &mut cursor,
                                &mut input_chars,
                                &mut last_input_at,
                                &mut pending_pastes,
                                &mut paste_capture,
                                &mut command_menu_suppress,
                            );
                            if send.trim().is_empty() {
                                continue;
                            }
                            // 已移除弹窗/窗口栈：不需要“清屏关闭弹窗”的额外处理。
                            if handle_command(HandleCommandArgs {
                                core: &mut core,
                                raw: send.trim_end(),
                                meta: &mut metamemo,
                                context_usage: &mut context_usage,
                                mode: &mut mode,
                                active_kind: &mut active_kind,
                                chat_target: &mut chat_target,
                                dog_state: &mut dog_state,
                                main_state: &mut main_state,
                                mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                screen: &mut screen,
                                should_exit: &mut should_exit,
                                sse_enabled: &mut sse_enabled,
                                enter_cmd_shell: &mut pending_cmd_shell,
                                exit_code: &mut exit_code,
                                sys_cfg: &mut sys_cfg,
                                sys_cfg_path: &sys_cfg_path,
                                sys_log: &mut sys_log,
                                sys_log_limit: config.sys_log_limit,
                            })? {
	                                reset_after_command(ResetAfterCommandArgs {
	                                    core: &mut core,
	                                    render_cache: &mut render_cache,
	                                    config: &config,
	                                    reveal_idx: &mut reveal_idx,
	                                    expanded_tool_idxs: &mut expanded_tool_idxs,
	                                    ctx_compact_notice_idx: &mut ctx_compact_notice_idx,
	                                    expanded_thinking_idxs: &mut expanded_thinking_idxs,
	                                    selected_msg_idx: &mut selected_msg_idx,
	                                    tool_preview_chat_idx: &mut tool_preview_chat_idx,
                                    thinking_text: &mut thinking_text,
                                    thinking_idx: &mut thinking_idx,
                                    thinking_in_progress: &mut thinking_in_progress,
                                    thinking_pending_idle: &mut thinking_pending_idle,
                                    thinking_scroll: &mut thinking_scroll,
                                    thinking_scroll_cap: &mut thinking_scroll_cap,
                                    thinking_full_text: &mut thinking_full_text,
                                    thinking_started_at: &mut thinking_started_at,
                                    tool_preview: &mut tool_preview,
                                    tool_preview_active: &mut tool_preview_active,
                                    tool_preview_pending_idle: &mut tool_preview_pending_idle,
                                    streaming_state: &mut streaming_state,
                                    active_tool_stream: &mut active_tool_stream,
                                    screen,
                                    settings: &mut settings,
                                    scroll: &mut scroll,
                                    follow_bottom: &mut follow_bottom,
                                });
                                if pending_cmd_shell && matches!(screen, Screen::Chat) {
                                    pending_cmd_shell = false;
                                    reset_input_buffer(
                                        &mut input,
                                        &mut cursor,
                                        &mut input_chars,
                                        &mut last_input_at,
                                    );
                                    let _ = run_native_shell(terminal);
                                    needs_redraw = true;
                                }
                                if should_exit {
                                    break;
                                }
                                continue;
                            }

                            let send_text = send.clone();
                            if let Some(pending) = pending_mind_half.take() {
                                let item = format!(
                                    "{MIND_CTX_GUARD}\n协同记录（未完成一轮）：{}",
                                    format_mind_half_line(
                                        pending.from,
                                        pending.to,
                                        &pending.brief,
                                        &pending.content
                                    )
                                );
                                main_state.push_ctx_pool_item(&sys_cfg, &item);
                                dog_state.push_ctx_pool_item(&sys_cfg, &item);
                            }
                            core.push_user(send);
                            // 聊天动态层：用户一发出消息就插入“思考占位”，用于首包前的 ● 闪烁。
                            // 注意：这是“可观测性”设计的一部分（降低延迟感），不依赖服务端首包。
                            let target = chat_target;
                            thinking_text.clear();
                            thinking_full_text.clear();
                            thinking_started_at = None;
                            thinking_pending_idle = false;
                            // 非流式模式（SSE 关闭）不显示思考；思考只在详情模式回看（由 stream_end 推入 Tool 消息）。
                            if sse_enabled {
                                thinking_in_progress = true;
                                let stub =
                                    build_thinking_tool_message_stub(target, "running", "Thinking");
                                core.push_tool(stub);
                                thinking_idx = Some(core.history.len().saturating_sub(1));
                            } else {
                                thinking_in_progress = false;
                                thinking_idx = None;
                            }
                            let user_agent = if matches!(chat_target, MindKind::Main) {
                                Some("main")
                            } else {
                                None
                            };
                            log_memos(
                                &mut metamemo,
                                &mut context_usage,
                                "user",
                                user_agent,
                                &send_text,
                            );
                            if matches!(chat_target, MindKind::Main) {
                                log_contextmemo(
                                    &config.contextmemo_path,
                                    &mut context_usage,
                                    "user",
                                    &send_text,
                                );
                            }
	                            defer_heartbeat(&mut next_heartbeat_at, now);
	                            expanded_tool_idxs.clear();
	                            expanded_thinking_idxs.clear();
	                            scroll = u16::MAX;
	                            follow_bottom = true;

                            active_kind = target;
                            let ctx_limit = sys_cfg.context_k.saturating_mul(1000).max(1);
                            let fastmemo = read_fastmemo_for_context();
                            match target {
                                MindKind::Main => main_state.refresh_fastmemo_system(&fastmemo),
                                MindKind::Sub => dog_state.refresh_fastmemo_system(&fastmemo),
                            }
                            match target {
                                MindKind::Main => {
                                    let _ = main_state.push_user(&send_text, ctx_limit);
                                }
                                MindKind::Sub => {
                                    let _ = dog_state.push_user(&send_text, ctx_limit);
                                }
                            }
                            let extra_system = if match target {
                                MindKind::Main => main_state.begin_context_compact(),
                                MindKind::Sub => dog_state.begin_context_compact(),
                            } {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "↻ Context Summary 压缩中",
                                );
                                Some(render_context_compact_prompt(
                                    &context_compact_prompt_text,
                                    target,
                                    &sys_cfg,
                                    &send_text,
                                ))
                            } else {
                                None
                            };
                            let started = if matches!(target, MindKind::Main) {
                                try_start_main_generation(TryStartMainGenerationArgs {
                                    main_client: &main_client,
                                    main_state: &main_state,
                                    extra_system: extra_system.clone(),
                                    tx: &tx,
                                    config: &config,
                                    mode: &mut mode,
                                    active_kind: &mut active_kind,
                                    sending_until: &mut sending_until,
                                    sys_log: &mut sys_log,
                                    sys_log_limit: config.sys_log_limit,
                                    streaming_state: &mut streaming_state,
                                    request_seq: &mut request_seq,
                                    active_request_id: &mut active_request_id,
                                    active_cancel: &mut active_cancel,
                                    sse_enabled,
                                })
                            } else {
                                try_start_dog_generation(TryStartDogGenerationArgs {
                                    kind: MindKind::Sub,
                                    dog_client: &dog_client,
                                    dog_state: &dog_state,
                                    extra_system,
                                    tx: &tx,
                                    config: &config,
                                    mode: &mut mode,
                                    active_kind: &mut active_kind,
                                    sending_until: &mut sending_until,
                                    sys_log: &mut sys_log,
                                    sys_log_limit: config.sys_log_limit,
                                    streaming_state: &mut streaming_state,
                                    request_seq: &mut request_seq,
                                    active_request_id: &mut active_request_id,
                                    active_cancel: &mut active_cancel,
                                    sse_enabled,
                                })
                            };
                            if let Some(in_tokens) = started {
                                record_request_in_tokens(
                                    &mut core,
                                    &mut token_totals,
                                    &token_total_path,
                                    &context_usage,
                                    &mut active_request_in_tokens,
                                    in_tokens,
                                );
                            } else {
                                mode = Mode::Idle;
                                let label = mind_label(target);
                                let msg = format!("{label} 未配置 API Key，无法请求。");
                                push_system_and_log(
                                    &mut core,
                                    &mut metamemo,
                                    &mut context_usage,
                                    None,
                                    &msg,
                                );
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    format!("{label}: 未配置 API Key"),
                                );
                            }
                        }
                    }
                    KeyCode::Char(ch) => {
                        last_input_at = Some(now);
                        if let Some(c) = paste_capture.as_mut() {
                            cursor =
                                snap_cursor_out_of_placeholder(&input, &pending_pastes, cursor);
                            c.buf.push(ch);
                            c.last_at = now;
                            if flush_paste_capture_if_overflow(FlushPasteCaptureOverflowArgs {
                                now,
                                config: &config,
                                paste_capture: &mut paste_capture,
                                paste_capture_max_bytes,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                            }) {
                                continue;
                            }
                            continue;
                        }

                        handle_inline_insert(
                            InlineInsertArgs {
                                now,
                                config: &config,
                                paste_capture_max_bytes,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                toast: &mut toast,
                                max_input_chars,
                                burst_count: &mut burst_count,
                                burst_last_at: &mut burst_last_at,
                                burst_started_at: &mut burst_started_at,
                                burst_start_cursor: &mut burst_start_cursor,
                                paste_capture: &mut paste_capture,
                                paste_drop_until: &mut paste_drop_until,
                                paste_guard_until: &mut paste_guard_until,
                                command_menu_suppress: &mut command_menu_suppress,
                            },
                            |input, cursor, input_chars, max_input_chars| {
                                try_insert_char_limited(
                                    input,
                                    cursor,
                                    ch,
                                    input_chars,
                                    max_input_chars,
                                )
                            },
                        );
                    }
                    _ => {}
                }
                if should_exit {
                    break;
                }
            }
            Event::Paste(pasted) => {
                needs_redraw = true;
                let now = Instant::now();
                if screen == Screen::Settings {
                    if matches!(settings.focus, SettingsFocus::Input | SettingsFocus::Prompt) {
                        settings
                            .edit_buffer
                            .insert_str(settings.edit_cursor, &pasted);
                        settings.edit_cursor = settings.edit_cursor.saturating_add(pasted.len());
                    }
                    continue;
                }
                last_input_at = Some(now);
                paste_guard_until = Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
                if paste_drop_until.is_some_and(|t| now < t) {
                    continue;
                }
                if let Some(c) = paste_capture.as_mut() {
                    c.buf.push_str(&pasted);
                    c.last_at = now;
                    flush_paste_capture_if_overflow(FlushPasteCaptureOverflowArgs {
                        now,
                        config: &config,
                        paste_capture: &mut paste_capture,
                        paste_capture_max_bytes,
                        input: &mut input,
                        cursor: &mut cursor,
                        input_chars: &mut input_chars,
                        pending_pastes: &mut pending_pastes,
                        toast: &mut toast,
                        max_input_chars,
                        burst_count: &mut burst_count,
                        burst_last_at: &mut burst_last_at,
                        burst_started_at: &mut burst_started_at,
                        burst_start_cursor: &mut burst_start_cursor,
                        paste_drop_until: &mut paste_drop_until,
                        paste_guard_until: &mut paste_guard_until,
                    });
                    continue;
                }

                paste_capture = Some(PasteCapture {
                    last_at: now,
                    buf: pasted,
                });
                reset_paste_burst(
                    &mut burst_count,
                    &mut burst_last_at,
                    &mut burst_started_at,
                    &mut burst_start_cursor,
                );
                flush_paste_capture_if_overflow(FlushPasteCaptureOverflowArgs {
                    now,
                    config: &config,
                    paste_capture: &mut paste_capture,
                    paste_capture_max_bytes,
                    input: &mut input,
                    cursor: &mut cursor,
                    input_chars: &mut input_chars,
                    pending_pastes: &mut pending_pastes,
                    toast: &mut toast,
                    max_input_chars,
                    burst_count: &mut burst_count,
                    burst_last_at: &mut burst_last_at,
                    burst_started_at: &mut burst_started_at,
                    burst_start_cursor: &mut burst_start_cursor,
                    paste_drop_until: &mut paste_drop_until,
                    paste_guard_until: &mut paste_guard_until,
                });
            }
            Event::Resize(_, _) => {
                needs_redraw = true;
                if screen == Screen::Settings
                    && matches!(settings.focus, SettingsFocus::Prompt)
                    && let Ok(rect) = terminal.size()
                {
                    let width = rect.width.saturating_sub(2) as usize;
                    settings_editor_width_cache = width.max(1);
                    settings.edit_cursor = settings.edit_cursor.min(settings.edit_buffer.len());
                }
            }
            _ => {}
        }

        drain_events!();
    }

    if wake_lock_acquired {
        let _ = try_termux_wake_lock(false);
    }
    Ok(exit_code)
}

struct DrainAsyncEventsArgs<'a> {
    core: &'a mut Core,
    rx: &'a Receiver<AsyncEvent>,
    render_cache: &'a mut ui::ChatRenderCache,
    pty: &'a mut Option<PtyUiState>,
    pty_view: &'a mut bool,
    pty_done: &'a mut bool,
    pty_exit_requested: &'a mut bool,
    mode: &'a mut Mode,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
    active_kind: &'a mut MindKind,
    reveal_idx: &'a mut Option<usize>,
    reveal_len: &'a mut usize,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    dog_state: &'a mut DogState,
    dog_client: &'a Option<DogClient>,
    main_state: &'a mut DogState,
    main_client: &'a Option<DogClient>,
    mind_context: &'a mut MindContextPool,
    _mind_ctx_idx_main: &'a mut Option<usize>,
    _mind_ctx_idx_dog: &'a mut Option<usize>,
    sending_until: &'a mut Option<Instant>,
    token_totals: &'a mut TokenTotals,
    token_total_path: &'a Path,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    diary_state: &'a mut DiaryState,
    ctx_compact_notice_idx: &'a mut Option<usize>,
    mind_rate_window: &'a mut VecDeque<Instant>,
    pending_mind_half: &'a mut Option<PendingMindHalf>,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,
    active_request_is_mind: &'a mut bool,
    active_request_is_fastmemo_compact: &'a mut bool,
    streaming_state: &'a mut StreamingState,
    mind_pulse: &'a mut Option<MindPulse>,
    retry_status: &'a mut Option<String>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    config: &'a AppConfig,
    context_compact_prompt_text: &'a str,
    fastmemo_compact_prompt_text: &'a str,
    auto_fastmemo_compact: &'a mut bool,
    fastmemo_compact_inflight: &'a mut bool,
    fastmemo_compact_edit_mask: &'a mut u8,
    fastmemo_compact_retry_at: &'a mut Option<Instant>,
    sys_cfg: &'a mut SystemConfig,
    heartbeat_minutes_cache: &'a mut usize,
    next_heartbeat_at: &'a mut Instant,
    heartbeat_request_id: &'a mut Option<u64>,
    response_count: &'a mut u64,
    _pulse_notice: &'a mut Option<PulseNotice>,
	tx: &'a mpsc::Sender<AsyncEvent>,
	pending_tools: &'a mut VecDeque<ToolCall>,
	pending_tool_confirm: &'a mut Option<(ToolCall, String, usize)>,
	request_seq: &'a mut u64,
	active_request_id: &'a mut Option<u64>,
	active_cancel: &'a mut Option<Arc<AtomicBool>>,
	active_request_in_tokens: &'a mut Option<u64>,
    sse_enabled: bool,
    run_log_path: &'a str,
}

struct ModelStreamChunkArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    owner: MindKind,
    streaming_state: &'a mut StreamingState,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,
    sse_enabled: bool,
}

fn handle_model_stream_chunk(args: ModelStreamChunkArgs<'_>, content: &str, reasoning: &str) {
    let ModelStreamChunkArgs {
        core,
        render_cache,
        owner,
        streaming_state,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_started_at,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        sse_enabled,
    } = args;

    if !reasoning.is_empty() && !streaming_state.has_content {
        if thinking_started_at.is_none() {
            *thinking_started_at = Some(Instant::now());
        }
        thinking_full_text.push_str(reasoning);
        if sse_enabled && !*tool_preview_active {
            thinking_text.push_str(reasoning);
            *thinking_in_progress = true;
            *thinking_pending_idle = false;
        }
        if let Some(idx) = *thinking_idx {
            let preview = if !thinking_text.trim().is_empty() {
                thinking_text.as_str()
            } else {
                thinking_full_text.as_str()
            };
            if let Some(entry) = core.history.get_mut(idx) {
                entry.text = build_thinking_tool_message_running(owner, preview);
                render_cache.invalidate(idx);
            }
        }
    }

    if content.is_empty() {
        return;
    }

    let first_chunk = streaming_state.append_content(content);
    if sse_enabled && !*tool_preview_active {
        if let Some(tail) = open_thinking_tail(&streaming_state.raw_text) {
            if thinking_started_at.is_none() {
                *thinking_started_at = Some(Instant::now());
            }
            thinking_text.clear();
            thinking_text.push_str(&tail);
            *thinking_in_progress = true;
            *thinking_pending_idle = false;
            if let Some(idx) = *thinking_idx {
                let preview = thinking_text.as_str();
                if let Some(entry) = core.history.get_mut(idx) {
                    entry.text = build_thinking_tool_message_running(owner, preview);
                    render_cache.invalidate(idx);
                }
            }
        }
    }

    if streaming_state.raw_text.contains("<thinking") {
        streaming_state.text = strip_thinking_stream(&streaming_state.raw_text);
    } else {
        streaming_state.text.clone_from(&streaming_state.raw_text);
    }

    if sse_enabled {
        if streaming_state.tool_start.is_none()
            && let Some(tool_name) = extract_tool_name_hint(&streaming_state.text)
            && let Some(start) = find_tool_start(&streaming_state.text)
        {
            let prefix = safe_prefix(&streaming_state.text, start);
            let prefix_chars = prefix.trim().chars().count();
            if prefix.trim().is_empty() || prefix_chars <= 12 {
                streaming_state.tool_start = Some(start);
                // 不在“思考窗”展示工具预览，不展示工具 JSON，只在聊天区放一个极简 WORK 占位。
                tool_preview.clear();
                *tool_preview_active = false;
                *tool_preview_pending_idle = false;
                thinking_text.clear();
                *thinking_in_progress = false;
                *thinking_pending_idle = false;
                if let Some(idx) = streaming_state.idx {
                    if let Some(entry) = core.history.get_mut(idx) {
                        entry.role = Role::Tool;
                        // 工具 JSON 预览阶段：不显示原始 JSON，只给一个可读占位，等 ToolStreamStart 再补齐详情。
                        entry.text = crate::mcp::tool_display_label(tool_name.trim());
                        render_cache.invalidate(idx);
                    }
                    *tool_preview_chat_idx = Some(idx);
                }
            }
        }

        // 一旦进入 tool stub 阶段，就不再把 raw tool JSON 刷进聊天区（仅在 stream_end 时解析+执行）。
        if streaming_state.tool_start.is_some() {
            return;
        }

        if first_chunk {
            // 统一规则：思考一结束（正文开始）就立刻隐藏，不做“5 秒停留/折叠动画”。
            // 但仍把完整思考写回 Tool 消息，方便 Tab 切换到详情模式回看。
            if let Some(idx) = *thinking_idx {
                let has_thinking = !thinking_full_text.trim().is_empty();
                if has_thinking {
                    let thinking_secs = thinking_started_at
                        .as_ref()
                        .map(|t| t.elapsed().as_secs())
                        .unwrap_or(0);
                    let thinking_chars = thinking_full_text.chars().count();
                    let summary = build_thinking_summary(owner, thinking_secs);
                    let msg = build_thinking_tool_message(
                        &summary,
                        thinking_full_text,
                        thinking_secs,
                        thinking_chars,
                        owner,
                    );
                    if let Some(entry) = core.history.get_mut(idx) {
                        entry.text = msg.trim_end().to_string();
                        render_cache.invalidate(idx);
                    }
                } else if let Some(entry) = core.history.get_mut(idx) {
                    // 没有可展示思考：立刻标记为 done（详情模式也不刷屏）。
                    entry.text = build_thinking_tool_message_stub(owner, "0", "Thinking");
                    render_cache.invalidate(idx);
                }
            }
            thinking_text.clear();
            *thinking_in_progress = false;
            *thinking_pending_idle = false;
        }
        if let Some(idx) = streaming_state.idx {
            update_history_text_at(core, render_cache, idx, &streaming_state.text);
        }
    } else {
        if *tool_preview_active || *tool_preview_pending_idle {
            *tool_preview_active = false;
            *tool_preview_pending_idle = false;
            tool_preview.clear();
        }
        if first_chunk {
            *thinking_in_progress = false;
        }
        if let Some(idx) = streaming_state.idx {
            update_history_text_at(core, render_cache, idx, &streaming_state.text);
        }
    }
}

struct ModelStreamEndArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    mode: &'a mut Mode,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
    active_kind: &'a mut MindKind,
    reveal_idx: &'a mut Option<usize>,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    dog_state: &'a mut DogState,
    dog_client: &'a Option<DogClient>,
    main_state: &'a mut DogState,
    main_client: &'a Option<DogClient>,
    sending_until: &'a mut Option<Instant>,
    token_totals: &'a mut TokenTotals,
    token_total_path: &'a Path,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    diary_state: &'a mut DiaryState,
    ctx_compact_notice_idx: &'a mut Option<usize>,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,
    active_request_is_mind: &'a mut bool,
    active_request_is_fastmemo_compact: &'a mut bool,
    streaming_state: &'a mut StreamingState,
    retry_status: &'a mut Option<String>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    config: &'a AppConfig,
    context_compact_prompt_text: &'a str,
    fastmemo_compact_prompt_text: &'a str,
    auto_fastmemo_compact: &'a mut bool,
    fastmemo_compact_inflight: &'a mut bool,
    fastmemo_compact_edit_mask: &'a mut u8,
    fastmemo_compact_retry_at: &'a mut Option<Instant>,
    sys_cfg: &'a mut SystemConfig,
    heartbeat_request_id: &'a mut Option<u64>,
    response_count: &'a mut u64,
	tx: &'a mpsc::Sender<AsyncEvent>,
	pending_tools: &'a mut VecDeque<ToolCall>,
	pending_tool_confirm: &'a mut Option<(ToolCall, String, usize)>,
	request_seq: &'a mut u64,
	active_request_id: &'a mut Option<u64>,
	active_cancel: &'a mut Option<Arc<AtomicBool>>,
	active_request_in_tokens: &'a mut Option<u64>,
    sse_enabled: bool,
    run_log_path: &'a str,
}

fn handle_model_stream_end(
    args: ModelStreamEndArgs<'_>,
    kind: MindKind,
    usage: u64,
    error: Option<String>,
    request_id: u64,
) {
    let ModelStreamEndArgs {
        core,
        render_cache,
        mode,
        scroll,
	        follow_bottom,
	        active_kind,
	        reveal_idx,
	        expanded_tool_idxs,
	        active_tool_stream,
        dog_state,
        dog_client,
        main_state,
        main_client,
        sending_until,
        token_totals,
        token_total_path,
        meta,
        context_usage,
        diary_state,
        ctx_compact_notice_idx,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
	        thinking_pending_idle,
	        thinking_started_at,
	        expanded_thinking_idxs,
	        selected_msg_idx,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        active_request_is_mind,
        active_request_is_fastmemo_compact,
        streaming_state,
        retry_status,
        sys_log,
        sys_log_limit,
        config,
        context_compact_prompt_text,
        fastmemo_compact_prompt_text,
        auto_fastmemo_compact,
        fastmemo_compact_inflight,
        fastmemo_compact_edit_mask,
        fastmemo_compact_retry_at,
        sys_cfg,
        heartbeat_request_id,
        response_count,
        tx,
        pending_tools,
        pending_tool_confirm,
        request_seq,
        active_request_id,
        active_cancel,
        active_request_in_tokens,
        sse_enabled,
        run_log_path,
    } = args;

    let mut auto_context_compact: Option<MindKind> = None;
    let is_heartbeat = heartbeat_request_id
        .as_ref()
        .is_some_and(|v| *v == request_id);
    if is_heartbeat {
        *heartbeat_request_id = None;
    }
    *retry_status = None;
    *thinking_in_progress = false;
    let keep_busy = false;
    if sse_enabled {
        if *tool_preview_active && !tool_preview.trim().is_empty() {
            *tool_preview_active = false;
            *tool_preview_pending_idle = true;
        }
        // 思考结束即隐藏：不保留 pending_idle（避免“卡住/被旧状态遮挡”的显示问题）。
        *thinking_pending_idle = false;
        thinking_text.clear();
    } else {
        *thinking_pending_idle = false;
        thinking_text.clear();
        *tool_preview_pending_idle = false;
        *tool_preview_active = false;
        tool_preview.clear();
    }
    if let Some(msg) = error {
        *active_request_is_mind = false;
        *active_request_is_fastmemo_compact = false;
        update_ctx_compact_notice_if_system(
            core,
            render_cache,
            ctx_compact_notice_idx,
            "↻ CONTEXT SUMMARY 执行已中断",
        );
		        if let Some(idx) = *thinking_idx {
		            remove_message_at(RemoveMessageAtArgs {
		                core,
		                render_cache,
		                idx,
		                reveal_idx,
		                expanded_tool_idxs,
		                thinking_idx,
		                expanded_thinking_idxs,
		                selected_msg_idx,
		                tool_preview_chat_idx,
		                streaming_state,
		                active_tool_stream,
		            });
		            *thinking_idx = None;
        }
        let msg_short = truncate_with_suffix(&compact_ws_inline(&msg), 360);
        let user_err = summarize_api_error_for_user(&msg);
        let had_partial = !streaming_state.raw_text.trim().is_empty();
        runlog_event(
            "ERROR",
            "model.response.error",
            json!({
                "mind": mind_label(kind),
                "request_id": request_id,
                "is_heartbeat": is_heartbeat,
                "had_partial": had_partial,
                "error": &msg,
                "raw_text": &streaming_state.raw_text,
                "thinking": thinking_full_text.as_str(),
                "tool_preview": tool_preview.as_str(),
                "sse_enabled": sse_enabled,
            }),
        );
        if had_partial
            && let Some(idx) = streaming_state.idx
            && let Some(entry) = core.history.get_mut(idx)
        {
            if tool_preview_chat_idx.is_some_and(|v| v == idx) {
                entry.role = Role::Assistant;
                entry.mind = Some(kind);
                *tool_preview_chat_idx = None;
            }
            entry.text.clear();
            render_cache.invalidate(idx);
        }
        let log_level = "ERROR";
        let log_tag = if had_partial {
            "stream interrupted (discarded)"
        } else {
            "API error"
        };
        append_run_log(
            run_log_path,
            log_level,
            &format!("{} {log_tag}: {msg_short}", mind_label(kind)),
        );
        if !is_heartbeat {
            if had_partial {
                let note = format!("系统错误：流式中断（已丢弃部分输出）。{user_err}");
                push_system_and_log(core, meta, context_usage, None, &note);
            } else {
                let note = format!("系统错误：生成失败。{user_err}");
                push_system_and_log(core, meta, context_usage, None, &note);
            }
        }
        push_sys_log(
            sys_log,
            sys_log_limit,
            format!("[{}] 系统错误：{user_err}", mind_label(kind)),
        );
        match kind {
            MindKind::Sub => dog_state.abort_context_compact(),
            MindKind::Main => main_state.abort_context_compact(),
        }
        if diary_state.active() {
            diary_state.stage = DiaryStage::Idle;
        }
    } else if !is_heartbeat {
        let raw_assistant = streaming_state.raw_text.trim_end().to_string();
        let mut assistant_text = raw_assistant.clone();
        if raw_assistant.contains("<thinking>") {
            let (extra_thinking, cleaned) = extract_thinking_tags(&raw_assistant);
            if !extra_thinking.trim().is_empty() {
                if !thinking_full_text.trim().is_empty() {
                    thinking_full_text.push('\n');
                }
                thinking_full_text.push_str(extra_thinking.trim());
            }
            assistant_text = cleaned.trim().to_string();
        }
        let mut extracted_from_assistant = false;
        let mut tool_block_from_assistant: Option<String> = find_tool_start(&assistant_text)
            .map(|start| assistant_text[start..].trim().to_string())
            .filter(|s| !s.is_empty());
        let mut extracted: Vec<ToolCall> = Vec::new();
        if let Ok((calls, cleaned)) = extract_tool_calls(&assistant_text) {
            if !calls.is_empty() {
                extracted = calls;
                extracted_from_assistant = true;
            }
            assistant_text = cleaned;
        }
        let (assistant_clean, tool_echo_blocks) = extract_internal_tool_echo_blocks(&assistant_text);
        assistant_text = assistant_clean;
        let tool_echo_msgs: Vec<String> = tool_echo_blocks
            .into_iter()
            .filter(|s| !s.trim().is_empty())
            .map(|b| wrap_tool_echo_block_as_tool_message(&b))
            .collect();
        if extracted_from_assistant && !extracted.is_empty() {
            let mut preview = tool_block_from_assistant.take().unwrap_or_else(|| {
                extracted
                    .iter()
                    .map(format_tool_call_preview)
                    .collect::<Vec<_>>()
                    .join("\n\n")
            });
            truncate_to_max_bytes(&mut preview, TOOL_STREAM_PREVIEW_MAX);
            tool_preview.clear();
            tool_preview.push_str(preview.trim_end());
            *tool_preview_active = false;
            *tool_preview_pending_idle = true;
        }
        // 已移除弹窗/窗口栈：正文只保留在聊天区（必要时由“动态层/回看”机制提供可观测性）。
        let tool_calls_log: Vec<serde_json::Value> =
            extracted.iter().map(tool_call_log_fields).collect();
        runlog_event(
            "INFO",
            "model.response.end",
            json!({
                "mind": mind_label(kind),
                "request_id": request_id,
                "is_heartbeat": false,
                "usage_total_tokens": usage,
                "raw_assistant": &raw_assistant,
                "assistant_text": &assistant_text,
                "thinking": thinking_full_text.as_str(),
                "extracted_from_assistant": extracted_from_assistant,
                "tool_calls": tool_calls_log,
                "tool_preview": tool_preview.as_str(),
                "sse_enabled": sse_enabled,
            }),
        );
        if extracted_from_assistant {
            extracted.retain(|c| c.tool != "mind_msg");
        }
        if *active_request_is_mind {
            extracted.retain(|c| c.tool == "mind_msg");
        }
        if *active_request_is_fastmemo_compact {
            extracted.retain(|c| c.tool == "memory_read" || c.tool == "memory_edit");
        }
        if matches!(diary_state.stage, DiaryStage::WaitingMain) {
            if *active_request_is_mind {
                extracted.retain(|c| c.tool == "datememo_add");
            } else {
                extracted.retain(|c| c.tool == "context_compact");
            }
        }
        if extracted.is_empty()
            && let Some(stub_idx) = tool_preview_chat_idx.take()
            && streaming_state.idx == Some(stub_idx)
            && let Some(entry) = core.history.get_mut(stub_idx)
        {
            entry.role = Role::Assistant;
            entry.mind = Some(kind);
            entry.text.clone_from(&assistant_text);
            render_cache.invalidate(stub_idx);
        }
        if *active_request_is_fastmemo_compact && extracted.is_empty() {
            // fastmemo 压缩必须通过工具链落地；若未收到任何工具调用，则视为失败并允许后续重试。
            if *fastmemo_compact_inflight {
                *fastmemo_compact_inflight = false;
            }
            *fastmemo_compact_edit_mask = 0;
            *fastmemo_compact_retry_at = Some(Instant::now() + Duration::from_secs(12));
            push_sys_log(sys_log, sys_log_limit, "fastmemo 压缩失败：未收到工具调用");
        }
        let has_diary_tool = extracted.iter().any(is_datememo_add);
        let total_tokens = if usage > 0 {
            usage
        } else {
            estimate_tokens(&assistant_text) as u64
        };
        let in_tokens = active_request_in_tokens.take().unwrap_or(0);
        let out_tokens = if usage > 0 {
            total_tokens.saturating_sub(in_tokens)
        } else {
            total_tokens
        };
        if out_tokens > 0 {
            core.add_run_tokens(out_tokens);
            match kind {
                MindKind::Sub => dog_state.set_last_usage_total(out_tokens),
                MindKind::Main => main_state.set_last_usage_total(out_tokens),
            }
            token_totals.total_out_tokens =
                token_totals.total_out_tokens.saturating_add(out_tokens);
            token_totals.total_tokens = token_totals
                .total_in_tokens
                .saturating_add(token_totals.total_out_tokens);
            token_totals.context_tokens = context_usage.tokens() as u64;
            let _ = store_token_totals(token_total_path, token_totals);
        }

        if !assistant_text.trim().is_empty()
            && !*active_request_is_mind
            && !*active_request_is_fastmemo_compact
        {
            match kind {
                MindKind::Sub => dog_state.push_assistant(&assistant_text),
                MindKind::Main => main_state.push_assistant(&assistant_text),
            }
            let agent = if matches!(kind, MindKind::Sub) {
                Some("dog")
            } else {
                Some("main")
            };
            log_memos(meta, context_usage, "assistant", agent, &assistant_text);
            if matches!(kind, MindKind::Main) && !is_pass_message(&assistant_text) {
                log_contextmemo(
                    &config.contextmemo_path,
                    context_usage,
                    "main",
                    &assistant_text,
                );
            }
            *active_kind = kind;
            push_sys_log(
                sys_log,
                sys_log_limit,
                format!("现在[萤]{}响应完成", mind_label(kind)),
            );
            let armed = match kind {
                MindKind::Sub => dog_state.arm_context_compact_if_needed(sys_cfg),
                MindKind::Main => main_state.arm_context_compact_if_needed(sys_cfg),
            };
            if armed {
                // mind 协同不触发 context_compact，避免频繁压缩影响协同链路成本/节奏。
                if !*active_request_is_mind {
                    auto_context_compact = Some(kind);
                    push_sys_log(sys_log, sys_log_limit, "↻ Context Summary 自动压缩准备中");
                }
            }
        } else if extracted.is_empty() && !*active_request_is_mind && !is_heartbeat {
            append_run_log(
                run_log_path,
                "WARN",
                &format!("{} empty assistant response", mind_label(kind)),
            );
        }
        *active_request_is_mind = false;
        if !extracted.is_empty() {
            if let Some(idx) = streaming_state.idx {
                let is_tool_stub = tool_preview_chat_idx.is_some_and(|v| v == idx)
                    && core.history.get(idx).is_some_and(|m| m.role == Role::Tool);
                if !is_tool_stub && let Some(entry) = core.history.get_mut(idx) {
                    entry.text.clone_from(&assistant_text);
                    render_cache.invalidate(idx);
                }
            }
            pending_tools.extend(extracted);
            let _ = try_start_next_tool(TryStartNextToolArgs {
                pending_tools,
                pending_tool_confirm,
                tx: tx.clone(),
                core,
                meta,
                context_usage,
                mode,
                sys_log,
                sys_log_limit,
                sys_cfg,
                owner: *active_kind,
            });
        }
        if matches!(kind, MindKind::Main)
            && matches!(diary_state.stage, DiaryStage::WaitingMain)
            && !has_diary_tool
        {
            diary_state.stage = DiaryStage::Idle;
            push_system_and_log(
                core,
                meta,
                context_usage,
                Some("main"),
                "未收到日记工具调用，已取消压缩。",
            );
            push_sys_log(sys_log, sys_log_limit, "日记工具缺失");
        }
        if !tool_echo_msgs.is_empty() {
            for msg in tool_echo_msgs {
                core.push_tool(msg);
            }
            jump_to_bottom(scroll, follow_bottom);
        }
    } else {
        let raw_assistant = streaming_state.raw_text.trim_end().to_string();
        let mut assistant_text = raw_assistant.clone();
        if raw_assistant.contains("<thinking>") {
            let (extra_thinking, cleaned) = extract_thinking_tags(&raw_assistant);
            if !extra_thinking.trim().is_empty() {
                if !thinking_full_text.trim().is_empty() {
                    thinking_full_text.push('\n');
                }
                thinking_full_text.push_str(extra_thinking.trim());
            }
            assistant_text = cleaned.trim().to_string();
        }
        // 心跳场景：DeepSeek 偶发把“答复”放进 reasoning（thinking_full_text）而 content 为空。
        // 若本次 content 为空，则用 thinking 内容回填到正文，避免只看到心跳 banner。
        assistant_text = heartbeat_reply_text(assistant_text, thinking_full_text);
        let (assistant_clean, tool_echo_blocks) = extract_internal_tool_echo_blocks(&assistant_text);
        assistant_text = assistant_clean;
        let tool_echo_msgs: Vec<String> = tool_echo_blocks
            .into_iter()
            .filter(|s| !s.trim().is_empty())
            .map(|b| wrap_tool_echo_block_as_tool_message(&b))
            .collect();
        runlog_event(
            "INFO",
            "model.response.end",
            json!({
                "mind": mind_label(kind),
                "request_id": request_id,
                "is_heartbeat": true,
                "usage_total_tokens": usage,
                "raw_assistant": &raw_assistant,
                "assistant_text": &assistant_text,
                "thinking": thinking_full_text.as_str(),
                "tool_calls": [],
                "sse_enabled": sse_enabled,
            }),
        );
        if let Some(idx) = streaming_state.idx
            && let Some(entry) = core.history.get_mut(idx)
        {
            entry.text.clone_from(&assistant_text);
            render_cache.invalidate(idx);
        }

        let total_tokens = if usage > 0 {
            usage
        } else {
            estimate_tokens(&assistant_text) as u64
        };
        let in_tokens = active_request_in_tokens.take().unwrap_or(0);
        let out_tokens = if usage > 0 {
            total_tokens.saturating_sub(in_tokens)
        } else {
            total_tokens
        };
        core.add_run_tokens(out_tokens);
        if out_tokens > 0 {
            main_state.set_last_usage_total(out_tokens);
            token_totals.total_out_tokens =
                token_totals.total_out_tokens.saturating_add(out_tokens);
            token_totals.total_tokens = token_totals
                .total_in_tokens
                .saturating_add(token_totals.total_out_tokens);
            token_totals.context_tokens = context_usage.tokens() as u64;
        }

        if !assistant_text.trim().is_empty() {
            main_state.push_assistant(&assistant_text);
            log_memos(
                meta,
                context_usage,
                "assistant",
                Some("main"),
                &assistant_text,
            );
            if !is_pass_message(&assistant_text) {
                log_contextmemo(
                    &config.contextmemo_path,
                    context_usage,
                    "main",
                    &assistant_text,
                );
            }
            if !is_pass_message(&assistant_text) {
                *response_count = response_count.saturating_add(1);
                token_totals.total_heartbeat_responses = *response_count;
            }
            *active_kind = kind;
            push_sys_log(
                sys_log,
                sys_log_limit,
                format!("现在[萤]{}响应完成", mind_label(kind)),
            );
        } else {
            append_run_log(run_log_path, "WARN", "MAIN heartbeat empty response");
        }
        if out_tokens > 0 || !assistant_text.trim().is_empty() {
            let _ = store_token_totals(token_total_path, token_totals);
        }
        if !tool_echo_msgs.is_empty() {
            for msg in tool_echo_msgs {
                core.push_tool(msg);
            }
            jump_to_bottom(scroll, follow_bottom);
        }
    }
    {
        let has_thinking = !thinking_full_text.trim().is_empty();
        let thinking_secs = thinking_started_at
            .map(|t| t.elapsed().as_secs())
            .unwrap_or(0);
        let thinking_chars = thinking_full_text.chars().count();
        let summary = build_thinking_summary(kind, thinking_secs);
        // 统一规则：
        // - SSE 流式：只在 running 阶段显示思考；一旦正文开始/思考结束就隐藏
        // - 非流式：默认不显示思考；仅在详情模式回看（Tab 切换）
        if has_thinking && !is_heartbeat {
            let msg = build_thinking_tool_message(
                &summary,
                thinking_full_text,
                thinking_secs,
                thinking_chars,
                kind,
            );
            if let Some(idx) = *thinking_idx {
                if let Some(entry) = core.history.get_mut(idx) {
                    entry.text = msg.trim_end().to_string();
                    render_cache.invalidate(idx);
                }
            } else {
                core.push_tool(msg.trim_end().to_string());
            }
            log_memos(meta, context_usage, "tool", None, msg.trim());
        } else if let Some(idx) = *thinking_idx {
            // 没有思考内容：把占位标记为 done（详情模式也不刷屏）。
            if let Some(entry) = core.history.get_mut(idx) {
                entry.text = build_thinking_tool_message_stub(kind, "0", &summary);
                render_cache.invalidate(idx);
            }
        }

        // 思考结束即隐藏：不做 5 秒停留与折叠动画。
        *thinking_idx = None;
        thinking_full_text.clear();
        *thinking_started_at = None;
    }
    *active_request_id = None;
    *active_cancel = None;
    *active_request_in_tokens = None;
    streaming_state.reset();
    if !keep_busy && !matches!(*mode, Mode::ApprovingTool | Mode::ExecutingTool) {
        *mode = Mode::Idle;
    }
    if let Some(target) = auto_context_compact
        && !is_heartbeat
        && pending_tools.is_empty()
        && pending_tool_confirm.is_none()
        && active_tool_stream.is_none()
        && matches!(*mode, Mode::Idle)
    {
        let begin_ok = match target {
            MindKind::Main => main_state.begin_context_compact(),
            MindKind::Sub => dog_state.begin_context_compact(),
        };
        if begin_ok {
            // 刷新 fastmemo（尽量保证压缩请求包含最新摘要）。
            let fastmemo = read_fastmemo_for_context();
            match target {
                MindKind::Main => main_state.refresh_fastmemo_system(&fastmemo),
                MindKind::Sub => dog_state.refresh_fastmemo_system(&fastmemo),
            }
            push_sys_log(sys_log, sys_log_limit, "↻ Context Summary 压缩中");
            let extra_system = Some(render_context_compact_prompt(
                context_compact_prompt_text,
                target,
                sys_cfg,
                "(自动压缩，无新用户消息)",
            ));
            let in_tokens = if matches!(target, MindKind::Main) {
                try_start_main_generation(TryStartMainGenerationArgs {
                    main_client,
                    main_state,
                    extra_system,
                    tx,
                    config,
                    mode,
                    active_kind,
                    sending_until,
                    sys_log,
                    sys_log_limit,
                    streaming_state,
                    request_seq,
                    active_request_id,
                    active_cancel,
                    sse_enabled,
                })
            } else {
                try_start_dog_generation(TryStartDogGenerationArgs {
                    kind: MindKind::Sub,
                    dog_client,
                    dog_state,
                    extra_system,
                    tx,
                    config,
                    mode,
                    active_kind,
                    sending_until,
                    sys_log,
                    sys_log_limit,
                    streaming_state,
                    request_seq,
                    active_request_id,
                    active_cancel,
                    sse_enabled,
                })
            };
            if let Some(in_tokens) = in_tokens {
                *active_request_in_tokens = Some(in_tokens);
                // 聊天区：显示 CONTEXT SUMMARY 系统提示（直到压缩完成再更新为“已更新”）。
                let note = "↻ CONTEXT SUMMARY";
                push_system_and_log(core, meta, context_usage, None, note);
                *ctx_compact_notice_idx = Some(core.history.len().saturating_sub(1));
            } else {
                // API 未就绪时不要卡在 inflight，回退为 pending，等待后续再压缩。
                match target {
                    MindKind::Main => main_state.abort_context_compact(),
                    MindKind::Sub => dog_state.abort_context_compact(),
                }
            }
        }
    }
    let fastmemo_retry_ready = fastmemo_compact_retry_at
        .as_ref()
        .map(|t| Instant::now() >= *t)
        .unwrap_or(true);
    if *auto_fastmemo_compact
        && fastmemo_retry_ready
        && !*fastmemo_compact_inflight
        && !is_heartbeat
        && pending_tools.is_empty()
        && pending_tool_confirm.is_none()
        && active_tool_stream.is_none()
        && matches!(*mode, Mode::Idle)
    {
        if dog_client.is_some() {
            // 刷新 fastmemo（尽量保证 DOG 在压缩前看到最新内容）。
            let fastmemo = read_fastmemo_for_context();
            dog_state.refresh_fastmemo_system(&fastmemo);
            push_sys_log(sys_log, sys_log_limit, "fastmemo 自动压缩中");
            *active_request_is_fastmemo_compact = true;
            let extra_system = Some(fastmemo_compact_prompt_text.to_string());
            if let Some(in_tokens) = try_start_dog_generation(TryStartDogGenerationArgs {
                kind: MindKind::Sub,
                dog_client,
                dog_state,
                extra_system,
                tx,
                config,
                mode,
                active_kind,
                sending_until,
                sys_log,
                sys_log_limit,
                streaming_state,
                request_seq,
                active_request_id,
                active_cancel,
                sse_enabled,
            }) {
                record_request_in_tokens(
                    core,
                    token_totals,
                    token_total_path,
                    context_usage,
                    active_request_in_tokens,
                    in_tokens,
                );
                *fastmemo_compact_inflight = true;
                *fastmemo_compact_edit_mask = 0;
                *fastmemo_compact_retry_at = None;
            }
        } else {
            push_sys_log(sys_log, sys_log_limit, "DOG: API 未就绪，fastmemo 压缩暂停");
        }
    }
    jump_to_bottom(scroll, follow_bottom);
}

fn drain_async_events(args: DrainAsyncEventsArgs<'_>) {
	    let DrainAsyncEventsArgs {
        core,
        rx,
        render_cache,
        pty,
        pty_view,
        pty_done,
        pty_exit_requested,
        mode,
        scroll,
        follow_bottom,
        active_kind,
	        reveal_idx,
	        reveal_len,
	        expanded_tool_idxs,
	        active_tool_stream,
        dog_state,
        dog_client,
        main_state,
        main_client,
        mind_context,
        _mind_ctx_idx_main,
        _mind_ctx_idx_dog,
        sending_until,
        token_totals,
        token_total_path,
        meta,
        context_usage,
        diary_state,
        ctx_compact_notice_idx,
        mind_rate_window,
        pending_mind_half,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
	        thinking_pending_idle,
	        thinking_started_at,
	        expanded_thinking_idxs,
	        selected_msg_idx,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        active_request_is_mind,
        active_request_is_fastmemo_compact,
        streaming_state,
        mind_pulse,
        retry_status,
        sys_log,
        sys_log_limit,
        config,
        context_compact_prompt_text,
        fastmemo_compact_prompt_text,
        auto_fastmemo_compact,
        fastmemo_compact_inflight,
        fastmemo_compact_edit_mask,
        fastmemo_compact_retry_at,
        sys_cfg,
        heartbeat_minutes_cache,
        next_heartbeat_at,
        heartbeat_request_id,
        response_count,
        _pulse_notice,
        tx,
        pending_tools,
        pending_tool_confirm,
        request_seq,
        active_request_id,
        active_cancel,
        active_request_in_tokens,
        sse_enabled,
        run_log_path,
    } = args;
    while let Ok(ev) = rx.try_recv() {
        if let Some(request_id) = event_request_id(&ev)
            && should_ignore_request(active_request_id, request_id)
        {
            continue;
        }
	        match ev {
	            AsyncEvent::ModelStreamStart { kind, request_id } => {
	                handle_model_stream_start(ModelStreamStartArgs {
	                    core,
	                    kind,
	                    request_id,
	                    heartbeat_request_id,
	                    sse_enabled,
	                    retry_status,
	                    active_tool_stream,
	                    active_kind,
	                    thinking_idx,
	                    streaming_state,
                    tool_preview,
                    tool_preview_active,
                    tool_preview_pending_idle,
                    tool_preview_chat_idx,
                    thinking_text,
                    thinking_full_text,
                    thinking_started_at,
                    thinking_in_progress,
                    thinking_pending_idle,
                    mode,
                    reveal_idx,
                    reveal_len,
                });
            }
            AsyncEvent::ModelStreamChunk {
                content,
                reasoning,
                request_id: _,
            } => {
	                handle_model_stream_chunk(
	                    ModelStreamChunkArgs {
	                        core,
	                        render_cache,
	                        owner: *active_kind,
	                        streaming_state,
	                        thinking_text,
	                        thinking_full_text,
	                        thinking_idx,
	                        thinking_in_progress,
	                        thinking_pending_idle,
	                        thinking_started_at,
	                        tool_preview,
	                        tool_preview_active,
	                        tool_preview_pending_idle,
	                        tool_preview_chat_idx,
	                        sse_enabled,
	                    },
                    &content,
                    &reasoning,
                );
            }
            AsyncEvent::ModelStreamEnd {
                kind,
                usage,
                error,
                request_id,
	            } => {
	                handle_model_stream_end(
	                    ModelStreamEndArgs {
	                        core,
	                        render_cache,
	                        mode,
	                        scroll,
	                        follow_bottom,
	                        active_kind,
	                        reveal_idx,
	                        expanded_tool_idxs,
	                        active_tool_stream,
                        dog_state,
                        dog_client,
                        main_state,
                        main_client,
                        sending_until,
                        token_totals,
                        token_total_path,
                        meta,
                        context_usage,
                        diary_state,
                        ctx_compact_notice_idx,
                        thinking_text,
                        thinking_full_text,
                        thinking_idx,
		                        thinking_in_progress,
		                        thinking_pending_idle,
		                        thinking_started_at,
		                        expanded_thinking_idxs,
		                        selected_msg_idx,
	                        tool_preview,
                        tool_preview_active,
                        tool_preview_pending_idle,
                        tool_preview_chat_idx,
                        active_request_is_mind,
                        active_request_is_fastmemo_compact,
                        streaming_state,
                        retry_status,
                        sys_log,
                        sys_log_limit,
                        config,
                        context_compact_prompt_text,
                        fastmemo_compact_prompt_text,
                        auto_fastmemo_compact,
                        fastmemo_compact_inflight,
                        fastmemo_compact_edit_mask,
                        fastmemo_compact_retry_at,
                        sys_cfg,
                        heartbeat_request_id,
                        response_count,
                        tx,
                        pending_tools,
                        pending_tool_confirm,
                        request_seq,
                        active_request_id,
                        active_cancel,
                        active_request_in_tokens,
                        sse_enabled,
                        run_log_path,
                    },
                    kind,
                    usage,
                    error,
                    request_id,
                );
            }
            AsyncEvent::ToolStreamStart {
                owner,
                call,
                sys_msg,
            } => {
	                handle_tool_stream_start(ToolStreamStartArgs {
	                    core,
	                    render_cache,
	                    expanded_tool_idxs,
	                    active_tool_stream,
	                    tool_preview_chat_idx,
	                    mind_pulse,
                    sys_log,
                    sys_log_limit,
                    thinking_in_progress,
                    thinking_pending_idle,
                    thinking_idx,
                    mode,
                    scroll,
                    follow_bottom,
                    owner,
                    call,
                    sys_msg,
                });
            }
            AsyncEvent::PtyReady {
                ctrl_tx,
                cols,
                rows,
                saved_path,
            } => {
                let cols = cols.max(1);
                let rows = rows.max(1);
                *pty = Some(PtyUiState::new(ctrl_tx, cols, rows, saved_path));
                *pty_view = true;
                *pty_done = false;
                *pty_exit_requested = false;
            }
            AsyncEvent::PtyOutput { bytes } => {
                if let Some(state) = pty.as_mut() {
                    state.process_output(&bytes);
                }
            }
            AsyncEvent::ToolStreamEnd {
                mut outcome,
                sys_msg,
            } => {
                // 交互 PTY：工具结束时把“终端屏幕快照”写回工具 output（避免只剩一条 saved path 让 AI 丢上下文）。
                // 同时收尾 PTY 状态，回到正常聊天视图。
                let is_interactive_bash = active_tool_stream
                    .as_ref()
                    .is_some_and(|s| s.call.tool == "bash" && s.call.interactive.unwrap_or(false));
                if is_interactive_bash {
                    if let Some(state) = pty.as_mut() {
                        let mut parts: Vec<String> = Vec::new();

                        let snap_full = state
                            .last_user_snapshot
                            .clone()
                            .unwrap_or_else(|| state.snapshot_plain());
                        let snap_chars = snap_full.chars().count();
                        if !snap_full.trim().is_empty() {
                            let snap = if snap_chars > PTY_RETURN_SCREEN_MAX_CHARS {
                                truncate_tail(&snap_full, PTY_RETURN_SCREEN_MAX_CHARS)
                            } else {
                                snap_full.clone()
                            };
                            let label = if state.last_user_snapshot.is_some() {
                                "snapshot"
                            } else {
                                "screen"
                            };
                            parts.push(format!("[{label}]\n{snap}"));
                        }

                        // 若屏幕内容被截断，补充一段 raw tail（可能含 ANSI 控制序列）。
                        if snap_chars > PTY_RETURN_SCREEN_MAX_CHARS {
                            if let Some(raw) = read_tail_text(&state.saved_path, PTY_RETURN_RAW_TAIL_BYTES) {
                                let raw = truncate_tail(&raw, PTY_RETURN_RAW_TAIL_MAX_CHARS);
                                parts.push(format!("[raw_tail]\n{raw}"));
                            }
                        }

                        let stdin_full = state.input_log.trim_end_matches('\n').to_string();
                        if !stdin_full.trim().is_empty() {
                            let stdin = if stdin_full.chars().count() > PTY_RETURN_STDIN_MAX_CHARS {
                                truncate_tail(&stdin_full, PTY_RETURN_STDIN_MAX_CHARS)
                            } else {
                                stdin_full
                            };
                            parts.push(format!("[stdin]\n{stdin}"));
                        }

                        outcome.user_message = parts.join("\n\n").trim_end().to_string();
                        outcome
                            .log_lines
                            .push(format!("saved:{}", state.saved_path));
                    }
                    if *pty_exit_requested {
                        // 用户主动中断：结束后直接清理。
                        *pty_view = false;
                        *pty = None;
                        *pty_done = false;
                        *pty_exit_requested = false;
                    } else {
                        // 自然结束：保留终端内容给用户确认（Esc 关闭）。
                        *pty_done = true;
                    }
                }
                let active_call_log = active_tool_stream
                    .as_ref()
                    .map(|s| tool_call_log_fields(&s.call));
                let active_owner_log = active_tool_stream.as_ref().map(|s| mind_label(s.owner));
                runlog_event(
                    "INFO",
                    "tool.stream.end",
                    json!({
                        "owner": active_owner_log,
                        "call": active_call_log,
                        "outcome_user_message": &outcome.user_message,
                        "outcome_log_lines": &outcome.log_lines,
                        "sys_msg": sys_msg.as_deref(),
                    }),
                );
                if let Some(msg) = sys_msg {
                    push_sys_log(sys_log, sys_log_limit, msg);
                }
                let mut tool_message = None;
                let mut tool_message_raw = None;
                let mut saw_diary_add = false;
                let mut reload_sys_cfg = false;
                let mut skip_owner_resume = false;
                let mut skip_tool_context = false;
                let mut mind_transfer: Option<(MindKind, MindKind, String, String)> = None;
                let mut mind_pulse_dir: Option<PulseDir> = None;
                if let Some(state) = active_tool_stream.as_mut() {
                    if matches!(diary_state.stage, DiaryStage::WaitingMain)
                        && is_datememo_add(&state.call)
                    {
                        saw_diary_add = true;
                    }
                    if state.call.tool == "system_config" {
                        reload_sys_cfg = true;
                    }
                    if state.call.tool == "context_compact" {
                        skip_tool_context = true;
                        handle_context_compact_tool(ContextCompactToolArgs {
                            state,
                            outcome: &mut outcome,
                            main_state,
                            dog_state,
                            sys_cfg,
                            sys_log,
                            sys_log_limit,
                            core,
                            render_cache,
                            ctx_compact_notice_idx,
                        });
                        if main_state.fastmemo_compact_pending || dog_state.fastmemo_compact_pending
                        {
                            *auto_fastmemo_compact = true;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo: 已达阈值，待压缩");
                        }
                    }
                    if state.call.tool == "mind_msg" {
                        skip_owner_resume = true;
                        skip_tool_context = true;
                        if let Some((from, target, brief, content)) =
                            handle_mind_msg_tool(MindMsgToolArgs {
                                state,
                                pending_mind_half,
                                main_state,
                                dog_state,
                                sys_cfg,
                                config,
                                context_usage,
                            })
                        {
                            mind_transfer = Some((from, target, brief, content));
                            mind_pulse_dir = Some(pulse_dir(from, target));
                        }
                        if main_state.fastmemo_compact_pending || dog_state.fastmemo_compact_pending
                        {
                            *auto_fastmemo_compact = true;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo: 已达阈值，待压缩");
                        }
                    }
                    if state.call.tool == "memory_add" && matches!(state.owner, MindKind::Main) {
                        let raw = state
                            .call
                            .path
                            .as_deref()
                            .unwrap_or(state.call.input.trim())
                            .to_ascii_lowercase();
                        let is_fastmemo = raw.contains("fastmemo");
                        let pending = outcome
                            .log_lines
                            .iter()
                            .any(|l| l.trim() == "fastmemo_compact_pending:1");
                        if is_fastmemo && pending {
                            *auto_fastmemo_compact = true;
                            main_state.fastmemo_compact_pending = true;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo: 已达阈值，待压缩");
                        }
                    }
                    if state.call.tool == "memory_edit"
                        && matches!(state.owner, MindKind::Sub)
                        && *fastmemo_compact_inflight
                    {
                        let raw = state
                            .call
                            .path
                            .as_deref()
                            .unwrap_or(state.call.input.trim())
                            .to_ascii_lowercase();
                        if raw.contains("fastmemo") {
                            if let Some(mut sec) = state.call.section.as_deref().map(str::trim) {
                                if sec.starts_with('[') && sec.contains(']') {
                                    if let Some(end) = sec.find(']') {
                                        sec = &sec[1..end];
                                    }
                                }
                                let bit = match sec {
                                    "自我感知" => Some(1u8 << 0),
                                    "用户感知" => Some(1u8 << 1),
                                    "环境感知" => Some(1u8 << 2),
                                    "历史感知" => Some(1u8 << 3),
                                    "动态context池" => Some(1u8 << 4),
                                    _ => None,
                                };
                                if let Some(b) = bit {
                                    *fastmemo_compact_edit_mask |= b;
                                }
                            }
                        }
                    }
                    tool_message_raw = Some(format_tool_message_raw(&state.call, &outcome));
                    if outcome.user_message.contains("工具执行失败")
                        || outcome
                            .log_lines
                            .iter()
                            .any(|l| l.contains("状态:timeout") || l.contains("超时"))
                    {
                        append_run_log(
                            run_log_path,
                            "ERROR",
                            &format!("tool {} failed: {}", state.call.tool, outcome.user_message),
                        );
                    }
                    let tail = outcome.user_message.trim_end();
                    if !tail.is_empty() {
                        push_tool_stream_chunk(state, tail);
                    }
                    if !outcome.log_lines.is_empty() {
                        state.meta = outcome.log_lines.clone();
                    }
                    let msg = build_tool_stream_message(state);
                    update_history_text_at(core, render_cache, state.idx, &msg);
                    log_memos(meta, context_usage, "tool", None, &msg);
                    tool_message = Some(msg);
                }
                let owner = active_tool_stream
                    .as_ref()
                    .map(|s| s.owner)
                    .unwrap_or(*active_kind);
                *active_tool_stream = None;
                let mut mind_request: Option<(MindKind, String, String)> = None;
                if let Some((from, target, brief, content)) = mind_transfer.take() {
                    // 聊天区可视化：○ Dog ➠ Main：brief / ● Main ➠ Dog：brief
                    // 下一行用 ↳ 承载内容（箭头与内容斜体由 UI 负责）。
                    let from_key = if matches!(from, MindKind::Sub) {
                        "dog"
                    } else {
                        "main"
                    };
                    let to_key = if matches!(target, MindKind::Sub) {
                        "dog"
                    } else {
                        "main"
                    };
                    core.history.push(Message {
                        role: Role::Assistant,
                        text: format!(
                            "[mind_msg]\nfrom:{from_key}\nto:{to_key}\nbrief:{brief}\ncontent:\n{content}"
                        ),
                        mind: Some(from),
                    });
                    let now = Instant::now();
                    trim_mind_rate_window(mind_rate_window, now);
                    if mind_rate_window.len() >= MIND_RATE_MAX_HALF_TURNS {
                        let note = "协同沟通暂停：10分钟内已达10轮，本次消息未发送。";
                        push_system_and_log(core, meta, context_usage, Some("mind"), note);
                        push_sys_log(sys_log, sys_log_limit, note);
                    } else {
                        mind_rate_window.push_back(now);
                        let half_turns = mind_rate_window.len();
                        let mind_system = build_mind_system_prompt(mind_context, half_turns);
                        let mind_user = build_mind_user_message(from, &brief, &content);
                        mind_context.push(from, target, &content);
                        log_memos(meta, context_usage, "system", Some("mind"), &mind_user);
                        mind_request = Some((target, mind_system, mind_user));
                    }
                }
                if let Some(dir) = mind_pulse_dir {
                    *mind_pulse = Some(MindPulse {
                        dir,
                        until: Instant::now() + Duration::from_millis(1200),
                    });
                }
                if matches!(*mode, Mode::ExecutingTool) {
                    *mode = Mode::Idle;
                }
                jump_to_bottom(scroll, follow_bottom);
                if saw_diary_add && matches!(diary_state.stage, DiaryStage::WaitingMain) {
                    diary_state.stage = DiaryStage::Idle;
                    context_usage.reset();
                    clear_contextmemo(&config.contextmemo_path);
                    push_sys_log(sys_log, sys_log_limit, "日记压缩完成");
                }
                if !skip_tool_context
                    && let Some(msg) = tool_message_raw.as_ref().or(tool_message.as_ref())
                {
                    match owner {
                        MindKind::Sub => dog_state.push_tool(msg),
                        MindKind::Main => main_state.push_tool(msg),
                    }
                }
                if reload_sys_cfg {
                    let (new_cfg, err, _) = load_system_config();
                    if let Some(e) = err {
                        push_sys_log(sys_log, sys_log_limit, format!("SYS: 配置刷新失败 {e}"));
                    } else {
                        *sys_cfg = new_cfg;
                        *heartbeat_minutes_cache = sys_cfg.heartbeat_minutes;
                        *next_heartbeat_at =
                            Instant::now() + heartbeat_interval(*heartbeat_minutes_cache);
                    }
                }
                let has_next = try_start_next_tool(TryStartNextToolArgs {
                    pending_tools,
                    pending_tool_confirm,
                    tx: tx.clone(),
                    core,
                    meta,
                    context_usage,
                    mode,
                    sys_log,
                    sys_log_limit,
                    sys_cfg,
                    owner,
                });
                if !has_next && !skip_owner_resume && matches!(owner, MindKind::Sub) {
                    // fastmemo 自动压缩：工具链结束后不再让 DOG 输出正文，保持“静默维护”。
                    if *fastmemo_compact_inflight {
                        let pending = std::fs::read_to_string(FASTMEMO_PATH)
                            .ok()
                            .map(|t| fastmemo_dynamic_count_and_any_ge10(&t).1)
                            .unwrap_or(false);
                        if !pending {
                            *fastmemo_compact_inflight = false;
                            *fastmemo_compact_edit_mask = 0;
                            *fastmemo_compact_retry_at = None;
                            *auto_fastmemo_compact = false;
                            *active_request_is_fastmemo_compact = false;
                            main_state.fastmemo_compact_pending = false;
                            dog_state.fastmemo_compact_pending = false;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo 自动压缩完成");
                        } else {
                            // 仍超阈值：认为本轮压缩未生效（或未覆盖全部需要压缩的区），稍后自动重试。
                            *fastmemo_compact_inflight = false;
                            *fastmemo_compact_edit_mask = 0;
                            *active_request_is_fastmemo_compact = false;
                            *auto_fastmemo_compact = true;
                            main_state.fastmemo_compact_pending = true;
                            dog_state.fastmemo_compact_pending = true;
                            *fastmemo_compact_retry_at =
                                Some(Instant::now() + Duration::from_secs(12));
                            push_sys_log(sys_log, sys_log_limit, "fastmemo 仍超阈值：稍后重试");
                        }
                    } else {
                        let extra_system = if *fastmemo_compact_inflight {
                            *active_request_is_fastmemo_compact = true;
                            Some(fastmemo_compact_prompt_text.to_string())
                        } else {
                            None
                        };
                        if let Some(in_tokens) =
                            try_start_dog_generation(TryStartDogGenerationArgs {
                                kind: MindKind::Sub,
                                dog_client,
                                dog_state,
                                extra_system,
                                tx,
                                config,
                                mode,
                                active_kind,
                                sending_until,
                                sys_log,
                                sys_log_limit,
                                streaming_state,
                                request_seq,
                                active_request_id,
                                active_cancel,
                                sse_enabled,
                            })
                        {
                            record_request_in_tokens(
                                core,
                                token_totals,
                                token_total_path,
                                context_usage,
                                active_request_in_tokens,
                                in_tokens,
                            );
                        } else {
                            push_sys_log(sys_log, sys_log_limit, "DOG: API 未就绪，工具链暂停");
                            push_system_and_log(
                                core,
                                meta,
                                context_usage,
                                Some("dog"),
                                "DOG: API 未就绪，工具链暂停",
                            );
                        }
                    }
                } else if !has_next
                    && !skip_owner_resume
                    && matches!(owner, MindKind::Main)
                    && !saw_diary_add
                {
                    if let Some(in_tokens) = try_start_main_generation(TryStartMainGenerationArgs {
                        main_client,
                        main_state,
                        extra_system: None,
                        tx,
                        config,
                        mode,
                        active_kind,
                        sending_until,
                        sys_log,
                        sys_log_limit,
                        streaming_state,
                        request_seq,
                        active_request_id,
                        active_cancel,
                        sse_enabled,
                    }) {
                        record_request_in_tokens(
                            core,
                            token_totals,
                            token_total_path,
                            context_usage,
                            active_request_in_tokens,
                            in_tokens,
                        );
                    } else {
                        push_sys_log(sys_log, sys_log_limit, "MAIN: API 未就绪，工具链暂停");
                        push_system_and_log(
                            core,
                            meta,
                            context_usage,
                            Some("main"),
                            "MAIN: API 未就绪，工具链暂停",
                        );
                    }
                }
                if !has_next
                    && skip_owner_resume
                    && mind_request.is_some()
                    && matches!(*mode, Mode::Idle)
                    && pending_tool_confirm.is_none()
                    && pending_tools.is_empty()
                    && active_tool_stream.is_none()
                {
                    if let Some((target, mind_system, mind_user)) = mind_request.take() {
                        *active_request_is_mind = true;
                        let client = if matches!(target, MindKind::Main) {
                            main_client
                        } else {
                            dog_client
                        };
                        let state = if matches!(target, MindKind::Main) {
                            &*main_state
                        } else {
                            &*dog_state
                        };
                        let started = try_start_mind_generation(TryStartMindGenerationArgs {
                            target,
                            client,
                            state,
                            mind_system: &mind_system,
                            mind_user: &mind_user,
                            tx,
                            config,
                            mode,
                            active_kind,
                            sending_until,
                            sys_log,
                            sys_log_limit,
                            streaming_state,
                            request_seq,
                            active_request_id,
                            active_cancel,
                            sse_enabled,
                        });
                        if let Some(in_tokens) = started {
                            record_request_in_tokens(
                                core,
                                token_totals,
                                token_total_path,
                                context_usage,
                                active_request_in_tokens,
                                in_tokens,
                            );
                        } else {
                            *active_request_is_mind = false;
                            let label = if matches!(target, MindKind::Main) {
                                "MAIN"
                            } else {
                                "DOG"
                            };
                            push_sys_log(
                                sys_log,
                                sys_log_limit,
                                format!("{label}: API 未就绪，沟通暂停"),
                            );
                            let msg = format!("{label}: API 未就绪，沟通暂停");
                            push_system_and_log(core, meta, context_usage, Some("mind"), &msg);
                        }
                    }
                }
            }
            AsyncEvent::ErrorRetry {
                attempt,
                max,
                request_id: _,
            } => {
                handle_error_retry(retry_status, attempt, max);
            }
        }
    }

	    prune_history_if_needed(PruneHistoryArgs {
	        core,
	        render_cache,
	        config,
	        reveal_idx,
	        expanded_tool_idxs,
	        thinking_idx,
	        expanded_thinking_idxs,
	        selected_msg_idx,
	        tool_preview_chat_idx,
	        ctx_compact_notice_idx,
	        streaming_state,
	        active_tool_stream,
	    });
}

fn adjust_index(idx: &mut Option<usize>, removed: usize) {
    if let Some(value) = *idx {
        if value < removed {
            *idx = None;
        } else {
            *idx = Some(value.saturating_sub(removed));
        }
    }
}

fn adjust_index_set(set: &mut BTreeSet<usize>, removed: usize) {
    if removed == 0 || set.is_empty() {
        return;
    }
    let mut next: BTreeSet<usize> = BTreeSet::new();
    for v in set.iter().copied() {
        if v < removed {
            continue;
        }
        next.insert(v.saturating_sub(removed));
    }
    *set = next;
}

struct PruneHistoryArgs<'a> {
    core: &'a mut Core,
    render_cache: &'a mut ui::ChatRenderCache,
    config: &'a AppConfig,
    reveal_idx: &'a mut Option<usize>,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    thinking_idx: &'a mut Option<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview_chat_idx: &'a mut Option<usize>,
    ctx_compact_notice_idx: &'a mut Option<usize>,
    streaming_state: &'a mut StreamingState,
    active_tool_stream: &'a mut Option<ToolStreamState>,
}

fn prune_history_if_needed(args: PruneHistoryArgs<'_>) {
    let PruneHistoryArgs {
        core,
        render_cache,
        config,
        reveal_idx,
        expanded_tool_idxs,
        thinking_idx,
        expanded_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
        ctx_compact_notice_idx,
        streaming_state,
        active_tool_stream,
    } = args;
    let removed = core.prune_history(config.max_history_messages, config.max_history_bytes);
    if removed == 0 {
        return;
    }
    *render_cache = ui::ChatRenderCache::new();
    adjust_index(reveal_idx, removed);
    adjust_index_set(expanded_tool_idxs, removed);
    adjust_index(thinking_idx, removed);
    adjust_index_set(expanded_thinking_idxs, removed);
    adjust_index(selected_msg_idx, removed);
    adjust_index(tool_preview_chat_idx, removed);
    adjust_index(ctx_compact_notice_idx, removed);
    streaming_state.adjust_index(removed);
    if let Some(state) = active_tool_stream.as_mut() {
        if state.idx < removed {
            *active_tool_stream = None;
        } else {
            state.idx = state.idx.saturating_sub(removed);
        }
    }
}

fn push_sys_log(sys_log: &mut VecDeque<String>, limit: usize, msg: impl Into<String>) {
    fn cuteify_sys_log_message(raw: &str) -> String {
        let mut text = raw.trim().replace('\n', " ");
        if text.is_empty() {
            return text;
        }
        if text == "等待工具确认" {
            text = "等你确认工具请求哦".to_string();
        } else if text == "日记压缩完成" {
            text = "日记写好啦".to_string();
        } else if text == "心跳已发送" {
            text = "心跳已发送啦".to_string();
        }
        if text.starts_with("♡") || text.starts_with("♥") || text.starts_with("✿") {
            return text;
        }
        let is_sad = text.contains("失败")
            || text.contains("无效")
            || text.contains("未就绪")
            || text.contains("启用失败")
            || text.contains("读取失败")
            || text.contains("缺失")
            || text.contains("取消")
            || text.contains("暂停")
            || text.contains("ERROR")
            || text.contains("WARN");
        let prefix = if is_sad { "✿ " } else { "♡ " };
        format!("{prefix}{text}")
    }

    let raw = msg.into();
    let mut text = cuteify_sys_log_message(&raw);
    if text.len() > 180 {
        let mut end = 180;
        while end > 0 && !text.is_char_boundary(end) {
            end = end.saturating_sub(1);
        }
        text.truncate(end);
        text.push_str("...");
    }
    if sys_log.len() >= limit {
        sys_log.pop_front();
    }
    sys_log.push_back(text);
}

struct HandleCommandArgs<'a> {
    core: &'a mut Core,
    raw: &'a str,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    chat_target: &'a mut MindKind,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    screen: &'a mut Screen,
    should_exit: &'a mut bool,
    sse_enabled: &'a mut bool,
    enter_cmd_shell: &'a mut bool,
    exit_code: &'a mut i32,
    sys_cfg: &'a mut SystemConfig,
    sys_cfg_path: &'a Path,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
}

fn handle_command(args: HandleCommandArgs<'_>) -> anyhow::Result<bool> {
    let HandleCommandArgs {
        core,
        raw,
        meta,
        context_usage,
        mode,
        active_kind,
        chat_target,
        dog_state,
        main_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        screen,
        should_exit,
        sse_enabled,
        enter_cmd_shell,
        exit_code,
        sys_cfg,
        sys_cfg_path,
        sys_log,
        sys_log_limit,
    } = args;

    let s = raw.trim();
    if !s.starts_with('/') || s.contains('\n') {
        return Ok(false);
    }
    let mut parts = s.splitn(2, char::is_whitespace);
    let cmd = parts.next().unwrap_or("").to_ascii_lowercase();
    let arg = parts.next().unwrap_or("").trim().to_ascii_lowercase();

    match cmd.as_str() {
        "/quit" | "/q" => {
            *should_exit = true;
            // 仅退出 AItermux，返回到当前 shell/session。
            // Termux 的 session 是否退出由启动器（~/.zshrc）按 exit code 决定；这里默认不触发“结束 session”。
            *exit_code = 0;
        }
        "/cmd" => {
            *enter_cmd_shell = true;
        }
        "/settings" => {
            *screen = Screen::Settings;
        }
        "/turn" | "/trun" | "/switch" => {
            let target = match arg.as_str() {
                "main" => MindKind::Main,
                "dog" | "memory" => MindKind::Sub,
                _ => {
                    push_system_and_log(
                        core,
                        meta,
                        context_usage,
                        None,
                        "用法：/Switch Main 或 /Switch Memory",
                    );
                    return Ok(true);
                }
            };
            *chat_target = target;
            sys_cfg.chat_target = if matches!(target, MindKind::Main) {
                "main".to_string()
            } else {
                "dog".to_string()
            };
            let _ = store_config_file(sys_cfg_path, sys_cfg);
            reset_state_for_target(
                target,
                dog_state,
                main_state,
                mind_ctx_idx_main,
                mind_ctx_idx_dog,
            );
            if matches!(*mode, Mode::Idle) {
                *active_kind = target;
            }
            let msg = build_switch_prompt(target);
            push_system_and_log(core, meta, context_usage, None, &msg);
            push_sys_log(sys_log, sys_log_limit, msg);
        }
        "/sse" => match arg.as_str() {
            "open" => {
                *sse_enabled = true;
                sys_cfg.sse_enabled = true;
                let _ = store_config_file(sys_cfg_path, sys_cfg);
            }
            "close" => {
                *sse_enabled = false;
                sys_cfg.sse_enabled = false;
                let _ = store_config_file(sys_cfg_path, sys_cfg);
            }
            _ => {
                push_system_and_log(
                    core,
                    meta,
                    context_usage,
                    None,
                    "用法：/SSE Open 或 /SSE Close",
                );
            }
        },
        _ => {
            const UNKNOWN: &str =
                "未知命令：/Cmd /Settings /Switch Main /Switch Memory /SSE Open /SSE Close /Quit";
            push_system_and_log(core, meta, context_usage, None, UNKNOWN);
        }
    }

    *mode = Mode::Idle;
    Ok(true)
}

fn spawn_dog_request(
    client: DogClient,
    messages: Vec<ApiMessage>,
    tx: mpsc::Sender<AsyncEvent>,
    kind: MindKind,
    sse_enabled: bool,
    request_id: u64,
    cancel: Arc<AtomicBool>,
) {
    thread::spawn(move || {
        let result = if sse_enabled {
            client.send_chat_stream(messages, tx.clone(), kind, request_id, cancel.clone())
        } else {
            client.send_chat(messages, tx.clone(), kind, request_id, cancel.clone())
        };
        if let Err(e) = result {
            let _ = tx.send(AsyncEvent::ModelStreamEnd {
                kind,
                usage: 0,
                error: Some(format!("{e:#}")),
                request_id,
            });
        }
    });
}

fn begin_request(
    request_seq: &mut u64,
    active_request_id: &mut Option<u64>,
    active_cancel: &mut Option<Arc<AtomicBool>>,
) -> (u64, Arc<AtomicBool>) {
    *request_seq = request_seq.saturating_add(1).max(1);
    let request_id = *request_seq;
    let cancel = Arc::new(AtomicBool::new(false));
    *active_request_id = Some(request_id);
    *active_cancel = Some(cancel.clone());
    (request_id, cancel)
}

struct TryStartMainDiaryArgs<'a> {
    main_client: &'a Option<DogClient>,
    main_prompt: &'a str,
    system_prompt: &'a str,
    last_diary: &'a str,
    context_text: &'a str,
    tx: &'a mpsc::Sender<AsyncEvent>,
    config: &'a AppConfig,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    sse_enabled: bool,
}

fn try_start_main_diary(args: TryStartMainDiaryArgs<'_>) -> bool {
    let TryStartMainDiaryArgs {
        main_client,
        main_prompt,
        system_prompt,
        last_diary,
        context_text,
        tx,
        config,
        mode,
        active_kind,
        sending_until,
        sys_log,
        sys_log_limit,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        sse_enabled,
    } = args;

    let Some(client) = main_client.clone() else {
        return false;
    };
    *mode = Mode::Generating;
    *active_kind = MindKind::Main;
    let now = Instant::now();
    *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    push_sys_log(sys_log, sys_log_limit, "萤现在在写日记");
    streaming_state.reset();
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    let messages: Vec<ApiMessage> = vec![
        ApiMessage {
            role: "system".to_string(),
            content: build_main_diary_prompt(main_prompt, system_prompt, last_diary, context_text),
        },
        ApiMessage {
            role: "user".to_string(),
            content: "请开始写日记。".to_string(),
        },
    ];
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        MindKind::Main,
        sse_enabled,
        request_id,
        cancel,
    );
    true
}

struct TryStartMainGenerationArgs<'a> {
    main_client: &'a Option<DogClient>,
    main_state: &'a DogState,
    extra_system: Option<String>,
    tx: &'a mpsc::Sender<AsyncEvent>,
    config: &'a AppConfig,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    sse_enabled: bool,
}

fn try_start_main_generation(args: TryStartMainGenerationArgs<'_>) -> Option<u64> {
    let TryStartMainGenerationArgs {
        main_client,
        main_state,
        extra_system,
        tx,
        config,
        mode,
        active_kind,
        sending_until,
        sys_log,
        sys_log_limit,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        sse_enabled,
    } = args;

    let client = main_client.clone()?;
    *mode = Mode::Generating;
    *active_kind = MindKind::Main;
    let now = Instant::now();
    *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    push_sys_log(sys_log, sys_log_limit, "现在[萤]MAIN正在思考");
    streaming_state.reset();
    let messages = main_state.message_snapshot(extra_system.as_deref());
    let in_tokens = estimate_messages_in_tokens(&messages);
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        MindKind::Main,
        sse_enabled,
        request_id,
        cancel,
    );
    Some(in_tokens)
}

struct TryStartMindGenerationArgs<'a> {
    target: MindKind,
    client: &'a Option<DogClient>,
    state: &'a DogState,
    mind_system: &'a str,
    mind_user: &'a str,
    tx: &'a mpsc::Sender<AsyncEvent>,
    config: &'a AppConfig,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    sse_enabled: bool,
}

fn try_start_mind_generation(args: TryStartMindGenerationArgs<'_>) -> Option<u64> {
    let TryStartMindGenerationArgs {
        target,
        client,
        state,
        mind_system,
        mind_user,
        tx,
        config,
        mode,
        active_kind,
        sending_until,
        sys_log,
        sys_log_limit,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        sse_enabled,
    } = args;

    let client = client.clone()?;
    *mode = Mode::Generating;
    *active_kind = target;
    let now = Instant::now();
    *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    push_sys_log(sys_log, sys_log_limit, "协同沟通中");
    streaming_state.reset();
    let mut messages = state.messages.clone();
    let mind_system = mind_system.trim();
    if !mind_system.is_empty() {
        messages.push(ApiMessage {
            role: "system".to_string(),
            content: mind_system.to_string(),
        });
    }
    let mind_user = mind_user.trim();
    if !mind_user.is_empty() {
        messages.push(ApiMessage {
            role: "user".to_string(),
            content: mind_user.to_string(),
        });
    }
    let messages = normalize_messages_for_deepseek(&messages);
    let in_tokens = estimate_messages_in_tokens(&messages);
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        target,
        sse_enabled,
        request_id,
        cancel,
    );
    Some(in_tokens)
}

struct TryStartMainHeartbeatArgs<'a> {
    main_client: &'a Option<DogClient>,
    main_state: &'a DogState,
    tx: &'a mpsc::Sender<AsyncEvent>,
    config: &'a AppConfig,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    heartbeat_request_id: &'a mut Option<u64>,
    sse_enabled: bool,
}

fn try_start_main_heartbeat(args: TryStartMainHeartbeatArgs<'_>) -> Option<u64> {
    let TryStartMainHeartbeatArgs {
        main_client,
        main_state,
        tx,
        config,
        mode,
        active_kind,
        sending_until,
        sys_log,
        sys_log_limit,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        heartbeat_request_id,
        sse_enabled,
    } = args;

    let client = main_client.clone()?;
    *mode = Mode::Generating;
    *active_kind = MindKind::Main;
    let now = Instant::now();
    *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    push_sys_log(sys_log, sys_log_limit, "心跳已发送");
    streaming_state.reset();
    let messages = main_state.message_snapshot(None);
    let in_tokens = estimate_messages_in_tokens(&messages);
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    *heartbeat_request_id = Some(request_id);
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        MindKind::Main,
        sse_enabled,
        request_id,
        cancel,
    );
    Some(in_tokens)
}

struct TryStartDogGenerationArgs<'a> {
    kind: MindKind,
    dog_client: &'a Option<DogClient>,
    dog_state: &'a DogState,
    extra_system: Option<String>,
    tx: &'a mpsc::Sender<AsyncEvent>,
    config: &'a AppConfig,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    sse_enabled: bool,
}

fn try_start_dog_generation(args: TryStartDogGenerationArgs<'_>) -> Option<u64> {
    let TryStartDogGenerationArgs {
        kind,
        dog_client,
        dog_state,
        extra_system,
        tx,
        config,
        mode,
        active_kind,
        sending_until,
        sys_log,
        sys_log_limit,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        sse_enabled,
    } = args;

    let client = dog_client.clone()?;
    *mode = Mode::Generating;
    *active_kind = kind;
    let now = Instant::now();
    *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    push_sys_log(sys_log, sys_log_limit, "现在[萤]DOG正在思考");
    streaming_state.reset();
    let messages = dog_state.message_snapshot(extra_system.as_deref());
    let in_tokens = estimate_messages_in_tokens(&messages);
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        kind,
        sse_enabled,
        request_id,
        cancel,
    );
    Some(in_tokens)
}

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--selfcheck" || a == "selfcheck") {
        return run_selfcheck();
    }
    if args.iter().any(|a| a == "--bootstrap" || a == "bootstrap") {
        run_bootstrap_script(true)?;
        return Ok(());
    }
    if !args
        .iter()
        .any(|a| a == "--no-bootstrap" || a == "--skip-bootstrap")
    {
        if let Err(e) = run_bootstrap_script(false) {
            eprintln!("[ying] 依赖 bootstrap 失败：{e:#}");
        }
    }
    let code = run()?;
    std::process::exit(code);
}

fn find_repo_root() -> Option<PathBuf> {
    let mut dir = std::env::current_dir().ok()?;
    for _ in 0..10 {
        if dir.join("Cargo.toml").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

fn run_bootstrap_script(strict: bool) -> anyhow::Result<()> {
    let Some(root) = find_repo_root() else {
        if strict {
            anyhow::bail!("未找到项目根目录（上溯 10 层未发现 Cargo.toml）");
        }
        return Ok(());
    };
    let script = root.join("scripts/bootstrap.sh");
    if !script.is_file() {
        if strict {
            anyhow::bail!("未找到 bootstrap 脚本：{}", script.display());
        }
        return Ok(());
    }
    let status = Command::new("bash")
        .arg(&script)
        .current_dir(&root)
        .status()
        .with_context(|| format!("执行 bootstrap 失败：{}", script.display()))?;
    if status.success() {
        Ok(())
    } else {
        anyhow::bail!(
            "bootstrap 失败（exit={:?}）：{}",
            status.code(),
            script.display()
        );
    }
}

fn run_selfcheck() -> anyhow::Result<()> {
    if let Some(root) = find_repo_root() {
        let _ = std::env::set_current_dir(root);
    }
    println!("ying selfcheck: start");

    let calls = vec![
        ToolCall {
            tool: "list_dir".to_string(),
            path: Some(".".to_string()),
            brief: Some("selfcheck".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "stat_file".to_string(),
            path: Some("Cargo.toml".to_string()),
            brief: Some("selfcheck".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "read_file".to_string(),
            path: Some("Cargo.toml".to_string()),
            head: Some(true),
            max_lines: Some(40),
            brief: Some("selfcheck: head".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "read_file".to_string(),
            path: Some("src/main.rs".to_string()),
            tail: Some(true),
            max_lines: Some(40),
            brief: Some("selfcheck: tail".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "search".to_string(),
            pattern: Some("ToolCall".to_string()),
            root: Some("src".to_string()),
            count: Some(20),
            brief: Some("selfcheck: text search".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "search".to_string(),
            pattern: Some("Cargo".to_string()),
            root: Some(".".to_string()),
            file: Some(true),
            count: Some(20),
            brief: Some("selfcheck: file search".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "memory_read".to_string(),
            path: Some("fastmemo".to_string()),
            head: Some(true),
            max_lines: Some(60),
            brief: Some("selfcheck: memory fastmemo".to_string()),
            ..ToolCall::default()
        },
    ];

    let mut failed = 0usize;
    for call in calls {
        let label = call.tool.clone();
        let outcome = handle_tool_call_with_retry(&call, 2);
        let status_line = outcome
            .log_lines
            .first()
            .map(|s| s.as_str())
            .unwrap_or("(no meta)");
        let ok = !status_line.contains("timeout")
            && !status_line.contains("fail")
            && !outcome.user_message.contains("格式错误");
        if !ok {
            failed = failed.saturating_add(1);
        }
        println!(
            "[{}] ok={} | {}",
            label,
            if ok { "yes" } else { "no" },
            status_line
        );
    }

    if failed > 0 {
        anyhow::bail!("ying selfcheck: failed={failed}");
    }
    println!("ying selfcheck: ok");
    Ok(())
}

mod types {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Mode {
        Idle,
        Generating,
        ApprovingTool,
        ExecutingTool,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Screen {
        Chat,
        Settings,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum SettingsFocus {
        Tabs,
        Fields,
        Input,
        Prompt,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Role {
        User,
        Assistant,
        System,
        Tool,
    }

    pub const THINKING_MARKER: &str = "[thinking]";

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum MindKind {
        Main,
        Sub,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PulseDir {
        MainToDog,
        DogToMain,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ContextLine {
        pub sc_tokens: usize,
        pub sc_pct: u8,
        pub ctx_pct: u8,
        pub run_in_tokens: u64,
        pub run_out_tokens: u64,
        pub total_in_tokens: u64,
        pub total_out_tokens: u64,
        pub run_secs: u64,
        pub heartbeat_count: u64,
        pub response_count: u64,
    }

    #[derive(Debug, Clone)]
    pub struct Message {
        pub role: Role,
        pub text: String,
        pub mind: Option<MindKind>,
    }

    #[derive(Debug)]
    pub struct Core {
        pub history: Vec<Message>,
        run_in_tokens: u64,
        run_out_tokens: u64,
    }

    impl Default for Core {
        fn default() -> Self {
            Self::new()
        }
    }

    impl Core {
        pub fn new() -> Self {
            Self {
                history: Vec::new(),
                run_in_tokens: 0,
                run_out_tokens: 0,
            }
        }

        pub fn add_run_in_tokens(&mut self, n: u64) {
            self.run_in_tokens = self.run_in_tokens.saturating_add(n);
        }

        pub fn add_run_tokens(&mut self, n: u64) {
            self.run_out_tokens = self.run_out_tokens.saturating_add(n);
        }

        pub fn run_in_token_total(&self) -> u64 {
            self.run_in_tokens
        }

        pub fn run_out_token_total(&self) -> u64 {
            self.run_out_tokens
        }

        pub fn context_stats(&self) -> (u8, usize, usize) {
            let limit = 128_000usize;
            let reserve = limit / 6; // 预留 ~17%，避免 UI 贴边溢出
            let mut bytes: usize = 0;
            for m in &self.history {
                bytes = bytes.saturating_add(m.text.len());
            }
            let used = reserve
                .saturating_add(bytes.saturating_add(3) / 4)
                .min(limit);
            let left = limit.saturating_sub(used);
            let left_pct = ((left.saturating_mul(100)) / limit).min(100) as u8;
            (left_pct, used, limit)
        }

        pub fn push_user(&mut self, text: String) {
            let clean = text.trim_end().to_string();
            if clean.is_empty() {
                return;
            }
            self.history.push(Message {
                role: Role::User,
                text: clean,
                mind: None,
            });
        }

        pub fn push_assistant(&mut self, text: String) {
            let clean = text.trim_end().to_string();
            if clean.is_empty() {
                return;
            }
            self.history.push(Message {
                role: Role::Assistant,
                text: clean,
                mind: None,
            });
        }

        pub fn push_system(&mut self, text: &str) {
            let clean = text.trim_end().to_string();
            if clean.is_empty() {
                return;
            }
            self.history.push(Message {
                role: Role::System,
                text: clean,
                mind: None,
            });
        }

        pub fn push_tool(&mut self, text: String) {
            let clean = text.trim_end().to_string();
            if clean.is_empty() {
                return;
            }
            self.history.push(Message {
                role: Role::Tool,
                text: clean,
                mind: None,
            });
        }

        pub fn clear_chat(&mut self) {
            self.history.clear();
        }

        pub fn prune_history(&mut self, max_messages: usize, max_bytes: usize) -> usize {
            let max_messages = if max_messages == 0 {
                usize::MAX
            } else {
                max_messages
            };
            let max_bytes = if max_bytes == 0 {
                usize::MAX
            } else {
                max_bytes
            };
            let mut total_bytes: usize = self.history.iter().map(|m| m.text.len()).sum();
            let mut remove_count: usize = 0;
            while remove_count < self.history.len()
                && (self.history.len().saturating_sub(remove_count) > max_messages
                    || total_bytes > max_bytes)
            {
                total_bytes = total_bytes.saturating_sub(self.history[remove_count].text.len());
                remove_count = remove_count.saturating_add(1);
            }
            if remove_count > 0 {
                self.history.drain(0..remove_count);
            }
            remove_count
        }
    }
}

mod commands {
    use super::MindKind;
    #[derive(Debug, Clone, Copy)]
    pub struct CommandSpec {
        pub cmd: &'static str,
        pub desc: &'static str,
    }

    pub fn should_show_command_menu(input: &str) -> bool {
        if !input.starts_with('/') {
            return false;
        }
        if input.contains('\n') {
            return false;
        }
        let after = &input[1..];
        !after.chars().any(|c| c.is_whitespace())
    }

    pub fn filter_commands_for_input(
        input: &str,
        sse_enabled: bool,
        chat_target: MindKind,
    ) -> Vec<CommandSpec> {
        if !should_show_command_menu(input) {
            return Vec::new();
        }
        let mut items: Vec<CommandSpec> = Vec::new();
        items.push(CommandSpec {
            cmd: "/Cmd",
            desc: "Terminal",
        });
        items.push(CommandSpec {
            cmd: "/Settings",
            desc: "Settings",
        });
        let switch_target = if matches!(chat_target, MindKind::Main) {
            CommandSpec {
                cmd: "/Switch to",
                desc: "Memory",
            }
        } else {
            CommandSpec {
                cmd: "/Switch to",
                desc: "Main",
            }
        };
        items.push(switch_target);
        if sse_enabled {
            items.push(CommandSpec {
                cmd: "/SSE",
                desc: "Off",
            });
        } else {
            items.push(CommandSpec {
                cmd: "/SSE",
                desc: "On",
            });
        }
        items.push(CommandSpec {
            cmd: "/Quit",
            desc: "Quit",
        });
        let q = input.strip_prefix('/').unwrap_or("").to_ascii_lowercase();
        if q.is_empty() {
            return items;
        }
        let prefix = format!("/{q}");
        items
            .into_iter()
            .filter(|c| c.cmd.to_ascii_lowercase().starts_with(&prefix))
            .collect()
    }
}

mod config {
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug)]
    pub struct AppConfig {
        pub max_input_chars: usize,
        pub paste_capture_max_bytes: usize,
        pub paste_placeholder_char_threshold: usize,
        pub paste_placeholder_line_threshold: usize,
        pub paste_send_inhibit_ms: u64,
        pub paste_capture_flush_gap_ms: u64,
        pub paste_drop_cooldown_ms: u64,
        pub sys_scroll_ms: u64,
        pub sys_scroll_burst_ms: u64,
        pub reveal_frame_ms: u64,
        pub reveal_step: usize,
        pub active_frame_ms: u64,
        pub paste_redraw_throttle_ms: u64,
        pub send_status_ms: u64,
        pub input_status_ms: u64,
        pub exit_poll_ms: u64,
        pub sys_log_limit: usize,
        pub sys_display_limit: usize,
        pub max_history_messages: usize,
        pub max_history_bytes: usize,
        pub metamemo_path: String,
        pub datememo_path: String,
        pub contextmemo_path: String,
        pub run_log_path: String,
        pub memo_db_path: String,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct DogApiConfig {
        pub api_key: Option<String>,
        pub provider: String,
        pub base_url: String,
        pub model: String,
        pub temperature: Option<f32>,
        pub timeout_secs: u64,
        pub max_tokens: Option<u32>,
        pub prompt_path: String,
        pub prompt_reinject_pct: u8,
        pub token_total_path: String,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct MainApiConfig {
        pub api_key: Option<String>,
        pub provider: String,
        pub base_url: String,
        pub model: String,
        pub temperature: Option<f32>,
        pub timeout_secs: u64,
        pub max_tokens: Option<u32>,
        pub prompt_path: String,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct ContextPromptConfig {
        pub main_prompt: String,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct SystemConfig {
        pub context_k: usize,
        pub heartbeat_minutes: usize,
        #[serde(default = "default_true")]
        pub sse_enabled: bool,
        #[serde(default)]
        pub tool_full_access: bool,
        #[serde(default = "default_ctx_recent_max_tokens")]
        pub ctx_recent_max_tokens: usize,
        #[serde(default = "default_ctx_pool_max_items")]
        pub ctx_pool_max_items: usize,
        #[serde(default = "default_context_compact_prompt_path")]
        pub context_compact_prompt_path: String,
        #[serde(default)]
        pub chat_target: String,
    }

    fn default_true() -> bool {
        true
    }

    fn default_ctx_recent_max_tokens() -> usize {
        3000
    }

    fn default_ctx_pool_max_items() -> usize {
        10
    }

    fn default_context_compact_prompt_path() -> String {
        "prompts/context_compact.txt".to_string()
    }

    impl AppConfig {
        pub fn from_env() -> Self {
            let max_input_chars: usize = std::env::var("YING_MAX_INPUT_CHARS")
                .ok()
                .and_then(|s| s.trim().parse::<usize>().ok())
                .filter(|v| *v >= 1000 && *v <= 200_000)
                .unwrap_or(30_000);
            let paste_capture_max_bytes = max_input_chars.saturating_mul(20).min(1_000_000);
            let send_status_ms: u64 = std::env::var("YING_SEND_STATUS_MS")
                .ok()
                .and_then(|s| s.trim().parse::<u64>().ok())
                .filter(|v| *v >= 200 && *v <= 5000)
                .unwrap_or(650);
            let input_status_ms: u64 = std::env::var("YING_INPUT_STATUS_MS")
                .ok()
                .and_then(|s| s.trim().parse::<u64>().ok())
                .filter(|v| *v >= 200 && *v <= 5000)
                .unwrap_or(900);
            Self {
                max_input_chars,
                paste_capture_max_bytes,
                paste_placeholder_char_threshold: 100,
                paste_placeholder_line_threshold: 5,
                paste_send_inhibit_ms: 360,
                paste_capture_flush_gap_ms: 160,
                paste_drop_cooldown_ms: 1200,
                sys_scroll_ms: 70,
                sys_scroll_burst_ms: 1600,
                reveal_frame_ms: 50,
                reveal_step: 24,
                active_frame_ms: 60,
                paste_redraw_throttle_ms: 90,
                send_status_ms,
                input_status_ms,
                // 省电：空闲时降低 event::poll 唤醒频率（仍保持 1Hz 的运行时钟刷新）。
                exit_poll_ms: 1000,
                sys_log_limit: 6,
                sys_display_limit: 3,
                max_history_messages: std::env::var("YING_MAX_HISTORY_MESSAGES")
                    .ok()
                    .and_then(|s| s.trim().parse::<usize>().ok())
                    .filter(|v| *v >= 100)
                    .unwrap_or(800),
                max_history_bytes: std::env::var("YING_MAX_HISTORY_BYTES")
                    .ok()
                    .and_then(|s| s.trim().parse::<usize>().ok())
                    .filter(|v| *v >= 100_000)
                    .unwrap_or(3_000_000),
                metamemo_path: std::env::var("YING_METAMEMO_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/metamemo.jsonl".to_string()),
                datememo_path: std::env::var("YING_DATEMEMO_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/datememo.jsonl".to_string()),
                contextmemo_path: std::env::var("YING_CONTEXTMEMO_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/contextmemo.jsonl".to_string()),
                run_log_path: std::env::var("YING_RUN_LOG_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "log/runtime.log".to_string()),
                memo_db_path: std::env::var("YING_MEMO_DB_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/memo.db".to_string()),
            }
        }
    }

    impl Default for DogApiConfig {
        fn default() -> Self {
            Self {
                api_key: None,
                provider: "deepseek".to_string(),
                base_url: "https://api.deepseek.com/v1".to_string(),
                model: "deepseek-reasoner".to_string(),
                temperature: Some(0.6),
                timeout_secs: 60,
                max_tokens: Some(5_000),
                prompt_path: "prompts/dog.txt".to_string(),
                prompt_reinject_pct: 80,
                token_total_path: "memory/token_total.json".to_string(),
            }
        }
    }

    impl Default for MainApiConfig {
        fn default() -> Self {
            Self {
                api_key: None,
                provider: "deepseek".to_string(),
                base_url: "https://api.deepseek.com/v1".to_string(),
                model: "deepseek-chat".to_string(),
                temperature: Some(0.6),
                timeout_secs: 60,
                max_tokens: Some(5_000),
                prompt_path: "prompts/main.txt".to_string(),
            }
        }
    }

    impl Default for ContextPromptConfig {
        fn default() -> Self {
            Self {
                main_prompt: "系统：context 已满，需要进行上下文压缩。\
请你根据当前记忆生成日记，并直接调用 memory_add 写入 datememo。\
系统会附带上一条日记用于对比，请避免重复。仅输出工具 JSON，不要附加说明。\
格式：<tool>{\"tool\":\"memory_add\",\"path\":\"datememo\",\"content\":\"YYYY-MM-DD HH:MM:SS | main | 关键词: ... | 日记: ...\",\"brief\":\"我来更新日记。\"}</tool>"
                    .to_string(),
            }
        }
    }

    impl Default for SystemConfig {
        fn default() -> Self {
            Self {
                context_k: 128,
                heartbeat_minutes: 5,
                sse_enabled: true,
                tool_full_access: false,
                ctx_recent_max_tokens: default_ctx_recent_max_tokens(),
                ctx_pool_max_items: default_ctx_pool_max_items(),
                context_compact_prompt_path: default_context_compact_prompt_path(),
                chat_target: "dog".to_string(),
            }
        }
    }
}

mod input {
    use std::collections::HashSet;
    use std::time::{Duration, Instant};

    #[derive(Debug, Clone, Copy)]
    pub(crate) enum PlaceholderRemove {
        Backspace,
        Delete,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct PasteCapture {
        pub(crate) last_at: Instant,
        pub(crate) buf: String,
    }

    #[derive(Debug, Clone, Copy, Default)]
    pub(crate) struct PasteApplyResult {
        pub(crate) inserted: bool,
    }

    #[derive(Debug, Clone, Copy, Default)]
    pub(crate) struct PasteFinalizeResult {
        pub(crate) flushed: bool,
        pub(crate) rejected: bool,
    }

    pub(crate) fn update_paste_burst(
        now: Instant,
        last_at: &mut Option<Instant>,
        started_at: &mut Option<Instant>,
        count: &mut usize,
        start_cursor: &mut Option<usize>,
        cursor_before_insert: usize,
    ) {
        const GAP_MS: u64 = 45;
        if last_at
            .is_some_and(|t| now.saturating_duration_since(t) <= Duration::from_millis(GAP_MS))
        {
            *count = count.saturating_add(1);
        } else {
            *count = 1;
            *started_at = Some(now);
            *start_cursor = Some(cursor_before_insert);
        }
        *last_at = Some(now);
    }

    pub(crate) fn is_paste_like_activity(
        now: Instant,
        last_at: Option<Instant>,
        started_at: Option<Instant>,
        count: usize,
    ) -> bool {
        let Some(started) = started_at else {
            return false;
        };
        let dur = now.saturating_duration_since(started);
        let very_fast = count >= 4 && dur <= Duration::from_millis(200);
        let dense = count >= 12
            && last_at
                .is_some_and(|t| now.saturating_duration_since(t) <= Duration::from_millis(120));
        very_fast || dense
    }

    pub(crate) struct MaybeBeginPasteCaptureArgs<'a> {
        pub(crate) now: Instant,
        pub(crate) capture: &'a mut Option<PasteCapture>,
        pub(crate) input: &'a mut String,
        pub(crate) cursor: &'a mut usize,
        pub(crate) input_chars: &'a mut usize,
        pub(crate) toast: &'a mut Option<(Instant, String)>,
        pub(crate) burst_count: usize,
        pub(crate) burst_started_at: Option<Instant>,
        pub(crate) burst_start_cursor: Option<usize>,
    }

    pub(crate) fn maybe_begin_paste_capture(args: MaybeBeginPasteCaptureArgs<'_>) {
        let MaybeBeginPasteCaptureArgs {
            now,
            capture,
            input,
            cursor,
            input_chars,
            toast,
            burst_count,
            burst_started_at,
            burst_start_cursor,
        } = args;
        if capture.is_some() {
            return;
        }
        let Some(started) = burst_started_at else {
            return;
        };
        let dur = now.saturating_duration_since(started);
        let fast = burst_count >= 32 && dur <= Duration::from_millis(450);
        let slow = burst_count >= 64 && dur <= Duration::from_millis(2000);
        if !fast && !slow {
            return;
        }

        let start = burst_start_cursor.unwrap_or(*cursor).min(input.len());
        let end = (*cursor).min(input.len());
        if end <= start {
            return;
        }
        let already = input.get(start..end).unwrap_or("").to_string();
        if already.is_empty() {
            return;
        }
        input.drain(start..end);
        *cursor = start;
        *input_chars = input_chars.saturating_sub(count_chars(&already));

        *capture = Some(PasteCapture {
            last_at: now,
            buf: already,
        });
        *toast = Some((
            // 标题栏现在会走“●闪烁→乱码展开”动画；给足时间让短提示能完整读到。
            now + Duration::from_millis(1600),
            "检测到粘贴，捕获中…".to_string(),
        ));
    }

    fn next_large_paste_placeholder(
        pending: &[(String, String)],
        lines: usize,
        chars: usize,
    ) -> String {
        let base = format!("[Pasted Content {chars} chars / {lines} lines]");
        let count = pending
            .iter()
            .filter(|(ph, _)| ph.starts_with(&base))
            .count();
        if count == 0 {
            base
        } else {
            format!("{base} #{}", count + 1)
        }
    }

    pub(crate) fn materialize_pastes(input: &str, pending: &[(String, String)]) -> String {
        if pending.is_empty() {
            return input.to_string();
        }
        let mut out = input.to_string();
        for _ in 0..=pending.len() {
            let mut changed = false;
            for (ph, actual) in pending {
                if out.contains(ph) {
                    out = out.replace(ph, actual);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        out
    }

    pub(crate) fn materialize_char_count(input: &str, pending: &[(String, String)]) -> usize {
        let mut total = count_chars(input);
        if pending.is_empty() {
            return total;
        }
        for (ph, actual) in pending {
            if ph.is_empty() {
                continue;
            }
            let occurrences = input.matches(ph).count();
            if occurrences == 0 {
                continue;
            }
            let ph_chars = count_chars(ph);
            let actual_chars = count_chars(actual);
            if actual_chars >= ph_chars {
                let delta = actual_chars - ph_chars;
                total = total.saturating_add(delta.saturating_mul(occurrences));
            } else {
                let delta = ph_chars - actual_chars;
                total = total.saturating_sub(delta.saturating_mul(occurrences));
            }
        }
        total
    }

    pub(crate) fn can_accept_more(
        input: &str,
        pending: &[(String, String)],
        input_chars: usize,
        add: usize,
        max_input_chars: usize,
    ) -> bool {
        let current = if pending.is_empty() {
            input_chars
        } else {
            materialize_char_count(input, pending)
        };
        current.saturating_add(add) <= max_input_chars
    }

    fn placeholder_padding(input: &str, cursor: usize) -> (String, String) {
        let cur = cursor.min(input.len());
        let prev = input[..cur].chars().last();
        let next = input[cur..].chars().next();
        let mut prefix = String::new();
        let mut suffix = String::new();
        if prev.is_some() && prev != Some('\n') {
            prefix.push('\n');
        }
        if next.map(|c| c != '\n').unwrap_or(true) {
            suffix.push('\n');
        }
        (prefix, suffix)
    }

    pub(crate) fn prune_pending_pastes(input: &str, pending: &mut Vec<(String, String)>) {
        if pending.is_empty() {
            return;
        }
        let mut keep: HashSet<String> = HashSet::new();
        for (ph, _) in pending.iter() {
            if input.contains(ph) {
                keep.insert(ph.clone());
            }
        }
        loop {
            let before = keep.len();
            for (ph, actual) in pending.iter() {
                if !keep.contains(ph) {
                    continue;
                }
                for (ph2, _) in pending.iter() {
                    if keep.contains(ph2) {
                        continue;
                    }
                    if actual.contains(ph2) {
                        keep.insert(ph2.clone());
                    }
                }
            }
            if keep.len() == before {
                break;
            }
        }
        pending.retain(|(ph, _)| keep.contains(ph));
    }

    pub(crate) fn snap_cursor_out_of_placeholder(
        input: &str,
        pending: &[(String, String)],
        cursor: usize,
    ) -> usize {
        let cur = cursor.min(input.len());
        if pending.is_empty() || input.is_empty() {
            return cur;
        }
        for (ph, _) in pending {
            if ph.is_empty() {
                continue;
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
        }
        cur
    }

    pub(crate) struct ApplyPasteArgs<'a> {
        pub(crate) input: &'a mut String,
        pub(crate) cursor: &'a mut usize,
        pub(crate) input_chars: &'a mut usize,
        pub(crate) pending: &'a mut Vec<(String, String)>,
        pub(crate) toast: &'a mut Option<(Instant, String)>,
        pub(crate) now: Instant,
        pub(crate) pasted: String,
        pub(crate) per_paste_line_threshold: usize,
        pub(crate) per_paste_char_threshold: usize,
        pub(crate) max_input_chars: usize,
    }

    pub(crate) fn apply_paste(args: ApplyPasteArgs<'_>) -> PasteApplyResult {
        let ApplyPasteArgs {
            input,
            cursor,
            input_chars,
            pending,
            toast,
            now,
            mut pasted,
            per_paste_line_threshold,
            per_paste_char_threshold,
            max_input_chars,
        } = args;
        const PASTE_BLOCK_CHARS: usize = 6000;
        let max_bytes = max_input_chars.saturating_mul(20).min(1_000_000);
        let mut truncated = false;
        if pasted.len() > max_bytes {
            let mut end = max_bytes.min(pasted.len());
            while end > 0 && !pasted.is_char_boundary(end) {
                end = end.saturating_sub(1);
            }
            pasted.truncate(end);
            truncated = true;
        }

        let mut current_total = materialize_char_count(input, pending);
        if current_total >= max_input_chars {
            *toast = Some((
                now + Duration::from_millis(1600),
                format!("超出输入上限：{max_input_chars} 字符"),
            ));
            return PasteApplyResult { inserted: false };
        }
        let mut remaining_allow = max_input_chars.saturating_sub(current_total);
        let mut remaining = pasted.as_str();
        let mut total_chars = 0usize;
        let mut total_lines = 0usize;

        *cursor = snap_cursor_out_of_placeholder(input, pending, *cursor);

        while !remaining.is_empty() && remaining_allow > 0 {
            let take = remaining_allow.min(PASTE_BLOCK_CHARS);
            let end = byte_end_for_n_chars(remaining, take);
            let chunk = &remaining[..end];
            let chunk_chars = count_chars(chunk);
            let chunk_lines = chunk.split('\n').count().max(1);
            let optimize =
                chunk_lines > per_paste_line_threshold || chunk_chars > per_paste_char_threshold;
            let (prefix, suffix) = if optimize {
                placeholder_padding(input, *cursor)
            } else {
                (String::new(), String::new())
            };
            let extra = count_chars(&prefix).saturating_add(count_chars(&suffix));
            if current_total
                .saturating_add(chunk_chars)
                .saturating_add(extra)
                > max_input_chars
            {
                truncated = true;
                break;
            }

            if optimize {
                let placeholder = next_large_paste_placeholder(pending, chunk_lines, chunk_chars);
                let combined = format!("{prefix}{placeholder}{suffix}");
                if !try_insert_str_limited(input, cursor, &combined, input_chars, max_input_chars) {
                    truncated = true;
                    break;
                }
                pending.push((placeholder, chunk.to_string()));
            } else if !try_insert_str_limited(input, cursor, chunk, input_chars, max_input_chars) {
                truncated = true;
                break;
            }

            current_total = current_total.saturating_add(chunk_chars.saturating_add(extra));
            remaining_allow = max_input_chars.saturating_sub(current_total);
            remaining = &remaining[end..];
            total_chars = total_chars.saturating_add(chunk_chars);
            total_lines = total_lines.saturating_add(chunk_lines);
        }

        if !remaining.is_empty() {
            truncated = true;
        }
        if total_chars == 0 {
            *toast = Some((
                now + Duration::from_millis(1600),
                format!("超出输入上限：{max_input_chars} 字符"),
            ));
            return PasteApplyResult { inserted: false };
        }

        if truncated {
            *toast = Some((
                now + Duration::from_millis(1800),
                format!("超出输入上限：{max_input_chars} 字符（已截断）"),
            ));
        } else {
            *toast = Some((
                now + Duration::from_millis(1400),
                format!("已粘贴 {total_lines} 行 / {total_chars} 字符"),
            ));
        }

        prune_pending_pastes(input, pending);
        PasteApplyResult { inserted: true }
    }

    pub(crate) struct MaybeFinalizePasteCaptureArgs<'a> {
        pub(crate) force: bool,
        pub(crate) now: Instant,
        pub(crate) capture: &'a mut Option<PasteCapture>,
        pub(crate) input: &'a mut String,
        pub(crate) cursor: &'a mut usize,
        pub(crate) input_chars: &'a mut usize,
        pub(crate) pending: &'a mut Vec<(String, String)>,
        pub(crate) toast: &'a mut Option<(Instant, String)>,
        pub(crate) per_paste_line_threshold: usize,
        pub(crate) per_paste_char_threshold: usize,
        pub(crate) max_input_chars: usize,
        pub(crate) flush_gap_ms: u64,
    }

    pub(crate) fn maybe_finalize_paste_capture(
        args: MaybeFinalizePasteCaptureArgs<'_>,
    ) -> PasteFinalizeResult {
        let MaybeFinalizePasteCaptureArgs {
            force,
            now,
            capture,
            input,
            cursor,
            input_chars,
            pending,
            toast,
            per_paste_line_threshold,
            per_paste_char_threshold,
            max_input_chars,
            flush_gap_ms,
        } = args;
        let Some(c) = capture.as_ref() else {
            return PasteFinalizeResult::default();
        };
        let gap = now.saturating_duration_since(c.last_at);
        let should_flush = force || gap >= Duration::from_millis(flush_gap_ms);
        if !should_flush {
            return PasteFinalizeResult::default();
        }

        let Some(mut c) = capture.take() else {
            return PasteFinalizeResult::default();
        };
        let pasted = std::mem::take(&mut c.buf);
        if pasted.is_empty() {
            return PasteFinalizeResult {
                flushed: true,
                rejected: false,
            };
        }

        let res = apply_paste(ApplyPasteArgs {
            input,
            cursor,
            input_chars,
            pending,
            toast,
            now,
            pasted,
            per_paste_line_threshold,
            per_paste_char_threshold,
            max_input_chars,
        });
        PasteFinalizeResult {
            flushed: true,
            rejected: !res.inserted,
        }
    }

    pub(crate) fn try_remove_paste_placeholder_at_cursor(
        input: &mut String,
        cursor: &mut usize,
        input_chars: &mut usize,
        pending: &mut Vec<(String, String)>,
        how: PlaceholderRemove,
    ) -> bool {
        if pending.is_empty() {
            return false;
        }
        let cur = (*cursor).min(input.len());
        for idx in 0..pending.len() {
            let ph = pending[idx].0.clone();
            let ph_len = ph.len();
            if ph_len == 0 {
                continue;
            }

            let mut search = 0usize;
            while let Some(pos) = input.get(search..).and_then(|s| s.find(&ph)) {
                let start = search + pos;
                let end = start + ph_len;
                if cur > start && cur < end {
                    input.drain(start..end);
                    *cursor = start;
                    *input_chars = input_chars.saturating_sub(ph.chars().count());
                    pending.remove(idx);
                    return true;
                }
                search = end;
                if search >= input.len() {
                    break;
                }
            }

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
                if input.get(start..end) == Some(ph.as_str()) {
                    input.drain(start..end);
                    *cursor = start;
                    *input_chars = input_chars.saturating_sub(ph.chars().count());
                    pending.remove(idx);
                    return true;
                }
            }
        }
        false
    }

    pub(crate) fn insert_char(s: &mut String, cursor: &mut usize, ch: char) {
        s.insert(*cursor, ch);
        *cursor += ch.len_utf8();
    }

    pub(crate) fn insert_str(s: &mut String, cursor: &mut usize, text: &str) {
        if text.is_empty() {
            return;
        }
        s.insert_str(*cursor, text);
        *cursor += text.len();
    }

    pub(crate) fn count_chars(s: &str) -> usize {
        s.chars().count()
    }

    pub(crate) fn clamp_cursor_to_char_boundary(s: &str, cursor: &mut usize) {
        *cursor = (*cursor).min(s.len());
        while *cursor > 0 && !s.is_char_boundary(*cursor) {
            *cursor = (*cursor).saturating_sub(1);
        }
        if !s.is_char_boundary(*cursor) {
            *cursor = 0;
        }
    }

    pub(crate) fn try_insert_char_limited(
        input: &mut String,
        cursor: &mut usize,
        ch: char,
        input_chars: &mut usize,
        max_input_chars: usize,
    ) -> bool {
        if *input_chars >= max_input_chars {
            return false;
        }
        clamp_cursor_to_char_boundary(input, cursor);
        insert_char(input, cursor, ch);
        *input_chars = input_chars.saturating_add(1);
        true
    }

    pub(crate) fn try_insert_str_limited(
        input: &mut String,
        cursor: &mut usize,
        text: &str,
        input_chars: &mut usize,
        max_input_chars: usize,
    ) -> bool {
        if text.is_empty() {
            return true;
        }
        let need = count_chars(text);
        let remain = max_input_chars.saturating_sub(*input_chars);
        if need > remain {
            return false;
        }
        clamp_cursor_to_char_boundary(input, cursor);
        insert_str(input, cursor, text);
        *input_chars = input_chars.saturating_add(need);
        true
    }

    fn byte_end_for_n_chars(s: &str, n: usize) -> usize {
        if n == 0 {
            return 0;
        }
        let mut seen = 0usize;
        for (i, ch) in s.char_indices() {
            seen = seen.saturating_add(1);
            if seen == n {
                return i + ch.len_utf8();
            }
        }
        s.len()
    }

    pub(crate) fn prev_char_boundary(s: &str, mut idx: usize) -> usize {
        idx = idx.min(s.len());
        if idx == 0 {
            return 0;
        }
        if !s.is_char_boundary(idx) {
            idx = idx.saturating_sub(1);
            while idx > 0 && !s.is_char_boundary(idx) {
                idx = idx.saturating_sub(1);
            }
        }
        s[..idx].char_indices().last().map(|(i, _)| i).unwrap_or(0)
    }

    pub(crate) fn next_char_boundary(s: &str, mut idx: usize) -> usize {
        idx = idx.min(s.len());
        if idx >= s.len() {
            return s.len();
        }
        if !s.is_char_boundary(idx) {
            while idx < s.len() && !s.is_char_boundary(idx) {
                idx += 1;
            }
            if idx >= s.len() {
                return s.len();
            }
        }
        let mut iter = s[idx..].char_indices();
        let _ = iter.next();
        iter.next().map(|(i, _)| idx + i).unwrap_or(s.len())
    }
}
