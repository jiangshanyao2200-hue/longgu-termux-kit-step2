use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fs;
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::Context;
use crossterm::event::{
    DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste, EnableMouseCapture, Event,
    KeyCode, KeyModifiers, MouseEventKind,
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
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json::json;
use signal_hook::consts::signal::{SIGHUP, SIGINT, SIGQUIT, SIGTERM};
use signal_hook::iterator::Signals;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

mod api;
mod context;
mod mcp;
mod memory;
mod providers;
mod pseudo_terminal;
mod test;
mod ui;

pub(crate) use context::{
    ApiMessage, ContextUsage, DogState, FASTMEMO_PATH, clear_contextmemo, clear_dyncontext_file,
    contextmemo_size_kb, fastmemo_event_count_and_any_ge10, fastmemo_max_section_usage,
    load_contextmemo_tokens, log_contextmemo, log_dyncontext, normalize_messages_for_model_context,
    read_contextmemo_text, read_fastmemo_for_context,
};
use providers::DogClient;

// ===== 分区索引 =====
//（1）入口：run_loop。
//（2）请求：start_user_chat_request -> provider -> stream。
//（3）工具：stream_end -> tool -> 回执入 context。

mod messages {
    use crate::mcp::ToolCall;
    fn default_heartbeat_user() -> String {
        "[sys:心跳]{STAMP} | idle\n请像正常消息一样回复（可简短）。若无需回应请只回复 [mainpass]。不要调用工具。"
            .to_string()
    }

    fn default_tool_confirm_prompt() -> String {
        "[sys:工具确认]需要确认执行：{TOOL}（y确认 n拒绝）\n原因：{REASON}\n\n请求：\n```text\n{INPUT_PREVIEW}\n```\n\n回复：y/n（或 是/否）。"
            .to_string()
    }

    fn default_tool_confirm_user_allowed() -> String {
        "[sys:工具确认]用户允许执行".to_string()
    }

    fn default_tool_confirm_user_denied() -> String {
        "[sys:工具确认]用户拒绝执行".to_string()
    }

    fn default_tool_result_assistant() -> String {
        //（1）工具回执头必须零歧义。
        //（2）避免误判为仍在等待回执。
        "{HEADER}\n{RESULT}".to_string()
    }

    #[derive(Debug, Clone)]
    pub(crate) struct McpMessages {
        pub(crate) heartbeat_user: String,
        pub(crate) tool_confirm_prompt: String,
        pub(crate) tool_confirm_user_allowed: String,
        pub(crate) tool_confirm_user_denied: String,
        pub(crate) tool_result_assistant: String,
    }

    impl Default for McpMessages {
        fn default() -> Self {
            Self {
                heartbeat_user: default_heartbeat_user(),
                tool_confirm_prompt: default_tool_confirm_prompt(),
                tool_confirm_user_allowed: default_tool_confirm_user_allowed(),
                tool_confirm_user_denied: default_tool_confirm_user_denied(),
                tool_result_assistant: default_tool_result_assistant(),
            }
        }
    }

    fn default_pty_started_model_note() -> String {
        //（1）默认不注入 PTY 启动提示到模型上下文。
        //（2）启动阶段只做界面提示；等待 Done 或审计事件。
        String::new()
    }

    fn default_pty_done_auto_report_prompt() -> String {
        "[sys:PTYDone]PTY 已结束（{JOBS} 个任务）。请基于上一条 PTY Done 回执，简要总结关键结果（exit/耗时/关键输出）；如需用户介入或补充信息，请直接提问。"
            .to_string()
    }

    #[derive(Debug, Clone)]
    pub(crate) struct PtyMessages {
        pub(crate) pty_started_model_note: String,
        pub(crate) pty_done_auto_report_prompt: String,
    }

    impl Default for PtyMessages {
        fn default() -> Self {
            Self {
                pty_started_model_note: default_pty_started_model_note(),
                pty_done_auto_report_prompt: default_pty_done_auto_report_prompt(),
            }
        }
    }

    pub(crate) fn render_template(template: &str, pairs: &[(&str, &str)]) -> String {
        let mut out = template.to_string();
        for (k, v) in pairs {
            out = out.replace(&format!("{{{k}}}"), v);
        }
        out
    }

    pub(crate) fn render_heartbeat_user(msgs: &McpMessages, stamp: &str) -> String {
        render_template(&msgs.heartbeat_user, &[("STAMP", stamp)])
    }

    pub(crate) fn render_tool_confirm_prompt(
        msgs: &McpMessages,
        reason: &str,
        call: &ToolCall,
    ) -> String {
        let tool_label = crate::mcp::tool_display_label(&call.tool);
        let brief_raw = call.brief.as_deref().unwrap_or("").trim();
        let mut brief = crate::truncate_with_ellipsis(brief_raw, 90);
        if brief.trim().is_empty() {
            brief = "（无）".to_string();
        }
        let mut reason_preview = reason.replace('\n', " ");
        reason_preview = reason_preview
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        reason_preview = crate::truncate_with_ellipsis(&reason_preview, 140);
        let mut input_preview = call.input.replace('\n', " ");
        input_preview = input_preview
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        input_preview = crate::truncate_with_ellipsis(&input_preview, 180);
        if input_preview.trim().is_empty() {
            input_preview = "（空）".to_string();
        }

        //（1）兼容旧模板（如引用 {CALL_JSON}）。
        //（2）仅提供单行摘要，避免过长。
        let call_json = serde_json::json!({
            "tool": call.tool.as_str(),
            "brief": brief,
            "input": crate::truncate_with_ellipsis(&call.input, 240),
        });
        let mut call_json = serde_json::to_string(&call_json).unwrap_or_else(|_| "{}".to_string());
        call_json = crate::truncate_with_ellipsis(&call_json, 900);

        let mut out = render_template(
            &msgs.tool_confirm_prompt,
            &[
                ("REASON", &reason_preview),
                ("TOOL", &tool_label),
                ("CALL_JSON", &call_json),
                ("BRIEF", &brief),
                ("INPUT_PREVIEW", &input_preview),
            ],
        );

        //（1）若模板未写 y/n 提示，补到首行。
        //（2）保证手机端折叠态可操作。
        if let Some(first) = out.lines().next().map(str::trim).filter(|s| !s.is_empty())
            && first.starts_with("[sys:工具确认]")
            && !first.contains("y确认")
            && !first.contains("y/n")
        {
            if let Some(pos) = out.find('\n') {
                let (head, tail) = out.split_at(pos);
                out = format!("{head}（y确认 n拒绝）{tail}");
            } else {
                out.push_str("（y确认 n拒绝）");
            }
        }
        out
    }
}

use crate::commands::filter_commands_for_input;
use crate::config::{
    ApiProviderProfile, ApiProviderProfiles, AppConfig, ContextPromptConfig, DogApiConfig,
    MainApiConfig, SystemConfig,
};
use crate::input::{
    PasteCapture, PlaceholderRemove, can_accept_more, count_chars, is_paste_like_activity,
    maybe_begin_paste_capture, maybe_finalize_paste_capture, next_char_boundary,
    prev_char_boundary, prune_pending_pastes, snap_cursor_out_of_placeholder,
    try_insert_char_limited, try_insert_str_limited, try_remove_paste_placeholder_at_cursor,
    update_paste_burst,
};
use crate::mcp::{
    ToolCall, ToolOutcome, extract_tool_calls, format_tool_message_for_model,
    handle_tool_call_with_retry, tool_requires_confirmation,
};
use crate::memory::{MemoDb, MemoKind, build_memo_entry};
use crate::messages::{McpMessages, PtyMessages};
use crate::pseudo_terminal::{
    DrawPtyPanelArgs, HandleAsyncEventPtyJobDoneArgs, HandleAsyncEventPtyReadyArgs,
    HandleAsyncEventPtyToolRequestArgs, HandlePtyViewKeyArgs, PendingPtySnapshot, PtyControl,
    PtyDoneBatches, PtyDoneFollowup, PtyFocus, PtyUiState, TryShowPtyViewOnHomeArgs,
    apply_mouse_wheel_to_pty_view, apply_touch_drag_to_pty_view, current_pty_audit_target,
    defer_pty_audit, draw_pty_panel, handle_async_event_pty_job_done,
    handle_async_event_pty_output, handle_async_event_pty_ready, handle_async_event_pty_spawned,
    handle_async_event_pty_tool_request, handle_pty_view_key, is_pty_panel_active,
    load_pty_help_prompt, load_pty_started_notice_prompt, prune_pending_pty_snapshot,
    pty_bottom_height, pty_status_line_override, render_pty_audit_prompt, running_owner_count,
    snap_cursor_out_of_pty_snapshot_placeholder, spawn_interactive_bash_execution,
    try_remove_pty_snapshot_placeholder_at_cursor, try_show_pty_view_on_home,
};

pub use crate::commands::CommandSpec;
use crate::types::{BRIEF_TOOL_MARKER, THINK_TOOL_MARKER, THINKING_MARKER};
pub use crate::types::{
    ChatFocus, ContextLine, Core, Message, MindKind, Mode, PulseDir, Role, Screen, SettingsFocus,
};

// ===== 常量 =====
//（1）预算阈值集中在此。
//（2）默认偏保守，避免撑爆上下文。
//（3）具体策略在对应逻辑处解释。

const TOOL_STREAM_PREVIEW_MAX: usize = 8000;
const TOOL_CALLS_MAX_PER_ASSISTANT_MSG: usize = 3;
const HEARTBEAT_FADE_FRAMES: u64 = 4;
const HEARTBEAT_BLINK_FRAMES: u64 = 4;
const HEARTBEAT_CYCLE_FRAMES: u64 = HEARTBEAT_FADE_FRAMES * 3 + HEARTBEAT_BLINK_FRAMES;
const HEARTBEAT_CYCLES: u64 = 2;
const HEARTBEAT_BANNER: &str = "♡";
const PTY_SCROLLBACK_MAX: u16 = 3000;
const PTY_SNAPSHOT_MAX_CHARS: usize = 8000;
const PTY_DOUBLE_ESC_MS: u64 = 350;
//（1）交互 PTY 回传预算，防撑爆上下文。
const PTY_RETURN_SCREEN_MAX_CHARS: usize = 12_000;
const PTY_RETURN_RAW_TAIL_BYTES: u64 = 64 * 1024;
const PTY_RETURN_RAW_TAIL_MAX_CHARS: usize = 6_000;
const PTY_RETURN_STDIN_MAX_CHARS: usize = 2_000;
//（1）对齐命令类工具导出阈值。
const PTY_DONE_BATCH_TOOL_MAX_CHARS: usize = 20_000;
const PTY_DONE_BATCH_TAIL_MAX_CHARS: usize = 1_600;
const DEFAULT_WELCOME_SHORTCUTS_PROMPT: &str =
    include_str!("../config/Documents/Systemwelcome.txt");
const USER_PASTE_MARKER_BEGIN: &str = "[[AITERMUX_PASTE_BEGIN|";
const USER_PASTE_MARKER_END: &str = "[[AITERMUX_PASTE_END|";
const USER_PTY_SNAPSHOT_MARKER_BEGIN: &str = "[[AITERMUX_PTY_SNAPSHOT_BEGIN|";
const USER_PTY_SNAPSHOT_MARKER_END: &str = "[[AITERMUX_PTY_SNAPSHOT_END|";
const USER_PTY_SNAPSHOT_PLACEHOLDER: &str = "〔PTY,Terminal快照〕";
const MODEL_NOTE_PREFIX: &str = "[[AITERMUX_MODEL_NOTE]]";
//（1）工具调用只走 `<tool>...</tool>` 原文。
//（2）工具回执只走 `[MCP:...]` 标准块。
//（3）占位与伪交互不写入上下文。

// ===== 低层工具 =====
//（1）进程/IO 等系统级小工具。
//（2）失败优先静默返回，不影响主流程。
//（3）仅在必要处调用。

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

fn bash_single_quote(s: &str) -> String {
    if s.is_empty() {
        return "''".to_string();
    }
    let mut out = String::from("'");
    for ch in s.chars() {
        if ch == '\'' {
            out.push_str("'\"'\"'");
        } else {
            out.push(ch);
        }
    }
    out.push('\'');
    out
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

// ===== 工具解析（Hint）=====
//（1）只做轻量扫描，用于 UI/预览。
//（2）最终解析以完整块为准。
//（3）失败返回 None，不做纠错。
fn find_tool_start(text: &str) -> Option<usize> {
    text.find("<tool>")
}

fn extract_tool_name_hint(text: &str) -> Option<String> {
    let pos = text.find("\"tool\"")?;
    let rest = &text[pos + "\"tool\"".len()..];
    let mut iter = rest.chars();
    //（1）skip whitespace until ':'
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

fn extract_json_string_field_hint(text: &str, field: &str) -> Option<String> {
    let needle = format!("\"{field}\"");
    let pos = text.find(&needle)?;
    let rest = &text[pos + needle.len()..];
    let mut iter = rest.chars();
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

fn extract_json_array_first_string_hint(text: &str, field: &str) -> Option<String> {
    let needle = format!("\"{field}\"");
    let pos = text.find(&needle)?;
    let rest = &text[pos + needle.len()..];
    let mut iter = rest.chars();
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
    let arr = trimmed.strip_prefix('[')?;
    let qpos = arr.find('"')?;
    let after = &arr[qpos + 1..];
    let end = after.find('"')?;
    Some(after[..end].to_string())
}

fn safe_tail_chars(text: &str, max_chars: usize) -> String {
    if max_chars == 0 {
        return String::new();
    }
    let mut buf: Vec<char> = Vec::with_capacity(max_chars.min(256));
    for ch in text.chars() {
        if buf.len() == max_chars {
            buf.remove(0);
        }
        buf.push(ch);
    }
    buf.into_iter().collect()
}

fn build_tool_stream_stub_text(label: &str, tool_slice: &str) -> String {
    let brief = extract_json_string_field_hint(tool_slice, "brief")
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "...".to_string());

    let mut input = extract_json_string_field_hint(tool_slice, "input")
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .or_else(|| {
            extract_json_array_first_string_hint(tool_slice, "commands")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
        })
        .unwrap_or_default();

    if input.is_empty() {
        //（1）兜底：展示正在输出的 JSON 尾部（压缩空白），用于“跑马提示”。
        let compact = tool_slice
            .replace('\n', " ")
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        input = safe_tail_chars(&compact, 160);
        if input.is_empty() {
            input = "...".to_string();
        }
    }

    format!(
        "操作: {label}\nexplain: {brief}\ninput: {input}\noutput:\n```text\n...\n```\nmeta:\n```text\n状态:parsing\n```\n"
    )
}

fn mind_label(kind: MindKind) -> &'static str {
    match kind {
        MindKind::Main => "MAIN",
        MindKind::Sub => "DOG",
        MindKind::Memory => "MEM",
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
        "memory" | "mem" | "tomemory" | "to_memory" | "to-memory" | "记忆" => {
            Some(MindKind::Memory)
        }
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

// ===== 输入编辑 =====
//（1）处理粘贴、游标、限长与插入。
//（2）以手机端体验为优先。
//（3）不在此处做业务规则。
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
    force_working: bool,
    mode: Mode,
    reveal_idx: Option<usize>,
    streaming_has_content: bool,
    brief_expected: bool,
    brief_text: &'a str,
    thinking_text: &'a str,
    tool_preview_any: bool,
    tool_preview: &'a str,
    pending_tool_confirm: Option<&'a ToolCall>,
    active_tool_stream: Option<&'a ToolStreamState>,
    mind_pulse: Option<PulseDir>,
    input_active: bool,
    diary_active: bool,
}

fn build_active_status(args: ActiveStatusArgs<'_>) -> Option<String> {
    let ActiveStatusArgs {
        now,
        retry_status,
        sending_until,
        force_working,
        mode,
        reveal_idx,
        streaming_has_content,
        brief_expected,
        brief_text,
        thinking_text,
        tool_preview_any,
        tool_preview,
        pending_tool_confirm,
        active_tool_stream,
        mind_pulse,
        input_active,
        diary_active,
    } = args;
    fn mind_name(kind: MindKind) -> &'static str {
        match kind {
            MindKind::Main => "Main",
            MindKind::Sub => "Dog",
            MindKind::Memory => "Memory",
        }
    }
    fn tool_name(tool: &str) -> String {
        match tool.trim() {
            "adb" => "ADB".to_string(),
            "bash" => "BASH".to_string(),
            "termux_api" => "Termux API".to_string(),
            "list" => "List".to_string(),
            "apply_patch" => "Patch".to_string(),
            "memory_add" => "MAdd".to_string(),
            "memory_check" => "MFind".to_string(),
            "memory_read" => "MRead".to_string(),
            "mind_msg" => "Mind".to_string(),
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
        //（1）仅 Codex 会设置 sending_until（用于首包/连接期间的体感提示）。
        return Some("Connecting...".to_string());
    }
    if let Some(dir) = mind_pulse {
        let s = match dir {
            PulseDir::MainToDog => "Linking Main → Dog",
            PulseDir::DogToMain => "Linking Dog → Main",
        };
        return Some(s.to_string());
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
        return Some("Tool...".to_string());
    }
    if force_working {
        if let Some(header) = status_header_from_brief_text(brief_text) {
            return Some(header);
        }
        return Some("Connecting...".to_string());
    }
    //（1）Codex Responses：伪流式。
    //（2）顶栏不显示 Thinking/Typing。
    //（3）只显示 Working -> brief。
    //（4）brief_expected 由 brief stub 决定。
    if brief_expected {
        if let Some(header) = status_header_from_brief_text(brief_text) {
            return Some(header);
        }
        return Some("Connecting...".to_string());
    }
    if streaming_has_content || reveal_idx.is_some() {
        return Some("Typing...".to_string());
    }
    if !thinking_text.trim().is_empty() {
        return Some("Thinking...".to_string());
    }
    if let Some(header) = status_header_from_brief_text(brief_text) {
        return Some(header);
    }
    if brief_expected {
        return Some("Connecting...".to_string());
    }
    match mode {
        Mode::Generating => Some("Thinking...".to_string()),
        Mode::ExecutingTool => Some("Running tool...".to_string()),
        Mode::ApprovingTool => Some("Approve tool".to_string()),
        _ => {
            if input_active {
                Some("Typing...".to_string())
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
        _ => match kind {
            SettingsFieldKind::Static => "",
            SettingsFieldKind::Provider => "Select provider",
            SettingsFieldKind::BaseUrl => "API base URL",
            SettingsFieldKind::ApiKey => "API key",
            SettingsFieldKind::Model => "Model",
            SettingsFieldKind::ReasoningEffort => "Reasoning effort",
            SettingsFieldKind::Temperature => "Temperature (0~2)",
            SettingsFieldKind::MaxTokens => "Max tokens",
            SettingsFieldKind::DateKbLimit => "Diary trigger threshold (KB, 0=off)",
            SettingsFieldKind::HeartbeatMinutes => "Heartbeat interval (min, 0=off)",
            SettingsFieldKind::SseEnabled => "Toggle SSE",
            SettingsFieldKind::ExecPermission => "Tool execution permission (safe/full)",
            SettingsFieldKind::DogPrompt => "Edit DOG prompt",
            SettingsFieldKind::MainPrompt => "Edit MAIN prompt",
            SettingsFieldKind::ContextMainPrompt => "Edit context prompt",
            SettingsFieldKind::PromptFile => "Edit prompt file",
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
    pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
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
        pending_pty_snapshot,
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
    *cursor = snap_cursor_out_of_pty_snapshot_placeholder(input, pending_pty_snapshot, *cursor);
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
    prune_pending_pty_snapshot(input, pending_pty_snapshot);
}

struct InsertNewlineArgs<'a> {
    now: Instant,
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    pending_pastes: &'a mut Vec<(String, String)>,
    pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
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
        pending_pty_snapshot,
        toast,
        max_input_chars,
        command_menu_suppress,
    } = args;
    *cursor = snap_cursor_out_of_placeholder(input, pending_pastes, *cursor);
    *cursor = snap_cursor_out_of_pty_snapshot_placeholder(input, pending_pty_snapshot, *cursor);
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
    prune_pending_pty_snapshot(input, pending_pty_snapshot);
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

fn input_cursor_for_click(
    text: &str,
    width: usize,
    height: usize,
    cursor: usize,
    click_x: usize,
    click_y_visible: usize,
) -> usize {
    if text.is_empty() || width == 0 || height == 0 {
        return 0;
    }
    let cursor = cursor.min(text.len());
    let map = build_cursor_map(text, width);
    let (_cx, cy) = cursor_xy_from_map(&map, cursor);
    let scroll_y = cy.saturating_sub(height.saturating_sub(1));
    let target_y = scroll_y.saturating_add(click_y_visible);
    cursor_index_for_xy(&map, click_x.min(width), target_y)
}

fn compact_ws_inline(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn load_prompt(path: &Path) -> anyhow::Result<String> {
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("读取提示词失败: {}", path.display()))?;
    Ok(text.trim().to_string())
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
    fn normalize_merges_consecutive_assistants_for_deepseek() {
        let msgs = vec![
            ApiMessage {
                role: "assistant".to_string(),
                content: "a".to_string(),
            },
            ApiMessage {
                role: "assistant".to_string(),
                content: "b".to_string(),
            },
        ];
        let snap = crate::context::normalize_messages_for_provider("deepseek", &msgs);
        let assistants: Vec<&ApiMessage> = snap.iter().filter(|m| m.role == "assistant").collect();
        assert_eq!(assistants.len(), 1);
        assert!(assistants[0].content.contains("a"));
        assert!(assistants[0].content.contains("b"));
    }

    #[test]
    fn tool_context_snapshot_keeps_system_receipt() {
        let mut state = DogState::new(
            String::new(),
            80,
            "Tool result:
{RESULT}"
                .to_string(),
        );
        state.push_tool("ok");
        let snap = state.message_snapshot(None);
        assert!(
            snap.iter()
                .any(|m| m.role == "system" && m.content.contains("Tool result:"))
        );
        assert!(
            snap.iter()
                .any(|m| m.role == "system" && m.content.contains("ok"))
        );
    }
}

#[cfg(test)]
mod think_association_tests {
    use super::*;

    #[test]
    fn find_think_for_tool_selection_with_multiple_tool_messages() {
        let mut core = Core::new();
        core.push_user("u".to_string());

        let thinking = "reasoning text";
        let think_msg =
            build_thinking_tool_message("", thinking, 1, thinking.chars().count(), MindKind::Main);
        core.push_tool(think_msg);

        core.push_tool("TOOL: BASH\nEXPLAIN: a\nINPUT: pwd\n".to_string());
        core.push_tool("TOOL: BASH\nEXPLAIN: b\nINPUT: date\n".to_string());

        //（1）选中任意 tool 回执，都应关联同一个 think 块。
        assert_eq!(find_think_message_for_selection(&core, 2), Some(1));
        assert_eq!(find_think_message_for_selection(&core, 3), Some(1));
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
                expect_brief: false,
                request_id: 1
            }),
            Some(1)
        );
        assert_eq!(
            event_request_id(&AsyncEvent::ModelStreamChunk {
                content: String::new(),
                reasoning: String::new(),
                brief: String::new(),
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
    let path_buf = resolve_config_path_from_env("YING_DOG_CONFIG", "config/dogAPI.json");
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
    let path_buf = resolve_config_path_from_env("YING_MAIN_CONFIG", "config/mainAPI.json");
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

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(default)]
struct MainApiOverrides {
    api_key: Option<String>,
    provider: Option<String>,
    base_url: Option<String>,
    model: Option<String>,
    reasoning_effort: Option<String>,
    temperature: Option<f32>,
    timeout_secs: Option<u64>,
    max_tokens: Option<u32>,
    provider_profiles: Option<ApiProviderProfilesOverrides>,
    prompt_path: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(default)]
struct ApiProviderProfilesOverrides {
    deepseek: Option<ApiProviderProfileOverrides>,
    codex: Option<ApiProviderProfileOverrides>,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(default)]
struct ApiProviderProfileOverrides {
    base_url: Option<String>,
    api_key: Option<String>,
    model: Option<String>,
    reasoning_effort: Option<String>,
    temperature: Option<f32>,
    max_tokens: Option<u32>,
}

fn merge_main_api_overrides(mut base: MainApiConfig, ov: MainApiOverrides) -> MainApiConfig {
    let set_opt_str = |dst: &mut String, v: Option<String>| {
        if let Some(v) = v {
            let t = v.trim().to_string();
            if !t.is_empty() {
                *dst = t;
            }
        }
    };
    let set_opt_u64 = |dst: &mut u64, v: Option<u64>| {
        if let Some(v) = v
            && v > 0
        {
            *dst = v;
        }
    };
    let set_opt_u32 = |dst: &mut Option<u32>, v: Option<u32>| {
        if let Some(v) = v
            && v > 0
        {
            *dst = Some(v);
        }
    };

    if let Some(v) = ov.api_key
        && !v.trim().is_empty()
    {
        base.api_key = Some(v.trim().to_string());
    }
    set_opt_str(&mut base.provider, ov.provider);
    set_opt_str(&mut base.base_url, ov.base_url);
    set_opt_str(&mut base.model, ov.model);
    if let Some(v) = ov.reasoning_effort {
        let t = v.trim().to_string();
        if !t.is_empty() {
            base.reasoning_effort = Some(t);
        }
    }
    base.temperature = ov.temperature;
    set_opt_u64(&mut base.timeout_secs, ov.timeout_secs);
    set_opt_u32(&mut base.max_tokens, ov.max_tokens);
    if let Some(v) = ov.prompt_path {
        let t = v.trim().to_string();
        if !t.is_empty() {
            base.prompt_path = t;
        }
    }

    if let Some(pp) = ov.provider_profiles {
        if let Some(p) = pp.deepseek {
            set_opt_str(&mut base.provider_profiles.deepseek.base_url, p.base_url);
            if let Some(v) = p.api_key
                && !v.trim().is_empty()
            {
                base.provider_profiles.deepseek.api_key = Some(v.trim().to_string());
            }
            set_opt_str(&mut base.provider_profiles.deepseek.model, p.model);
            if let Some(v) = p.reasoning_effort {
                let t = v.trim().to_string();
                if !t.is_empty() {
                    base.provider_profiles.deepseek.reasoning_effort = Some(t);
                }
            }
            base.provider_profiles.deepseek.temperature = p.temperature;
            set_opt_u32(
                &mut base.provider_profiles.deepseek.max_tokens,
                p.max_tokens,
            );
        }
        if let Some(p) = pp.codex {
            set_opt_str(&mut base.provider_profiles.codex.base_url, p.base_url);
            if let Some(v) = p.api_key
                && !v.trim().is_empty()
            {
                base.provider_profiles.codex.api_key = Some(v.trim().to_string());
            }
            set_opt_str(&mut base.provider_profiles.codex.model, p.model);
            if let Some(v) = p.reasoning_effort {
                let t = v.trim().to_string();
                if !t.is_empty() {
                    base.provider_profiles.codex.reasoning_effort = Some(t);
                }
            }
            base.provider_profiles.codex.temperature = p.temperature;
            set_opt_u32(&mut base.provider_profiles.codex.max_tokens, p.max_tokens);
        }
    }

    base
}

fn load_memory_api_config(main_cfg: &MainApiConfig) -> (MainApiConfig, Option<String>, PathBuf) {
    //（1）Memory 默认复用 MAIN 配置；memoryAPI.json 可覆盖。
    let path_buf = resolve_config_path_from_env("YING_MEMORY_CONFIG", "config/memoryAPI.json");
    let (mut ov, err) = load_json_config::<MainApiOverrides>(&path_buf, "MEMORY 配置");
    if let Ok(key) =
        std::env::var("DEEPSEEK_API_KEY").or_else(|_| std::env::var("YING_MEMORY_API_KEY"))
        && !key.trim().is_empty()
    {
        ov.api_key = Some(key);
    }
    let mut cfg = merge_main_api_overrides(main_cfg.clone(), ov);
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
    let path_buf = resolve_config_path_from_env("YING_SYSTEM_CONFIG", "config/System.json");
    let (mut cfg, err) = load_json_config::<SystemConfig>(&path_buf, "系统配置");
    if cfg.context_k < 8 || cfg.context_k > 1024 {
        cfg.context_k = 128;
    }
    //（1）日记压缩阈值：以 contextmemo 文件大小（KB）计算；0 表示关闭自动写日记。
    if cfg.date_kb_limit > 200_000 {
        cfg.date_kb_limit = 256;
    }
    cfg.heartbeat_minutes = normalize_heartbeat_minutes(cfg.heartbeat_minutes);
    if cfg.pty_audit_prompt_path.trim().is_empty() {
        cfg.pty_audit_prompt_path = "prompt/ptycheck.txt".to_string();
    }
    if cfg.pty_help_prompt_path.trim().is_empty() {
        cfg.pty_help_prompt_path = "config/Documents/Terminalwelcome.txt".to_string();
    }
    if cfg.welcome_shortcuts_prompt_path.trim().is_empty() {
        cfg.welcome_shortcuts_prompt_path = "config/Documents/Systemwelcome.txt".to_string();
    }
    cfg = normalize_system_config_paths(cfg, &path_buf);
    (cfg, err, path_buf)
}

fn normalize_common_api_fields(
    provider: &mut String,
    temperature: &mut Option<f32>,
    timeout_secs: &mut u64,
    max_tokens: &mut Option<u32>,
) {
    const MAX_TOKENS_HARD_MAX: u32 = 65_536;
    if provider.trim().is_empty() {
        *provider = "deepseek".to_string();
    }
    let p = provider.trim().to_ascii_lowercase();
    if let Some(temp) = *temperature {
        if !(0.0..=1.5).contains(&temp) {
            *temperature = None;
        } else if p == "deepseek" && (temp - 1.0).abs() < 1e-6 {
            //（1）DeepSeek temperature 默认 1；把 1 视为默认值。
            *temperature = None;
        }
    }
    if *timeout_secs < 5 {
        *timeout_secs = 60;
    }
    if let Some(tokens) = *max_tokens {
        if tokens == 0 {
            *max_tokens = Some(5_000);
        } else if tokens > MAX_TOKENS_HARD_MAX {
            *max_tokens = Some(MAX_TOKENS_HARD_MAX);
        }
    } else {
        *max_tokens = Some(5_000);
    }
}

fn normalize_heartbeat_minutes(value: usize) -> usize {
    match value {
        0 | 5 | 10 | 30 | 60 => value,
        _ => 5,
    }
}

const HEARTBEAT_DEFER_SECS: u64 = 600;
//（1）关闭心跳时，把 next_heartbeat_at 推到足够远，避免极端情况下被其它逻辑误触发。
const HEARTBEAT_DISABLED_SECS: u64 = 60 * 60 * 24 * 365 * 10;
const PTY_AUDIT_INTERVAL_SECS: u64 = 600;
const PTY_AUDIT_DEFER_SECS: u64 = 120;

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
        "memory_add" => "日记",
        "memory_check" | "memory_read" => "回忆",
        "bash" => "命令",
        "adb" | "termux_api" => "调试",
        "list" => "文件",
        "apply_patch" => "补丁",
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

fn extract_tool_blocks_verbatim(text: &str) -> String {
    //（1）从模型原文里提取 <tool>...</tool> 块（逐块拼接），用于“原样回放”进上下文：
    //（2）不注入任何 UI/摘要格式
    //（3）不把工具回执伪装成工具调用
    let mut out: Vec<String> = Vec::new();
    let mut rest = text;
    loop {
        let Some(start) = rest.find("<tool>") else {
            break;
        };
        let after = &rest[start..];
        let Some(end_rel) = after.find("</tool>") else {
            break;
        };
        let end = start + end_rel + "</tool>".len();
        let block = rest.get(start..end).unwrap_or("").trim();
        if !block.is_empty() {
            out.push(block.to_string());
        }
        rest = rest.get(end..).unwrap_or("");
    }
    out.join("\n")
}

fn dedupe_tool_calls_in_place(calls: &mut Vec<ToolCall>) -> usize {
    if calls.len() <= 1 {
        return 0;
    }
    let mut kept: Vec<ToolCall> = Vec::new();
    let mut seen: Vec<String> = Vec::new();
    for call in calls.iter() {
        let key = format!("{call:?}");
        if seen.iter().any(|k| k == &key) {
            continue;
        }
        seen.push(key);
        kept.push(call.clone());
    }
    let removed = calls.len().saturating_sub(kept.len());
    *calls = kept;
    removed
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
        resolve_config_path_from_env("YING_CTX_MAIN_PROMPT_PATH", "prompt/datesummary.txt");
    let mut errors = Vec::new();
    let (main_prompt, err_main) =
        load_prompt_file_with_default(&main_path, &defaults.main_prompt, "CONTEXT-MAIN");
    if let Some(err) = err_main {
        errors.push(err);
    }
    (ContextPromptConfig { main_prompt }, errors, main_path)
}

const DEFAULT_PTY_AUDIT_PROMPT: &str = "[sys:PTY审计]后台 PTY 仍在运行（已到审计间隔）。\nowner:{OWNER} | running_total:{RUNNING_TOTAL} | running_owner:{RUNNING_OWNER}\n\n请先调用 1 次 `pty.list`。\n然后用 2-4 行回答：是否仍在运行、是否建议 `pty.kill(job_id)`；不要输出键位/触控说明；不要反复轮询。";

fn load_pty_audit_prompt(sys_cfg: &SystemConfig) -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path(&sys_cfg.pty_audit_prompt_path, true);
    let (text, err) = load_prompt_file_with_default(&path, DEFAULT_PTY_AUDIT_PROMPT, "PTY-AUDIT");
    (text, err, path)
}

const DEFAULT_FASTMEMO_COMPACT_PROMPT: &str = "系统：fastmemo 某分区已达到10条，需要压缩（由 MEMORY 执行）。\n\n目标：只压缩1个分区，然后停止。\n步骤：\n1 调用 memory_read(path=fastmemo) 读取原文\n2 找到条目数>=10的第一个分区，优先级：自我感知>用户感知>环境感知>动态上下文\n3 将该分区10条压缩为1条 bullet（长期稳定/可复用/关键信息；不确定丢弃；不要写回大段日志/代码）\n4 调用 memory_add(path=fastmemo, section=..., compact=true, content=\"- ...\") 写回";

fn load_fastmemo_compact_prompt() -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path_from_env(
        "YING_FASTMEMO_COMPACT_PROMPT_PATH",
        "prompt/fastmemorycompact.txt",
    );
    let (text, err) =
        load_prompt_file_with_default(&path, DEFAULT_FASTMEMO_COMPACT_PROMPT, "FASTMEMO-COMPACT");
    (text, err, path)
}

const DEFAULT_WELCOME_SHORTCUTS_PROMPT_LABEL: &str = "WELCOME";

fn load_welcome_shortcuts_prompt(sys_cfg: &SystemConfig) -> (String, Option<String>, PathBuf) {
    let path = resolve_config_path(&sys_cfg.welcome_shortcuts_prompt_path, true);
    let (text, err) = load_prompt_file_with_default(
        &path,
        DEFAULT_WELCOME_SHORTCUTS_PROMPT,
        DEFAULT_WELCOME_SHORTCUTS_PROMPT_LABEL,
    );
    (text, err, path)
}

fn resolve_config_path(path: &str, prefer_project: bool) -> PathBuf {
    let raw = PathBuf::from(path);
    if raw.is_absolute() {
        return raw;
    }
    if raw.exists() {
        return raw;
    }
    //（1）优先：按项目根目录解析（从 exe 向上探测）。
    //（2）Termux：exe 常在 target/{debug,release}/。
    //（3）配置与提示词在根目录 config/ 与 prompt/。
    if let Some(root) = resolve_project_root_from_exe() {
        return root.join(&raw);
    }
    //（1）次选：以 exe 目录为基准向上探测。
    //（2）最多 5 层，找到存在的相对路径。
    if prefer_project && let Some(candidate) = resolve_relative_to_exe(&raw) {
        return candidate;
    }
    if !prefer_project && let Some(candidate) = resolve_relative_to_exe(&raw) {
        return candidate;
    }
    raw
}

fn resolve_project_root_from_exe() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    let mut dir = exe.parent().map(|p| p.to_path_buf());
    for _ in 0..8 {
        let current = dir.clone()?;
        if current.join("Cargo.toml").exists() || current.join("config").is_dir() {
            return Some(current);
        }
        dir = current.parent().map(|p| p.to_path_buf());
    }
    None
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
    hydrate_provider_profiles_dog(&mut cfg);
    cfg
}

fn normalize_main_config_paths(mut cfg: MainApiConfig, cfg_path: &Path) -> MainApiConfig {
    if let Some(base) = config_root_dir(cfg_path) {
        cfg.prompt_path = resolve_relative_path(&cfg.prompt_path, &base);
    }
    hydrate_provider_profiles_main(&mut cfg);
    cfg
}

fn normalize_system_config_paths(mut cfg: SystemConfig, cfg_path: &Path) -> SystemConfig {
    if let Some(base) = config_root_dir(cfg_path) {
        cfg.pty_audit_prompt_path = resolve_relative_path(&cfg.pty_audit_prompt_path, &base);
        cfg.pty_help_prompt_path = resolve_relative_path(&cfg.pty_help_prompt_path, &base);
        cfg.welcome_shortcuts_prompt_path =
            resolve_relative_path(&cfg.welcome_shortcuts_prompt_path, &base);
    }
    cfg
}

fn build_main_client(cfg: &MainApiConfig) -> anyhow::Result<DogClient> {
    let dog_cfg = DogApiConfig {
        api_key: cfg.api_key.clone(),
        provider: cfg.provider.clone(),
        base_url: cfg.base_url.clone(),
        model: cfg.model.clone(),
        reasoning_effort: cfg.reasoning_effort.clone(),
        temperature: cfg.temperature,
        timeout_secs: cfg.timeout_secs,
        max_tokens: cfg.max_tokens,
        provider_profiles: cfg.provider_profiles.clone(),
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

const MIND_CTX_MAX_ROUNDS: usize = 20;
const MIND_CTX_MAX_TOKENS: usize = 5000;
const MIND_RATE_WINDOW_SECS: u64 = 10 * 60;
const MIND_RATE_MAX_ROUNDS: usize = 10; // 一来一回=1轮
const MIND_RATE_MAX_HALF_TURNS: usize = MIND_RATE_MAX_ROUNDS * 2;

fn mind_used_rounds(half_turns: usize) -> usize {
    //（1）1 次发送=进入第 1 轮；2 次（来回）仍算第 1 轮；3 次进入第 2 轮……
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
                (MindKind::Main, MindKind::Memory) => "萤→记忆",
                (MindKind::Memory, MindKind::Main) => "记忆→萤",
                (MindKind::Sub, MindKind::Memory) => "潜意识→记忆",
                (MindKind::Memory, MindKind::Sub) => "记忆→潜意识",
                (MindKind::Memory, MindKind::Memory) => "记忆→记忆",
            };
            out.push_str(&format!("- {label}: {}\n", entry.text));
        }
        out.trim_end().to_string()
    }
}

fn truncate_to_token_budget(text: &str, max_tokens: usize) -> String {
    let max_bytes = max_tokens.saturating_mul(4).max(1);
    let mut out = text.to_string();
    truncate_to_max_bytes(&mut out, max_bytes);
    out
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

fn hash64(text: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

fn metamemo_ts() -> String {
    chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

fn init_run_logger(path: &str) {
    crate::test::init_runtime_log(path);
}

fn runlog_event(level: &str, event: &str, data: serde_json::Value) {
    crate::test::runlog_event(level, event, data);
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
    crate::test::append_run_log(path, level, msg);
}

fn compact_metamemo_text(role: &str, agent: Option<&str>, text: &str) -> Option<String> {
    let clean = text.trim();
    if clean.is_empty() {
        return None;
    }
    //（1）MetaMemo 只保留“真正聊天记录”：user/assistant（以及极简的 tool 摘要）。
    //（2）system（含提示词/上下文注入/状态通知）一律不写入，避免撑爆 db。
    //（3）tool 只写入操作摘要，不写 output/meta 内容。
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
        //（1）明确的 UI 指令不属于“聊天内容”，不进 MetaMemo。
        if lower == "/quit"
            || lower == "/q"
            || lower == "/cmd"
            || lower == "/settings"
            || lower.starts_with("/sse ")
            || lower == "/help"
            || lower == "/terminal"
            || lower == "/term"
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
    //（1）仅保留 tool 头部信息：操作/说明/input/mind，并在 output/meta 前截止。
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

fn truncate_with_ellipsis(text: &str, max_chars: usize) -> String {
    let total = text.chars().count();
    if total <= max_chars {
        return text.to_string();
    }
    if max_chars <= 1 {
        return "…".to_string();
    }
    let mut out = String::new();
    for (count, ch) in text.chars().enumerate() {
        if count >= max_chars.saturating_sub(1) {
            break;
        }
        out.push(ch);
    }
    out.push('…');
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
    //（1）PTY 原始字节可能含控制符；仅做 lossy 解码与 NUL 清理。
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

// ===== 工具链（Tool Stream 状态机 + 回执注入 + 续跑）=====
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
    Api,
    System,
    PromptCenter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ApiConfigKind {
    Main,
    Dog,
    Memory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SettingsFieldKind {
    Static,
    Provider,
    BaseUrl,
    ApiKey,
    Model,
    ReasoningEffort,
    Temperature,
    MaxTokens,
    DateKbLimit,
    HeartbeatMinutes,
    SseEnabled,
    ExecPermission,
    DogPrompt,
    MainPrompt,
    ContextMainPrompt,
    PromptFile,
}

struct SettingsFieldSpec {
    label: &'static str,
    value: String,
    kind: SettingsFieldKind,
    api_kind: Option<ApiConfigKind>,
    prompt_path: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConfirmChoice {
    Yes,
    No,
}

#[derive(Debug, Clone)]
struct ConfirmDialogState {
    msg: String,
    selected: ConfirmChoice,
    start_tick: usize,
}

struct SettingsState {
    menu_index: usize,
    api_kind: ApiConfigKind,
    field_index: usize,
    focus: SettingsFocus,
    edit_kind: Option<SettingsFieldKind>,
    edit_api_kind: Option<ApiConfigKind>,
    edit_prompt_path: Option<PathBuf>,
    edit_buffer: String,
    edit_original: String,
    edit_cursor: usize,
    confirm: Option<ConfirmDialogState>,
    notice: Option<(Instant, String)>,
}

impl SettingsState {
    fn new() -> Self {
        Self {
            menu_index: 0,
            api_kind: ApiConfigKind::Main,
            field_index: 0,
            focus: SettingsFocus::Tabs,
            edit_kind: None,
            edit_api_kind: None,
            edit_prompt_path: None,
            edit_buffer: String::new(),
            edit_original: String::new(),
            edit_cursor: 0,
            confirm: None,
            notice: None,
        }
    }
}

fn set_settings_notice(settings: &mut SettingsState, now: Instant, msg: String) {
    settings.notice = Some((now + Duration::from_millis(1600), msg));
}

fn reset_settings_edit(settings: &mut SettingsState) {
    settings.edit_kind = None;
    settings.edit_api_kind = None;
    settings.edit_prompt_path = None;
    settings.edit_buffer.clear();
    settings.edit_original.clear();
    settings.edit_cursor = 0;
    settings.confirm = None;
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

fn settings_tab_index_for_click(
    menu_items: &[String],
    max_w: usize,
    line: u16,
    rel_col: u16,
) -> Option<usize> {
    let mut width1 = 0usize;
    let mut width2 = 0usize;
    let mut use_second = false;
    for (idx, item) in menu_items.iter().enumerate() {
        let label = format!(" {item} ");
        let label_w = UnicodeWidthStr::width(label.as_str());
        let gap_w = 2usize;
        if !use_second {
            let needed = if width1 == 0 {
                label_w
            } else {
                label_w.saturating_add(gap_w)
            };
            if width1.saturating_add(needed) > max_w && width1 > 0 {
                use_second = true;
            }
        }
        if use_second {
            let start = if width2 == 0 {
                0
            } else {
                width2.saturating_add(gap_w)
            };
            let end = start.saturating_add(label_w);
            if line == 1 {
                let c = rel_col as usize;
                if c >= start && c < end {
                    return Some(idx);
                }
            }
            width2 = end;
        } else {
            let start = if width1 == 0 {
                0
            } else {
                width1.saturating_add(gap_w)
            };
            let end = start.saturating_add(label_w);
            if line == 0 {
                let c = rel_col as usize;
                if c >= start && c < end {
                    return Some(idx);
                }
            }
            width1 = end;
        }
    }
    None
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

#[derive(Debug, Clone)]
struct DrainedSendText {
    ui_text: String,
    model_text: String,
}

fn materialize_placeholders(base: &str, pairs: &[(String, String)]) -> String {
    if pairs.is_empty() || base.is_empty() {
        return base.to_string();
    }
    let mut idx = 0usize;
    let mut out = String::new();
    while idx < base.len() {
        let mut best_pos: Option<usize> = None;
        let mut best_ph: Option<&str> = None;
        let mut best_actual: Option<&str> = None;
        for (ph, actual) in pairs {
            let ph = ph.as_str();
            if ph.is_empty() {
                continue;
            }
            if let Some(rel) = base[idx..].find(ph) {
                let pos = idx + rel;
                let better = match best_pos {
                    None => true,
                    Some(cur) => pos < cur,
                };
                let tie_longer = best_pos.is_some_and(|cur| pos == cur)
                    && best_ph.is_some_and(|cur_ph| ph.len() > cur_ph.len());
                if better || tie_longer {
                    best_pos = Some(pos);
                    best_ph = Some(ph);
                    best_actual = Some(actual.as_str());
                }
            }
        }
        let Some(pos) = best_pos else {
            out.push_str(&base[idx..]);
            break;
        };
        if pos > idx {
            out.push_str(&base[idx..pos]);
        }
        if let (Some(ph), Some(actual)) = (best_ph, best_actual) {
            out.push_str(actual);
            idx = pos.saturating_add(ph.len());
        } else {
            //（1）不应发生：兜底避免死循环
            out.push_str(&base[idx..]);
            break;
        }
    }
    out
}

struct DrainInputForSendArgs<'a> {
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    last_input_at: &'a mut Option<Instant>,
    pending_pastes: &'a mut Vec<(String, String)>,
    paste_capture: &'a mut Option<PasteCapture>,
    command_menu_suppress: &'a mut bool,
    pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
}

fn drain_input_for_send(args: DrainInputForSendArgs<'_>) -> DrainedSendText {
    let DrainInputForSendArgs {
        input,
        cursor,
        input_chars,
        last_input_at,
        pending_pastes,
        paste_capture,
        command_menu_suppress,
        pending_pty_snapshot,
    } = args;
    let base = input.trim_end().to_string();
    let mut ui_text = base.clone();
    let mut placeholders: Vec<(String, String)> = Vec::new();
    let model_text_base = base.clone();
    if !pending_pastes.is_empty() {
        for (i, (ph, actual)) in pending_pastes.iter().enumerate() {
            let id = format!("paste_{}", i.saturating_add(1));
            let chars = count_chars(actual);
            let lines = actual.lines().count().max(1);

            //（1）UI：隐藏原文（展开时可见）
            ui_text.push_str("\n\n");
            ui_text.push_str(&format!(
                "{USER_PASTE_MARKER_BEGIN}id={id}|lines={lines}|chars={chars}|saved=]]\n"
            ));
            ui_text.push_str(actual.trim_end());
            ui_text.push('\n');
            ui_text.push_str(&format!("{USER_PASTE_MARKER_END}id={id}]]"));

            //（1）发给模型：必须是粘贴原文（协议层原文），禁止只发摘要/路径，
            //（2）否则会造成“模型没收到原文”的错觉，并导致后续分析/修复偏离。
            placeholders.push((ph.clone(), actual.clone()));
        }
    }
    if let Some(snap) = pending_pty_snapshot.as_ref()
        && !snap.content.trim().is_empty()
        && !snap.placeholder.trim().is_empty()
        && base.contains(snap.placeholder.as_str())
    {
        let id = format!("pty_{}", snap.job_id);
        let chars = count_chars(&snap.content);
        let lines = snap.content.lines().count().max(1);
        ui_text.push_str("\n\n");
        ui_text.push_str(&format!(
            "{USER_PTY_SNAPSHOT_MARKER_BEGIN}id={id}|job_id={}|lines={lines}|chars={chars}]]\n",
            snap.job_id
        ));
        ui_text.push_str(snap.content.trim_end());
        ui_text.push('\n');
        ui_text.push_str(&format!("{USER_PTY_SNAPSHOT_MARKER_END}id={id}]]"));
        placeholders.push((snap.placeholder.clone(), snap.content.clone()));
    }
    let model_text = if placeholders.is_empty() {
        model_text_base
    } else {
        materialize_placeholders(&model_text_base, &placeholders)
    };
    reset_input_buffer(input, cursor, input_chars, last_input_at);
    pending_pastes.clear();
    *paste_capture = None;
    *command_menu_suppress = false;
    *pending_pty_snapshot = None;
    DrainedSendText {
        ui_text,
        model_text,
    }
}

fn sync_system_toggles(sys_cfg: &SystemConfig, sse_enabled: &mut bool) {
    *sse_enabled = sys_cfg.sse_enabled;
}

struct CommitSettingsInputArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    api_kind: ApiConfigKind,
    buffer: &'a str,
    settings: &'a mut SettingsState,
    now: Instant,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    memory_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    memory_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    memory_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    mind_ctx_idx_memory: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    memory_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
    sse_enabled: &'a mut bool,
}

fn commit_settings_input(args: CommitSettingsInputArgs<'_>) {
    let CommitSettingsInputArgs {
        kind,
        section,
        api_kind,
        buffer,
        settings,
        now,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        memory_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        memory_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        mind_ctx_idx_memory,
        main_prompt_text,
        dog_client,
        main_client,
        memory_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
        sse_enabled,
    } = args;

    apply_settings_with_notice(ApplySettingsWithNoticeArgs {
        kind,
        section,
        api_kind,
        value: buffer,
        settings,
        now,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        memory_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        memory_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        mind_ctx_idx_memory,
        main_prompt_text,
        dog_client,
        main_client,
        memory_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
    });
    sync_system_toggles(sys_cfg, sse_enabled);
    settings.focus = SettingsFocus::Fields;
    reset_settings_edit(settings);
}

fn settings_menu_items() -> Vec<String> {
    vec![
        "1 API配置".to_string(),
        "2 系统配置".to_string(),
        "3 提示词管理".to_string(),
    ]
}

fn settings_section_for_menu(idx: usize) -> SettingsSection {
    match idx {
        0 => SettingsSection::Api,
        1 => SettingsSection::System,
        2 => SettingsSection::PromptCenter,
        _ => SettingsSection::Api,
    }
}

fn settings_section_title(section: SettingsSection) -> &'static str {
    match section {
        SettingsSection::Api => "API配置",
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
            | SettingsFieldKind::PromptFile
    )
}
fn list_prompt_files() -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = Vec::new();
    if let Ok(rd) = std::fs::read_dir("prompt") {
        for ent in rd.flatten() {
            let p = ent.path();
            if p.is_file() {
                out.push(p);
            }
        }
    }
    out.sort_by(|a, b| {
        let an = a.file_name().and_then(|s| s.to_str()).unwrap_or("").to_ascii_lowercase();
        let bn = b.file_name().and_then(|s| s.to_str()).unwrap_or("").to_ascii_lowercase();
        an.cmp(&bn)
    });
    out
}

fn summarize_prompt_path(path: &Path) -> String {
    match std::fs::metadata(path) {
        Ok(m) => {
            let bytes = m.len() as usize;
            if bytes >= 1024 {
                format!("{}KB", (bytes + 1023) / 1024)
            } else {
                format!("{bytes}B")
            }
        }
        Err(_) => "missing".to_string(),
    }
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
    api::provider_label(value)
}

fn normalize_provider(value: &str) -> &str {
    api::normalize_provider(value)
}

fn chat_target_is_codex(
    chat_target: MindKind,
    dog_client: &Option<DogClient>,
    main_client: &Option<DogClient>,
    memory_client: &Option<DogClient>,
) -> bool {
    let provider = match chat_target {
        MindKind::Sub => dog_client
            .as_ref()
            .map(|c| c.provider())
            .unwrap_or("deepseek"),
        MindKind::Main => main_client
            .as_ref()
            .map(|c| c.provider())
            .unwrap_or("deepseek"),
        MindKind::Memory => memory_client
            .as_ref()
            .map(|c| c.provider())
            .unwrap_or("deepseek"),
    };
    normalize_provider(provider) == "codex"
}

fn model_label(value: &str) -> &'static str {
    api::model_label(value)
}

fn available_models_for_provider(provider: &str) -> &'static [&'static str] {
    api::available_models_for_provider(provider)
}

fn default_model_for_provider(provider: &str) -> &'static str {
    api::default_model_for_provider(provider)
}

fn normalize_reasoning_effort(value: Option<&str>) -> Option<&'static str> {
    api::normalize_reasoning_effort(value)
}

fn normalize_reasoning_effort_or_default(value: Option<&str>) -> &'static str {
    api::normalize_reasoning_effort_or_default(value)
}

fn format_reasoning_effort(provider: &str, value: Option<&str>) -> String {
    api::format_reasoning_effort(provider, value)
}

fn format_temp(temp: Option<f32>) -> String {
    match temp {
        Some(t) if (t - 1.0).abs() < 1e-6 => "Default".to_string(),
        Some(t) => format!("{t:.2}"),
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
    memory_cfg: &'a MainApiConfig,
    sys_cfg: &'a SystemConfig,
    dog_prompt_text: &'a str,
    main_prompt_text: &'a str,
    context_prompts: &'a ContextPromptConfig,
}

fn build_settings_fields(args: BuildSettingsFieldsArgs<'_>) -> Vec<SettingsFieldSpec> {
    let BuildSettingsFieldsArgs {
        section,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_prompt_text,
        main_prompt_text,
        context_prompts,
    } = args;
    match section {
        SettingsSection::Api => {
            let mut fields: Vec<SettingsFieldSpec> = Vec::new();
            fields.push(SettingsFieldSpec {
                label: "主 API 配置",
                value: String::new(),
                kind: SettingsFieldKind::Static,
                api_kind: None,
                    prompt_path: None,
                });
            {
                let k = ApiConfigKind::Main;
                let provider_raw = main_cfg.provider.as_str();
                let base_url = main_cfg.base_url.as_str();
                let api_key = main_cfg.api_key.as_deref();
                let model = main_cfg.model.as_str();
                let reasoning_effort = main_cfg.reasoning_effort.as_deref();
                let temperature = main_cfg.temperature;
                let max_tokens = main_cfg.max_tokens;

                let provider = normalize_provider(provider_raw);
                let caps = api::capabilities(provider);
                fields.push(SettingsFieldSpec {
                    label: "Provider",
                    value: provider_label(provider_raw).to_string(),
                    kind: SettingsFieldKind::Provider,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Base URL",
                    value: base_url.to_string(),
                    kind: SettingsFieldKind::BaseUrl,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Key",
                    value: mask_api_key(api_key),
                    kind: SettingsFieldKind::ApiKey,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Model",
                    value: model_label(model).to_string(),
                    kind: SettingsFieldKind::Model,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                if caps.supports_reasoning_effort {
                    fields.push(SettingsFieldSpec {
                        label: "Reasoning",
                        value: format_reasoning_effort(provider_raw, reasoning_effort),
                        kind: SettingsFieldKind::ReasoningEffort,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_temperature {
                    fields.push(SettingsFieldSpec {
                        label: "Temperature",
                        value: format_temp(temperature),
                        kind: SettingsFieldKind::Temperature,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_max_tokens {
                    fields.push(SettingsFieldSpec {
                        label: "MaxToken",
                        value: format_max_tokens(max_tokens),
                        kind: SettingsFieldKind::MaxTokens,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
            }
            fields.push(SettingsFieldSpec {
                label: "分隔线",
                value: String::new(),
                kind: SettingsFieldKind::Static,
                api_kind: None,
                    prompt_path: None,
                });
            fields.push(SettingsFieldSpec {
                label: "辅 API 配置",
                value: String::new(),
                kind: SettingsFieldKind::Static,
                api_kind: None,
                    prompt_path: None,
                });
            {
                let k = ApiConfigKind::Dog;
                let provider_raw = dog_cfg.provider.as_str();
                let base_url = dog_cfg.base_url.as_str();
                let api_key = dog_cfg.api_key.as_deref();
                let model = dog_cfg.model.as_str();
                let reasoning_effort = dog_cfg.reasoning_effort.as_deref();
                let temperature = dog_cfg.temperature;
                let max_tokens = dog_cfg.max_tokens;

                let provider = normalize_provider(provider_raw);
                let caps = api::capabilities(provider);
                fields.push(SettingsFieldSpec {
                    label: "Provider",
                    value: provider_label(provider_raw).to_string(),
                    kind: SettingsFieldKind::Provider,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Base URL",
                    value: base_url.to_string(),
                    kind: SettingsFieldKind::BaseUrl,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Key",
                    value: mask_api_key(api_key),
                    kind: SettingsFieldKind::ApiKey,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Model",
                    value: model_label(model).to_string(),
                    kind: SettingsFieldKind::Model,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                if caps.supports_reasoning_effort {
                    fields.push(SettingsFieldSpec {
                        label: "Reasoning",
                        value: format_reasoning_effort(provider_raw, reasoning_effort),
                        kind: SettingsFieldKind::ReasoningEffort,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_temperature {
                    fields.push(SettingsFieldSpec {
                        label: "Temperature",
                        value: format_temp(temperature),
                        kind: SettingsFieldKind::Temperature,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_max_tokens {
                    fields.push(SettingsFieldSpec {
                        label: "MaxToken",
                        value: format_max_tokens(max_tokens),
                        kind: SettingsFieldKind::MaxTokens,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
            }
            fields.push(SettingsFieldSpec {
                label: "分隔线",
                value: String::new(),
                kind: SettingsFieldKind::Static,
                api_kind: None,
                    prompt_path: None,
                });
            fields.push(SettingsFieldSpec {
                label: "记忆 API 配置",
                value: String::new(),
                kind: SettingsFieldKind::Static,
                api_kind: None,
                    prompt_path: None,
                });
            {
                let k = ApiConfigKind::Memory;
                let provider_raw = memory_cfg.provider.as_str();
                let base_url = memory_cfg.base_url.as_str();
                let api_key = memory_cfg.api_key.as_deref();
                let model = memory_cfg.model.as_str();
                let reasoning_effort = memory_cfg.reasoning_effort.as_deref();
                let temperature = memory_cfg.temperature;
                let max_tokens = memory_cfg.max_tokens;

                let provider = normalize_provider(provider_raw);
                let caps = api::capabilities(provider);
                fields.push(SettingsFieldSpec {
                    label: "Provider",
                    value: provider_label(provider_raw).to_string(),
                    kind: SettingsFieldKind::Provider,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Base URL",
                    value: base_url.to_string(),
                    kind: SettingsFieldKind::BaseUrl,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Key",
                    value: mask_api_key(api_key),
                    kind: SettingsFieldKind::ApiKey,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                fields.push(SettingsFieldSpec {
                    label: "Model",
                    value: model_label(model).to_string(),
                    kind: SettingsFieldKind::Model,
                    api_kind: Some(k),
                    prompt_path: None,
                });
                if caps.supports_reasoning_effort {
                    fields.push(SettingsFieldSpec {
                        label: "Reasoning",
                        value: format_reasoning_effort(provider_raw, reasoning_effort),
                        kind: SettingsFieldKind::ReasoningEffort,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_temperature {
                    fields.push(SettingsFieldSpec {
                        label: "Temperature",
                        value: format_temp(temperature),
                        kind: SettingsFieldKind::Temperature,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
                if caps.supports_max_tokens {
                    fields.push(SettingsFieldSpec {
                        label: "MaxToken",
                        value: format_max_tokens(max_tokens),
                        kind: SettingsFieldKind::MaxTokens,
                        api_kind: Some(k),
                    prompt_path: None,
                });
                }
            }

            fields
        }
        SettingsSection::System => vec![
            SettingsFieldSpec {
                label: "Date diary (KB)",
                value: if sys_cfg.date_kb_limit == 0 {
                    "Off".to_string()
                } else {
                    format!("{}KB", sys_cfg.date_kb_limit)
                },
                kind: SettingsFieldKind::DateKbLimit,
                api_kind: None,
                    prompt_path: None,
                },
            SettingsFieldSpec {
                label: "Heartbeat",
                value: if sys_cfg.heartbeat_minutes == 0 {
                    "Off".to_string()
                } else {
                    format!("{}m", sys_cfg.heartbeat_minutes)
                },
                kind: SettingsFieldKind::HeartbeatMinutes,
                api_kind: None,
                    prompt_path: None,
                },
            SettingsFieldSpec {
                label: "Exec perm",
                value: if sys_cfg.tool_full_access {
                    "Full".to_string()
                } else {
                    "Safe".to_string()
                },
                kind: SettingsFieldKind::ExecPermission,
                api_kind: None,
                    prompt_path: None,
                },
            SettingsFieldSpec {
                label: "SSE",
                value: format_toggle(sys_cfg.sse_enabled),
                kind: SettingsFieldKind::SseEnabled,
                api_kind: None,
                    prompt_path: None,
                },
        ],
        SettingsSection::PromptCenter => {
            let files = list_prompt_files();
            if files.is_empty() {
                vec![SettingsFieldSpec {
                    label: "Prompt",
                    value: "prompt/ 为空".to_string(),
                    kind: SettingsFieldKind::Static,
                    api_kind: None,
                    prompt_path: None,
                }]
            } else {
                let mut out: Vec<SettingsFieldSpec> = Vec::new();
                for (i, p) in files.into_iter().enumerate() {
                    let name = p.file_name().and_then(|s| s.to_str()).unwrap_or("unknown");
                    let value = format!("{:02} {name}  {}", i + 1, summarize_prompt_path(&p));
                    out.push(SettingsFieldSpec {
                        label: "Prompt",
                        value,
                        kind: SettingsFieldKind::PromptFile,
                        api_kind: None,
                        prompt_path: Some(p),
                    });
                }
                out
            }
        },
    }
}

struct FieldRawValueArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    api_kind: ApiConfigKind,
    dog_cfg: &'a DogApiConfig,
    main_cfg: &'a MainApiConfig,
    memory_cfg: &'a MainApiConfig,
    sys_cfg: &'a SystemConfig,
    dog_prompt_text: &'a str,
    main_prompt_text: &'a str,
    context_prompts: &'a ContextPromptConfig,
}

fn field_raw_value(args: FieldRawValueArgs<'_>) -> String {
    let FieldRawValueArgs {
        kind,
        section,
        api_kind,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_prompt_text,
        main_prompt_text,
        context_prompts,
    } = args;
    match kind {
        SettingsFieldKind::Static => String::new(),
        SettingsFieldKind::Provider => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg.provider.clone(),
                ApiConfigKind::Dog => dog_cfg.provider.clone(),
                ApiConfigKind::Memory => memory_cfg.provider.clone(),
            },
            _ => "deepseek".to_string(),
        },
        SettingsFieldKind::BaseUrl => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg.base_url.clone(),
                ApiConfigKind::Dog => dog_cfg.base_url.clone(),
                ApiConfigKind::Memory => memory_cfg.base_url.clone(),
            },
            _ => String::new(),
        },
        SettingsFieldKind::ApiKey => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg.api_key.clone().unwrap_or_default(),
                ApiConfigKind::Dog => dog_cfg.api_key.clone().unwrap_or_default(),
                ApiConfigKind::Memory => memory_cfg.api_key.clone().unwrap_or_default(),
            },
            _ => String::new(),
        },
        SettingsFieldKind::Model => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg.model.clone(),
                ApiConfigKind::Dog => dog_cfg.model.clone(),
                ApiConfigKind::Memory => memory_cfg.model.clone(),
            },
            _ => String::new(),
        },
        SettingsFieldKind::ReasoningEffort => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => {
                    normalize_reasoning_effort_or_default(main_cfg.reasoning_effort.as_deref())
                        .to_string()
                }
                ApiConfigKind::Dog => {
                    normalize_reasoning_effort_or_default(dog_cfg.reasoning_effort.as_deref())
                        .to_string()
                }
                ApiConfigKind::Memory => {
                    normalize_reasoning_effort_or_default(memory_cfg.reasoning_effort.as_deref())
                        .to_string()
                }
            },
            _ => "high".to_string(),
        },
        SettingsFieldKind::Temperature => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg
                    .temperature
                    .map(|t| t.to_string())
                    .unwrap_or_default(),
                ApiConfigKind::Dog => dog_cfg
                    .temperature
                    .map(|t| t.to_string())
                    .unwrap_or_default(),
                ApiConfigKind::Memory => memory_cfg
                    .temperature
                    .map(|t| t.to_string())
                    .unwrap_or_default(),
            },
            _ => String::new(),
        },
        SettingsFieldKind::MaxTokens => match section {
            SettingsSection::Api => match api_kind {
                ApiConfigKind::Main => main_cfg
                    .max_tokens
                    .map(|v| v.to_string())
                    .unwrap_or_default(),
                ApiConfigKind::Dog => dog_cfg
                    .max_tokens
                    .map(|v| v.to_string())
                    .unwrap_or_default(),
                ApiConfigKind::Memory => memory_cfg
                    .max_tokens
                    .map(|v| v.to_string())
                    .unwrap_or_default(),
            },
            _ => String::new(),
        },
        SettingsFieldKind::DogPrompt => dog_prompt_text.to_string(),
        SettingsFieldKind::MainPrompt => main_prompt_text.to_string(),
        SettingsFieldKind::DateKbLimit => sys_cfg.date_kb_limit.to_string(),
        SettingsFieldKind::HeartbeatMinutes => sys_cfg.heartbeat_minutes.to_string(),
        SettingsFieldKind::SseEnabled => {
            if sys_cfg.sse_enabled {
                "on".to_string()
            } else {
                "off".to_string()
            }
        }
        SettingsFieldKind::ExecPermission => {
            if sys_cfg.tool_full_access {
                "full".to_string()
            } else {
                "safe".to_string()
            }
        }
        SettingsFieldKind::ContextMainPrompt => context_prompts.main_prompt.to_string(),
        SettingsFieldKind::PromptFile => String::new(),
    }
}

fn selected_provider(
    api_kind: ApiConfigKind,
    dog_cfg: &DogApiConfig,
    main_cfg: &MainApiConfig,
    memory_cfg: &MainApiConfig,
) -> String {
    match api_kind {
        ApiConfigKind::Dog => normalize_provider(&dog_cfg.provider).to_string(),
        ApiConfigKind::Main => normalize_provider(&main_cfg.provider).to_string(),
        ApiConfigKind::Memory => normalize_provider(&memory_cfg.provider).to_string(),
    }
}

fn selected_model(
    api_kind: ApiConfigKind,
    dog_cfg: &DogApiConfig,
    main_cfg: &MainApiConfig,
    memory_cfg: &MainApiConfig,
) -> String {
    match api_kind {
        ApiConfigKind::Dog => dog_cfg.model.trim().to_string(),
        ApiConfigKind::Main => main_cfg.model.trim().to_string(),
        ApiConfigKind::Memory => memory_cfg.model.trim().to_string(),
    }
}

fn selected_reasoning_effort(
    api_kind: ApiConfigKind,
    dog_cfg: &DogApiConfig,
    main_cfg: &MainApiConfig,
    memory_cfg: &MainApiConfig,
) -> String {
    match api_kind {
        ApiConfigKind::Dog => {
            normalize_reasoning_effort_or_default(dog_cfg.reasoning_effort.as_deref()).to_string()
        }
        ApiConfigKind::Main => {
            normalize_reasoning_effort_or_default(main_cfg.reasoning_effort.as_deref()).to_string()
        }
        ApiConfigKind::Memory => {
            normalize_reasoning_effort_or_default(memory_cfg.reasoning_effort.as_deref())
                .to_string()
        }
    }
}

fn next_provider(current: &str) -> String {
    if normalize_provider(current) == "deepseek" {
        "codex".to_string()
    } else {
        "deepseek".to_string()
    }
}

fn next_model(current: &str, provider: &str) -> String {
    api::next_model(current, provider)
}

fn next_reasoning_effort(current: &str) -> String {
    api::next_reasoning_effort(current)
}

fn provider_profile_ref<'a>(
    profiles: &'a ApiProviderProfiles,
    provider: &str,
) -> &'a ApiProviderProfile {
    if normalize_provider(provider) == "codex" {
        &profiles.codex
    } else {
        &profiles.deepseek
    }
}

fn provider_profile_mut<'a>(
    profiles: &'a mut ApiProviderProfiles,
    provider: &str,
) -> &'a mut ApiProviderProfile {
    if normalize_provider(provider) == "codex" {
        &mut profiles.codex
    } else {
        &mut profiles.deepseek
    }
}

fn save_active_to_profile_dog(cfg: &mut DogApiConfig) {
    let provider = cfg.provider.clone();
    let profile = provider_profile_mut(&mut cfg.provider_profiles, &provider);
    profile.base_url = cfg.base_url.trim().to_string();
    profile.api_key = cfg
        .api_key
        .clone()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    profile.model = cfg.model.trim().to_string();
    profile.reasoning_effort = cfg.reasoning_effort.clone();
    profile.temperature = cfg.temperature;
    profile.max_tokens = cfg.max_tokens;
}

fn save_active_to_profile_main(cfg: &mut MainApiConfig) {
    let provider = cfg.provider.clone();
    let profile = provider_profile_mut(&mut cfg.provider_profiles, &provider);
    profile.base_url = cfg.base_url.trim().to_string();
    profile.api_key = cfg
        .api_key
        .clone()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    profile.model = cfg.model.trim().to_string();
    profile.reasoning_effort = cfg.reasoning_effort.clone();
    profile.temperature = cfg.temperature;
    profile.max_tokens = cfg.max_tokens;
}

fn apply_profile_to_active_dog(cfg: &mut DogApiConfig) {
    let profile = provider_profile_ref(&cfg.provider_profiles, &cfg.provider).clone();
    if !profile.base_url.trim().is_empty() {
        cfg.base_url = profile.base_url;
    }
    cfg.api_key = profile.api_key;
    if !profile.model.trim().is_empty() {
        cfg.model = profile.model;
    }
    cfg.reasoning_effort = Some(
        normalize_reasoning_effort_or_default(profile.reasoning_effort.as_deref()).to_string(),
    );
    if profile.temperature.is_some() {
        cfg.temperature = profile.temperature;
    }
    if profile.max_tokens.is_some() {
        cfg.max_tokens = profile.max_tokens;
    }
    if !available_models_for_provider(&cfg.provider)
        .iter()
        .any(|m| m.eq_ignore_ascii_case(cfg.model.trim()))
    {
        cfg.model = default_model_for_provider(&cfg.provider).to_string();
    }
}

fn apply_profile_to_active_main(cfg: &mut MainApiConfig) {
    let profile = provider_profile_ref(&cfg.provider_profiles, &cfg.provider).clone();
    if !profile.base_url.trim().is_empty() {
        cfg.base_url = profile.base_url;
    }
    cfg.api_key = profile.api_key;
    if !profile.model.trim().is_empty() {
        cfg.model = profile.model;
    }
    cfg.reasoning_effort = Some(
        normalize_reasoning_effort_or_default(profile.reasoning_effort.as_deref()).to_string(),
    );
    if profile.temperature.is_some() {
        cfg.temperature = profile.temperature;
    }
    if profile.max_tokens.is_some() {
        cfg.max_tokens = profile.max_tokens;
    }
    if !available_models_for_provider(&cfg.provider)
        .iter()
        .any(|m| m.eq_ignore_ascii_case(cfg.model.trim()))
    {
        cfg.model = default_model_for_provider(&cfg.provider).to_string();
    }
}

fn hydrate_provider_profiles_dog(cfg: &mut DogApiConfig) {
    cfg.provider = normalize_provider(&cfg.provider).to_string();
    save_active_to_profile_dog(cfg);
    apply_profile_to_active_dog(cfg);
}

fn hydrate_provider_profiles_main(cfg: &mut MainApiConfig) {
    cfg.provider = normalize_provider(&cfg.provider).to_string();
    save_active_to_profile_main(cfg);
    apply_profile_to_active_main(cfg);
}

struct ApplySettingsEditArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    api_kind: ApiConfigKind,
    value: &'a str,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    memory_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    memory_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    _memory_state: &'a mut DogState,
    _mind_ctx_idx_main: &'a mut Option<usize>,
    _mind_ctx_idx_dog: &'a mut Option<usize>,
    _mind_ctx_idx_memory: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    memory_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
}

fn apply_settings_edit(args: ApplySettingsEditArgs<'_>) -> Result<String, String> {
    let ApplySettingsEditArgs {
        kind,
        section,
        api_kind,
        value,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        memory_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        _memory_state: _,
        _mind_ctx_idx_main: _,
        _mind_ctx_idx_dog: _,
        _mind_ctx_idx_memory: _,
        main_prompt_text,
        dog_client,
        main_client,
        memory_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
    } = args;

    let trimmed = value.trim();
    match kind {
        SettingsFieldKind::Static => {}
        SettingsFieldKind::Provider => {
            let next = normalize_provider(if trimmed.is_empty() {
                "deepseek"
            } else {
                trimmed
            })
            .to_string();
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        save_active_to_profile_dog(dog_cfg);
                        dog_cfg.provider = next.clone();
                        apply_profile_to_active_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        save_active_to_profile_main(main_cfg);
                        main_cfg.provider = next.clone();
                        apply_profile_to_active_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        save_active_to_profile_main(memory_cfg);
                        memory_cfg.provider = next.clone();
                        apply_profile_to_active_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::BaseUrl => {
            if trimmed.is_empty() {
                return Err("域名不能为空".to_string());
            }
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.base_url = trimmed.to_string();
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.base_url = trimmed.to_string();
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.base_url = trimmed.to_string();
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::ApiKey => {
            let next = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.api_key = next.clone();
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.api_key = next.clone();
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.api_key = next;
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::Model => {
            if trimmed.is_empty() {
                return Err("模型不能为空".to_string());
            }
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.model = trimmed.to_string();
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.model = trimmed.to_string();
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.model = trimmed.to_string();
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::ReasoningEffort => {
            let next = normalize_reasoning_effort(if trimmed.is_empty() {
                Some("high")
            } else {
                Some(trimmed)
            })
            .ok_or_else(|| "Reasoning 仅支持 low/medium/high/xhigh".to_string())?
            .to_string();
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.reasoning_effort = Some(next.clone());
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.reasoning_effort = Some(next.clone());
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.reasoning_effort = Some(next);
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::Temperature => {
            let next = if trimmed.is_empty() {
                None
            } else {
                let t: f32 = trimmed.parse().map_err(|_| "温度需为数字".to_string())?;
                if !(0.0..=1.5).contains(&t) {
                    return Err("温度范围 0~1.5".to_string());
                }
                if (t - 1.0).abs() < 1e-6 {
                    None
                } else {
                    Some(t)
                }
            };
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.temperature = next;
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.temperature = next;
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.temperature = next;
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
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
            if matches!(section, SettingsSection::Api) {
                match api_kind {
                    ApiConfigKind::Dog => {
                        dog_cfg.max_tokens = Some(v);
                        save_active_to_profile_dog(dog_cfg);
                        store_config_file(dog_cfg_path, dog_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Main => {
                        main_cfg.max_tokens = Some(v);
                        save_active_to_profile_main(main_cfg);
                        store_config_file(main_cfg_path, main_cfg).map_err(|e| e.to_string())?;
                    }
                    ApiConfigKind::Memory => {
                        memory_cfg.max_tokens = Some(v);
                        save_active_to_profile_main(memory_cfg);
                        store_config_file(memory_cfg_path, memory_cfg)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }
        SettingsFieldKind::DateKbLimit => {
            if trimmed.is_empty() {
                return Err("日记阈值不能为空".to_string());
            }
            let raw = trimmed
                .trim_end_matches(['k', 'K', 'b', 'B'])
                .trim_end_matches(['k', 'K']); // 兼容输入 "256KB"/"256k"
            let v: usize = raw.parse().map_err(|_| "日记阈值需为数字".to_string())?;
            if v > 200_000 {
                return Err("日记阈值范围 0~200000KB（0=关闭）".to_string());
            }
            sys_cfg.date_kb_limit = v;
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
                        return Err(format!("执行权限仅支持 safe/full（当前：{other}）"));
                    }
                }
            };
            sys_cfg.tool_full_access = next;
            store_config_file(sys_cfg_path, sys_cfg).map_err(|e| e.to_string())?;
        }
        SettingsFieldKind::DogPrompt => {
            let text = value.to_string();
            store_prompt_file(Path::new(&dog_cfg.prompt_path), &text).map_err(|e| e.to_string())?;
            dog_state.prompt = text.clone();
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
        SettingsFieldKind::PromptFile => {}
    }

    if matches!(section, SettingsSection::Api)
        && matches!(api_kind, ApiConfigKind::Dog)
        && matches!(
            kind,
            SettingsFieldKind::Provider
                | SettingsFieldKind::BaseUrl
                | SettingsFieldKind::ApiKey
                | SettingsFieldKind::Model
                | SettingsFieldKind::ReasoningEffort
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
    if matches!(section, SettingsSection::Api)
        && matches!(api_kind, ApiConfigKind::Main)
        && matches!(
            kind,
            SettingsFieldKind::Provider
                | SettingsFieldKind::BaseUrl
                | SettingsFieldKind::ApiKey
                | SettingsFieldKind::Model
                | SettingsFieldKind::ReasoningEffort
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
    if matches!(section, SettingsSection::Api)
        && matches!(api_kind, ApiConfigKind::Memory)
        && matches!(
            kind,
            SettingsFieldKind::Provider
                | SettingsFieldKind::BaseUrl
                | SettingsFieldKind::ApiKey
                | SettingsFieldKind::Model
                | SettingsFieldKind::ReasoningEffort
                | SettingsFieldKind::Temperature
                | SettingsFieldKind::MaxTokens
        )
    {
        match build_main_client(memory_cfg) {
            Ok(client) => {
                *memory_client = Some(client);
                push_sys_log(sys_log, sys_log_limit, "MEM: 配置已更新");
            }
            Err(e) => {
                *memory_client = None;
                push_sys_log(sys_log, sys_log_limit, format!("MEM: 配置无效 {e}"));
            }
        }
    }

    Ok("已保存".to_string())
}

struct ApplySettingsWithNoticeArgs<'a> {
    kind: SettingsFieldKind,
    section: SettingsSection,
    api_kind: ApiConfigKind,
    value: &'a str,
    settings: &'a mut SettingsState,
    now: Instant,
    dog_cfg: &'a mut DogApiConfig,
    main_cfg: &'a mut MainApiConfig,
    memory_cfg: &'a mut MainApiConfig,
    sys_cfg: &'a mut SystemConfig,
    dog_cfg_path: &'a Path,
    main_cfg_path: &'a Path,
    memory_cfg_path: &'a Path,
    sys_cfg_path: &'a Path,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    memory_state: &'a mut DogState,
    mind_ctx_idx_main: &'a mut Option<usize>,
    mind_ctx_idx_dog: &'a mut Option<usize>,
    mind_ctx_idx_memory: &'a mut Option<usize>,
    main_prompt_text: &'a mut String,
    dog_client: &'a mut Option<DogClient>,
    main_client: &'a mut Option<DogClient>,
    memory_client: &'a mut Option<DogClient>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    context_prompts: &'a mut ContextPromptConfig,
    context_prompt_path: &'a Path,
}

fn apply_settings_with_notice(args: ApplySettingsWithNoticeArgs<'_>) {
    let ApplySettingsWithNoticeArgs {
        kind,
        section,
        api_kind,
        value,
        settings,
        now,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        memory_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        memory_state,
        mind_ctx_idx_main,
        mind_ctx_idx_dog,
        mind_ctx_idx_memory,
        main_prompt_text,
        dog_client,
        main_client,
        memory_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
    } = args;

    match apply_settings_edit(ApplySettingsEditArgs {
        kind,
        section,
        api_kind,
        value,
        dog_cfg,
        main_cfg,
        memory_cfg,
        sys_cfg,
        dog_cfg_path,
        main_cfg_path,
        memory_cfg_path,
        sys_cfg_path,
        dog_state,
        main_state,
        _memory_state: memory_state,
        _mind_ctx_idx_main: mind_ctx_idx_main,
        _mind_ctx_idx_dog: mind_ctx_idx_dog,
        _mind_ctx_idx_memory: mind_ctx_idx_memory,
        main_prompt_text,
        dog_client,
        main_client,
        memory_client,
        sys_log,
        sys_log_limit,
        context_prompts,
        context_prompt_path,
    }) {
        Ok(msg) => set_settings_notice(settings, now, msg),
        Err(e) => set_settings_notice(settings, now, e),
    }
}

fn tool_confirm_prompt(mcp_messages: &McpMessages, reason: &str, call: &ToolCall) -> String {
    crate::messages::render_tool_confirm_prompt(mcp_messages, reason, call)
}

static SEND_QUEUE_SEQ: AtomicU64 = AtomicU64::new(1);

fn next_send_queue_id() -> u64 {
    SEND_QUEUE_SEQ.fetch_add(1, Ordering::Relaxed).max(1)
}

#[derive(Debug, Clone)]
struct QueuedUserMessage {
    id: u64,
    target: MindKind,
    ui_text: String,
    model_text: String,
}

#[derive(Debug, Clone)]
struct InternalUserMessage {
    target: MindKind,
    model_text: String,
    extra_system: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SendQueueUiState {
    Closed,
    Selecting { selected: usize },
    Editing { id: u64 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HelpUiState {
    Closed,
    Selecting { selected: usize },
}

const HELP_KEYMAP_PATH: &str = "config/Documents/keymap.txt";

fn help_menu_items() -> Vec<ui::OwnedMenuItem> {
    vec![ui::OwnedMenuItem {
        left: "快捷键".to_string(),
        right: "发送 keymap".to_string(),
    }]
}

fn send_queue_len(high: &VecDeque<QueuedUserMessage>, low: &VecDeque<QueuedUserMessage>) -> usize {
    high.len().saturating_add(low.len())
}

fn send_queue_get_flat<'a>(
    high: &'a VecDeque<QueuedUserMessage>,
    low: &'a VecDeque<QueuedUserMessage>,
    idx: usize,
) -> Option<&'a QueuedUserMessage> {
    if idx < high.len() {
        return high.get(idx);
    }
    low.get(idx.saturating_sub(high.len()))
}

fn send_queue_find_id(
    high: &VecDeque<QueuedUserMessage>,
    low: &VecDeque<QueuedUserMessage>,
    id: u64,
) -> Option<(bool, usize)> {
    for (i, m) in high.iter().enumerate() {
        if m.id == id {
            return Some((true, i));
        }
    }
    for (i, m) in low.iter().enumerate() {
        if m.id == id {
            return Some((false, i));
        }
    }
    None
}

fn send_queue_update_text_id(
    high: &mut VecDeque<QueuedUserMessage>,
    low: &mut VecDeque<QueuedUserMessage>,
    id: u64,
    text: String,
) -> bool {
    if let Some((is_high, idx)) = send_queue_find_id(high, low, id) {
        if is_high {
            if let Some(m) = high.get_mut(idx) {
                m.ui_text = text.clone();
                m.model_text = text;
                return true;
            }
        } else if let Some(m) = low.get_mut(idx) {
            m.ui_text = text.clone();
            m.model_text = text;
            return true;
        }
    }
    false
}

fn spawn_tool_execution(
    call: ToolCall,
    owner: MindKind,
    tx: mpsc::Sender<AsyncEvent>,
    pty_started_notice_prompt_text: String,
    pty_started_model_note_template: String,
) {
    let _ = tx.send(AsyncEvent::ToolStreamStart {
        owner,
        call: Box::new(call.clone()),
        sys_msg: Some("工具执行中".to_string()),
    });
    if call.tool == "pty" {
        let _ = tx.send(AsyncEvent::PtyToolRequest {
            owner,
            call: Box::new(call),
        });
        return;
    }
	    if call.tool == "bash" && call.interactive.unwrap_or(false) {
	        let _spawn = spawn_interactive_bash_execution(
	            call,
	            owner,
	            tx,
	            pty_started_notice_prompt_text,
	            pty_started_model_note_template,
	            true,
	            false,
	        );
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

#[allow(clippy::too_many_arguments)]
struct StartUserChatRequestArgs<'a> {
    now: Instant,
    ui_text: String,
    model_text: String,
    target: MindKind,
    extra_system_override: Option<String>,
    core: &'a mut Core,
    metamemo: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    dog_state: &'a mut DogState,
    main_state: &'a mut DogState,
    memory_state: &'a mut DogState,
    sys_cfg: &'a SystemConfig,
    config: &'a AppConfig,
    tx: &'a mpsc::Sender<AsyncEvent>,
    mode: &'a mut Mode,
    active_kind: &'a mut MindKind,
    active_request_is_internal_placeholder: &'a mut bool,
    sending_until: &'a mut Option<Instant>,
    sys_log: &'a mut VecDeque<String>,
    streaming_state: &'a mut StreamingState,
    request_seq: &'a mut u64,
    active_request_id: &'a mut Option<u64>,
    active_cancel: &'a mut Option<Arc<AtomicBool>>,
    sse_enabled: bool,
    next_heartbeat_at: &'a mut Instant,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
    token_totals: &'a mut TokenTotals,
    token_total_path: &'a Path,
    active_request_in_tokens: &'a mut Option<u64>,
    dog_client: &'a Option<DogClient>,
    main_client: &'a Option<DogClient>,
    memory_client: &'a Option<DogClient>,
}

// ===== 请求生命周期（用户输入 -> 上下文快照 -> provider 请求 -> 流式事件）=====
fn start_user_chat_request(args: StartUserChatRequestArgs<'_>) -> anyhow::Result<()> {
    let StartUserChatRequestArgs {
        now,
        ui_text,
        model_text,
        target,
        extra_system_override,
        core,
        metamemo,
        context_usage,
        dog_state,
        main_state,
        memory_state,
        sys_cfg,
        config,
        tx,
        mode,
        active_kind,
        active_request_is_internal_placeholder,
        sending_until,
        sys_log,
        streaming_state,
        request_seq,
        active_request_id,
        active_cancel,
        sse_enabled,
        next_heartbeat_at,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_started_at,
        expanded_tool_idxs: _expanded_tool_idxs,
        expanded_thinking_idxs: _expanded_thinking_idxs,
        scroll,
        follow_bottom,
        token_totals,
        token_total_path,
        active_request_in_tokens,
        dog_client,
        main_client,
        memory_client,
    } = args;

    let ui_text = ui_text.trim_end().to_string();
    let model_text = model_text.trim_end().to_string();
    if model_text.trim().is_empty() {
        return Ok(());
    }
    //（1）Memory 测试入口：输入 test 直接填充 Date 计数到阈值。
    //（2）验证：计数 -> 写日记 -> 写入 datememo -> 清空 contextmemo。
    //（3）写入带 [TEST-FILL] 标记；datesummary 会要求忽略。
    //（4）本次不发起对话请求，只触发写日记条件。
    if target == MindKind::Memory && model_text.trim().eq_ignore_ascii_case("test") {
        let kb_limit = sys_cfg.date_kb_limit;
        if kb_limit > 0 {
            fill_contextmemo_to_kb_limit_for_test(
                &config.contextmemo_path,
                context_usage,
                kb_limit,
            );
        }
        core.history.push(Message {
            role: Role::System,
            text: "··· 已触发日记测试：Date 计数已填充到阈值（空闲时将启动写日记）".to_string(),
            mind: Some(MindKind::Memory),
        });
        *scroll = u16::MAX;
        *follow_bottom = true;
        return Ok(());
    }
    //（1）DeepSeek role-needed 内部占位符（协议需要）。
    //（2）UI 不显示（ui_text 为空）。
    //（3）不得写入长期上下文（DogState/memo）。
    //（4）尾占位由 provider 在请求期按需追加。
    let internal_role_needed_placeholder =
        ui_text.trim().is_empty() && crate::context::is_role_needed_user_placeholder(&model_text);
    *active_request_is_internal_placeholder = internal_role_needed_placeholder;

    //（1）用户输入：写入对应 Tab 的聊天流（Main/Dog/Memory 完全隔离显示）。
    //（2）ui_text 为空表示内部占位符：UI 不显示。
    if !ui_text.trim().is_empty() {
        core.history.push(Message {
            role: Role::User,
            text: ui_text.clone(),
            mind: Some(target),
        });
    }
    thinking_text.clear();
    thinking_full_text.clear();
    *thinking_started_at = None;
    *thinking_pending_idle = false;
    //（1）占位由 ModelStreamStart 决定（think/brief/working）。
    //（2）避免预判 provider 导致双占位或状态不一致。
    //（3）减少新增 provider 时的漏改风险。
    *thinking_in_progress = false;
    *thinking_idx = None;

    if !internal_role_needed_placeholder {
        let user_agent = if matches!(target, MindKind::Main) {
            Some("main")
        } else {
            None
        };
        log_memos(metamemo, context_usage, "user", user_agent, &model_text);
        if matches!(target, MindKind::Main) {
            log_contextmemo(&config.contextmemo_path, context_usage, "user", &model_text);
        }
        let mind = match target {
            MindKind::Main => "main",
            MindKind::Sub => "dog",
            MindKind::Memory => "memory",
        };
        let p = context_path_for_mind(config, mind);
        log_dyncontext(p, mind, "user", &model_text, None, None);
    }

    defer_heartbeat(next_heartbeat_at, now);
    //（1）保留用户展开状态：发送新消息不应强制把 Tab/Shift+Tab 的展开形态重置。
    *scroll = u16::MAX;
    *follow_bottom = true;

    *active_kind = target;

    if sys_cfg.fastmemo_inject_enabled {
        let fastmemo = read_fastmemo_for_context();
        match target {
            MindKind::Main => main_state.refresh_fastmemo_system(&fastmemo),
            MindKind::Sub => dog_state.refresh_fastmemo_system(&fastmemo),
            MindKind::Memory => memory_state.refresh_fastmemo_system(&fastmemo),
        }
    }

    let extra_system = extra_system_override;

    let started = match target {
        MindKind::Main => try_start_main_generation(TryStartMainGenerationArgs {
            main_client,
            main_state,
            extra_system: extra_system.clone(),
            tx,
            config,
            mode,
            active_kind,
            sending_until,
            sys_log,
            sys_log_limit: config.sys_log_limit,
            streaming_state,
            request_seq,
            active_request_id,
            active_cancel,
            sse_enabled,
        }),
        MindKind::Sub => try_start_dog_generation(TryStartDogGenerationArgs {
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
            sys_log_limit: config.sys_log_limit,
            streaming_state,
            request_seq,
            active_request_id,
            active_cancel,
            sse_enabled,
        }),
        MindKind::Memory => try_start_memory_generation(TryStartMemoryGenerationArgs {
            memory_client,
            memory_state,
            extra_system,
            tx,
            config,
            mode,
            active_kind,
            sending_until,
            sys_log,
            sys_log_limit: config.sys_log_limit,
            streaming_state,
            request_seq,
            active_request_id,
            active_cancel,
            sse_enabled,
        }),
    };
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
        *mode = Mode::Idle;
        let label = mind_label(target);
        let msg = format!("{label} 未配置 API Key，无法请求。");
        push_system_and_log(core, metamemo, context_usage, None, &msg);
        push_sys_log(
            sys_log,
            config.sys_log_limit,
            format!("{label}: 未配置 API Key"),
        );
    }
    Ok(())
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
    mcp_messages: &'a McpMessages,
    pty_messages: &'a PtyMessages,
    pty_started_notice_prompt_text: &'a str,
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
        mcp_messages,
        pty_messages,
        pty_started_notice_prompt_text,
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
    if !sys_cfg.tool_full_access
        && let Some(reason) = tool_requires_confirmation(&call)
    {
        let prompt = tool_confirm_prompt(mcp_messages, &reason, &call);
        let msg_idx = core.history.len();
        push_system_and_log(core, meta, context_usage, Some("tool"), &prompt);
        *pending_tool_confirm = Some((call, reason, msg_idx));
        *mode = Mode::ApprovingTool;
        push_sys_log(sys_log, sys_log_limit, "等待工具确认");
        return true;
    }
    spawn_tool_execution(
        call,
        owner,
        tx,
        pty_started_notice_prompt_text.to_string(),
        pty_messages.pty_started_model_note.clone(),
    );
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

fn is_idle_like(mode: Mode) -> bool {
    matches!(mode, Mode::Idle)
}

fn is_idx_expanded(
    global: bool,
    expanded: &BTreeSet<usize>,
    collapsed: &BTreeSet<usize>,
    idx: usize,
) -> bool {
    if collapsed.contains(&idx) {
        return false;
    }
    if expanded.contains(&idx) {
        return true;
    }
    global
}

fn toggle_idx_expanded(
    global: bool,
    expanded: &mut BTreeSet<usize>,
    collapsed: &mut BTreeSet<usize>,
    idx: usize,
) -> bool {
    let cur = is_idx_expanded(global, expanded, collapsed, idx);
    if cur {
        expanded.remove(&idx);
        if global {
            collapsed.insert(idx);
        } else {
            collapsed.remove(&idx);
        }
        return false;
    }
    collapsed.remove(&idx);
    if !global {
        expanded.insert(idx);
    } else {
        expanded.remove(&idx);
    }
    true
}

fn build_tool_stream_message(state: &ToolStreamState) -> String {
    //（1）工具消息在聊天区“可观察/可收敛”：
    //（2）执行中：展示可读摘要（包含 explain/input），output 用占位符
    //（3）执行完：展示截断后的 output/meta（由 UI 决定是否展开/收敛）
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
    thinking_global_expanded: bool,
    expanded_thinking_idxs: &BTreeSet<usize>,
    collapsed_thinking_idxs: &BTreeSet<usize>,
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
        let is_think = is_think_tool_message(msg);
        let is_brief = is_brief_tool_message(msg);
        if is_think {
            let allow_think = is_idx_expanded(
                thinking_global_expanded,
                expanded_thinking_idxs,
                collapsed_thinking_idxs,
                msg_idx,
            );
            if !details_mode && !allow_think && thinking_idx != Some(msg_idx) {
                return false;
            }
        }
        if is_brief && msg.text.contains("provider:codex") {
            //（1）Codex 的 brief 只用于顶栏状态。
            //（2）聊天区不渲染，也不进入可选中消息。
            return false;
        }
    }
    true
}

fn is_think_tool_message(msg: &Message) -> bool {
    if msg.role != Role::Tool {
        return false;
    }
    let head = msg.text.trim_start();
    let prefix = head.get(..THINK_TOOL_MARKER.len()).unwrap_or("");
    prefix.eq_ignore_ascii_case(THINK_TOOL_MARKER)
}

fn is_brief_tool_message(msg: &Message) -> bool {
    if msg.role != Role::Tool {
        return false;
    }
    let head = msg.text.trim_start();
    let prefix = head.get(..BRIEF_TOOL_MARKER.len()).unwrap_or("");
    prefix.eq_ignore_ascii_case(BRIEF_TOOL_MARKER)
}

fn find_think_message_for_selection(core: &Core, selected_idx: usize) -> Option<usize> {
    let selected = core.history.get(selected_idx)?;
    if is_think_tool_message(selected) {
        return Some(selected_idx);
    }
    //（1）Codex：选中 brief 时，think 常在其后。
    //（2）同轮内允许向下关联一次。
    if is_brief_tool_message(selected) && selected.text.contains("provider:codex") {
        for idx in selected_idx.saturating_add(1)..core.history.len() {
            let msg = core.history.get(idx)?;
            if msg.role == Role::User {
                break;
            }
            if is_think_tool_message(msg) {
                return Some(idx);
            }
        }
        return None;
    }
    //（1）仅 assistant/tool 允许向上关联 think。
    //（2）tool 回执可跨过同轮 assistant。
    //（3）user/system 不参与关联，避免跨轮误判。
    if !matches!(selected.role, Role::Assistant | Role::Tool) {
        return None;
    }
    //（1）同轮内：优先向上找最近的 think（通常 think 出现在 tool 回执之前）。
    for idx in (0..selected_idx).rev() {
        let msg = core.history.get(idx)?;
        if is_think_tool_message(msg) {
            return Some(idx);
        }
        //（1）以“上一条 user”为同轮边界：assistant/tool/system 都可能穿插在同轮中。
        if msg.role == Role::User {
            break;
        }
    }
    //（1）兜底：若本轮 think 出现在其后（例如多工具回执先落盘、think 后到），向下关联一次。
    for idx in selected_idx.saturating_add(1)..core.history.len() {
        let msg = core.history.get(idx)?;
        if msg.role == Role::User {
            break;
        }
        if is_think_tool_message(msg) {
            return Some(idx);
        }
    }
    None
}

fn find_assistant_for_think_message(core: &Core, think_idx: usize) -> Option<usize> {
    if core
        .history
        .get(think_idx)
        .is_none_or(|m| !is_think_tool_message(m))
    {
        return None;
    }
    for idx in think_idx.saturating_add(1)..core.history.len() {
        let msg = core.history.get(idx)?;
        //（1）同轮：assistant/tool 交替；think 在正文前/中。
        if msg.role == Role::Assistant {
            return Some(idx);
        }
        if msg.role == Role::User {
            break;
        }
    }
    None
}

fn find_tool_message_for_assistant(core: &Core, assistant_idx: usize) -> Option<usize> {
    if core
        .history
        .get(assistant_idx)
        .is_none_or(|m| m.role != Role::Assistant)
    {
        return None;
    }
    for idx in assistant_idx.saturating_add(1)..core.history.len() {
        let msg = core.history.get(idx)?;
        if msg.role == Role::Tool && !is_think_tool_message(msg) && !is_brief_tool_message(msg) {
            return Some(idx);
        }
        if matches!(msg.role, Role::User | Role::Assistant) {
            break;
        }
    }
    None
}

fn related_think_and_tool_for_selection(
    core: &Core,
    selected_idx: usize,
) -> (Option<usize>, Option<usize>) {
    let msg = core.history.get(selected_idx);
    let Some(msg) = msg else {
        return (None, None);
    };

    //（1）think：选中 think 工具本身则为该条，否则按“同一轮向上找”。
    let think_idx = if is_think_tool_message(msg) {
        Some(selected_idx)
    } else {
        find_think_message_for_selection(core, selected_idx)
    };

    //（1）tool：选中 assistant/think 时取同轮后续 tool 回执。
    let tool_idx =
        if msg.role == Role::Tool && !is_think_tool_message(msg) && !is_brief_tool_message(msg) {
            Some(selected_idx)
        } else {
            let assistant_idx = if msg.role == Role::Assistant {
                Some(selected_idx)
            } else if is_think_tool_message(msg) {
                find_assistant_for_think_message(core, selected_idx)
            } else if is_brief_tool_message(msg) {
                find_think_message_for_selection(core, selected_idx)
                    .and_then(|ti| find_assistant_for_think_message(core, ti))
            } else {
                None
            };
            assistant_idx.and_then(|ai| find_tool_message_for_assistant(core, ai))
        };

    (think_idx, tool_idx)
}

fn select_last_chat_message(
    core: &Core,
    thinking_idx: Option<usize>,
    thinking_global_expanded: bool,
    expanded_thinking_idxs: &BTreeSet<usize>,
    collapsed_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    for (idx, msg) in core.history.iter().enumerate().rev() {
        if is_chat_message_selectable(
            idx,
            msg,
            thinking_idx,
            thinking_global_expanded,
            expanded_thinking_idxs,
            collapsed_thinking_idxs,
            details_mode,
        ) {
            return Some(idx);
        }
    }
    None
}

fn select_prev_chat_message(
    core: &Core,
    cur: Option<usize>,
    thinking_idx: Option<usize>,
    thinking_global_expanded: bool,
    expanded_thinking_idxs: &BTreeSet<usize>,
    collapsed_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    let start = cur.unwrap_or(core.history.len());
    for idx in (0..start).rev() {
        if let Some(msg) = core.history.get(idx)
            && is_chat_message_selectable(
                idx,
                msg,
                thinking_idx,
                thinking_global_expanded,
                expanded_thinking_idxs,
                collapsed_thinking_idxs,
                details_mode,
            )
        {
            return Some(idx);
        }
    }
    cur.or_else(|| {
        select_last_chat_message(
            core,
            thinking_idx,
            thinking_global_expanded,
            expanded_thinking_idxs,
            collapsed_thinking_idxs,
            details_mode,
        )
    })
}

fn select_next_chat_message(
    core: &Core,
    cur: Option<usize>,
    thinking_idx: Option<usize>,
    thinking_global_expanded: bool,
    expanded_thinking_idxs: &BTreeSet<usize>,
    collapsed_thinking_idxs: &BTreeSet<usize>,
    details_mode: bool,
) -> Option<usize> {
    if core.history.is_empty() {
        return None;
    }
    let start = cur.map(|v| v.saturating_add(1)).unwrap_or(0);
    for idx in start..core.history.len() {
        if let Some(msg) = core.history.get(idx)
            && is_chat_message_selectable(
                idx,
                msg,
                thinking_idx,
                thinking_global_expanded,
                expanded_thinking_idxs,
                collapsed_thinking_idxs,
                details_mode,
            )
        {
            return Some(idx);
        }
    }
    cur.or_else(|| {
        select_last_chat_message(
            core,
            thinking_idx,
            thinking_global_expanded,
            expanded_thinking_idxs,
            collapsed_thinking_idxs,
            details_mode,
        )
    })
}

#[derive(Debug, Clone, Copy)]
struct ScrollAnchor {
    msg_idx: usize,
    //（1）期望保持 `scroll - msg_start` 不变（防止展开/收起导致视图漂移）。
    view_top_minus_msg_start: i32,
}

fn capture_scroll_anchor(
    scroll: u16,
    max_scroll_cache: usize,
    selected_msg_idx: Option<usize>,
    msg_line_ranges: &[Option<(usize, usize)>],
) -> Option<ScrollAnchor> {
    let view_top = (scroll as usize).min(max_scroll_cache) as i32;
    if let Some(sel) = selected_msg_idx
        && let Some((start, _)) = msg_line_ranges.get(sel).copied().flatten()
    {
        return Some(ScrollAnchor {
            msg_idx: sel,
            view_top_minus_msg_start: view_top - start as i32,
        });
    }
    let line = view_top.max(0) as usize;
    for (msg_idx, range) in msg_line_ranges.iter().enumerate() {
        if let Some((start, end)) = range
            && *start <= line
            && line < *end
        {
            return Some(ScrollAnchor {
                msg_idx,
                view_top_minus_msg_start: view_top - *start as i32,
            });
        }
    }
    None
}

fn apply_scroll_anchor(
    scroll: &mut u16,
    follow_bottom: &mut bool,
    anchor: ScrollAnchor,
    msg_line_ranges: &[Option<(usize, usize)>],
    max_scroll_cache: usize,
) -> bool {
    let Some((start, _)) = msg_line_ranges.get(anchor.msg_idx).copied().flatten() else {
        return false;
    };
    let mut next = start as i32 + anchor.view_top_minus_msg_start;
    if next < 0 {
        next = 0;
    }
    let max = max_scroll_cache as i32;
    if next > max {
        next = max;
    }
    *scroll = next as u16;
    *follow_bottom = (*scroll as usize) >= max_scroll_cache;
    true
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
    //（1）若把 assistant 前缀与 tool stub 拆成两条消息。
    //（2）此字段指向流式结束时写回清洗文本的 assistant 索引。
    assistant_idx_before_tool: Option<usize>,
    raw_text: String,
    text: String,
    has_content: bool,
    tool_start: Option<usize>,
    //（1）标记当前流式是否来自 Codex。
    //（2）用于启用 Codex 专属展示策略。
    codex_mode: bool,
    //（1）Codex summary(brief) 已出现。
    //（2）尚未在 reasoning/正文前插入分隔换行。
    codex_summary_pending_sep: bool,
}

impl StreamingState {
    fn reset(&mut self) {
        self.idx = None;
        self.assistant_idx_before_tool = None;
        self.raw_text.clear();
        self.text.clear();
        self.has_content = false;
        self.tool_start = None;
        self.codex_mode = false;
        self.codex_summary_pending_sep = false;
    }

    fn start(&mut self, idx: usize) {
        self.idx = Some(idx);
        self.assistant_idx_before_tool = None;
        self.raw_text.clear();
        self.text.clear();
        self.has_content = false;
        self.tool_start = None;
        self.codex_mode = false;
        self.codex_summary_pending_sep = false;
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
        if let Some(value) = self.assistant_idx_before_tool {
            if value < removed {
                self.assistant_idx_before_tool = None;
            } else {
                self.assistant_idx_before_tool = Some(value.saturating_sub(removed));
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

const THINK_OUTPUT_MAX_LINES: usize = 1200;
const THINK_OUTPUT_MAX_CHARS: usize = 80_000;

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
        MindKind::Memory => String::new(),
    }
}

fn build_brief_tool_message(kind: MindKind, brief: &str, status: &str) -> String {
    let mut msg = String::new();
    msg.push_str(BRIEF_TOOL_MARKER);
    msg.push('\n');
    let brief = compact_ws_inline(brief).trim().to_string();
    if brief.is_empty() {
        msg.push_str("brief:\n");
    } else {
        msg.push_str(&format!("brief: {brief}\n"));
    }
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    msg.push_str(&format!("mind: {mind}\n"));
    msg.push_str("meta:\n```text\n");
    msg.push_str(status.trim());
    msg.push_str("\n```\n");
    msg
}

fn extract_last_bold_md(text: &str) -> Option<String> {
    //（1）提取最后一个 `**...**` 的第一行（用于 Codex brief/status header）。
    //（2）Codex 的 summary 可能包含多个粗体标题（brief 迭代更新），应以“最新”为准。
    let text = text.trim();
    if !text.contains("**") {
        return None;
    }
    let mut pos = 0usize;
    let mut last: Option<String> = None;
    while let Some(open_rel) = text[pos..].find("**") {
        let open = pos + open_rel;
        let rest = &text[open + 2..];
        let Some(close_rel) = rest.find("**") else {
            break;
        };
        let close = open + 2 + close_rel;
        let inner = rest[..close_rel].trim();
        if !inner.is_empty() {
            let first_line = inner.lines().next().unwrap_or(inner).trim();
            if !first_line.is_empty() {
                last = Some(first_line.to_string());
            }
        }
        pos = (close + 2).min(text.len());
    }
    last
}

fn first_non_empty_line(text: &str) -> Option<&str> {
    for line in text.lines() {
        let t = line.trim();
        if !t.is_empty() {
            return Some(t);
        }
    }
    None
}

fn status_header_from_brief_text(brief_text: &str) -> Option<String> {
    //（1）状态栏只从粗体标题提取 brief header：避免把 thinking/正文误判为 brief。
    let raw = extract_last_bold_md(brief_text)?;
    let clean = compact_ws_inline(&raw.replace("**", ""));
    let header = truncate_with_suffix(clean.trim(), 160);
    (!header.trim().is_empty()).then_some(header)
}

fn build_brief_tool_message_stub(kind: MindKind, status: &str) -> String {
    build_brief_tool_message(kind, "", &format!("状态:{status}"))
}

fn build_brief_tool_message_running(kind: MindKind, brief: &str) -> String {
    build_brief_tool_message(kind, brief, "状态:running")
}

fn build_brief_tool_message_done(kind: MindKind, brief: &str) -> String {
    let chars = brief.chars().count();
    build_brief_tool_message(kind, brief, &format!("状态:0 | chars:{chars}"))
}

fn build_codex_brief_tool_message_stub(kind: MindKind) -> String {
    build_codex_brief_tool_message(kind, "", "状态:running\nprovider:codex")
}

fn build_codex_brief_tool_message_running(kind: MindKind, brief: &str) -> String {
    build_codex_brief_tool_message(kind, brief, "状态:running\nprovider:codex")
}

fn build_codex_brief_tool_message_done(kind: MindKind, brief: &str) -> String {
    let chars = brief.chars().count();
    build_codex_brief_tool_message(
        kind,
        brief,
        &format!("状态:0 | chars:{chars}\nprovider:codex"),
    )
}

fn build_codex_brief_tool_message(kind: MindKind, full_text: &str, status: &str) -> String {
    let mut msg = String::new();
    msg.push_str(BRIEF_TOOL_MARKER);
    msg.push('\n');
    let header = extract_last_bold_md(full_text)
        .or_else(|| first_non_empty_line(full_text).map(|s| s.to_string()))
        .unwrap_or_default();
    let header = compact_ws_inline(&header.replace("**", ""));
    let header_md = if header.trim().is_empty() {
        String::new()
    } else {
        //（1）在 tool 文本中保留 markdown：UI 渲染时可把它当作“brief 标题”上色。
        format!("**{}**", truncate_with_suffix(header.trim(), 160))
    };
    if header_md.trim().is_empty() {
        msg.push_str("brief:\n");
    } else {
        msg.push_str(&format!("brief: {header_md}\n"));
    }
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    msg.push_str(&format!("mind: {mind}\n"));
    msg.push_str("meta:\n```text\n");
    msg.push_str(status.trim());
    msg.push_str("\n```\n");
    msg
}

fn normalize_codex_summary_fragment(fragment: &str) -> String {
    //（1）Codex 的 summary 往往是相邻的粗体片段：`**A****B****C**`；
    //（2）这里插入空格，避免状态栏/思考区变成一坨。
    fragment.replace("****", "** **")
}

fn build_codex_thinking_tool_message_stub(kind: MindKind) -> String {
    //（1）Codex：input 留空；收到 reasoning 后再显示。
    build_thinking_tool_message_stub(kind, "running\nprovider:codex", "")
}

fn build_codex_thinking_tool_message_running(kind: MindKind, full_text: &str) -> String {
    let clean = full_text.trim();
    if clean.is_empty() {
        return build_codex_thinking_tool_message_stub(kind);
    }
    //（1）在 meta 区补一个 provider 标记，便于 UI 区分展示策略（不影响模型上下文）。
    let mut msg = build_thinking_tool_message_running(kind, clean);
    if let Some(pos) = msg.rfind("状态:running") {
        msg.insert_str(pos + "状态:running".len(), "\nprovider:codex");
    }
    msg
}

fn build_codex_thinking_tool_message_done(
    kind: MindKind,
    full_text: &str,
    secs: u64,
    chars: usize,
) -> String {
    let clean = full_text.trim();
    if clean.is_empty() {
        //（1）空思考：返回一个带 provider 标记的空 stub，UI 会忽略它。
        return build_thinking_tool_message_stub(kind, "0\nprovider:codex", "Thinking");
    }
    let mut msg = build_thinking_tool_message("", clean, secs, chars, kind);
    if let Some(pos) = msg.rfind("meta:\n```text\n") {
        msg.insert_str(pos + "meta:\n```text\n".len(), "provider:codex\n");
    }
    msg
}

fn build_thinking_tool_message_stub(kind: MindKind, status: &str, input: &str) -> String {
    let mut msg = String::new();
    msg.push_str(THINK_TOOL_MARKER);
    msg.push('\n');
    msg.push_str(&format!("input: {}\n", input.trim()));
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else if matches!(kind, MindKind::Memory) {
        "memory"
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
格式：<tool>{\"tool\":\"memory_add\",\"path\":\"datememo\",\"content\":\"YYYY-MM-DD HH:MM:SS | memory | 关键词: ... | 日记: ...\",\"brief\":\"我来更新日记。\"}</tool>";
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

fn fill_contextmemo_to_kb_limit_for_test(path: &str, usage: &mut ContextUsage, kb_limit: usize) {
    if kb_limit == 0 {
        return;
    }
    //（1）开放但有上限：只用于测试把 Date 计数打满，不要把上下文写爆到不可控。
    //（2）每次写入约 8KB 的 filler（包含明确标记，便于日记提示词忽略）。
    const CHUNK_BYTES: usize = 8 * 1024;
    const MAX_WRITES: usize = 64;
    let mut writes = 0usize;
    while contextmemo_size_kb(path) < kb_limit && writes < MAX_WRITES {
        let mut filler = String::new();
        filler.push_str("[TEST-FILL] ");
        filler.push_str("x".repeat(CHUNK_BYTES.saturating_sub(12)).as_str());
        log_contextmemo(path, usage, "user", &filler);
        writes = writes.saturating_add(1);
    }
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
    msg.push_str(THINK_TOOL_MARKER);
    msg.push('\n');
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
    msg.push_str(THINK_TOOL_MARKER);
    msg.push('\n');
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
    collapsed_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_user_idxs: &'a mut BTreeSet<usize>,
    collapsed_user_idxs: &'a mut BTreeSet<usize>,
    thinking_idx: &'a mut Option<usize>,
    brief_idx: &'a mut Option<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    collapsed_thinking_idxs: &'a mut BTreeSet<usize>,
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        thinking_idx,
        brief_idx,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
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
    pull_index_set(collapsed_tool_idxs, idx);
    pull_index_set(expanded_user_idxs, idx);
    pull_index_set(collapsed_user_idxs, idx);
    pull_index(thinking_idx, idx);
    pull_index(brief_idx, idx);
    pull_index_set(expanded_thinking_idxs, idx);
    pull_index_set(collapsed_thinking_idxs, idx);
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

struct ClearBriefStateArgs<'a> {
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
}

fn clear_brief_state(args: ClearBriefStateArgs<'_>) {
    let ClearBriefStateArgs {
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
    } = args;
    brief_text.clear();
    *brief_idx = None;
    *brief_in_progress = false;
    *brief_pending_idle = false;
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
    collapsed_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_user_idxs: &'a mut BTreeSet<usize>,
    collapsed_user_idxs: &'a mut BTreeSet<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    collapsed_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview_chat_idx: &'a mut Option<usize>,
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
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
    collapsed_tool_idxs.clear();
    expanded_user_idxs.clear();
    collapsed_user_idxs.clear();
    expanded_thinking_idxs.clear();
    collapsed_thinking_idxs.clear();
    clear_brief_state(ClearBriefStateArgs {
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
    });
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        thinking_idx,
        brief_idx,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
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
        expect_brief: bool,
        request_id: u64,
    },
    ModelStreamChunk {
        content: String,
        reasoning: String,
        brief: String,
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
        owner: MindKind,
        user_initiated: bool,
        job_id: u64,
        cmd: String,
        saved_path: String,
        status_path: String,
    },
    PtyOutput {
        job_id: u64,
        bytes: Vec<u8>,
    },
    PtySpawned {
        job_id: u64,
        pid: Option<i32>,
        pgrp: Option<i32>,
    },
    PtyJobDone {
        owner: MindKind,
        user_initiated: bool,
        job_id: u64,
        cmd: String,
        saved_path: String,
        status_path: String,
        exit_code: i32,
        timed_out: bool,
        user_exit: bool,
        elapsed_ms: u128,
        bytes: usize,
        lines: usize,
    },
    PtyToolRequest {
        owner: MindKind,
        call: Box<ToolCall>,
    },
}

fn arm_parent_death_signal() {
    //（1）Android/Termux 是 Linux 内核；target_os 为 android。
    //（2）若不设置 PDEATHSIG，父进程（zsh/login）退出后，TUI 可能变成孤儿进程并继续烧 CPU。
    #[cfg(any(target_os = "linux", target_os = "android"))]
    unsafe {
        libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGTERM);
    }
    //（1）竞态兜底：如果父进程已在 prctl 前退出，PDEATHSIG 不会再触发，这里主动退出。
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
    //（1）兜底：某些环境 /dev/tty 可能不可用，但至少要确保 stdio 是终端。
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
    let tip1 = "提示：这是用户手动打开的终端窗口";
    let tip2 = "该窗口由用户自己执行命令与退出，非 AI 自动化任务";
    let tip3 = "tips：输入 /AI 回到主面板";
    let _ = writeln!(out, "\n{}{}", " ".repeat(left_pad), tip1);
    let _ = writeln!(out, "{}{}", " ".repeat(left_pad), tip2);
    let _ = writeln!(out, "{}{}", " ".repeat(left_pad), tip3);
    let _ = writeln!(out);
    let _ = out.flush();
}

fn run_native_shell(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> anyhow::Result<()> {
    disable_raw_mode().ok();
    execute!(
        terminal.backend_mut(),
        DisableBracketedPaste,
        DisableMouseCapture,
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
    trace_startup("run: raw_mode ok");
    execute!(
        terminal.backend_mut(),
        EnterAlternateScreen,
        EnableBracketedPaste,
        EnableMouseCapture
    )
    .ok();
    terminal.clear().ok();

    match status {
        Ok(_) => Ok(()),
        Err(e) => Err(anyhow::anyhow!("启动 shell 失败：{e}")),
    }
}

fn recover_terminal_best_effort() {
    crate::test::recover_terminal_best_effort();
}

fn trace_startup(msg: &str) {
    crate::test::trace_startup(msg);
}

fn install_crash_hook() {
    crate::test::install_crash_hook();
}

struct UiGuard;

impl Drop for UiGuard {
    fn drop(&mut self) {
        recover_terminal_best_effort();
    }
}

pub fn run() -> anyhow::Result<i32> {
    trace_startup("run: enter");
    arm_parent_death_signal();
    trace_startup("run: pdeath ok");
    let exit_flag = setup_exit_flag()?;
    trace_startup("run: exit_flag ok");
    if !tty_is_alive() {
        trace_startup("run: tty not alive (exit 0)");
        return Ok(0);
    }
    trace_startup("run: tty ok");

    let mut stdout = io::stdout();
    enable_raw_mode().context("enable_raw_mode failed")?;
    if let Err(e) = execute!(
        stdout,
        EnterAlternateScreen,
        EnableBracketedPaste,
        EnableMouseCapture
    ) {
        disable_raw_mode().ok();
        return Err(anyhow::anyhow!("EnterAlternateScreen failed: {e}"));
    }

    trace_startup("run: alt_screen ok");
    let _ui = UiGuard;

    trace_startup("run: backend");
    let backend = CrosstermBackend::new(stdout);
    trace_startup("run: terminal new");
    let mut terminal = Terminal::new(backend)?;
    execute!(terminal.backend_mut(), Clear(ClearType::All)).ok();
    terminal.clear().ok();

    trace_startup("run: entering run_loop");
    run_loop(&mut terminal, exit_flag)
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
    expect_brief: bool,
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
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
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
        expect_brief,
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
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
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
            "expect_brief": expect_brief,
            "sse_enabled": sse_enabled,
        }),
    );
    *retry_status = None;
    *active_tool_stream = None;
    //（1）在请求启动前由发起方设置（mind 协同/普通对话/心跳/写日记）；这里不改写标志。
    //（2）以实际开始流式输出的 mind 为准，避免顶栏“亮错边”（例如主意识发起的工具/请求）。
    *active_kind = kind;
    tool_preview.clear();
    *tool_preview_active = false;
    *tool_preview_pending_idle = false;
    *tool_preview_chat_idx = None;
    brief_text.clear();
    *brief_idx = None;
    *brief_in_progress = false;
    *brief_pending_idle = false;
    //（1）思考占位：用于把 think 工具块固定在正文上方，避免非 SSE 时“思考块跑到正文下方”。
    //（2）SSE：running 阶段会在聊天区闪烁（降低首包延迟感）
    //（3）非 SSE：默认仍隐藏（除非用户 Shift+Tab 展开），但保留正确的时间线顺序
    let codex_mode = expect_brief;
    if !is_heartbeat && thinking_idx.is_none() && !codex_mode {
        let stub = build_thinking_tool_message_stub(kind, "running", "Thinking");
        core.push_tool_mind(stub, Some(kind));
        *thinking_idx = Some(core.history.len().saturating_sub(1));
    }
    if !is_heartbeat && codex_mode {
        //（1）Codex：把 brief/think 作为“可选元数据”放在正文前，但默认不显示占位。
        //（2）Codex：空 stub UI 忽略，直到收到真实 brief/reasoning。
        if brief_idx.is_none() {
            core.push_tool_mind(build_codex_brief_tool_message_stub(kind), Some(kind));
            *brief_idx = Some(core.history.len().saturating_sub(1));
        }
        if thinking_idx.is_none() {
            core.push_tool_mind(build_codex_thinking_tool_message_stub(kind), Some(kind));
            *thinking_idx = Some(core.history.len().saturating_sub(1));
        }
    }
    let assistant_idx = core.history.len();
    streaming_state.start(assistant_idx);
    //（1）expect_brief 仅用于 Codex。
    //（2）标记本轮为 Codex 模式。
    //（3）summary 只在顶栏展示。
    streaming_state.codex_mode = codex_mode;
    streaming_state.codex_summary_pending_sep = false;
    core.history.push(Message {
        role: Role::Assistant,
        //（1）Codex 下不再创建“思考工具占位”，避免出现两个闪烁点；在正文首包前给一个轻量提示。
        text: if codex_mode {
            "Working...".to_string()
        } else {
            String::new()
        },
        mind: Some(kind),
    });
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
            //（1）迅速的一次性“乱码脉冲”，避免长时间持续动画造成干扰。
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
            entry.mind = Some(owner);
            render_cache.invalidate(stub_idx);
        }
        state.idx = stub_idx;
    } else {
        core.push_tool_mind(tool_text.clone(), Some(owner));
        state.idx = core.history.len().saturating_sub(1);
    }
    //（1）默认只展示 brief：工具详情由用户手动展开（→）。
    expanded_tool_idxs.remove(&state.idx);
    *active_tool_stream = Some(state);
    *thinking_in_progress = false;
    *thinking_pending_idle = thinking_idx.is_some();
    *mode = Mode::ExecutingTool;
    jump_to_bottom(scroll, follow_bottom);
}

fn handle_mind_msg_tool(state: &ToolStreamState) -> Option<(MindKind, MindKind, String, String)> {
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

    Some((owner, target, brief_clean, content_clean))
}

fn handle_error_retry(retry_status: &mut Option<String>, attempt: usize, max: usize) {
    *retry_status = Some(format!("♡ 现在[萤]第{attempt}/{max}次重试中…"));
}

fn context_path_for_mind<'a>(config: &'a AppConfig, mind: &str) -> &'a str {
    if mind.trim().eq_ignore_ascii_case("memory") {
        config.memory_context_path.as_str()
    } else if mind.trim().eq_ignore_ascii_case("dog") {
        config.dog_context_path.as_str()
    } else {
        config.dyncontext_path.as_str()
    }
}

// ===== 主循环（启动加载配置，清空外置 context，进入事件循环）=====
fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    exit_flag: Arc<AtomicBool>,
) -> anyhow::Result<i32> {
    trace_startup("run_loop: enter");
    let mut core = Core::new();
    trace_startup("run_loop: core ok");
    let test_cfg = crate::test::init_from_env();
    let mut config = AppConfig::from_env();
    trace_startup("run_loop: config ok");
    let (mut dog_cfg, dog_cfg_err, dog_cfg_path) = load_dog_api_config();
    trace_startup("run_loop: dog_cfg ok");
    let (mut main_cfg, main_cfg_err, main_cfg_path) = load_main_api_config();
    trace_startup("run_loop: main_cfg ok");
    let (mut memory_cfg, memory_cfg_err, memory_cfg_path) = load_memory_api_config(&main_cfg);
    trace_startup("run_loop: memory_cfg ok");
    let (mut sys_cfg, sys_cfg_err, sys_cfg_path) = load_system_config();
    trace_startup("run_loop: sys_cfg ok");
    let (mut context_prompts, context_prompt_errs, context_prompt_path) = load_context_prompts();
    trace_startup("run_loop: ctx_prompts ok");
    if let Some(root) = config_root_dir(&dog_cfg_path) {
        config.metamemo_path = resolve_relative_path(&config.metamemo_path, &root);
        config.datememo_path = resolve_relative_path(&config.datememo_path, &root);
        config.contextmemo_path = resolve_relative_path(&config.contextmemo_path, &root);
        config.dyncontext_path = resolve_relative_path(&config.dyncontext_path, &root);
        config.dog_context_path = resolve_relative_path(&config.dog_context_path, &root);
        config.memory_context_path = resolve_relative_path(&config.memory_context_path, &root);
        config.run_log_path = resolve_relative_path(&config.run_log_path, &root);
        config.memo_db_path = resolve_relative_path(&config.memo_db_path, &root);
        let _ = std::env::set_current_dir(&root);
    }
    trace_startup("run_loop: root resolved");
    //（1）会话上下文外置：每次启动清空 context JSONL（MAIN/DOG 与 MEMORY 分离）。
    //（2）暂不做阈值/截断；文件增长仅影响本次会话。
    clear_dyncontext_file(&config.dyncontext_path);
    clear_dyncontext_file(&config.dog_context_path);
    clear_dyncontext_file(&config.memory_context_path);
    trace_startup("run_loop: init_run_logger");
    init_run_logger(&config.run_log_path);
    trace_startup("run_loop: init_run_logger ok");
    runlog_event(
        "INFO",
        "app.start",
        json!({
            "cwd": std::env::current_dir().ok().map(|p| p.to_string_lossy().to_string()),
            "exe": std::env::current_exe().ok().map(|p| p.to_string_lossy().to_string()),
            "run_log_path": config.run_log_path,
            "metamemo_path": config.metamemo_path,
            "datememo_path": config.datememo_path,
            "contextmemo_path": config.contextmemo_path,
            "dyncontext_path": config.dyncontext_path,
            "dog_context_path": config.dog_context_path,
            "memory_context_path": config.memory_context_path,
            "memo_db_path": config.memo_db_path,
            "version": env!("CARGO_PKG_VERSION"),
            "build_profile": if cfg!(debug_assertions) { "debug" } else { "release" },
        }),
    );
    trace_startup("run_loop: runlog_event ok");
    //（1）set_var 在当前 toolchain 中为 unsafe，集中处理并降低散落风险。
    unsafe {
        std::env::set_var("YING_MEMO_DB_PATH", &config.memo_db_path);
        std::env::set_var("YING_METAMEMO_PATH", &config.metamemo_path);
        std::env::set_var("YING_DATEMEMO_PATH", &config.datememo_path);
    }
    trace_startup("run_loop: set_var ok");
    let mut memo_db: Option<MemoDb> = None;
    trace_startup("run_loop: memo_db var ok");
    let mut memo_db_err: Option<String> = None;
    trace_startup("run_loop: memo_db open start");
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
    let mut _memory_prompt_err: Option<String> = None;
    let memory_prompt = match load_prompt(Path::new(&memory_cfg.prompt_path)) {
        Ok(text) => text,
        Err(e) => {
            _memory_prompt_err = Some(format!("{e:#}"));
            String::new()
        }
    };
    let (pty_audit_prompt_text, pty_audit_prompt_err, pty_audit_prompt_path) =
        load_pty_audit_prompt(&sys_cfg);
    let (pty_help_prompt_text, pty_help_prompt_err, pty_help_prompt_path) =
        load_pty_help_prompt(&sys_cfg);
    let pty_started_notice_prompt_text = load_pty_started_notice_prompt();
    let pty_started_notice_prompt_err: Option<String> = None;
    let (fastmemo_compact_prompt_text, fastmemo_compact_prompt_err, fastmemo_compact_prompt_path) =
        load_fastmemo_compact_prompt();
    let (
        welcome_shortcuts_prompt_text,
        welcome_shortcuts_prompt_err,
        welcome_shortcuts_prompt_path,
    ) = load_welcome_shortcuts_prompt(&sys_cfg);
    let mcp_messages_err: Option<String> = None;
    let pty_messages_err: Option<String> = None;
    let mcp_messages = McpMessages::default();
    let pty_messages = PtyMessages::default();
    let mut dog_state = DogState::new(
        dog_prompt,
        dog_cfg.prompt_reinject_pct,
        mcp_messages.tool_result_assistant.clone(),
    );
    let mut main_prompt_text = main_prompt;
    let mut main_state = DogState::new(
        main_prompt_text.clone(),
        80,
        mcp_messages.tool_result_assistant.clone(),
    );
    let memory_state = DogState::new(
        memory_prompt.clone(),
        80,
        mcp_messages.tool_result_assistant.clone(),
    );
    let mut memory_state = memory_state;
    memory_state.set_include_tool_context(true);
    //（1）MAIN 也会发起工具调用（记忆/系统配置等）。
    //（2）需回注工具结果，避免重复请求。
    main_state.set_include_tool_context(true);
    dog_state.reset_context();
    main_state.reset_context();
    memory_state.reset_context();
    let mut mind_context = MindContextPool::new(MIND_CTX_MAX_ROUNDS, MIND_CTX_MAX_TOKENS);
    let mut mind_ctx_idx_main: Option<usize> = None;
    let mut mind_ctx_idx_dog: Option<usize> = None;
    let mut mind_ctx_idx_memory: Option<usize> = None;
    let mut mind_rate_window: VecDeque<Instant> = VecDeque::new();
    let mut active_request_is_mind: bool = false;
    let mut active_request_is_fastmemo_compact: bool = false;
    //（1）内部占位请求标记。
    //（2）避免自愈占位触发二次自愈成环。
    let mut active_request_is_internal_placeholder: bool = false;
    let mut auto_fastmemo_compact: bool = false;
    let mut fastmemo_compact_inflight: bool = false;
    let mut fastmemo_compact_edit_mask: u8 = 0;
    let mut fastmemo_compact_retry_at: Option<Instant> = None;
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
    let memory_client_result = build_main_client(&memory_cfg);
    let mut memory_client = memory_client_result.as_ref().ok().cloned();
    let _memory_client_err = memory_client_result.err().map(|e| e.to_string());
    let theme = ui::Theme::from_env();
    let mut render_cache = ui::ChatRenderCache::new();
    //（1）欢迎语：折叠 system info（简约态只显示一行，详情态在工具详情展开）。
    let welcome_header = "♡ · 系统信息 · 欢迎使用AItermux。";
    let welcome = if welcome_shortcuts_prompt_text.trim().is_empty() {
        welcome_header.to_string()
    } else {
        format!(
            "{welcome_header}\n\n{}",
            welcome_shortcuts_prompt_text.trim_end()
        )
    };
    core.push_system(&welcome);
    let mut input = String::new();
    let mut pending_pty_snapshot: Option<PendingPtySnapshot> = None;
    let mut cursor: usize = 0;
    let mut input_chars: usize = 0;
    let mut scroll: u16 = 0;
    let mut follow_bottom = true;
    //（1）End/Ins：快速跳到底/回到之前位置（单向恢复，避免来回切换）。
    let mut scroll_restore_before_end: Option<(u16, bool)> = None;
    let mut max_scroll_cache: usize = 0;
    let mut chat_width_cache: usize = 0;
    let status_width_cache: usize = 0;
    let mut settings_editor_width_cache: usize = 0;
    let mut settings_editor_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut settings_confirm_yes_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut settings_confirm_no_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut input_width_cache: usize = 0;
    let mut mode = Mode::Idle;
    let mut sse_enabled = sys_cfg.sse_enabled;
    let mut request_seq: u64 = 0;
    let mut active_request_id: Option<u64> = None;
    let mut active_cancel: Option<Arc<AtomicBool>> = None;
    let mut active_request_in_tokens: Option<u64> = None;
    let mut spinner_tick: usize = 0;
    //（1）启动默认 Tab：Matrix（Main）。
    //（2）Tab 仅影响“下一条消息发给哪个 API”，不做持久化（避免把 UI 状态写进系统配置）。
    let mut chat_target = MindKind::Main;
    let mut active_kind = MindKind::Main;
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
    let mut next_pty_audit_at: Option<Instant> = None;
    let mut last_pty_running_total: usize = 0;
    let mut pulse_notice: Option<PulseNotice> = None;
    let mut heartbeat_count: u64 = token_totals.total_heartbeat_count;
    let mut response_count: u64 = token_totals.total_heartbeat_responses;
    let mut hint_last = String::new();
    let mut hint_anim_start_tick: usize = 0;
    let mut hint_anim_seed: u64 = 0;
    let mut pending_cmd_shell = false;
    let mut pending_user_terminal = false;
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
    if let Some(err) = memory_cfg_err {
        push_sys_log(&mut sys_log, config.sys_log_limit, format!("MEM: {err}"));
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("MEM: 配置 {}", memory_cfg_path.display()),
        );
    }
    if memory_cfg
        .api_key
        .as_deref()
        .unwrap_or("")
        .trim()
        .is_empty()
    {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MEM: API Key 为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MEM: API Key 已配置");
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
    if let Some(err) = pty_audit_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-AUDIT: {err} ({})", pty_audit_prompt_path.display()),
        );
    } else if pty_audit_prompt_text.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "PTY-AUDIT: 提示词为空");
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-AUDIT: 配置 {}", pty_audit_prompt_path.display()),
        );
    }
    if let Some(err) = pty_help_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-HELP: {err} ({})", pty_help_prompt_path.display()),
        );
    } else if pty_help_prompt_text.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "PTY-HELP: 提示词为空");
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-HELP: 配置 {}", pty_help_prompt_path.display()),
        );
    }
    if let Some(err) = pty_started_notice_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-START: {err}"),
        );
    } else if pty_started_notice_prompt_text.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "PTY-START: 提示词为空");
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "PTY-START: 内置");
    }
    if let Some(err) = fastmemo_compact_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!(
                "FASTMEMO-COMPACT: {err} ({})",
                fastmemo_compact_prompt_path.display()
            ),
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
            format!(
                "FASTMEMO-COMPACT: 配置 {}",
                fastmemo_compact_prompt_path.display()
            ),
        );
    }
    if let Some(err) = welcome_shortcuts_prompt_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!(
                "WELCOME: {err} ({})",
                welcome_shortcuts_prompt_path.display()
            ),
        );
    } else if welcome_shortcuts_prompt_text.trim().is_empty() {
        push_sys_log(&mut sys_log, config.sys_log_limit, "WELCOME: 提示词为空");
    } else {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("WELCOME: 配置 {}", welcome_shortcuts_prompt_path.display()),
        );
    }
    if let Some(err) = mcp_messages_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("MCP-MESSAGES: {err}"),
        );
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "MCP-MESSAGES: 内置");
    }
    if let Some(err) = pty_messages_err {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("PTY-MESSAGES: {err}"),
        );
    } else {
        push_sys_log(&mut sys_log, config.sys_log_limit, "PTY-MESSAGES: 内置");
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
    if test_cfg.enabled {
        let mut note = String::from("TEST: 已启用");
        if test_cfg.show_system_bootstrap {
            note.push_str(" | show_system_boot=on");
        } else {
            note.push_str(" | show_system_boot=off");
        }
        if test_cfg.block_model_requests {
            note.push_str(" | block_model=on");
        } else {
            note.push_str(" | block_model=off");
        }
        push_sys_log(&mut sys_log, config.sys_log_limit, note);
    }
    let injected = crate::test::inject_boot_system_messages(&mut core, &sys_log);
    if injected > 0 {
        push_sys_log(
            &mut sys_log,
            config.sys_log_limit,
            format!("TEST: 已注入启动系统信息到聊天区（{injected} 条）"),
        );
    }
    let mut sys_scroll: usize = 0;
    let mut last_context_save = Instant::now();
    let mut last_sys_scroll = Instant::now();
    let mut sys_scroll_until: Option<Instant> = None;
    let mut last_sys_line = String::new();
    let mut retry_status: Option<String> = None;
    let mut mind_pulse: Option<MindPulse> = None;
    let mut brief_text = String::new();
    let mut brief_idx: Option<usize> = None;
    let mut brief_in_progress = false;
    let mut brief_pending_idle = false;
    let mut thinking_text = String::new();
    let mut thinking_full_text = String::new();
    let mut thinking_idx: Option<usize> = None;
    let mut thinking_in_progress = false;
    let mut thinking_pending_idle = false;
    let mut thinking_started_at: Option<Instant> = None;
    let mut thinking_scroll: usize = 0;
    let mut thinking_scroll_cap: usize = 0;
    let mut last_thinking_text = String::new();
    let mut last_status_label = "状态".to_string();
    let mut tool_preview = String::new();
    let mut tool_preview_active = false;
    let mut tool_preview_pending_idle = false;
    let mut tool_preview_chat_idx: Option<usize> = None;
    let mut reveal_idx: Option<usize> = None;
    let mut reveal_len: usize = 0;
    let mut last_reveal_at = Instant::now();
    let mut sending_until: Option<Instant> = None;
    let mut streaming_state = StreamingState::default();
    let mut last_input_at: Option<Instant> = None;
    let mut chat_focus = ChatFocus::Input;
    let mut expanded_tool_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut collapsed_tool_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut codex_expanded_tool_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut codex_collapsed_tool_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut expanded_user_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut collapsed_user_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut expanded_thinking_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut collapsed_thinking_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut codex_expanded_thinking_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut codex_collapsed_thinking_idxs: BTreeSet<usize> = BTreeSet::new();
    let mut tool_global_expanded = false;
    let mut user_global_expanded = false;
    let mut thinking_global_expanded = false;
    let mut codex_tool_global_expanded = false;
    let mut codex_thinking_global_expanded = false;
    let mut selected_msg_idx: Option<usize> = None;
    let details_mode = false;
    let mut msg_line_ranges_cache: Vec<Option<(usize, usize)>> = Vec::new();
    let mut pending_scroll_anchor: Option<ScrollAnchor> = None;
    let mut chat_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut tabs_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut input_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut pty_rect_cache: Option<ratatui::layout::Rect> = None;
    let mut settings_rect_cache: Option<ratatui::layout::Rect> = None;

    let mut settings_fields_scroll_cache: usize = 0;
    let mut chat_height_cache: usize = 0;
    let mut active_tool_stream: Option<ToolStreamState> = None;
    let mut pending_tools: VecDeque<ToolCall> = VecDeque::new();
    let mut pending_tool_confirm: Option<(ToolCall, String, usize)> = None;
    let mut send_queue_high: VecDeque<QueuedUserMessage> = VecDeque::new();
    let mut send_queue_low: VecDeque<QueuedUserMessage> = VecDeque::new();
    let mut send_queue_internal: VecDeque<InternalUserMessage> = VecDeque::new();
    let mut send_queue_ui: SendQueueUiState = SendQueueUiState::Closed;
    let mut help_ui: HelpUiState = HelpUiState::Closed;
    //（1）自动出队节流。
    //（2）避免空闲瞬间连发多条导致误读。
    let mut send_queue_dequeue_cooldown_until: Option<Instant> = None;
    let mut screen = Screen::Chat;
    let mut settings = SettingsState::new();
    let mut pty_tabs: Vec<PtyUiState> = Vec::new();
    let mut pty_active_idx: usize = 0;
    let mut pty_handles: HashMap<u64, mpsc::Sender<PtyControl>> = HashMap::new();
    let mut pty_view = false;
    let mut pty_focus = PtyFocus::Terminal;
    //（1）多 PTY：允许多个后台终端任务共存。
    //（2）PgUp/PgDn 快速切换焦点 tab。
    let mut last_esc_at: Option<Instant> = None;
    //（1）Home 双击：回到简约模式。
    //（2）收敛展开状态，并退出消息选择。
    let mut last_home_at: Option<Instant> = None;
    //（1）PTY DONE 聚合：合并同批结束事件。
    //（2）避免逐个回传触发多次续跑。
    let mut pty_done_batches = PtyDoneBatches::default();
    let mut pty_done_followups: VecDeque<PtyDoneFollowup> = VecDeque::new();
    let mut prev_pty_view = false;
    let mut chat_anim_fast_forward = false;

    let (tx, rx) = mpsc::channel::<AsyncEvent>();
    let mut paste_guard_until: Option<Instant> = None;
    let mut paste_drop_until: Option<Instant> = None;
    let mut burst_count: usize = 0;
    let mut burst_last_at: Option<Instant> = None;
    let mut burst_started_at: Option<Instant> = None;
    let mut burst_start_cursor: Option<usize> = None;
    let mut paste_capture: Option<PasteCapture> = None;
    let mut toast: Option<(Instant, String)> = None;
    //（1）状态栏右侧动作提示（短暂显示）。
    //（2）仅影响界面，不进入上下文与协议层。
    let mut action_hint: Option<(Instant, String)> = None;
    //（1）大粘贴占位符缓存：pending_pastes 非空表示存在占位符。
    //（2）为避免误触 Enter 提交，粘贴期间 Enter 一律视为换行（不发送）。
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
    let mut last_term_size: Option<(u16, u16)> = None;
    let mut touch_drag_last_row: Option<u16> = None;
    let mut touch_drag_last_col: Option<u16> = None;
    //（1）触控拖拽滚动：用起点锚定绑定 scroll 与手指位移。
    //（2）避免高频 Drag 事件导致滚动过快。
    let mut touch_drag_anchor_row: Option<u16> = None;
    let mut touch_drag_anchor_scroll: Option<u16> = None;

    macro_rules! drain_events {
        () => {
            drain_async_events(LoopCtx {
                core: &mut core,
                rx: &rx,
                render_cache: &mut render_cache,
                pty_tabs: &mut pty_tabs,
                pty_active_idx: &mut pty_active_idx,
                pty_handles: &mut pty_handles,
                pty_view: &mut pty_view,
                pty_done_batches: &mut pty_done_batches,
                pty_done_followups: &mut pty_done_followups,
                send_queue_internal: &mut send_queue_internal,
                mode: &mut mode,
                scroll: &mut scroll,
                follow_bottom: &mut follow_bottom,
                active_kind: &mut active_kind,
                reveal_idx: &mut reveal_idx,
                reveal_len: &mut reveal_len,
                expanded_tool_idxs: &mut expanded_tool_idxs,
                collapsed_tool_idxs: &mut collapsed_tool_idxs,
                expanded_user_idxs: &mut expanded_user_idxs,
                collapsed_user_idxs: &mut collapsed_user_idxs,
                active_tool_stream: &mut active_tool_stream,
                dog_state: &mut dog_state,
                dog_client: &dog_client,
                main_state: &mut main_state,
                main_client: &main_client,
                memory_state: &mut memory_state,
                memory_client: &memory_client,
                mind_context: &mut mind_context,
                _mind_ctx_idx_main: &mut mind_ctx_idx_main,
                _mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                sending_until: &mut sending_until,
                token_totals: &mut token_totals,
                token_total_path: &token_total_path,
                meta: &mut metamemo,
                context_usage: &mut context_usage,
                diary_state: &mut diary_state,
                mind_rate_window: &mut mind_rate_window,
                brief_text: &mut brief_text,
                brief_idx: &mut brief_idx,
                brief_in_progress: &mut brief_in_progress,
                brief_pending_idle: &mut brief_pending_idle,
                thinking_text: &mut thinking_text,
                thinking_full_text: &mut thinking_full_text,
                thinking_idx: &mut thinking_idx,
                thinking_in_progress: &mut thinking_in_progress,
                thinking_pending_idle: &mut thinking_pending_idle,
                thinking_started_at: &mut thinking_started_at,
                expanded_thinking_idxs: &mut expanded_thinking_idxs,
                collapsed_thinking_idxs: &mut collapsed_thinking_idxs,
                selected_msg_idx: &mut selected_msg_idx,
                tool_preview: &mut tool_preview,
                tool_preview_active: &mut tool_preview_active,
                tool_preview_pending_idle: &mut tool_preview_pending_idle,
                tool_preview_chat_idx: &mut tool_preview_chat_idx,
                input: &mut input,
                cursor: &mut cursor,
                input_chars: &mut input_chars,
                last_input_at: &mut last_input_at,
                pending_pty_snapshot: &mut pending_pty_snapshot,
                active_request_is_mind: &mut active_request_is_mind,
                active_request_is_fastmemo_compact: &mut active_request_is_fastmemo_compact,
                active_request_is_internal_placeholder: &mut active_request_is_internal_placeholder,
                streaming_state: &mut streaming_state,
                mind_pulse: &mut mind_pulse,
                retry_status: &mut retry_status,
                sys_log: &mut sys_log,
                sys_log_limit: config.sys_log_limit,
                config: &config,
                fastmemo_compact_prompt_text: &fastmemo_compact_prompt_text,
                pty_help_prompt_text: &pty_help_prompt_text,
                pty_started_notice_prompt_text: &pty_started_notice_prompt_text,
                mcp_messages: &mcp_messages,
                pty_messages: &pty_messages,
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
            //（1）兜底：若父进程已退出（PPID=1），立刻退出，避免极端情况下变成孤儿进程持续占用 CPU。
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

        //（1）PTY 批量回传：聚合 tool result 写入聊天并入上下文。
        //（2）可选：注入一条“内部提示”让模型简短汇报执行结果（由 pty_messages 配置控制）。
        if !pty_done_followups.is_empty()
            && pty_tabs.is_empty()
            && can_inject_heartbeat(
                mode,
                &active_request_id,
                &pending_tools,
                &pending_tool_confirm,
                &active_tool_stream,
            )
        {
            let front = &mut pty_done_followups[0];
            if !front.pushed {
                core.push_tool_mind(front.tool_text.clone(), Some(front.owner));
                let mind = match front.owner {
                    MindKind::Sub => "dog",
                    MindKind::Main => "main",
                    MindKind::Memory => "memory",
                };
                let p = context_path_for_mind(&config, mind);
                //（1）会话上下文外置：把 PTY 批量完成回执写入 context.jsonl，供后续请求回放到模型上下文。
                log_dyncontext(p, mind, "tool", &front.tool_text, Some("pty"), None);
                render_cache.invalidate(core.history.len().saturating_sub(1));
                front.pushed = true;
                push_sys_log(&mut sys_log, config.sys_log_limit, "Terminal: batch ready");
                let mut auto_reply = false;
                let tpl = pty_messages.pty_done_auto_report_prompt.trim();
                if !tpl.is_empty() {
                    let prompt = crate::messages::render_template(
                        tpl,
                        &[
                            ("JOBS", &front.jobs.to_string()),
                            ("OWNER", mind_label(front.owner)),
                        ],
                    );
                    if !prompt.trim().is_empty() {
                        send_queue_internal.push_back(InternalUserMessage {
                            target: front.owner,
                            //（1）DeepSeek 要求最后一条为 user。
                            //（2）这里只放协议占位文本，不写入长期上下文。
                            model_text: {
                                let code = match front.owner {
                                    MindKind::Sub => dog_state.last_tool_output_code(),
                                    _ => main_state.last_tool_output_code(),
                                };
                                crate::context::role_needed_user_placeholder_to(code).to_string()
                            },
                            extra_system: Some(prompt),
                        });
                        auto_reply = true;
                    }
                }
                runlog_event(
                    "INFO",
                    "pty.batch.ready",
                    json!({"owner": mind_label(front.owner), "jobs": front.jobs, "auto_reply": auto_reply}),
                );
            }
            pty_done_followups.pop_front();
            needs_redraw = true;
        }

        //（1）内部队列：空闲时自动出队（不展示给用户）。
        //（2）约束：不与模型请求/工具并发；不抢占 diary/压缩流程；不与队列 UI 并发。
        if !send_queue_internal.is_empty()
            && send_queue_dequeue_cooldown_until.is_none_or(|t| now >= t)
            && matches!(send_queue_ui, SendQueueUiState::Closed)
            && !diary_state.active()
            && can_inject_heartbeat(
                mode,
                &active_request_id,
                &pending_tools,
                &pending_tool_confirm,
                &active_tool_stream,
            )
            && let Some(msg) = send_queue_internal.pop_front()
        {
            send_queue_dequeue_cooldown_until = Some(now + Duration::from_millis(450));
            clear_brief_state(ClearBriefStateArgs {
                brief_text: &mut brief_text,
                brief_idx: &mut brief_idx,
                brief_in_progress: &mut brief_in_progress,
                brief_pending_idle: &mut brief_pending_idle,
            });
            //（1）内部消息不写入聊天区。
            //（2）ui_text 为空即可（push_user 会忽略空）。
            start_user_chat_request(StartUserChatRequestArgs {
                now,
                ui_text: String::new(),
                model_text: msg.model_text,
                target: msg.target,
                extra_system_override: msg.extra_system,
                core: &mut core,
                metamemo: &mut metamemo,
                context_usage: &mut context_usage,
                dog_state: &mut dog_state,
                main_state: &mut main_state,
                memory_state: &mut memory_state,
                sys_cfg: &sys_cfg,
                config: &config,
                tx: &tx,
                mode: &mut mode,
                active_kind: &mut active_kind,
                active_request_is_internal_placeholder: &mut active_request_is_internal_placeholder,
                sending_until: &mut sending_until,
                sys_log: &mut sys_log,
                streaming_state: &mut streaming_state,
                request_seq: &mut request_seq,
                active_request_id: &mut active_request_id,
                active_cancel: &mut active_cancel,
                sse_enabled,
                next_heartbeat_at: &mut next_heartbeat_at,
                thinking_text: &mut thinking_text,
                thinking_full_text: &mut thinking_full_text,
                thinking_idx: &mut thinking_idx,
                thinking_in_progress: &mut thinking_in_progress,
                thinking_pending_idle: &mut thinking_pending_idle,
                thinking_started_at: &mut thinking_started_at,
                expanded_tool_idxs: &mut expanded_tool_idxs,
                expanded_thinking_idxs: &mut expanded_thinking_idxs,
                scroll: &mut scroll,
                follow_bottom: &mut follow_bottom,
                token_totals: &mut token_totals,
                token_total_path: &token_total_path,
                active_request_in_tokens: &mut active_request_in_tokens,
                dog_client: &dog_client,
                main_client: &main_client,
                memory_client: &memory_client,
            })?;
            needs_redraw = true;
            continue;
        }
        //（1）发送队列：空闲时自动出队（High 优先，Low 最低优先级）。
        //（2）约束：不与模型请求/工具并发；不抢占 diary/压缩流程；不与队列 UI 并发。
        if send_queue_len(&send_queue_high, &send_queue_low) > 0
            && send_queue_dequeue_cooldown_until.is_none_or(|t| now >= t)
            && matches!(send_queue_ui, SendQueueUiState::Closed)
            && !diary_state.active()
            && can_inject_heartbeat(
                mode,
                &active_request_id,
                &pending_tools,
                &pending_tool_confirm,
                &active_tool_stream,
            )
        {
            let next = send_queue_high
                .pop_front()
                .or_else(|| send_queue_low.pop_front());
            if let Some(msg) = next {
                send_queue_dequeue_cooldown_until = Some(now + Duration::from_millis(450));
                push_sys_log(
                    &mut sys_log,
                    config.sys_log_limit,
                    format!("队列出队：#{} → {:?}", msg.id, msg.target),
                );
                //（1）队列允许斜杠命令。
                //（2）在 idle 阶段依序执行。
                if handle_command(HandleCommandArgs {
                    core: &mut core,
                    raw: msg.model_text.trim_end(),
                    meta: &mut metamemo,
                    context_usage: &mut context_usage,
                    mode: &mut mode,
                    screen: &mut screen,
                    help_ui: &mut help_ui,
                    should_exit: &mut should_exit,
                    sse_enabled: &mut sse_enabled,
                    enter_cmd_shell: &mut pending_cmd_shell,
                    open_user_terminal: &mut pending_user_terminal,
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
                        collapsed_tool_idxs: &mut collapsed_tool_idxs,
                        expanded_user_idxs: &mut expanded_user_idxs,
                        collapsed_user_idxs: &mut collapsed_user_idxs,
                        expanded_thinking_idxs: &mut expanded_thinking_idxs,
                        collapsed_thinking_idxs: &mut collapsed_thinking_idxs,
                        selected_msg_idx: &mut selected_msg_idx,
                        tool_preview_chat_idx: &mut tool_preview_chat_idx,
                        brief_text: &mut brief_text,
                        brief_idx: &mut brief_idx,
                        brief_in_progress: &mut brief_in_progress,
                        brief_pending_idle: &mut brief_pending_idle,
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
                    }
                    if pending_user_terminal && matches!(screen, Screen::Chat) {
                        pending_user_terminal = false;
                        reset_input_buffer(
                            &mut input,
                            &mut cursor,
                            &mut input_chars,
                            &mut last_input_at,
                        );
                        core.push_system("··· Terminal 已打开（用户手动）\n- 该窗口中的命令由用户自己执行\n- 退出/关闭请在 Terminal 内自行完成\n- 这不是 AI 启动的自动化任务");
                        push_sys_log(&mut sys_log, config.sys_log_limit, "Terminal: user session");
                        let call = ToolCall {
                            tool: "bash".to_string(),
                            input: "exec bash -li".to_string(),
                            brief: Some("terminal".to_string()),
                            interactive: Some(true),
                            cwd: Some("~".to_string()),
                            timeout_secs: Some(30 * 60),
                            ..ToolCall::default()
                        };
	                        let _ = spawn_interactive_bash_execution(
	                            call,
	                            MindKind::Main,
	                            tx.clone(),
	                            String::new(),
	                            String::new(),
	                            false,
	                            true,
	                        );
                    }
                    if should_exit {
                        break;
                    }
                    needs_redraw = true;
                    continue;
                }

                start_user_chat_request(StartUserChatRequestArgs {
                    now,
                    ui_text: msg.ui_text,
                    model_text: msg.model_text,
                    target: msg.target,
                    extra_system_override: None,
                    core: &mut core,
                    metamemo: &mut metamemo,
                    context_usage: &mut context_usage,
                    dog_state: &mut dog_state,
                    main_state: &mut main_state,
                    memory_state: &mut memory_state,
                    sys_cfg: &sys_cfg,
                    config: &config,
                    tx: &tx,
                    mode: &mut mode,
                    active_kind: &mut active_kind,
                    active_request_is_internal_placeholder:
                        &mut active_request_is_internal_placeholder,
                    sending_until: &mut sending_until,
                    sys_log: &mut sys_log,
                    streaming_state: &mut streaming_state,
                    request_seq: &mut request_seq,
                    active_request_id: &mut active_request_id,
                    active_cancel: &mut active_cancel,
                    sse_enabled,
                    next_heartbeat_at: &mut next_heartbeat_at,
                    thinking_text: &mut thinking_text,
                    thinking_full_text: &mut thinking_full_text,
                    thinking_idx: &mut thinking_idx,
                    thinking_in_progress: &mut thinking_in_progress,
                    thinking_pending_idle: &mut thinking_pending_idle,
                    thinking_started_at: &mut thinking_started_at,
                    expanded_tool_idxs: &mut expanded_tool_idxs,
                    expanded_thinking_idxs: &mut expanded_thinking_idxs,
                    scroll: &mut scroll,
                    follow_bottom: &mut follow_bottom,
                    token_totals: &mut token_totals,
                    token_total_path: &token_total_path,
                    active_request_in_tokens: &mut active_request_in_tokens,
                    dog_client: &dog_client,
                    main_client: &main_client,
                    memory_client: &memory_client,
                })?;
                needs_redraw = true;
                continue;
            }
        }
        if heartbeat_minutes_cache > 0
            && now >= next_heartbeat_at
            && send_queue_len(&send_queue_high, &send_queue_low) == 0
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
            //（1）DeepSeek 对 system 位置与排序较敏感。
            //（2）心跳按 user 消息注入上下文。
            //（3）顺序：assistant -> user(heartbeat) -> assistant(reply)。
            let hb_user = crate::messages::render_heartbeat_user(&mcp_messages, &stamp);
            let p = context_path_for_mind(&config, "main");
            log_dyncontext(p, "main", "user", &hb_user, None, None);
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

        //（1）PTY 自动审计（可选）：默认关闭，避免在用户未要求时触发额外的 BASH 调用造成资源浪费。
        if sys_cfg.pty_audit_enabled {
            //（1）PTY 定时审计（每 10 分钟）：以“额外系统消息”注入，让模型按需读取 status/log。
            //（2）说明：不与用户请求并发；若当前忙，则延后。
            let running_total_now = pty_tabs.len();
            //（1）只要“PTY 总数增加”（新任务启动），就重置一次审计计时：确保“本次启动的 PTY”
            //（2）至少运行满间隔才会触发首轮审计。
            if running_total_now > last_pty_running_total && running_total_now > 0 {
                next_pty_audit_at = Some(now + Duration::from_secs(PTY_AUDIT_INTERVAL_SECS));
            }
            last_pty_running_total = running_total_now;
            let pty_info = current_pty_audit_target(pty_tabs.as_slice(), pty_active_idx);
            if pty_info.is_some() && next_pty_audit_at.is_none() {
                next_pty_audit_at = Some(now + Duration::from_secs(PTY_AUDIT_INTERVAL_SECS));
            }
            if pty_info.is_none() {
                next_pty_audit_at = None;
            }
            if let (Some(target), Some(next_at)) = (pty_info, next_pty_audit_at)
                && now >= next_at
                //（1）若用户正在看终端或正在交互，则不打扰：只在“后台运行”时审计更符合预期。
                && !pty_view
                && send_queue_len(&send_queue_high, &send_queue_low) == 0
                && can_inject_heartbeat(
                    mode,
                    &active_request_id,
                    &pending_tools,
                    &pending_tool_confirm,
                    &active_tool_stream,
                )
            {
                let owner = target.owner;
                let running_total = pty_tabs.len();
                let running_owner = running_owner_count(pty_tabs.as_slice(), owner);
                let prompt = render_pty_audit_prompt(
                    &pty_audit_prompt_text,
                    owner,
                    running_total,
                    running_owner,
                );
                let (client_ref, state_ref) = if matches!(owner, MindKind::Sub) {
                    (&dog_client, &dog_state)
                } else {
                    (&main_client, &main_state)
                };
                if let Some(in_tokens) = try_start_dog_generation(TryStartDogGenerationArgs {
                    kind: owner,
                    dog_client: client_ref,
                    dog_state: state_ref,
                    extra_system: Some(prompt),
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
                    push_sys_log(&mut sys_log, config.sys_log_limit, "Terminal: audit tick");
                    record_request_in_tokens(
                        &mut core,
                        &mut token_totals,
                        &token_total_path,
                        &context_usage,
                        &mut active_request_in_tokens,
                        in_tokens,
                    );
                    next_pty_audit_at = Some(now + Duration::from_secs(PTY_AUDIT_INTERVAL_SECS));
                } else if let Some(next_at) = next_pty_audit_at.as_mut() {
                    defer_pty_audit(next_at, now);
                }
            }
        } else {
            next_pty_audit_at = None;
            last_pty_running_total = 0;
        }
        let menu_items = if screen == Screen::Chat
            && !command_menu_suppress
            && is_idle_like(mode)
            && (matches!(chat_focus, ChatFocus::Input) || input.starts_with('/'))
            && matches!(send_queue_ui, SendQueueUiState::Closed)
            && matches!(help_ui, HelpUiState::Closed)
            && !is_pty_panel_active(screen, pty_view, pty_tabs.as_slice())
        {
            filter_commands_for_input(&input, sse_enabled)
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
            if !brief_text.trim().is_empty() {
                true
            } else if tool_preview_active || tool_preview_pending_idle {
                !tool_preview.trim().is_empty()
            } else {
                !thinking_text.trim().is_empty()
            }
        } else {
            false
        };
        let thinking_scroll_active = status_has_text
            && (brief_in_progress
                || brief_pending_idle
                || thinking_in_progress
                || tool_preview_active
                || tool_preview_pending_idle
                || thinking_pending_idle);
        //（1）聊天区相关动效（乱码/思考滚动/心跳脉冲）只在聊天页驱动 tick；
        //（2）否则在 Settings 页会出现“无操作也在动”的错觉。
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
        if in_chat && pty_view && !pty_tabs.is_empty() {
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
                force_working: mode == Mode::Generating
                    && match active_kind {
                        MindKind::Main => {
                            main_client.as_ref().is_some_and(|c| c.is_codex_provider())
                        }
                        MindKind::Sub => dog_client.as_ref().is_some_and(|c| c.is_codex_provider()),
                        MindKind::Memory => memory_client
                            .as_ref()
                            .is_some_and(|c| c.is_codex_provider()),
                    },
                mode,
                reveal_idx,
                streaming_has_content: streaming_state.has_content,
                brief_expected: brief_idx.is_some(),
                brief_text: brief_text.as_str(),
                thinking_text: thinking_text.as_str(),
                tool_preview_any,
                tool_preview: tool_preview.as_str(),
                pending_tool_confirm: pending_tool_confirm.as_ref().map(|(call, _, _)| call),
                active_tool_stream: active_tool_stream.as_ref(),
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
            memory_cfg: &memory_cfg,
            sys_cfg: &sys_cfg,
            dog_prompt_text: &dog_state.prompt,
            main_prompt_text: &main_prompt_text,
            context_prompts: &context_prompts,
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
                SettingsFocus::Tabs => "Select tab (←→/Tab, Enter, Esc)".to_string(),
                SettingsFocus::Fields => {
                    if let Some(kind) = selected_kind {
                        match kind {
                            SettingsFieldKind::Provider
                            | SettingsFieldKind::Model
                            | SettingsFieldKind::ReasoningEffort => {
                                format!("{} (Enter)", settings_field_hint(settings_section, kind))
                            }
                            SettingsFieldKind::SseEnabled | SettingsFieldKind::ExecPermission => {
                                format!("{} (Enter)", settings_field_hint(settings_section, kind))
                            }
                            SettingsFieldKind::DogPrompt
                            | SettingsFieldKind::MainPrompt
                            | SettingsFieldKind::ContextMainPrompt => {
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
            //（1）压缩/写日记期间：输入框状态栏强制占用并屏蔽其它提示（toast/菜单/输入等），直到流程结束再解除。
            let locked = if diary_state.active() {
                Some("Updating diary".to_string())
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
        if let Some(pty_line) =
            pty_status_line_override(screen, pty_tabs.as_slice(), pty_active_idx, pty_view)
        {
            anim_enabled = true;
            input_line = pty_line;
        }
        let queue_waiting = send_queue_len(&send_queue_high, &send_queue_low);
        if queue_waiting > 0 {
            input_line = format!("{input_line} / {queue_waiting} 条待发");
        }
        match send_queue_ui {
            SendQueueUiState::Selecting { .. } => {
                input_line = format!("Queue / {queue_waiting} 条待发 · ↑↓选择 Enter编辑 Home退出");
            }
            SendQueueUiState::Editing { id } => {
                input_line = format!("Queue edit #{id} · Alt+↓保存 Home退出");
            }
            SendQueueUiState::Closed => {}
        }
        //（1）输入提示行（横杠上方）：文本变化时触发（●闪烁 → 乱码展开）。
        //（2）需要把它并入 anim_enabled，否则 idle 时 poll_timeout 会很长导致动画不刷新。
        const HINT_ANIM_MAX_FRAMES: usize = 16;
        if input_line != hint_last {
            hint_last = input_line.clone();
            hint_anim_start_tick = spinner_tick;
            //（1）文本变化时触发一次“●闪烁→乱码收敛”，包括 Ready（更有生命感）。
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
        //（1）状态栏右侧动作提示（短暂显示）。存在时也需要刷新 tick，
        //（2）否则在 idle/poll_timeout 较长时会导致提示无法及时消失。
        let mut action_hint_text = String::new();
        let mut action_hint_expired = false;
        if let Some((t0, text)) = action_hint.as_ref() {
            if now.saturating_duration_since(*t0) <= Duration::from_millis(1200) {
                action_hint_text = text.clone();
                anim_enabled = true;
            } else {
                action_hint_expired = true;
            }
        }
        if action_hint_expired {
            action_hint = None;
        }
        let (status_label, status_snapshot) = if sse_enabled {
            if !thinking_text.trim().is_empty() {
                ("Thinking".to_string(), thinking_text.trim().to_string())
            } else if !brief_text.trim().is_empty() {
                ("Brief".to_string(), brief_text.trim().to_string())
            } else if tool_preview_active || tool_preview_pending_idle {
                (
                    info_label_for_tool_preview(&tool_preview),
                    tool_preview.trim().to_string(),
                )
            } else {
                ("Thinking".to_string(), String::new())
            }
        } else {
            ("Thinking".to_string(), String::new())
        };
        cursor = snap_cursor_out_of_pty_snapshot_placeholder(&input, &pending_pty_snapshot, cursor);
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
            let status_active = brief_in_progress || thinking_in_progress || tool_preview_active;
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

        if (brief_pending_idle || tool_preview_pending_idle || thinking_pending_idle)
            && thinking_scroll < thinking_scroll_cap
        {
            thinking_scroll = thinking_scroll_cap;
            needs_redraw = true;
        }
        if brief_pending_idle && thinking_scroll >= thinking_scroll_cap {
            brief_pending_idle = false;
            brief_in_progress = false;
            brief_text.clear();
        } else if tool_preview_pending_idle && thinking_scroll >= thinking_scroll_cap {
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

        //（1）若 PTY 视图从“显示”切回“聊天”，应把历史动画快速推进到尾部：
        //（2）避免因 PTY 占用导致聊天区未渲染，回到聊天后把一大段旧文本从头“重播乱码/打字机”。
        if prev_pty_view && !pty_view {
            chat_anim_fast_forward = true;
            needs_redraw = true;
        }
        prev_pty_view = pty_view;

        if needs_redraw && allow_draw {
            let pulse_dir = mind_pulse.as_ref().map(|p| p.dir);
            //（1）已移除弹窗/窗口栈：思考/工具/正文都在聊天流内以“动态层”呈现。
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
	                let pty_panel_active = is_pty_panel_active(screen, pty_view, pty_tabs.as_slice());
	                let input_inner_w = size.width.saturating_sub(2).max(1) as usize;
	                let input_lines = ui::wrapped_line_count(input_inner_w, input_text);
								let input_cursor_x = if input_active {
									let map = build_cursor_map(input_text, input_inner_w);
									let (cx, _cy) = cursor_xy_from_map(&map, input_cursor.min(input_text.len()));
									Some(cx.saturating_add(1))
								} else {
									None
								};
	                //（1）PTY 展开时仍显示输入框：输入框固定较小高度，避免把终端窗口挤没。
	                let desired_input_h = if pty_panel_active {
	                    2
	                } else {
	                    // 输入框默认加高 1 行：更适合手机小屏编辑（尤其是带工具 JSON/多行提示）。
	                    (input_lines.clamp(4, 10) as u16).saturating_add(1)
	                };
			                let fixed = 1 + 1 + 1 + 1 + 1 + 1 + 1; // top-sep + header + status + header-sep + tabs + input-sep + ctx
			                let avail = size.height.saturating_sub(fixed).max(2); // 至少给 chat+bottom 留 2 行
			                let max_input_h = avail.saturating_sub(1); // 给 chat 至少 1 行
			                let input_h = desired_input_h.min(max_input_h.max(1));
			                let bottom_h = pty_bottom_height(avail, input_h, pty_panel_active);
			                let chunks = Layout::default()
			                    .direction(Direction::Vertical)
			                    .constraints([
			                        Constraint::Length(1), // top-sep
			                        Constraint::Length(1), // header
			                        Constraint::Length(1), // status (Ready/read)
			                        Constraint::Length(1), // header-sep
			                        Constraint::Min(1),    // body
			                        Constraint::Length(1), // tabs
			                        Constraint::Length(1), // input-sep
			                        Constraint::Length(bottom_h),
			                        Constraint::Length(1),
			                    ])
			                    .split(size);

                let main_mode = match mode {
                    Mode::Generating => match active_kind {
                        MindKind::Main => mode,
                        MindKind::Sub | MindKind::Memory => Mode::Idle,
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
                };
                let dog_mode = match mode {
                    Mode::Generating => match active_kind {
                        MindKind::Sub => mode,
                        MindKind::Main | MindKind::Memory => Mode::Idle,
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
                };
                let memory_mode = match mode {
                    Mode::Generating => match active_kind {
                        MindKind::Memory => mode,
                        MindKind::Main | MindKind::Sub => Mode::Idle,
                    },
                    Mode::ExecutingTool | Mode::ApprovingTool => {
                        if active_tool_stream
                            .as_ref()
                            .is_some_and(|s| matches!(s.owner, MindKind::Memory))
                        {
                            mode
                        } else {
                            Mode::Idle
                        }
                    }
                    _ => Mode::Idle,
                };

		                // Top separator line.
ui::draw_separator(
    f,
    ui::DrawSeparatorArgs {
        theme: &theme,
        area: chunks[0],
        mode,
        active_kind,
        user_active: false,
        highlight: matches!(chat_focus, ChatFocus::Chat),
        tick: spinner_tick,
        cursor_x: None,
        hint: "",
    },
);

ui::draw_header(
		                    f,
		                    ui::DrawHeaderArgs {
		                        theme: &theme,
		                        area: chunks[1],
                        center: "",
                        main_mode,
                        dog_mode,
                        memory_mode,
                        user_active: input_active,
                        pulse_dir,
                        tick: spinner_tick,
                        run_secs,
                        right: {
                            let p = match chat_target {
                                MindKind::Main => selected_provider(
                                    ApiConfigKind::Main,
                                    &dog_cfg,
                                    &main_cfg,
                                    &memory_cfg,
                                ),
                                MindKind::Sub => selected_provider(
                                    ApiConfigKind::Dog,
                                    &dog_cfg,
                                    &main_cfg,
                                    &memory_cfg,
                                ),
                                MindKind::Memory => selected_provider(
                                    ApiConfigKind::Memory,
                                    &dog_cfg,
                                    &main_cfg,
                                    &memory_cfg,
                                ),
                            };
                            provider_label(&p)
		                        },
		                    },
		                );

		                //（1）read/Ready 状态栏：放到顶栏区域（位于 AItermux 顶栏与横杠之间）。
		                //（2）Settings 也复用这一行显示状态提示。
		                if screen == Screen::Chat {
		                    ui::draw_hint_line(
		                        f,
		                        ui::DrawHintLineArgs {
		                            theme: &theme,
		                            area: chunks[2],
		                            hint: &input_line,
		                            right_hint: action_hint_text.as_str(),
		                            mode,
		                            active_kind,
		                            user_active: input_active,
		                            tick: spinner_tick,
		                            anim_tick: hint_anim_tick,
		                            anim_seed: hint_anim_seed,
		                        },
		                    );
		                } else if screen == Screen::Settings {
		                    ui::draw_settings_status(
		                        f,
		                        &theme,
		                        chunks[2],
		                        settings.focus,
		                        spinner_tick,
		                        &settings_line,
		                        action_hint_text.as_str(),
		                    );
		                }

		                //（1）顶栏横杠：固定在顶部（标题与正文的分隔）。
		                ui::draw_separator(
		                    f,
		                    ui::DrawSeparatorArgs {
		                        theme: &theme,
		                        area: chunks[3],
		                        mode,
		                        active_kind,
		                        user_active: false,
		                        highlight: matches!(chat_focus, ChatFocus::Chat),
	                        tick: spinner_tick,
        cursor_x: None,
        hint: "",
	                    },
		                );
		                if screen == Screen::Chat {
		                    //（1）Chat 始终可见：PTY 仅占用底部面板（半屏），不再“整块替换聊天区”。
		                    chat_rect_cache = Some(chunks[4]);
	                    if pty_panel_active {
                        //（1）底部区域：上方终端 + 下方输入框
	                        let ih = input_h.min(chunks[7].height.saturating_sub(3).max(1));
	                        let input_area = ratatui::layout::Rect {
	                            x: chunks[7].x,
	                            y: chunks[7]
	                                .y
	                                .saturating_add(chunks[7].height.saturating_sub(ih)),
                            width: chunks[7].width,
                            height: ih,
                        };
	                        let pty_area = ratatui::layout::Rect {
	                            x: chunks[7].x,
	                            y: chunks[7].y,
	                            width: chunks[7].width,
	                            height: chunks[7].height.saturating_sub(ih),
	                        };
	                        input_rect_cache = Some(input_area);
	                        pty_rect_cache = Some(pty_area);
		                    } else {
		                        input_rect_cache = Some(chunks[7]);
		                        pty_rect_cache = None;
		                    }
		                    settings_rect_cache = None;
		                    								let chat_area = ratatui::layout::Rect {
								    x: chunks[4].x,
								    y: chunks[4].y,
								    width: chunks[4].width,
								    height: chunks[4].height.saturating_sub(1).max(1),
								};
								let chat_width = chat_area.width.max(1) as usize;
	                    if chat_anim_fast_forward {
	                        render_cache.prepare(chat_width, core.history.len());
	                        render_cache.fast_forward_all_animations_to_tail(&core);
	                        chat_anim_fast_forward = false;
	                    }
                    let layout = ui::build_chat_layout(ui::BuildChatLinesArgs {
                        theme: &theme,
                        core: &core,
                        render_cache: &mut render_cache,
                        width: chat_width,
                        active_tab: chat_target,
                        streaming_idx: streaming_state.idx,
                        reveal_idx,
                        reveal_len,
                        tick: spinner_tick,
                        selected_msg_idx,
                        tool_global_expanded: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                codex_tool_global_expanded
                            } else {
                                tool_global_expanded
                            }
                        },
                        expanded_tool_idxs: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                &codex_expanded_tool_idxs
                            } else {
                                &expanded_tool_idxs
                            }
                        },
                        collapsed_tool_idxs: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                &codex_collapsed_tool_idxs
                            } else {
                                &collapsed_tool_idxs
                            }
                        },
                        user_global_expanded,
                        expanded_user_idxs: &expanded_user_idxs,
                        collapsed_user_idxs: &collapsed_user_idxs,
                        thinking_idx,
                        thinking_global_expanded: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                codex_thinking_global_expanded
                            } else {
                                thinking_global_expanded
                            }
                        },
                        expanded_thinking_idxs: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                &codex_expanded_thinking_idxs
                            } else {
                                &expanded_thinking_idxs
                            }
                        },
                        collapsed_thinking_idxs: {
                            if chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ) {
                                &codex_collapsed_thinking_idxs
                            } else {
                                &collapsed_thinking_idxs
                            }
                        },
                        details_mode,
                        //（1）Codex：transport 用 SSE，但中转商常是伪流式。
                        //（2）UI 侧禁用流式动效，避免出现乱码/逐行刷屏的错觉与状态错乱。
                        streaming_enabled: sse_enabled
                            && !chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            ),
                        pulse_idx,
                        pulse_style,
                    });
                    msg_line_ranges_cache = layout.msg_line_ranges;
                    let chat_lines = layout.lines;
                    chat_width_cache = chat_width;
		                    let chat_height = chat_area.height.max(1) as usize;
		                    chat_height_cache = chat_height;
                    let total_lines = chat_lines.len();
                    let max_scroll = total_lines.saturating_sub(chat_height) as u16;
                    max_scroll_cache = max_scroll as usize;
                    if let Some(anchor) = pending_scroll_anchor.take() {
                        apply_scroll_anchor(
                            &mut scroll,
                            &mut follow_bottom,
                            anchor,
                            &msg_line_ranges_cache,
                            max_scroll_cache,
                        );
                    }
                    if follow_bottom || scroll > max_scroll {
                        scroll = max_scroll;
                    }
		                    ui::draw_chat(f, &theme, chat_area, chat_lines, scroll);
                    //（1）（已移除弹窗/窗口栈：不在此处绘制“思考窗”）

                    //（1）（已移除弹窗/窗口栈：不在此处绘制“工具/正文弹窗”与“思考置顶窗”）
		                } else {
		                    chat_rect_cache = None;
		                    input_rect_cache = None;
		                    pty_rect_cache = None;
		                    settings_rect_cache = Some(chunks[4]);
		                    let menu_items = settings_menu_items();
		                    let settings_area = chunks[4];
		                    let max_fields = settings_area.height.saturating_sub(5).max(1) as usize;
                    let total = settings_fields.len();
                    let scroll = if total > max_fields {
                        let max_scroll = total.saturating_sub(max_fields);
                        let s = settings
                            .field_index
                            .saturating_sub(max_fields.saturating_sub(1));
                        s.min(max_scroll)
                    } else {
                        0
                    };
                    settings_fields_scroll_cache = scroll;
                    let visible_pairs: Vec<(String, String)> = settings_fields
                        .iter()
                        .skip(scroll)
                        .take(max_fields)
                        .map(|f| {
                            if matches!(f.kind, SettingsFieldKind::Static) {
                                if !f.value.trim().is_empty() {
                                    (String::new(), f.value.clone())
                                } else if f.label.contains("API") {
                                    (String::new(), f.label.to_string())
                                } else {
                                    (String::new(), "─".repeat(512))
                                }
                            } else {
                                (f.label.to_string(), f.value.clone())
                            }
                        })
                        .collect();
                    let visible_selected = settings.field_index.saturating_sub(scroll);
                    let prompt_editor = if matches!(settings.focus, SettingsFocus::Prompt) {
                        Some((settings.edit_buffer.as_str(), settings.edit_cursor))
                    } else {
                        None
                    };
		                    settings_draw = ui::draw_settings(
		                        f,
		                        ui::DrawSettingsArgs {
		                            theme: &theme,
		                            area: chunks[4],
		                            title: settings_section_title(settings_section),
		                            menu_items: &menu_items,
		                            menu_selected: settings.menu_index,
		                            fields: &visible_pairs,
                            field_selected: visible_selected,
                            focus: settings.focus,
                            prompt_editor,
		                            tick: spinner_tick,
		                        },
		                    );
					//（1）未保存确认弹窗：Settings 内部使用。
					settings_confirm_yes_rect_cache = None;
					settings_confirm_no_rect_cache = None;
					if let Some(dlg) = settings.confirm.as_ref() {
					    let anim = spinner_tick.wrapping_sub(dlg.start_tick);
					    if let Some(r) = ui::draw_confirm_dialog(
					        f,
					        &theme,
					        settings_area,
					        dlg.msg.as_str(),
					        matches!(dlg.selected, ConfirmChoice::Yes),
					        anim,
					    ) {
					        settings_confirm_yes_rect_cache = Some(r.yes);
					        settings_confirm_no_rect_cache = Some(r.no);
					    }
					}
		                }

		                //（1）标签页：位于输入框上方（status 下方）。
		                tabs_rect_cache = Some(chunks[5]);
		                ui::draw_chat_tabs(
		                    f,
		                    ui::DrawChatTabsArgs {
		                        theme: &theme,
		                        area: chunks[5],
		                        active_tab: chat_target,
		                        main_busy: !is_idle_like(main_mode),
		                        dog_busy: !is_idle_like(dog_mode),
		                        memory_busy: !is_idle_like(memory_mode),
		                        tick: spinner_tick,
		                        sse_enabled,
		                    },
		                );

		                let mode_input_sep = if input_active { mode } else { Mode::Idle };
		                ui::draw_separator(
		                    f,
		                    ui::DrawSeparatorArgs {
		                        theme: &theme,
		                        area: chunks[6],
		                        mode: mode_input_sep,
		                        active_kind,
		                        user_active: input_active,
		                        highlight: if pty_panel_active {
		                            matches!(screen, Screen::Chat)
		                                && matches!(pty_focus, PtyFocus::Terminal)
		                        } else {
		                            matches!(screen, Screen::Chat) && (matches!(chat_focus, ChatFocus::Input) || input.starts_with('/'))
		                        },
		                        tick: spinner_tick,
        cursor_x: input_cursor_x,
        hint: "",
		                    },
		                );

	                if matches!(screen, Screen::Chat) && pty_panel_active {
	                    let ih = input_h.min(chunks[7].height.saturating_sub(3).max(1));
	                    let input_area = ratatui::layout::Rect {
	                        x: chunks[7].x,
                        y: chunks[7]
                            .y
                            .saturating_add(chunks[7].height.saturating_sub(ih)),
                        width: chunks[7].width,
                        height: ih,
                    };
                    let pty_area = ratatui::layout::Rect {
                        x: chunks[7].x,
                        y: chunks[7].y,
                        width: chunks[7].width,
                        height: chunks[7].height.saturating_sub(ih),
                    };
                    draw_pty_panel(
                        f,
                        DrawPtyPanelArgs {
                            theme: &theme,
                            area: pty_area,
                            pty_tabs: &mut pty_tabs,
                            pty_active_idx,
                            pty_focus,
                        },
                    );
                    ui::draw_input(
                        f,
                        ui::DrawInputArgs {
                            theme: &theme,
                            area: input_area,
                            input: input_text,
                            cursor: input_cursor,
                            //（1）PTY 展开时：输入框仅在“聊天焦点”下可编辑（点击输入框会切到 PtyFocus::Chat）。
                            focused: matches!(screen, Screen::Chat)
                                && matches!(pty_focus, PtyFocus::Chat),
                        },
                    );
                } else {
                    ui::draw_input(
                        f,
                        ui::DrawInputArgs {
                            theme: &theme,
                            area: chunks[7],
                            input: input_text,
                            cursor: input_cursor,
                            focused: matches!(screen, Screen::Chat),
                        },
                    );
                }
	                if let Some(pos) = settings_draw.cursor {
	                    f.set_cursor_position(pos);
	                }
                //（1）底部菜单（与聊天区域互斥覆盖）：Help > “/”命令 > 发送队列。
		                let help_open = matches!(help_ui, HelpUiState::Selecting { .. });
		                if help_open && matches!(screen, Screen::Chat) {
		                    let items = help_menu_items();
		                    let available = chunks[4].height.saturating_sub(1).max(1) as usize;
		                    let menu_h = items.len().min(available).max(1) as u16;
		                    let menu_area = ratatui::layout::Rect {
		                        x: chunks[4].x,
		                        y: chunks[5].y.saturating_sub(menu_h),
		                        width: chunks[4].width,
		                        height: menu_h,
		                    };
                    let sel = match help_ui {
                        HelpUiState::Selecting { selected } => selected,
                        HelpUiState::Closed => 0,
                    };
                    ui::draw_owned_menu(f, &theme, menu_area, &items, sel);
		                } else if menu_open && matches!(screen, Screen::Chat) {
		                    //（1）“/”命令菜单：以横杠为基准向上展开（占用聊天区域底部），避免遮挡提示/解释行。
		                    let available = chunks[4].height.saturating_sub(1).max(1) as usize;
		                    let menu_h = menu_items.len().min(available).max(1) as u16;
		                    let menu_area = ratatui::layout::Rect {
		                        x: chunks[4].x,
		                        y: chunks[5].y.saturating_sub(menu_h),
		                        width: chunks[4].width,
		                        height: menu_h,
		                    };
	                    ui::draw_command_menu(f, &theme, menu_area, &menu_items, command_menu_selected);
		                } else if matches!(screen, Screen::Chat)
		                    && matches!(send_queue_ui, SendQueueUiState::Selecting { .. })
		                {
                    //（1）发送队列菜单（Alt+↑ 打开）：与 “/” 菜单互斥，避免同时覆盖同一块区域。
		                    let available = chunks[4].height.saturating_sub(1).max(1) as usize;
                    let mut items: Vec<ui::OwnedMenuItem> = Vec::new();
                    for m in send_queue_high.iter() {
                        let mut right = m.model_text.trim().replace('\n', " ⏎ ");
                        right = truncate_with_suffix(&right, 120);
                        if right.trim().is_empty() {
                            right = "(empty)".to_string();
                        }
                        items.push(ui::OwnedMenuItem {
                            left: format!("H#{} {}", m.id, mind_label(m.target)),
                            right,
                        });
                    }
                    for m in send_queue_low.iter() {
                        let mut right = m.model_text.trim().replace('\n', " ⏎ ");
                        right = truncate_with_suffix(&right, 120);
                        if right.trim().is_empty() {
                            right = "(empty)".to_string();
                        }
                        items.push(ui::OwnedMenuItem {
                            left: format!("L#{} {}", m.id, mind_label(m.target)),
                            right,
                        });
                    }
		                    if !items.is_empty() {
		                        let menu_h = items.len().min(available).max(1) as u16;
		                        let menu_area = ratatui::layout::Rect {
		                            x: chunks[4].x,
		                            y: chunks[5].y.saturating_sub(menu_h),
		                            width: chunks[4].width,
		                            height: menu_h,
		                        };
                        let sel = match send_queue_ui {
                            SendQueueUiState::Selecting { selected } => selected,
                            _ => 0,
                        };
                        let sel = sel.min(items.len().saturating_sub(1));
                        ui::draw_owned_menu(f, &theme, menu_area, &items, sel);
                    }
                }
                let date_kb = contextmemo_size_kb(&config.contextmemo_path);
                let date_kb_limit = sys_cfg.date_kb_limit;
                let (fastmemo_label, fastmemo_n) = {
                    let raw = std::fs::read_to_string(FASTMEMO_PATH).unwrap_or_default();
                    match fastmemo_max_section_usage(&raw) {
                        Some((label, n)) => (Some(label), n),
                        None => (None, 0),
                    }
                };
                let context_line = ContextLine {
                    fastmemo_label,
                    fastmemo_n,
                    date_kb,
                    date_kb_limit,
                    date_pct: if date_kb_limit == 0 {
                        0
                    } else {
                        calc_pct(date_kb, date_kb_limit.max(1))
                    },
                    run_in_tokens: core.run_in_token_total(),
                    run_out_tokens: core.run_out_token_total(),
                    total_in_tokens: token_totals.total_in_tokens,
                    total_out_tokens: token_totals.total_out_tokens,
                    run_secs,
                    heartbeat_count,
                    response_count,
                };
	                ui::draw_context_bar(f, &theme, chunks[8], context_line);
	                input_width_cache = chunks[6].width.saturating_sub(2).max(1) as usize;
	            })?;
            settings_editor_width_cache = settings_draw
                .editor_rect
                .map(|r| r.width.max(1) as usize)
                .unwrap_or(0);
            settings_editor_rect_cache = settings_draw.editor_rect;
            needs_redraw = false;
            last_draw_at = now;
        }

        //（1）Termux 下某些“软键盘弹出/收回”不稳定触发 Event::Resize。
        //（2）为保证 UI/PTY 能及时重绘并同步尺寸，这里每轮做一次 size 探测。
        if let Ok(rect) = terminal.size() {
            let cur = (rect.width, rect.height);
            if last_term_size != Some(cur) {
                last_term_size = Some(cur);
                needs_redraw = true;
            }
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
                        touch_drag_last_row = None;
                        touch_drag_last_col = None;
                        let up = matches!(me.kind, MouseEventKind::ScrollUp);
                        //（1）Termux 触控滚动会高频触发 ScrollUp/Down。
                        //（2）步长过大会显得跑得比手势快。
                        let delta: u16 = 2;
                        //（1）已移除弹窗/窗口栈。
                        //（2）滚轮滚聊天；交互 PTY 视图滚 PTY 回看。
                        if matches!(screen, Screen::Chat)
                            && apply_mouse_wheel_to_pty_view(
                                pty_view && matches!(pty_focus, PtyFocus::Terminal),
                                pty_tabs.as_mut_slice(),
                                pty_active_idx,
                                up,
                                delta,
                            )
                        {
                        } else if matches!(screen, Screen::Chat)
                            && (matches!(chat_focus, ChatFocus::Input) || input.starts_with('/'))
                        {
                            //（1）输入焦点：滚轮用于移动输入光标。
                            //（2）不滚动聊天。
                            if !input.is_empty() {
                                let width = input_width_cache.max(1);
                                let dir = if up { -1 } else { 1 };
                                cursor = move_prompt_cursor_vertical(&input, width, cursor, dir);
                                last_input_at = Some(now);
                                command_menu_suppress = false;
                            }
                        } else if matches!(screen, Screen::Settings) {
                            let delta: usize = 1;
                            if up {
                                settings.field_index = settings.field_index.saturating_sub(delta);
                            } else {
                                let max = settings_fields.len().saturating_sub(1);
                                settings.field_index = (settings.field_index + delta).min(max);
                            }
                            settings.focus = SettingsFocus::Fields;
                        } else if matches!(screen, Screen::Chat) {
                            if up {
                                scroll = scroll.saturating_sub(delta);
                                follow_bottom = false;
                            } else {
                                let max_scroll = max_scroll_cache as u16;
                                scroll = (scroll + delta).min(max_scroll);
                                follow_bottom = scroll >= max_scroll;
                            }
                        }
                        changed = true;
                    }
                    MouseEventKind::Down(_) => {
                        //（1）触摸/拖拽起点（Termux 下触摸常映射为 Down+Drag）。
                        touch_drag_last_row = Some(me.row);
                        touch_drag_last_col = Some(me.column);
                        touch_drag_anchor_row = Some(me.row);
                        touch_drag_anchor_scroll = Some(scroll);
                        if matches!(screen, Screen::Settings) {
                            let col = me.column;
                            let row = me.row;
                            let contains = |r: ratatui::layout::Rect| {
                                col >= r.x
                                    && col < r.x.saturating_add(r.width)
                                    && row >= r.y
                                    && row < r.y.saturating_add(r.height)
                            };
                            if let Some(area) = settings_rect_cache.filter(|r| contains(*r)) {
                                //（1）未保存确认弹窗：优先处理触控点击（YES/NO）。
                                if settings.confirm.is_some() {
                                    if settings_confirm_yes_rect_cache.is_some_and(contains) {
                                        settings.focus = SettingsFocus::Fields;
                                        reset_settings_edit(&mut settings);
                                        changed = true;
                                        continue;
                                    }
                                    if settings_confirm_no_rect_cache.is_some_and(contains) {
                                        settings.confirm = None;
                                        settings.focus = SettingsFocus::Prompt;
                                        changed = true;
                                        continue;
                                    }
                                }

                                if let Some(r) = settings_editor_rect_cache.filter(|r| contains(*r)) {
                                    settings.focus = SettingsFocus::Prompt;
                                    //（1）提示词编辑：触控点击移动光标。
                                    let click_x = col.saturating_sub(r.x) as usize;
                                    let click_y = row.saturating_sub(r.y) as usize;
                                    let w = r.width.max(1) as usize;
                                    let h = r.height.max(1) as usize;
                                    if !settings.edit_buffer.is_empty() {
                                        settings.edit_cursor = input_cursor_for_click(
                                            &settings.edit_buffer,
                                            w,
                                            h,
                                            settings.edit_cursor,
                                            click_x,
                                            click_y,
                                        );
                                    }
                                    changed = true;
                                } else if row >= area.y && row < area.y.saturating_add(2) {
                                    let rel_row = row.saturating_sub(area.y);
                                    let rel_col = col.saturating_sub(area.x);
                                    if let Some(idx) = settings_tab_index_for_click(
                                        &settings_menu_items(),
                                        area.width.max(1) as usize,
                                        rel_row,
                                        rel_col,
                                    ) {
                                        settings.menu_index = idx;
                                        reset_settings_to_tabs(&mut settings);
                                        settings.field_index = 0;
                                        changed = true;
                                    }
                                } else {
                                    let tabs_h: u16 = 3;
                                    let body_y = area.y.saturating_add(tabs_h);
                                    if row >= body_y {
                                        let body = ratatui::layout::Rect {
                                            x: area.x,
                                            y: body_y,
                                            width: area.width,
                                            height: area.height.saturating_sub(tabs_h),
                                        };
                                        let inner = ratatui::layout::Rect {
                                            x: body.x.saturating_add(1),
                                            y: body.y.saturating_add(1),
                                            width: body.width.saturating_sub(2),
                                            height: body.height.saturating_sub(2),
                                        };
                                        if contains(inner) {
                                            let rel_row = row.saturating_sub(inner.y) as usize;
                                            if !settings_fields.is_empty() {
                                                let idx = settings_fields_scroll_cache
                                                    .saturating_add(rel_row);
                                                settings.field_index = idx
                                                    .min(settings_fields.len().saturating_sub(1));
                                            }
                                            settings.focus = SettingsFocus::Fields;
                                            changed = true;
                                        }
                                    }
                                }
                            }
                        } else if matches!(screen, Screen::Chat) {
                            let col = me.column;
                            let row = me.row;
                            let contains = |r: ratatui::layout::Rect| {
                                col >= r.x
                                    && col < r.x.saturating_add(r.width)
                                    && row >= r.y
                                    && row < r.y.saturating_add(r.height)
                            };
	                            //（1）标签页：单击切换发送目标（Matrix/WatchDog/Memory）。
	                            if let Some(tabs_area) = tabs_rect_cache.filter(|r| contains(*r)) {
	                                let rel_col = col.saturating_sub(tabs_area.x) as usize;
	                                let labels = ["Matrix", "WatchDog", "Memory"];
	                                let kinds = [MindKind::Main, MindKind::Sub, MindKind::Memory];
	
	                                // 触控更友好：不要求精确点中“文字”，而是按“最近 tab 中心”命中。
	                                // 同时避免误点右侧的 "/ SSE" 状态区。
	                                let tabs_w = tabs_area.width.max(1) as usize;
	                                let right = if sse_enabled { "/ SSE" } else { "" };
	                                let right_w = unicode_width::UnicodeWidthStr::width(right);
	                                let right_start = if right_w > 0 {
	                                    tabs_w
	                                        .saturating_sub(2) // "▓░"
	                                        .saturating_sub(1 + right_w) // gap + text
	                                } else {
	                                    tabs_w
	                                };
	
	                                let mut segs: Vec<(MindKind, usize, usize, usize)> = Vec::new();
	                                let mut x = 3usize; // left border (2) + leading space (1)
	                                for i in 0..3 {
	                                    let label_w =
	                                        unicode_width::UnicodeWidthStr::width(labels[i]);
	                                    let w = 1usize // edge
	                                        + 1usize // space
	                                        + label_w
	                                        + 1usize // space
	                                        + 1usize; // edge
	                                    let start = x;
	                                    let end = x.saturating_add(w);
	                                    let center = start.saturating_add(w / 2);
	                                    segs.push((kinds[i], start, end, center));
	                                    x = end.saturating_add(2); // gap between tabs
	                                }

	                                let mut hit: Option<MindKind> = None;
	                                if rel_col < right_start {
	                                    if let (Some(first), Some(last)) = (segs.first(), segs.last()) {
	                                        let left = first.1.saturating_sub(3);
	                                        let right = last.2.saturating_add(3);
	                                        if rel_col >= left && rel_col < right {
	                                            let mut best = segs[0].0;
	                                            let mut best_dist = usize::MAX;
	                                            for (k, _s, _e, c) in segs.iter().copied() {
	                                                let dist = if rel_col >= c {
	                                                    rel_col - c
	                                                } else {
	                                                    c - rel_col
	                                                };
	                                                if dist < best_dist {
	                                                    best_dist = dist;
	                                                    best = k;
	                                                }
	                                            }
	                                            hit = Some(best);
	                                        }
	                                    }
	                                }

	                                if let Some(new_target) = hit {
	                                    if chat_target != new_target {
	                                        chat_target = new_target;
	                                        //（1）切 tab：不改变 active_kind，不重置上下文；只影响“下一条用户消息”发给谁。
	                                        selected_msg_idx = None;
	                                        follow_bottom = true;
                                        scroll = u16::MAX;
                                        action_hint =
                                            Some((now, format!("Tab: {}", mind_label(new_target))));
                                        changed = true;
                                    }
                                }
                            }
                            //（1）PTY 面板：单击切换焦点到终端（用于终端滚动/交互）。
                            if pty_view
                                && !pty_tabs.is_empty()
                                && pty_rect_cache.is_some_and(contains)
                            {
                                pty_focus = PtyFocus::Terminal;
                                chat_focus = ChatFocus::Chat;
                                selected_msg_idx = None;
                                action_hint = Some((now, "焦点: 终端".to_string()));
                                changed = true;
                            } else if let Some(area) = input_rect_cache.filter(|r| contains(*r)) {
                                let prev_focus = chat_focus;
                                let prev_selected = selected_msg_idx;
                                let prev_cursor = cursor;
                                chat_focus = ChatFocus::Input;
                                if pty_view && !pty_tabs.is_empty() {
                                    pty_focus = PtyFocus::Chat;
                                }
                                selected_msg_idx = None;
                                action_hint = Some((now, "焦点: 输入".to_string()));

                                //（1）单击输入框：把光标跳到点击位置（不依赖 Termux 是否上报水平拖拽列变化）。
                                let inner_x = area.x.saturating_add(1);
                                let inner_y = area.y;
                                let inner_w = area.width.saturating_sub(2).max(1) as usize;
                                let inner_h = area.height.max(1) as usize;
                                let click_x = col.saturating_sub(inner_x) as usize;
                                let click_y = row.saturating_sub(inner_y) as usize;
                                if !input.is_empty() {
                                    cursor = input_cursor_for_click(
                                        &input, inner_w, inner_h, cursor, click_x, click_y,
                                    );
                                    last_input_at = Some(Instant::now());
                                    command_menu_suppress = false;
                                }
                                if !matches!(prev_focus, ChatFocus::Input)
                                    || prev_selected != selected_msg_idx
                                    || prev_cursor != cursor
                                {
                                    changed = true;
                                }
                            } else if let Some(area) = chat_rect_cache.filter(|r| contains(*r)) {
                                //（1）点击聊天区：切焦点 + 支持“点哪条选哪条，再点取消选中”。
                                let prev_focus = chat_focus;
                                let prev_selected = selected_msg_idx;
                                let prev_pty_focus = pty_focus;
                                chat_focus = ChatFocus::Chat;
                                if pty_view && !pty_tabs.is_empty() {
                                    pty_focus = PtyFocus::Chat;
                                }
                                if !matches!(prev_focus, ChatFocus::Chat)
                                    || (pty_view
                                        && !pty_tabs.is_empty()
                                        && !matches!(prev_pty_focus, PtyFocus::Chat))
                                {
                                    action_hint = Some((now, "焦点: 聊天".to_string()));
                                }

                                //（1）避免“切焦点 + 选中”同时触发：
                                //（2）先用一次点击把焦点切到聊天区
                                //（3）已在聊天区焦点时，再点击消息才会选中/取消选中
                                let pty_panel_active = pty_view && !pty_tabs.is_empty();
                                let allow_select = matches!(prev_focus, ChatFocus::Chat)
                                    && (!pty_panel_active
                                        || matches!(prev_pty_focus, PtyFocus::Chat));
                                if allow_select {
                                    let rel_row = row.saturating_sub(area.y) as usize;
                                    let line = (scroll as usize).saturating_add(rel_row);
                                    let mut clicked: Option<usize> = None;
                                    for (idx, range) in msg_line_ranges_cache.iter().enumerate() {
                                        if let Some((start, end)) = range
                                            && *start <= line
                                            && line < *end
                                        {
                                            clicked = Some(idx);
                                            break;
                                        }
                                    }
                                    if let Some(idx) = clicked {
                                        if prev_selected == Some(idx) {
                                            selected_msg_idx = None;
                                            action_hint = Some((now, "取消选中".to_string()));
                                        } else {
                                            selected_msg_idx = Some(idx);
                                            action_hint = Some((now, "选中消息".to_string()));
                                        }
                                    }
                                    if !matches!(prev_focus, ChatFocus::Chat)
                                        || prev_selected != selected_msg_idx
                                    {
                                        changed = true;
                                    }
                                } else if !matches!(prev_focus, ChatFocus::Chat)
                                    || (pty_panel_active
                                        && !matches!(prev_pty_focus, PtyFocus::Chat))
                                {
                                    changed = true;
                                }
                            }
                        }
                    }
                    MouseEventKind::Drag(_) => {
                        let prev_row = touch_drag_last_row.unwrap_or(me.row);
                        let prev_col = touch_drag_last_col.unwrap_or(me.column);
                        let dy = prev_row as i32 - me.row as i32;
                        let dx = prev_col as i32 - me.column as i32;
                        touch_drag_last_row = Some(me.row);
                        touch_drag_last_col = Some(me.column);
                        if matches!(screen, Screen::Chat) && (matches!(chat_focus, ChatFocus::Input) || input.starts_with('/'))
                        {
                            //（1）输入焦点：手势滑动用于移动输入光标（↑↓←→）。
                            let step_x = dx.unsigned_abs().min(8) as usize;
                            if step_x > 0 && !input.is_empty() {
                                for _ in 0..step_x {
                                    if dx > 0 {
                                        if cursor > 0 {
                                            cursor = prev_char_boundary(&input, cursor);
                                        }
                                    } else if cursor < input.len() {
                                        cursor = next_char_boundary(&input, cursor);
                                    }
                                }
                                last_input_at = Some(now);
                                command_menu_suppress = false;
                                changed = true;
                            }
                            let step_y = dy.unsigned_abs().min(6) as usize;
                            if step_y > 0 && !input.is_empty() {
                                let width = input_width_cache.max(1);
                                //（1）以体感为准：手指下滑 => 光标下移；手指上滑 => 光标上移。
                                let dir = if dy > 0 { 1 } else { -1 };
                                for _ in 0..step_y {
                                    cursor =
                                        move_prompt_cursor_vertical(&input, width, cursor, dir);
                                }
                                last_input_at = Some(now);
                                command_menu_suppress = false;
                                changed = true;
                            }
                        } else if dy != 0 {
                            //（1）触控拖拽：用锚定起点计算绝对偏移。
                            //（2）避免高频 Drag 造成累计过快。
                            let (dy_total, base_scroll) = if let Some(anchor_row) =
                                touch_drag_anchor_row
                                && let Some(anchor_scroll) = touch_drag_anchor_scroll
                            {
                                (anchor_row as i32 - me.row as i32, anchor_scroll)
                            } else {
                                (dy, scroll)
                            };
                            //（1）Termux 拖拽行坐标是格子级，dy 容易跳变。
                            //（2）用温和步长映射：约 2 行手势 -> 1 行内容。
                            let abs = dy_total.unsigned_abs().min(128) as u16;
                            let delta = abs.div_ceil(2).max(1);
                            if matches!(screen, Screen::Chat)
                                && apply_touch_drag_to_pty_view(
                                    pty_view && matches!(pty_focus, PtyFocus::Terminal),
                                    pty_tabs.as_mut_slice(),
                                    pty_active_idx,
                                    dy_total,
                                    delta.min(4),
                                )
                            {
                            } else if dy_total > 0 {
                                let max_scroll = max_scroll_cache as u16;
                                scroll = (base_scroll + delta).min(max_scroll);
                                follow_bottom = scroll >= max_scroll;
                            } else {
                                scroll = base_scroll.saturating_sub(delta);
                                follow_bottom = false;
                            }
                            changed = true;
                        }
                    }
                    MouseEventKind::Up(_) => {
                        touch_drag_last_row = None;
                        touch_drag_last_col = None;
                        touch_drag_anchor_row = None;
                        touch_drag_anchor_scroll = None;
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
                let shift = key.modifiers.contains(KeyModifiers::SHIFT);
                //（1）标签页切换：Shift+←/→（Matrix/WatchDog/Memory）。
                if matches!(screen, Screen::Chat)
                    && shift
                    && !alt
                    && matches!(key.code, KeyCode::Left | KeyCode::Right)
                {
                    let forward = matches!(key.code, KeyCode::Right);
                    let next = match (chat_target, forward) {
                        (MindKind::Main, true) => MindKind::Sub,
                        (MindKind::Sub, true) => MindKind::Memory,
                        (MindKind::Memory, true) => MindKind::Main,
                        (MindKind::Main, false) => MindKind::Memory,
                        (MindKind::Memory, false) => MindKind::Sub,
                        (MindKind::Sub, false) => MindKind::Main,
                    };
                    if next != chat_target {
                        chat_target = next;
                        selected_msg_idx = None;
                        follow_bottom = true;
                        scroll = u16::MAX;
                        action_hint = Some((now, format!("Tab: {}", mind_label(next))));
                    }
                    continue;
                }
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
                                    let api_kind = spec.api_kind.unwrap_or(settings.api_kind);
                                    if matches!(spec.kind, SettingsFieldKind::Static) {
                                        continue;
                                    }
                                    if matches!(spec.kind, SettingsFieldKind::Provider) {
                                        let current = selected_provider(
                                            api_kind,
                                            &dog_cfg,
                                            &main_cfg,
                                            &memory_cfg,
                                        );
                                        let next = next_provider(&current);
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            api_kind,
                                            value: &next,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            memory_cfg: &mut memory_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            memory_cfg_path: &memory_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            memory_state: &mut memory_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                            main_prompt_text: &mut main_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            memory_client: &mut memory_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                        });
                                        continue;
                                    }
                                    if matches!(spec.kind, SettingsFieldKind::Model) {
                                        let provider = selected_provider(
                                            api_kind,
                                            &dog_cfg,
                                            &main_cfg,
                                            &memory_cfg,
                                        );
                                        let current = selected_model(
                                            api_kind,
                                            &dog_cfg,
                                            &main_cfg,
                                            &memory_cfg,
                                        );
                                        let next = next_model(&current, &provider);
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            api_kind,
                                            value: &next,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            memory_cfg: &mut memory_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            memory_cfg_path: &memory_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            memory_state: &mut memory_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                            main_prompt_text: &mut main_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            memory_client: &mut memory_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                        });
                                        continue;
                                    }
                                    if matches!(spec.kind, SettingsFieldKind::ReasoningEffort) {
                                        let provider = selected_provider(
                                            api_kind,
                                            &dog_cfg,
                                            &main_cfg,
                                            &memory_cfg,
                                        );
                                        if !api::capabilities(&provider).supports_reasoning_effort {
                                            set_settings_notice(
                                                &mut settings,
                                                now,
                                                "当前供应商不使用 Reasoning".to_string(),
                                            );
                                            continue;
                                        }
                                        let current = selected_reasoning_effort(
                                            api_kind,
                                            &dog_cfg,
                                            &main_cfg,
                                            &memory_cfg,
                                        );
                                        let next = next_reasoning_effort(&current);
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind: spec.kind,
                                            section,
                                            api_kind,
                                            value: &next,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            memory_cfg: &mut memory_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            memory_cfg_path: &memory_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            memory_state: &mut memory_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                            main_prompt_text: &mut main_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            memory_client: &mut memory_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                        });
                                        continue;
                                    }
                                    if matches!(
                                        spec.kind,
                                        SettingsFieldKind::SseEnabled
                                            | SettingsFieldKind::ExecPermission
                                    ) {
                                        let value = match spec.kind {
                                            SettingsFieldKind::SseEnabled => {
                                                if sys_cfg.sse_enabled { "off" } else { "on" }
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
                                            api_kind,
                                            value,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            memory_cfg: &mut memory_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            memory_cfg_path: &memory_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            memory_state: &mut memory_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                            main_prompt_text: &mut main_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            memory_client: &mut memory_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                        });
                                        sync_system_toggles(&sys_cfg, &mut sse_enabled);
                                        continue;
                                    }

                                    settings.edit_kind = Some(spec.kind);
                                    settings.edit_api_kind = spec.api_kind;
                                    settings.edit_prompt_path = match spec.kind {
                                        SettingsFieldKind::PromptFile => spec.prompt_path.clone(),
                                        SettingsFieldKind::DogPrompt => Some(PathBuf::from(&dog_cfg.prompt_path)),
                                        SettingsFieldKind::MainPrompt => Some(PathBuf::from(&main_cfg.prompt_path)),
                                        SettingsFieldKind::ContextMainPrompt => Some(context_prompt_path.to_path_buf()),
                                        _ => None,
                                    };

                                    settings.edit_buffer = if matches!(spec.kind, SettingsFieldKind::PromptFile) {
                                        if let Some(path) = spec.prompt_path.as_ref() {
                                            std::fs::read_to_string(path).unwrap_or_default()
                                        } else {
                                            String::new()
                                        }
                                    } else {
                                        field_raw_value(FieldRawValueArgs {
                                            kind: spec.kind,
                                            section,
                                            api_kind,
                                            dog_cfg: &dog_cfg,
                                            main_cfg: &main_cfg,
                                            memory_cfg: &memory_cfg,
                                            sys_cfg: &sys_cfg,
                                            dog_prompt_text: &dog_state.prompt,
                                            main_prompt_text: &main_prompt_text,
                                            context_prompts: &context_prompts,
                                        })
                                    };
                                    settings.edit_original = settings.edit_buffer.clone();
                                    settings.edit_cursor = settings.edit_buffer.len();
                                    settings.confirm = None;
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
                            let api_kind = settings.edit_api_kind.unwrap_or(settings.api_kind);
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
                                        api_kind,
                                        buffer: &buffer,
                                        settings: &mut settings,
                                        now,
                                        dog_cfg: &mut dog_cfg,
                                        main_cfg: &mut main_cfg,
                                        memory_cfg: &mut memory_cfg,
                                        sys_cfg: &mut sys_cfg,
                                        dog_cfg_path: &dog_cfg_path,
                                        main_cfg_path: &main_cfg_path,
                                        memory_cfg_path: &memory_cfg_path,
                                        sys_cfg_path: &sys_cfg_path,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        memory_state: &mut memory_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                        main_prompt_text: &mut main_prompt_text,
                                        dog_client: &mut dog_client,
                                        main_client: &mut main_client,
                                        memory_client: &mut memory_client,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                        context_prompts: &mut context_prompts,
                                        context_prompt_path: &context_prompt_path,
                                        sse_enabled: &mut sse_enabled,
                                    });
                                }
                                KeyCode::Enter => {
                                    let buffer = settings.edit_buffer.clone();
                                    commit_settings_input(CommitSettingsInputArgs {
                                        kind,
                                        section,
                                        api_kind,
                                        buffer: &buffer,
                                        settings: &mut settings,
                                        now,
                                        dog_cfg: &mut dog_cfg,
                                        main_cfg: &mut main_cfg,
                                        memory_cfg: &mut memory_cfg,
                                        sys_cfg: &mut sys_cfg,
                                        dog_cfg_path: &dog_cfg_path,
                                        main_cfg_path: &main_cfg_path,
                                        memory_cfg_path: &memory_cfg_path,
                                        sys_cfg_path: &sys_cfg_path,
                                        dog_state: &mut dog_state,
                                        main_state: &mut main_state,
                                        memory_state: &mut memory_state,
                                        mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                        mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                        mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                        main_prompt_text: &mut main_prompt_text,
                                        dog_client: &mut dog_client,
                                        main_client: &mut main_client,
                                        memory_client: &mut memory_client,
                                        sys_log: &mut sys_log,
                                        sys_log_limit: config.sys_log_limit,
                                        context_prompts: &mut context_prompts,
                                        context_prompt_path: &context_prompt_path,
                                        sse_enabled: &mut sse_enabled,
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
                            if let Some(mut dlg) = settings.confirm.clone() {
                                match key.code {
                                    KeyCode::Left | KeyCode::Right => {
                                        dlg.selected = if matches!(dlg.selected, ConfirmChoice::Yes) {
                                            ConfirmChoice::No
                                        } else {
                                            ConfirmChoice::Yes
                                        };
                                        settings.confirm = Some(dlg);
                                        continue;
                                    }
                                    KeyCode::Esc => {
                                        settings.confirm = None;
                                        continue;
                                    }
                                    KeyCode::Enter => {
                                        if matches!(dlg.selected, ConfirmChoice::Yes) {
                                            settings.focus = SettingsFocus::Fields;
                                            reset_settings_edit(&mut settings);
                                        } else {
                                            settings.confirm = None;
                                        }
                                        continue;
                                    }
                                    _ => {}
                                }
                                settings.confirm = Some(dlg);
                            }
                            match key.code {
                                KeyCode::Esc => {
                                    if settings.edit_buffer != settings.edit_original {
                                        settings.confirm = Some(ConfirmDialogState {
                                            msg: "当前改动未保存，是否退出".to_string(),
                                            selected: ConfirmChoice::No,
                                            start_tick: spinner_tick,
                                        });
                                    } else {
                                        settings.focus = SettingsFocus::Fields;
                                        reset_settings_edit(&mut settings);
                                    }
                                }
                                KeyCode::Char('s')
                                    if key.modifiers.contains(KeyModifiers::CONTROL) =>
                                {
                                    if matches!(kind, SettingsFieldKind::PromptFile) {
                                        if let Some(path) = settings.edit_prompt_path.clone() {
                                            let text = settings.edit_buffer.clone();
                                            match store_prompt_file(&path, &text) {
                                                Ok(()) => {
                                                    settings.edit_original = text.clone();
                                                    if path == PathBuf::from(&dog_cfg.prompt_path) {
                                                        dog_state.prompt = text.clone();
                                                        dog_state.reset_context();
                                                    }
                                                    if path == PathBuf::from(&main_cfg.prompt_path) {
                                                        main_prompt_text = text.clone();
                                                        main_state.prompt = main_prompt_text.clone();
                                                        main_state.reset_context();
                                                    }
                                                    if path == PathBuf::from(&memory_cfg.prompt_path) {
                                                        memory_state.prompt = text.clone();
                                                        memory_state.reset_context();
                                                    }
                                                    if path == context_prompt_path.to_path_buf() {
                                                        context_prompts.main_prompt = text.clone();
                                                    }
                                                    set_settings_notice(&mut settings, now, "已保存".to_string());
                                                }
                                                Err(e) => {
                                                    set_settings_notice(&mut settings, now, format!("保存失败：{e}"));
                                                }
                                            }
                                        } else {
                                            set_settings_notice(&mut settings, now, "保存失败：path 为空".to_string());
                                        }
                                    } else {
                                        let buffer = settings.edit_buffer.clone();
                                        apply_settings_with_notice(ApplySettingsWithNoticeArgs {
                                            kind,
                                            section,
                                            api_kind: settings.api_kind,
                                            value: &buffer,
                                            settings: &mut settings,
                                            now,
                                            dog_cfg: &mut dog_cfg,
                                            main_cfg: &mut main_cfg,
                                            memory_cfg: &mut memory_cfg,
                                            sys_cfg: &mut sys_cfg,
                                            dog_cfg_path: &dog_cfg_path,
                                            main_cfg_path: &main_cfg_path,
                                            memory_cfg_path: &memory_cfg_path,
                                            sys_cfg_path: &sys_cfg_path,
                                            dog_state: &mut dog_state,
                                            main_state: &mut main_state,
                                            memory_state: &mut memory_state,
                                            mind_ctx_idx_main: &mut mind_ctx_idx_main,
                                            mind_ctx_idx_dog: &mut mind_ctx_idx_dog,
                                            mind_ctx_idx_memory: &mut mind_ctx_idx_memory,
                                            main_prompt_text: &mut main_prompt_text,
                                            dog_client: &mut dog_client,
                                            main_client: &mut main_client,
                                            memory_client: &mut memory_client,
                                            sys_log: &mut sys_log,
                                            sys_log_limit: config.sys_log_limit,
                                            context_prompts: &mut context_prompts,
                                            context_prompt_path: &context_prompt_path,
                                        });
                                        settings.edit_original = settings.edit_buffer.clone();
                                    }
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

                //（1）交互 PTY 视图：优先处理（避免被命令菜单/选择模式等逻辑吃掉）。
                //（2）但 Home 在“消息已选中”时优先用于退出选择（避免 Home 被 PTY 先吃掉导致逻辑冲突）。
                if matches!(screen, Screen::Chat)
                    && (!matches!(key.code, KeyCode::Home)
                        || selected_msg_idx.is_none()
                        || ctrl
                        || alt)
                    && handle_pty_view_key(HandlePtyViewKeyArgs {
                        now,
                        key,
                        ctrl,
                        alt,
                        pty_tabs: &mut pty_tabs,
                        pty_active_idx: &mut pty_active_idx,
                        pty_focus: &mut pty_focus,
                        pty_handles: &mut pty_handles,
                        pty_view: &mut pty_view,
                        pending_pty_snapshot: &mut pending_pty_snapshot,
                        input: &mut input,
                        cursor: &mut cursor,
                        input_chars: &mut input_chars,
                        last_input_at: &mut last_input_at,
                        last_esc_at: &mut last_esc_at,
                        sys_log: &mut sys_log,
                        sys_log_limit: config.sys_log_limit,
                        action_hint: &mut action_hint,
                    })
                {
                    continue;
                }
                //（1）交互 PTY 存在但未显示：Home 快速切回终端视图。
                if matches!(screen, Screen::Chat)
                    && try_show_pty_view_on_home(TryShowPtyViewOnHomeArgs {
                        key,
                        ctrl,
                        alt,
                        pty_view: &mut pty_view,
                        pty_tabs: pty_tabs.as_slice(),
                        pty_focus: &mut pty_focus,
                        selected_msg_idx,
                        send_queue_closed: matches!(send_queue_ui, SendQueueUiState::Closed),
                        menu_open,
                    })
                {
                    action_hint = Some((now, "Home: 打开终端".to_string()));
                    continue;
                }

                if paste_drop_until.is_some_and(|t| now < t)
                    && matches!(key.code, KeyCode::Char(_) | KeyCode::Enter)
                {
                    continue;
                }

                //（1）Help 菜单（/Help）：↑↓选择；Enter 注入系统消息；Esc/Home 关闭。
                if matches!(screen, Screen::Chat)
                    && matches!(help_ui, HelpUiState::Selecting { .. })
                    && !menu_open
                {
                    let total = help_menu_items().len().max(1);
                    let selected = match help_ui {
                        HelpUiState::Selecting { selected } => {
                            selected.min(total.saturating_sub(1))
                        }
                        HelpUiState::Closed => 0,
                    };
                    match key.code {
                        KeyCode::Up => {
                            help_ui = HelpUiState::Selecting {
                                selected: selected.saturating_sub(1),
                            };
                            continue;
                        }
                        KeyCode::Down => {
                            help_ui = HelpUiState::Selecting {
                                selected: (selected + 1).min(total.saturating_sub(1)),
                            };
                            continue;
                        }
                        KeyCode::Esc | KeyCode::Home | KeyCode::Left => {
                            help_ui = HelpUiState::Closed;
                            continue;
                        }
                        KeyCode::Enter => {
                            //（1）目前仅一个条目：快捷键（keymap.txt）
                            let path = resolve_config_path(HELP_KEYMAP_PATH, true);
                            match std::fs::read_to_string(&path) {
                                Ok(text) => {
                                    push_system_and_log(
                                        &mut core,
                                        &mut metamemo,
                                        &mut context_usage,
                                        None,
                                        text.trim_end(),
                                    );
                                    push_sys_log(
                                        &mut sys_log,
                                        config.sys_log_limit,
                                        "Help: 快捷键已发送",
                                    );
                                    chat_focus = ChatFocus::Chat;
                                    selected_msg_idx = None;
                                    scroll = u16::MAX;
                                    follow_bottom = true;
                                }
                                Err(e) => {
                                    push_sys_log(
                                        &mut sys_log,
                                        config.sys_log_limit,
                                        format!("Help: 读取失败：{} ({})", path.display(), e),
                                    );
                                }
                            }
                            help_ui = HelpUiState::Closed;
                            continue;
                        }
                        _ => {}
                    }
                }

                //（1）发送队列 UI：Alt+↑ 打开；↑↓选择；Enter 编辑。
                if matches!(screen, Screen::Chat)
                    && !menu_open
                    && matches!(help_ui, HelpUiState::Closed)
                {
                    let total = send_queue_len(&send_queue_high, &send_queue_low);
                    match send_queue_ui {
                        SendQueueUiState::Selecting { selected } => {
                            if total == 0 {
                                send_queue_ui = SendQueueUiState::Closed;
                                push_sys_log(&mut sys_log, config.sys_log_limit, "队列为空");
                                continue;
                            }
                            match key.code {
                                KeyCode::Up => {
                                    send_queue_ui = SendQueueUiState::Selecting {
                                        selected: selected.saturating_sub(1),
                                    };
                                    continue;
                                }
                                KeyCode::Down => {
                                    let max = total.saturating_sub(1);
                                    send_queue_ui = SendQueueUiState::Selecting {
                                        selected: (selected + 1).min(max),
                                    };
                                    continue;
                                }
                                KeyCode::Home => {
                                    send_queue_ui = SendQueueUiState::Closed;
                                    continue;
                                }
                                KeyCode::Enter => {
                                    let sel = selected.min(total.saturating_sub(1));
                                    if let Some(m) =
                                        send_queue_get_flat(&send_queue_high, &send_queue_low, sel)
                                    {
                                        reset_input_buffer(
                                            &mut input,
                                            &mut cursor,
                                            &mut input_chars,
                                            &mut last_input_at,
                                        );
                                        input.push_str(&m.model_text);
                                        cursor = input.len();
                                        input_chars = count_chars(&input);
                                        last_input_at = Some(now);
                                        command_menu_suppress = true;
                                        send_queue_ui = SendQueueUiState::Editing { id: m.id };
                                    } else {
                                        send_queue_ui = SendQueueUiState::Closed;
                                    }
                                    continue;
                                }
                                _ => {}
                            }
                        }
                        SendQueueUiState::Editing { id } => {
                            if alt && !ctrl && matches!(key.code, KeyCode::Down) {
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
                                let ok = send_queue_update_text_id(
                                    &mut send_queue_high,
                                    &mut send_queue_low,
                                    id,
                                    input.clone(),
                                );
                                reset_input_buffer(
                                    &mut input,
                                    &mut cursor,
                                    &mut input_chars,
                                    &mut last_input_at,
                                );
                                send_queue_ui = SendQueueUiState::Closed;
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    if ok {
                                        format!("队列消息已保存：#{id}")
                                    } else {
                                        format!("队列消息不存在：#{id}")
                                    },
                                );
                                continue;
                            }
                        }
                        SendQueueUiState::Closed => {}
                    }
                    if alt && !ctrl && matches!(key.code, KeyCode::Up) {
                        if total == 0 {
                            push_sys_log(&mut sys_log, config.sys_log_limit, "队列为空");
                        } else {
                            send_queue_ui = SendQueueUiState::Selecting { selected: 0 };
                        }
                        continue;
                    }
                }
                //（1）Alt+↓：复制选中消息（避免占用 Enter；兼容用户单手操作）。
                if alt
                    && matches!(key.code, KeyCode::Down)
                    && input.trim().is_empty()
                    && is_idle_like(mode)
                    && let Some(sel) = selected_msg_idx
                    && let Some(msg) = core.history.get(sel)
                {
                    if try_termux_clipboard_set(msg.text.trim_end()) {
                        push_sys_log(
                            &mut sys_log,
                            config.sys_log_limit,
                            "Copied selected message",
                        );
                        action_hint = Some((now, "已复制".to_string()));
                    } else {
                        push_sys_log(
                            &mut sys_log,
                            config.sys_log_limit,
                            "Copy failed: termux-clipboard-set unavailable",
                        );
                        action_hint = Some((now, "复制失败".to_string()));
                    }
                    continue;
                }

                //（1）输入焦点快捷键：允许在输入框里一键切到聊天区进行“浏览/展开/置底”。
                //（2）目的：提升单手/触控操作效率，同时不破坏“按键按焦点区域生效”的主规则。
                if matches!(screen, Screen::Chat)
                    && (matches!(chat_focus, ChatFocus::Input) || input.starts_with('/'))
                    && matches!(send_queue_ui, SendQueueUiState::Closed)
                    && !ctrl
                    && !pty_view
                {
                    let codex_mode = chat_target_is_codex(
                        chat_target,
                        &dog_client,
                        &main_client,
                        &memory_client,
                    );
                    let (thinking_global, expanded_thinking_ref, collapsed_thinking_ref) =
                        if codex_mode {
                            (
                                codex_thinking_global_expanded,
                                &codex_expanded_thinking_idxs,
                                &codex_collapsed_thinking_idxs,
                            )
                        } else {
                            (
                                thinking_global_expanded,
                                &expanded_thinking_idxs,
                                &collapsed_thinking_idxs,
                            )
                        };
                    match key.code {
                        KeyCode::PageUp => {
                            chat_focus = ChatFocus::Chat;
                            command_menu_suppress = true;
                            selected_msg_idx = select_prev_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                thinking_global,
                                expanded_thinking_ref,
                                collapsed_thinking_ref,
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
                            action_hint = Some((now, "PgUp: 选中消息".to_string()));
                            continue;
                        }
                        KeyCode::PageDown => {
                            chat_focus = ChatFocus::Chat;
                            command_menu_suppress = true;
                            //（1）PgDn 首次触发也进入选择：从最后一条消息开始选中（符合 PgUp 行为），
                            //（2）之后再 PgDn 才是“向下选”（通常会停在最后一条）。
                            if selected_msg_idx.is_some() {
                                selected_msg_idx = select_next_chat_message(
                                    &core,
                                    selected_msg_idx,
                                    thinking_idx,
                                    thinking_global,
                                    expanded_thinking_ref,
                                    collapsed_thinking_ref,
                                    details_mode,
                                );
                            } else {
                                selected_msg_idx = select_last_chat_message(
                                    &core,
                                    thinking_idx,
                                    thinking_global,
                                    expanded_thinking_ref,
                                    collapsed_thinking_ref,
                                    details_mode,
                                );
                            }
                            follow_bottom = false;
                            ensure_selected_visible(
                                &mut scroll,
                                &mut follow_bottom,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                                chat_height_cache,
                                max_scroll_cache,
                            );
                            action_hint = Some((now, "PgDn: 选中消息".to_string()));
                            continue;
                        }
                        KeyCode::End if !shift => {
                            chat_focus = ChatFocus::Chat;
                            command_menu_suppress = true;
                            scroll_restore_before_end = Some((scroll, follow_bottom));
                            scroll = max_scroll_cache as u16;
                            follow_bottom = true;
                            action_hint = Some((now, "End: 置底".to_string()));
                            continue;
                        }
                        KeyCode::BackTab => {
                            //（1）Shift+Tab：形态切换（简约 -> 工具 -> 思考 -> 简约）
                            chat_focus = ChatFocus::Chat;
                            command_menu_suppress = true;
                            render_cache.fast_forward_all_animations_to_tail(&core);
                            if reveal_idx.is_some() {
                                reveal_idx = None;
                                reveal_len = 0;
                            }
                            let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                            if !at_bottom {
                                pending_scroll_anchor = capture_scroll_anchor(
                                    scroll,
                                    max_scroll_cache,
                                    selected_msg_idx,
                                    &msg_line_ranges_cache,
                                );
                            }
                            let next_cycle = |tool: bool, thinking: bool| -> (bool, bool) {
                                match (tool, thinking) {
                                    (false, false) => (true, false),
                                    (true, false) => (false, true),
                                    (false, true) => (false, false),
                                    (true, true) => (true, false),
                                }
                            };
                            let (tool_cur, think_cur) = if codex_mode {
                                (codex_tool_global_expanded, codex_thinking_global_expanded)
                            } else {
                                (tool_global_expanded, thinking_global_expanded)
                            };
                            let (want_tool, want_think) = next_cycle(tool_cur, think_cur);
                            if codex_mode {
                                codex_tool_global_expanded = want_tool;
                                codex_thinking_global_expanded = want_think;
                                codex_expanded_tool_idxs.clear();
                                codex_collapsed_tool_idxs.clear();
                                codex_expanded_thinking_idxs.clear();
                                codex_collapsed_thinking_idxs.clear();
                            } else {
                                tool_global_expanded = want_tool;
                                thinking_global_expanded = want_think;
                                expanded_tool_idxs.clear();
                                collapsed_tool_idxs.clear();
                                expanded_thinking_idxs.clear();
                                collapsed_thinking_idxs.clear();
                            }
                            render_cache.invalidate_all();
                            let hint = if want_tool {
                                "Shift+Tab: 工具"
                            } else if want_think {
                                "Shift+Tab: 思考"
                            } else {
                                "Shift+Tab: 简约"
                            };
                            push_sys_log(&mut sys_log, config.sys_log_limit, hint);
                            action_hint = Some((now, hint.to_string()));
                            continue;
                        }
                        KeyCode::Tab => {
                            //（1）Tab：全局显示切换
                            chat_focus = ChatFocus::Chat;
                            command_menu_suppress = true;
                            render_cache.fast_forward_all_animations_to_tail(&core);
                            if reveal_idx.is_some() {
                                reveal_idx = None;
                                reveal_len = 0;
                            }
                            let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                            if !at_bottom {
                                pending_scroll_anchor = capture_scroll_anchor(
                                    scroll,
                                    max_scroll_cache,
                                    selected_msg_idx,
                                    &msg_line_ranges_cache,
                                );
                            }
                            if codex_mode {
                                //（1）Codex：Tab 切换“工具详情 + 思维链”全局展开/全局收起（与聊天区 Tab 一致）。
                                let next =
                                    !(codex_tool_global_expanded && codex_thinking_global_expanded);
                                codex_tool_global_expanded = next;
                                codex_thinking_global_expanded = next;
                                codex_expanded_tool_idxs.clear();
                                codex_collapsed_tool_idxs.clear();
                                codex_expanded_thinking_idxs.clear();
                                codex_collapsed_thinking_idxs.clear();
                            } else {
                                //（1）DeepSeek：Tab 只切换“工具详情 + 思考块”，不强制改动用户折叠。
                                let next_expanded =
                                    !(tool_global_expanded && thinking_global_expanded);
                                tool_global_expanded = next_expanded;
                                thinking_global_expanded = next_expanded;
                                expanded_tool_idxs.clear();
                                collapsed_tool_idxs.clear();
                                expanded_thinking_idxs.clear();
                                collapsed_thinking_idxs.clear();
                            }
                            render_cache.invalidate_all();
                            let note = if codex_mode {
                                if codex_tool_global_expanded && codex_thinking_global_expanded {
                                    "Codex 显示：已展开（工具详情 + 思维链）"
                                } else {
                                    "Codex 显示：已收起（工具简约 + 思维链）"
                                }
                            } else if tool_global_expanded && thinking_global_expanded {
                                "显示：已展开（工具详情 + 思考块）"
                            } else {
                                "显示：已收起（工具简约 + 思考块）"
                            };
                            push_sys_log(&mut sys_log, config.sys_log_limit, note);
                            let expanded_now = if codex_mode {
                                codex_tool_global_expanded && codex_thinking_global_expanded
                            } else {
                                tool_global_expanded && thinking_global_expanded
                            };
                            action_hint = Some((
                                now,
                                if expanded_now {
                                    "Tab: 全局详情".to_string()
                                } else {
                                    "Tab: 简约".to_string()
                                },
                            ));
                            continue;
                        }
                        _ => {}
                    }
                }

                if is_idle_like(mode) && menu_open && !ctrl {
                    let items_now = filter_commands_for_input(&input, sse_enabled);
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
                                } else {
                                    cmd.to_string()
                                };
                                if cmd.eq_ignore_ascii_case("/quit")
                                    || cmd.eq_ignore_ascii_case("/terminal")
                                    || cmd.eq_ignore_ascii_case("/cmd")
                                    || cmd.eq_ignore_ascii_case("/settings")
                                    || cmd.eq_ignore_ascii_case("/help")
                                    || cmd.eq_ignore_ascii_case("/sse")
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
                                        screen: &mut screen,
                                        help_ui: &mut help_ui,
                                        should_exit: &mut should_exit,
                                        sse_enabled: &mut sse_enabled,
                                        enter_cmd_shell: &mut pending_cmd_shell,
                                        open_user_terminal: &mut pending_user_terminal,
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
                                            collapsed_tool_idxs: &mut collapsed_tool_idxs,
                                            expanded_user_idxs: &mut expanded_user_idxs,
                                            collapsed_user_idxs: &mut collapsed_user_idxs,
                                            expanded_thinking_idxs: &mut expanded_thinking_idxs,
                                            collapsed_thinking_idxs: &mut collapsed_thinking_idxs,
                                            selected_msg_idx: &mut selected_msg_idx,
                                            tool_preview_chat_idx: &mut tool_preview_chat_idx,
                                            brief_text: &mut brief_text,
                                            brief_idx: &mut brief_idx,
                                            brief_in_progress: &mut brief_in_progress,
                                            brief_pending_idle: &mut brief_pending_idle,
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
                                        if pending_user_terminal && matches!(screen, Screen::Chat) {
                                            pending_user_terminal = false;
                                            reset_input_buffer(
                                                &mut input,
                                                &mut cursor,
                                                &mut input_chars,
                                                &mut last_input_at,
                                            );
                                            core.push_system("··· Terminal 已打开（用户手动）\n- 该窗口中的命令由用户自己执行\n- 退出/关闭请在 Terminal 内自行完成\n- 这不是 AI 启动的自动化任务");
                                            push_sys_log(
                                                &mut sys_log,
                                                config.sys_log_limit,
                                                "Terminal: user session",
                                            );
                                            let call = ToolCall {
                                                tool: "bash".to_string(),
                                                input: "exec bash -li".to_string(),
                                                brief: Some("terminal".to_string()),
                                                interactive: Some(true),
                                                cwd: Some("~".to_string()),
                                                timeout_secs: Some(30 * 60),
                                                ..ToolCall::default()
                                            };
	                                            let _ = spawn_interactive_bash_execution(
	                                                call,
	                                                MindKind::Main,
	                                                tx.clone(),
	                                                String::new(),
	                                                String::new(),
	                                                false,
	                                                true,
	                                            );
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
                            streaming_state.reset();
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
                            streaming_state.reset();
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
                    //（1）Shift+Tab：
                    //（2）未选中消息：形态切换（简约 -> 工具 -> 思考 -> 简约）
                    //（3）选中消息：对该条消息做同样的形态切换（工具/思考/简约）
                    KeyCode::BackTab
                        if !alt && !ctrl && matches!(chat_focus, ChatFocus::Chat) && !menu_open =>
                    {
                        render_cache.fast_forward_all_animations_to_tail(&core);
                        if reveal_idx.is_some() {
                            reveal_idx = None;
                            reveal_len = 0;
                        }
                        let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                        if !at_bottom {
                            pending_scroll_anchor = capture_scroll_anchor(
                                scroll,
                                max_scroll_cache,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                            );
                        }
                        let codex_mode = chat_target_is_codex(
                            chat_target,
                            &dog_client,
                            &main_client,
                            &memory_client,
                        );

                        //（1）形态循环：简约 -> 工具 -> 思考 -> 简约。
                        //（2）tool+think 同时为 true 时按 tool 优先。
                        let next_cycle = |tool: bool, thinking: bool| -> (bool, bool) {
                            match (tool, thinking) {
                                (false, false) => (true, false),
                                (true, false) => (false, true),
                                (false, true) => (false, false),
                                (true, true) => (true, false),
                            }
                        };

                        if let Some(sel) = selected_msg_idx {
                            follow_bottom = false;
                            //（1）system 折叠条目也走工具路线
                            let is_folded_system = core.history.get(sel).is_some_and(|m| {
                                m.role == Role::System
                                    && m.text.trim_start().starts_with("♡ · 系统信息 · ")
                            });
                            let (think_idx, tool_idx) = if is_folded_system {
                                (None, Some(sel))
                            } else {
                                related_think_and_tool_for_selection(&core, sel)
                            };

                            let tool_cur = tool_idx
                                .map(|ti| {
                                    if codex_mode {
                                        is_idx_expanded(
                                            codex_tool_global_expanded,
                                            &codex_expanded_tool_idxs,
                                            &codex_collapsed_tool_idxs,
                                            ti,
                                        )
                                    } else {
                                        is_idx_expanded(
                                            tool_global_expanded,
                                            &expanded_tool_idxs,
                                            &collapsed_tool_idxs,
                                            ti,
                                        )
                                    }
                                })
                                .unwrap_or(false);
                            let think_cur = think_idx
                                .map(|ti| {
                                    if codex_mode {
                                        is_idx_expanded(
                                            codex_thinking_global_expanded,
                                            &codex_expanded_thinking_idxs,
                                            &codex_collapsed_thinking_idxs,
                                            ti,
                                        )
                                    } else {
                                        is_idx_expanded(
                                            thinking_global_expanded,
                                            &expanded_thinking_idxs,
                                            &collapsed_thinking_idxs,
                                            ti,
                                        )
                                    }
                                })
                                .unwrap_or(false);
                            let (mut want_tool, mut want_think) = next_cycle(tool_cur, think_cur);
                            if tool_idx.is_none() {
                                want_tool = false;
                            }
                            if think_idx.is_none() {
                                want_think = false;
                            }

                            if let Some(ti) = tool_idx
                                && want_tool != tool_cur
                            {
                                let _ = if codex_mode {
                                    toggle_idx_expanded(
                                        codex_tool_global_expanded,
                                        &mut codex_expanded_tool_idxs,
                                        &mut codex_collapsed_tool_idxs,
                                        ti,
                                    )
                                } else {
                                    toggle_idx_expanded(
                                        tool_global_expanded,
                                        &mut expanded_tool_idxs,
                                        &mut collapsed_tool_idxs,
                                        ti,
                                    )
                                };
                            }
                            if let Some(ti) = think_idx
                                && want_think != think_cur
                            {
                                let expanded_now = if codex_mode {
                                    toggle_idx_expanded(
                                        codex_thinking_global_expanded,
                                        &mut codex_expanded_thinking_idxs,
                                        &mut codex_collapsed_thinking_idxs,
                                        ti,
                                    )
                                } else {
                                    toggle_idx_expanded(
                                        thinking_global_expanded,
                                        &mut expanded_thinking_idxs,
                                        &mut collapsed_thinking_idxs,
                                        ti,
                                    )
                                };
                                //（1）若用户把“思考消息”折叠掉，它会立即从 UI 消失；此时 selected 不能悬空。
                                if !expanded_now && selected_msg_idx == Some(ti) {
                                    selected_msg_idx =
                                        find_assistant_for_think_message(&core, ti).or(Some(sel));
                                }
                            }
                            render_cache.invalidate_all();
                            let hint = if want_tool {
                                "Shift+Tab: 工具"
                            } else if want_think {
                                "Shift+Tab: 思考"
                            } else {
                                "Shift+Tab: 简约"
                            };
                            push_sys_log(&mut sys_log, config.sys_log_limit, hint);
                            action_hint = Some((now, hint.to_string()));
                            continue;
                        }

                        //（1）未选中：全局形态切换（互斥）
                        let (tool_cur, think_cur) = if codex_mode {
                            (codex_tool_global_expanded, codex_thinking_global_expanded)
                        } else {
                            (tool_global_expanded, thinking_global_expanded)
                        };
                        let (want_tool, want_think) = next_cycle(tool_cur, think_cur);
                        if codex_mode {
                            codex_tool_global_expanded = want_tool;
                            codex_thinking_global_expanded = want_think;
                            codex_expanded_tool_idxs.clear();
                            codex_collapsed_tool_idxs.clear();
                            codex_expanded_thinking_idxs.clear();
                            codex_collapsed_thinking_idxs.clear();
                        } else {
                            tool_global_expanded = want_tool;
                            thinking_global_expanded = want_think;
                            expanded_tool_idxs.clear();
                            collapsed_tool_idxs.clear();
                            expanded_thinking_idxs.clear();
                            collapsed_thinking_idxs.clear();
                        }
                        render_cache.invalidate_all();
                        let hint = if want_tool {
                            "Shift+Tab: 工具"
                        } else if want_think {
                            "Shift+Tab: 思考"
                        } else {
                            "Shift+Tab: 简约"
                        };
                        push_sys_log(&mut sys_log, config.sys_log_limit, hint);
                        action_hint = Some((now, hint.to_string()));
                        continue;
                    }
                    //（1）Tab：
                    //（2）未选中消息：切换“全部展开/全部收起”（工具详情 + 思考块 + 用户折叠）
                    //（3）选中消息：切换该条“全部详情”（工具/思考/用户三者择一，按消息类型决定）
                    KeyCode::Tab
                        if !alt && !ctrl && matches!(chat_focus, ChatFocus::Chat) && !menu_open =>
                    {
                        render_cache.fast_forward_all_animations_to_tail(&core);
                        if reveal_idx.is_some() {
                            reveal_idx = None;
                            reveal_len = 0;
                        }
                        let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                        if !at_bottom {
                            pending_scroll_anchor = capture_scroll_anchor(
                                scroll,
                                max_scroll_cache,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                            );
                        }

                        if let Some(sel) = selected_msg_idx {
                            follow_bottom = false;
                            if let Some(msg) = core.history.get(sel) {
                                let codex_mode = chat_target_is_codex(
                                    chat_target,
                                    &dog_client,
                                    &main_client,
                                    &memory_client,
                                );
                                let mut handled = false;
                                match msg.role {
                                    Role::User => {
                                        let expanded_now = toggle_idx_expanded(
                                            user_global_expanded,
                                            &mut expanded_user_idxs,
                                            &mut collapsed_user_idxs,
                                            sel,
                                        );
                                        let note = if expanded_now {
                                            "用户消息：已展开（当前选中）"
                                        } else {
                                            "用户消息：已收起（当前选中）"
                                        };
                                        push_sys_log(&mut sys_log, config.sys_log_limit, note);
                                        action_hint = Some((
                                            now,
                                            if expanded_now {
                                                "Tab: 条目展开".to_string()
                                            } else {
                                                "Tab: 条目收起".to_string()
                                            },
                                        ));
                                        handled = true;
                                    }
                                    Role::System => {
                                        //（1）折叠 system info（欢迎语/PTY 已启动等）：按 Tab 展开/收起该条详情。
                                        let expanded_now = if codex_mode {
                                            toggle_idx_expanded(
                                                codex_tool_global_expanded,
                                                &mut codex_expanded_tool_idxs,
                                                &mut codex_collapsed_tool_idxs,
                                                sel,
                                            )
                                        } else {
                                            toggle_idx_expanded(
                                                tool_global_expanded,
                                                &mut expanded_tool_idxs,
                                                &mut collapsed_tool_idxs,
                                                sel,
                                            )
                                        };
                                        let note = if expanded_now {
                                            "系统信息：已展开（当前选中）"
                                        } else {
                                            "系统信息：已收起（当前选中）"
                                        };
                                        push_sys_log(&mut sys_log, config.sys_log_limit, note);
                                        action_hint = Some((
                                            now,
                                            if expanded_now {
                                                "Tab: 条目展开".to_string()
                                            } else {
                                                "Tab: 条目收起".to_string()
                                            },
                                        ));
                                        handled = true;
                                    }
                                    Role::Tool | Role::Assistant => {
                                        let (think_idx, tool_idx) =
                                            related_think_and_tool_for_selection(&core, sel);
                                        if think_idx.is_none() && tool_idx.is_none() {
                                            //（1）该条没有可展开的“详情”组件（例如纯文本 assistant 且无 think/tool）。
                                        } else if codex_mode {
                                            //（1）Codex：Tab 一键实现“该条全展开/全收起”（工具详情 + 思维链）。
                                            let tool_cur = tool_idx
                                                .map(|ti| {
                                                    is_idx_expanded(
                                                        codex_tool_global_expanded,
                                                        &codex_expanded_tool_idxs,
                                                        &codex_collapsed_tool_idxs,
                                                        ti,
                                                    )
                                                })
                                                .unwrap_or(true);
                                            let think_cur = think_idx
                                                .map(|ti| {
                                                    is_idx_expanded(
                                                        codex_thinking_global_expanded,
                                                        &codex_expanded_thinking_idxs,
                                                        &codex_collapsed_thinking_idxs,
                                                        ti,
                                                    )
                                                })
                                                .unwrap_or(true);
                                            let want_expand = !(tool_cur && think_cur);
                                            if let Some(ti) = tool_idx
                                                && want_expand != tool_cur
                                            {
                                                let _ = toggle_idx_expanded(
                                                    codex_tool_global_expanded,
                                                    &mut codex_expanded_tool_idxs,
                                                    &mut codex_collapsed_tool_idxs,
                                                    ti,
                                                );
                                            }
                                            if let Some(ti) = think_idx
                                                && want_expand != think_cur
                                            {
                                                let _ = toggle_idx_expanded(
                                                    codex_thinking_global_expanded,
                                                    &mut codex_expanded_thinking_idxs,
                                                    &mut codex_collapsed_thinking_idxs,
                                                    ti,
                                                );
                                            }
                                            //（1）若 Tab 把“思考消息”收起掉（从 UI 消失），selected 不能悬空。
                                            if !want_expand
                                                && let Some(ti) = think_idx
                                                && selected_msg_idx == Some(ti)
                                            {
                                                selected_msg_idx =
                                                    find_assistant_for_think_message(&core, ti)
                                                        .or(Some(sel));
                                            }
                                            let note = if want_expand {
                                                "Codex：工具详情/思维链已展开（当前选中）"
                                            } else {
                                                "Codex：工具简约/思维链已收起（当前选中）"
                                            };
                                            push_sys_log(&mut sys_log, config.sys_log_limit, note);
                                            action_hint = Some((
                                                now,
                                                if want_expand {
                                                    "Tab: 条目详情".to_string()
                                                } else {
                                                    "Tab: 条目简约".to_string()
                                                },
                                            ));
                                            handled = true;
                                        } else {
                                            //（1）DeepSeek：Tab 一键实现“该条全展开/全收起”（工具详情 + 思考块）。
                                            let tool_cur = tool_idx
                                                .map(|ti| {
                                                    is_idx_expanded(
                                                        tool_global_expanded,
                                                        &expanded_tool_idxs,
                                                        &collapsed_tool_idxs,
                                                        ti,
                                                    )
                                                })
                                                .unwrap_or(true);
                                            let think_cur = think_idx
                                                .map(|ti| {
                                                    is_idx_expanded(
                                                        thinking_global_expanded,
                                                        &expanded_thinking_idxs,
                                                        &collapsed_thinking_idxs,
                                                        ti,
                                                    )
                                                })
                                                .unwrap_or(true);
                                            let want_expand = !(tool_cur && think_cur);
                                            if let Some(ti) = tool_idx
                                                && want_expand != tool_cur
                                            {
                                                let _ = toggle_idx_expanded(
                                                    tool_global_expanded,
                                                    &mut expanded_tool_idxs,
                                                    &mut collapsed_tool_idxs,
                                                    ti,
                                                );
                                            }
                                            if let Some(ti) = think_idx
                                                && want_expand != think_cur
                                            {
                                                let _ = toggle_idx_expanded(
                                                    thinking_global_expanded,
                                                    &mut expanded_thinking_idxs,
                                                    &mut collapsed_thinking_idxs,
                                                    ti,
                                                );
                                            }
                                            //（1）若 Tab 把“思考消息”收起掉（从 UI 消失），selected 不能悬空。
                                            if !want_expand
                                                && let Some(ti) = think_idx
                                                && selected_msg_idx == Some(ti)
                                            {
                                                selected_msg_idx =
                                                    find_assistant_for_think_message(&core, ti)
                                                        .or(Some(sel));
                                            }
                                            let note = if want_expand {
                                                "工具详情/思考块：已展开（当前选中）"
                                            } else {
                                                "工具详情/思考块：已收起（当前选中）"
                                            };
                                            push_sys_log(&mut sys_log, config.sys_log_limit, note);
                                            action_hint = Some((
                                                now,
                                                if want_expand {
                                                    "Tab: 条目详情".to_string()
                                                } else {
                                                    "Tab: 条目简约".to_string()
                                                },
                                            ));
                                            handled = true;
                                        }
                                    }
                                }
                                if !handled {
                                    push_sys_log(
                                        &mut sys_log,
                                        config.sys_log_limit,
                                        "提示：选中消息后按 Tab 展开/收起详情",
                                    );
                                    action_hint = Some((now, "Tab: 无详情".to_string()));
                                }
                                render_cache.invalidate_all();
                            }
                            continue;
                        }

                        let codex_mode = chat_target_is_codex(
                            chat_target,
                            &dog_client,
                            &main_client,
                            &memory_client,
                        );
                        if codex_mode {
                            //（1）Codex：Tab 切换“工具详情 + 思维链”全局展开/全局折叠（折叠态仅显示 brief）。
                            let next =
                                !(codex_tool_global_expanded && codex_thinking_global_expanded);
                            codex_tool_global_expanded = next;
                            codex_thinking_global_expanded = next;
                            codex_expanded_tool_idxs.clear();
                            codex_collapsed_tool_idxs.clear();
                            codex_expanded_thinking_idxs.clear();
                            codex_collapsed_thinking_idxs.clear();
                        } else {
                            //（1）DeepSeek：沿用原逻辑（工具 + 思考 + 用户）。
                            let next_expanded = !(tool_global_expanded && thinking_global_expanded);
                            tool_global_expanded = next_expanded;
                            thinking_global_expanded = next_expanded;
                            expanded_tool_idxs.clear();
                            collapsed_tool_idxs.clear();
                            expanded_thinking_idxs.clear();
                            collapsed_thinking_idxs.clear();
                        }
                        render_cache.invalidate_all();
                        let note = if codex_mode {
                            if codex_tool_global_expanded && codex_thinking_global_expanded {
                                "Codex 显示：已展开（工具详情 + 思维链）"
                            } else {
                                "Codex 显示：已收起（工具简约 + 思维链）"
                            }
                        } else if tool_global_expanded && thinking_global_expanded {
                            "显示：已展开（工具详情 + 思考块）"
                        } else {
                            "显示：已收起（工具简约 + 思考块）"
                        };
                        push_sys_log(&mut sys_log, config.sys_log_limit, note);
                        let expanded_now = if codex_mode {
                            codex_tool_global_expanded && codex_thinking_global_expanded
                        } else {
                            tool_global_expanded && thinking_global_expanded
                        };
                        action_hint = Some((
                            now,
                            if expanded_now {
                                "Tab: 全局详情".to_string()
                            } else {
                                "Tab: 简约".to_string()
                            },
                        ));
                        continue;
                    }
                    KeyCode::Home
                        if !ctrl
                            && !alt
                            && matches!(chat_focus, ChatFocus::Chat)
                            && selected_msg_idx.is_some() =>
                    {
                        let sel = selected_msg_idx.unwrap_or(0);
                        let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                        if !at_bottom {
                            pending_scroll_anchor = capture_scroll_anchor(
                                scroll,
                                max_scroll_cache,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                            );
                        }
                        let codex_mode = chat_target_is_codex(
                            chat_target,
                            &dog_client,
                            &main_client,
                            &memory_client,
                        );

                        let double_home = last_home_at.is_some_and(|t| {
                            now.saturating_duration_since(t) <= Duration::from_millis(350)
                        });
                        last_home_at = Some(now);

                        //（1）Home 规则（避免与 Tab/Shift+Tab 的全局形态冲突）：
                        //（2）单击：仅收敛“当前选中消息”的展开/折叠（不触碰全局展开形态）
                        //（3）双击：强制回归简约模式（收敛任何展开形态）并退出选择
                        if double_home {
                            user_global_expanded = false;
                            expanded_user_idxs.clear();
                            collapsed_user_idxs.clear();
                            if codex_mode {
                                codex_tool_global_expanded = false;
                                codex_thinking_global_expanded = false;
                                codex_expanded_tool_idxs.clear();
                                codex_collapsed_tool_idxs.clear();
                                codex_expanded_thinking_idxs.clear();
                                codex_collapsed_thinking_idxs.clear();
                            } else {
                                tool_global_expanded = false;
                                thinking_global_expanded = false;
                                expanded_tool_idxs.clear();
                                collapsed_tool_idxs.clear();
                                expanded_thinking_idxs.clear();
                                collapsed_thinking_idxs.clear();
                            }
                            render_cache.invalidate_all();
                            follow_bottom = false;
                            selected_msg_idx = None;
                            push_sys_log(
                                &mut sys_log,
                                config.sys_log_limit,
                                "双击 Home：已回归简约模式并退出选择",
                            );
                            action_hint = Some((now, "Home×2: 简约+退出".to_string()));
                            continue;
                        }

                        let mut cleared_any = false;
                        if core
                            .history
                            .get(sel)
                            .is_some_and(|m| m.role == Role::Tool && !is_think_tool_message(m))
                        {
                            if codex_mode {
                                cleared_any |= codex_expanded_tool_idxs.remove(&sel);
                                cleared_any |= codex_collapsed_tool_idxs.remove(&sel);
                            } else {
                                cleared_any |= expanded_tool_idxs.remove(&sel);
                                cleared_any |= collapsed_tool_idxs.remove(&sel);
                            }
                        } else if core.history.get(sel).is_some_and(|m| m.role == Role::User) {
                            cleared_any |= expanded_user_idxs.remove(&sel);
                            cleared_any |= collapsed_user_idxs.remove(&sel);
                        } else if let Some(ti) = find_think_message_for_selection(&core, sel) {
                            if codex_mode {
                                cleared_any |= codex_expanded_thinking_idxs.remove(&ti);
                                cleared_any |= codex_collapsed_thinking_idxs.remove(&ti);
                            } else {
                                cleared_any |= expanded_thinking_idxs.remove(&ti);
                                cleared_any |= collapsed_thinking_idxs.remove(&ti);
                            }
                        }

                        if cleared_any {
                            render_cache.invalidate_all();
                            follow_bottom = false;
                            push_sys_log(
                                &mut sys_log,
                                config.sys_log_limit,
                                "已收敛当前选中消息的展开/折叠",
                            );
                            action_hint = Some((now, "Home: 收敛".to_string()));
                        } else {
                            selected_msg_idx = None;
                            push_sys_log(&mut sys_log, config.sys_log_limit, "已退出选择模式");
                            action_hint = Some((now, "Home: 退出选择".to_string()));
                        }
                    }
                    //（1）↑/↓：
                    //（2）输入框焦点且有内容：移动输入光标（多行）
                    //（3）非输入焦点：滚动聊天
                    KeyCode::Up => {
                        let input_focused = matches!(chat_focus, ChatFocus::Input)
                            && !menu_open
                            && matches!(send_queue_ui, SendQueueUiState::Closed)
                            && !(matches!(screen, Screen::Chat) && pty_view);
                        let input_force_keys = matches!(chat_focus, ChatFocus::Input)
                            && !menu_open
                            && matches!(send_queue_ui, SendQueueUiState::Closed)
                            && (!pty_view || matches!(pty_focus, PtyFocus::Chat));
                        if input_force_keys && shift {
                            //（1）强制发送：Shift+↑
                            //（2）空闲：直接发送；忙碌：入队（高优先级）
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
                            let send = drain_input_for_send(DrainInputForSendArgs {
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                last_input_at: &mut last_input_at,
                                pending_pastes: &mut pending_pastes,
                                paste_capture: &mut paste_capture,
                                command_menu_suppress: &mut command_menu_suppress,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
                            });
                            if send.model_text.trim().is_empty() {
                                continue;
                            }

                            //（1）命令：仍然先走命令解析（/Help /Settings 等）。
                            if handle_command(HandleCommandArgs {
                                core: &mut core,
                                raw: send.model_text.trim_end(),
                                meta: &mut metamemo,
                                context_usage: &mut context_usage,
                                mode: &mut mode,
                                screen: &mut screen,
                                help_ui: &mut help_ui,
                                should_exit: &mut should_exit,
                                sse_enabled: &mut sse_enabled,
                                enter_cmd_shell: &mut pending_cmd_shell,
                                open_user_terminal: &mut pending_user_terminal,
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
                                    collapsed_tool_idxs: &mut collapsed_tool_idxs,
                                    expanded_user_idxs: &mut expanded_user_idxs,
                                    collapsed_user_idxs: &mut collapsed_user_idxs,
                                    expanded_thinking_idxs: &mut expanded_thinking_idxs,
                                    collapsed_thinking_idxs: &mut collapsed_thinking_idxs,
                                    selected_msg_idx: &mut selected_msg_idx,
                                    tool_preview_chat_idx: &mut tool_preview_chat_idx,
                                    brief_text: &mut brief_text,
                                    brief_idx: &mut brief_idx,
                                    brief_in_progress: &mut brief_in_progress,
                                    brief_pending_idle: &mut brief_pending_idle,
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
                                if pending_user_terminal && matches!(screen, Screen::Chat) {
                                    pending_user_terminal = false;
                                    reset_input_buffer(
                                        &mut input,
                                        &mut cursor,
                                        &mut input_chars,
                                        &mut last_input_at,
                                    );
                                    core.push_system("··· Terminal 已打开（用户手动）\n- 该窗口中的命令由用户自己执行\n- 退出/关闭请在 Terminal 内自行完成\n- 这不是 AI 启动的自动化任务");
                                    push_sys_log(
                                        &mut sys_log,
                                        config.sys_log_limit,
                                        "Terminal: user session",
                                    );
                                    let call = ToolCall {
                                        tool: "bash".to_string(),
                                        input: "exec bash -li".to_string(),
                                        brief: Some("terminal".to_string()),
                                        interactive: Some(true),
                                        cwd: Some("~".to_string()),
                                        timeout_secs: Some(30 * 60),
                                        ..ToolCall::default()
                                    };
	                                    let _ = spawn_interactive_bash_execution(
	                                        call,
	                                        MindKind::Main,
	                                        tx.clone(),
	                                        String::new(),
	                                        String::new(),
	                                        false,
	                                        true,
	                                    );
                                    needs_redraw = true;
                                }
                                if should_exit {
                                    break;
                                }
                                continue;
                            }

                            let target = chat_target;
                            if matches!(mode, Mode::Generating | Mode::ExecutingTool) {
                                let id = next_send_queue_id();
                                send_queue_high.push_back(QueuedUserMessage {
                                    id,
                                    target,
                                    ui_text: send.ui_text,
                                    model_text: send.model_text,
                                });
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    format!("已入队（高优先级）：#{id} → {:?}", target),
                                );
                                continue;
                            }
                            if !is_idle_like(mode) {
                                //（1）其它非空闲态（如 ApprovingTool/编辑态）：不强行发送，改为入队。
                                let id = next_send_queue_id();
                                send_queue_high.push_back(QueuedUserMessage {
                                    id,
                                    target,
                                    ui_text: send.ui_text,
                                    model_text: send.model_text,
                                });
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    format!("已入队（高优先级）：#{id} → {:?}", target),
                                );
                                continue;
                            }

                            clear_brief_state(ClearBriefStateArgs {
                                brief_text: &mut brief_text,
                                brief_idx: &mut brief_idx,
                                brief_in_progress: &mut brief_in_progress,
                                brief_pending_idle: &mut brief_pending_idle,
                            });
                            start_user_chat_request(StartUserChatRequestArgs {
                                now,
                                ui_text: send.ui_text,
                                model_text: send.model_text,
                                target,
                                extra_system_override: None,
                                core: &mut core,
                                metamemo: &mut metamemo,
                                context_usage: &mut context_usage,
                                dog_state: &mut dog_state,
                                main_state: &mut main_state,
                                memory_state: &mut memory_state,
                                sys_cfg: &sys_cfg,
                                config: &config,
                                tx: &tx,
                                mode: &mut mode,
                                active_kind: &mut active_kind,
                                active_request_is_internal_placeholder:
                                    &mut active_request_is_internal_placeholder,
                                sending_until: &mut sending_until,
                                sys_log: &mut sys_log,
                                streaming_state: &mut streaming_state,
                                request_seq: &mut request_seq,
                                active_request_id: &mut active_request_id,
                                active_cancel: &mut active_cancel,
                                sse_enabled,
                                next_heartbeat_at: &mut next_heartbeat_at,
                                thinking_text: &mut thinking_text,
                                thinking_full_text: &mut thinking_full_text,
                                thinking_idx: &mut thinking_idx,
                                thinking_in_progress: &mut thinking_in_progress,
                                thinking_pending_idle: &mut thinking_pending_idle,
                                thinking_started_at: &mut thinking_started_at,
                                expanded_tool_idxs: &mut expanded_tool_idxs,
                                expanded_thinking_idxs: &mut expanded_thinking_idxs,
                                scroll: &mut scroll,
                                follow_bottom: &mut follow_bottom,
                                token_totals: &mut token_totals,
                                token_total_path: &token_total_path,
                                active_request_in_tokens: &mut active_request_in_tokens,
                                dog_client: &dog_client,
                                main_client: &main_client,
                                memory_client: &memory_client,
                            })?;
                            continue;
                        }
                        if input_focused {
                            //（1）输入焦点：↑↓一律用于光标移动（即使输入为空也不滚动聊天）。
                            if !input.is_empty() {
                                let width = input_width_cache.max(1);
                                cursor = move_prompt_cursor_vertical(&input, width, cursor, -1);
                                last_input_at = Some(now);
                                command_menu_suppress = false;
                            }
                        } else if matches!(chat_focus, ChatFocus::Chat)
                            && selected_msg_idx.is_some()
                        {
                            //（1）选择态：↑↓用于切换选中消息。
                            selected_msg_idx = select_prev_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                thinking_global_expanded,
                                &expanded_thinking_idxs,
                                &collapsed_thinking_idxs,
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
                        } else {
                            scroll = scroll.saturating_sub(1);
                            follow_bottom = false;
                        }
                    }
                    KeyCode::Down => {
                        let input_focused = matches!(chat_focus, ChatFocus::Input)
                            && !menu_open
                            && matches!(send_queue_ui, SendQueueUiState::Closed)
                            && !(matches!(screen, Screen::Chat) && pty_view);
                        let input_force_keys = matches!(chat_focus, ChatFocus::Input)
                            && !menu_open
                            && matches!(send_queue_ui, SendQueueUiState::Closed)
                            && (!pty_view || matches!(pty_focus, PtyFocus::Chat));
                        if input_force_keys && shift {
                            //（1）强制换行：Shift+↓
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
                            insert_newline_limited(InsertNewlineArgs {
                                now,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
                                toast: &mut toast,
                                max_input_chars,
                                command_menu_suppress: Some(&mut command_menu_suppress),
                            });
                            continue;
                        }
                        if input_focused {
                            //（1）输入焦点：↑↓一律用于光标移动（即使输入为空也不滚动聊天）。
                            if !input.is_empty() {
                                let width = input_width_cache.max(1);
                                cursor = move_prompt_cursor_vertical(&input, width, cursor, 1);
                                last_input_at = Some(now);
                                command_menu_suppress = false;
                            }
                        } else if matches!(chat_focus, ChatFocus::Chat)
                            && selected_msg_idx.is_some()
                        {
                            selected_msg_idx = select_next_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                thinking_global_expanded,
                                &expanded_thinking_idxs,
                                &collapsed_thinking_idxs,
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
                        } else {
                            let max_scroll = max_scroll_cache as u16;
                            scroll = (scroll + 1).min(max_scroll);
                            follow_bottom = scroll >= max_scroll;
                        }
                    }
                    KeyCode::Left
                        if matches!(chat_focus, ChatFocus::Chat) && selected_msg_idx.is_some() =>
                    {
                        let sel = selected_msg_idx.unwrap_or(0);
                        if let Some(ti) = find_think_message_for_selection(&core, sel) {
                            let codex_mode = chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            );
                            let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                            if !at_bottom {
                                pending_scroll_anchor = capture_scroll_anchor(
                                    scroll,
                                    max_scroll_cache,
                                    selected_msg_idx,
                                    &msg_line_ranges_cache,
                                );
                            }
                            let expanded_now = if codex_mode {
                                toggle_idx_expanded(
                                    codex_thinking_global_expanded,
                                    &mut codex_expanded_thinking_idxs,
                                    &mut codex_collapsed_thinking_idxs,
                                    ti,
                                )
                            } else {
                                toggle_idx_expanded(
                                    thinking_global_expanded,
                                    &mut expanded_thinking_idxs,
                                    &mut collapsed_thinking_idxs,
                                    ti,
                                )
                            };
                            render_cache.invalidate_all();
                            follow_bottom = false;
                            action_hint = Some((
                                now,
                                if expanded_now {
                                    "←: 思考展开".to_string()
                                } else {
                                    "←: 思考收起".to_string()
                                },
                            ));

                            //（1）若用户把“思考消息”折叠掉，它会立即从 UI 消失；此时 selected 不能悬空。
                            if !expanded_now && selected_msg_idx == Some(ti) {
                                selected_msg_idx =
                                    find_assistant_for_think_message(&core, ti).or(Some(sel));
                            }
                        } else {
                            push_sys_log(
                                &mut sys_log,
                                config.sys_log_limit,
                                "提示：选中 AI 正文后按 ← 展开/收起思考",
                            );
                            action_hint = Some((now, "←: 无思考".to_string()));
                        }
                    }
                    KeyCode::Right
                        if matches!(chat_focus, ChatFocus::Chat) && selected_msg_idx.is_some() =>
                    {
                        let sel = selected_msg_idx.unwrap_or(0);
                        let at_bottom = (scroll as usize) >= max_scroll_cache.saturating_sub(1);
                        if !at_bottom {
                            pending_scroll_anchor = capture_scroll_anchor(
                                scroll,
                                max_scroll_cache,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                            );
                        }
                        if core.history.get(sel).is_some_and(|m| m.role == Role::User) {
                            let expanded_now = toggle_idx_expanded(
                                user_global_expanded,
                                &mut expanded_user_idxs,
                                &mut collapsed_user_idxs,
                                sel,
                            );
                            render_cache.invalidate_all();
                            follow_bottom = false;
                            action_hint = Some((
                                now,
                                if expanded_now {
                                    "→: 条目展开".to_string()
                                } else {
                                    "→: 条目收起".to_string()
                                },
                            ));
                        } else {
                            let codex_mode = chat_target_is_codex(
                                chat_target,
                                &dog_client,
                                &main_client,
                                &memory_client,
                            );
                            //（1）system 折叠条目（如欢迎语/PTY已启动）：走“工具详情”折叠路线。
                            if core.history.get(sel).is_some_and(|m| {
                                m.role == Role::System
                                    && m.text.trim_start().starts_with("♡ · 系统信息 · ")
                            }) {
                                let expanded_now = if codex_mode {
                                    toggle_idx_expanded(
                                        codex_tool_global_expanded,
                                        &mut codex_expanded_tool_idxs,
                                        &mut codex_collapsed_tool_idxs,
                                        sel,
                                    )
                                } else {
                                    toggle_idx_expanded(
                                        tool_global_expanded,
                                        &mut expanded_tool_idxs,
                                        &mut collapsed_tool_idxs,
                                        sel,
                                    )
                                };
                                render_cache.invalidate_all();
                                follow_bottom = false;
                                action_hint = Some((
                                    now,
                                    if expanded_now {
                                        "→: 系统详情展开".to_string()
                                    } else {
                                        "→: 系统详情收起".to_string()
                                    },
                                ));
                                continue;
                            }
                            let (_think_idx, tool_idx) =
                                related_think_and_tool_for_selection(&core, sel);
                            if let Some(ti) = tool_idx {
                                if codex_mode {
                                    let expanded_now = toggle_idx_expanded(
                                        codex_tool_global_expanded,
                                        &mut codex_expanded_tool_idxs,
                                        &mut codex_collapsed_tool_idxs,
                                        ti,
                                    );
                                    action_hint = Some((
                                        now,
                                        if expanded_now {
                                            "→: 工具展开".to_string()
                                        } else {
                                            "→: 工具收起".to_string()
                                        },
                                    ));
                                } else {
                                    let expanded_now = toggle_idx_expanded(
                                        tool_global_expanded,
                                        &mut expanded_tool_idxs,
                                        &mut collapsed_tool_idxs,
                                        ti,
                                    );
                                    action_hint = Some((
                                        now,
                                        if expanded_now {
                                            "→: 工具展开".to_string()
                                        } else {
                                            "→: 工具收起".to_string()
                                        },
                                    ));
                                }
                                render_cache.invalidate_all();
                                follow_bottom = false;
                            } else {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "提示：选中工具/用户消息后按 → 展开/收起",
                                );
                                action_hint = Some((now, "→: 无工具".to_string()));
                            }
                        }
                    }
                    KeyCode::Left => {
                        if matches!(chat_focus, ChatFocus::Input) && cursor > 0 {
                            cursor = prev_char_boundary(&input, cursor);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    KeyCode::Right => {
                        if matches!(chat_focus, ChatFocus::Input) && cursor < input.len() {
                            cursor = next_char_boundary(&input, cursor);
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    //（1）PgUp/PgDn：消息选择（聊天区焦点时生效）
                    //（2）PgUp：进入/上移选择
                    //（3）PgDn：下移选择
                    KeyCode::PageUp => {
                        if matches!(chat_focus, ChatFocus::Chat) {
                            selected_msg_idx = select_prev_chat_message(
                                &core,
                                selected_msg_idx,
                                thinking_idx,
                                thinking_global_expanded,
                                &expanded_thinking_idxs,
                                &collapsed_thinking_idxs,
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
                            action_hint = Some((now, "PgUp: 上一条".to_string()));
                        }
                    }
                    KeyCode::PageDown => {
                        if matches!(chat_focus, ChatFocus::Chat) {
                            if selected_msg_idx.is_some() {
                                selected_msg_idx = select_next_chat_message(
                                    &core,
                                    selected_msg_idx,
                                    thinking_idx,
                                    thinking_global_expanded,
                                    &expanded_thinking_idxs,
                                    &collapsed_thinking_idxs,
                                    details_mode,
                                );
                            } else {
                                selected_msg_idx = select_last_chat_message(
                                    &core,
                                    thinking_idx,
                                    thinking_global_expanded,
                                    &expanded_thinking_idxs,
                                    &collapsed_thinking_idxs,
                                    details_mode,
                                );
                            }
                            follow_bottom = false;
                            ensure_selected_visible(
                                &mut scroll,
                                &mut follow_bottom,
                                selected_msg_idx,
                                &msg_line_ranges_cache,
                                chat_height_cache,
                                max_scroll_cache,
                            );
                            action_hint = Some((now, "PgDn: 下一条".to_string()));
                        }
                    }
                    KeyCode::Home => {
                        if matches!(chat_focus, ChatFocus::Input) && !input.is_empty() {
                            cursor = 0;
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        }
                    }
                    KeyCode::End => {
                        if matches!(chat_focus, ChatFocus::Input) && !input.is_empty() {
                            cursor = input.len();
                            last_input_at = Some(now);
                            command_menu_suppress = false;
                        } else {
                            scroll_restore_before_end = Some((scroll, follow_bottom));
                            scroll = max_scroll_cache as u16;
                            follow_bottom = true;
                            action_hint = Some((now, "End: 置底".to_string()));
                        }
                    }
                    KeyCode::Insert => {
                        if matches!(chat_focus, ChatFocus::Input) {
                            //（1）Insert 在输入框不做特殊行为（避免影响输入法/键盘习惯）。
                        } else if let Some((prev_scroll, prev_follow)) =
                            scroll_restore_before_end.take()
                        {
                            let max_scroll = max_scroll_cache as u16;
                            scroll = prev_scroll.min(max_scroll);
                            follow_bottom = prev_follow && scroll >= max_scroll;
                            action_hint = Some((now, "Ins: 回溯".to_string()));
                        }
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
                        if try_remove_pty_snapshot_placeholder_at_cursor(
                            &mut input,
                            &mut cursor,
                            &mut input_chars,
                            &mut pending_pty_snapshot,
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
                        prune_pending_pty_snapshot(&input, &mut pending_pty_snapshot);
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
                        if try_remove_pty_snapshot_placeholder_at_cursor(
                            &mut input,
                            &mut cursor,
                            &mut input_chars,
                            &mut pending_pty_snapshot,
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
                        prune_pending_pty_snapshot(&input, &mut pending_pty_snapshot);
                    }
                    KeyCode::Enter => {
                        if !ctrl
                            && !shift
                            && input.trim().is_empty()
                            && is_idle_like(mode)
                            && let Some(sel) = selected_msg_idx
                            && let Some(msg) = core.history.get(sel)
                        {
                            if try_termux_clipboard_set(msg.text.trim_end()) {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "已复制选中消息到剪贴板",
                                );
                                action_hint = Some((now, "已复制".to_string()));
                            } else {
                                push_sys_log(
                                    &mut sys_log,
                                    config.sys_log_limit,
                                    "复制失败：termux-clipboard-set 不可用",
                                );
                                action_hint = Some((now, "复制失败".to_string()));
                            }
                            continue;
                        }

                        //（1）PTY 展开且焦点在聊天输入框：Enter 作为“发送到终端”（用于测试/快速执行）。
                        if matches!(screen, Screen::Chat)
                            && is_pty_panel_active(screen, pty_view, pty_tabs.as_slice())
                            && matches!(pty_focus, PtyFocus::Chat)
                        {
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
                            let send = drain_input_for_send(DrainInputForSendArgs {
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                last_input_at: &mut last_input_at,
                                pending_pastes: &mut pending_pastes,
                                paste_capture: &mut paste_capture,
                                command_menu_suppress: &mut command_menu_suppress,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
                            });
                            let text = send.model_text;
                            if text.trim().is_empty() {
                                continue;
                            }
                            let active = pty_active_idx.min(pty_tabs.len().saturating_sub(1));
                            if let Some(state) = pty_tabs.get_mut(active) {
                                let mut bytes = text.into_bytes();
                                bytes.push(b'\r');
                                state.record_input_bytes(&bytes);
                                if let Some(tx_in) = pty_handles.get(&state.job_id) {
                                    let _ = tx_in.send(PtyControl::Input(bytes));
                                }
                                action_hint = Some((now, "已发送到终端".to_string()));
                            }
                            continue;
                        }

                        if matches!(mode, Mode::Generating | Mode::ExecutingTool) {
                            //（1）API 活跃：
                            //（2）默认 Enter 仅换行（避免打断当前请求）
                            insert_newline_limited(InsertNewlineArgs {
                                now,
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                pending_pastes: &mut pending_pastes,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
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
                            let send = drain_input_for_send(DrainInputForSendArgs {
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                last_input_at: &mut last_input_at,
                                pending_pastes: &mut pending_pastes,
                                paste_capture: &mut paste_capture,
                                command_menu_suppress: &mut command_menu_suppress,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
                            });

                            if let Some(decision) = normalize_confirm_input(&send.model_text) {
                                if let Some((call, _reason, msg_idx)) = pending_tool_confirm.take()
                                {
                                    if decision {
                                        update_history_text_at(
                                            &mut core,
                                            &mut render_cache,
                                            msg_idx,
                                            "您已选择：立即执行",
                                        );
                                        push_sys_log(
                                            &mut sys_log,
                                            config.sys_log_limit,
                                            mcp_messages.tool_confirm_user_allowed.clone(),
                                        );
                                        let mind = match active_kind {
                                            MindKind::Sub => "dog",
                                            MindKind::Main => "main",
                                            MindKind::Memory => "memory",
                                        };
                                        let p = context_path_for_mind(&config, mind);
                                        log_dyncontext(
                                            p,
                                            mind,
                                            "system",
                                            mcp_messages.tool_confirm_user_allowed.as_str(),
                                            None,
                                            None,
                                        );
                                        spawn_tool_execution(
                                            call,
                                            active_kind,
                                            tx.clone(),
                                            pty_started_notice_prompt_text.clone(),
                                            pty_messages.pty_started_model_note.clone(),
                                        );
                                        mode = Mode::ExecutingTool;
                                    } else {
                                        update_history_text_at(
                                            &mut core,
                                            &mut render_cache,
                                            msg_idx,
                                            "您已选择：拒绝执行",
                                        );
                                        push_sys_log(
                                            &mut sys_log,
                                            config.sys_log_limit,
                                            mcp_messages.tool_confirm_user_denied.clone(),
                                        );
                                        //（1）会话上下文外置：将“用户拒绝执行”作为 tool 回执写入 context.jsonl，供模型下轮自愈。
                                        let denied_note = "用户拒绝执行该工具调用，本轮未执行。\n请立即向用户说明：你正准备做什么/为什么需要这一步；并询问用户是否有担忧（安全/隐私/副作用/资源占用）。\n然后给出替代方案（更安全的只读命令、或先解释再请求确认）。";
                                        let denied_meta = vec![
                                            "ok:false | result:confirmation_denied | exit:0 | elapsed_ms:0 | 状态:fail"
                                                .to_string(),
                                        ];
                                        let mind = match active_kind {
                                            MindKind::Sub => "dog",
                                            MindKind::Main => "main",
                                            MindKind::Memory => "memory",
                                        };
                                        let p = context_path_for_mind(&config, mind);
                                        log_dyncontext(
                                            p,
                                            mind,
                                            "tool",
                                            denied_note,
                                            Some(call.tool.as_str()),
                                            Some(denied_meta.as_slice()),
                                        );
                                        //（1）用户拒绝工具执行：停止后续工具链，交由模型在后续对话中解释并礼貌询问原因。
                                        pending_tools.clear();
                                        mode = Mode::Idle;
                                        push_sys_log(
                                            &mut sys_log,
                                            config.sys_log_limit,
                                            "工具被拒绝",
                                        );
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
                        } else if is_idle_like(mode) {
                            let kb_limit = sys_cfg.date_kb_limit;
                            let kb_used = contextmemo_size_kb(&config.contextmemo_path);
                            if kb_limit > 0
                                && matches!(diary_state.stage, DiaryStage::Idle)
                                && kb_used >= kb_limit
                            {
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
                                if try_start_memory_diary(TryStartMemoryDiaryArgs {
                                    memory_client: &memory_client,
                                    main_prompt: &context_prompts.main_prompt,
                                    system_prompt: &memory_state.prompt,
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
                                        Some("memory"),
                                        "MEMORY 未配置 API Key，无法写日记。",
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
                                || looks_like_paste
                                || multi_line_guard
                                || guard_active;
                            if paste_context {
                                //（1）粘贴期间：Enter 一律视为换行（不发送），避免把注入的 Enter 误当作“提交”。
                                last_input_at = Some(now);
                                paste_guard_until =
                                    Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
                                let inserted = insert_newline_limited(InsertNewlineArgs {
                                    now,
                                    input: &mut input,
                                    cursor: &mut cursor,
                                    input_chars: &mut input_chars,
                                    pending_pastes: &mut pending_pastes,
                                    pending_pty_snapshot: &mut pending_pty_snapshot,
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
                            let send = drain_input_for_send(DrainInputForSendArgs {
                                input: &mut input,
                                cursor: &mut cursor,
                                input_chars: &mut input_chars,
                                last_input_at: &mut last_input_at,
                                pending_pastes: &mut pending_pastes,
                                paste_capture: &mut paste_capture,
                                command_menu_suppress: &mut command_menu_suppress,
                                pending_pty_snapshot: &mut pending_pty_snapshot,
                            });
                            if send.model_text.trim().is_empty() {
                                continue;
                            }
                            //（1）已移除弹窗/窗口栈：不需要“清屏关闭弹窗”的额外处理。
                            if handle_command(HandleCommandArgs {
                                core: &mut core,
                                raw: send.model_text.trim_end(),
                                meta: &mut metamemo,
                                context_usage: &mut context_usage,
                                mode: &mut mode,
                                screen: &mut screen,
                                help_ui: &mut help_ui,
                                should_exit: &mut should_exit,
                                sse_enabled: &mut sse_enabled,
                                enter_cmd_shell: &mut pending_cmd_shell,
                                open_user_terminal: &mut pending_user_terminal,
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
                                    collapsed_tool_idxs: &mut collapsed_tool_idxs,
                                    expanded_user_idxs: &mut expanded_user_idxs,
                                    collapsed_user_idxs: &mut collapsed_user_idxs,
                                    expanded_thinking_idxs: &mut expanded_thinking_idxs,
                                    collapsed_thinking_idxs: &mut collapsed_thinking_idxs,
                                    selected_msg_idx: &mut selected_msg_idx,
                                    tool_preview_chat_idx: &mut tool_preview_chat_idx,
                                    brief_text: &mut brief_text,
                                    brief_idx: &mut brief_idx,
                                    brief_in_progress: &mut brief_in_progress,
                                    brief_pending_idle: &mut brief_pending_idle,
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
                                if pending_user_terminal && matches!(screen, Screen::Chat) {
                                    pending_user_terminal = false;
                                    reset_input_buffer(
                                        &mut input,
                                        &mut cursor,
                                        &mut input_chars,
                                        &mut last_input_at,
                                    );
                                    core.push_system("··· Terminal 已打开（用户手动）\n- 该窗口中的命令由用户自己执行\n- 退出/关闭请在 Terminal 内自行完成\n- 这不是 AI 启动的自动化任务");
                                    push_sys_log(
                                        &mut sys_log,
                                        config.sys_log_limit,
                                        "Terminal: user session",
                                    );
                                    let call = ToolCall {
                                        tool: "bash".to_string(),
                                        input: "exec bash -li".to_string(),
                                        brief: Some("terminal".to_string()),
                                        interactive: Some(true),
                                        cwd: Some("~".to_string()),
                                        timeout_secs: Some(30 * 60),
                                        ..ToolCall::default()
                                    };
	                                    let _ = spawn_interactive_bash_execution(
	                                        call,
	                                        MindKind::Main,
	                                        tx.clone(),
	                                        String::new(),
	                                        String::new(),
	                                        false,
	                                        true,
	                                    );
                                    needs_redraw = true;
                                }
                                if should_exit {
                                    break;
                                }
                                continue;
                            }

                            //（1）发送目标：由当前标签页决定。
                            let target = chat_target;
                            clear_brief_state(ClearBriefStateArgs {
                                brief_text: &mut brief_text,
                                brief_idx: &mut brief_idx,
                                brief_in_progress: &mut brief_in_progress,
                                brief_pending_idle: &mut brief_pending_idle,
                            });
                            start_user_chat_request(StartUserChatRequestArgs {
                                now,
                                ui_text: send.ui_text,
                                model_text: send.model_text,
                                target,
                                extra_system_override: None,
                                core: &mut core,
                                metamemo: &mut metamemo,
                                context_usage: &mut context_usage,
                                dog_state: &mut dog_state,
                                main_state: &mut main_state,
                                memory_state: &mut memory_state,
                                sys_cfg: &sys_cfg,
                                config: &config,
                                tx: &tx,
                                mode: &mut mode,
                                active_kind: &mut active_kind,
                                active_request_is_internal_placeholder:
                                    &mut active_request_is_internal_placeholder,
                                sending_until: &mut sending_until,
                                sys_log: &mut sys_log,
                                streaming_state: &mut streaming_state,
                                request_seq: &mut request_seq,
                                active_request_id: &mut active_request_id,
                                active_cancel: &mut active_cancel,
                                sse_enabled,
                                next_heartbeat_at: &mut next_heartbeat_at,
                                thinking_text: &mut thinking_text,
                                thinking_full_text: &mut thinking_full_text,
                                thinking_idx: &mut thinking_idx,
                                thinking_in_progress: &mut thinking_in_progress,
                                thinking_pending_idle: &mut thinking_pending_idle,
                                thinking_started_at: &mut thinking_started_at,
                                expanded_tool_idxs: &mut expanded_tool_idxs,
                                expanded_thinking_idxs: &mut expanded_thinking_idxs,
                                scroll: &mut scroll,
                                follow_bottom: &mut follow_bottom,
                                token_totals: &mut token_totals,
                                token_total_path: &token_total_path,
                                active_request_in_tokens: &mut active_request_in_tokens,
                                dog_client: &dog_client,
                                main_client: &main_client,
                                memory_client: &memory_client,
                            })?;
                        }
                    }
                    KeyCode::Char(ch) => {
                        last_input_at = Some(now);
                        //（1）体验：输入 `/` 需立即弹出菜单。
                        //（2）若处于粘贴捕获，先强制 flush。
                        //（3）避免 `/` 被捕获缓冲吞掉。
                        //（4）避免必须点屏幕后才出菜单。
                        if ch == '/' && input.is_empty() && paste_capture.is_some() {
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
                        }
                        if let Some(c) = paste_capture.as_mut() {
                            cursor =
                                snap_cursor_out_of_placeholder(&input, &pending_pastes, cursor);
                            cursor = snap_cursor_out_of_pty_snapshot_placeholder(
                                &input,
                                &pending_pty_snapshot,
                                cursor,
                            );
                            c.buf.push(ch);
                            c.last_at = now;
                            paste_guard_until =
                                Some(now + Duration::from_millis(config.paste_send_inhibit_ms));
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
                                pending_pty_snapshot: &mut pending_pty_snapshot,
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
                } else {
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
                }
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

struct LoopCtx<'a> {
    //（1）Core + async event input
    core: &'a mut Core,
    rx: &'a Receiver<AsyncEvent>,

    //（1）UI render state
    render_cache: &'a mut ui::ChatRenderCache,

    //（1）PTY UI + background jobs
    pty_tabs: &'a mut Vec<PtyUiState>,
    pty_active_idx: &'a mut usize,
    pty_handles: &'a mut HashMap<u64, mpsc::Sender<PtyControl>>,
    pty_view: &'a mut bool,
    pty_done_batches: &'a mut PtyDoneBatches,
    pty_done_followups: &'a mut VecDeque<PtyDoneFollowup>,

    //（1）Chat loop state
    send_queue_internal: &'a mut VecDeque<InternalUserMessage>,
    mode: &'a mut Mode,
    scroll: &'a mut u16,
    follow_bottom: &'a mut bool,
    active_kind: &'a mut MindKind,
    reveal_idx: &'a mut Option<usize>,
    reveal_len: &'a mut usize,
    expanded_tool_idxs: &'a mut BTreeSet<usize>,
    collapsed_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_user_idxs: &'a mut BTreeSet<usize>,
    collapsed_user_idxs: &'a mut BTreeSet<usize>,
    active_tool_stream: &'a mut Option<ToolStreamState>,

    //（1）Agents + model clients
    dog_state: &'a mut DogState,
    #[allow(dead_code)]
    dog_client: &'a Option<DogClient>,
    main_state: &'a mut DogState,
    main_client: &'a Option<DogClient>,
    memory_state: &'a mut DogState,
    memory_client: &'a Option<DogClient>,
    mind_context: &'a mut MindContextPool,
    _mind_ctx_idx_main: &'a mut Option<usize>,
    _mind_ctx_idx_dog: &'a mut Option<usize>,
    sending_until: &'a mut Option<Instant>,

    //（1）Usage + diary/memory
    token_totals: &'a mut TokenTotals,
    token_total_path: &'a Path,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    diary_state: &'a mut DiaryState,
    mind_rate_window: &'a mut VecDeque<Instant>,

    //（1）Streaming UI overlays (brief / thinking / tool preview)
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    collapsed_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,

    //（1）Input editor
    input: &'a mut String,
    cursor: &'a mut usize,
    input_chars: &'a mut usize,
    last_input_at: &'a mut Option<Instant>,

    //（1）Request lifecycle / retry / cancellation
    pending_pty_snapshot: &'a mut Option<PendingPtySnapshot>,
    active_request_is_mind: &'a mut bool,
    active_request_is_fastmemo_compact: &'a mut bool,
    active_request_is_internal_placeholder: &'a mut bool,
    streaming_state: &'a mut StreamingState,
    mind_pulse: &'a mut Option<MindPulse>,
    retry_status: &'a mut Option<String>,

    //（1）System log + config/prompts
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    config: &'a AppConfig,
    fastmemo_compact_prompt_text: &'a str,
    pty_help_prompt_text: &'a str,
    pty_started_notice_prompt_text: &'a str,
    mcp_messages: &'a McpMessages,
    pty_messages: &'a PtyMessages,

    //（1）Fastmemo compact state
    auto_fastmemo_compact: &'a mut bool,
    fastmemo_compact_inflight: &'a mut bool,
    fastmemo_compact_edit_mask: &'a mut u8,
    fastmemo_compact_retry_at: &'a mut Option<Instant>,

    //（1）Heartbeat + system config
    sys_cfg: &'a mut SystemConfig,
    heartbeat_minutes_cache: &'a mut usize,
    next_heartbeat_at: &'a mut Instant,
    heartbeat_request_id: &'a mut Option<u64>,
    response_count: &'a mut u64,
    _pulse_notice: &'a mut Option<PulseNotice>,

    //（1）Toolchain scheduling + async event output
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
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
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

fn handle_model_stream_chunk(
    args: ModelStreamChunkArgs<'_>,
    content: &str,
    reasoning: &str,
    brief: &str,
) {
    let ModelStreamChunkArgs {
        core,
        render_cache,
        owner,
        streaming_state,
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
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

    //（1）Codex 的 summary/reasoning 可能在正文开始后仍继续分段到达（中转/实现差异较大）。
    //（2）Codex：has_content=true 后仍允许接收 meta。
    let allow_meta_after_content = streaming_state.codex_mode;
    if !brief.is_empty() && (!streaming_state.has_content || allow_meta_after_content) {
        if streaming_state.codex_mode {
            //（1）Codex：brief 只作为可选元数据展示（聊天区 + 状态栏），不强制存在。
            //（2）不并入 thinking（避免混淆）
            //（3）preserve markdown（例如 **标题**）
            let frag = normalize_codex_summary_fragment(brief);
            brief_text.push_str(&frag);
            *brief_in_progress = true;
            *brief_pending_idle = false;
            if let Some(idx) = *brief_idx
                && let Some(entry) = core.history.get_mut(idx)
            {
                entry.text = build_codex_brief_tool_message_running(owner, brief_text);
                render_cache.invalidate(idx);
            }
            //（1）某些中转/实现不会发 `response.reasoning.delta`，只会发 summary。
            //（2）Codex：summary 写入 think tool，供 Shift+Tab/Tab 展开。
            if thinking_full_text.trim().is_empty() {
                if thinking_started_at.is_none() {
                    *thinking_started_at = Some(Instant::now());
                }
                if let Some(ti) = *thinking_idx
                    && let Some(entry) = core.history.get_mut(ti)
                {
                    entry.text = build_codex_thinking_tool_message_running(owner, brief_text);
                    render_cache.invalidate(ti);
                }
            }
        } else {
            if brief_idx.is_none() {
                let stub = build_brief_tool_message_stub(owner, "running");
                core.push_tool_mind(stub, Some(owner));
                *brief_idx = Some(core.history.len().saturating_sub(1));
            }
            brief_text.push_str(brief);
            *brief_in_progress = true;
            *brief_pending_idle = false;
            if let Some(idx) = *brief_idx
                && let Some(entry) = core.history.get_mut(idx)
            {
                entry.text = build_brief_tool_message_running(owner, brief_text);
                render_cache.invalidate(idx);
            }
        }
    }

    if !reasoning.is_empty() && (!streaming_state.has_content || allow_meta_after_content) {
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
                entry.text = if streaming_state.codex_mode {
                    build_codex_thinking_tool_message_running(owner, preview)
                } else {
                    build_thinking_tool_message_running(owner, preview)
                };
                render_cache.invalidate(idx);
            }
        }
    }

    if content.is_empty() {
        return;
    }

    let first_chunk = streaming_state.append_content(content);
    if sse_enabled
        && !*tool_preview_active
        && let Some(tail) = open_thinking_tail(&streaming_state.raw_text)
    {
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

    if streaming_state.raw_text.contains("<thinking") {
        streaming_state.text = strip_thinking_stream(&streaming_state.raw_text);
    } else {
        streaming_state.text.clone_from(&streaming_state.raw_text);
    }

    if sse_enabled {
        //（1）流式阶段：不要仅凭看到 `<tool>` 就进入 tool stub。
        //（2）否则会在模型输出“示例/协议说明/半包 `<tool>`”时误判为工具调用，导致 UI 卡顿/乱码动画。
        //
        //（1）这里改为：确认出现完整 `</tool>` 且能解析出至少 1 个合法工具 JSON 后才进入 stub。
        //（2）更快的兜底：如果回复从 `<tool>` 开始（真实工具调用的典型形态），则一旦看到 `<tool>`
        //（3）就立刻切换成 tool stub，避免在流式阶段把大段 JSON 原文刷到屏幕上。
        //（4）最终解析/执行仍在 stream_end 进行（需要完整 `</tool>` 才会执行）。
        if streaming_state.tool_start.is_none() {
            let trimmed = streaming_state.text.trim_start();
            let start = streaming_state.text.len().saturating_sub(trimmed.len());
            if trimmed.starts_with("<tool>") {
                let line_prefix = streaming_state.text[..start]
                    .rsplit('\n')
                    .next()
                    .unwrap_or("");
                if line_prefix.trim().is_empty() {
                    let prefix = safe_prefix(&streaming_state.text, start);
                    streaming_state.tool_start = Some(start);
                    tool_preview.clear();
                    *tool_preview_active = false;
                    *tool_preview_pending_idle = false;
                    thinking_text.clear();
                    *thinking_in_progress = false;
                    *thinking_pending_idle = false;
                    if let Some(idx) = streaming_state.idx {
                        let has_prefix = !prefix.trim().is_empty();
                        if has_prefix {
                            streaming_state.assistant_idx_before_tool = Some(idx);
                            if let Some(entry) = core.history.get_mut(idx) {
                                entry.role = Role::Assistant;
                                entry.text = prefix.trim_end().to_string();
                                render_cache.invalidate(idx);
                            }
                            let tool_name = extract_tool_name_hint(&streaming_state.text[start..])
                                .unwrap_or_else(|| "tool".to_string());
                            let label = crate::mcp::tool_display_label(tool_name.trim());
                            let stub = format!(
                                "操作: {label}\nexplain: 解析工具调用\ninput: ...\noutput:\n```text\n...\n```\nmeta:\n```text\n状态:parsing\n```\n"
                            );
                            core.push_tool_mind(stub, Some(owner));
                            let tool_idx = core.history.len().saturating_sub(1);
                            render_cache.invalidate(tool_idx);
                            streaming_state.idx = Some(tool_idx);
                            *tool_preview_chat_idx = Some(tool_idx);
                        } else if let Some(entry) = core.history.get_mut(idx) {
                            entry.role = Role::Tool;
                            let tool_name = extract_tool_name_hint(&streaming_state.text[start..])
                                .unwrap_or_else(|| "tool".to_string());
                            let label = crate::mcp::tool_display_label(tool_name.trim());
                            entry.text = format!(
                                "操作: {label}\nexplain: 解析工具调用\ninput: ...\noutput:\n```text\n...\n```\nmeta:\n```text\n状态:parsing\n```\n"
                            );
                            render_cache.invalidate(idx);
                            *tool_preview_chat_idx = Some(idx);
                        }
                    }
                }
            }
        }

        if streaming_state.tool_start.is_none()
            && let Some(start) = streaming_state.text.find("<tool>")
        {
            //（1）增强：当回复前半段有自然语言、后半段开始工具 JSON 时，
            //（2）不必等待完整 </tool> 才进入 tool stub。
            //（3）只要能解析出 tool 名称（"tool" 字段已出现），就切换为 stub。
            //（4）风险：若模型在正文里展示示例 <tool>，这里可能误判；用“行首 + 可解析 tool 名”做最小约束。
            let line_prefix = streaming_state.text[..start]
                .rsplit('\n')
                .next()
                .unwrap_or("");
            if line_prefix.trim().is_empty()
                && extract_tool_name_hint(&streaming_state.text[start..]).is_some()
            {
                let prefix = safe_prefix(&streaming_state.text, start);
                streaming_state.tool_start = Some(start);
                tool_preview.clear();
                *tool_preview_active = false;
                *tool_preview_pending_idle = false;
                thinking_text.clear();
                *thinking_in_progress = false;
                *thinking_pending_idle = false;
                if let Some(idx) = streaming_state.idx {
                    let has_prefix = !prefix.trim().is_empty();
                    let tool_name = extract_tool_name_hint(&streaming_state.text[start..])
                        .unwrap_or_else(|| "tool".to_string());
                    let label = crate::mcp::tool_display_label(tool_name.trim());
                    let stub = build_tool_stream_stub_text(&label, &streaming_state.text[start..]);
                    if has_prefix {
                        streaming_state.assistant_idx_before_tool = Some(idx);
                        if let Some(entry) = core.history.get_mut(idx) {
                            entry.role = Role::Assistant;
                            entry.text = prefix.trim_end().to_string();
                            render_cache.invalidate(idx);
                        }
                        core.push_tool_mind(stub, Some(owner));
                        let tool_idx = core.history.len().saturating_sub(1);
                        render_cache.invalidate(tool_idx);
                        streaming_state.idx = Some(tool_idx);
                        *tool_preview_chat_idx = Some(tool_idx);
                    } else if let Some(entry) = core.history.get_mut(idx) {
                        entry.role = Role::Tool;
                        entry.text = stub;
                        render_cache.invalidate(idx);
                        *tool_preview_chat_idx = Some(idx);
                    }
                }
            }
        }

        if streaming_state.tool_start.is_none()
            && let Some(start) = streaming_state.text.find("<tool>")
            && streaming_state.text[start..].contains("</tool>")
        {
            //（1）只接受“行首（可有空白）”的 tool 块，避免把行内示例误判。
            let line_prefix = streaming_state.text[..start]
                .rsplit('\n')
                .next()
                .unwrap_or("");
            if line_prefix.trim().is_empty() {
                let parsed = crate::mcp::extract_tool_calls(&streaming_state.text)
                    .map(|(calls, _cleaned)| calls)
                    .unwrap_or_default();
                if !parsed.is_empty() {
                    let prefix = safe_prefix(&streaming_state.text, start);
                    streaming_state.tool_start = Some(start);
                    //（1）工具预览不进入思考窗。
                    //（2）不展示工具 JSON。
                    //（3）聊天区只放极简 WORK 占位。
                    tool_preview.clear();
                    *tool_preview_active = false;
                    *tool_preview_pending_idle = false;
                    thinking_text.clear();
                    *thinking_in_progress = false;
                    *thinking_pending_idle = false;
                    if let Some(idx) = streaming_state.idx {
                        //（1）保留前置正文为 assistant。
                        //（2）工具 stub 作为独立 tool 条目。
                        let has_prefix = !prefix.trim().is_empty();
                        if has_prefix {
                            streaming_state.assistant_idx_before_tool = Some(idx);
                            if let Some(entry) = core.history.get_mut(idx) {
                                entry.role = Role::Assistant;
                                entry.text = prefix.trim_end().to_string();
                                render_cache.invalidate(idx);
                            }
                            let tool_name = extract_tool_name_hint(&streaming_state.text[start..])
                                .unwrap_or_else(|| "tool".to_string());
                            let label = crate::mcp::tool_display_label(tool_name.trim());
                            let stub =
                                build_tool_stream_stub_text(&label, &streaming_state.text[start..]);
                            core.push_tool_mind(stub, Some(owner));
                            let tool_idx = core.history.len().saturating_sub(1);
                            render_cache.invalidate(tool_idx);
                            streaming_state.idx = Some(tool_idx);
                            *tool_preview_chat_idx = Some(tool_idx);
                        } else {
                            if let Some(entry) = core.history.get_mut(idx) {
                                entry.role = Role::Tool;
                                let tool_name =
                                    extract_tool_name_hint(&streaming_state.text[start..])
                                        .unwrap_or_else(|| "tool".to_string());
                                let label = crate::mcp::tool_display_label(tool_name.trim());
                                entry.text = build_tool_stream_stub_text(
                                    &label,
                                    &streaming_state.text[start..],
                                );
                                render_cache.invalidate(idx);
                            }
                            *tool_preview_chat_idx = Some(idx);
                        }
                    }
                }
            }
        }

        //（1）进入 tool stub 后不再刷 tool JSON；stream_end 解析执行。
        if let Some(start) = streaming_state.tool_start {
            if let Some(idx) = streaming_state.idx
                && let Some(entry) = core.history.get_mut(idx)
            {
                let tool_name = extract_tool_name_hint(&streaming_state.text[start..])
                    .unwrap_or_else(|| "tool".to_string());
                let label = crate::mcp::tool_display_label(tool_name.trim());
                entry.text = build_tool_stream_stub_text(&label, &streaming_state.text[start..]);
                render_cache.invalidate(idx);
            }
            return;
        }

        if first_chunk {
            //（1）统一规则：思考一结束（正文开始）就立刻隐藏，不做“5 秒停留/折叠动画”。
            //（2）但仍把完整思考写回 Tool 消息，方便 Tab 切换到详情模式回看。
            if let Some(idx) = *thinking_idx {
                let effective_thinking =
                    if streaming_state.codex_mode && thinking_full_text.trim().is_empty() {
                        brief_text.as_str()
                    } else {
                        thinking_full_text.as_str()
                    };
                let has_thinking = !effective_thinking.trim().is_empty();
                if has_thinking {
                    let thinking_secs = thinking_started_at
                        .as_ref()
                        .map(|t| t.elapsed().as_secs())
                        .unwrap_or(0);
                    let thinking_chars = effective_thinking.chars().count();
                    let summary = build_thinking_summary(owner, thinking_secs);
                    let msg = build_thinking_tool_message(
                        &summary,
                        effective_thinking,
                        thinking_secs,
                        thinking_chars,
                        owner,
                    );
                    if let Some(entry) = core.history.get_mut(idx) {
                        entry.text = if streaming_state.codex_mode {
                            build_codex_thinking_tool_message_done(
                                owner,
                                effective_thinking,
                                thinking_secs,
                                thinking_chars,
                            )
                            .trim_end()
                            .to_string()
                        } else {
                            msg.trim_end().to_string()
                        };
                        render_cache.invalidate(idx);
                    }
                } else if let Some(entry) = core.history.get_mut(idx) {
                    //（1）没有可展示思考：立刻标记为 done（详情模式也不刷屏）。
                    entry.text = if streaming_state.codex_mode {
                        build_codex_thinking_tool_message_done(owner, "", 0, 0)
                    } else {
                        build_thinking_tool_message_stub(owner, "0", "Thinking")
                    };
                    render_cache.invalidate(idx);
                }
            }
            thinking_text.clear();
            *thinking_in_progress = false;
            *thinking_pending_idle = false;
            if let Some(idx) = *brief_idx
                && let Some(entry) = core.history.get_mut(idx)
            {
                entry.text = if streaming_state.codex_mode {
                    build_codex_brief_tool_message_done(owner, brief_text)
                } else {
                    build_brief_tool_message_done(owner, brief_text)
                };
                render_cache.invalidate(idx);
            }
            *brief_in_progress = false;
            *brief_pending_idle = false;
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
            *brief_in_progress = false;
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
    collapsed_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_user_idxs: &'a mut BTreeSet<usize>,
    collapsed_user_idxs: &'a mut BTreeSet<usize>,
    active_tool_stream: &'a mut Option<ToolStreamState>,
    dog_state: &'a mut DogState,
    #[allow(dead_code)]
    dog_client: &'a Option<DogClient>,
    main_state: &'a mut DogState,
    #[allow(dead_code)]
    main_client: &'a Option<DogClient>,
    memory_state: &'a mut DogState,
    #[allow(dead_code)]
    memory_client: &'a Option<DogClient>,
    sending_until: &'a mut Option<Instant>,
    token_totals: &'a mut TokenTotals,
    token_total_path: &'a Path,
    meta: &'a mut Option<MetaMemo>,
    context_usage: &'a mut ContextUsage,
    diary_state: &'a mut DiaryState,
    brief_text: &'a mut String,
    brief_idx: &'a mut Option<usize>,
    brief_in_progress: &'a mut bool,
    brief_pending_idle: &'a mut bool,
    thinking_text: &'a mut String,
    thinking_full_text: &'a mut String,
    thinking_idx: &'a mut Option<usize>,
    thinking_in_progress: &'a mut bool,
    thinking_pending_idle: &'a mut bool,
    thinking_started_at: &'a mut Option<Instant>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    collapsed_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview: &'a mut String,
    tool_preview_active: &'a mut bool,
    tool_preview_pending_idle: &'a mut bool,
    tool_preview_chat_idx: &'a mut Option<usize>,
    active_request_is_mind: &'a mut bool,
    active_request_is_fastmemo_compact: &'a mut bool,
    active_request_is_internal_placeholder: &'a mut bool,
    send_queue_internal: &'a mut VecDeque<InternalUserMessage>,
    streaming_state: &'a mut StreamingState,
    retry_status: &'a mut Option<String>,
    sys_log: &'a mut VecDeque<String>,
    sys_log_limit: usize,
    config: &'a AppConfig,
    fastmemo_compact_prompt_text: &'a str,
    pty_started_notice_prompt_text: &'a str,
    mcp_messages: &'a McpMessages,
    pty_messages: &'a PtyMessages,
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

// ===== 模型回复收尾（工具提取、外置 context 落盘、工具链启动）=====
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        active_tool_stream,
        dog_state,
        dog_client: _,
        main_state,
        main_client: _,
        memory_state,
        memory_client,
        sending_until,
        token_totals,
        token_total_path,
        meta,
        context_usage,
        diary_state,
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_started_at,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        active_request_is_mind,
        active_request_is_fastmemo_compact,
        active_request_is_internal_placeholder,
        send_queue_internal,
        streaming_state,
        retry_status,
        sys_log,
        sys_log_limit,
        config,
        fastmemo_compact_prompt_text,
        pty_started_notice_prompt_text,
        mcp_messages,
        pty_messages,
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

    let is_heartbeat = heartbeat_request_id
        .as_ref()
        .is_some_and(|v| *v == request_id);
    if is_heartbeat {
        *heartbeat_request_id = None;
    }
    *retry_status = None;
    *brief_in_progress = false;
    *thinking_in_progress = false;
    let keep_busy = false;
    if sse_enabled {
        if *tool_preview_active && !tool_preview.trim().is_empty() {
            *tool_preview_active = false;
            *tool_preview_pending_idle = true;
        }
        *brief_pending_idle = false;
        //（1）思考结束即隐藏：不保留 pending_idle（避免“卡住/被旧状态遮挡”的显示问题）。
        *thinking_pending_idle = false;
        thinking_text.clear();
    } else {
        *brief_pending_idle = false;
        brief_text.clear();
        *thinking_pending_idle = false;
        thinking_text.clear();
        *tool_preview_pending_idle = false;
        *tool_preview_active = false;
        tool_preview.clear();
    }
    if let Some(msg) = error {
        crate::test::contexttest_log_response_end(crate::test::ContextTestResponseEndArgs {
            mind: kind,
            request_id,
            usage_total_tokens: usage,
            error: Some(msg.as_str()),
            brief: Some(brief_text.as_str()),
            thinking_full: Some(thinking_full_text.as_str()),
            raw_text: Some(streaming_state.raw_text.as_str()),
            assistant_text: None,
        });

        *active_request_is_mind = false;
        *active_request_is_fastmemo_compact = false;
        let mut remove_idxs: Vec<usize> = Vec::new();
        if let Some(idx) = *thinking_idx {
            remove_idxs.push(idx);
        }
        if let Some(idx) = *brief_idx {
            remove_idxs.push(idx);
        }
        remove_idxs.sort_unstable();
        remove_idxs.dedup();
        for idx in remove_idxs.into_iter().rev() {
            remove_message_at(RemoveMessageAtArgs {
                core,
                render_cache,
                idx,
                reveal_idx,
                expanded_tool_idxs,
                collapsed_tool_idxs,
                expanded_user_idxs,
                collapsed_user_idxs,
                thinking_idx,
                brief_idx,
                expanded_thinking_idxs,
                collapsed_thinking_idxs,
                selected_msg_idx,
                tool_preview_chat_idx,
                streaming_state,
                active_tool_stream,
            });
        }
        *thinking_idx = None;
        *brief_idx = None;
        brief_text.clear();
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
                "brief": brief_text.as_str(),
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
        if diary_state.active() {
            diary_state.stage = DiaryStage::Idle;
        }
    } else if !is_heartbeat {
        let raw_assistant = streaming_state.raw_text.trim_end().to_string();
        //（1）Codex：如果全程没收到正文首包，placeholder "Working..." 不应残留在聊天记录里。
        if streaming_state.codex_mode
            && raw_assistant.trim().is_empty()
            && let Some(idx) = streaming_state.idx
            && let Some(entry) = core.history.get_mut(idx)
            && entry.role == Role::Assistant
            && entry.text.trim() == "Working..."
        {
            entry.text.clear();
            render_cache.invalidate(idx);
        }
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
        //（1）兜底：若模型把历史 `[t:HHMMSS]` 时间戳样式复读到正文里，清掉它，避免污染显示与工具解析。
        assistant_text = crate::context::strip_stamp_footer(&assistant_text)
            .trim()
            .to_string();
        let mut extracted_from_assistant = false;
        let mut malformed_tool_call = false;
        let mut tool_block_from_assistant: Option<String> = find_tool_start(&assistant_text)
            .map(|start| assistant_text[start..].trim().to_string())
            .filter(|s| !s.is_empty());
        let mut extracted: Vec<ToolCall> = Vec::new();
        //（1）“工具意图”判定：只有当回复从 <tool> 开始时，才认为模型在尝试发起真实工具调用。
        //（2）正文里的 <tool> 示例不应触发解析失败弹窗。
        let tool_intent = assistant_text.trim_start().starts_with("<tool>");
        let has_tool_markers = assistant_text.contains("<tool>")
            && assistant_text.to_ascii_lowercase().contains("</tool>");
        if let Ok((calls, cleaned)) = extract_tool_calls(&assistant_text) {
            if !calls.is_empty() {
                extracted = calls;
                extracted_from_assistant = true;
                assistant_text = cleaned;
            } else if tool_intent && (has_tool_markers || streaming_state.tool_start.is_some()) {
                //（1）看到 <tool> 但解析失败：多因 JSON 未正确转义。
                //（2）也可能是流式阶段进入了 tool stub，但最终缺失 `</tool>` 或 JSON 不完整。
                //（3）给出明确提示，便于用户定位是“模型格式错误”而非前端/Tab 导致。
                malformed_tool_call = true;
                //（1）解析失败：要写入“模型上下文”，用于自愈重试。
                //（2）统一用 [MCP:工具输出code...] 头，便于 role 占位关联。
                let model_note = "工具调用未执行：检测到 <tool> 块但解析失败（格式错误）。\n请确保：\n- `<tool>{...}</tool>` 独立成行\n- JSON 合法且字段齐全（至少包含 tool + 该工具所需字段）\n- `</tool>` 后不要追加任何文字/占位/伪造回执"
                    .to_string();
                let meta_lines =
                    vec!["ok:false | result:format_error | exit:0 | elapsed_ms:0".to_string()];
                let mind = match kind {
                    MindKind::Sub => "dog",
                    MindKind::Main => "main",
                    MindKind::Memory => "memory",
                };
                let p = context_path_for_mind(config, mind);
                log_dyncontext(
                    p,
                    mind,
                    "tool",
                    &model_note,
                    Some("tool"),
                    Some(meta_lines.as_slice()),
                );
                push_system_and_log(
                    core,
                    meta,
                    context_usage,
                    Some("tool"),
                    "♡ · 系统信息 · 侦测到执行错误\n原因：工具调用失败（格式错误）\n本轮未执行。\n\n排查：\n- `<tool>{...}</tool>` 需独立成行\n- JSON 必须合法且字段齐全（至少包含 tool + 该工具所需字段）\n- `</tool>` 后不要追加任何文字/占位/伪造回执\n\n建议：把本轮工具调用拆成更小、更确定的 1 个调用先跑通。",
                );
                push_sys_log(sys_log, sys_log_limit, "工具 JSON 解析失败：未执行");
                tool_block_from_assistant = None;
                tool_preview.clear();
                *tool_preview_active = false;
                *tool_preview_pending_idle = false;
                assistant_text = crate::mcp::strip_any_tool_blocks(&assistant_text)
                    .trim()
                    .to_string();
            }
        }
        if extracted_from_assistant {
            let removed = dedupe_tool_calls_in_place(&mut extracted);
            if removed > 0 {
                push_sys_log(
                    sys_log,
                    sys_log_limit,
                    format!("检测到重复工具调用，已自动去重：{removed} 条"),
                );
            }
        }
        if extracted_from_assistant && extracted.len() > TOOL_CALLS_MAX_PER_ASSISTANT_MSG {
            let n = extracted.len();
            //（1）写入“模型上下文”：让模型知道自己这一轮被拒绝，可自愈重试（下轮拆分为 <=3）。
            //（2）同样用 [MCP:工具输出code...] 统一格式，便于占位关联。
            let model_note = format!(
                "工具调用未执行：工具调用过多。\n本轮检测到 {n} 个（上限 {TOOL_CALLS_MAX_PER_ASSISTANT_MSG}），已拒绝执行。\n请拆分多轮，每轮最多 {TOOL_CALLS_MAX_PER_ASSISTANT_MSG} 个 <tool> JSON 调用。"
            );
            let meta_lines =
                vec!["ok:false | result:too_many_tools | exit:0 | elapsed_ms:0".to_string()];
            let mind = match kind {
                MindKind::Sub => "dog",
                MindKind::Main => "main",
                MindKind::Memory => "memory",
            };
            let p = context_path_for_mind(config, mind);
            log_dyncontext(
                p,
                mind,
                "tool",
                &model_note,
                Some("tool"),
                Some(meta_lines.as_slice()),
            );

            extracted.clear();
            extracted_from_assistant = false;
            tool_block_from_assistant = None;
            tool_preview.clear();
            *tool_preview_active = false;
            *tool_preview_pending_idle = false;
            push_system_and_log(
                core,
                meta,
                context_usage,
                Some("tool"),
                &format!(
                    "♡ · 系统信息 · 侦测到执行错误\n原因：工具调用过多\n本轮检测到 {n} 个（上限 {TOOL_CALLS_MAX_PER_ASSISTANT_MSG}）\n单次请求最多 {TOOL_CALLS_MAX_PER_ASSISTANT_MSG} 个 JSON 工具调用\n已拒绝执行。\n请拆分多轮。",
                ),
            );
            push_sys_log(sys_log, sys_log_limit, "工具调用过多：已拒绝执行");

            //（1）将该错误“主动回送给模型”以自愈：仅对用户消息触发的请求生效（避免内部占位/自动续写造成死循环）。
            if !*active_request_is_internal_placeholder {
                let prompt = format!(
                    "[sys:工具自愈]上轮输出的 <tool> 调用数量超限：检测到 {n} 个，上限 {TOOL_CALLS_MAX_PER_ASSISTANT_MSG}，系统已拒绝执行。\n请立刻改为重新输出 1~{TOOL_CALLS_MAX_PER_ASSISTANT_MSG} 个 <tool>{{...}}</tool>（必须独立成行）；若需要更多操作，请拆分到后续轮次。\n要求：不要解释，不要复述上下文；只输出新的 <tool> 块（或 1 行说明无需工具并等待用户）。"
                );
                let code = match kind {
                    MindKind::Sub => dog_state.last_tool_output_code(),
                    _ => main_state.last_tool_output_code(),
                };
                send_queue_internal.push_back(InternalUserMessage {
                    target: kind,
                    model_text: crate::context::role_needed_user_placeholder_to(code).to_string(),
                    extra_system: Some(prompt),
                });
            }
        }
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
        //（1）已移除弹窗/窗口栈：正文只保留在聊天区（必要时由“动态层/回看”机制提供可观测性）。
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
                "brief": brief_text.as_str(),
                "thinking": thinking_full_text.as_str(),
                "extracted_from_assistant": extracted_from_assistant,
                "tool_calls": tool_calls_log,
                "tool_preview": tool_preview.as_str(),
                "sse_enabled": sse_enabled,
            }),
        );
        crate::test::contexttest_log_response_end(crate::test::ContextTestResponseEndArgs {
            mind: kind,
            request_id,
            usage_total_tokens: usage,
            error: None,
            brief: Some(brief_text.as_str()),
            thinking_full: Some(thinking_full_text.as_str()),
            raw_text: Some(raw_assistant.as_str()),
            assistant_text: Some(assistant_text.as_str()),
        });
        if extracted_from_assistant {
            extracted.retain(|c| c.tool != "mind_msg");
        }
        if *active_request_is_mind {
            extracted.retain(|c| c.tool == "mind_msg");
        }
        if *active_request_is_fastmemo_compact {
            extracted.retain(|c| c.tool == "memory_read");
        }
        if matches!(diary_state.stage, DiaryStage::WaitingMain) {
            extracted.retain(is_datememo_add);
        }
        //（1）工具调用：按解析顺序依次执行（受每轮最多 3 个工具调用上限约束）。
        if malformed_tool_call
            && let Some(stub_idx) = tool_preview_chat_idx.take()
            && streaming_state.idx == Some(stub_idx)
        {
            remove_message_at(RemoveMessageAtArgs {
                core,
                render_cache,
                idx: stub_idx,
                reveal_idx,
                expanded_tool_idxs,
                collapsed_tool_idxs,
                expanded_user_idxs,
                collapsed_user_idxs,
                thinking_idx,
                brief_idx,
                expanded_thinking_idxs,
                collapsed_thinking_idxs,
                selected_msg_idx,
                tool_preview_chat_idx,
                streaming_state,
                active_tool_stream,
            });
        } else if extracted.is_empty()
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
            //（1）fastmemo 压缩必须通过工具链落地；若未收到任何工具调用，则视为失败并允许后续重试。
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
                MindKind::Memory => memory_state.set_last_usage_total(out_tokens),
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
            let agent = match kind {
                MindKind::Sub => Some("dog"),
                MindKind::Main => Some("main"),
                MindKind::Memory => Some("memory"),
            };
            log_memos(meta, context_usage, "assistant", agent, &assistant_text);
            let mind = match kind {
                MindKind::Sub => "dog",
                MindKind::Main => "main",
                MindKind::Memory => "memory",
            };
            //（1）会话上下文外置：若本轮包含工具调用，外置 context 会在下方按“前置正文 + tool原文 + 后置正文”
            //（2）拆分写入；这里避免重复落盘（否则会把前/后正文写两遍）。
            if extracted.is_empty() {
                let p = context_path_for_mind(config, mind);
                log_dyncontext(p, mind, "assistant", &assistant_text, None, None);
            }
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
            //（1）动态上下文压缩链路已移除：这里只做常规落盘/统计即可。
        } else if extracted.is_empty() && !*active_request_is_mind && !is_heartbeat {
            append_run_log(
                run_log_path,
                "WARN",
                &format!("{} empty assistant response", mind_label(kind)),
            );
        }
        *active_request_is_mind = false;
        if !extracted.is_empty() {
            //（1）SSE tool stub 模式下，模型可能输出：
            //（2）前置正文 + <tool>... + 后置正文
            //（3）我们希望 UI 能稳定呈现：
            //（4）上方：前置正文（assistant）
            //（5）中间：工具占位/回执（tool）
            //（6）下方：后置正文（assistant）
            //
            //（1）之前的逻辑会把“清理后的全文（去掉 tool blocks）”覆盖到前置正文，导致前/后正文混成一条。
            //（2）保留前置正文；剩余正文作为新 assistant 追加。
            let cleaned_ui_text = assistant_text.trim().to_string();
            let mut prefix_for_ctx: Option<String> = None;
            let mut suffix_for_ctx: Option<String> = None;
            let mut suffix_to_push: Option<String> = None;
            if let Some(ai) = streaming_state.assistant_idx_before_tool.take() {
                if let Some(entry) = core.history.get_mut(ai) {
                    entry.role = Role::Assistant;
                    entry.mind = Some(kind);
                    let prefix = entry.text.trim().to_string();
                    if !prefix.is_empty()
                        && !cleaned_ui_text.is_empty()
                        && cleaned_ui_text.starts_with(&prefix)
                    {
                        //（1）suffix = cleaned - prefix
                        let rest = cleaned_ui_text[prefix.len()..].trim().to_string();
                        entry.text = prefix;
                        if !entry.text.trim().is_empty() {
                            prefix_for_ctx = Some(entry.text.trim().to_string());
                        }
                        if !rest.is_empty() {
                            suffix_to_push = Some(rest);
                            suffix_for_ctx = suffix_to_push.clone();
                        }
                    } else if !cleaned_ui_text.is_empty() {
                        //（1）Fallback：无法可靠切分时，至少保证“清理后的正文”可见。
                        entry.text.clone_from(&cleaned_ui_text);
                        prefix_for_ctx = Some(cleaned_ui_text.clone());
                    }
                    render_cache.invalidate(ai);
                }
            } else if tool_preview_chat_idx.is_some() && !cleaned_ui_text.is_empty() {
                //（1）tool stub 可无前置正文，但可能有后置正文。
                //（2）追加一条 assistant，避免“后置正文消失”。
                suffix_to_push = Some(cleaned_ui_text);
                suffix_for_ctx = suffix_to_push.clone();
            }
            if let Some(t) = suffix_to_push
                .as_deref()
                .map(str::trim)
                .filter(|s| !s.is_empty())
            {
                core.history.push(Message {
                    role: Role::Assistant,
                    text: t.to_string(),
                    mind: Some(kind),
                });
                render_cache.invalidate(core.history.len().saturating_sub(1));
            }
            //（1）外置上下文：当回复中存在“工具调用块”时，也要把前/后正文写入 context（否则下一轮会丢失）。
            let mind = match kind {
                MindKind::Sub => "dog",
                MindKind::Main => "main",
                MindKind::Memory => "memory",
            };
            let p = context_path_for_mind(config, mind);
            if let Some(prefix) = prefix_for_ctx
                .as_deref()
                .map(str::trim)
                .filter(|s| !s.is_empty())
            {
                log_dyncontext(p, mind, "assistant", prefix, None, None);
            }
            //（1）关键：把“工具调用原文（<tool>...</tool>）”按原样写入上下文，避免任何摘要/前端格式污染。
            //（2）否则模型下一轮只能看到工具回执，会误判为外部执行，或误学到错误的工具调用格式。
            let tool_calls_raw = extract_tool_blocks_verbatim(&raw_assistant);
            if !tool_calls_raw.trim().is_empty() {
                log_dyncontext(p, mind, "assistant", &tool_calls_raw, None, None);
            }
            if let Some(suffix) = suffix_for_ctx
                .as_deref()
                .map(str::trim)
                .filter(|s| !s.is_empty())
            {
                log_dyncontext(p, mind, "assistant", suffix, None, None);
            }
            if let Some(idx) = streaming_state.idx {
                let is_tool_stub = tool_preview_chat_idx.is_some_and(|v| v == idx)
                    && core.history.get(idx).is_some_and(|m| m.role == Role::Tool);
                if !is_tool_stub && let Some(entry) = core.history.get_mut(idx) {
                    entry.text.clone_from(&assistant_text);
                    render_cache.invalidate(idx);
                }
            }
            pending_tools.extend(extracted);
            //（1）工具执行 owner 必须以“该次回复的 mind(kind)”为准：
            //（2）否则 Memory 工具可能被当成 Main 执行。
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
                owner: kind,
                mcp_messages,
                pty_messages,
                pty_started_notice_prompt_text,
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
        //（1）心跳场景：DeepSeek 可能把答复放进 reasoning。
        //（2）若本次 content 为空，则用 thinking 内容回填到正文，避免只看到心跳 banner。
        assistant_text = heartbeat_reply_text(assistant_text, thinking_full_text);
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
                "brief": brief_text.as_str(),
                "thinking": thinking_full_text.as_str(),
                "tool_calls": [],
                "sse_enabled": sse_enabled,
            }),
        );
        crate::test::contexttest_log_response_end(crate::test::ContextTestResponseEndArgs {
            mind: kind,
            request_id,
            usage_total_tokens: usage,
            error: None,
            brief: Some(brief_text.as_str()),
            thinking_full: Some(thinking_full_text.as_str()),
            raw_text: Some(raw_assistant.as_str()),
            assistant_text: Some(assistant_text.as_str()),
        });
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
            log_memos(
                meta,
                context_usage,
                "assistant",
                Some("main"),
                &assistant_text,
            );
            let p = context_path_for_mind(config, "main");
            log_dyncontext(p, "main", "assistant", &assistant_text, None, None);
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
    }
    {
        let has_thinking = !thinking_full_text.trim().is_empty();
        let thinking_secs = thinking_started_at
            .map(|t| t.elapsed().as_secs())
            .unwrap_or(0);
        let thinking_chars = thinking_full_text.chars().count();
        let summary = build_thinking_summary(kind, thinking_secs);
        let is_codex = streaming_state.codex_mode;
        //（1）统一规则：
        //（2）SSE 流式：只在 running 阶段显示思考；一旦正文开始/思考结束就隐藏
        //（3）非流式：默认不显示思考；仅在详情模式回看（Tab 切换）
        if has_thinking && !is_heartbeat {
            let msg = if is_codex {
                build_codex_thinking_tool_message_done(
                    kind,
                    thinking_full_text,
                    thinking_secs,
                    thinking_chars,
                )
            } else {
                build_thinking_tool_message(
                    &summary,
                    thinking_full_text,
                    thinking_secs,
                    thinking_chars,
                    kind,
                )
            };
            if let Some(idx) = *thinking_idx {
                if let Some(entry) = core.history.get_mut(idx) {
                    entry.text = msg.trim_end().to_string();
                    render_cache.invalidate(idx);
                }
            } else {
                core.push_tool_mind(msg.trim_end().to_string(), Some(kind));
            }
            log_memos(meta, context_usage, "tool", None, msg.trim());
        } else if let Some(idx) = *thinking_idx {
            //（1）没有思考内容：把占位标记为 done（详情模式也不刷屏）。
            if let Some(entry) = core.history.get_mut(idx) {
                entry.text = if is_codex {
                    build_codex_thinking_tool_message_done(kind, "", 0, 0)
                } else {
                    build_thinking_tool_message_stub(kind, "0", &summary)
                };
                render_cache.invalidate(idx);
            }
        }

        //（1）思考结束即隐藏：不做 5 秒停留与折叠动画。
        *thinking_idx = None;
        thinking_full_text.clear();
        *thinking_started_at = None;
    }
    {
        let has_brief = !brief_text.trim().is_empty();
        if let Some(idx) = *brief_idx {
            if has_brief && !is_heartbeat {
                if let Some(entry) = core.history.get_mut(idx) {
                    entry.text = if streaming_state.codex_mode {
                        build_codex_brief_tool_message_done(kind, brief_text)
                    } else {
                        build_brief_tool_message_done(kind, brief_text)
                    };
                    render_cache.invalidate(idx);
                }
            } else {
                remove_message_at(RemoveMessageAtArgs {
                    core,
                    render_cache,
                    idx,
                    reveal_idx,
                    expanded_tool_idxs,
                    collapsed_tool_idxs,
                    expanded_user_idxs,
                    collapsed_user_idxs,
                    thinking_idx,
                    brief_idx,
                    expanded_thinking_idxs,
                    collapsed_thinking_idxs,
                    selected_msg_idx,
                    tool_preview_chat_idx,
                    streaming_state,
                    active_tool_stream,
                });
            }
        }
        *brief_idx = None;
        *brief_in_progress = false;
        *brief_pending_idle = false;
        brief_text.clear();
    }
    *active_request_id = None;
    *active_cancel = None;
    *active_request_in_tokens = None;
    streaming_state.reset();
    if !keep_busy && !matches!(*mode, Mode::ApprovingTool | Mode::ExecutingTool) {
        *mode = Mode::Idle;
    }
    let fastmemo_retry_ready = fastmemo_compact_retry_at
        .as_ref()
        .map(|t| Instant::now() >= *t)
        .unwrap_or(true);
    if sys_cfg.fastmemo_compact_enabled
        && *auto_fastmemo_compact
        && fastmemo_retry_ready
        && !*fastmemo_compact_inflight
        && !is_heartbeat
        && pending_tools.is_empty()
        && pending_tool_confirm.is_none()
        && active_tool_stream.is_none()
        && matches!(*mode, Mode::Idle)
    {
        if memory_client.is_some() {
            //（1）刷新 fastmemo（尽量保证 Memory 在压缩前看到最新内容）。
            if sys_cfg.fastmemo_inject_enabled {
                let fastmemo = read_fastmemo_for_context();
                main_state.refresh_fastmemo_system(&fastmemo);
                dog_state.refresh_fastmemo_system(&fastmemo);
                memory_state.refresh_fastmemo_system(&fastmemo);
            }
            push_sys_log(sys_log, sys_log_limit, "fastmemo 自动压缩中（MEM）");
            *active_request_is_fastmemo_compact = true;
            let extra_system = Some(fastmemo_compact_prompt_text.to_string());
            if let Some(in_tokens) = try_start_memory_generation(TryStartMemoryGenerationArgs {
                memory_client,
                memory_state,
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
            push_sys_log(
                sys_log,
                sys_log_limit,
                "MEMORY: API 未就绪，fastmemo 压缩暂停",
            );
        }
    }
    jump_to_bottom(scroll, follow_bottom);
}

// ===== 事件循环回收（AsyncEvent -> 状态机推进）=====
fn drain_async_events(args: LoopCtx<'_>) {
    let LoopCtx {
        core,
        rx,
        render_cache,
        pty_tabs,
        pty_active_idx,
        pty_handles,
        pty_view,
        pty_done_batches,
        pty_done_followups,
        send_queue_internal,
        mode,
        scroll,
        follow_bottom,
        active_kind,
        reveal_idx,
        reveal_len,
        expanded_tool_idxs,
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        active_tool_stream,
        dog_state,
        dog_client,
        main_state,
        main_client,
        memory_state,
        memory_client,
        mind_context,
        _mind_ctx_idx_main,
        _mind_ctx_idx_dog,
        sending_until,
        token_totals,
        token_total_path,
        meta,
        context_usage,
        diary_state,
        mind_rate_window,
        brief_text,
        brief_idx,
        brief_in_progress,
        brief_pending_idle,
        thinking_text,
        thinking_full_text,
        thinking_idx,
        thinking_in_progress,
        thinking_pending_idle,
        thinking_started_at,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview,
        tool_preview_active,
        tool_preview_pending_idle,
        tool_preview_chat_idx,
        input,
        cursor,
        input_chars,
        last_input_at,
        pending_pty_snapshot,
        active_request_is_mind,
        active_request_is_fastmemo_compact,
        active_request_is_internal_placeholder,
        streaming_state,
        mind_pulse,
        retry_status,
        sys_log,
        sys_log_limit,
        config,
        fastmemo_compact_prompt_text,
        pty_help_prompt_text,
        pty_started_notice_prompt_text,
        mcp_messages,
        pty_messages,
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
            AsyncEvent::ModelStreamStart {
                kind,
                expect_brief,
                request_id,
            } => {
                handle_model_stream_start(ModelStreamStartArgs {
                    core,
                    kind,
                    expect_brief,
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
                    brief_text,
                    brief_idx,
                    brief_in_progress,
                    brief_pending_idle,
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
                brief,
                request_id: _,
            } => {
                handle_model_stream_chunk(
                    ModelStreamChunkArgs {
                        core,
                        render_cache,
                        owner: *active_kind,
                        streaming_state,
                        brief_text,
                        brief_idx,
                        brief_in_progress,
                        brief_pending_idle,
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
                    &brief,
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
                        collapsed_tool_idxs,
                        expanded_user_idxs,
                        collapsed_user_idxs,
                        active_tool_stream,
                        dog_state,
                        dog_client,
                        main_state,
                        main_client,
                        memory_state,
                        memory_client,
                        sending_until,
                        token_totals,
                        token_total_path,
                        meta,
                        context_usage,
                        diary_state,
                        brief_text,
                        brief_idx,
                        brief_in_progress,
                        brief_pending_idle,
                        thinking_text,
                        thinking_full_text,
                        thinking_idx,
                        thinking_in_progress,
                        thinking_pending_idle,
                        thinking_started_at,
                        expanded_thinking_idxs,
                        collapsed_thinking_idxs,
                        selected_msg_idx,
                        tool_preview,
                        tool_preview_active,
                        tool_preview_pending_idle,
                        tool_preview_chat_idx,
                        active_request_is_mind,
                        active_request_is_fastmemo_compact,
                        active_request_is_internal_placeholder,
                        send_queue_internal,
                        streaming_state,
                        retry_status,
                        sys_log,
                        sys_log_limit,
                        config,
                        fastmemo_compact_prompt_text,
                        pty_started_notice_prompt_text,
                        mcp_messages,
                        pty_messages,
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
	                owner,
	                user_initiated,
	                job_id,
	                cmd,
	                saved_path,
	                status_path,
	            } => {
	                handle_async_event_pty_ready(HandleAsyncEventPtyReadyArgs {
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
	                });
	            }
            AsyncEvent::PtyOutput { job_id, bytes } => {
                handle_async_event_pty_output(pty_tabs.as_mut_slice(), job_id, bytes);
            }
            AsyncEvent::PtySpawned { job_id, pid, pgrp } => {
                handle_async_event_pty_spawned(pty_tabs.as_mut_slice(), job_id, pid, pgrp);
            }
	            AsyncEvent::PtyJobDone {
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
	            } => {
	                handle_async_event_pty_job_done(HandleAsyncEventPtyJobDoneArgs {
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
	                });
	            }
            AsyncEvent::PtyToolRequest { owner, call } => {
                handle_async_event_pty_tool_request(HandleAsyncEventPtyToolRequestArgs {
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
                });
            }
            AsyncEvent::ToolStreamEnd {
                mut outcome,
                sys_msg,
            } => {
                let is_interactive_bash = active_tool_stream
                    .as_ref()
                    .is_some_and(|s| s.call.tool == "bash" && s.call.interactive.unwrap_or(false));
                let is_pty_started = outcome
                    .log_lines
                    .iter()
                    .any(|l| l.contains("phase:pty_started"));
                let is_pty_final = outcome
                    .log_lines
                    .iter()
                    .any(|l| l.contains("phase:pty_final"));
                //（1）兼容旧链路：仅在“final”阶段才把屏幕快照写回工具 output。
                //（2）后台 PTY：启动即结束工具调用，后台发 PtyJobDone。
                if is_interactive_bash && !is_pty_started && is_pty_final {
                    let active = (*pty_active_idx).min(pty_tabs.len().saturating_sub(1));
                    if let Some(state) = pty_tabs.get_mut(active) {
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

                        //（1）若屏幕内容被截断，补充一段 raw tail（可能含 ANSI 控制序列）。
                        if snap_chars > PTY_RETURN_SCREEN_MAX_CHARS
                            && let Some(raw) =
                                read_tail_text(&state.saved_path, PTY_RETURN_RAW_TAIL_BYTES)
                        {
                            let raw = truncate_tail(&raw, PTY_RETURN_RAW_TAIL_MAX_CHARS);
                            parts.push(format!("[raw_tail]\n{raw}"));
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

	                        let body = parts.join("\n\n").trim_end().to_string();
	                        outcome.user_message = if state.user_initiated {
	                            format!(
	                                "[用户手动 Terminal]\n说明: 以下输出来自用户在 Terminal 面板执行的命令, 不是 AI 自动化任务\njob_id:{}\n\n{}",
	                                state.job_id, body
	                            )
	                        } else {
	                            body
	                        };
                        outcome
                            .log_lines
                            .push(format!("saved:{}", state.saved_path));
                    }
                    //（1）旧链路收尾：结束后清理终端状态（不残留）。
                    *pty_view = false;
                    pty_tabs.clear();
                    pty_handles.clear();
                    *pty_active_idx = 0;
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
                let mut tool_message_model = None;
                let mut saw_diary_add = false;
                let mut reload_sys_cfg = false;
                let mut skip_owner_resume = false;
                let mut skip_tool_context = false;
                let mut mind_transfer: Option<(MindKind, MindKind, String, String)> = None;
                let mut mind_pulse_dir: Option<PulseDir> = None;
                if let Some(state) = active_tool_stream.as_mut() {
                    //（1）交互式 bash（PTY）在“启动阶段”提前结束工具调用：不向模型注入、也不继续自动生成。
                    //（2）退出策略：等待 PTY Done 或 PTY 审计 tick（由系统注入），用户可在 UI 里交互。
                    if is_interactive_bash && is_pty_started {
                        skip_tool_context = true;
                        skip_owner_resume = true;
                    }
                    //（1）写日记收尾：active 且 datememo 写入成功即落盘。
                    //（2）后续应清空 fastcontext，准备新一轮记录。
                    //
                    //（1）这里不再绑定某个具体 stage 名称（例如 WaitingMain），避免未来 stage 调整导致漏清空。
                    if diary_state.active()
                        && matches!(state.owner, MindKind::Memory)
                        && is_datememo_add(&state.call)
                        && outcome
                            .log_lines
                            .iter()
                            .any(|l| l.trim_start().starts_with("状态:0"))
                    {
                        saw_diary_add = true;
                    }
                    if state.call.tool == "system_config" {
                        reload_sys_cfg = true;
                    }
                    if state.call.tool == "mind_msg" {
                        skip_owner_resume = true;
                        skip_tool_context = true;
                        if let Some((from, target, brief, content)) = handle_mind_msg_tool(state) {
                            mind_transfer = Some((from, target, brief, content));
                            mind_pulse_dir = Some(pulse_dir(from, target));
                        }
                        if sys_cfg.fastmemo_compact_enabled && memory_state.fastmemo_compact_pending
                        {
                            *auto_fastmemo_compact = true;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo: 已达阈值，待压缩");
                        }
                    }
                    if state.call.tool == "memory_add" {
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
                        if sys_cfg.fastmemo_compact_enabled && is_fastmemo && pending {
                            *auto_fastmemo_compact = true;
                            memory_state.fastmemo_compact_pending = true;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo: 已达阈值，待压缩");
                        }
                    }
                    //（1）fastmemo 是固定注入的上下文块：任意写入后应立即重载，避免后续请求仍拿旧内容。
                    if matches!(state.call.tool.as_str(), "memory_add") {
                        let raw = state
                            .call
                            .path
                            .as_deref()
                            .unwrap_or(state.call.input.trim())
                            .to_ascii_lowercase();
                        if raw.contains("fastmemo") && sys_cfg.fastmemo_inject_enabled {
                            let fastmemo = read_fastmemo_for_context();
                            main_state.refresh_fastmemo_system(&fastmemo);
                            dog_state.refresh_fastmemo_system(&fastmemo);
                            memory_state.refresh_fastmemo_system(&fastmemo);
                        }
                    }
                    tool_message_model = Some(format_tool_message_for_model(&state.call, &outcome));
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
                }
                let owner = active_tool_stream
                    .as_ref()
                    .map(|s| s.owner)
                    .unwrap_or(*active_kind);
                let tool_name_for_ctx = active_tool_stream
                    .as_ref()
                    .map(|s| s.call.tool.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .unwrap_or_else(|| "tool".to_string());
                *active_tool_stream = None;
                let mut mind_request: Option<(MindKind, String, String)> = None;
                if let Some((from, target, brief, content)) = mind_transfer.take() {
                    //（1）聊天区可视化：○ Dog ➠ Main：brief / ● Main ➠ Dog：brief
                    //（2）下一行用 ↳ 承载内容（箭头与内容斜体由 UI 负责）。
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
                if saw_diary_add {
                    diary_state.stage = DiaryStage::Idle;
                    context_usage.reset();
                    clear_contextmemo(&config.contextmemo_path);
                    push_sys_log(sys_log, sys_log_limit, "日记压缩完成");
                }
                //（1）会话上下文外置：工具回执写入 context.jsonl，供后续请求回放到模型上下文。
                if !skip_tool_context && let Some(msg) = tool_message_model.as_ref() {
                    let mind = match owner {
                        MindKind::Sub => "dog",
                        MindKind::Main => "main",
                        MindKind::Memory => "memory",
                    };
                    let p = context_path_for_mind(config, mind);
                    log_dyncontext(
                        p,
                        mind,
                        "tool",
                        msg,
                        Some(tool_name_for_ctx.as_str()),
                        Some(outcome.log_lines.as_slice()),
                    );
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
                    pty_started_notice_prompt_text,
                    mcp_messages,
                    pty_messages,
                });
                //（1）fastmemo 完成判定：最后工具结束后检查分区>=10。
                //（2）仅依据 fastmemo 文件本身判断，避免依赖“是否回显 notice”导致漏判。
                if !has_next && *fastmemo_compact_inflight {
                    let pending = std::fs::read_to_string(FASTMEMO_PATH)
                        .ok()
                        .map(|t| fastmemo_event_count_and_any_ge10(&t).1)
                        .unwrap_or(false);
                    if !pending {
                        *fastmemo_compact_inflight = false;
                        *fastmemo_compact_edit_mask = 0;
                        *fastmemo_compact_retry_at = None;
                        *auto_fastmemo_compact = false;
                        *active_request_is_fastmemo_compact = false;
                        memory_state.fastmemo_compact_pending = false;
                        push_sys_log(sys_log, sys_log_limit, "fastmemo 自动压缩完成");
                    } else {
                        //（1）仍超阈值：稍后自动重试（或下一轮继续压缩下一分区）。
                        *fastmemo_compact_inflight = false;
                        *fastmemo_compact_edit_mask = 0;
                        *active_request_is_fastmemo_compact = false;
                        if sys_cfg.fastmemo_compact_enabled {
                            *auto_fastmemo_compact = true;
                            memory_state.fastmemo_compact_pending = true;
                            *fastmemo_compact_retry_at =
                                Some(Instant::now() + Duration::from_secs(12));
                            push_sys_log(sys_log, sys_log_limit, "fastmemo 仍超阈值：稍后重试");
                        } else {
                            *auto_fastmemo_compact = false;
                            memory_state.fastmemo_compact_pending = false;
                            *fastmemo_compact_retry_at = None;
                            push_sys_log(sys_log, sys_log_limit, "fastmemo 压缩已停用：跳过重试");
                        }
                    }
                }
                if !has_next && !skip_owner_resume && matches!(owner, MindKind::Sub) {
                    if let Some(in_tokens) = try_start_dog_generation(TryStartDogGenerationArgs {
                        kind: MindKind::Sub,
                        dog_client,
                        dog_state,
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
                        push_sys_log(sys_log, sys_log_limit, "DOG: API 未就绪，工具链暂停");
                        push_system_and_log(
                            core,
                            meta,
                            context_usage,
                            Some("dog"),
                            "DOG: API 未就绪，工具链暂停",
                        );
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
                    && matches!(*mode, Mode::Idle)
                    && pending_tool_confirm.is_none()
                    && pending_tools.is_empty()
                    && active_tool_stream.is_none()
                    && let Some((target, mind_system, mind_user)) = mind_request.take()
                {
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        thinking_idx,
        brief_idx,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
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
    collapsed_tool_idxs: &'a mut BTreeSet<usize>,
    expanded_user_idxs: &'a mut BTreeSet<usize>,
    collapsed_user_idxs: &'a mut BTreeSet<usize>,
    thinking_idx: &'a mut Option<usize>,
    brief_idx: &'a mut Option<usize>,
    expanded_thinking_idxs: &'a mut BTreeSet<usize>,
    collapsed_thinking_idxs: &'a mut BTreeSet<usize>,
    selected_msg_idx: &'a mut Option<usize>,
    tool_preview_chat_idx: &'a mut Option<usize>,
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
        collapsed_tool_idxs,
        expanded_user_idxs,
        collapsed_user_idxs,
        thinking_idx,
        brief_idx,
        expanded_thinking_idxs,
        collapsed_thinking_idxs,
        selected_msg_idx,
        tool_preview_chat_idx,
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
    adjust_index_set(collapsed_tool_idxs, removed);
    adjust_index_set(expanded_user_idxs, removed);
    adjust_index_set(collapsed_user_idxs, removed);
    adjust_index(thinking_idx, removed);
    adjust_index(brief_idx, removed);
    adjust_index_set(expanded_thinking_idxs, removed);
    adjust_index_set(collapsed_thinking_idxs, removed);
    adjust_index(selected_msg_idx, removed);
    adjust_index(tool_preview_chat_idx, removed);
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
    screen: &'a mut Screen,
    help_ui: &'a mut HelpUiState,
    should_exit: &'a mut bool,
    sse_enabled: &'a mut bool,
    enter_cmd_shell: &'a mut bool,
    open_user_terminal: &'a mut bool,
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
        screen,
        help_ui,
        should_exit,
        sse_enabled,
        enter_cmd_shell,
        open_user_terminal,
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
            //（1）仅退出 AItermux，返回到当前 shell/session。
            //（2）是否结束 Termux session 由启动器按 exit code 决定。
            //（3）这里仅退出程序，不主动结束 session。
            *exit_code = 0;
            push_sys_log(sys_log, sys_log_limit, "Quit");
        }
        "/terminal" | "/term" => {
            *open_user_terminal = true;
        }
        "/cmd" => {
            *enter_cmd_shell = true;
        }
        "/settings" => {
            *screen = Screen::Settings;
        }
        "/help" => {
            *screen = Screen::Chat;
            *help_ui = HelpUiState::Selecting { selected: 0 };
        }
        "/sse" => match arg.as_str() {
            "open" => {
                *sse_enabled = true;
                sys_cfg.sse_enabled = true;
                let _ = store_config_file(sys_cfg_path, sys_cfg);
                push_sys_log(sys_log, sys_log_limit, "SSE: on");
            }
            "close" => {
                *sse_enabled = false;
                sys_cfg.sse_enabled = false;
                let _ = store_config_file(sys_cfg_path, sys_cfg);
                push_sys_log(sys_log, sys_log_limit, "SSE: off");
            }
            _ => {
                push_system_and_log(
                    core,
                    meta,
                    context_usage,
                    None,
                    "用法：/SSE Open 或 /SSE Close",
                );
                push_sys_log(sys_log, sys_log_limit, "SSE: invalid args");
            }
        },
        _ => {
            const UNKNOWN: &str =
                "未知命令：/Terminal /Cmd /Settings /Help /SSE Open /SSE Close /Quit";
            push_system_and_log(core, meta, context_usage, None, UNKNOWN);
            push_sys_log(sys_log, sys_log_limit, "Unknown command");
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

struct TryStartMemoryDiaryArgs<'a> {
    memory_client: &'a Option<DogClient>,
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

fn try_start_memory_diary(args: TryStartMemoryDiaryArgs<'_>) -> bool {
    let TryStartMemoryDiaryArgs {
        memory_client,
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

    let Some(client) = memory_client.clone() else {
        return false;
    };
    *mode = Mode::Generating;
    *active_kind = MindKind::Memory;
    *sending_until = None;
    let now = Instant::now();
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    push_sys_log(sys_log, sys_log_limit, "记忆模型正在写日记");
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
        MindKind::Memory,
        sse_enabled,
        request_id,
        cancel,
    );
    true
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
    *sending_until = None;
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    push_sys_log(sys_log, sys_log_limit, "现在[萤]MAIN正在思考");
    streaming_state.reset();
    let p = context_path_for_mind(config, "main");
    let messages =
        main_state.message_snapshot_with_context_file(extra_system.as_deref(), p, "main");
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

struct TryStartMemoryGenerationArgs<'a> {
    memory_client: &'a Option<DogClient>,
    memory_state: &'a DogState,
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

fn try_start_memory_generation(args: TryStartMemoryGenerationArgs<'_>) -> Option<u64> {
    let TryStartMemoryGenerationArgs {
        memory_client,
        memory_state,
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

    let client = memory_client.clone()?;
    *mode = Mode::Generating;
    *active_kind = MindKind::Memory;
    let now = Instant::now();
    *sending_until = None;
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    push_sys_log(sys_log, sys_log_limit, "现在[萤]MEM正在思考");
    streaming_state.reset();
    let p = context_path_for_mind(config, "memory");
    let messages =
        memory_state.message_snapshot_with_context_file(extra_system.as_deref(), p, "memory");
    let in_tokens = estimate_messages_in_tokens(&messages);
    let (request_id, cancel) = begin_request(request_seq, active_request_id, active_cancel);
    spawn_dog_request(
        client,
        messages,
        tx.clone(),
        MindKind::Memory,
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
    *sending_until = None;
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    push_sys_log(sys_log, sys_log_limit, "协同沟通中");
    streaming_state.reset();
    let mut messages = state.messages_clone();
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
    //（1）这里不依赖具体 provider：仅做基础清洗（空消息过滤 + 连续 assistant 合并）。
    let messages = normalize_messages_for_model_context(&messages);
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
    *sending_until = None;
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    push_sys_log(sys_log, sys_log_limit, "心跳已发送");
    streaming_state.reset();
    let p = context_path_for_mind(config, "main");
    let messages = main_state.message_snapshot_with_context_file(None, p, "main");
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
    *sending_until = None;
    if client.is_codex_provider() {
        *sending_until = Some(now + Duration::from_millis(config.send_status_ms));
    }
    let label = if matches!(kind, MindKind::Sub) {
        "DOG"
    } else {
        "MAIN"
    };
    push_sys_log(sys_log, sys_log_limit, format!("现在[萤]{label}正在思考"));
    streaming_state.reset();
    let mind = if matches!(kind, MindKind::Sub) {
        "dog"
    } else {
        "main"
    };
    let p = context_path_for_mind(config, mind);
    let messages = dog_state.message_snapshot_with_context_file(extra_system.as_deref(), p, mind);
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
    install_crash_hook();
    trace_startup("main: start");

    let args: Vec<String> = std::env::args().collect();
    if args
        .iter()
        .any(|a| a == "--diag-runtime" || a == "diag-runtime")
    {
        let path = args
            .iter()
            .position(|a| a == "--path")
            .and_then(|i| args.get(i.saturating_add(1)))
            .map(|s| s.to_string())
            .unwrap_or_else(|| "log/runtime.txt".to_string());
        return run_diag_runtime(&path);
    }
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
        && let Err(e) = run_bootstrap_script(false)
    {
        eprintln!("[ying] 依赖 bootstrap 失败：{e:#}");
    }

    let code = match run() {
        Ok(code) => code,
        Err(e) => {
            recover_terminal_best_effort();
            eprintln!("[ying] 启动失败：{e:#}");
            1
        }
    };
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
            tool: "list".to_string(),
            op: Some("dir".to_string()),
            cwd: Some(".".to_string()),
            depth: Some(1),
            brief: Some("selfcheck".to_string()),
            ..ToolCall::default()
        },
        ToolCall {
            tool: "list".to_string(),
            op: Some("files".to_string()),
            cwd: Some(".".to_string()),
            names: Some(vec!["Cargo.toml".to_string()]),
            brief: Some("selfcheck".to_string()),
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

fn run_diag_runtime(path: &str) -> anyhow::Result<()> {
    use chrono::NaiveDateTime;

    let path = path.trim();
    if path.is_empty() {
        anyhow::bail!("diag-runtime: path 为空");
    }
    let text = std::fs::read_to_string(path).context("读取 runtime log 失败")?;
    let mut counts: std::collections::BTreeMap<String, u64> = std::collections::BTreeMap::new();
    let mut pty_ready: Vec<(NaiveDateTime, u64, String)> = Vec::new();
    let mut pty_done: Vec<(NaiveDateTime, u64)> = Vec::new();

    fn parse_runtime_header(line: &str) -> Option<(Option<NaiveDateTime>, String)> {
        //（1）新格式（文本块）：
        // ==== [2026-02-21 13:45:12] [INFO] event.name seq=1 run_id=... thread=... ====
        let s = line.trim().strip_prefix("==== [")?;
        let ts_end = s.find(']')?;
        let ts_str = &s[..ts_end];
        let ts = NaiveDateTime::parse_from_str(ts_str, "%Y-%m-%d %H:%M:%S").ok();
        let s = s.get(ts_end.saturating_add(2)..)?;
        let s = s.strip_prefix('[')?;
        let lvl_end = s.find(']')?;
        let s = s.get(lvl_end.saturating_add(2)..)?;
        let seq_pos = s.find(" seq=").unwrap_or(s.len());
        let event = s[..seq_pos].trim().to_string();
        Some((ts, event))
    }

    let mut iter = text.lines().enumerate().peekable();
    while let Some((lineno, raw)) = iter.next() {
        let line = raw.trim_end();
        let line_trim = line.trim();
        if line_trim.is_empty() {
            continue;
        }

        //（1）新版文本块 runtime log
        if line_trim.starts_with("==== [") {
            let Some((ts_opt, event)) = parse_runtime_header(line_trim) else {
                continue;
            };
            *counts.entry(event.clone()).or_default() += 1;

            //（1）header 之后紧跟 data(JSON pretty) + ---- 分隔。只在需要时解析。
            let mut data_block = String::new();
            for (_, l) in iter.by_ref() {
                if l.trim_end() == "----" {
                    break;
                }
                data_block.push_str(l);
                data_block.push('\n');
            }

            if let Some(ts) = ts_opt {
                if event == "pty.ready" {
                    if let Ok(v) = serde_json::from_str::<serde_json::Value>(&data_block) {
                        let job_id = v.get("job_id").and_then(|x| x.as_u64()).unwrap_or(0);
                        let cmd = v.get("cmd").and_then(|x| x.as_str()).unwrap_or("");
                        pty_ready.push((ts, job_id, cmd.to_string()));
                    }
                } else if event == "pty.done"
                    && let Ok(v) = serde_json::from_str::<serde_json::Value>(&data_block)
                {
                    let job_id = v.get("job_id").and_then(|x| x.as_u64()).unwrap_or(0);
                    pty_done.push((ts, job_id));
                }
            }
            continue;
        }

        //（1）兼容旧版 JSONL runtime log（历史文件可能仍存在）
        let v: serde_json::Value = match serde_json::from_str(line_trim) {
            Ok(v) => v,
            Err(_) => continue,
        };
        let ts = v
            .get("ts")
            .and_then(|x| x.as_str())
            .and_then(|s| NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S").ok());
        let event = v
            .get("event")
            .and_then(|x| x.as_str())
            .unwrap_or("(unknown)")
            .to_string();
        *counts.entry(event.clone()).or_default() += 1;

        if let Some(ts) = ts {
            if event == "pty.ready" {
                let data = v.get("data").cloned().unwrap_or(serde_json::Value::Null);
                let job_id = data.get("job_id").and_then(|x| x.as_u64()).unwrap_or(0);
                let cmd = data.get("cmd").and_then(|x| x.as_str()).unwrap_or("");
                pty_ready.push((ts, job_id, cmd.to_string()));
            } else if event == "pty.done" {
                let data = v.get("data").cloned().unwrap_or(serde_json::Value::Null);
                let job_id = data.get("job_id").and_then(|x| x.as_u64()).unwrap_or(0);
                pty_done.push((ts, job_id));
            }
        } else if lineno < 3 {
            //（1）避免完全静默：若头几行格式不符合预期，给一点提示。
            //（2）但不当作错误：runtime log 可能混有旧格式/其它日志。
        }
    }

    println!("diag-runtime: {path}");
    println!("events:");
    for (k, v) in counts.iter() {
        println!("  - {k}: {v}");
    }

    //（1）PTY storm：30s 内启动 >=4 个 PTY
    if !pty_ready.is_empty() {
        pty_ready.sort_by_key(|(ts, _, _)| *ts);
        let mut storm_found = false;
        for i in 0..pty_ready.len() {
            let (t0, _, _) = pty_ready[i];
            let mut j = i;
            while j < pty_ready.len() && (pty_ready[j].0 - t0).num_seconds().abs() <= 30 {
                j += 1;
            }
            let n = j.saturating_sub(i);
            if n >= 4 {
                storm_found = true;
                println!("\nwarn: PTY storm detected ({n} starts within 30s)");
                for (ts, job_id, cmd) in pty_ready.iter().take(j).skip(i) {
                    let cmd = crate::truncate_with_suffix(cmd.as_str(), 120);
                    println!("  - {ts} job_id={job_id} cmd={cmd}");
                }
                break;
            }
        }
        if !storm_found {
            println!("\npty: ready={} done={}", pty_ready.len(), pty_done.len());
        }
    }

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
    pub enum ChatFocus {
        Input,
        Chat,
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
    pub const THINK_TOOL_MARKER: &str = "[AITERMUX_THINK]";
    pub const BRIEF_TOOL_MARKER: &str = "[AITERMUX_BRIEF]";

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum MindKind {
        Main,
        Sub,
        Memory,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PulseDir {
        MainToDog,
        DogToMain,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct ContextLine {
        pub fastmemo_label: Option<&'static str>,
        pub fastmemo_n: usize,
        pub date_kb: usize,
        pub date_kb_limit: usize,
        pub date_pct: u8,
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

        pub fn push_tool_mind(&mut self, text: String, mind: Option<MindKind>) {
            let clean = text.trim_end().to_string();
            if clean.is_empty() {
                return;
            }
            self.history.push(Message {
                role: Role::Tool,
                text: clean,
                mind,
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

    pub fn filter_commands_for_input(input: &str, sse_enabled: bool) -> Vec<CommandSpec> {
        if !should_show_command_menu(input) {
            return Vec::new();
        }
        let mut items: Vec<CommandSpec> = vec![
            CommandSpec {
                cmd: "/Terminal",
                desc: "Terminal",
            },
            CommandSpec {
                cmd: "/Cmd",
                desc: "Shell",
            },
            CommandSpec {
                cmd: "/Settings",
                desc: "Settings",
            },
            CommandSpec {
                cmd: "/Help",
                desc: "Help",
            },
        ];
        items.push(CommandSpec {
            cmd: "/SSE",
            desc: if sse_enabled { "Off" } else { "On" },
        });
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
        pub dyncontext_path: String,
        pub dog_context_path: String,
        pub memory_context_path: String,
        pub run_log_path: String,
        pub memo_db_path: String,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct ApiProviderProfile {
        pub base_url: String,
        pub api_key: Option<String>,
        pub model: String,
        pub reasoning_effort: Option<String>,
        pub temperature: Option<f32>,
        pub max_tokens: Option<u32>,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct ApiProviderProfiles {
        pub deepseek: ApiProviderProfile,
        pub codex: ApiProviderProfile,
    }

    #[derive(Clone, Debug, Serialize, Deserialize)]
    #[serde(default)]
    pub struct DogApiConfig {
        pub api_key: Option<String>,
        pub provider: String,
        pub base_url: String,
        pub model: String,
        #[serde(default = "default_reasoning_effort")]
        pub reasoning_effort: Option<String>,
        pub temperature: Option<f32>,
        pub timeout_secs: u64,
        pub max_tokens: Option<u32>,
        #[serde(default)]
        pub provider_profiles: ApiProviderProfiles,
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
        #[serde(default = "default_reasoning_effort")]
        pub reasoning_effort: Option<String>,
        pub temperature: Option<f32>,
        pub timeout_secs: u64,
        pub max_tokens: Option<u32>,
        #[serde(default)]
        pub provider_profiles: ApiProviderProfiles,
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
        #[serde(default = "default_date_kb_limit")]
        pub date_kb_limit: usize,
        pub heartbeat_minutes: usize,
        #[serde(default = "default_true")]
        pub sse_enabled: bool,
        #[serde(default)]
        pub tool_full_access: bool,
        #[serde(default)]
        pub paste_cache_enabled: bool,
        #[serde(default)]
        pub fastmemo_inject_enabled: bool,
        #[serde(default)]
        pub fastmemo_compact_enabled: bool,
        #[serde(default = "default_pty_audit_prompt_path")]
        pub pty_audit_prompt_path: String,
        #[serde(default = "default_pty_help_prompt_path")]
        pub pty_help_prompt_path: String,
        #[serde(default = "default_welcome_shortcuts_prompt_path")]
        pub welcome_shortcuts_prompt_path: String,
        #[serde(default)]
        pub pty_audit_enabled: bool,
    }

    fn default_true() -> bool {
        true
    }

    fn default_date_kb_limit() -> usize {
        256
    }

    fn default_reasoning_effort() -> Option<String> {
        Some("high".to_string())
    }

    fn default_deepseek_base_url() -> String {
        "https://api.deepseek.com/v1".to_string()
    }

    fn default_codex_base_url() -> String {
        "http://154.44.8.149:3500/openai".to_string()
    }

    fn default_pty_audit_prompt_path() -> String {
        "prompt/ptycheck.txt".to_string()
    }

    fn default_pty_help_prompt_path() -> String {
        "config/Documents/Terminalwelcome.txt".to_string()
    }

    fn default_welcome_shortcuts_prompt_path() -> String {
        "config/Documents/Systemwelcome.txt".to_string()
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
                paste_placeholder_char_threshold: 300,
                paste_placeholder_line_threshold: 10,
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
                //（1）省电：空闲时降低 event::poll 唤醒频率（仍保持 1Hz 的运行时钟刷新）。
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
                    .unwrap_or_else(|| "memory/fastcontext.jsonl".to_string()),
                dyncontext_path: std::env::var("YING_DYNCONTEXT_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| crate::context::DYNCONTEXT_PATH_DEFAULT.to_string()),
                dog_context_path: std::env::var("YING_DOG_CONTEXT_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/context/dogcontext.jsonl".to_string()),
                memory_context_path: std::env::var("YING_MEMORY_CONTEXT_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/context/memorycontext.jsonl".to_string()),
                run_log_path: std::env::var("YING_RUN_LOG_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "log/runtime.txt".to_string()),
                memo_db_path: std::env::var("YING_MEMO_DB_PATH")
                    .ok()
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "memory/metamemory.db".to_string()),
            }
        }
    }

    impl Default for ApiProviderProfile {
        fn default() -> Self {
            Self {
                base_url: default_deepseek_base_url(),
                api_key: None,
                model: "deepseek-reasoner".to_string(),
                reasoning_effort: default_reasoning_effort(),
                temperature: Some(1.0),
                max_tokens: Some(5_000),
            }
        }
    }

    impl Default for ApiProviderProfiles {
        fn default() -> Self {
            Self {
                deepseek: ApiProviderProfile::default(),
                codex: ApiProviderProfile {
                    base_url: default_codex_base_url(),
                    api_key: None,
                    model: "gpt-5.3-codex".to_string(),
                    reasoning_effort: default_reasoning_effort(),
                    temperature: Some(1.0),
                    max_tokens: Some(5_000),
                },
            }
        }
    }

    fn default_dog_provider_profiles() -> ApiProviderProfiles {
        let mut profiles = ApiProviderProfiles::default();
        profiles.deepseek.model = "deepseek-reasoner".to_string();
        profiles.deepseek.max_tokens = Some(5_000);
        profiles
    }

    fn default_main_provider_profiles() -> ApiProviderProfiles {
        let mut profiles = ApiProviderProfiles::default();
        profiles.deepseek.model = "deepseek-chat".to_string();
        profiles.deepseek.max_tokens = Some(5_000);
        profiles
    }

    impl Default for DogApiConfig {
        fn default() -> Self {
            Self {
                api_key: None,
                provider: "deepseek".to_string(),
                base_url: default_deepseek_base_url(),
                model: "deepseek-reasoner".to_string(),
                reasoning_effort: default_reasoning_effort(),
                temperature: Some(1.0),
                timeout_secs: 60,
                max_tokens: Some(5_000),
                provider_profiles: default_dog_provider_profiles(),
                prompt_path: "prompt/dogprompt.txt".to_string(),
                prompt_reinject_pct: 80,
                token_total_path: "config/Tokencount.json".to_string(),
            }
        }
    }

    impl Default for MainApiConfig {
        fn default() -> Self {
            Self {
                api_key: None,
                provider: "deepseek".to_string(),
                base_url: default_deepseek_base_url(),
                model: "deepseek-chat".to_string(),
                reasoning_effort: default_reasoning_effort(),
                temperature: Some(1.0),
                timeout_secs: 60,
                max_tokens: Some(5_000),
                provider_profiles: default_main_provider_profiles(),
                prompt_path: "prompt/mainprompt.txt".to_string(),
            }
        }
    }

    impl Default for ContextPromptConfig {
        fn default() -> Self {
            Self {
                main_prompt: "系统：context 已满，需要进行上下文压缩。\
请你根据当前记忆生成日记，并直接调用 memory_add 写入 datememo。\
系统会附带上一条日记用于对比，请避免重复。仅输出工具 JSON，不要附加说明。\
格式：<tool>{\"tool\":\"memory_add\",\"path\":\"datememo\",\"content\":\"YYYY-MM-DD HH:MM:SS | memory | 关键词: ... | 日记: ...\",\"brief\":\"我来更新日记。\"}</tool>"
                    .to_string(),
            }
        }
    }

    impl Default for SystemConfig {
        fn default() -> Self {
            Self {
                context_k: 128,
                date_kb_limit: 256,
                heartbeat_minutes: 5,
                sse_enabled: true,
                tool_full_access: false,
                paste_cache_enabled: false,
                fastmemo_inject_enabled: true,
                fastmemo_compact_enabled: true,
                pty_audit_prompt_path: default_pty_audit_prompt_path(),
                pty_help_prompt_path: default_pty_help_prompt_path(),
                welcome_shortcuts_prompt_path: default_welcome_shortcuts_prompt_path(),
                pty_audit_enabled: false,
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
            //（1）标题栏现在会走“●闪烁→乱码展开”动画；给足时间让短提示能完整读到。
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
