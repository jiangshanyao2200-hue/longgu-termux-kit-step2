use std::fs;
use std::io::{BufRead, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::Instant;

use anyhow::{Context, anyhow};
use chrono::{Local, NaiveDate};
use regex::Regex;
use serde::Deserialize;
use serde_json::Value;

use crate::memory::{DEFAULT_MEMO_DB_PATH, MemoDb, MemoKind, MemoRow};

const BASH_SHELL: &str = "/data/data/com.termux/files/usr/bin/bash";
// 在 Termux 场景下，PATH 可能被其它软件注入导致 `rg` 解析到不可执行的 wrapper。
// 这里优先使用 Termux 自带的绝对路径，避免 search 退化到 grep。
const TERMUX_RG: &str = "/data/data/com.termux/files/usr/bin/rg";
const ADB_SERIAL: &str = "127.0.0.1:5555";
pub(crate) const OUTPUT_MAX_CHARS: usize = 20_000;
pub(crate) const OUTPUT_MAX_LINES: usize = 400;
const READ_MAX_LINES_CAP: usize = 1200;
const SEARCH_DEFAULT_MAX_MATCHES: usize = 200;
const SEARCH_MAX_MATCHES_CAP: usize = 1000;
const SEARCH_MAX_FILESIZE: &str = "1M";
const SEARCH_MAX_FILE_BYTES: usize = 1_048_576;
const SEARCH_MAX_KEYWORDS: usize = 8;
const SEARCH_MAX_GLOBS: usize = 16;
const SEARCH_MAX_FILES_SCAN: usize = 8_000;
const SEARCH_TIMEOUT_SECS: u64 = 30;
const SEARCH_CONTEXT_DEFAULT_LINES: usize = 20;
const SEARCH_CONTEXT_DEFAULT_FILES: usize = 20;
const SEARCH_CONTEXT_DEFAULT_HITS_PER_FILE: usize = 3;
const SEARCH_CONTEXT_MAX_LINES: usize = 80;
const SEARCH_CONTEXT_MAX_FILES: usize = 60;
const SEARCH_CONTEXT_MAX_HITS_PER_FILE: usize = 20;
const TOOL_TIMEOUT_SECS: u64 = 10;
const TOOL_TIMEOUT_KILL_SECS: u64 = 2;
// termux_api 默认给更宽松的超时：避免某些系统 API（如 wifi 扫描/定位/SAF）在弱机上经常误超时。
const TERMUX_API_TIMEOUT_SECS: u64 = 25;
const PATCH_TIMEOUT_MIN_SECS: u64 = 25;
const PATCH_TIMEOUT_MID_SECS: u64 = 60;
const PATCH_TIMEOUT_MAX_SECS: u64 = 120;

// bash/adb/termux_api 等 shell 类工具：输出过大时落盘到这里，避免把 TUI/上下文撑爆。
const SHELL_CACHE_DIR: &str = "log/bash-cache";
const ADB_CACHE_DIR: &str = "log/adb-cache";
const TERMUX_API_CACHE_DIR: &str = "log/termux-api-cache";
// 统一导出触发的字节兜底阈值：当输出极大时无论行数/字符数是否超限，都落盘。
pub(crate) const EXPORT_SAVE_THRESHOLD_BYTES: usize = 1_000_000;
const SEARCH_CACHE_DIR: &str = "log/search-cache";
const SEARCH_SAVE_THRESHOLD_BYTES: usize = 400_000;
const MEMORY_CACHE_DIR: &str = "log/memory-cache";
const MEMORY_SAVE_THRESHOLD_BYTES: usize = 400_000;
const TOOL_OUTPUT_MAX_CHARS: usize = 12_000;
const TOOL_OUTPUT_MAX_LINES: usize = 240;
const TOOL_OUTPUT_RAW_MAX_CHARS: usize = 20_000;
const TOOL_OUTPUT_RAW_MAX_LINES: usize = 400;
const TOOL_META_RAW_MAX_CHARS: usize = 20_000;
const TOOL_META_RAW_MAX_LINES: usize = 200;
// 当工具输出过大需要落盘时，优先返回“头尾预览”，避免上下文被整段日志撑爆。
const EXPORTED_PREVIEW_MAX_LINES: usize = 160;
const EXPORTED_PREVIEW_MAX_CHARS: usize = 12_000;

#[derive(Debug, Clone)]
pub(crate) struct ExportedOutputMeta {
    pub(crate) path: String,
    pub(crate) bytes: usize,
    pub(crate) lines: usize,
}

pub(crate) fn exported_meta(path: String, bytes: usize, lines: usize) -> ExportedOutputMeta {
    ExportedOutputMeta { path, bytes, lines }
}

pub(crate) fn format_exported_notice(meta: &ExportedOutputMeta) -> String {
    let size = format_bytes(meta.bytes as u64);
    format!(
        "【输出已导出】saved:{} | size:{} | lines:{}",
        meta.path, size, meta.lines
    )
}

pub(crate) fn truncate_export_preview(text: &str) -> String {
    truncate_tool_payload(text, EXPORTED_PREVIEW_MAX_LINES, EXPORTED_PREVIEW_MAX_CHARS)
}

fn maybe_export_text(
    cache_dir: &str,
    file_prefix: &str,
    text: &str,
    out_lines_cap: usize,
    out_chars_cap: usize,
) -> (String, Option<ExportedOutputMeta>) {
    let total_bytes = text.as_bytes().len();
    let raw_lines = text.lines().count();
    let truncated_by_lines = raw_lines > out_lines_cap;
    let truncated_by_chars = text.chars().count() > out_chars_cap;
    let need_save =
        total_bytes > MEMORY_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    if !need_save {
        return (
            truncate_tool_payload(text, out_lines_cap, out_chars_cap),
            None,
        );
    }
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let pid = unsafe { libc::getpid() };
    let path = format!("{cache_dir}/{file_prefix}_{ts}_{pid}.log");
    let saved = try_write_shell_cache_impl(cache_dir, &path, text.as_bytes(), &[]);
    if !saved {
        return (
            truncate_tool_payload(text, out_lines_cap, out_chars_cap),
            None,
        );
    }
    let meta = exported_meta(path, total_bytes, raw_lines);
    let preview = truncate_export_preview(text);
    let mut out = String::new();
    out.push_str(&format_exported_notice(&meta));
    if !preview.trim().is_empty() {
        out.push_str("\n\n");
        out.push_str(preview.trim_end());
    }
    (out, Some(meta))
}

fn memo_db_path_display() -> String {
    std::env::var("YING_MEMO_DB_PATH").unwrap_or_else(|_| DEFAULT_MEMO_DB_PATH.to_string())
}

fn pick_search_output_limits(call: &ToolCall) -> (usize, usize) {
    // 低/中/高：低 400/20000，中翻倍，高=低*3
    let lvl = call
        .output_level
        .as_deref()
        .unwrap_or("mid")
        .trim()
        .to_ascii_lowercase();
    let (base_lines, base_chars) = match lvl.as_str() {
        "low" | "l" | "small" => (400usize, 20_000usize),
        "high" | "h" | "large" => (1200usize, 60_000usize),
        _ => (800usize, 40_000usize),
    };
    let lines = call.max_lines.unwrap_or(base_lines).clamp(80, 1200);
    let chars = call.max_chars.unwrap_or(base_chars).clamp(4_000, 60_000);
    (lines, chars)
}

fn tool_output_limits_for_history(call: &ToolCall) -> (usize, usize) {
    if call.tool == "search" {
        return pick_search_output_limits(call);
    }
    (TOOL_OUTPUT_RAW_MAX_LINES, TOOL_OUTPUT_RAW_MAX_CHARS)
}
const PATCH_MAX_BYTES: usize = 500_000;
pub(crate) const EDIT_MAX_FILE_BYTES: usize = 800_000;
pub(crate) const EDIT_MAX_MATCHES: usize = 400;
const MEMORY_CHECK_DEFAULT_RESULTS: usize = 10;
const MEMORY_CHECK_MAX_RESULTS: usize = 20;
const MEMORY_ADD_PREVIEW_LINES: usize = 8;
const MEMORY_ADD_PREVIEW_CHARS: usize = 800;
const SEARCH_EXCLUDE_DIRS: &[&str] = &[
    ".git",
    ".svn",
    ".hg",
    "node_modules",
    "target",
    "dist",
    "build",
    "out",
    ".cache",
];

fn tool_tag_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    // 只匹配“独立成行”的 <tool>...</tool>：
    // - 避免把正文里的示例（如行内代码 `...<tool>{...}</tool>...`）误判为真实工具调用
    // - 符合协议：工具调用应单独输出，不夹杂其它文字
    RE.get_or_init(|| {
        Regex::new(r"(?ims)^[ \t]*<tool>([\s\S]*?)</tool>[ \t]*$").expect("tool regex")
    })
}

fn parse_usize_value(v: &Value) -> Option<usize> {
    v.as_u64()
        .map(|n| n as usize)
        .or_else(|| v.as_str().and_then(|s| s.trim().parse::<usize>().ok()))
}

fn parse_u64_value(v: &Value) -> Option<u64> {
    v.as_u64()
        .or_else(|| v.as_i64().and_then(|n| (n > 0).then_some(n as u64)))
        .or_else(|| v.as_str().and_then(|s| s.trim().parse::<u64>().ok()))
}

fn parse_u64_list_value_max(v: &Value, max: usize) -> Option<Vec<u64>> {
    match v {
        Value::Null => None,
        Value::Array(arr) => {
            let mut out: Vec<u64> = Vec::new();
            for item in arr {
                if let Some(n) = parse_u64_value(item).filter(|x| *x > 0) {
                    if out.contains(&n) {
                        continue;
                    }
                    out.push(n);
                    if out.len() >= max {
                        break;
                    }
                }
            }
            (!out.is_empty()).then_some(out)
        }
        other => value_to_nonempty_string(other).and_then(|s| {
            let mut out: Vec<u64> = Vec::new();
            for tok in parse_list_tokens(&s, max) {
                if let Ok(n) = tok.trim().parse::<u64>() {
                    if n == 0 || out.contains(&n) {
                        continue;
                    }
                    out.push(n);
                    if out.len() >= max {
                        break;
                    }
                }
            }
            (!out.is_empty()).then_some(out)
        }),
    }
}

fn parse_toggle_input(s: &str) -> Option<bool> {
    let t = s.trim().to_ascii_lowercase();
    match t.as_str() {
        "1" | "true" | "on" | "yes" | "y" => Some(true),
        "0" | "false" | "off" | "no" | "n" => Some(false),
        _ => None,
    }
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct ToolCall {
    #[serde(default)]
    pub tool: String,
    #[serde(default)]
    pub input: String,
    #[serde(default)]
    // brief: 一句话说明调用目的，用于可视化与审计。
    pub brief: Option<String>,
    #[serde(default)]
    pub path: Option<String>,
    // list：批量目标路径（优先于 path/input）
    #[serde(default)]
    pub paths: Option<Vec<String>>,
    // list：同目录下的文件名数组（用于一次查看多个文件信息）
    #[serde(default)]
    pub names: Option<Vec<String>>,
    #[serde(default)]
    pub content: Option<String>,
    #[serde(default)]
    pub pattern: Option<String>,
    #[serde(default)]
    pub root: Option<String>,
    #[serde(default)]
    pub patch: Option<String>,
    #[serde(default)]
    pub find: Option<String>,
    #[serde(default)]
    pub replace: Option<String>,
    #[serde(default)]
    pub count: Option<usize>,
    #[serde(default)]
    pub start_line: Option<usize>,
    #[serde(default)]
    pub max_lines: Option<usize>,
    #[serde(default)]
    pub max_chars: Option<usize>,
    #[serde(default)]
    pub head: Option<bool>,
    #[serde(default)]
    pub tail: Option<bool>,
    #[serde(default)]
    pub file: Option<bool>,
    #[serde(default)]
    pub time: Option<String>,
    #[serde(default)]
    pub keywords: Option<String>,
    #[serde(default)]
    pub diary: Option<String>,
    #[serde(default)]
    pub category: Option<String>,
    #[serde(default)]
    pub section: Option<String>,
    #[serde(default)]
    pub target: Option<String>,
    #[serde(default)]
    pub date_start: Option<String>,
    #[serde(default)]
    pub date_end: Option<String>,
    #[serde(default)]
    pub heartbeat_minutes: Option<usize>,
    #[serde(default)]
    pub interactive: Option<bool>,
    // pty tool：目标 job_id（kill 等）
    #[serde(default)]
    pub job_id: Option<u64>,
    // pty tool：目标 pid（kill 等）；兼容用户习惯，等价于 job_id
    #[serde(default)]
    pub pid: Option<u64>,
    // pty tool：批量目标 job_ids（kill 等）
    #[serde(default)]
    pub job_ids: Option<Vec<u64>>,
    // pty tool：批量目标 pids（kill 等）；等价于 job_ids
    #[serde(default)]
    pub pids: Option<Vec<u64>>,
    // pty tool：批量启动命令（run）
    #[serde(default)]
    pub commands: Option<Vec<String>>,
    #[serde(default)]
    pub timeout_secs: Option<u64>,
    #[serde(default)]
    pub timeout_ms: Option<u64>,
    // shell tools：工作目录（bash/adb/termux_api）
    #[serde(default)]
    pub cwd: Option<String>,
    // shell tools：输出落盘策略 auto/always/never
    #[serde(default)]
    pub save: Option<String>,
    // shell tools：额外可接受退出码（除 0 外也视为“成功/可预期”）
    #[serde(default)]
    pub ok_exit_codes: Option<Vec<i32>>,

    // memory tools：将对应工具切换到“记忆源”而不是文件系统源
    #[serde(default)]
    pub memory: Option<bool>,
    // memory tools：datememo/metamemo 的“区域/范围”选择（避免与 fastmemo 的 section 混用）
    // - datememo: recent|past|older
    // - metamemo: day
    #[serde(default)]
    pub region: Option<String>,
    // memory tools：sqlite 扫描上限（用于 datememo past/older 的 OR 拉取候选）
    #[serde(default)]
    pub scan_limit: Option<usize>,
    // read_file：显式全量阅读（仅对小文件允许）
    #[serde(default)]
    pub full: Option<bool>,

    // search 强化（可选）：命中上下文片段 + glob 过滤
    #[serde(default)]
    pub context: Option<bool>,
    #[serde(default)]
    pub context_lines: Option<usize>,
    #[serde(default)]
    pub context_files: Option<usize>,
    #[serde(default)]
    pub context_hits_per_file: Option<usize>,
    #[serde(default)]
    pub include_glob: Option<Vec<String>>,
    #[serde(default)]
    pub exclude_glob: Option<Vec<String>>,
    #[serde(default)]
    pub exclude_dirs: Option<Vec<String>>,

    // search 输出上限控制（可选）
    #[serde(default)]
    pub output_level: Option<String>,

    // list：递归深度（默认 1，最大 3）。为避免 token/性能暴涨，仍受 max_entries/输出预算约束。
    #[serde(default)]
    pub depth: Option<usize>,
    // list：最多返回多少条 entry（每条一行）。超出会截断并提示。
    #[serde(default)]
    pub max_entries: Option<usize>,
    // write_file：覆盖已存在的非空文件（需要显式确认）
    #[serde(default)]
    pub overwrite: Option<bool>,

    // 通用 op（用于 pty/list 等工具）
    #[serde(default)]
    pub op: Option<String>,
    // steps：顺序执行多个子步骤（保留字段；当前未作为工具对外暴露）
    #[serde(default)]
    pub steps: Option<Vec<ToolCall>>,
    #[serde(default)]
    pub src: Option<String>,
    #[serde(default)]
    pub dst: Option<String>,
    // 是否自动创建父目录（仅部分工具用到）
    #[serde(default)]
    pub parents: Option<bool>,
    #[serde(default)]
    pub recursive: Option<bool>,
    #[serde(default)]
    pub mode: Option<String>,
    // search(format)：输出格式（如 coords 只返回坐标）
    #[serde(default)]
    pub format: Option<String>,
}

fn parse_list_tokens(raw: &str, max: usize) -> Vec<String> {
    let s = raw.trim();
    if s.is_empty() || max == 0 {
        return Vec::new();
    }
    let mut out: Vec<String> = Vec::new();
    let mut buf = String::new();
    let mut quote_end: Option<char> = None;

    fn push_token(out: &mut Vec<String>, buf: &mut String) {
        let t = compact_ws_inline(buf.trim());
        buf.clear();
        if t.is_empty() {
            return;
        }
        if out.iter().any(|x| x.eq_ignore_ascii_case(&t)) {
            return;
        }
        out.push(t);
    }

    for ch in s.chars() {
        if let Some(end) = quote_end {
            if ch == end {
                quote_end = None;
                push_token(&mut out, &mut buf);
            } else {
                buf.push(ch);
            }
            continue;
        }
        match ch {
            '"' => {
                push_token(&mut out, &mut buf);
                quote_end = Some('"');
            }
            '“' => {
                push_token(&mut out, &mut buf);
                quote_end = Some('”');
            }
            '‘' => {
                push_token(&mut out, &mut buf);
                quote_end = Some('’');
            }
            ',' | '，' | ';' | '；' | '\n' | '\t' | ' ' => {
                push_token(&mut out, &mut buf);
            }
            _ => buf.push(ch),
        }
        if out.len() >= max {
            break;
        }
    }
    if quote_end.is_some() {
        push_token(&mut out, &mut buf);
    } else {
        push_token(&mut out, &mut buf);
    }
    out.truncate(max);
    out
}

fn parse_string_list_value_max(v: &Value, max: usize) -> Option<Vec<String>> {
    match v {
        Value::Null => None,
        Value::String(s) => {
            let tokens = parse_list_tokens(s, max);
            (!tokens.is_empty()).then_some(tokens)
        }
        Value::Array(arr) => {
            let mut out: Vec<String> = Vec::new();
            for item in arr {
                if let Value::String(s) = item {
                    let t = s.trim();
                    if t.is_empty() {
                        continue;
                    }
                    if out.iter().any(|x| x.eq_ignore_ascii_case(t)) {
                        continue;
                    }
                    out.push(t.to_string());
                    if out.len() >= max {
                        break;
                    }
                }
            }
            out.truncate(max);
            (!out.is_empty()).then_some(out)
        }
        Value::Object(_) | Value::Bool(_) | Value::Number(_) => value_to_nonempty_string(v)
            .and_then(|s| {
                let tokens = parse_list_tokens(&s, max);
                (!tokens.is_empty()).then_some(tokens)
            }),
    }
}

fn parse_steps_value_max(v: &Value, max: usize) -> Option<Vec<ToolCall>> {
    let Value::Array(arr) = v else {
        return None;
    };
    if max == 0 {
        return None;
    }
    let mut out: Vec<ToolCall> = Vec::new();
    for item in arr.iter() {
        let Value::Object(obj) = item else {
            continue;
        };
        let mut m = obj.clone();
        let mut st = ToolCall::default();

        if let Some(v) = m
            .remove("tool")
            .or_else(|| m.remove("name"))
            .or_else(|| m.remove("tool_name"))
            .and_then(|x| x.as_str().map(|s| s.trim().to_string()))
            .filter(|s| !s.is_empty())
        {
            st.tool = v;
        }

        if let Some(v) = m.remove("brief").or_else(|| m.remove("desc"))
            && let Some(s) = v.as_str().map(str::trim).filter(|s| !s.is_empty())
        {
            st.brief = Some(s.to_string());
        }

        if let Some(v) = m
            .remove("input")
            .or_else(|| m.remove("command"))
            .or_else(|| m.remove("cmd"))
        {
            match v {
                Value::String(s) => st.input = s,
                Value::Object(obj) => apply_args_object(&mut st, obj),
                Value::Array(arr) => {
                    let joined = arr
                        .into_iter()
                        .filter_map(|x| x.as_str().map(|s| s.to_string()))
                        .collect::<Vec<_>>()
                        .join(" ");
                    st.input = joined;
                }
                other => st.input = other.to_string(),
            }
        }

        if let Some(Value::Object(obj)) = m.remove("args").or_else(|| m.remove("arguments")) {
            apply_args_object(&mut st, obj);
        }

        apply_flat_fields(&mut st, &mut m);
        out.push(st);
        if out.len() >= max {
            break;
        }
    }
    (!out.is_empty()).then_some(out)
}

fn parse_tool_call_payload(payload: &str) -> Option<ToolCall> {
    let v: Value = serde_json::from_str(payload).ok()?;
    normalize_tool_call(v).ok()
}

fn normalize_tool_call(v: Value) -> anyhow::Result<ToolCall> {
    let Value::Object(mut m) = v else {
        return Err(anyhow!("tool call 不是对象"));
    };

    let tool = m
        .remove("tool")
        .and_then(|x| x.as_str().map(|s| s.trim().to_string()))
        .unwrap_or_default();

    let mut call = ToolCall {
        tool,
        ..Default::default()
    };

    if let Some(v) = m.remove("brief")
        && let Some(s) = v.as_str().map(str::trim).filter(|s| !s.is_empty())
    {
        call.brief = Some(s.to_string());
    }

    if let Some(v) = m.remove("input") {
        match v {
            Value::String(s) => call.input = s,
            other => call.input = other.to_string(),
        }
    }

    apply_flat_fields(&mut call, &mut m);
    let raw_tool = call.tool.clone();
    apply_tool_defaults(&mut call);
    apply_mind_msg_defaults(&mut call, &raw_tool);

    if call.tool.trim().is_empty() {
        return Err(anyhow!("缺少 tool"));
    }
    Ok(call)
}

fn apply_flat_fields(call: &mut ToolCall, m: &mut serde_json::Map<String, Value>) {
    let taken = std::mem::take(m);
    for (key, val) in taken {
        let s = value_to_nonempty_string(&val);
        match key.as_str() {
            "path" => call.path = s,
            "paths" => {
                call.paths = parse_string_list_value_max(&val, 64);
            }
            "name" => {
                call.names = parse_string_list_value_max(&val, 64);
            }
            "content" => call.content = s,
            "message" => call.content = s,
            "pattern" => call.pattern = s,
            "root" => call.root = s,
            "patch" => call.patch = s,
            "find" => call.find = s,
            "replace" => call.replace = s,
            "time" => call.time = s,
            "keywords" => call.keywords = s,
            "diary" => call.diary = s,
            "category" => call.category = s,
            "section" => call.section = s,
            "region" => call.region = s,
            "target" => call.target = s,
            "date_start" => call.date_start = s,
            "date_end" => call.date_end = s,
            "heartbeat_minutes" => {
                if let Some(v) = parse_usize_value(&val) {
                    call.heartbeat_minutes = Some(v);
                }
            }
            "interactive" => {
                call.interactive = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "timeout_secs" => {
                call.timeout_secs = val
                    .as_u64()
                    .or_else(|| val.as_str().and_then(|x| x.trim().parse::<u64>().ok()));
            }
            "timeout_ms" => {
                call.timeout_ms = val
                    .as_u64()
                    .or_else(|| val.as_str().and_then(|x| x.trim().parse::<u64>().ok()));
            }
            "cwd" => {
                call.cwd = s;
            }
            "op" => {
                call.op = s;
            }
            "src" => {
                call.src = s;
            }
            // `to/dest` 对 mind_msg 是 target；否则作为 dst。
            "to" => {
                if call.tool == "mind_msg" {
                    call.target = s;
                } else {
                    call.dst = s;
                }
            }
            "dst" => {
                call.dst = s;
            }
            "parents" => {
                call.parents = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "recursive" => {
                call.recursive = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "mode" => {
                call.mode = s;
            }
            "format" => {
                call.format = s;
            }
            "overwrite" => {
                call.overwrite = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "steps" => {
                call.steps = parse_steps_value_max(&val, 64);
            }
            // pty kill 目标字段：兼容 snake_case 与常见 camelCase（部分模型会输出 jobId/jobIds）。
            "job_id" => {
                call.job_id = parse_u64_value(&val);
            }
            "pid" => {
                // 兼容统一写法：pid:[1,2,3]（数组形式会归一到 pids）
                if val.is_array() {
                    call.pids = parse_u64_list_value_max(&val, 16);
                } else if let Some(list) = parse_u64_list_value_max(&val, 16) {
                    if list.len() > 1 {
                        call.pids = Some(list);
                    } else if let Some(first) = list.first().copied() {
                        call.pid = Some(first);
                    }
                } else {
                    call.pid = parse_u64_value(&val);
                }
            }
            "job_ids" => {
                call.job_ids = parse_u64_list_value_max(&val, 16);
            }
            "pids" => {
                call.pids = parse_u64_list_value_max(&val, 16);
            }
            "commands" => {
                let mut out: Vec<String> = Vec::new();
                match val {
                    Value::Array(arr) => {
                        for x in arr {
                            if let Some(t) = x.as_str().map(str::trim).filter(|t| !t.is_empty()) {
                                out.push(t.to_string());
                            }
                        }
                    }
                    Value::String(text) => {
                        for line in text.lines() {
                            let t = line.trim();
                            if !t.is_empty() {
                                out.push(t.to_string());
                            }
                        }
                    }
                    other => {
                        if let Some(text) = value_to_nonempty_string(&other) {
                            for line in text.lines() {
                                let t = line.trim();
                                if !t.is_empty() {
                                    out.push(t.to_string());
                                }
                            }
                        }
                    }
                }
                if !out.is_empty() {
                    call.commands = Some(out);
                }
            }
            "save" => {
                call.save = s;
            }
            "ok_exit_codes" => {
                if let Value::Array(arr) = val {
                    let mut out: Vec<i32> = Vec::new();
                    for x in arr {
                        if let Some(n) = x.as_i64() {
                            out.push(n as i32);
                        } else if let Some(t) = x.as_str().map(str::trim).filter(|t| !t.is_empty())
                            && let Ok(n) = t.parse::<i32>()
                        {
                            out.push(n);
                        }
                    }
                    if !out.is_empty() {
                        call.ok_exit_codes = Some(out);
                    }
                } else if let Some(t) = s.as_deref() {
                    let nums = parse_list_tokens(t, 16)
                        .into_iter()
                        .filter_map(|x| x.parse::<i32>().ok())
                        .collect::<Vec<_>>();
                    if !nums.is_empty() {
                        call.ok_exit_codes = Some(nums);
                    }
                }
            }
            "memory" => {
                call.memory = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "scope" => {
                call.region = s;
            }
            "scan_limit" => {
                call.scan_limit = parse_usize_value(&val);
            }
            "full" => {
                call.full = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "output_level" => {
                call.output_level = s;
            }
            "depth" => {
                call.depth = parse_usize_value(&val);
            }
            "max_entries" => {
                call.max_entries = parse_usize_value(&val);
            }
            "context" => {
                call.context = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "context_lines" => {
                call.context_lines = parse_usize_value(&val);
            }
            "context_files" => {
                call.context_files = parse_usize_value(&val);
            }
            "context_hits_per_file" => {
                call.context_hits_per_file = parse_usize_value(&val);
            }
            "include_glob" => {
                call.include_glob = parse_string_list_value_max(&val, SEARCH_MAX_GLOBS);
            }
            "exclude_glob" => {
                call.exclude_glob = parse_string_list_value_max(&val, SEARCH_MAX_GLOBS);
            }
            "exclude_dirs" => {
                call.exclude_dirs = parse_string_list_value_max(&val, 32);
            }
            "count" => call.count = parse_usize_value(&val),
            "start_line" => {
                call.start_line = parse_usize_value(&val);
            }
            "max_lines" => {
                call.max_lines = parse_usize_value(&val);
            }
            "max_chars" => {
                call.max_chars = parse_usize_value(&val);
            }
            "head" => {
                call.head = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|v| v.trim().parse::<bool>().ok()));
            }
            "tail" => {
                call.tail = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|v| v.trim().parse::<bool>().ok()));
            }
            "file" => {
                call.file = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|v| v.trim().parse::<bool>().ok()));
            }
            _ => {}
        }
    }
}

fn apply_args_object(call: &mut ToolCall, obj: serde_json::Map<String, Value>) {
    let mut m = obj;
    // 图灵化模块：允许把参数按模块放入子对象中（run/out/expect/policy），最后再合并扁平字段。
    for key in ["run", "out", "expect", "policy"] {
        if let Some(Value::Object(sub)) = m.remove(key) {
            let mut sub = sub;
            apply_flat_fields(call, &mut sub);
        }
    }
    if call.input.trim().is_empty()
        && let Some(s) = m
            .get("input")
            .and_then(|v| v.as_str().map(str::trim).filter(|s| !s.is_empty()))
    {
        call.input = s.to_string();
    }
    apply_flat_fields(call, &mut m);
}

fn value_to_nonempty_string(v: &Value) -> Option<String> {
    match v {
        Value::String(s) => {
            let t = s.trim();
            (!t.is_empty()).then(|| t.to_string())
        }
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        Value::Null => None,
        other => {
            let t = other.to_string();
            let t = t.trim();
            (!t.is_empty()).then(|| t.to_string())
        }
    }
}

fn apply_tool_defaults(call: &mut ToolCall) {
    match call.tool.as_str() {
        "search" => {
            if call.root.as_deref().unwrap_or("").trim().is_empty()
                && call.path.as_deref().unwrap_or("").trim().is_empty()
                && call.input.trim().is_empty()
            {
                call.root = Some(".".to_string());
            }
        }
        "skills_mcp" => {
            if call.category.as_deref().unwrap_or("").trim().is_empty()
                && call.input.trim().is_empty()
            {
                call.input = "编程类".to_string();
            }
        }
        _ => {}
    }
}

fn normalize_mind_target_value(raw: &str) -> Option<String> {
    let t = raw.trim().to_ascii_lowercase();
    if t.is_empty() {
        return None;
    }
    match t.as_str() {
        "dog" | "sub" | "todog" | "to_dog" | "to-dog" | "潜意识" => Some("dog".to_string()),
        "main" | "tomain" | "to_main" | "to-main" | "主意识" | "萤" => Some("main".to_string()),
        _ => None,
    }
}

fn apply_mind_msg_defaults(call: &mut ToolCall, raw_tool: &str) {
    if call.tool != "mind_msg" {
        return;
    }
    if call.target.is_none()
        && let Some(target) = normalize_mind_target_value(raw_tool)
    {
        call.target = Some(target);
    }
    if call.target.is_none()
        && let Some(target) = normalize_mind_target_value(&call.input)
    {
        call.target = Some(target);
        call.input.clear();
    }
}

fn parse_search_keywords(raw: &str) -> Vec<String> {
    parse_list_tokens(raw, SEARCH_MAX_KEYWORDS)
}

fn extract_search_inline_ctx_marker(raw_pattern: &str) -> (String, Option<usize>, bool) {
    // 允许在 pattern 字符串里写快捷控制：
    // - ctx / context：开启上下文（使用默认行数）
    // - ctx:20 / ctx=20 / ctx:+20 / ctx:-20 / ctx:±20：开启上下文并设置行数
    //
    // 轻量容错：识别后会从 pattern 里剔除该标记，避免误匹配。
    let s = raw_pattern.trim();
    if s.is_empty() {
        return (String::new(), None, false);
    }
    let mut enabled = false;
    let mut lines: Option<usize> = None;
    let mut kept: Vec<&str> = Vec::new();
    for tok in s.split_whitespace() {
        let lower = tok.trim().to_ascii_lowercase();
        if lower == "ctx" || lower == "context" {
            enabled = true;
            continue;
        }
        if let Some(rest) = lower
            .strip_prefix("ctx:")
            .or_else(|| lower.strip_prefix("ctx="))
        {
            enabled = true;
            let rest = rest.trim();
            let rest = rest.strip_prefix('±').unwrap_or(rest);
            let rest = rest.strip_prefix('+').unwrap_or(rest);
            let rest = rest.strip_prefix('-').unwrap_or(rest);
            if let Ok(n) = rest.parse::<usize>() {
                lines = Some(n);
            }
            continue;
        }
        if let Some(rest) = lower
            .strip_prefix("context:")
            .or_else(|| lower.strip_prefix("context="))
        {
            enabled = true;
            let rest = rest.trim();
            let rest = rest.strip_prefix('±').unwrap_or(rest);
            let rest = rest.strip_prefix('+').unwrap_or(rest);
            let rest = rest.strip_prefix('-').unwrap_or(rest);
            if let Ok(n) = rest.parse::<usize>() {
                lines = Some(n);
            }
            continue;
        }
        kept.push(tok);
    }
    (kept.join(" ").trim().to_string(), lines, enabled)
}

fn normalize_search_pattern(raw_pattern: &str) -> (String, Vec<String>) {
    let raw_pattern = raw_pattern.trim();
    let keywords = parse_search_keywords(raw_pattern);
    if keywords.len() == 1 {
        return (keywords[0].clone(), keywords);
    }
    (raw_pattern.to_string(), keywords)
}

fn search_pattern_looks_like_regex(raw: &str) -> bool {
    // 默认 search 走正则（rg/grep），但“多关键词 AND”是额外增强：
    // - 仅在看起来像“纯关键词”时启用 AND，避免破坏用户正则语义（尤其是带空格的正则）。
    // - 一旦包含常见正则元字符，就视为 regex。
    let s = raw.trim();
    if s.is_empty() {
        return false;
    }
    const META: &str = "^$.*+?()[]{}|\\";
    s.chars().any(|c| META.contains(c))
}

fn parse_search_dsl_fallback(raw: &str) -> Option<(String, String, Option<bool>)> {
    // 兼容一些“误把字段写进 pattern 字符串”的情况：
    // - pattern=<...> in <root>
    // - file=<...> in <root>
    // - root=<root>, pattern=<...>
    let s = raw.trim();
    if s.is_empty() {
        return None;
    }

    // 1) <key>=<val> in <root>
    if let Some((left, right)) = s.split_once(" in ") {
        let left = left.trim();
        let right = right.trim();
        if !right.is_empty()
            && (left.to_ascii_lowercase().starts_with("pattern=")
                || left.to_ascii_lowercase().starts_with("file="))
        {
            let lower = left.to_ascii_lowercase();
            let is_file = lower.starts_with("file=");
            let val = left.split_once('=').map(|(_, v)| v.trim()).unwrap_or("");
            if !val.is_empty() {
                return Some((val.to_string(), right.to_string(), Some(is_file)));
            }
        }
    }

    // 2) root=... pattern=...
    let lower = s.to_ascii_lowercase();
    if lower.contains("root=") && lower.contains("pattern=") {
        // 粗解析：按逗号分段
        let mut root = None;
        let mut pat = None;
        for part in s.split(',') {
            let t = part.trim();
            let tl = t.to_ascii_lowercase();
            if let Some(v) = tl.strip_prefix("root=") {
                let v = t[t.len().saturating_sub(v.len())..].trim(); // keep original
                if !v.is_empty() {
                    root = Some(v.to_string());
                }
            }
            if let Some(v) = tl.strip_prefix("pattern=") {
                let v = t[t.len().saturating_sub(v.len())..].trim();
                if !v.is_empty() {
                    pat = Some(v.to_string());
                }
            }
        }
        if let (Some(p), Some(r)) = (pat, root) {
            return Some((p, r, None));
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SearchHit {
    path: PathBuf,
    line_no: usize,
    line: String,
}

fn glob_to_regex(glob: &str) -> Option<Regex> {
    let g = glob.trim();
    if g.is_empty() {
        return None;
    }
    let mut re = String::new();
    re.push('^');
    let mut chars = g.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '*' => {
                if chars.peek() == Some(&'*') {
                    let _ = chars.next();
                    re.push_str(".*");
                } else {
                    re.push_str("[^/]*");
                }
            }
            '?' => re.push_str("[^/]"),
            '.' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '\\' => {
                re.push('\\');
                re.push(ch);
            }
            _ => re.push(ch),
        }
    }
    re.push('$');
    Regex::new(&re).ok()
}

fn compile_globs(globs: &[String]) -> Vec<Regex> {
    globs
        .iter()
        .filter_map(|g| glob_to_regex(g))
        .collect::<Vec<_>>()
}

fn search_path_allowed(
    path: &Path,
    include_re: &[Regex],
    exclude_re: &[Regex],
    extra_exclude_dirs: &[String],
) -> bool {
    let s = path.to_string_lossy().replace('\\', "/");
    if !include_re.is_empty() && !include_re.iter().any(|re| re.is_match(&s)) {
        return false;
    }
    if exclude_re.iter().any(|re| re.is_match(&s)) {
        return false;
    }
    if extra_exclude_dirs.is_empty() {
        return true;
    }
    for comp in path.components() {
        let c = comp.as_os_str().to_string_lossy();
        if extra_exclude_dirs
            .iter()
            .any(|d| d.eq_ignore_ascii_case(c.as_ref()))
        {
            return false;
        }
    }
    true
}

fn parse_search_hit_line(line: &str) -> Option<(String, usize, String)> {
    // 兼容 rg/grep 的常见输出：path:line:content
    let raw = line.trim_end();
    let a = raw.find(':')?;
    let b_rel = raw[a + 1..].find(':')?;
    let b = a + 1 + b_rel;
    let path = raw[..a].trim().to_string();
    let line_no = raw[a + 1..b].trim().parse::<usize>().ok()?;
    let content = raw[b + 1..].to_string();
    Some((path, line_no, content))
}

pub(crate) fn sanitize_search_line(raw: &str) -> String {
    // 防止把文件内容里的控制字符/ANSI escape 注入到 TUI，导致闪烁/乱码/震动等现象。
    // 规则：保留可见字符与 '\t'；其它控制字符全部替换为空格。
    let mut out = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch == '\t' {
            out.push('\t');
            continue;
        }
        if ch >= ' ' && ch != '\u{7f}' {
            // 过滤 ESC（ANSI）与其它 C0/C1 控制
            if ch == '\u{1b}' {
                out.push(' ');
            } else {
                out.push(ch);
            }
        } else {
            out.push(' ');
        }
    }
    out
}

pub(crate) fn is_probably_binary_file(path: &Path) -> bool {
    // 快速二进制探测：存在 NUL 基本可判定为 binary。
    // 这能避免 sqlite/db 等文件被 AND/context 模式扫描后产生乱码与控制序列。
    let Ok(mut f) = fs::File::open(path) else {
        return false;
    };
    let mut buf = [0u8; 2048];
    let Ok(n) = f.read(&mut buf) else {
        return false;
    };
    buf[..n].iter().any(|b| *b == 0)
}

fn resolve_search_hit_path(root: &str, hit_path: &str) -> PathBuf {
    let hit = hit_path.trim();
    if hit.starts_with('/') {
        return PathBuf::from(hit);
    }
    let root = root.trim();
    if root.is_empty() || root == "." {
        return PathBuf::from(hit);
    }
    if hit.starts_with(root) {
        return PathBuf::from(hit);
    }
    Path::new(root).join(hit)
}

fn run_search_coords(call: &ToolCall, pattern: &str, root: &str) -> anyhow::Result<ToolOutcome> {
    let started = Instant::now();
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
        .or(Some(SEARCH_TIMEOUT_SECS));

    let max_matches = call
        .count
        .unwrap_or(SEARCH_DEFAULT_MAX_MATCHES)
        .clamp(1, SEARCH_MAX_MATCHES_CAP);

    let include_globs = call.include_glob.clone().unwrap_or_default();
    let exclude_globs = call.exclude_glob.clone().unwrap_or_default();
    let extra_exclude_dirs = call.exclude_dirs.clone().unwrap_or_default();
    let include_re = compile_globs(&include_globs);
    let exclude_re = compile_globs(&exclude_globs);

    let mut engine = "rg";
    let mut rg_args: Vec<String> = vec![
        "--line-number".to_string(),
        "--no-heading".to_string(),
        "-S".to_string(),
        "--max-count".to_string(),
        max_matches.to_string(),
        "--max-filesize".to_string(),
        SEARCH_MAX_FILESIZE.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in ["!**/*.db", "!**/*.sqlite", "!**/*.sqlite3"] {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in include_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for g in exclude_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!{g}"));
    }
    rg_args.push("--".to_string());
    rg_args.push(pattern.to_string());
    rg_args.push(root.to_string());

    let mut grep_args: Vec<String> = vec![
        "-RIn".to_string(),
        "-I".to_string(),
        "--binary-files=without-match".to_string(),
        "-m".to_string(),
        max_matches.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push((*d).to_string());
    }
    for g in ["*.db", "*.sqlite", "*.sqlite3"] {
        grep_args.push("--exclude".to_string());
        grep_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push(d.to_string());
    }
    for g in include_globs.iter() {
        grep_args.push(format!("--include={g}"));
    }
    for g in exclude_globs.iter() {
        grep_args.push(format!("--exclude={g}"));
    }
    grep_args.push("--".to_string());
    grep_args.push(pattern.to_string());
    grep_args.push(root.to_string());

    let rg_refs: Vec<&str> = rg_args.iter().map(|s| s.as_str()).collect();
    let grep_refs: Vec<&str> = grep_args.iter().map(|s| s.as_str()).collect();
    let rg_bin = if Path::new(TERMUX_RG).is_file() {
        TERMUX_RG
    } else {
        "rg"
    };
    let mut out = run_command_output_with_optional_timeout(rg_bin, &rg_refs, timeout_secs);
    if out.is_err() {
        engine = "grep";
        out = run_command_output_with_optional_timeout("grep", &grep_refs, timeout_secs);
    }
    let (code, stdout, _stderr, timed_out) =
        out.unwrap_or_else(|_| (127, String::new(), "search failed".to_string(), false));
    let status = status_label(code, timed_out);

    fn real_display_pathbuf(path: &Path) -> String {
        std::fs::canonicalize(path)
            .unwrap_or_else(|_| path.to_path_buf())
            .to_string_lossy()
            .to_string()
    }

    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    let mut coords: Vec<String> = Vec::new();
    for l in stdout.lines() {
        let Some((p, ln, _content)) = parse_search_hit_line(l) else {
            continue;
        };
        let path = resolve_search_hit_path(root, &p);
        if !search_path_allowed(&path, &include_re, &exclude_re, &extra_exclude_dirs) {
            continue;
        }
        let real = real_display_pathbuf(&path);
        let key = format!("{real}:{ln}");
        if seen.insert(key.clone()) {
            coords.push(key);
        }
        if coords.len() >= max_matches {
            break;
        }
    }
    if coords.is_empty() {
        return Ok(ToolOutcome {
            user_message: "未找到匹配".to_string(),
            log_lines: vec![format!(
                "状态:{status} | 耗时:{}ms | op:search | format:coords | engine:{} | root:{} | matches:0",
                started.elapsed().as_millis(),
                engine,
                shorten_path(root),
            )],
        });
    }

    let mut body = coords.join("\n");
    if timed_out {
        body = annotate_timeout(body, true, timeout_secs);
    }
    let raw_lines = body.lines().count();
    let raw_chars = body.chars().count();
    let (out_lines, out_chars) = pick_search_output_limits(call);
    let body2 = truncate_tool_payload(&body, out_lines, out_chars);
    let log = format!(
        "状态:{status} | 耗时:{}ms | op:search | format:coords | engine:{} | root:{} | matches:{} | lines:{} | chars:{}",
        started.elapsed().as_millis(),
        engine,
        shorten_path(root),
        coords.len(),
        raw_lines,
        raw_chars
    );
    Ok(ToolOutcome {
        user_message: body2,
        log_lines: vec![log],
    })
}

fn run_search_ranges(call: &ToolCall, pattern: &str, root: &str) -> anyhow::Result<ToolOutcome> {
    let started = Instant::now();
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
        .or(Some(SEARCH_TIMEOUT_SECS));

    let max_matches = call
        .count
        .unwrap_or(SEARCH_DEFAULT_MAX_MATCHES)
        .clamp(1, SEARCH_MAX_MATCHES_CAP);

    let include_globs = call.include_glob.clone().unwrap_or_default();
    let exclude_globs = call.exclude_glob.clone().unwrap_or_default();
    let extra_exclude_dirs = call.exclude_dirs.clone().unwrap_or_default();
    let include_re = compile_globs(&include_globs);
    let exclude_re = compile_globs(&exclude_globs);

    let mut engine = "rg";
    let mut rg_args: Vec<String> = vec![
        "--line-number".to_string(),
        "--no-heading".to_string(),
        "-S".to_string(),
        "--max-count".to_string(),
        max_matches.to_string(),
        "--max-filesize".to_string(),
        SEARCH_MAX_FILESIZE.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in ["!**/*.db", "!**/*.sqlite", "!**/*.sqlite3"] {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in include_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for g in exclude_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!{g}"));
    }
    rg_args.push("--".to_string());
    rg_args.push(pattern.to_string());
    rg_args.push(root.to_string());

    let mut grep_args: Vec<String> = vec![
        "-RIn".to_string(),
        "-I".to_string(),
        "--binary-files=without-match".to_string(),
        "-m".to_string(),
        max_matches.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push((*d).to_string());
    }
    for g in ["*.db", "*.sqlite", "*.sqlite3"] {
        grep_args.push("--exclude".to_string());
        grep_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push(d.to_string());
    }
    for g in include_globs.iter() {
        grep_args.push(format!("--include={g}"));
    }
    for g in exclude_globs.iter() {
        grep_args.push(format!("--exclude={g}"));
    }
    grep_args.push("--".to_string());
    grep_args.push(pattern.to_string());
    grep_args.push(root.to_string());

    let rg_refs: Vec<&str> = rg_args.iter().map(|s| s.as_str()).collect();
    let grep_refs: Vec<&str> = grep_args.iter().map(|s| s.as_str()).collect();
    let rg_bin = if Path::new(TERMUX_RG).is_file() {
        TERMUX_RG
    } else {
        "rg"
    };
    let mut out = run_command_output_with_optional_timeout(rg_bin, &rg_refs, timeout_secs);
    if out.is_err() {
        engine = "grep";
        out = run_command_output_with_optional_timeout("grep", &grep_refs, timeout_secs);
    }
    let (code, stdout, _stderr, timed_out) =
        out.unwrap_or_else(|_| (127, String::new(), "search failed".to_string(), false));
    let status = status_label(code, timed_out);

    fn real_display_pathbuf(path: &Path) -> String {
        std::fs::canonicalize(path)
            .unwrap_or_else(|_| path.to_path_buf())
            .to_string_lossy()
            .to_string()
    }

    use std::collections::{BTreeMap, BTreeSet};
    let mut by_file: BTreeMap<String, BTreeSet<usize>> = BTreeMap::new();
    let mut total = 0usize;
    for l in stdout.lines() {
        let Some((p, ln, _content)) = parse_search_hit_line(l) else {
            continue;
        };
        let path = resolve_search_hit_path(root, &p);
        if !search_path_allowed(&path, &include_re, &exclude_re, &extra_exclude_dirs) {
            continue;
        }
        let real = real_display_pathbuf(&path);
        let set = by_file.entry(real).or_default();
        if set.insert(ln) {
            total = total.saturating_add(1);
        }
        if total >= max_matches {
            break;
        }
    }

    if by_file.is_empty() {
        return Ok(ToolOutcome {
            user_message: "未找到匹配".to_string(),
            log_lines: vec![format!(
                "状态:{status} | 耗时:{}ms | op:search | format:ranges | engine:{} | root:{} | matches:0",
                started.elapsed().as_millis(),
                engine,
                shorten_path(root),
            )],
        });
    }

    let mut out_lines: Vec<String> = Vec::new();
    let mut range_count = 0usize;
    for (file, lines) in by_file.into_iter() {
        let mut it = lines.into_iter();
        let Some(mut start) = it.next() else { continue };
        let mut prev = start;
        for ln in it {
            if ln == prev + 1 {
                prev = ln;
                continue;
            }
            // flush range
            if start == prev {
                out_lines.push(format!("{file}:L{start}"));
            } else {
                out_lines.push(format!("{file}:L{start}-L{prev}"));
            }
            range_count = range_count.saturating_add(1);
            start = ln;
            prev = ln;
        }
        if start == prev {
            out_lines.push(format!("{file}:L{start}"));
        } else {
            out_lines.push(format!("{file}:L{start}-L{prev}"));
        }
        range_count = range_count.saturating_add(1);
    }

    let mut body = out_lines.join("\n");
    if timed_out {
        body = annotate_timeout(body, true, timeout_secs);
    }
    let raw_lines = body.lines().count();
    let raw_chars = body.chars().count();
    let (out_cap_lines, out_cap_chars) = pick_search_output_limits(call);
    let body2 = truncate_tool_payload(&body, out_cap_lines, out_cap_chars);
    let log = format!(
        "状态:{status} | 耗时:{}ms | op:search | format:ranges | engine:{} | root:{} | matches:{} | ranges:{} | lines:{} | chars:{}",
        started.elapsed().as_millis(),
        engine,
        shorten_path(root),
        total,
        range_count,
        raw_lines,
        raw_chars
    );
    Ok(ToolOutcome {
        user_message: body2,
        log_lines: vec![log],
    })
}

fn resolve_mind_target(call: &ToolCall) -> Option<String> {
    call.target
        .as_deref()
        .and_then(normalize_mind_target_value)
        .or_else(|| normalize_mind_target_value(call.input.trim()))
}

fn resolve_mind_message(call: &ToolCall) -> String {
    if let Some(content) = call
        .content
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        return content.to_string();
    }
    let input = call.input.trim();
    if normalize_mind_target_value(input).is_some() {
        return String::new();
    }
    input.to_string()
}

pub fn extract_tool_calls(text: &str) -> anyhow::Result<(Vec<ToolCall>, String)> {
    if text.trim().is_empty() {
        return Ok((vec![], String::new()));
    }
    // 先走严格模式：只接受“独立成行”的 <tool>...</tool>（最安全）。
    let (mut calls, mut spans) = extract_tool_calls_strict(text);
    if calls.is_empty() && text.contains("<tool>") && text.contains("</tool>") {
        // 宽松兜底：某些供应商/中转会把 `<tool>` 块与其它文本拼在同一行（或尾部追加噪音），
        // 导致严格模式 0 命中，从而误判“工具解析失败”。
        //
        // 注意：仍需要满足 JSON 可解析为合法 ToolCall，且避免在 ``` code fence 内触发。
        let (calls2, spans2) = extract_tool_calls_relaxed(text);
        if !calls2.is_empty() {
            calls = calls2;
            spans = spans2;
        }
    }
    let cleaned = remove_spans(text, &spans);
    Ok((calls, cleaned.trim().to_string()))
}

pub fn strip_any_tool_blocks(text: &str) -> String {
    if text.trim().is_empty() {
        return String::new();
    }
    let (_calls, mut spans) = extract_tool_calls_strict(text);
    if spans.is_empty() && text.contains("<tool>") && text.contains("</tool>") {
        let (_calls2, spans2) = extract_tool_calls_relaxed(text);
        spans = spans2;
    }
    remove_spans(text, &spans).trim().to_string()
}

fn extract_tool_calls_strict(text: &str) -> (Vec<ToolCall>, Vec<(usize, usize)>) {
    let re = tool_tag_re();
    let mut calls = Vec::new();
    let mut spans: Vec<(usize, usize)> = Vec::new();
    for caps in re.captures_iter(text) {
        let Some(block) = caps.get(0) else {
            continue;
        };
        let Some(payload) = caps.get(1).map(|m| m.as_str()) else {
            continue;
        };
        let payload = normalize_tool_payload(payload);
        if let Some(call) = parse_tool_call_payload(&payload) {
            calls.push(call);
            spans.push((block.start(), block.end()));
        }
    }
    (calls, spans)
}

fn extract_tool_calls_relaxed(text: &str) -> (Vec<ToolCall>, Vec<(usize, usize)>) {
    // 仅做线性扫描：提取 `<tool>` 与 `</tool>` 之间的 payload 并尝试解析 JSON。
    // 规则：
    // - 跳过 ``` code fence 内的内容（避免示例/文档被误触发执行）。
    // - 只有 parse 成合法 ToolCall 才算命中。
    let mut calls: Vec<ToolCall> = Vec::new();
    let mut spans: Vec<(usize, usize)> = Vec::new();

    // 预计算 fence 区域：任何位于奇数段 fence 内的 `<tool>` 都跳过。
    let mut fence_starts: Vec<usize> = Vec::new();
    let mut i = 0usize;
    while let Some(pos) = text[i..].find("```") {
        let at = i + pos;
        fence_starts.push(at);
        i = at + 3;
    }
    fn in_code_fence(fences: &[usize], idx: usize) -> bool {
        // fences 记录每个 ``` 的起点；偶数->进入，奇数->退出。
        let mut n = 0usize;
        for &p in fences {
            if p > idx {
                break;
            }
            n += 1;
        }
        n % 2 == 1
    }

    fn line_prefix_allows_relaxed_tool(prefix: &str) -> bool {
        // 允许：
        // - 行首空白
        // - UI/文本里常见的 bullet 前缀（例如 "● " / "○ " / "- "）
        let p = prefix.trim();
        p.is_empty() || matches!(p, "●" | "○" | "-" | "*" | ">")
    }

    let mut cursor = 0usize;
    while let Some(start_rel) = text[cursor..].find("<tool>") {
        let start = cursor + start_rel;
        if in_code_fence(&fence_starts, start) {
            cursor = start + 6;
            continue;
        }
        let line_prefix = text[..start].rsplit('\n').next().unwrap_or("");
        if !line_prefix_allows_relaxed_tool(line_prefix) {
            cursor = start + 6;
            continue;
        }
        let after = start + "<tool>".len();
        let Some(end_rel) = text[after..].find("</tool>") else {
            break;
        };
        let end = after + end_rel + "</tool>".len();
        let payload_raw = &text[after..(after + end_rel)];
        let payload = normalize_tool_payload(payload_raw);
        if let Some(call) = parse_tool_call_payload(&payload) {
            calls.push(call);
            spans.push((start, end));
        }
        cursor = end;
    }
    (calls, spans)
}

fn normalize_tool_payload(payload: &str) -> String {
    let s = payload.trim();
    if !s.starts_with("```") {
        return s.to_string();
    }
    // 兼容模型输出：
    // ```json
    // {"tool":"bash","input":"ls"}
    // ```
    let mut body = s;
    if let Some(pos) = body.find('\n') {
        body = &body[(pos + 1)..];
    } else {
        body = "";
    }
    if let Some(end) = body.rfind("```") {
        body = &body[..end];
    }
    body.trim().to_string()
}

fn remove_spans(text: &str, spans: &[(usize, usize)]) -> String {
    if spans.is_empty() {
        return text.to_string();
    }
    let mut spans = spans.to_vec();
    spans.sort_by_key(|(s, _)| *s);
    let mut out = String::new();
    let mut last = 0usize;
    for (s, e) in spans {
        if s > last {
            out.push_str(&text[last..s]);
        }
        if e > last {
            last = e;
        }
    }
    if last < text.len() {
        out.push_str(&text[last..]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_tool_call_basic() {
        let call =
            parse_tool_call_payload(r#"{"tool":"list","op":"dir","cwd":".","brief":"列目录"}"#).expect("call");
        assert_eq!(call.tool, "list");
        assert_eq!(call.op.as_deref(), Some("dir"));
        assert_eq!(call.cwd.as_deref(), Some("."));
        assert_eq!(call.brief.as_deref(), Some("列目录"));
    }

    #[test]
    fn parse_tool_call_names() {
        let call = parse_tool_call_payload(
            r#"{"tool":"list","op":"files","brief":"stat","cwd":".","name":["a","b","c"]}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "list");
        assert_eq!(call.names.as_ref().map(|v| v.len()), Some(3));

    }

    #[test]
    fn extract_tool_calls_multiple_blocks_and_pids() {
        let text = "<tool>{\"tool\":\"pty\",\"op\":\"kill\",\"pids\":[8,9,10],\"brief\":\"kill batch\"}</tool>\n<tool>{\"tool\":\"bash\",\"input\":\"pwd\",\"brief\":\"cwd\"}</tool>\n[[t:123456]]";
        let (calls, cleaned) = extract_tool_calls(text).expect("extract ok");
        assert_eq!(calls.len(), 2);
        let pty = calls.iter().find(|c| c.tool == "pty").expect("pty call");
        assert_eq!(pty.op.as_deref(), Some("kill"));
        assert!(pty.pids.as_ref().is_some_and(|v| v.contains(&8)));
        assert!(pty.pids.as_ref().is_some_and(|v| v.contains(&10)));
        assert!(cleaned.trim().starts_with("[[t:"));
    }

    #[test]
    fn validate_pty_op_case_insensitive() {
        let mut call = ToolCall::default();
        call.tool = "pty".to_string();
        call.op = Some("RUN".to_string());
        call.input = "echo hi".to_string();
        call.brief = Some("test".to_string());
        assert!(validate_tool_call(&call).is_ok());
    }

    #[test]
    fn validate_pty_kill_requires_job_id() {
        let mut call = ToolCall::default();
        call.tool = "pty".to_string();
        call.op = Some("kill".to_string());
        call.brief = Some("kill".to_string());
        let err = validate_tool_call(&call).unwrap_err();
        assert!(err.user_message.contains("job_id"));
    }

    #[test]
    fn parse_tool_call_pty_commands_array() {
        let call = parse_tool_call_payload(
            r#"{"tool":"pty","op":"run","commands":["echo 1","echo 2"],"brief":"batch"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "pty");
        assert_eq!(call.op.as_deref(), Some("run"));
        assert_eq!(call.commands.as_ref().map(|v| v.len()), Some(2));
        assert_eq!(call.brief.as_deref(), Some("batch"));
    }

    #[test]
    fn parse_tool_call_pty_kill_pid_and_pids() {
        let call =
            parse_tool_call_payload(r#"{"tool":"pty","op":"kill","pid":7,"brief":"kill one"}"#)
                .expect("call");
        assert_eq!(call.tool, "pty");
        assert_eq!(call.op.as_deref(), Some("kill"));
        assert_eq!(call.pid, Some(7));

        let call = parse_tool_call_payload(
            r#"{"tool":"pty","op":"kill","pids":[1,"2",3],"brief":"kill batch"}"#,
        )
        .expect("call");
        assert_eq!(call.pids.as_ref().map(|v| v.len()), Some(3));
        assert!(call.pids.as_ref().is_some_and(|v| v.contains(&2)));

        let call = parse_tool_call_payload(
            r#"{"tool":"pty","op":"kill","job_ids":"4,5 6","brief":"kill jobs"}"#,
        )
        .expect("call");
        assert!(call.job_ids.as_ref().is_some_and(|v| v.contains(&4)));
        assert!(call.job_ids.as_ref().is_some_and(|v| v.contains(&6)));
    }

    #[test]
    fn parse_tool_call_memory_region_and_scan_limit() {
        let call = parse_tool_call_payload(
            r#"{"tool":"search","pattern":"a b","root":"datememo","memory":true,"region":"past","scan_limit":12000,"brief":"搜记忆"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "search");
        assert_eq!(call.memory, Some(true));
        assert_eq!(call.region.as_deref(), Some("past"));
        assert_eq!(call.scan_limit, Some(12000));
    }

    #[test]
    fn parse_tool_call_search_context_and_globs() {
        let call = parse_tool_call_payload(
            r#"{"tool":"search","pattern":"foo bar","root":"src","context":true,"context_lines":25,"include_glob":["*.rs","**/*.toml"],"exclude_dirs":"target,node_modules"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "search");
        assert_eq!(call.pattern.as_deref(), Some("foo bar"));
        assert_eq!(call.root.as_deref(), Some("src"));
        assert_eq!(call.context, Some(true));
        assert_eq!(call.context_lines, Some(25));
        assert!(call.include_glob.as_ref().is_some_and(|v| v.len() == 2));
        assert!(
            call.exclude_dirs
                .as_ref()
                .is_some_and(|v| v.iter().any(|x| x == "target"))
        );
    }

    #[test]
    fn search_regex_meta_disables_keyword_and() {
        assert!(search_pattern_looks_like_regex("^apple"));
        assert!(search_pattern_looks_like_regex("foo|bar"));
        assert!(search_pattern_looks_like_regex("foo\\s+bar"));
        assert!(!search_pattern_looks_like_regex("apple banana"));
    }

    #[test]
    fn search_dsl_fallback_parses_pattern_in_root() {
        let (p, r, f) = parse_search_dsl_fallback("pattern=^apple in ./prompts").expect("dsl");
        assert_eq!(p, "^apple");
        assert_eq!(r, "./prompts");
        assert_eq!(f, Some(false));
        let (p, r, f) = parse_search_dsl_fallback("file=motd.sh in .").expect("dsl");
        assert_eq!(p, "motd.sh");
        assert_eq!(r, ".");
        assert_eq!(f, Some(true));
    }

    #[test]
    fn normalize_aliases() {
        let call = parse_tool_call_payload(r#"{"tool":"ls","input":"."}"#).expect("call");
        assert_eq!(call.tool, "ls");
        assert_eq!(call.input, ".");
    }

    #[test]
    fn truncate_tool_payload_truncates_by_lines() {
        let input = (0..10)
            .map(|i| format!("line{i}"))
            .collect::<Vec<_>>()
            .join("\n");
        let out = truncate_tool_payload(&input, 4, 10_000);
        assert!(out.contains("line0"));
        assert!(out.contains("line1"));
        assert!(out.contains("line8"));
        assert!(out.contains("line9"));
        assert!(out.contains("\n...\n"));
        assert!(!out.contains("line5"));
    }

    #[test]
    fn truncate_tool_payload_truncates_by_chars_with_unicode() {
        let input = "你好世界你好世界你好世界"; // 12 chars
        let out = truncate_tool_payload(input, 10, 6);
        assert!(out.contains("\n...\n"));
        assert!(!out.contains(input));
    }

    #[test]
    fn truncate_command_output_truncates_by_chars() {
        let input = "a".repeat(OUTPUT_MAX_CHARS + 10);
        let out = truncate_command_output(input);
        assert!(out.contains("[输出已截断"));
    }

    #[test]
    fn fastmemo_v1_migrates_to_v3_and_dedupes() {
        let v1 = r#"
fastmemo v1 | max_chars: 1800 | updated: 2026-01-27 20:30:13

[动态成长人格]：萤的人格成长
- 冷静理性
- 冷静理性

[用户感知画像]：对用户的近期感知
- 用户偏好高效
- 用户偏好高效

[淡化池]：将被遗忘的信息
- 临时信息
"#;
        let out = migrate_fastmemo_to_v2(v1);
        assert!(is_fastmemo_v3_struct(&out));
        assert!(!out.contains("淡化池"));
        assert!(out.contains("[自我感知]"));
        assert!(out.contains("[用户感知]"));
        assert!(out.contains("[环境感知]"));
        assert!(out.contains("[事件感知]"));
        // 去重后，“冷静理性”只应保留一次（作为 bullet）
        assert_eq!(out.matches("冷静理性").count(), 1);
    }

    #[test]
    fn ensure_memory_file_repairs_unknown_sections() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("ying-fastmemo-test-{}.jsonl", std::process::id()));
        let raw = r#"
fastmemo v2 | max_chars: 1800 | updated: 2026-02-01 00:00:00

[自我感知]
- a

[用户感知]
- b

[环境感知]
- c

[历史感知]
- d

[动态context池]
- e

[淡化池]
- should_drop
"#;
        std::fs::write(&path, raw).expect("write tmp");
        ensure_memory_file("fastmemo", path.to_string_lossy().as_ref()).expect("repair ok");
        let repaired = std::fs::read_to_string(&path).expect("read tmp");
        let _ = std::fs::remove_file(&path);
        assert!(is_fastmemo_v3_struct(&repaired));
        assert!(!repaired.contains("淡化池"));
        assert!(!repaired.contains("should_drop"));
    }
}

#[allow(dead_code)]
pub fn format_tool_hint(call: &ToolCall) -> String {
    let label = tool_display_label(&call.tool);
    if let Some(brief) = call
        .brief
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        return brief.to_string();
    }
    let preview = describe_tool_input(call, 60);
    if preview.is_empty() {
        label
    } else {
        format!("{label} {preview}")
    }
}

struct ToolMessageTemplates {
    tool_line: &'static str,
    explain_line: &'static str,
    input_line: &'static str,
    output_header: &'static str,
    output_footer: &'static str,
    meta_header: &'static str,
    meta_footer: &'static str,
    no_output: &'static str,
    unknown_tool: &'static str,
    tool_failed: &'static str,
    tool_failed_generic: &'static str,
}

const TOOL_MESSAGES: ToolMessageTemplates = ToolMessageTemplates {
    tool_line: "TOOL: {TOOL}\n",
    explain_line: "EXPLAIN: {BRIEF}\n",
    input_line: "INPUT: {INPUT}\n",
    output_header: "OUTPUT:\n```text\n",
    output_footer: "\n```\n",
    meta_header: "META:\n```text\n",
    meta_footer: "\n```\n",
    no_output: "(NO OUTPUT)",
    unknown_tool: "UNKNOWN TOOL: {TOOL}",
    tool_failed: "TOOL FAILED: {ERR}",
    tool_failed_generic: "TOOL FAILED",
};

fn render_tool_template(template: &str, pairs: &[(&str, &str)]) -> String {
    let mut out = template.to_string();
    for (k, v) in pairs {
        out = out.replace(&format!("{{{k}}}"), v);
    }
    out
}

fn format_tool_message_with_limits(
    call: &ToolCall,
    outcome: &ToolOutcome,
    output_max_lines: usize,
    output_max_chars: usize,
    meta_max_lines: usize,
    meta_max_chars: usize,
) -> String {
    const MODEL_NOTE_PREFIX: &str = "[[AITERMUX_MODEL_NOTE]]";
    let msgs = &TOOL_MESSAGES;
    let mut msg = String::new();
    let label = tool_display_label(&call.tool);
    if !label.is_empty() {
        msg.push_str(&render_tool_template(&msgs.tool_line, &[("TOOL", &label)]));
    }
    let mut input_preview = describe_tool_input(call, 400);
    let log_lines_filtered: Vec<String> = outcome
        .log_lines
        .iter()
        .map(|s| s.trim_end().to_string())
        .filter(|s| !s.trim_start().starts_with(MODEL_NOTE_PREFIX))
        .collect();
    if let Some((add, del, unit)) = extract_delta_from_log(&log_lines_filtered)
        && (add > 0 || del > 0)
    {
        let suffix = if let Some(unit) = unit {
            format!("(+{add} -{del} {unit})")
        } else {
            format!("(+{add} -{del})")
        };
        if !input_preview.is_empty() {
            input_preview.push(' ');
        }
        input_preview.push_str(&suffix);
    }
    if let Some(brief) = call
        .brief
        .as_ref()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
    {
        msg.push_str(&render_tool_template(
            &msgs.explain_line,
            &[("BRIEF", brief)],
        ));
    }
    if !input_preview.is_empty() {
        msg.push_str(&render_tool_template(
            &msgs.input_line,
            &[("INPUT", &input_preview)],
        ));
    }
    msg.push_str(&msgs.output_header);
    let output = if outcome.user_message.trim().is_empty() {
        msgs.no_output.to_string()
    } else {
        truncate_tool_payload(
            outcome.user_message.trim_end(),
            output_max_lines,
            output_max_chars,
        )
    };
    msg.push_str(&output);
    msg.push_str(&msgs.output_footer);
    if !log_lines_filtered.is_empty() {
        msg.push_str(&msgs.meta_header);
        let meta_join = log_lines_filtered.join("\n");
        let meta = truncate_tool_payload(meta_join.trim_end(), meta_max_lines, meta_max_chars);
        msg.push_str(&meta);
        msg.push_str(&msgs.meta_footer);
    }
    msg.trim_end().to_string()
}

pub fn format_tool_message_raw(call: &ToolCall, outcome: &ToolOutcome) -> String {
    let (out_lines, out_chars) = tool_output_limits_for_history(call);
    format_tool_message_with_limits(
        call,
        outcome,
        out_lines,
        out_chars,
        TOOL_META_RAW_MAX_LINES,
        TOOL_META_RAW_MAX_CHARS,
    )
}

pub fn format_tool_message_for_model(call: &ToolCall, outcome: &ToolOutcome) -> String {
    // 模型侧：只注入“工具输出 + 必要 meta”，避免把 UI/可视化用的 TOOL/INPUT/EXPLAIN 结构回灌给模型，
    // 否则会干扰模型对 `<tool>{...}</tool>` 工具调用协议的判断（把回执误学成调用格式）。
    //
    // 约束：
    // - 仍保持输出预算（避免上下文被日志撑爆）
    // - 工具自身会在超阈值时导出到文件并返回 saved/path/lines/bytes 等信息
    let (out_lines, out_chars) = tool_output_limits_for_history(call);
    let output = if outcome.user_message.trim().is_empty() {
        TOOL_MESSAGES.no_output.to_string()
    } else {
        truncate_tool_payload(outcome.user_message.trim_end(), out_lines, out_chars)
    };
    // 允许工具通过 `[[AITERMUX_MODEL_NOTE]]...` 向模型注入“低歧义提示”（不在 UI 中展示）。
    // 为避免模型误学内部前缀，这里把它规范成 meta 的 `note:` 行。
    const MODEL_NOTE_PREFIX: &str = "[[AITERMUX_MODEL_NOTE]]";
    let mut log_lines_for_model: Vec<String> = Vec::new();
    for line in &outcome.log_lines {
        let t = line.trim_end();
        if t.trim_start().starts_with(MODEL_NOTE_PREFIX) {
            let rest = t.trim_start().trim_start_matches(MODEL_NOTE_PREFIX).trim();
            if !rest.is_empty() {
                log_lines_for_model.push(format!("note:{rest}"));
            }
            continue;
        }
        if !t.is_empty() {
            log_lines_for_model.push(t.to_string());
        }
    }
    let mut msg = String::new();
    msg.push_str(output.trim_end());
    if !log_lines_for_model.is_empty() {
        let meta_join = log_lines_for_model.join("\n");
        let meta = truncate_tool_payload(
            meta_join.trim_end(),
            TOOL_META_RAW_MAX_LINES,
            TOOL_META_RAW_MAX_CHARS,
        );
        if !msg.trim_end().is_empty() {
            msg.push_str("\n\n");
        }
        msg.push_str("meta:\n");
        msg.push_str(meta.trim_end());
    }
    msg.trim_end().to_string()
}

fn extract_delta_from_log(lines: &[String]) -> Option<(usize, usize, Option<&'static str>)> {
    for line in lines {
        let clean = line.replace('|', " ");
        let mut add = None;
        let mut del = None;
        let mut expect_delta_tokens = false;
        let mut unit: Option<&'static str> = None;
        for token in clean.split_whitespace() {
            let mut tok = token;
            for (prefix, u) in [
                ("delta_lines:", Some("lines")),
                ("delta_chars:", Some("chars")),
                ("delta:", None),
            ] {
                if let Some(rest) = tok.strip_prefix(prefix) {
                    if let Some(u) = u {
                        unit = Some(u);
                    }
                    if rest.is_empty() {
                        expect_delta_tokens = true;
                        tok = "";
                    } else {
                        tok = rest;
                        expect_delta_tokens = true;
                    }
                    break;
                }
            }
            if !expect_delta_tokens && !tok.starts_with('+') && !tok.starts_with('-') {
                continue;
            }
            if let Some(rest) = tok.strip_prefix('+')
                && let Ok(v) = rest.parse::<usize>()
            {
                add = Some(v);
            } else if let Some(rest) = tok.strip_prefix('-')
                && let Ok(v) = rest.parse::<usize>()
            {
                del = Some(v);
            }
        }
        if add.is_some() || del.is_some() {
            return Some((add.unwrap_or(0), del.unwrap_or(0), unit));
        }
    }
    None
}

pub fn validate_termux_api(input: &str) -> anyhow::Result<()> {
    let s = input.trim();
    if s.is_empty() {
        return Err(anyhow!("termux_api 需要 input"));
    }
    Ok(())
}

fn tool_usage(tool: &str) -> &'static str {
    match tool {
        "bash" => r#"{"tool":"bash","input":"ls","brief":"查看目录"}"#,
        "adb" => {
            r#"{"tool":"adb","input":"shell getprop ro.product.model","brief":"查询设备型号"}"#
        }
        "termux_api" => {
            r#"{"tool":"termux_api","input":"termux-battery-status","brief":"读取电池状态"}"#
        }
        "apply_patch" => {
            r#"{"tool":"apply_patch","brief":"用 unified diff 修改文件","input":"--- a/src/main.rs\n+++ b/src/main.rs\n@@\n-old\n+new\n"}"#
        }
        "mind_msg" => {
            r#"{"tool":"mind_msg","target":"dog","content":"需要你协助检查工具结果。","brief":"同步需求"}"#
        }
        "system_config" => {
            r#"{"tool":"system_config","heartbeat_minutes":10,"brief":"调整心跳间隔"}"#
        }
        "skills_mcp" => r#"{"tool":"skills_mcp","category":"编程类","brief":"获取编程类工具说明"}"#,
        "list" => r#"{"tool":"list","op":"dir","cwd":".","depth":1,"brief":"列目录"}
{"tool":"list","op":"files","cwd":".","name":["main.rs"],"brief":"查看文件信息"}"#,
        "pty" => {
            r#"{"tool":"pty","op":"run","commands":["long_task.sh","tail -f log.txt"],"brief":"后台终端任务（默认保持会话）"}"#
        }
        _ => r#"{"tool":"<tool>","input":"...","brief":"一句话说明"}"#,
    }
}

pub(crate) fn tool_format_error(tool: &str, reason: &str) -> ToolOutcome {
    let usage = tool_usage(tool);
    ToolOutcome {
        // 只把“简短原因”展示给用户；具体用法放进 meta 供模型自愈，避免 UI 被大段 JSON 占满。
        user_message: format!("格式错误：{reason}"),
        log_lines: vec!["状态:fail".to_string(), format!("usage:{usage}")],
    }
}

fn ensure_outcome_status(outcome: &mut ToolOutcome) {
    if outcome
        .log_lines
        .iter()
        .any(|l| l.trim_start().starts_with("状态:"))
    {
        return;
    }
    let msg = outcome.user_message.trim();
    // 约定：只有明确 `状态:fail` 才算“失败”。此处只做“兜底补状态”，避免误判把“情况汇报”
    //（如 ok_not_found/ok_timeout）标成失败。
    //
    // 若未来要更精确，请让各工具显式写入 `状态:*` 行，而不是依赖该兜底逻辑。
    let fail = msg.contains("格式错误") || msg.contains("未知工具") || msg.contains("工具执行失败");
    if fail {
        outcome.log_lines.insert(0, "状态:fail".to_string());
    } else {
        outcome.log_lines.insert(0, "状态:0".to_string());
    }
}

fn validate_tool_call(call: &ToolCall) -> Result<(), ToolOutcome> {
    let tool = call.tool.as_str();
    // 大多数工具必须提供 brief（审计/可视化用途）。
    if call.brief.as_deref().unwrap_or("").trim().is_empty() {
        return Err(tool_format_error(tool, "缺少 brief"));
    }
    match tool {
        "bash" | "adb" | "termux_api" => {
            if call.input.trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 input"));
            }
        }
        "apply_patch" => {
            let patch_text = call
                .patch
                .as_deref()
                .unwrap_or(call.content.as_deref().unwrap_or(call.input.as_str()))
                .trim();
            if patch_text.is_empty() {
                return Err(tool_format_error(tool, "缺少 input（unified diff 文本）"));
            }
            if patch_text.contains("*** Begin Patch")
                || patch_text.contains("*** Update File:")
                || patch_text.contains("*** Add File:")
                || patch_text.contains("*** Delete File:")
            {
                return Err(tool_format_error(
                    tool,
                    "仅支持 unified diff（含 ---/+++ 与 @@），不要使用 *** Begin Patch 格式",
                ));
            }
        }
        "search" => {
            let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
            if pattern.is_empty() {
                return Err(tool_format_error(tool, "缺少 pattern"));
            }
        }
        "mind_msg" => {
            if resolve_mind_target(call).is_none() {
                return Err(tool_format_error(tool, "缺少 target"));
            }
            if resolve_mind_message(call).trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 content"));
            }
        }
        "memory_check" => {
            let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
            if pattern.is_empty() {
                return Err(tool_format_error(tool, "缺少 pattern"));
            }
        }
        "memory_read" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            if resolve_memory_path_label(path).is_none() {
                return Err(tool_format_error(tool, "缺少有效 path"));
            }
        }
        "memory_edit" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            let Some((label, _)) = resolve_memory_path_label(path) else {
                return Err(tool_format_error(tool, "缺少有效 path"));
            };
            if label != "fastmemo" {
                return Err(tool_format_error(tool, "仅支持 fastmemo"));
            }
            let section = call.section.as_deref().unwrap_or("").trim();
            let content = call.content.as_deref().unwrap_or("").trim();
            let find = call.find.as_deref().unwrap_or("").trim();
            if !(!section.is_empty() && !content.is_empty()) && find.is_empty() {
                return Err(tool_format_error(tool, "缺少 find（或 section+content）"));
            }
        }
        "memory_add" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            let Some((label, _)) = resolve_memory_path_label(path) else {
                return Err(tool_format_error(tool, "缺少有效 path"));
            };
            if label != "fastmemo" {
                return Err(tool_format_error(tool, "仅支持 fastmemo"));
            }
            let content = call.content.as_deref().unwrap_or("").trim();
            if content.is_empty() {
                return Err(tool_format_error(tool, "缺少 content"));
            }
            let section = call.section.as_deref().unwrap_or("").trim();
            if section.is_empty() {
                return Err(tool_format_error(tool, "fastmemo 需要 section"));
            }
        }
        "system_config" => {
            let Some(v) = parse_heartbeat_minutes(call) else {
                return Err(tool_format_error(tool, "缺少 heartbeat_minutes"));
            };
            if !is_valid_heartbeat_minutes(v) {
                return Err(tool_format_error(tool, "心跳仅支持 5/10/30/60 分钟"));
            }
        }
        "skills_mcp" => {
            let category = call.category.as_deref().unwrap_or(call.input.trim()).trim();
            if category.is_empty() {
                return Err(tool_format_error(tool, "缺少 category"));
            }
        }
        "pty" => {
            let op_raw = call.op.as_deref().unwrap_or("").trim();
            if op_raw.is_empty() {
                return Err(tool_format_error(tool, "缺少 op"));
            }
            let op = op_raw.to_ascii_lowercase();
            match op.as_str() {
                "list" => {}
                "kill" => {
                    let has_single = call.job_id.is_some() || call.pid.is_some();
                    let has_batch = call
                        .job_ids
                        .as_ref()
                        .is_some_and(|v| v.iter().any(|id| *id > 0));
                    let has_batch_pid = call
                        .pids
                        .as_ref()
                        .is_some_and(|v| v.iter().any(|id| *id > 0));
                    if !has_single && !has_batch && !has_batch_pid {
                        return Err(tool_format_error(
                            tool,
                            "pty.kill 缺少 pid/pids（或 job_id/job_ids）",
                        ));
                    }
                }
                "run" => {
                    let has_cmds = call.commands.as_ref().is_some_and(|v| !v.is_empty());
                    if !has_cmds && call.input.trim().is_empty() {
                        return Err(tool_format_error(tool, "pty.run 缺少 commands/input"));
                    }
                }
                _ => {
                    return Err(tool_format_error(tool, "op 仅支持 run/list/kill"));
                }
            }
        }
        "list" => {
            // list 不支持 paths（避免多路径心智负担）；path 为空时默认 "."（执行阶段兜底）。
            if call
                .paths
                .as_ref()
                .is_some_and(|v| v.iter().any(|p| !p.trim().is_empty()))
            {
                return Err(tool_format_error(tool, "list 不支持 paths，请用 cwd 指定目录"));
            }
            let op = call.op.as_deref().unwrap_or("").trim().to_ascii_lowercase();
            if op.is_empty() {
                return Err(tool_format_error(tool, "缺少 op"));
            }
            if !op.is_empty() && op != "dir" && op != "files" {
                return Err(tool_format_error(tool, "list.op 仅支持 dir/files"));
            }
            // 收敛协议：list 只基于 cwd 工作；不要使用 path/input 指向其它目录。
            if call.path.as_deref().unwrap_or("").trim().len() > 0 || !call.input.trim().is_empty() {
                return Err(tool_format_error(tool, "list 不支持 path/input；请用 cwd 指定目录"));
            }
            let has_names = call
                .names
                .as_ref()
                .is_some_and(|v| v.iter().any(|s| !s.trim().is_empty()));
            if op == "files" && !has_names {
                return Err(tool_format_error(tool, "list.op=files 需要 name"));
            }
            if op == "dir" && has_names {
                return Err(tool_format_error(tool, "list.op=dir 不支持 name"));
            }
        }
        _ => {
            return Err(tool_format_error(
                tool,
                "未知工具（可用：bash/adb/termux_api/apply_patch/search/pty/list/memory_check/memory_read/memory_edit/memory_add/system_config/skills_mcp）",
            ));
        }
    }
    Ok(())
}

fn requires_confirmation_reason(input: &str) -> Option<String> {
    let s = input.trim();
    if s.is_empty() {
        return None;
    }
    let lower = s.to_ascii_lowercase();

    // 1) 典型破坏性/高风险命令：宁可多确认，也不要误伤。
    fn danger_cmd_re() -> &'static Regex {
        static RE: OnceLock<Regex> = OnceLock::new();
        RE.get_or_init(|| {
            Regex::new(
                r"(?i)(^|[\s;|&()])(?:(rm)|(dd)|(wipefs)|(fdisk)|(parted)|(sgdisk)|(mkfs(?:\.[a-z0-9_-]+)?)|(shred))($|\s)",
            )
            .expect("danger cmd regex")
        })
    }
    if let Some(caps) = danger_cmd_re().captures(&lower) {
        let reason = if caps.get(1).is_some() {
            "RM 删除"
        } else if caps.get(2).is_some() {
            "DD 原始写盘"
        } else if caps.get(3).is_some() {
            "WIPEFS 清空签名"
        } else if caps.get(4).is_some() {
            "FDISK 分区修改"
        } else if caps.get(5).is_some() {
            "PARTED 分区修改"
        } else if caps.get(6).is_some() {
            "SGDISK 分区修改"
        } else if caps.get(7).is_some() {
            "MKFS 格式化"
        } else if caps.get(8).is_some() {
            "SHRED 覆写销毁"
        } else {
            "高风险命令"
        };
        return Some(reason.to_string());
    }

    // 2) 触碰系统敏感路径：这里宁可保守确认，避免误伤 Android 系统分区/伪文件系统
    let sensitive_paths = [
        "/system/",
        "/vendor/",
        "/product/",
        "/odm/",
        "/proc/",
        "/sys/",
        "/dev/",
    ];
    let hit_path = sensitive_paths
        .iter()
        .find(|p| lower.contains(**p))
        .copied();
    if let Some(path) = hit_path {
        // 读操作不确认；只在明显“写/改”的情况下确认。
        let write_markers = [
            ">",
            ">>",
            "tee ",
            "chmod ",
            "chown ",
            "mv ",
            "cp ",
            "dd ",
            "truncate ",
            "mount ",
            "umount ",
            "ln ",
            "mkdir ",
            "rmdir ",
            "touch ",
            "sed -i",
        ];
        if let Some(marker) = write_markers.iter().find(|m| lower.contains(**m)).copied() {
            return Some(format!("系统敏感路径写操作（path:{path} marker:{marker}）"));
        }
    }
    None
}

pub fn tool_requires_confirmation(call: &ToolCall) -> Option<String> {
    match call.tool.as_str() {
        "bash" | "adb" | "termux_api" => {
            let input = call.input.trim();
            requires_confirmation_reason(input)
        }
        _ => None,
    }
}

pub fn handle_tool_call(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    if let Err(outcome) = validate_tool_call(call) {
        let mut outcome = outcome;
        ensure_outcome_status(&mut outcome);
        return Ok(outcome);
    }
    match call.tool.as_str() {
        "bash" => run_bash(call),
        "adb" => run_adb(call),
        "termux_api" => run_termux_api(call),
        "apply_patch" => run_apply_patch(call),
        "search" => run_search(call),
        "pty" => Ok(ToolOutcome {
            user_message: "pty 工具由 TUI 主线程处理（不在此处执行）。".to_string(),
            log_lines: vec!["状态:fail".to_string()],
        }),
        "list" => run_list_tool(call),
        "memory_check" => run_memory_check(call),
        "memory_read" => run_memory_read(call),
        "memory_edit" => run_memory_edit(call),
        "memory_add" => run_memory_add(call),
        "mind_msg" => run_mind_msg(call),
        "system_config" => run_system_config(call),
        "skills_mcp" => run_skills_mcp(call),
        other => {
            let msgs = &TOOL_MESSAGES;
            Ok(ToolOutcome {
                user_message: render_tool_template(&msgs.unknown_tool, &[("TOOL", other)]),
                log_lines: vec!["状态:fail".to_string()],
            })
        }
    }
}

fn outcome_is_timeout(outcome: &ToolOutcome) -> bool {
    outcome
        .log_lines
        .iter()
        .any(|l| l.contains("状态:timeout") || l.contains("状态:ok_timeout") || l.contains("超时"))
}

pub fn handle_tool_call_with_retry(call: &ToolCall, retries: usize) -> ToolOutcome {
    fn timeout_retry_safe(tool: &str) -> bool {
        matches!(tool, "memory_check" | "memory_read" | "skills_mcp")
    }
    let mut last = None;
    for _ in 0..retries.max(1) {
        match handle_tool_call(call) {
            Ok(outcome) => {
                let mut outcome = outcome;
                ensure_outcome_status(&mut outcome);
                if outcome_is_timeout(&outcome) {
                    last = Some(outcome);
                    // 超时不一定是“错误”，更不应对有副作用的工具进行自动重试。
                    let safe = timeout_retry_safe(call.tool.as_str());
                    if !safe {
                        break;
                    }
                    continue;
                }
                return outcome;
            }
            Err(e) => {
                last = Some(ToolOutcome {
                    user_message: {
                        let err = format!("{e:#}");
                        let msgs = &TOOL_MESSAGES;
                        render_tool_template(&msgs.tool_failed, &[("ERR", &err)])
                    },
                    log_lines: vec!["状态:fail".to_string()],
                });
            }
        }
    }
    let mut out = last.unwrap_or_else(|| ToolOutcome {
        user_message: TOOL_MESSAGES.tool_failed_generic.to_string(),
        log_lines: vec!["状态:fail".to_string()],
    });
    ensure_outcome_status(&mut out);
    out
}

#[derive(Debug, Clone)]
pub struct ToolOutcome {
    pub user_message: String,
    pub log_lines: Vec<String>,
}

fn timeout_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("timeout")
            .arg("--help")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok()
    })
}

fn build_command(program: &str, args: &[&str]) -> (Command, bool) {
    if timeout_available() {
        let mut cmd = Command::new("timeout");
        cmd.arg("-k")
            .arg(format!("{TOOL_TIMEOUT_KILL_SECS}s"))
            .arg(format!("{TOOL_TIMEOUT_SECS}s"))
            .arg(program)
            .args(args);
        (cmd, true)
    } else {
        let mut cmd = Command::new(program);
        cmd.args(args);
        (cmd, false)
    }
}

fn build_command_with_timeout(program: &str, args: &[&str], timeout_secs: u64) -> (Command, bool) {
    if timeout_available() {
        let mut cmd = Command::new("timeout");
        cmd.arg("-k")
            .arg(format!("{TOOL_TIMEOUT_KILL_SECS}s"))
            .arg(format!("{timeout_secs}s"))
            .arg(program)
            .args(args);
        (cmd, true)
    } else {
        let mut cmd = Command::new(program);
        cmd.args(args);
        (cmd, false)
    }
}

fn build_command_with_optional_timeout(
    program: &str,
    args: &[&str],
    timeout_secs: Option<u64>,
) -> (Command, bool) {
    match timeout_secs {
        Some(0) => {
            let mut cmd = Command::new(program);
            cmd.args(args);
            (cmd, false)
        }
        Some(secs) => build_command_with_timeout(program, args, secs.max(1)),
        None => build_command(program, args),
    }
}

fn status_code(code: Option<i32>) -> i32 {
    code.unwrap_or(-1)
}

fn is_timeout_status(code: i32) -> bool {
    code == 124 || code == 137
}

fn current_dir_display() -> String {
    std::env::current_dir()
        .ok()
        .and_then(|p| p.to_str().map(short_cwd_display))
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "(unknown)".to_owned())
}

pub(crate) fn default_workdir() -> String {
    // 工具默认在 AItermux 工程目录执行，避免用户从其它目录启动时“相对路径”失效。
    //
    // 优先级：
    // 1) AITERMUX_WORKDIR（允许用户显式覆盖）
    // 2) $HOME/AItermux/system（若存在）
    // 3) 当前进程 cwd
    if let Ok(v) = std::env::var("AITERMUX_WORKDIR") {
        let t = v.trim();
        if !t.is_empty() {
            return expand_cwd_alias(t);
        }
    }
    let home = std::env::var("HOME").unwrap_or_default();
    let home = home.trim_end_matches('/').to_string();
    if !home.is_empty() {
        let p = format!("{home}/AItermux/system");
        if std::fs::metadata(&p).is_ok_and(|m| m.is_dir()) {
            return p;
        }
    }
    std::env::current_dir()
        .ok()
        .and_then(|p| p.to_str().map(|s| s.to_string()))
        .unwrap_or_else(|| ".".to_string())
}

fn expand_cwd_alias(raw: &str) -> String {
    // cwd 支持最少必要的别名，避免模型混淆：
    // - . / ./... : 工程根目录 ~/AItermux
    // - ~ / ~/... : 工程根目录 ~/AItermux
    // - home / home/... : 工程根目录 ~/AItermux
    let s = raw.trim();
    if s.is_empty() {
        return String::new();
    }
    let home = std::env::var("HOME").unwrap_or_default();
    let home = home.trim_end_matches('/').to_string();
    if home.is_empty() {
        return s.to_string();
    }
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
    if s == "home" {
        return project;
    }
    if let Some(rest) = s.strip_prefix("home/") {
        return format!("{project}/{rest}");
    }
    s.to_string()
}

fn status_label(code: i32, timed_out: bool) -> String {
    if timed_out {
        "timeout".to_string()
    } else if code == 0 {
        "0".to_string()
    } else {
        code.to_string()
    }
}

#[derive(Clone, Copy, Debug)]
enum ShellSaveMode {
    Auto,
    Always,
    Never,
}

fn parse_shell_save_mode(raw: Option<&str>) -> ShellSaveMode {
    let t = raw.unwrap_or("auto").trim().to_ascii_lowercase();
    match t.as_str() {
        "always" | "save" | "export" | "1" | "true" | "on" => ShellSaveMode::Always,
        "never" | "nosave" | "no_save" | "0" | "false" | "off" => ShellSaveMode::Never,
        _ => ShellSaveMode::Auto,
    }
}

fn exit_is_ok(code: i32, ok_exit_codes: Option<&[i32]>) -> bool {
    if code == 0 {
        return true;
    }
    ok_exit_codes.unwrap_or(&[]).iter().any(|x| *x == code)
}

pub(crate) fn format_bytes(bytes: u64) -> String {
    const UNITS: [&str; 5] = ["B", "KiB", "MiB", "GiB", "TiB"];
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

fn annotate_timeout(body: String, timed_out: bool, timeout_hint_secs: Option<u64>) -> String {
    if !timed_out {
        return body;
    }
    let secs = timeout_hint_secs.unwrap_or(TOOL_TIMEOUT_SECS);
    if body.trim().is_empty() {
        format!("Timed out and terminated ({secs}s).")
    } else {
        format!("【Timed out】\n{}\n", body.trim_end())
    }
}

fn annotate_nonzero_exit(body: String, timed_out: bool, code: i32) -> String {
    if timed_out || code == 0 {
        return body;
    }
    if body.trim().is_empty() {
        return format!("执行失败（exit:{code}）。");
    }
    body
}

fn run_bash(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let mut cwd_display = current_dir_display();
    let cmd = call.input.trim();
    if cmd.is_empty() {
        return Ok(ToolOutcome {
            user_message: "No bash input.".to_string(),
            log_lines: vec![format!("状态:无 | 耗时:0ms | cwd:{cwd_display}")],
        });
    }

    let started = Instant::now();
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000));
    let timeout_hint = timeout_secs.or(Some(TOOL_TIMEOUT_SECS));
    let (mut command, timeout_used) =
        build_command_with_optional_timeout(BASH_SHELL, &["-lc", cmd], timeout_secs);
    let out = command
        .env("TERM", "xterm-256color")
        .env("COLORTERM", "truecolor")
        .env("LANG", "C.UTF-8")
        .env("LC_ALL", "C.UTF-8")
        .env("TERM_PROGRAM", "AItermux")
        .current_dir({
            if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
                let cwd = expand_cwd_alias(cwd);
                cwd_display = short_cwd_display(&cwd);
                PathBuf::from(cwd)
            } else {
                let cwd = default_workdir();
                cwd_display = short_cwd_display(&cwd);
                PathBuf::from(cwd)
            }
        })
        .output()
        .with_context(|| format!("bash 执行失败：{cmd}"))?;
    let elapsed = started.elapsed();
    Ok(build_shell_outcome_bash(
        out,
        elapsed,
        timeout_used,
        &cwd_display,
        cmd,
        timeout_hint,
        parse_shell_save_mode(call.save.as_deref()),
        call.ok_exit_codes.as_deref(),
    ))
}

pub(crate) fn try_write_shell_cache_impl(
    dir: &str,
    path: &str,
    stdout: &[u8],
    stderr: &[u8],
) -> bool {
    let _ = fs::create_dir_all(dir);
    let mut file = match fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
    {
        Ok(f) => f,
        Err(_) => return false,
    };
    if !stdout.is_empty() {
        let _ = file.write_all(b"[stdout]\n");
        let _ = file.write_all(stdout);
        if !stdout.ends_with(b"\n") {
            let _ = file.write_all(b"\n");
        }
    }
    if !stderr.is_empty() {
        let _ = file.write_all(b"[stderr]\n");
        let _ = file.write_all(stderr);
        if !stderr.ends_with(b"\n") {
            let _ = file.write_all(b"\n");
        }
    }
    let _ = file.flush();
    true
}

fn try_write_shell_cache(path: &str, stdout: &[u8], stderr: &[u8]) -> bool {
    try_write_shell_cache_impl(SHELL_CACHE_DIR, path, stdout, stderr)
}

fn build_shell_outcome_bash(
    out: std::process::Output,
    elapsed: std::time::Duration,
    timeout_used: bool,
    cwd_display: &str,
    cmd: &str,
    timeout_hint_secs: Option<u64>,
    save_mode: ShellSaveMode,
    ok_exit_codes: Option<&[i32]>,
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let combined_lines = combined.lines().count();
    let truncated_by_lines = combined_lines > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let mut need_save =
        total_bytes > EXPORT_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    match save_mode {
        ShellSaveMode::Always => need_save = true,
        ShellSaveMode::Never => need_save = false,
        ShellSaveMode::Auto => {}
    }

    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SHELL_CACHE_DIR}/bash_{ts}_{pid}.log");
        if try_write_shell_cache(&path, &out.stdout, &out.stderr) {
            Some(exported_meta(path, total_bytes, combined_lines))
        } else {
            None
        }
    } else {
        None
    };

    let mut body = if let Some(meta) = saved_meta.as_ref() {
        let preview = truncate_export_preview(&combined);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_command_output(combined)
    };
    body = annotate_timeout(body, timed_out, timeout_hint_secs);
    body = annotate_nonzero_exit(body, timed_out, code);
    // 提示：bash 工具每次都是独立 shell，不支持 job control（%1/%2...）。
    if code != 0 {
        let cmd_trim = cmd.trim();
        let looks_like_job_kill = cmd_trim.starts_with("kill %") || cmd_trim.contains(" kill %");
        if looks_like_job_kill && stderr.to_ascii_lowercase().contains("no such job") {
            body.push_str("\n\n提示：bash 工具每次在独立的非交互 shell 中执行，`kill %1` 这类 job control 不生效。\n如需终止正在运行的 Terminal（PTY），请用 `pty.kill(job_id)`。");
        }
    }
    let mut status = if timed_out {
        "timeout".to_string()
    } else if exit_is_ok(code, ok_exit_codes) {
        "0".to_string()
    } else {
        code.to_string()
    };
    if !timed_out && code != 0 {
        let cmd_trim = cmd.trim();
        let stderr_lower = stderr.to_ascii_lowercase();
        let looks_like_kill = cmd_trim == "kill"
            || cmd_trim.starts_with("kill ")
            || cmd_trim.contains("; kill ")
            || cmd_trim.contains("&& kill ")
            || cmd_trim.contains("| kill ");
        let not_found = stderr_lower.contains("no such job")
            || stderr_lower.contains("no such process")
            || stderr_lower.contains("not found");
        if looks_like_kill && not_found {
            // 这类情况通常是“目标不存在/已结束”，对用户而言是可预期状态。
            status = "ok_not_found".to_string();
        }
    }
    let ok = if timed_out {
        "timeout"
    } else if exit_is_ok(code, ok_exit_codes) {
        "true"
    } else {
        "false"
    };
    let log_lines = vec![format!(
        "状态:{status} | ok:{ok} | exit:{code} | elapsed_ms:{} | cwd:{cwd_display} | cmd_chars:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else {
            body
        },
        log_lines,
    }
}

fn build_shell_outcome_adb(
    out: std::process::Output,
    elapsed: std::time::Duration,
    timeout_used: bool,
    cwd_display: &str,
    cmd: &str,
    timeout_hint_secs: Option<u64>,
    save_mode: ShellSaveMode,
    ok_exit_codes: Option<&[i32]>,
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let combined_lines = combined.lines().count();
    let truncated_by_lines = combined_lines > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let mut need_save =
        total_bytes > EXPORT_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    match save_mode {
        ShellSaveMode::Always => need_save = true,
        ShellSaveMode::Never => need_save = false,
        ShellSaveMode::Auto => {}
    }

    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{ADB_CACHE_DIR}/adb_{ts}_{pid}.log");
        if try_write_shell_cache_impl(ADB_CACHE_DIR, &path, &out.stdout, &out.stderr) {
            Some(exported_meta(path, total_bytes, combined_lines))
        } else {
            None
        }
    } else {
        None
    };

    let body = if let Some(meta) = saved_meta.as_ref() {
        let preview = truncate_export_preview(&combined);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_command_output(combined)
    };
    let body = annotate_timeout(body, timed_out, timeout_hint_secs);
    let body = annotate_nonzero_exit(body, timed_out, code);
    let status = if timed_out {
        "timeout".to_string()
    } else if exit_is_ok(code, ok_exit_codes) {
        "0".to_string()
    } else {
        code.to_string()
    };
    let ok = if timed_out {
        "timeout"
    } else if exit_is_ok(code, ok_exit_codes) {
        "true"
    } else {
        "false"
    };
    let log_lines = vec![format!(
        "状态:{status} | ok:{ok} | exit:{code} | elapsed_ms:{} | cwd:{cwd_display} | cmd_chars:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else {
            body
        },
        log_lines,
    }
}

fn ensure_adb_connected() -> bool {
    if is_adb_ready() {
        return true;
    }
    // 尽量自愈：adb server 未启动时先拉起。
    let _ = adb_quick(&["start-server"], 6);
    // 尝试直连（可能已处于 tcp 模式，仅断线）。
    let _ = adb_quick(&["connect", ADB_SERIAL], 8);
    if is_adb_ready() {
        return true;
    }
    // 尝试脚本化打开 tcp 并重启 adbd（需要 root）。
    let _ = try_prepare_adb_tcp();
    let _ = adb_quick(&["connect", ADB_SERIAL], 10);
    is_adb_ready()
}

fn adb_quick(args: &[&str], timeout_secs: u64) -> std::io::Result<std::process::Output> {
    let (mut cmd, _) = build_command_with_timeout("adb", args, timeout_secs.max(1));
    cmd.output()
}

fn is_adb_ready() -> bool {
    let out = Command::new("adb")
        .args(["-s", ADB_SERIAL, "get-state"])
        .output();
    let Ok(out) = out else { return false };
    if !out.status.success() {
        return false;
    }
    let state = String::from_utf8_lossy(&out.stdout);
    state.trim() == "device"
}

fn run_adb(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let mut cwd_display = current_dir_display();
    let input = call.input.trim();
    if input.is_empty() {
        return Ok(ToolOutcome {
            user_message: "未提供 ADB 命令参数。".to_string(),
            log_lines: vec![format!("状态:无 | 耗时:0ms | cwd:{cwd_display}")],
        });
    }

    let started = Instant::now();
    let first = input.split_whitespace().next().unwrap_or("");
    let raw_mode = first.starts_with('-');
    let global_cmd = raw_mode
        || matches!(
            first,
            "devices" | "connect" | "disconnect" | "start-server" | "kill-server" | "version"
        );
    let skip_autoconnect = raw_mode
        || matches!(
            first,
            "connect" | "disconnect" | "start-server" | "kill-server" | "version"
        );

    // 第一次使用 adb 时尽量自动拉起 tcp 连接；只有在“确实需要设备”的命令且自愈失败时才报错返回。
    // 对 devices 这类全局命令：尝试自愈，但不因失败而提前返回，避免干扰用户自助排查。
    let auto_connected = if skip_autoconnect {
        false
    } else {
        ensure_adb_connected()
    };

    // 兼容 deepseek-cli：input 不含 adb 前缀。
    let cmd = if global_cmd {
        if first == "devices" && !auto_connected {
            // ignore: 允许直接展示 adb devices 自身输出
        }
        format!("adb {input}")
    } else {
        if !auto_connected {
            return Ok(ToolOutcome {
                user_message: format!(
                    "自动连接失败，无法连接 ADB 设备 {ADB_SERIAL}。\n可手动建立连接：\n1) su -c 'setprop service.adb.tcp.port 5555; setprop persist.adb.tcp.port 5555; setprop ctl.restart adbd'\n2) adb connect {ADB_SERIAL}\n完成后重试。"
                ),
                log_lines: vec![format!(
                    "ok:false | result:adb_offline | exit:0 | elapsed_ms:{} | 状态:adb_offline | cwd:{cwd_display}",
                    started.elapsed().as_millis()
                )],
            });
        }
        format!("adb -s {ADB_SERIAL} {input}")
    };
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000));
    let timeout_hint = timeout_secs.or(Some(TOOL_TIMEOUT_SECS));
    let (mut command, timeout_used) =
        build_command_with_optional_timeout(BASH_SHELL, &["-lc", &cmd], timeout_secs);
    command
        .env("TERM", "xterm-256color")
        .env("COLORTERM", "truecolor")
        .env("LANG", "C.UTF-8")
        .env("LC_ALL", "C.UTF-8")
        .env("TERM_PROGRAM", "AItermux")
        .current_dir({
            if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
                let cwd = expand_cwd_alias(cwd);
                cwd_display = short_cwd_display(&cwd);
                PathBuf::from(cwd)
            } else {
                let cwd = default_workdir();
                cwd_display = short_cwd_display(&cwd);
                PathBuf::from(cwd)
            }
        });
    let out = command
        .output()
        .with_context(|| format!("adb 执行失败：{cmd}"))?;
    let elapsed = started.elapsed();
    Ok(build_shell_outcome_adb(
        out,
        elapsed,
        timeout_used,
        &cwd_display,
        input,
        timeout_hint,
        parse_shell_save_mode(call.save.as_deref()),
        call.ok_exit_codes.as_deref(),
    ))
}

fn try_prepare_adb_tcp() -> bool {
    // 兼容不同 ROM：service/persist 两种 key 都尝试，最后 restart adbd。
    let cmd = "su -c 'setprop service.adb.tcp.port 5555; setprop persist.adb.tcp.port 5555; setprop ctl.restart adbd'";
    let (mut command, _) = build_command(BASH_SHELL, &["-lc", cmd]);
    command
        .output()
        .map(|out| out.status.success())
        .unwrap_or(false)
}

fn build_shell_outcome_termux_api(
    out: std::process::Output,
    elapsed: std::time::Duration,
    timeout_used: bool,
    cwd_display: &str,
    cmd: &str,
    timeout_hint_secs: Option<u64>,
    save_mode: ShellSaveMode,
    ok_exit_codes: Option<&[i32]>,
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let combined_lines = combined.lines().count();
    let truncated_by_lines = combined_lines > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let mut need_save =
        total_bytes > EXPORT_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    match save_mode {
        ShellSaveMode::Always => need_save = true,
        ShellSaveMode::Never => need_save = false,
        ShellSaveMode::Auto => {}
    }

    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{TERMUX_API_CACHE_DIR}/termux_api_{ts}_{pid}.log");
        if try_write_shell_cache_impl(TERMUX_API_CACHE_DIR, &path, &out.stdout, &out.stderr) {
            Some(exported_meta(path, total_bytes, combined_lines))
        } else {
            None
        }
    } else {
        None
    };

    let body = if let Some(meta) = saved_meta.as_ref() {
        let preview = truncate_export_preview(&combined);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_command_output(combined)
    };
    let body = annotate_timeout(body, timed_out, timeout_hint_secs);
    let body = annotate_nonzero_exit(body, timed_out, code);
    let status = if timed_out {
        "timeout".to_string()
    } else if exit_is_ok(code, ok_exit_codes) {
        "0".to_string()
    } else {
        code.to_string()
    };
    let log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else {
            body
        },
        log_lines,
    }
}

fn run_termux_api(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let input = call.input.trim();
    if let Err(e) = validate_termux_api(input) {
        return Ok(ToolOutcome {
            user_message: e.to_string(),
            log_lines: vec![
                "ok:false | result:termux_api_rejected | exit:0 | elapsed_ms:0 | 状态:fail"
                    .to_string(),
                format!("termux_api rejected -> {}", build_preview(input, 160)),
            ],
        });
    }

    let started = Instant::now();
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
        .or(Some(TERMUX_API_TIMEOUT_SECS));
    let timeout_hint = timeout_secs;
    let (mut command, timeout_used) =
        build_command_with_optional_timeout(BASH_SHELL, &["-lc", input], timeout_secs);
    let (cwd_exec, cwd_display) =
        if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
            let cwd = expand_cwd_alias(cwd);
            let disp = short_cwd_display(&cwd);
            (PathBuf::from(cwd), disp)
        } else {
            let cwd = default_workdir();
            let disp = short_cwd_display(&cwd);
            (PathBuf::from(cwd), disp)
        };
    command
        .env("TERM", "xterm-256color")
        .env("COLORTERM", "truecolor")
        .env("LANG", "C.UTF-8")
        .env("LC_ALL", "C.UTF-8")
        .env("TERM_PROGRAM", "AItermux")
        .current_dir(cwd_exec);
    let out = command
        .output()
        .with_context(|| format!("termux_api 执行失败：{input}"))?;
    let elapsed = started.elapsed();
    Ok(build_shell_outcome_termux_api(
        out,
        elapsed,
        timeout_used,
        &cwd_display,
        input,
        timeout_hint,
        parse_shell_save_mode(call.save.as_deref()),
        call.ok_exit_codes.as_deref(),
    ))
}

fn find_files_by_name_with_timeout(
    root: &Path,
    needles: &[String],
    limit: usize,
    started: Instant,
    timeout_secs: u64,
    include_re: &[Regex],
    exclude_re: &[Regex],
    extra_exclude_dirs: &[String],
    out: &mut Vec<String>,
) -> bool {
    fn is_excluded_dir(name: &str) -> bool {
        SEARCH_EXCLUDE_DIRS
            .iter()
            .any(|d| d.eq_ignore_ascii_case(name))
    }

    if needles.is_empty() || limit == 0 {
        return true;
    }

    fn walk(
        dir: &Path,
        needles: &[String],
        limit: usize,
        started: Instant,
        timeout_secs: u64,
        include_re: &[Regex],
        exclude_re: &[Regex],
        extra_exclude_dirs: &[String],
        out: &mut Vec<String>,
    ) -> bool {
        if out.len() >= limit {
            return true;
        }
        if timeout_secs > 0 && started.elapsed().as_secs() >= timeout_secs {
            return false;
        }
        let Ok(rd) = fs::read_dir(dir) else {
            return true;
        };
        for entry in rd.flatten() {
            if out.len() >= limit {
                return true;
            }
            if timeout_secs > 0 && started.elapsed().as_secs() >= timeout_secs {
                return false;
            }
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().to_string();
            if path.is_dir() {
                if is_excluded_dir(&name)
                    || extra_exclude_dirs
                        .iter()
                        .any(|d| d.eq_ignore_ascii_case(name.as_str()))
                {
                    continue;
                }
                if !walk(
                    &path,
                    needles,
                    limit,
                    started,
                    timeout_secs,
                    include_re,
                    exclude_re,
                    extra_exclude_dirs,
                    out,
                ) {
                    return false;
                }
                continue;
            }
            if !path.is_file() {
                continue;
            }
            let lower = name.to_ascii_lowercase();
            if needles.iter().all(|kw| lower.contains(kw)) {
                if !search_path_allowed(&path, include_re, exclude_re, extra_exclude_dirs) {
                    continue;
                }
                let display = path.to_string_lossy().to_string();
                let shown = short_display_path(&display);
                if shown.is_empty() {
                    out.push(shorten_path(&display));
                } else if shown == "." {
                    out.push(".".to_string());
                } else if shown.starts_with('/')
                    || shown.starts_with('~')
                    || shown.starts_with("./")
                    || shown.starts_with("...")
                {
                    out.push(shown);
                } else {
                    out.push(format!("./{shown}"));
                }
            }
        }
        true
    }

    walk(
        root,
        needles,
        limit,
        started,
        timeout_secs,
        include_re,
        exclude_re,
        extra_exclude_dirs,
        out,
    )
}


// -----------------------------
// list (filesystem base)
// -----------------------------

const LIST_MAX_DEPTH_CAP: usize = 3;
const LIST_MAX_DEPTH_DEFAULT: usize = 1;
const LIST_STAT_SCAN_MAX_BYTES: u64 = 2_000_000;
// list：目录条目默认上限（不可配置）
const LIST_DIR_ENTRY_CAP: usize = 300;
// list：当目录条目超过上限时，回显给模型的“条目预览”数量（中间截断）
const LIST_DIR_EXPORT_PREVIEW_ENTRIES: usize = 160;
const FS_CACHE_DIR: &str = "log/fs-cache";

fn fs_tool_base_dir(call: &ToolCall) -> PathBuf {
    let base = if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
        expand_cwd_alias(cwd)
    } else {
        default_workdir()
    };
    let p = PathBuf::from(base);
    if p.is_absolute() {
        p
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(p)
    }
}

fn display_path_best_effort(path: &Path) -> String {
    if path.as_os_str().is_empty() {
        return String::new();
    }
    std::fs::canonicalize(path)
        .unwrap_or_else(|_| path.to_path_buf())
        .to_string_lossy()
        .to_string()
}

fn abs_path_from_cwd(raw: &str) -> PathBuf {
    let norm = normalize_tool_path(raw);
    if norm.is_empty() {
        return PathBuf::from("");
    }
    let p = PathBuf::from(norm);
    if p.is_absolute() {
        return p;
    }
    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(p)
}


fn real_display_path(path: &Path) -> String {
    if path.as_os_str().is_empty() {
        return String::new();
    }
    let abs = abs_path_from_cwd(&path.to_string_lossy());
    std::fs::canonicalize(&abs)
        .unwrap_or(abs)
        .to_string_lossy()
        .to_string()
}

fn fmt_time(ts: std::io::Result<std::time::SystemTime>) -> String {
    ts.ok()
        .and_then(|t| {
            let dt: chrono::DateTime<chrono::Local> = t.into();
            Some(dt.format("%Y-%m-%d %H:%M:%S").to_string())
        })
        .unwrap_or_else(|| "unknown".to_string())
}

fn fmt_mode(meta: &fs::Metadata) -> String {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        format!("{:04o}", meta.permissions().mode() & 0o7777)
    }
    #[cfg(not(unix))]
    {
        let _ = meta;
        "?".to_string()
    }
}

fn maybe_export_fs_text(file_prefix: &str, text: &str) -> (String, Option<ExportedOutputMeta>) {
    let total_bytes = text.as_bytes().len();
    let raw_lines = text.lines().count();
    let truncated_by_lines = raw_lines > OUTPUT_MAX_LINES;
    let truncated_by_chars = text.chars().count() > OUTPUT_MAX_CHARS;
    let need_save =
        total_bytes > EXPORT_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    if !need_save {
        return (truncate_tool_payload(text, OUTPUT_MAX_LINES, OUTPUT_MAX_CHARS), None);
    }
    let _ = fs::create_dir_all(FS_CACHE_DIR);
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let pid = unsafe { libc::getpid() };
    let path = format!("{FS_CACHE_DIR}/{file_prefix}_{ts}_{pid}.log");
    let ok = try_write_shell_cache_impl(FS_CACHE_DIR, &path, text.as_bytes(), &[]);
    if !ok {
        return (truncate_tool_payload(text, OUTPUT_MAX_LINES, OUTPUT_MAX_CHARS), None);
    }
    let abs_saved = real_display_path(Path::new(&path));
    let meta = exported_meta(abs_saved, total_bytes, raw_lines);
    let preview = truncate_export_preview(text);
    let mut out = String::new();
    out.push_str(&format_exported_notice(&meta));
    if !preview.trim().is_empty() {
        out.push_str("\n\n");
        out.push_str(preview.trim_end());
    }
    (out, Some(meta))
}

fn export_fs_text_always(file_prefix: &str, text: &str) -> Option<ExportedOutputMeta> {
    let total_bytes = text.as_bytes().len();
    let raw_lines = text.lines().count();
    let _ = fs::create_dir_all(FS_CACHE_DIR);
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let pid = unsafe { libc::getpid() };
    let path = format!("{FS_CACHE_DIR}/{file_prefix}_{ts}_{pid}.log");
    let ok = try_write_shell_cache_impl(FS_CACHE_DIR, &path, text.as_bytes(), &[]);
    if !ok {
        return None;
    }
    let abs_saved = real_display_path(Path::new(&path));
    Some(exported_meta(abs_saved, total_bytes, raw_lines))
}

fn list_preview_middle_truncate(lines: &[String], want: usize) -> Vec<String> {
    let want = want.max(1);
    if lines.len() <= want {
        return lines.to_vec();
    }
    let head = want / 2;
    let tail = want - head;
    let omitted = lines.len().saturating_sub(head + tail);
    let mut out: Vec<String> = Vec::new();
    out.extend(lines.iter().take(head).cloned());
    out.push(format!("... (中间省略 {omitted} 条) ..."));
    out.extend(lines.iter().skip(lines.len().saturating_sub(tail)).cloned());
    out
}

fn list_one_file_abs(abs: &Path) -> anyhow::Result<(bool, u64, String)> {
    let abs_display = display_path_best_effort(abs);
    if abs_display.is_empty() {
        return Ok((false, 0, "path: (empty)\nstatus: fail\nreason: empty path".to_string()));
    }
    if !abs.exists() {
        return Ok((false, 0, format!("path: {abs_display}\nstatus: not_found")));
    }
    let meta = fs::symlink_metadata(abs).with_context(|| format!("获取文件信息失败：{abs_display}"))?;
    let real = abs_display;
    let readonly = meta.permissions().readonly();
    let mode = fmt_mode(&meta);
    let mtime = fmt_time(meta.modified());
    let atime = fmt_time(meta.accessed());
    let created = fmt_time(meta.created());

    let ft = meta.file_type();
    if ft.is_symlink() {
        let target = fs::read_link(abs)
            .ok()
            .and_then(|x| x.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| "unknown".to_string());
        let body = format!(
            "path: {real}\nkind: symlink\ntarget: {target}\ncreated: {created}\nmtime: {mtime}\natime: {atime}\nreadonly: {readonly}\nmode: {mode}"
        );
        return Ok((true, 0, body));
    }

    if meta.is_dir() {
        let body = format!(
            "path: {real}\nkind: dir\ncreated: {created}\nmtime: {mtime}\natime: {atime}\nreadonly: {readonly}\nmode: {mode}"
        );
        return Ok((true, 0, body));
    }

    let bytes = meta.len();
    let mut lines: Option<u64> = None;
    let mut chars: Option<u64> = None;
    let mut note = String::new();
    if bytes == 0 {
        lines = Some(0);
        chars = Some(0);
    } else if bytes <= LIST_STAT_SCAN_MAX_BYTES && !is_probably_binary_file(abs) {
        let f = fs::File::open(abs).with_context(|| format!("读取失败：{real}"))?;
        let mut reader = std::io::BufReader::new(f);
        let mut buf = String::new();
        let mut ln: u64 = 0;
        let mut ch: u64 = 0;
        while reader.read_line(&mut buf).unwrap_or(0) > 0 {
            ln = ln.saturating_add(1);
            ch = ch.saturating_add(buf.trim_end_matches(['\n', '\r']).chars().count() as u64);
            buf.clear();
        }
        lines = Some(ln);
        chars = Some(ch);
    } else {
        note = if is_probably_binary_file(abs) {
            "note: binary_file (skip lines/chars)".to_string()
        } else {
            format!(
                "note: too_large (>{} bytes) (skip lines/chars)",
                LIST_STAT_SCAN_MAX_BYTES
            )
        };
    }

    let mut body = String::new();
    body.push_str(&format!("path: {real}\n"));
    body.push_str("kind: file\n");
    body.push_str(&format!("bytes: {bytes} ({})\n", format_bytes(bytes)));
    if let Some(n) = lines {
        body.push_str(&format!("lines: {n}\n"));
    } else {
        body.push_str("lines: unknown\n");
    }
    if let Some(n) = chars {
        body.push_str(&format!("chars: {n}\n"));
    } else {
        body.push_str("chars: unknown\n");
    }
    body.push_str(&format!("created: {created}\n"));
    body.push_str(&format!("mtime: {mtime}\n"));
    body.push_str(&format!("atime: {atime}\n"));
    body.push_str(&format!("readonly: {readonly}\n"));
    body.push_str(&format!("mode: {mode}"));
    if !note.is_empty() {
        body.push('\n');
        body.push_str(&note);
    }
    Ok((true, bytes, body))
}

fn run_list_tool(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let started = Instant::now();
    let depth = call
        .depth
        .unwrap_or(LIST_MAX_DEPTH_DEFAULT)
        .clamp(1, LIST_MAX_DEPTH_CAP);
    let base_dir = fs_tool_base_dir(call);
    let cwd_log = short_cwd_display(&display_path_best_effort(&base_dir));
    let op = call.op.as_deref().unwrap_or("dir").trim().to_ascii_lowercase();
    // list 不支持 paths：避免模型产生“多路径 list”心智负担。
            if call
                .paths
                .as_ref()
                .is_some_and(|v| v.iter().any(|p| !p.trim().is_empty()))
            {
                return Ok(tool_format_error(
                    "list",
                    "list 不支持 paths；用 cwd 指定目录；同目录多文件用 name 数组。",
                ));
            }

    // 收敛协议：list 只基于 cwd 工作（由 validate_tool_call 保证）。
    let names = call
        .names
        .as_ref()
        .map(|v| {
            v.iter()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let abs = base_dir.clone();
    let abs_display = real_display_path(&base_dir);
    if abs_display.is_empty() {
        return Ok(tool_format_error("list", "缺少 cwd"));
    }
    if !abs.exists() {
        return Ok(ToolOutcome {
            user_message: format!("路径不存在：{abs_display}"),
            log_lines: vec![format!(
                "状态:ok_not_found | 耗时:{}ms | path:{}",
                started.elapsed().as_millis(),
                abs_display
            )],
        });
    }

    // op=files：输出 cwd 目录下 names 的文件信息（纯文件名）
    if op == "files" {
        if !abs.is_dir() {
            return Ok(tool_format_error("list", "list.op=files 需要 cwd 指向目录"));
        }
        let mut blocks: Vec<String> = Vec::new();
        let mut ok_count = 0usize;
        let mut fail_count = 0usize;
        let mut total_bytes: u64 = 0;
        for (idx, name) in names.iter().enumerate() {
            let t = name.trim();
            if t.is_empty()
                || t.contains('/')
                || t.contains('\\')
                || Path::new(t).is_absolute()
                || t == "."
                || t == ".."
            {
                fail_count = fail_count.saturating_add(1);
                blocks.push(format!(
                    "[#{}]\npath: {}/{}\nstatus: fail\nreason: invalid_file_name",
                    idx + 1,
                    abs_display,
                    t
                ));
                continue;
            }
            let joined = abs.join(t);
            let (ok, bytes, body) = list_one_file_abs(&joined)?;
            if ok {
                ok_count = ok_count.saturating_add(1);
            } else {
                fail_count = fail_count.saturating_add(1);
            }
            total_bytes = total_bytes.saturating_add(bytes);
            blocks.push(format!("[#{}]\n{}", idx + 1, body.trim_end()));
        }
        let body = blocks.join("\n\n---\n\n");
        let raw_lines = body.lines().count();
        let raw_chars = body.chars().count();
        let (body2, saved) = maybe_export_fs_text("list_files", &body);
        let mut log = format!(
            "状态:0 | 耗时:{}ms | op:list | type:dir_files | cwd:{} | base:{} | files:{} | ok:{} | fail:{} | bytes:{} | lines:{} | chars:{}",
            started.elapsed().as_millis(),
            cwd_log,
            abs_display,
            names.len(),
            ok_count,
            fail_count,
            total_bytes,
            raw_lines,
            raw_chars
        );
        if let Some(m) = saved.as_ref() {
            log.push_str(&format!(" | saved:{}", m.path));
        }
        return Ok(ToolOutcome {
            user_message: body2,
            log_lines: vec![log],
        });
    }

    // op=dir：列目录（若 path 实际是文件，则退化成“看文件信息”，兼容用户习惯）
    if abs.is_file() || abs.symlink_metadata().map(|m| m.is_file()).unwrap_or(false) {
        let (_ok, _bytes, body) = list_one_file_abs(&abs)?;
        let mut log = format!(
            "状态:0 | 耗时:{}ms | op:list | type:file | cwd:{} | path:{}",
            started.elapsed().as_millis(),
            cwd_log,
            abs_display
        );
        let (body2, saved) = maybe_export_fs_text("list_file", &body);
        if let Some(m) = saved.as_ref() {
            log.push_str(&format!(" | saved:{}", m.path));
        }
        return Ok(ToolOutcome {
            user_message: body2,
            log_lines: vec![log],
        });
    }

    // 目录：枚举（递归 depth），条目上限固定为 300（不可配置）
    let cwd_display = display_path_best_effort(&base_dir);

    let mut out: Vec<String> = Vec::new();
    out.push(format!("cwd: {cwd_display}"));
    out.push(format!("path: {abs_display}"));
    out.push(format!("depth: {depth}"));
    out.push(String::new());

    let mut produced = 0usize;
    let mut truncated = false;

    fn list_dir_rec(
        root: &Path,
        rel_prefix: &str,
        level: usize,
        max_level: usize,
        max_entries: usize,
        produced: &mut usize,
        truncated: &mut bool,
        out: &mut Vec<String>,
    ) -> anyhow::Result<()> {
        if *produced >= max_entries {
            *truncated = true;
            return Ok(());
        }
        let mut items: Vec<(String, PathBuf)> = Vec::new();
        let rd = match fs::read_dir(root) {
            Ok(rd) => rd,
            Err(_) => {
                let here = real_display_path(root);
                out.push(format!("dir | unreadable:1 | {here}"));
                *produced = produced.saturating_add(1);
                return Ok(());
            }
        };
        for entry in rd.flatten() {
            let name = sanitize_search_line(&entry.file_name().to_string_lossy());
            items.push((name, entry.path()));
        }
        items.sort_by(|a, b| a.0.cmp(&b.0));

        for (name, ep) in items.into_iter() {
            if *produced >= max_entries {
                *truncated = true;
                break;
            }
            let meta = fs::metadata(&ep).ok();
            let is_dir = meta.as_ref().is_some_and(|m| m.is_dir());
            let is_file = meta.as_ref().is_some_and(|m| m.is_file());
            let kind = if is_dir { "dir" } else if is_file { "file" } else { "other" };
            let hidden = if name.starts_with('.') { " hidden:1" } else { "" };
            let size_part = if is_file {
                let bytes = meta.as_ref().map(|m| m.len()).unwrap_or(0);
                format!(" size:{}({})", bytes, format_bytes(bytes))
            } else {
                String::new()
            };
            let mut rel = if rel_prefix.is_empty() {
                name.clone()
            } else {
                format!("{rel_prefix}/{name}")
            };
            if is_dir {
                rel = format!("{rel}/");
            }
            out.push(format!("{kind}{size_part}{hidden} | {rel}"));
            *produced = produced.saturating_add(1);

            if is_dir && level < max_level && !*truncated {
                list_dir_rec(
                    &ep,
                    &rel.trim_end_matches('/'),
                    level.saturating_add(1),
                    max_level,
                    max_entries,
                    produced,
                    truncated,
                    out,
                )?;
            }
        }
        Ok(())
    }

    list_dir_rec(
        &abs,
        "",
        1,
        depth,
        LIST_DIR_ENTRY_CAP,
        &mut produced,
        &mut truncated,
        &mut out,
    )?;

    // 将 entries/truncated 回填到 header（第 3 行）
    if let Some(line) = out.get_mut(2) {
        *line = format!(
            "depth: {depth} | entries: {produced} | truncated: {}",
            if truncated { 1 } else { 0 }
        );
    }

    let body = out.join("\n").trim_end().to_string();
    let raw_lines = body.lines().count();
    let raw_chars = body.chars().count();
    let (body2, saved) = if truncated {
        // 超过 300 条：强制导出，并只回显 160 条（中间截断）
        let meta = export_fs_text_always("list_dir", &body);
        let entries = if out.len() >= 4 { &out[4..] } else { &[] };
        let preview_entries = list_preview_middle_truncate(entries, LIST_DIR_EXPORT_PREVIEW_ENTRIES);
        let mut preview_lines: Vec<String> = Vec::new();
        preview_lines.extend(out.iter().take(4).cloned()); // header + blank
        preview_lines.extend(preview_entries);
        let preview_body = preview_lines.join("\n").trim_end().to_string();
        if let Some(m) = meta.clone() {
            let mut msg = String::new();
            msg.push_str(&format_exported_notice(&m));
            if !preview_body.trim().is_empty() {
                msg.push_str("\n\n");
                msg.push_str(preview_body.trim_end());
            }
            (msg, Some(m))
        } else {
            (preview_body, None)
        }
    } else {
        maybe_export_fs_text("list_dir", &body)
    };
    let mut log = format!(
        "状态:0 | 耗时:{}ms | op:list | type:dir | cwd:{} | path:{} | depth:{} | entries:{} | truncated:{} | lines:{} | chars:{}",
        started.elapsed().as_millis(),
        cwd_log,
        abs_display,
        depth,
        produced,
        if truncated { 1 } else { 0 },
        raw_lines,
        raw_chars
    );
    if let Some(m) = saved.as_ref() {
        log.push_str(&format!(" | saved:{}", m.path));
    }
    Ok(ToolOutcome {
        user_message: body2,
        log_lines: vec![log],
    })
}



pub(crate) fn run_search(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    if call.memory.unwrap_or(false) {
        return run_search_memory_like("search", call);
    }
    let mut pattern = call
        .pattern
        .as_deref()
        .unwrap_or(call.input.trim())
        .trim()
        .to_string();
    if pattern.is_empty() {
        return Ok(ToolOutcome {
            user_message: "search 需要 pattern".to_string(),
            log_lines: vec![],
        });
    }
    let mut root = call
        .root
        .as_deref()
        .or(call.path.as_deref())
        .unwrap_or(".")
        .trim()
        .to_string();
    let mut search_files = call.file.unwrap_or(false);
    // 若调用方没有显式提供 root（或仅是 "."），但把字段写进了 pattern 字符串，则尽量自愈解析。
    if call.root.as_deref().unwrap_or("").trim().is_empty()
        && call.path.as_deref().unwrap_or("").trim().is_empty()
    {
        if let Some((p2, r2, f2)) = parse_search_dsl_fallback(&pattern) {
            pattern = p2;
            root = r2;
            if let Some(f) = f2 {
                search_files = f;
            }
        }
    }

    // 允许在 pattern 内联 ctx 标记（仅当未显式传 context* 字段时生效）
    let inline_ctx_allowed = call.context.is_none()
        && call.context_lines.is_none()
        && call.context_files.is_none()
        && call.context_hits_per_file.is_none();
    let (pattern2, inline_ctx_lines, inline_ctx_enabled) =
        extract_search_inline_ctx_marker(&pattern);
    if inline_ctx_allowed && (inline_ctx_enabled || inline_ctx_lines.is_some()) {
        pattern = pattern2;
    }

    let (pattern_effective, keywords) = normalize_search_pattern(&pattern);
    if !Path::new(&root).exists() {
        return Ok(ToolOutcome {
            user_message: format!("search 根路径不存在：{root}"),
            log_lines: vec![format!("状态:ok_not_found | root:{}", shorten_path(&root))],
        });
    }
    let mut want_context = call.context.unwrap_or(false)
        || call.context_lines.is_some()
        || call.context_files.is_some()
        || call.context_hits_per_file.is_some();
    if inline_ctx_allowed {
        want_context = want_context || inline_ctx_enabled || inline_ctx_lines.is_some();
    }
    let max_matches = call
        .count
        .unwrap_or(SEARCH_DEFAULT_MAX_MATCHES)
        .clamp(1, SEARCH_MAX_MATCHES_CAP);
    let started = Instant::now();
    let timeout_secs = call
        .timeout_secs
        .or(call.timeout_ms.map(|ms| ms.saturating_add(999) / 1000))
        .or(Some(SEARCH_TIMEOUT_SECS));
    let timeout_hint = timeout_secs;
    let pattern_preview = build_preview(&pattern_effective, 80);
    let (out_lines, out_chars) = pick_search_output_limits(call);

    let include_globs = call.include_glob.clone().unwrap_or_default();
    let exclude_globs = call.exclude_glob.clone().unwrap_or_default();
    let extra_exclude_dirs = call.exclude_dirs.clone().unwrap_or_default();
    let include_re = compile_globs(&include_globs);
    let exclude_re = compile_globs(&exclude_globs);

    // search(format=coords)：只返回坐标（abs_path:line），不返回命中行内容。
    // search(format=ranges)：只返回行号范围（abs_path:Lx-Ly），用于指导按需 read。
    // 仅对“内容搜索”生效；文件名搜索（file:true）与上下文搜索（context:true）忽略该字段。
    let format = call
        .format
        .as_deref()
        .unwrap_or("")
        .trim()
        .to_ascii_lowercase();
    let want_coords = format == "coords" || format == "coord";
    let want_ranges = format == "ranges" || format == "range";
    if want_coords && !search_files && !want_context {
        return run_search_coords(call, &pattern_effective, &root);
    }
    if want_ranges && !search_files && !want_context {
        return run_search_ranges(call, &pattern_effective, &root);
    }
    if search_files {
        let needles: Vec<String> = keywords
            .iter()
            .map(|s| s.to_ascii_lowercase())
            .take(SEARCH_MAX_KEYWORDS)
            .collect();
        let mut matches: Vec<String> = Vec::new();
        let finished = find_files_by_name_with_timeout(
            Path::new(&root),
            &needles,
            max_matches,
            started,
            timeout_hint.unwrap_or(SEARCH_TIMEOUT_SECS),
            &include_re,
            &exclude_re,
            &extra_exclude_dirs,
            &mut matches,
        );
        let elapsed = started.elapsed();
        let match_count = matches.len();
        let mut body = if match_count == 0 {
            "未找到匹配".to_string()
        } else {
            matches.join("\n")
        };
        if match_count >= max_matches && match_count > 0 {
            body = format!(
                "【结果过多】已返回前 {max_matches} 条文件名匹配结果（结果可能不完整）。\n{}",
                body.trim_end()
            );
        }
        if !finished {
            body = annotate_timeout(body, true, timeout_hint);
        }

        let total_bytes = body.as_bytes().len();
        let raw_lines = body.lines().count();
        let truncated_by_lines = raw_lines > out_lines;
        let truncated_by_chars = body.chars().count() > out_chars;
        let need_save =
            total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
        let saved_meta = if need_save {
            let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
            let pid = unsafe { libc::getpid() };
            let path = format!("{SEARCH_CACHE_DIR}/search_files_{ts}_{pid}.log");
            if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
                Some(exported_meta(path, total_bytes, raw_lines))
            } else {
                None
            }
        } else {
            None
        };

        let body = if let Some(meta) = saved_meta.as_ref() {
            let preview = truncate_export_preview(&body);
            let mut out = String::new();
            out.push_str(&format_exported_notice(meta));
            if !preview.trim().is_empty() {
                out.push_str("\n\n");
                out.push_str(preview.trim_end());
            }
            out
        } else {
            truncate_tool_payload(&body, out_lines, out_chars)
        };
        let mut log = format!(
            "状态:0 | 耗时:{}ms | root:{} | matches:{} | engine:find | file:true | pattern:{pattern_preview}",
            elapsed.as_millis(),
            shorten_path(&root),
            match_count
        );
        if let Some(meta) = saved_meta.as_ref() {
            log.push_str(&format!(" | saved:{}", meta.path));
        }
        return Ok(ToolOutcome {
            user_message: body,
            log_lines: vec![log],
        });
    }
    let needles: Vec<String> = keywords
        .iter()
        .map(|s| s.to_ascii_lowercase())
        .take(SEARCH_MAX_KEYWORDS)
        .collect();

    if want_context {
        let context_lines = call
            .context_lines
            .or(inline_ctx_lines)
            .unwrap_or(SEARCH_CONTEXT_DEFAULT_LINES)
            .clamp(1, SEARCH_CONTEXT_MAX_LINES);
        let context_files = call
            .context_files
            .unwrap_or(SEARCH_CONTEXT_DEFAULT_FILES)
            .clamp(1, SEARCH_CONTEXT_MAX_FILES);
        let hits_per_file = call
            .context_hits_per_file
            .unwrap_or(SEARCH_CONTEXT_DEFAULT_HITS_PER_FILE)
            .clamp(1, SEARCH_CONTEXT_MAX_HITS_PER_FILE);
        return Ok(run_search_with_context(SearchWithContextArgs {
            root: root.as_str(),
            pattern: pattern_effective.as_str(),
            needles: &needles,
            max_matches,
            started,
            timeout_secs: timeout_hint.unwrap_or(SEARCH_TIMEOUT_SECS),
            pattern_preview: pattern_preview.as_str(),
            out_lines,
            out_chars,
            context_lines,
            context_files,
            hits_per_file,
            include_globs: &include_globs,
            exclude_globs: &exclude_globs,
            extra_exclude_dirs: &extra_exclude_dirs,
            include_re: &include_re,
            exclude_re: &exclude_re,
        }));
    }
    // 多关键词 AND：仅在“纯关键词”模式启用；带正则元字符时保留 regex 语义（rg/grep）。
    if needles.len() >= 2 && !search_pattern_looks_like_regex(&pattern_effective) {
        return Ok(run_search_keywords_and(
            &root,
            &needles,
            max_matches,
            started,
            timeout_hint.unwrap_or(SEARCH_TIMEOUT_SECS),
            &pattern_preview,
            out_lines,
            out_chars,
            &include_re,
            &exclude_re,
            &extra_exclude_dirs,
        ));
    }
    let mut engine = "rg";
    let mut rg_args: Vec<String> = vec![
        "--line-number".to_string(),
        "--no-heading".to_string(),
        "-S".to_string(),
        "--max-count".to_string(),
        max_matches.to_string(),
        "--max-filesize".to_string(),
        SEARCH_MAX_FILESIZE.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    // 默认排除常见二进制/数据库文件：避免输出乱码、抖动与无意义 token 消耗。
    for g in ["!**/*.db", "!**/*.sqlite", "!**/*.sqlite3"] {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in include_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for g in exclude_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!{g}"));
    }
    rg_args.push("--".to_string());
    rg_args.push(pattern_effective.clone());
    rg_args.push(root.clone());

    let mut grep_args: Vec<String> = vec![
        "-RIn".to_string(),
        "-I".to_string(),
        "--binary-files=without-match".to_string(),
        "-m".to_string(),
        max_matches.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push((*d).to_string());
    }
    for g in ["*.db", "*.sqlite", "*.sqlite3"] {
        grep_args.push("--exclude".to_string());
        grep_args.push(g.to_string());
    }
    for d in extra_exclude_dirs.iter() {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push(d.to_string());
    }
    for g in include_globs.iter() {
        grep_args.push(format!("--include={g}"));
    }
    for g in exclude_globs.iter() {
        grep_args.push(format!("--exclude={g}"));
    }
    grep_args.push("--".to_string());
    grep_args.push(pattern_effective);
    grep_args.push(root.clone());

    let rg_refs: Vec<&str> = rg_args.iter().map(|s| s.as_str()).collect();
    let grep_refs: Vec<&str> = grep_args.iter().map(|s| s.as_str()).collect();

    let mut out = run_command_output_with_optional_timeout("rg", &rg_refs, timeout_secs);
    if out.is_err() {
        engine = "grep";
        out = run_command_output_with_optional_timeout("grep", &grep_refs, timeout_secs);
    }
    let (code, stdout, stderr, timed_out) =
        out.unwrap_or_else(|_| (127, String::new(), "search failed".to_string(), false));
    let elapsed = started.elapsed();

    let match_count = stdout.lines().count();
    let compact_stdout = compact_search_stdout(&stdout);
    let combined = collect_output(&compact_stdout, &stderr);
    let no_match = combined.trim().is_empty() && code == 1 && !timed_out;
    let raw_body = if no_match {
        "未找到匹配".to_string()
    } else {
        combined
    };

    let total_bytes = stdout
        .as_bytes()
        .len()
        .saturating_add(stderr.as_bytes().len());
    let raw_lines = raw_body.lines().count();
    let truncated_by_lines = raw_lines > out_lines;
    let truncated_by_chars = raw_body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_{ts}_{pid}.log");
        if try_write_shell_cache_impl(
            SEARCH_CACHE_DIR,
            &path,
            stdout.as_bytes(),
            stderr.as_bytes(),
        ) {
            Some(exported_meta(path, total_bytes, raw_lines))
        } else {
            None
        }
    } else {
        None
    };

    let mut body = if let Some(meta) = saved_meta.as_ref()
        && !no_match
    {
        let preview = truncate_export_preview(&raw_body);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_tool_payload(&raw_body, out_lines, out_chars)
    };
    body = annotate_timeout(body, timed_out, timeout_hint);
    let status_code = if no_match { 0 } else { code };
    body = annotate_nonzero_exit(body, timed_out, status_code);
    let status = status_label(status_code, timed_out);
    let mut log = format!(
        "状态:{status} | 耗时:{}ms | root:{} | matches:{} | engine:{engine} | pattern:{pattern_preview}",
        elapsed.as_millis(),
        shorten_path(&root),
        match_count
    );
    if let Some(meta) = saved_meta.as_ref() {
        log.push_str(&format!(" | saved:{}", meta.path));
    }

    Ok(ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    })
}

#[derive(Debug)]
struct MemoryBlock {
    start_line: usize,
    end_line: usize,
    lines: Vec<String>,
}

fn parse_memory_keywords(pattern: &str) -> Vec<String> {
    let mut normalized = pattern.to_string();
    for sep in ['，', ',', '、', '|', ';', '；', '/', '\\', '\n', '\t'] {
        normalized = normalized.replace(sep, " ");
    }
    normalized
        .split_whitespace()
        .map(|s| s.trim().to_ascii_lowercase())
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
}

fn dedup_keywords(keywords: &[String], cap: usize) -> Vec<String> {
    use std::collections::HashSet;
    let mut seen: HashSet<String> = HashSet::new();
    let mut out: Vec<String> = Vec::new();
    for kw in keywords {
        let t = kw.trim().to_ascii_lowercase();
        if t.is_empty() {
            continue;
        }
        if seen.insert(t.clone()) {
            out.push(t);
        }
        if out.len() >= cap {
            break;
        }
    }
    out
}

fn parse_memory_date(raw: &str) -> Option<NaiveDate> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if let Ok(date) = NaiveDate::parse_from_str(trimmed, "%Y-%m-%d") {
        return Some(date);
    }
    let digits: String = trimmed.chars().filter(|c| c.is_ascii_digit()).collect();
    if digits.len() >= 8 {
        let y = digits.get(0..4)?.parse::<i32>().ok()?;
        let m = digits.get(4..6)?.parse::<u32>().ok()?;
        let d = digits.get(6..8)?.parse::<u32>().ok()?;
        return NaiveDate::from_ymd_opt(y, m, d);
    }
    None
}

fn parse_memory_date_range(
    start_raw: &str,
    end_raw: &str,
) -> Result<(Option<NaiveDate>, Option<NaiveDate>), &'static str> {
    let start_raw = start_raw.trim();
    let end_raw = end_raw.trim();
    let mut start = if start_raw.is_empty() {
        None
    } else {
        Some(parse_memory_date(start_raw).ok_or("start_date 无效")?)
    };
    let mut end = if end_raw.is_empty() {
        None
    } else {
        Some(parse_memory_date(end_raw).ok_or("end_date 无效")?)
    };
    if start.is_some() && end.is_none() {
        end = start;
    }
    if end.is_some() && start.is_none() {
        start = end;
    }
    if let (Some(left), Some(right)) = (start, end)
        && left > right
    {
        start = Some(right);
        end = Some(left);
    }
    Ok((start, end))
}

#[derive(Clone, Copy, Debug)]
enum MemoDateRegion {
    Recent7d,
    Past1y,
    Older,
}

fn parse_memo_date_region(raw: &str) -> Option<MemoDateRegion> {
    let t = raw.trim().to_ascii_lowercase();
    if t.is_empty() {
        return None;
    }
    match t.as_str() {
        "recent" | "week" | "7d" | "last7" | "last_7" | "最近" | "一周" | "近一周" => {
            Some(MemoDateRegion::Recent7d)
        }
        "past" | "year" | "1y" | "过去" | "一年" | "近一年" => Some(MemoDateRegion::Past1y),
        "older" | "old" | "以前" | "更早" | "一年以前" | ">1y" => {
            Some(MemoDateRegion::Older)
        }
        _ => None,
    }
}

fn call_region_or_section(call: &ToolCall) -> String {
    let r = call.region.as_deref().unwrap_or("").trim();
    if !r.is_empty() {
        return r.to_string();
    }
    call.section.as_deref().unwrap_or("").trim().to_string()
}

fn default_date_range_for_region(region: MemoDateRegion) -> (Option<NaiveDate>, Option<NaiveDate>) {
    let today = Local::now().date_naive();
    let recent_start = today - chrono::Duration::days(7);
    let one_year_ago = today - chrono::Duration::days(365);
    let older_end = one_year_ago - chrono::Duration::days(1);
    let past_end = recent_start - chrono::Duration::days(1);
    match region {
        MemoDateRegion::Recent7d => (Some(recent_start), Some(today)),
        MemoDateRegion::Past1y => (Some(one_year_ago), Some(past_end)),
        MemoDateRegion::Older => (None, Some(older_end)),
    }
}

fn extract_block_date(block: &MemoryBlock) -> Option<NaiveDate> {
    let head = block.lines.first()?.trim();
    parse_memory_date(head)
}

fn collect_memory_blocks(text: &str) -> Vec<MemoryBlock> {
    let mut blocks = Vec::new();
    let mut current: Vec<String> = Vec::new();
    let mut start_line = 0usize;
    for (idx, line) in text.lines().enumerate() {
        let line_no = idx + 1;
        if line.trim().is_empty() {
            if !current.is_empty() {
                blocks.push(MemoryBlock {
                    start_line,
                    end_line: line_no.saturating_sub(1),
                    lines: std::mem::take(&mut current),
                });
                start_line = 0;
            }
            continue;
        }
        if current.is_empty() && line.trim_start().starts_with('#') {
            continue;
        }
        if current.is_empty() {
            start_line = line_no;
        }
        current.push(line.to_string());
    }
    if !current.is_empty() {
        let end_line = text.lines().count().max(start_line);
        blocks.push(MemoryBlock {
            start_line,
            end_line,
            lines: current,
        });
    }
    blocks
}

fn memo_row_header(content: &str) -> String {
    content
        .lines()
        .find(|line| !line.trim().is_empty())
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| "(空记录)".to_string())
}

fn memo_row_preview(content: &str, keywords: &[String]) -> Option<String> {
    if keywords.is_empty() {
        return None;
    }
    for line in content.lines() {
        let lower = line.to_ascii_lowercase();
        if keywords.iter().any(|kw| lower.contains(kw)) {
            let trimmed = line.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_string());
            }
        }
    }
    None
}

fn render_memo_rows(rows: &[MemoRow]) -> String {
    let mut out = String::new();
    for (idx, row) in rows.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        out.push_str(&format!("[L{}]\n", row.rownum));
        out.push_str(row.content.trim_end());
        out.push('\n');
    }
    out.trim_end().to_string()
}

fn run_search_memory_like(tool_tag: &str, call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
    if pattern.is_empty() {
        return Ok(tool_format_error(tool_tag, "缺少 pattern"));
    }
    let target = call
        .root
        .as_deref()
        .or(call.path.as_deref())
        .unwrap_or("")
        .trim();
    let files = memory_paths_for_check(target);
    if files.is_empty() {
        return Ok(tool_format_error(tool_tag, "目标文件无效"));
    }
    let max_hits = call
        .count
        .unwrap_or(MEMORY_CHECK_DEFAULT_RESULTS)
        .clamp(1, MEMORY_CHECK_MAX_RESULTS);
    let started = Instant::now();
    let keywords = dedup_keywords(&parse_memory_keywords(pattern), SEARCH_MAX_KEYWORDS);
    if keywords.is_empty() {
        return Ok(tool_format_error(tool_tag, "缺少有效关键词"));
    }
    let (start_date, end_date) = match parse_memory_date_range(
        call.date_start.as_deref().unwrap_or(""),
        call.date_end.as_deref().unwrap_or(""),
    ) {
        Ok(range) => range,
        Err(reason) => return Ok(tool_format_error(tool_tag, reason)),
    };
    let range_active = start_date.is_some() || end_date.is_some();
    let mut hits = 0usize;
    let mut total_hits = 0usize;
    let mut out = String::new();
    let mut memo_db: Option<MemoDb> = None;
    let mut used_sqlite = false;
    let mut global_min: Option<NaiveDate> = None;
    let mut global_max: Option<NaiveDate> = None;

    for (label, path) in files {
        if label == "fastmemo" || label == "scopememo" {
            let text = fs::read_to_string(&path).unwrap_or_default();
            let mut block_hits = 0usize;
            // fastmemo 的 block 头部是日期；memory_check/search(memory) 默认不做复杂区间筛选，避免误命中。
            if range_active {
                continue;
            }
            for block in collect_memory_blocks(&text) {
                let mut hay = String::new();
                for line in &block.lines {
                    hay.push_str(line);
                    hay.push('\n');
                }
                let hay_lower = hay.to_ascii_lowercase();
                if !keywords.iter().all(|kw| hay_lower.contains(kw)) {
                    continue;
                }
                total_hits = total_hits.saturating_add(1);
                if hits >= max_hits {
                    continue;
                }
                let header_raw = block
                    .lines
                    .first()
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                    .unwrap_or("(空记录)");
                let header = build_preview(header_raw, 160);
                let mut preview = "";
                for line in &block.lines {
                    let lower = line.to_ascii_lowercase();
                    if keywords.iter().any(|kw| lower.contains(kw)) {
                        preview = line.trim();
                        break;
                    }
                }
                if block_hits == 0 {
                    if !out.is_empty() {
                        out.push('\n');
                    }
                    out.push_str(&format!("[{label}]"));
                    out.push('\n');
                }
                let span = if block.start_line == block.end_line {
                    format!("L{}", block.start_line)
                } else {
                    format!("L{}-{}", block.start_line, block.end_line)
                };
                out.push_str(&format!("- {span} {header}\n"));
                if !preview.is_empty() && preview != header_raw {
                    out.push_str(&format!("  {}\n", build_preview(preview, 160)));
                }
                hits = hits.saturating_add(1);
                block_hits = block_hits.saturating_add(1);
            }
            continue;
        }

        let db = if let Some(db) = memo_db.as_ref() {
            db.clone()
        } else {
            let db = MemoDb::open_default()?;
            memo_db = Some(db.clone());
            db
        };
        let (kind, is_meta) = if label == "datememo" {
            (MemoKind::Date, false)
        } else {
            (MemoKind::Meta, true)
        };
        used_sqlite = true;

        // --- datememo/metamemo 规则增强 ---
        // metamemo：仅允许“精确日期”检索（必须带日期），且要求 section 标记为 day（当天）。
        if is_meta {
            let reg = call_region_or_section(call);
            let sec = reg.trim();
            let sec_ok = matches!(sec, "day" | "当天" | "date" | "当日" | "日" | "天");
            if !sec_ok {
                return Ok(tool_format_error(
                    tool_tag,
                    "metamemo 需要 region=day（当天）",
                ));
            }
            if start_date.is_none() && end_date.is_none() {
                return Ok(tool_format_error(
                    tool_tag,
                    "metamemo 需要 date_start/date_end（精确日期）",
                ));
            }
            let (exact_start, exact_end) = match parse_memory_date_range(
                call.date_start.as_deref().unwrap_or(""),
                call.date_end.as_deref().unwrap_or(""),
            ) {
                Ok(v) => v,
                Err(reason) => return Ok(tool_format_error(tool_tag, reason)),
            };
            let Some(day) = exact_start.or(exact_end) else {
                return Ok(tool_format_error(
                    tool_tag,
                    "metamemo 需要 date_start/date_end（精确日期）",
                ));
            };
            let day_end = exact_end.unwrap_or(day);
            if day != day_end {
                return Ok(tool_format_error(tool_tag, "metamemo 仅支持精确到单日"));
            }
            let remaining = max_hits.saturating_sub(hits);
            let (rows, label_total, _stats, _min_date, _max_date) =
                db.check_by_keywords(kind, &keywords, Some(day), Some(day), remaining)?;
            total_hits = total_hits.saturating_add(label_total);
            if rows.is_empty() {
                continue;
            }
            if !out.is_empty() {
                out.push('\n');
            }
            out.push_str(&format!(
                "[{label} day:{} | db:{}]",
                day.format("%Y-%m-%d"),
                shorten_path(&memo_db_path_display())
            ));
            out.push('\n');
            for row in rows {
                let header_raw = memo_row_header(&row.content);
                let header = build_preview(&header_raw, 160);
                let preview = memo_row_preview(&row.content, &keywords);
                let span = format!("L{}", row.rownum);
                out.push_str(&format!("- {span} {header}\n"));
                if let Some(line) = preview
                    && line.trim() != header_raw.trim()
                {
                    out.push_str(&format!("  {}\n", build_preview(&line, 160)));
                }
                hits = hits.saturating_add(1);
            }
            continue;
        }

        // datememo：支持区域 recent/past/older；若未显式指定区域，则默认 recent7d。
        let region = parse_memo_date_region(&call_region_or_section(call))
            .unwrap_or(MemoDateRegion::Recent7d);
        let (mut region_start, mut region_end) = default_date_range_for_region(region);
        // 调用方显式指定日期范围则优先使用
        if start_date.is_some() || end_date.is_some() {
            region_start = start_date;
            region_end = end_date;
        }

        match region {
            MemoDateRegion::Recent7d => {
                // 一周内：支持单关键词，直接返回匹配记录（内容）。
                let limit = call.count.unwrap_or(80).clamp(1, 200);
                let (rows, label_total, _stats, min_date, max_date) =
                    db.check_by_keywords(kind, &keywords, region_start, region_end, limit)?;
                total_hits = total_hits.saturating_add(label_total);
                if let Some(date) = min_date.as_deref().and_then(parse_memory_date) {
                    global_min = Some(global_min.map_or(date, |cur| cur.min(date)));
                }
                if let Some(date) = max_date.as_deref().and_then(parse_memory_date) {
                    global_max = Some(global_max.map_or(date, |cur| cur.max(date)));
                }
                if rows.is_empty() {
                    continue;
                }
                let body = render_memo_rows(&rows);
                let (out_lines, out_chars) = pick_search_output_limits(call);
                let (body2, _saved) = maybe_export_text(
                    MEMORY_CACHE_DIR,
                    "datememo_recent",
                    &body,
                    out_lines,
                    out_chars,
                );

                if !out.is_empty() {
                    out.push('\n');
                }
                out.push_str(&format!("[{label} recent]"));
                out.push('\n');
                out.push_str(body2.trim_end());
                out.push('\n');
                hits = hits.saturating_add(rows.len());
                continue;
            }
            MemoDateRegion::Past1y | MemoDateRegion::Older => {
                // 过去/以前：必须多关键词；按“同一天内至少命中 2 个关键词”判定强命中。
                if keywords.len() < 2 {
                    return Ok(tool_format_error(
                        tool_tag,
                        "datememo 过去/以前区域检索至少需要 2 个关键词",
                    ));
                }
                let scan_limit = call.scan_limit.unwrap_or(5000).clamp(200, 50_000);
                let rows = db.fetch_entries_by_keywords_or(
                    kind,
                    &keywords,
                    region_start,
                    region_end,
                    scan_limit,
                )?;
                if rows.is_empty() {
                    continue;
                }
                use std::collections::BTreeMap;
                #[derive(Default)]
                struct DayAgg {
                    mask: u64,
                    samples: Vec<String>,
                }
                let mut by_day: BTreeMap<String, DayAgg> = BTreeMap::new();
                for (day, content) in rows {
                    let lower = content.to_ascii_lowercase();
                    let mut mask: u64 = 0;
                    for (i, kw) in keywords.iter().enumerate().take(60) {
                        if lower.contains(kw) {
                            mask |= 1u64 << i;
                        }
                    }
                    if mask == 0 {
                        continue;
                    }
                    let agg = by_day.entry(day).or_default();
                    agg.mask |= mask;
                    if agg.samples.len() < 3 {
                        let header = build_preview(&memo_row_header(&content), 160);
                        let mut line = header;
                        if let Some(p) = memo_row_preview(&content, &keywords) {
                            if !p.trim().is_empty() {
                                line.push_str(&format!(" | {}", build_preview(&p, 160)));
                            }
                        }
                        agg.samples.push(line);
                    }
                }
                let mut strong: Vec<(String, DayAgg)> = Vec::new();
                let mut weak: Vec<(String, DayAgg)> = Vec::new();
                for (day, agg) in by_day.into_iter() {
                    let distinct_keywords_hit = agg.mask.count_ones();
                    if distinct_keywords_hit >= 2 {
                        strong.push((day, agg));
                    } else {
                        weak.push((day, agg));
                    }
                }
                // 倒序（新日期优先）
                strong.sort_by(|a, b| b.0.cmp(&a.0));
                weak.sort_by(|a, b| b.0.cmp(&a.0));
                if strong.is_empty() && weak.is_empty() {
                    continue;
                }
                if !out.is_empty() {
                    out.push('\n');
                }
                let region_name = match region {
                    MemoDateRegion::Past1y => "past",
                    MemoDateRegion::Older => "older",
                    _ => "unknown",
                };
                out.push_str(&format!(
                    "[{label} {region_name} | db:{} | scan_limit:{}]",
                    shorten_path(&memo_db_path_display()),
                    scan_limit
                ));
                out.push('\n');
                if !strong.is_empty() {
                    out.push_str("[强命中：同日≥2关键词]\n");
                    for (day, agg) in strong.iter().take(20) {
                        out.push_str(&format!("- {day} kw_hits:{}\n", agg.mask.count_ones()));
                        for s in agg.samples.iter() {
                            out.push_str(&format!("  - {s}\n"));
                        }
                        hits = hits.saturating_add(1);
                    }
                }
                if strong.is_empty() && !weak.is_empty() {
                    out.push_str("[仅单关键词命中：返回出现日期]\n");
                    for (day, agg) in weak.iter().take(60) {
                        out.push_str(&format!("- {day} kw_hits:{}\n", agg.mask.count_ones()));
                    }
                }
                continue;
            }
        }
    }

    if out.trim().is_empty() {
        out = "未找到匹配".to_string();
    } else if total_hits > hits {
        out.push_str(&format!(
            "\n[已截断：展示 {hits}/{total_hits} 条，建议使用 start_date/end_date 缩小范围]"
        ));
        if let Some(min_date) = global_min {
            let max_date = global_max.unwrap_or(min_date);
            out.push_str(&format!(
                "\n[匹配日期范围: {} ~ {}]",
                min_date.format("%Y-%m-%d"),
                max_date.format("%Y-%m-%d")
            ));
        }
    }

    Ok(ToolOutcome {
        user_message: out,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | source:memory{} | hits:{} | total:{} | keywords:{}",
            started.elapsed().as_millis(),
            if used_sqlite {
                format!(" | db:{}", shorten_path(&memo_db_path_display()))
            } else {
                "".to_string()
            },
            hits,
            total_hits,
            keywords.join("/")
        )],
    })
}

struct SearchWithContextArgs<'a> {
    root: &'a str,
    pattern: &'a str,
    needles: &'a [String],
    max_matches: usize,
    started: Instant,
    timeout_secs: u64,
    pattern_preview: &'a str,
    out_lines: usize,
    out_chars: usize,
    context_lines: usize,
    context_files: usize,
    hits_per_file: usize,
    include_globs: &'a [String],
    exclude_globs: &'a [String],
    extra_exclude_dirs: &'a [String],
    include_re: &'a [Regex],
    exclude_re: &'a [Regex],
}

#[derive(Debug, Default)]
struct SearchScanStats {
    files_scanned: usize,
    skipped_large: usize,
    skipped_binary: usize,
    skipped_unreadable: usize,
    timed_out: bool,
    scan_limit_reached: bool,
    cap_reached: bool,
}

fn scan_keywords_and_hits(args: &SearchWithContextArgs<'_>) -> (Vec<SearchHit>, SearchScanStats) {
    let mut hits: Vec<SearchHit> = Vec::new();
    let mut stats = SearchScanStats::default();

    let root_path = Path::new(args.root);
    let mut stack: Vec<PathBuf> = vec![root_path.to_path_buf()];

    while let Some(dir) = stack.pop() {
        if args.timeout_secs > 0 && args.started.elapsed().as_secs() >= args.timeout_secs {
            stats.timed_out = true;
            break;
        }
        let Ok(rd) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in rd.flatten() {
            if args.timeout_secs > 0 && args.started.elapsed().as_secs() >= args.timeout_secs {
                stats.timed_out = true;
                break;
            }
            if hits.len() >= args.max_matches {
                stats.cap_reached = true;
                break;
            }
            if stats.files_scanned >= SEARCH_MAX_FILES_SCAN {
                stats.scan_limit_reached = true;
                break;
            }
            let path = entry.path();
            let name = entry.file_name().to_string_lossy().to_string();
            if path.is_dir() {
                if SEARCH_EXCLUDE_DIRS
                    .iter()
                    .any(|d| d.eq_ignore_ascii_case(name.as_str()))
                    || args
                        .extra_exclude_dirs
                        .iter()
                        .any(|d| d.eq_ignore_ascii_case(name.as_str()))
                {
                    continue;
                }
                stack.push(path);
                continue;
            }
            if !path.is_file() {
                continue;
            }
            stats.files_scanned = stats.files_scanned.saturating_add(1);
            if !search_path_allowed(
                &path,
                args.include_re,
                args.exclude_re,
                args.extra_exclude_dirs,
            ) {
                continue;
            }
            let size = fs::metadata(&path).map(|m| m.len() as usize).unwrap_or(0);
            if size > SEARCH_MAX_FILE_BYTES {
                stats.skipped_large = stats.skipped_large.saturating_add(1);
                continue;
            }
            if is_probably_binary_file(&path) {
                stats.skipped_binary = stats.skipped_binary.saturating_add(1);
                continue;
            }
            let file = match fs::File::open(&path) {
                Ok(f) => f,
                Err(_) => {
                    stats.skipped_unreadable = stats.skipped_unreadable.saturating_add(1);
                    continue;
                }
            };
            let mut reader = std::io::BufReader::new(file);
            let mut buf: Vec<u8> = Vec::new();
            let mut line_no = 0usize;
            loop {
                if args.timeout_secs > 0 && args.started.elapsed().as_secs() >= args.timeout_secs {
                    stats.timed_out = true;
                    break;
                }
                if hits.len() >= args.max_matches {
                    stats.cap_reached = true;
                    break;
                }
                buf.clear();
                let n = match reader.read_until(b'\n', &mut buf) {
                    Ok(n) => n,
                    Err(_) => {
                        stats.skipped_unreadable = stats.skipped_unreadable.saturating_add(1);
                        break;
                    }
                };
                if n == 0 {
                    break;
                }
                line_no = line_no.saturating_add(1);
                let line = String::from_utf8_lossy(&buf);
                let lower = line.to_ascii_lowercase();
                if !args.needles.iter().all(|kw| lower.contains(kw)) {
                    continue;
                }
                let clean = sanitize_search_line(line.trim_end_matches(['\n', '\r']));
                hits.push(SearchHit {
                    path: path.clone(),
                    line_no,
                    line: clean,
                });
            }
            if stats.timed_out || stats.cap_reached || stats.scan_limit_reached {
                break;
            }
        }
        if stats.timed_out || stats.cap_reached || stats.scan_limit_reached {
            break;
        }
    }

    (hits, stats)
}

fn build_context_pack(
    root: &str,
    mut hits: Vec<SearchHit>,
    context_lines: usize,
    context_files: usize,
    hits_per_file: usize,
    started: Instant,
    timed_out: bool,
    timeout_secs: u64,
    out_lines: usize,
    out_chars: usize,
    note_cap_reached: bool,
    note_scan_limit: bool,
    pattern_preview: &str,
) -> ToolOutcome {
    // 先按文件 + 行号排序，确保输出稳定可复现
    hits.sort_by(|a, b| a.path.cmp(&b.path).then(a.line_no.cmp(&b.line_no)));

    // file 去重 + 每文件 hit 限制
    let mut picked: Vec<(PathBuf, Vec<SearchHit>)> = Vec::new();
    let mut cur_path: Option<PathBuf> = None;
    let mut cur_hits: Vec<SearchHit> = Vec::new();
    for h in hits.into_iter() {
        if cur_path.as_ref().is_some_and(|p| *p == h.path) {
            cur_hits.push(h);
            continue;
        }
        if let Some(p) = cur_path.take() {
            picked.push((p, cur_hits));
        }
        cur_path = Some(h.path.clone());
        cur_hits = vec![h];
    }
    if let Some(p) = cur_path.take() {
        picked.push((p, cur_hits));
    }

    let too_many_files = picked.len() > context_files;
    picked.truncate(context_files);

    let mut skipped_large = 0usize;
    let mut skipped_binary = 0usize;
    let mut skipped_unreadable = 0usize;
    let mut blocks: Vec<String> = Vec::new();

    for (path, mut file_hits) in picked.into_iter() {
        file_hits.sort_by_key(|h| h.line_no);
        file_hits.dedup_by_key(|h| h.line_no);
        let too_many_hits = file_hits.len() > hits_per_file;
        file_hits.truncate(hits_per_file);

        let display = path.to_string_lossy().to_string();
        let shown = short_display_path(&display);
        let shown = if shown.is_empty() {
            shorten_path(&display)
        } else {
            shown
        };

        let size = fs::metadata(&path).map(|m| m.len() as usize).unwrap_or(0);
        if size > SEARCH_MAX_FILE_BYTES {
            skipped_large = skipped_large.saturating_add(1);
            blocks.push(format!(
                "=== {shown} ===\n(跳过：文件过大 > {SEARCH_MAX_FILE_BYTES} bytes)"
            ));
            continue;
        }
        if is_probably_binary_file(&path) {
            skipped_binary = skipped_binary.saturating_add(1);
            blocks.push(format!("=== {shown} ===\n(跳过：二进制文件)"));
            continue;
        }

        let file = match fs::File::open(&path) {
            Ok(f) => f,
            Err(_) => {
                skipped_unreadable = skipped_unreadable.saturating_add(1);
                blocks.push(format!("=== {shown} ===\n(跳过：无法读取)"));
                continue;
            }
        };
        let mut reader = std::io::BufReader::new(file);
        let mut lines: Vec<String> = Vec::new();
        let mut buf = String::new();
        while let Ok(n) = reader.read_line(&mut buf) {
            if n == 0 {
                break;
            }
            lines.push(buf.trim_end_matches(['\n', '\r']).to_string());
            buf.clear();
        }

        let total = lines.len().max(1);
        let mut ranges: Vec<(usize, usize)> = Vec::new();
        for h in file_hits.iter() {
            let start = h.line_no.saturating_sub(context_lines).max(1);
            let end = (h.line_no + context_lines).min(total);
            ranges.push((start, end));
        }
        ranges.sort_by_key(|(s, _)| *s);
        let mut merged: Vec<(usize, usize)> = Vec::new();
        for (s, e) in ranges.into_iter() {
            if let Some((ms, me)) = merged.last_mut() {
                if s <= *me + 1 {
                    *me = (*me).max(e);
                    *ms = (*ms).min(s);
                    continue;
                }
            }
            merged.push((s, e));
        }

        let mut block = String::new();
        block.push_str(&format!("=== {shown} ===\n"));
        if too_many_hits {
            block.push_str(&format!(
                "【命中过多】该文件仅展示前 {hits_per_file} 处命中上下文。\n"
            ));
        }
        let width = merged
            .iter()
            .map(|(_s, e)| e.to_string().len())
            .max()
            .unwrap_or(3)
            .max(3);
        for (s, e) in merged.into_iter() {
            block.push_str(&format!("({s}-{e})\n"));
            for ln in s..=e {
                let text = lines
                    .get(ln.saturating_sub(1))
                    .map(|s| sanitize_search_line(s))
                    .unwrap_or_default();
                block.push_str(&format!("{:>width$} | {}\n", ln, text, width = width));
            }
        }
        blocks.push(block.trim_end().to_string());
    }

    let mut notes: Vec<String> = Vec::new();
    if note_cap_reached {
        notes.push("【结果过多】已达到 count 上限，结果可能不完整。".to_string());
    }
    if note_scan_limit {
        notes.push(format!(
            "【扫描上限】已扫描 {SEARCH_MAX_FILES_SCAN} 个文件后提前终止（结果可能不完整）。"
        ));
    }
    if too_many_files {
        notes.push(format!(
            "【文件过多】仅展示前 {context_files} 个文件的命中上下文。"
        ));
    }
    if skipped_large > 0 {
        notes.push(format!("(跳过过大文件: {skipped_large})"));
    }
    if skipped_binary > 0 {
        notes.push(format!("(跳过二进制文件: {skipped_binary})"));
    }
    if skipped_unreadable > 0 {
        notes.push(format!("(跳过不可读文件: {skipped_unreadable})"));
    }

    let mut body = blocks.join("\n\n");
    if body.trim().is_empty() {
        body = "未找到匹配".to_string();
    }
    if !notes.is_empty() {
        body = format!("{}\n\n{}", notes.join("\n"), body.trim_end());
    }
    if timed_out {
        body = annotate_timeout(body, true, Some(timeout_secs));
    }

    let elapsed = started.elapsed();
    let total_bytes = body.as_bytes().len();
    let raw_lines = body.lines().count();
    let truncated_by_lines = raw_lines > out_lines;
    let truncated_by_chars = body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_ctx_{ts}_{pid}.log");
        if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
            Some(exported_meta(path, total_bytes, raw_lines))
        } else {
            None
        }
    } else {
        None
    };

    let body = if let Some(meta) = saved_meta.as_ref() {
        let preview = truncate_export_preview(&body);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_tool_payload(&body, out_lines, out_chars)
    };

    let status = status_label(0, timed_out);
    let mut log = format!(
        "状态:{status} | 耗时:{}ms | root:{} | engine:context | pattern:{pattern_preview} | ctx_lines:{context_lines}",
        elapsed.as_millis(),
        shorten_path(root),
    );
    if let Some(meta) = saved_meta.as_ref() {
        log.push_str(&format!(" | saved:{}", meta.path));
    }
    ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    }
}

fn run_search_with_context(args: SearchWithContextArgs<'_>) -> ToolOutcome {
    // AND 多关键词：走 Rust scanner（可控阈值 + 结果稳定）
    if args.needles.len() >= 2 {
        let (hits, stats) = scan_keywords_and_hits(&args);
        return build_context_pack(
            args.root,
            hits,
            args.context_lines,
            args.context_files,
            args.hits_per_file,
            args.started,
            stats.timed_out,
            args.timeout_secs,
            args.out_lines,
            args.out_chars,
            stats.cap_reached,
            stats.scan_limit_reached,
            args.pattern_preview,
        );
    }

    // 单 pattern：尽量复用 rg/grep 的输出语义（regex），再二次读取文件上下文
    let mut engine = "rg";
    let mut rg_args: Vec<String> = vec![
        "--line-number".to_string(),
        "--no-heading".to_string(),
        "-S".to_string(),
        "--max-count".to_string(),
        args.max_matches.to_string(),
        "--max-filesize".to_string(),
        SEARCH_MAX_FILESIZE.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for d in args.extra_exclude_dirs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!**/{d}/**"));
    }
    for g in args.include_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(g.to_string());
    }
    for g in args.exclude_globs.iter() {
        rg_args.push("--glob".to_string());
        rg_args.push(format!("!{g}"));
    }
    rg_args.push("--".to_string());
    rg_args.push(args.pattern.to_string());
    rg_args.push(args.root.to_string());

    let mut grep_args: Vec<String> = vec![
        "-RIn".to_string(),
        "--binary-files=without-match".to_string(),
        "-m".to_string(),
        args.max_matches.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push((*d).to_string());
    }
    for d in args.extra_exclude_dirs.iter() {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push(d.to_string());
    }
    for g in args.include_globs.iter() {
        grep_args.push(format!("--include={g}"));
    }
    for g in args.exclude_globs.iter() {
        grep_args.push(format!("--exclude={g}"));
    }
    grep_args.push("--".to_string());
    grep_args.push(args.pattern.to_string());
    grep_args.push(args.root.to_string());

    let rg_refs: Vec<&str> = rg_args.iter().map(|s| s.as_str()).collect();
    let grep_refs: Vec<&str> = grep_args.iter().map(|s| s.as_str()).collect();
    let mut out = run_command_output_with_optional_timeout("rg", &rg_refs, Some(args.timeout_secs));
    if out.is_err() {
        engine = "grep";
        out = run_command_output_with_optional_timeout("grep", &grep_refs, Some(args.timeout_secs));
    }
    let (code, stdout, stderr, timed_out) =
        out.unwrap_or_else(|_| (127, String::new(), "search failed".to_string(), false));

    let mut hits: Vec<SearchHit> = Vec::new();
    for l in stdout.lines() {
        let Some((p, ln, content)) = parse_search_hit_line(l) else {
            continue;
        };
        let path = resolve_search_hit_path(args.root, &p);
        if !search_path_allowed(
            &path,
            args.include_re,
            args.exclude_re,
            args.extra_exclude_dirs,
        ) {
            continue;
        }
        hits.push(SearchHit {
            path,
            line_no: ln,
            line: content,
        });
        if hits.len() >= args.max_matches {
            break;
        }
    }
    let no_match = hits.is_empty() && code == 1 && !timed_out;
    if no_match {
        return ToolOutcome {
            user_message: "未找到匹配".to_string(),
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | root:{} | engine:{engine} | pattern:{}",
                args.started.elapsed().as_millis(),
                shorten_path(args.root),
                args.pattern_preview
            )],
        };
    }

    // 若工具报错且无输出：返回失败信息（保留 stderr 关键摘要）
    if code != 0 && hits.is_empty() && !timed_out {
        let err = if stderr.trim().is_empty() {
            format!("search failed (exit:{code})")
        } else {
            build_preview(stderr.trim(), 200)
        };
        return ToolOutcome {
            user_message: format!("操作失败：{err}"),
            log_lines: vec![format!(
                "状态:{code} | 耗时:{}ms | root:{} | engine:{engine} | pattern:{}",
                args.started.elapsed().as_millis(),
                shorten_path(args.root),
                args.pattern_preview
            )],
        };
    }

    let note_cap_reached = hits.len() >= args.max_matches;
    build_context_pack(
        args.root,
        hits,
        args.context_lines,
        args.context_files,
        args.hits_per_file,
        args.started,
        timed_out,
        args.timeout_secs,
        args.out_lines,
        args.out_chars,
        note_cap_reached,
        false,
        args.pattern_preview,
    )
}

fn run_search_keywords_and(
    root: &str,
    needles: &[String],
    max_matches: usize,
    started: Instant,
    timeout_secs: u64,
    pattern_preview: &str,
    out_lines: usize,
    out_chars: usize,
    include_re: &[Regex],
    exclude_re: &[Regex],
    extra_exclude_dirs: &[String],
) -> ToolOutcome {
    let ctx_args = SearchWithContextArgs {
        root,
        pattern: "",
        needles,
        max_matches,
        started,
        timeout_secs,
        pattern_preview,
        out_lines,
        out_chars,
        context_lines: 1,
        context_files: 1,
        hits_per_file: 1,
        include_globs: &[],
        exclude_globs: &[],
        extra_exclude_dirs,
        include_re,
        exclude_re,
    };
    let (hits, stats) = scan_keywords_and_hits(&ctx_args);
    let mut results: Vec<String> = Vec::new();
    for h in hits.iter() {
        let display = h.path.to_string_lossy().to_string();
        let shown = short_display_path(&display);
        let shown = if shown.is_empty() {
            shorten_path(&display)
        } else {
            shown
        };
        let snippet = build_preview(h.line.trim(), 160);
        results.push(format!(
            "{shown}:{}:{snippet}",
            h.line_no,
            snippet = snippet
        ));
    }

    let elapsed = started.elapsed();
    let mut body = if results.is_empty() {
        "未找到匹配".to_string()
    } else {
        results.join("\n")
    };

    let mut notes: Vec<String> = Vec::new();
    if stats.cap_reached && max_matches > 0 {
        notes.push(format!(
            "【结果过多】已返回前 {max_matches} 条（结果可能不完整）。"
        ));
    }
    if stats.scan_limit_reached {
        notes.push(format!(
            "【扫描上限】已扫描 {SEARCH_MAX_FILES_SCAN} 个文件后提前终止（结果可能不完整）。"
        ));
    }
    if stats.skipped_large > 0 {
        notes.push(format!("(跳过过大文件: {})", stats.skipped_large));
    }
    if stats.skipped_binary > 0 {
        notes.push(format!("(跳过二进制文件: {})", stats.skipped_binary));
    }
    if stats.skipped_unreadable > 0 {
        notes.push(format!("(跳过不可读文件: {})", stats.skipped_unreadable));
    }
    if !notes.is_empty() {
        body = format!("{}\n{}", notes.join("\n"), body.trim_end());
    }
    if stats.timed_out {
        body = annotate_timeout(body, true, Some(timeout_secs));
    }

    let total_bytes = body.as_bytes().len();
    let raw_lines = body.lines().count();
    let truncated_by_lines = raw_lines > out_lines;
    let truncated_by_chars = body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_meta = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_kw_{ts}_{pid}.log");
        if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
            Some(exported_meta(path, total_bytes, raw_lines))
        } else {
            None
        }
    } else {
        None
    };

    let body = if let Some(meta) = saved_meta.as_ref() {
        let preview = truncate_export_preview(&body);
        let mut out = String::new();
        out.push_str(&format_exported_notice(meta));
        if !preview.trim().is_empty() {
            out.push_str("\n\n");
            out.push_str(preview.trim_end());
        }
        out
    } else {
        truncate_tool_payload(&body, out_lines, out_chars)
    };

    let status = status_label(0, stats.timed_out);
    let mut log = format!(
        "状态:{status} | 耗时:{}ms | root:{} | matches:{} | engine:and | pattern:{pattern_preview}",
        elapsed.as_millis(),
        shorten_path(root),
        results.len()
    );
    if stats.cap_reached {
        log.push_str(&format!(" | cap_reached:true | cap:{max_matches}"));
    }
    if stats.scan_limit_reached {
        log.push_str(&format!(
            " | scan_cap_reached:true | scan_cap:{SEARCH_MAX_FILES_SCAN}"
        ));
    }
    if stats.skipped_large > 0 {
        log.push_str(&format!(" | skipped_large:{}", stats.skipped_large));
    }
    if stats.skipped_binary > 0 {
        log.push_str(&format!(" | skipped_binary:{}", stats.skipped_binary));
    }
    if stats.skipped_unreadable > 0 {
        log.push_str(&format!(
            " | skipped_unreadable:{}",
            stats.skipped_unreadable
        ));
    }
    if let Some(meta) = saved_meta.as_ref() {
        log.push_str(&format!(" | saved:{}", meta.path));
    }

    ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    }
}

pub(crate) fn run_edit_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let path = if call.memory.unwrap_or(false) {
        let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
        let Some((label, path)) = resolve_memory_path_label(raw) else {
            return Ok(tool_format_error("edit_file", "memory 模式缺少有效 path"));
        };
        if label != "fastmemo" {
            return Ok(tool_format_error("edit_file", "memory 模式仅支持 fastmemo"));
        }
        ensure_memory_file(&label, &path)?;
        path
    } else {
        pick_path(call)?
    };
    let find = call.find.as_deref().unwrap_or("").to_string();
    if find.is_empty() {
        return Ok(ToolOutcome {
            user_message: "edit_file 需要 find".to_string(),
            log_lines: vec![],
        });
    }
    let replace = call.replace.as_deref().unwrap_or("").to_string();
    let count = call.count.unwrap_or(1);
    let started = Instant::now();
    let file_size = fs::metadata(&path).map(|m| m.len() as usize).unwrap_or(0);
    if file_size > EDIT_MAX_FILE_BYTES {
        return Ok(ToolOutcome {
            user_message: format!("安全限制：文件过大（>{EDIT_MAX_FILE_BYTES} bytes），拒绝编辑。"),
            log_lines: vec![],
        });
    }
    let original = fs::read_to_string(&path).with_context(|| format!("读取失败：{path}"))?;
    let matches = original.matches(&find).count();
    if matches == 0 {
        return Ok(ToolOutcome {
            user_message: "未找到匹配，未修改".to_string(),
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | matches:0",
                started.elapsed().as_millis(),
                shorten_path(&path)
            )],
        });
    }
    if count == 0 && matches > EDIT_MAX_MATCHES {
        return Ok(ToolOutcome {
            user_message: format!(
                "安全限制：匹配次数过多（>{EDIT_MAX_MATCHES}），请缩小范围或限定 count。"
            ),
            log_lines: vec![],
        });
    }
    let actual = if count == 0 {
        matches
    } else {
        matches.min(count)
    };
    let replaced = if count == 0 {
        original.replace(&find, &replace)
    } else {
        original.replacen(&find, &replace, count)
    };
    if replaced == original {
        return Ok(ToolOutcome {
            user_message: "未找到匹配，未修改".to_string(),
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | matches:0",
                started.elapsed().as_millis(),
                shorten_path(&path)
            )],
        });
    }
    fs::write(&path, replaced.as_bytes()).with_context(|| format!("写入失败：{path}"))?;
    let elapsed = started.elapsed();
    let add = replace.chars().count().saturating_mul(actual);
    let del = find.chars().count().saturating_mul(actual);

    let find_preview = build_preview(call.find.as_deref().unwrap_or(""), 120);
    let replace_preview = build_preview(call.replace.as_deref().unwrap_or(""), 120);
    let mut msg = String::new();
    msg.push_str(&format!("已编辑 {path}"));
    msg.push_str(&format!("\npath: {path}"));
    msg.push_str(&format!("\nfind: {find_preview}"));
    msg.push_str(&format!("\nreplace: {replace_preview}"));
    msg.push_str(&format!("\ncount: {actual}"));

    Ok(ToolOutcome {
        user_message: msg,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | path:{} | matches:{} | count:{} | delta:+{} -{}",
            elapsed.as_millis(),
            shorten_path(&path),
            matches,
            actual,
            add,
            del
        )],
    })
}

pub(crate) fn run_apply_patch(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let mut patch_text = call
        .patch
        .as_deref()
        .unwrap_or(call.content.as_deref().unwrap_or(call.input.as_str()))
        .to_string();
    if patch_text.trim().is_empty() {
        return Ok(ToolOutcome {
            user_message: "apply_patch 需要 patch".to_string(),
            log_lines: vec![],
        });
    }
    if patch_text.len() > PATCH_MAX_BYTES {
        return Ok(ToolOutcome {
            user_message: format!("安全限制：补丁过大（>{PATCH_MAX_BYTES} bytes）。"),
            log_lines: vec![],
        });
    }
    if !patch_text.ends_with("\n") {
        patch_text.push_str("\n");
    }
    if let Err(err) = validate_unified_patch(&patch_text) {
        return Ok(ToolOutcome {
            user_message: format!("补丁格式错误：{}（第{}行）", err.reason, err.line),
            log_lines: vec![format!("状态:format_error | line:{}", err.line)],
        });
    }

    let strip = if patch_text.contains("\n--- a/")
        || patch_text.starts_with("--- a/")
        || patch_text.contains("\n+++ b/")
        || patch_text.starts_with("+++ b/")
    {
        1
    } else {
        0
    };

    let started = Instant::now();
    let cwd_exec = if let Some(cwd) = call
        .cwd
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty())
    {
        expand_cwd_alias(cwd)
    } else {
        default_workdir()
    };
    let cwd_display = short_cwd_display(&cwd_exec);
    let (add, del) = count_patch_changes(&patch_text);
    // 先 dry-run：避免 patch 在失败时“部分应用”导致工作区混乱。
    let (pre_code, pre_stdout, pre_stderr, pre_timed_out) =
        run_patch_command(&patch_text, strip, true, &cwd_exec)?;
    let pre_elapsed = started.elapsed();
    let pre_body = annotate_timeout(
        truncate_command_output(collect_output(&pre_stdout, &pre_stderr)),
        pre_timed_out,
        None,
    );
    let pre_report = parse_patch_report(&pre_body);

    if pre_timed_out {
        return Ok(ToolOutcome {
            user_message: "补丁超时（未应用）".to_string(),
            log_lines: vec![format!(
                "状态:timeout | 耗时:{}ms | cwd:{} | strip:-p{strip} | delta_lines:+{} -{} | result:timeout",
                pre_elapsed.as_millis(),
                cwd_display,
                add,
                del
            )],
        });
    }
    if pre_code != 0 || pre_report.failed {
        return Ok(ToolOutcome {
            user_message: if pre_body.trim().is_empty() {
                "补丁失败（未应用）".to_string()
            } else {
                format!("补丁失败（未应用）\n{pre_body}")
            },
            log_lines: vec![format!(
                "状态:fail | 耗时:{}ms | cwd:{} | strip:-p{strip} | delta_lines:+{} -{} | result:precheck_fail",
                pre_elapsed.as_millis(),
                cwd_display,
                add,
                del
            )],
        });
    }
    // 自动化策略：
    // - fuzz：拒绝（未应用），因为上下文未精确匹配，风险更高。
    // - offset：允许（上下文匹配但行号偏移）。
    if pre_report.fuzz {
        return Ok(ToolOutcome {
            user_message: if pre_body.trim().is_empty() {
                "补丁命中不精确（fuzz，未应用）".to_string()
            } else {
                format!("补丁命中不精确（fuzz，未应用）\n{pre_body}")
            },
            log_lines: vec![format!(
                "状态:fail | 耗时:{}ms | cwd:{} | strip:-p{strip} | delta_lines:+{} -{} | result:fuzz_reject",
                pre_elapsed.as_millis(),
                cwd_display,
                add,
                del
            )],
        });
    }

    // 正式应用
    let (code, stdout, stderr, timed_out) = run_patch_command(&patch_text, strip, false, &cwd_exec)?;
    let elapsed = started.elapsed();
    let body = annotate_timeout(
        truncate_command_output(collect_output(&stdout, &stderr)),
        timed_out,
        None,
    );
    let status = status_label(code, timed_out);
    let report = parse_patch_report(&body);
    let success = code == 0 && !timed_out;
    let result_tag = if timed_out {
        "timeout"
    } else if success && report.fuzz {
        // 理论上 dry-run 已拦截 fuzz；保留兜底标记，避免静默。
        "ok_fuzz"
    } else if success && report.offset {
        "ok_offset"
    } else if success && !report.failed {
        "ok"
    } else {
        "fail"
    };

    let summary = if timed_out {
        "补丁超时"
    } else if result_tag == "ok_fuzz" {
        "补丁成功（fuzz）"
    } else if result_tag == "ok_offset" {
        "补丁成功（offset）"
    } else if result_tag == "ok" {
        "补丁成功"
    } else {
        "补丁失败"
    };
    let user_message = if body.trim().is_empty() {
        summary.to_string()
    } else {
        format!("{summary}\n{body}")
    };

    Ok(ToolOutcome {
        user_message,
        log_lines: vec![format!(
            "状态:{status} | 耗时:{}ms | cwd:{} | strip:-p{strip} | delta_lines:+{} -{} | result:{}",
            elapsed.as_millis(),
            cwd_display,
            add,
            del,
            result_tag
        )],
    })
}

fn run_memory_check(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    run_search_memory_like("memory_check", call)
}

fn run_read_memory_like(tool_tag: &str, call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let Some((label, path)) = resolve_memory_path_label(raw) else {
        return Ok(tool_format_error(tool_tag, "缺少有效 path"));
    };
    let started = Instant::now();
    let use_slice = call.start_line.is_some() || call.max_lines.is_some();
    let has_date = !call.date_start.as_deref().unwrap_or("").trim().is_empty()
        || !call.date_end.as_deref().unwrap_or("").trim().is_empty();
    if has_date && use_slice {
        return Ok(tool_format_error(tool_tag, "日期检索与行区间不可同时使用"));
    }
    if label == "datememo" || label == "metamemo" {
        let kind = if label == "datememo" {
            MemoKind::Date
        } else {
            MemoKind::Meta
        };
        // sqlite memo：read 仅支持“精确到单日”的读取（便于稳定检索与导出），不支持 start_line/max_lines。
        if use_slice {
            return Ok(tool_format_error(
                tool_tag,
                "sqlite 记忆读取不支持 start_line/max_lines；请使用 date_start（精确日期）",
            ));
        }
        // metamemo 要求显式标记 section=day（当天）
        if label == "metamemo" {
            let reg = call_region_or_section(call);
            let sec = reg.trim();
            let sec_ok = matches!(sec, "day" | "当天" | "date" | "当日" | "日" | "天");
            if !sec_ok {
                return Ok(tool_format_error(
                    tool_tag,
                    "metamemo 需要 region=day（当天）",
                ));
            }
        }
        if !has_date {
            return Ok(tool_format_error(
                tool_tag,
                "sqlite 记忆读取需要 date_start/date_end（精确日期）",
            ));
        }
        let (start_date, end_date) = match parse_memory_date_range(
            call.date_start.as_deref().unwrap_or(""),
            call.date_end.as_deref().unwrap_or(""),
        ) {
            Ok(range) => range,
            Err(reason) => return Ok(tool_format_error(tool_tag, reason)),
        };
        let Some(day) = start_date.or(end_date) else {
            return Ok(tool_format_error(
                tool_tag,
                "sqlite 记忆读取需要 date_start/date_end（精确日期）",
            ));
        };
        let end = end_date.unwrap_or(day);
        if day != end {
            return Ok(tool_format_error(
                tool_tag,
                "sqlite 记忆读取仅支持精确到单日",
            ));
        }
        let db = MemoDb::open_default()?;
        let (rows, stats) = db.read_by_date(kind, Some(day), Some(day))?;
        if rows.is_empty() {
            return Ok(ToolOutcome {
                user_message: "未找到匹配日期的条目".to_string(),
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | source:sqlite | db:{} | table:{} | day:{} | rows:{} | chars:{} | hits:0",
                    started.elapsed().as_millis(),
                    shorten_path(&memo_db_path_display()),
                    kind.table(),
                    day.format("%Y-%m-%d"),
                    stats.total_rows,
                    stats.total_chars
                )],
            });
        }
        let body = render_memo_rows(&rows);
        let (out_lines, out_chars) = pick_search_output_limits(call);
        let (body2, saved) = maybe_export_text(
            MEMORY_CACHE_DIR,
            if label == "datememo" {
                "datememo_day"
            } else {
                "metamemo_day"
            },
            &body,
            out_lines,
            out_chars,
        );
        let mut log = format!(
            "状态:0 | 耗时:{}ms | source:sqlite | db:{} | table:{} | day:{} | rows:{} | chars:{} | hits:{}",
            started.elapsed().as_millis(),
            shorten_path(&memo_db_path_display()),
            kind.table(),
            day.format("%Y-%m-%d"),
            stats.total_rows,
            stats.total_chars,
            rows.len()
        );
        if let Some(meta) = saved.as_ref() {
            log.push_str(&format!(" | saved:{}", meta.path));
        }
        return Ok(ToolOutcome {
            user_message: body2,
            log_lines: vec![log],
        });
    }

    ensure_memory_file(&label, &path)?;
    if has_date {
        if label != "datememo" && label != "metamemo" {
            return Ok(tool_format_error(
                tool_tag,
                "日期检索仅支持 datememo/metamemo",
            ));
        }
        let (start_date, end_date) = match parse_memory_date_range(
            call.date_start.as_deref().unwrap_or(""),
            call.date_end.as_deref().unwrap_or(""),
        ) {
            Ok(range) => range,
            Err(reason) => return Ok(tool_format_error(tool_tag, reason)),
        };
        let text = fs::read_to_string(&path).unwrap_or_default();
        let total_lines = text.lines().count();
        let total_chars = text.chars().count();
        let mut out = String::new();
        let mut hits = 0usize;
        for block in collect_memory_blocks(&text) {
            let Some(date) = extract_block_date(&block) else {
                continue;
            };
            if let Some(start) = start_date
                && date < start
            {
                continue;
            }
            if let Some(end) = end_date
                && date > end
            {
                continue;
            }
            hits = hits.saturating_add(1);
            if !out.is_empty() {
                out.push('\n');
            }
            if block.start_line == block.end_line {
                out.push_str(&format!("[L{}]\n", block.start_line));
            } else {
                out.push_str(&format!("[L{}-{}]\n", block.start_line, block.end_line));
            }
            for line in &block.lines {
                out.push_str(line);
                out.push('\n');
            }
        }
        if out.trim().is_empty() {
            return Ok(ToolOutcome {
                user_message: "未找到匹配日期范围内的条目".to_string(),
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{} | hits:0",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    total_lines,
                    total_chars
                )],
            });
        }
        let clipped = truncate_tool_payload(&out, TOOL_OUTPUT_MAX_LINES, TOOL_OUTPUT_MAX_CHARS);
        return Ok(ToolOutcome {
            user_message: clipped,
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{} | hits:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                total_lines,
                total_chars,
                hits
            )],
        });
    }
    let start_line = call.start_line.unwrap_or(1).max(1);
    let max_lines = call
        .max_lines
        .unwrap_or(TOOL_OUTPUT_MAX_LINES)
        .clamp(1, READ_MAX_LINES_CAP);
    let end_line = start_line.saturating_add(max_lines.saturating_sub(1));
    let mut total_lines = 0usize;
    let mut total_chars = 0usize;
    let mut collected: Vec<String> = Vec::new();
    let mut collecting = false;
    let mut exceeded = false;

    let file = fs::File::open(&path).with_context(|| format!("读取失败：{path}"))?;
    let reader = std::io::BufReader::new(file);
    for line in reader.lines() {
        let line = line.unwrap_or_default();
        total_lines = total_lines.saturating_add(1);
        total_chars = total_chars.saturating_add(line.chars().count());
        if use_slice {
            if total_lines >= start_line && total_lines <= end_line {
                collected.push(line);
                collecting = true;
            }
            continue;
        }
        if label == "fastmemo" || label == "scopememo" {
            collected.push(line);
            collecting = true;
            continue;
        }
        if exceeded {
            continue;
        }
        if total_lines > TOOL_OUTPUT_MAX_LINES || total_chars > TOOL_OUTPUT_MAX_CHARS {
            exceeded = true;
            collected.clear();
            collecting = false;
            continue;
        }
        collected.push(line);
        collecting = true;
    }

    if use_slice {
        if start_line > total_lines {
            return Ok(ToolOutcome {
                user_message: format!(
                    "起始行超出范围：start_line={start_line}，总行数={total_lines}"
                ),
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{}",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    total_lines,
                    total_chars
                )],
            });
        }
        let end = end_line.min(total_lines);
        let slice = collected.join("\n");
        let note = format!("\n\n[仅展示第 {start_line}-{end} 行，共 {total_lines} 行]");
        return Ok(ToolOutcome {
            user_message: format!("{slice}{note}"),
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{} | range:{}-{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                total_lines,
                total_chars,
                start_line,
                end
            )],
        });
    }

    if (label == "datememo" || label == "metamemo") && !collecting {
        let msg = format!(
            "文件过大（{total_lines} 行 / {total_chars} 字符），请先 memory_check 后分段读取。"
        );
        return Ok(ToolOutcome {
            user_message: msg,
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                total_lines,
                total_chars
            )],
        });
    }

    let mut body = collected.join("\n");
    if body.trim().is_empty() {
        body = "(空文件)".to_string();
    }
    let clipped = truncate_tool_payload(&body, TOOL_OUTPUT_MAX_LINES, TOOL_OUTPUT_MAX_CHARS);
    Ok(ToolOutcome {
        user_message: clipped,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{}",
            started.elapsed().as_millis(),
            shorten_path(&path),
            total_lines,
            total_chars
        )],
    })
}

fn run_memory_read(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    run_read_memory_like("memory_read", call)
}

fn run_memory_edit(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let Some((label, path)) = resolve_memory_path_label(raw) else {
        return Ok(tool_format_error("memory_edit", "缺少有效 path"));
    };
    if label != "fastmemo" && label != "scopememo" {
        return Ok(tool_format_error("memory_edit", "仅支持 fastmemo/scopememo"));
    }
    ensure_memory_file(&label, &path)?;
    let section_raw = call.section.as_deref().unwrap_or("").trim();
    let content_raw = call.content.as_deref().unwrap_or("").trim();
    if !section_raw.is_empty() && !content_raw.is_empty() {
        let Some(section) = normalize_fastmemo_section(section_raw) else {
            return Ok(tool_format_error("memory_edit", "未知 section"));
        };
        let started = Instant::now();
        let mut bullets: Vec<String> = Vec::new();
        for line in content_raw.lines() {
            let t = compact_ws_inline(line.trim());
            if t.is_empty() {
                continue;
            }
            if t.starts_with("- ") {
                bullets.push(t.to_string());
            } else {
                bullets.push(format!("- {t}"));
            }
            if bullets.len() >= 10 {
                break;
            }
        }
        if bullets.is_empty() {
            return Ok(tool_format_error("memory_edit", "content 为空"));
        }
        let bullet_len = bullets.len();
        let mut lines: Vec<String> = fs::read_to_string(&path)
            .unwrap_or_default()
            .lines()
            .map(|s| s.to_string())
            .collect();
        let header_idx = ensure_fastmemo_section(&mut lines, section);
        let mut next_header_idx = None;
        for (idx, line) in lines.iter().enumerate().skip(header_idx + 1) {
            let trimmed = line.trim();
            if trimmed.starts_with('[') && trimmed.contains(']') {
                next_header_idx = Some(idx);
                break;
            }
        }
        let end_idx = next_header_idx.unwrap_or(lines.len());
        let replace_start = (header_idx + 1).min(lines.len());
        // 移除旧内容（直到下一个 section header），再插入新 bullet 列表，并保留一个空行分隔。
        lines.splice(replace_start..end_idx, {
            let mut injected = bullets;
            injected.push(String::new());
            injected
        });
        let mut out = lines.join("\n");
        if !out.ends_with('\n') {
            out.push('\n');
        }
        fs::write(&path, &out).with_context(|| format!("写入失败：{path}"))?;
        return Ok(ToolOutcome {
            user_message: format!("已更新 fastmemo section：{section}"),
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | section:{} | lines:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                section,
                bullet_len
            )],
        });
    }

    let mut cloned = call.clone();
    cloned.path = Some(path);
    cloned.input.clear();
    if cloned.count.unwrap_or(1) == 0 {
        return Ok(tool_format_error("memory_edit", "不支持 count=0"));
    }
    if cloned.count.unwrap_or(1) > 20 {
        return Ok(tool_format_error("memory_edit", "count 过大，请缩小范围"));
    }
    if cloned.count.is_none() {
        cloned.count = Some(1);
    }
    run_edit_file(&cloned)
}

fn normalize_section_name(raw: &str) -> String {
    let mut s = raw.trim().to_string();
    if s.starts_with('[')
        && let Some(end) = s.find(']')
    {
        s = s[1..end].to_string();
    }
    if let Some(idx) = s.find('：') {
        s.truncate(idx);
    } else if let Some(idx) = s.find(':') {
        s.truncate(idx);
    }
    s.trim().to_string()
}

fn normalize_fastmemo_section(raw: &str) -> Option<&'static str> {
    let s = normalize_section_name(raw);
    if s.is_empty() {
        return None;
    }
    let key = s.to_ascii_lowercase();
    match key.as_str() {
        // v3 canonical（四区）
        "自我感知" | "自我" | "人格" | "风格" | "style" | "self" => Some("自我感知"),
        "用户感知" | "用户" | "画像" | "user" => Some("用户感知"),
        "环境感知" | "环境" | "env" | "system" => Some("环境感知"),
        "事件感知" | "事件" | "事件池" | "event" | "events" | "history" | "动态" | "动态池"
        | "dynamic" => Some("事件感知"),
        // v1/v2 -> v3 mapping (best-effort)
        "动态成长人格" => Some("自我感知"),
        "用户感知画像" => Some("用户感知"),
        "人生旅程" | "历史感知" | "里程碑" => Some("事件感知"),
        "动态context池" | "动态上下文池" | "动态摘要池" | "contextpool" | "ctx_pool"
        | "ctxpool" => Some("事件感知"),
        "淡化池" => None,
        _ => None,
    }
}

fn fastmemo_section_headers_v2() -> [&'static str; 5] {
    [
        "自我感知",
        "用户感知",
        "环境感知",
        "历史感知",
        "动态context池",
    ]
}

fn fastmemo_section_headers_v3() -> [&'static str; 4] {
    ["自我感知", "用户感知", "环境感知", "事件感知"]
}

fn is_fastmemo_v2_struct(text: &str) -> bool {
    let mut first_non_empty = "";
    for line in text.lines() {
        let t = line.trim();
        if !t.is_empty() {
            first_non_empty = t;
            break;
        }
    }
    if !first_non_empty.starts_with("fastmemo v2") {
        return false;
    }
    for sec in fastmemo_section_headers_v2() {
        let header = format!("[{sec}]");
        let count = text.lines().filter(|l| l.trim() == header).count();
        if count != 1 {
            return false;
        }
    }
    true
}

fn is_fastmemo_v3_struct(text: &str) -> bool {
    let mut first_non_empty = "";
    for line in text.lines() {
        let t = line.trim();
        if !t.is_empty() {
            first_non_empty = t;
            break;
        }
    }
    if !first_non_empty.starts_with("fastmemo v3") {
        return false;
    }
    for sec in fastmemo_section_headers_v3() {
        let header = format!("[{sec}]");
        let count = text.lines().filter(|l| l.trim() == header).count();
        if count != 1 {
            return false;
        }
    }
    true
}

fn fastmemo_has_unknown_sections(text: &str) -> bool {
    for line in text.lines() {
        let trimmed = line.trim();
        if !(trimmed.starts_with('[') && trimmed.contains(']')) {
            continue;
        }
        let name = normalize_section_name(trimmed);
        if name.is_empty() {
            continue;
        }
        if normalize_fastmemo_section(&name).is_none()
            && normalize_fastmemo_section(trimmed).is_none()
        {
            return true;
        }
    }
    false
}

fn build_fastmemo_v3_template(
    now: &str,
    seed: &std::collections::HashMap<&'static str, Vec<String>>,
) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "fastmemo v3 | max_chars: 1800 | updated: {now}\n\n"
    ));
    for name in fastmemo_section_headers_v3() {
        out.push_str(&format!("[{name}]\n"));
        if let Some(items) = seed.get(name) {
            for line in items {
                let t = line.trim();
                if t.is_empty() {
                    continue;
                }
                if t.starts_with("- ") {
                    out.push_str(t);
                } else {
                    out.push_str("- ");
                    out.push_str(t);
                }
                out.push('\n');
            }
        }
        out.push('\n');
    }
    out
}

fn parse_fastmemo_sections(raw: &str) -> std::collections::HashMap<String, Vec<String>> {
    let mut map: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    let mut current: Option<String> = None;
    for line in raw.lines() {
        let trimmed = line.trim_end();
        if trimmed.trim_start().starts_with('[') && trimmed.contains(']') {
            let name = normalize_section_name(trimmed);
            if !name.is_empty() {
                current = Some(name);
                map.entry(current.clone().unwrap()).or_default();
            }
            continue;
        }
        let Some(section) = current.clone() else {
            continue;
        };
        let t = trimmed.trim();
        if t.starts_with("- ") {
            map.entry(section).or_default().push(t.to_string());
        } else if !t.is_empty() && (t.starts_with('•') || t.starts_with('*')) {
            let body = t.trim_start_matches(['•', '*']).trim();
            if !body.is_empty() {
                map.entry(section).or_default().push(format!("- {body}"));
            }
        }
    }
    map
}

fn migrate_fastmemo_to_v2(raw: &str) -> String {
    let sections = parse_fastmemo_sections(raw);
    let mut seed: std::collections::HashMap<&'static str, Vec<String>> =
        std::collections::HashMap::new();
    for (name, items) in sections {
        let Some(dst) = normalize_fastmemo_section(&name) else {
            continue;
        };
        let dst_items = seed.entry(dst).or_default();
        for item in items {
            dst_items.push(item);
        }
    }

    // 结构自愈：
    // - 每区去重（保留最后一次出现）
    // - 每区最多保留最后 10 条（更贴近“近期有效信息”）
    for name in fastmemo_section_headers_v3() {
        let items = seed.entry(name).or_default();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut uniq_rev: Vec<String> = Vec::new();
        for line in items.iter().rev() {
            let t = compact_ws_inline(line.trim());
            if t.is_empty() {
                continue;
            }
            let key = t.to_ascii_lowercase();
            if seen.insert(key) {
                uniq_rev.push(t);
            }
        }
        uniq_rev.reverse();
        *items = uniq_rev;
        if items.len() > 10 {
            let start = items.len().saturating_sub(10);
            *items = items[start..].to_vec();
        }
    }
    let now = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    build_fastmemo_v3_template(&now, &seed)
}

fn parse_heartbeat_minutes(call: &ToolCall) -> Option<usize> {
    if let Some(v) = call.heartbeat_minutes {
        return Some(v);
    }
    let raw = call.input.trim();
    if raw.is_empty() {
        return None;
    }
    raw.trim_end_matches(['m', 'M']).parse::<usize>().ok()
}

fn is_valid_heartbeat_minutes(value: usize) -> bool {
    matches!(value, 5 | 10 | 30 | 60)
}

fn build_memory_add_message(path: &str, content: &str) -> String {
    let snippet =
        truncate_tool_payload(content, MEMORY_ADD_PREVIEW_LINES, MEMORY_ADD_PREVIEW_CHARS);
    let mut msg = format!("已追加 {path}");
    if !snippet.trim().is_empty() {
        msg.push_str("\ncontent:\n");
        msg.push_str(&snippet);
    }
    msg
}

fn compact_ws_inline(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn normalize_fastmemo_content_to_bullets(content: &str) -> Vec<String> {
    let clean = content.trim();
    if clean.is_empty() {
        return Vec::new();
    }
    let merged = compact_ws_inline(clean);
    if merged.is_empty() {
        return Vec::new();
    }
    vec![format!("- {}", merged)]
}

fn ensure_fastmemo_section(lines: &mut Vec<String>, section: &str) -> usize {
    for (idx, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.contains(']') {
            if normalize_fastmemo_section(trimmed) == Some(section) {
                return idx;
            }
        }
    }
    if !lines.is_empty() && !lines.last().unwrap_or(&String::new()).trim().is_empty() {
        lines.push(String::new());
    }
    lines.push(format!("[{section}]"));
    lines.push(String::new());
    lines.len().saturating_sub(2)
}

fn fastmemo_section_counts(text: &str) -> std::collections::HashMap<&'static str, usize> {
    let mut counts: std::collections::HashMap<&'static str, usize> =
        std::collections::HashMap::new();
    let mut current: Option<&'static str> = None;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') && trimmed.contains(']') {
            current = normalize_fastmemo_section(trimmed);
            continue;
        }
        if let Some(sec) = current {
            if trimmed.starts_with("- ") {
                *counts.entry(sec).or_insert(0) += 1;
            }
        }
    }
    for sec in fastmemo_section_headers_v3() {
        counts.entry(sec).or_insert(0);
    }
    counts
}

fn run_memory_add(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let Some((label, path)) = resolve_memory_path_label(raw) else {
        return Ok(tool_format_error("memory_add", "缺少有效 path"));
    };
    if label != "fastmemo" && label != "scopememo" {
        return Ok(tool_format_error("memory_add", "仅支持 fastmemo/scopememo"));
    }
    let content = call.content.as_deref().unwrap_or("").trim();
    if content.is_empty() {
        return Ok(tool_format_error("memory_add", "缺少 content"));
    }
    let started = Instant::now();

    ensure_memory_file(&label, &path)?;
    let section_raw = call.section.as_deref().unwrap_or("").trim();
    let Some(section) = normalize_fastmemo_section(section_raw) else {
        return Ok(tool_format_error("memory_add", "fastmemo 需要 section"));
    };
    let mut lines: Vec<String> = fs::read_to_string(&path)
        .unwrap_or_default()
        .lines()
        .map(|s| s.to_string())
        .collect();
    let header_idx = ensure_fastmemo_section(&mut lines, section);
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
    let content_lines = normalize_fastmemo_content_to_bullets(content);
    if content_lines.is_empty() {
        return Ok(tool_format_error("memory_add", "content 为空"));
    }
    let mut injected: Vec<String> = Vec::new();
    injected.extend(content_lines);
    if insert_idx < lines.len() && !lines[insert_idx].trim().is_empty() {
        injected.push(String::new());
    }
    lines.splice(insert_idx..insert_idx, injected);
    let mut out = lines.join("\n");
    if !out.ends_with('\n') {
        out.push('\n');
    }
    fs::write(&path, &out).with_context(|| format!("写入失败：{path}"))?;

    let counts = fastmemo_section_counts(&out);
    let pending = counts.values().any(|v| *v >= 10);
    let mut msg = build_memory_add_message(&path, content);
    if pending {
        msg.push_str("\n\n[fastmemo 达到阈值：已计划压缩（DOG）]");
    }
    Ok(ToolOutcome {
        user_message: msg,
        log_lines: vec![
            format!(
                "状态:0 | 耗时:{}ms | path:{} | section:{} | chars:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                section,
                content.chars().count()
            ),
            format!(
                "fastmemo_counts | 自我:{} | 用户:{} | 环境:{} | 事件:{}",
                counts.get("自我感知").copied().unwrap_or(0),
                counts.get("用户感知").copied().unwrap_or(0),
                counts.get("环境感知").copied().unwrap_or(0),
                counts.get("事件感知").copied().unwrap_or(0),
            ),
            format!("fastmemo_compact_pending:{}", if pending { 1 } else { 0 }),
        ],
    })
}

fn run_mind_msg(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let target = resolve_mind_target(call).unwrap_or_else(|| "unknown".to_string());
    let content = resolve_mind_message(call);
    let mut output = String::new();
    output.push_str("已发送\n");
    output.push_str("message:\n");
    output.push_str(content.trim());
    Ok(ToolOutcome {
        user_message: output.trim_end().to_string(),
        log_lines: vec![format!("to:{target}")],
    })
}

fn run_system_config(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let Some(minutes) = parse_heartbeat_minutes(call) else {
        return Ok(tool_format_error("system_config", "缺少 heartbeat_minutes"));
    };
    if !is_valid_heartbeat_minutes(minutes) {
        return Ok(tool_format_error(
            "system_config",
            "心跳仅支持 5/10/30/60 分钟",
        ));
    }
    let path = system_config_path();
    let started = Instant::now();
    let mut value: Value = if path.exists() {
        let text = fs::read_to_string(&path)
            .with_context(|| format!("读取系统配置失败：{}", path.display()))?;
        serde_json::from_str(&text).unwrap_or_else(|_| Value::Object(Default::default()))
    } else {
        Value::Object(Default::default())
    };
    let obj = value
        .as_object_mut()
        .ok_or_else(|| anyhow!("系统配置不是对象"))?;
    obj.insert("heartbeat_minutes".to_string(), Value::from(minutes));
    let output = serde_json::to_string_pretty(&value)?;
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建目录失败：{}", parent.display()))?;
    }
    fs::write(&path, output.as_bytes())
        .with_context(|| format!("写入系统配置失败：{}", path.display()))?;
    Ok(ToolOutcome {
        user_message: format!("已更新系统配置：心跳 {minutes}m"),
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | path:{} | heartbeat:{}m",
            started.elapsed().as_millis(),
            shorten_path(&path.to_string_lossy()),
            minutes
        )],
    })
}

pub(crate) fn ensure_memory_file(label: &str, path: &str) -> anyhow::Result<()> {
    if label != "fastmemo" && label != "scopememo" {
        MemoDb::open_default().ok();
        return Ok(());
    }
    if fs::metadata(path).is_ok() {
        // 结构自愈：任何“非规范 v2”都尝试重建为 v2（尽量保留原有 bullet）。
        let text = fs::read_to_string(path).unwrap_or_default();
        if text.trim().is_empty() {
            let now = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
            let template = build_fastmemo_v3_template(&now, &std::collections::HashMap::new());
            fs::write(path, template).with_context(|| format!("初始化 fastmemo 失败：{path}"))?;
            return Ok(());
        }
        let looks_v1 =
            text.trim_start().starts_with("fastmemo v1") || text.contains("[动态成长人格]");
        let is_v3 = is_fastmemo_v3_struct(&text);
        let is_v2 = is_fastmemo_v2_struct(&text);
        // 兼容：v2 结构将被迁移到 v3；v3 结构保持不动（除非发现未知 section）。
        let needs_repair =
            looks_v1 || is_v2 || (!is_v3 && !is_v2) || fastmemo_has_unknown_sections(&text);
        if needs_repair {
            // 保护：修复前做一次备份（避免结构修复导致信息丢失）。
            let now = Local::now().format("%Y%m%d-%H%M%S").to_string();
            let backup = format!("{path}.bak-{now}");
            let _ = fs::copy(path, &backup);
            let migrated = migrate_fastmemo_to_v2(&text);
            fs::write(path, migrated).with_context(|| format!("修复 fastmemo 失败：{path}"))?;
        }
        return Ok(());
    }
    if let Some(dir) = Path::new(path).parent() {
        fs::create_dir_all(dir).ok();
    }
    let now = Local::now().format("%Y-%m-%d %H:%M:%S");
    let header = match label {
        "fastmemo" | "scopememo" => {
            build_fastmemo_v3_template(&now.to_string(), &std::collections::HashMap::new())
        }
        "datememo" => format!(
            "# datememo v2 | created_at: {} | format: date time | speaker | message\n\n",
            now
        ),
        "metamemo" => format!(
            "# metamemo v2 | created_at: {} | format: date time | speaker | message\n\n",
            now
        ),
        _ => String::new(),
    };
    if header.trim().is_empty() {
        return Ok(());
    }
    fs::write(path, header).with_context(|| format!("初始化记忆文件失败：{path}"))?;
    Ok(())
}

fn extract_skills_section(raw: &str, category: &str) -> Option<String> {
    let mut collecting = false;
    let mut out: Vec<String> = Vec::new();
    for line in raw.lines() {
        if let Some(rest) = line.strip_prefix("## ") {
            let title = rest.trim();
            if collecting {
                break;
            }
            if title.eq_ignore_ascii_case(category) || title == category {
                collecting = true;
                out.push(line.to_string());
                continue;
            }
        }
        if collecting {
            out.push(line.to_string());
        }
    }
    if out.is_empty() {
        None
    } else {
        Some(out.join("\n").trim_end().to_string())
    }
}

fn run_skills_mcp(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let category = call.category.as_deref().unwrap_or(call.input.trim()).trim();
    let category = if category.is_empty() {
        "编程类"
    } else {
        category
    };
    let started = Instant::now();
    let path = Path::new("prompt/Agentskills.md");
    let raw = fs::read_to_string(path)
        .with_context(|| format!("读取 skills 失败：{}", path.display()))?;
    let section =
        extract_skills_section(&raw, category).unwrap_or_else(|| format!("未找到类别：{category}"));
    let clipped = truncate_tool_payload(&section, TOOL_OUTPUT_MAX_LINES, TOOL_OUTPUT_MAX_CHARS);
    Ok(ToolOutcome {
        user_message: clipped,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | category:{} | path:{}",
            started.elapsed().as_millis(),
            category,
            shorten_path(&path.to_string_lossy())
        )],
    })
}

fn collect_output(stdout: &str, stderr: &str) -> String {
    let mut sections = Vec::new();
    let out = stdout
        .trim_end_matches('\n')
        .trim_end_matches('\r')
        .to_string();
    let err = stderr
        .trim_end_matches('\n')
        .trim_end_matches('\r')
        .to_string();
    if !out.trim().is_empty() {
        sections.push(out.trim_end().to_string());
    }
    if !err.trim().is_empty() {
        sections.push(format!("[stderr]\n{}", err.trim_end()));
    }
    sections.join("\n").trim().to_string()
}

fn compact_search_stdout(stdout: &str) -> String {
    const SNIP: usize = 160;
    let mut out: Vec<String> = Vec::new();
    for line in stdout.lines() {
        if let Some((p, ln, content)) = parse_search_hit_line(line) {
            let snippet = build_preview(&sanitize_search_line(content.trim()), SNIP);
            out.push(format!("{p}:{ln}:{snippet}"));
        } else {
            out.push(build_preview(&sanitize_search_line(line.trim()), SNIP));
        }
    }
    out.join("\n").trim_end().to_string()
}

fn truncate_by_lines(text: &str, max_lines: usize) -> (String, bool) {
    let lines: Vec<&str> = text.split('\n').collect();
    if lines.len() <= max_lines {
        return (text.to_string(), false);
    }
    let head = max_lines / 2;
    let tail = max_lines.saturating_sub(head).max(1);
    let mut out = String::new();
    if head > 0 {
        out.push_str(&lines[..head].join("\n"));
        out.push('\n');
    }
    out.push_str("...\n");
    out.push_str(&lines[lines.len().saturating_sub(tail)..].join("\n"));
    (out, true)
}

fn truncate_owned_by_lines(text: String, max_lines: usize) -> (String, bool) {
    let lines: Vec<&str> = text.split('\n').collect();
    if lines.len() <= max_lines {
        return (text, false);
    }
    let head = max_lines / 2;
    let tail = max_lines.saturating_sub(head).max(1);
    let mut out = String::new();
    if head > 0 {
        out.push_str(&lines[..head].join("\n"));
        out.push('\n');
    }
    out.push_str("...\n");
    out.push_str(&lines[lines.len().saturating_sub(tail)..].join("\n"));
    (out, true)
}

fn truncate_by_chars(text: &str, max_chars: usize) -> (String, bool) {
    if text.chars().count() <= max_chars {
        return (text.to_string(), false);
    }
    let head = max_chars / 2;
    let tail = max_chars.saturating_sub(head).max(1);
    let head_str: String = text.chars().take(head).collect();
    let tail_str: String = text
        .chars()
        .rev()
        .take(tail)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect();
    (format!("{head_str}\n...\n{tail_str}"), true)
}

fn truncate_owned_by_chars(text: String, max_chars: usize) -> (String, bool) {
    if text.chars().count() <= max_chars {
        return (text, false);
    }
    let (out, truncated) = truncate_by_chars(&text, max_chars);
    (out, truncated)
}

fn truncate_command_output(text: String) -> String {
    if text.is_empty() {
        return text;
    }
    let truncated_by_chars = text.chars().count() > OUTPUT_MAX_CHARS;
    let (mut joined, truncated_by_lines) = truncate_owned_by_lines(text, OUTPUT_MAX_LINES);
    (joined, _) = truncate_owned_by_chars(joined, OUTPUT_MAX_CHARS);
    if truncated_by_lines || truncated_by_chars {
        joined.push_str(&format!(
            "\n\n[输出已截断：保留头尾，最多 {OUTPUT_MAX_LINES} 行 / {OUTPUT_MAX_CHARS} 字符；建议改用更窄的命令（如加 tail/head/rg/awk）]"
        ));
    }
    joined.trim().to_string()
}

pub(crate) fn truncate_tool_payload(text: &str, max_lines: usize, max_chars: usize) -> String {
    if text.is_empty() {
        return String::new();
    }
    let (mut body, _truncated_by_lines) = truncate_by_lines(text, max_lines);
    let (body2, _truncated_by_chars) = truncate_owned_by_chars(body, max_chars);
    body = body2;
    body
}

pub(crate) fn build_preview(text: &str, limit: usize) -> String {
    let compact = text.split_whitespace().collect::<Vec<_>>().join(" ");
    if compact.is_empty() {
        return "(空)".to_string();
    }
    let count = compact.chars().count();
    if count <= limit {
        return compact;
    }
    if limit <= 6 {
        return compact.chars().take(limit).collect();
    }
    let head = limit.saturating_mul(2) / 3;
    let tail = limit.saturating_sub(head).saturating_sub(3);
    let head_str: String = compact.chars().take(head).collect();
    let tail_str: String = compact
        .chars()
        .rev()
        .take(tail)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect();
    format!("{head_str}...{tail_str}")
}

fn memory_path_for_key(key: &str) -> Option<String> {
    match key.trim().to_ascii_lowercase().as_str() {
        "meta" | "metamemo" => Some("memory/metamemo".to_string()),
        "date" | "datememo" => Some("memory/datememo".to_string()),
        "fast" | "fastmemo" => Some(normalize_tool_path("memory/fastmemory.jsonl")),
        _ => None,
    }
}

fn system_config_path() -> PathBuf {
    std::env::var("YING_SYSTEM_CONFIG")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("config/System.json"))
}

fn memory_label_for_path(path: &str) -> String {
    let lower = path.to_ascii_lowercase();
    if lower.contains("metamemo") {
        "metamemo".to_string()
    } else if lower.contains("datememo") {
        "datememo".to_string()
    } else if lower.contains("scopememo") {
        "scopememo".to_string()
    } else if lower.contains("fastmemo") {
        "fastmemo".to_string()
    } else {
        String::new()
    }
}

fn resolve_memory_path_label(raw: &str) -> Option<(String, String)> {
    if raw.trim().is_empty() {
        return None;
    }
    let lower = raw.trim().to_ascii_lowercase();
    if lower.starts_with("scopememo:") || lower.starts_with("scope:") {
        let name = raw.trim().splitn(2, ":").nth(1).unwrap_or("").trim();
        let rel = crate::memory_scope::scopememo_rel_path(name)?;
        return Some(("scopememo".to_string(), normalize_tool_path(&rel)));
    }
    if lower.contains("metamemo") {
        return Some(("metamemo".to_string(), memory_path_for_key("metamemo")?));
    }
    if lower.contains("datememo") {
        return Some(("datememo".to_string(), memory_path_for_key("datememo")?));
    }
    if let Some(path) = memory_path_for_key(raw) {
        let label = memory_label_for_path(&path);
        if label.is_empty() {
            return None;
        }
        return Some((label, path));
    }
    let path = normalize_tool_path(raw);
    let label = memory_label_for_path(&path);
    if label.is_empty() {
        None
    } else {
        let canonical = memory_path_for_key(&label);
        if let Some(canon) = canonical {
            return Some((label, canon));
        }
        Some((label, path))
    }
}

fn memory_paths_for_check(raw: &str) -> Vec<(String, String)> {
    let target = raw.trim().to_ascii_lowercase();
    if target.starts_with("scopememo:") || target.starts_with("scope:") {
        let name = raw.trim().splitn(2, ":").nth(1).unwrap_or("").trim();
        if let Some(rel) = crate::memory_scope::scopememo_rel_path(name) {
            return vec![("scopememo".to_string(), normalize_tool_path(&rel))];
        }
        return vec![];
    }
    if target.is_empty() {
        return vec![
            ("datememo".to_string(), "memory/datememo".to_string()),
            (
                "fastmemo".to_string(),
                normalize_tool_path("memory/fastmemory.jsonl"),
            ),
        ];
    }
    if target == "all" || target == "*" {
        return vec![
            ("datememo".to_string(), "memory/datememo".to_string()),
            ("metamemo".to_string(), "memory/metamemo".to_string()),
            (
                "fastmemo".to_string(),
                normalize_tool_path("memory/fastmemory.jsonl"),
            ),
        ];
    }
    if let Some(path) = memory_path_for_key(&target) {
        let label = memory_label_for_path(&path);
        if !label.is_empty() {
            return vec![(label, path)];
        }
    }
    if target.ends_with(".jsonl") {
        let path = normalize_tool_path(&target);
        let label = memory_label_for_path(&path);
        if !label.is_empty() {
            return vec![(label, path)];
        }
    }
    vec![]
}

fn pick_path(call: &ToolCall) -> anyhow::Result<String> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let path = normalize_tool_path(raw);
    if path.is_empty() {
        Err(anyhow!("缺少 path"))
    } else {
        Ok(path)
    }
}

#[allow(dead_code)]
fn bytes_preview_hex(bytes: &[u8], limit: usize) -> String {
    let len = bytes.len().min(limit);
    let mut out = String::new();
    for (i, b) in bytes[..len].iter().enumerate() {
        if i > 0 {
            out.push(' ');
        }
        out.push_str(&format!("{:02x}", b));
    }
    out
}

fn describe_tool_input(call: &ToolCall, limit: usize) -> String {
    fn display_tool_path(path: &str) -> String {
        let display = short_display_path(path);
        if display.is_empty() {
            return shorten_path(path);
        }
        if display == "." {
            return ".".to_string();
        }
        if display.starts_with('/')
            || display.starts_with('~')
            || display.starts_with("./")
            || display.starts_with("...")
        {
            return display;
        }
        format!("./{display}")
    }


    fn format_date_range(start: Option<NaiveDate>, end: Option<NaiveDate>) -> Option<String> {
        if start.is_none() && end.is_none() {
            return None;
        }
        let left = start
            .map(|d| d.format("%Y-%m-%d").to_string())
            .unwrap_or_else(|| "-".to_string());
        let right = end
            .map(|d| d.format("%Y-%m-%d").to_string())
            .unwrap_or_else(|| "-".to_string());
        Some(format!("@{left}..{right}"))
    }

    match call.tool.as_str() {
        "bash" | "adb" | "termux_api" => {
            let mut s = call.input.trim().to_string();
            if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
                if !s.is_empty() {
                    s.push_str(" in ");
                }
                s.push_str(&display_tool_path(cwd));
            }
            let save = call.save.as_deref().unwrap_or("").trim();
            if !save.is_empty() && !save.eq_ignore_ascii_case("auto") {
                if !s.is_empty() {
                    s.push(' ');
                }
                s.push_str(&format!("save:{save}"));
            }
            if let Some(codes) = call.ok_exit_codes.as_deref().filter(|v| !v.is_empty()) {
                let mut head = codes
                    .iter()
                    .take(4)
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>();
                head.sort();
                if !s.is_empty() {
                    s.push(' ');
                }
                if codes.len() > 4 {
                    s.push_str(&format!(
                        "ok_exit:[{}…+{}]",
                        head.join(","),
                        codes.len() - 4
                    ));
                } else {
                    s.push_str(&format!("ok_exit:[{}]", head.join(",")));
                }
            }
            build_preview(&s, limit)
        }
        "pty" => {
            let op = call.op.as_deref().unwrap_or("").trim().to_ascii_lowercase();
            let mut s = String::new();
            if !op.is_empty() {
                s.push_str(op.as_str());
            }
            match op.as_str() {
                "kill" => {
                    let mut ids: Vec<u64> = Vec::new();
                    if let Some(id) = call.job_id.filter(|v| *v > 0) {
                        ids.push(id);
                    }
                    if let Some(id) = call.pid.filter(|v| *v > 0) {
                        ids.push(id);
                    }
                    if let Some(list) = call.job_ids.as_ref() {
                        ids.extend(list.iter().copied().filter(|v| *v > 0));
                    }
                    if let Some(list) = call.pids.as_ref() {
                        ids.extend(list.iter().copied().filter(|v| *v > 0));
                    }
                    ids.sort_unstable();
                    ids.dedup();
                    let job = if ids.is_empty() {
                        "(missing pid/pids/job_id/job_ids)".to_string()
                    } else if ids.len() == 1 {
                        ids[0].to_string()
                    } else {
                        let head = ids
                            .iter()
                            .take(4)
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>();
                        if ids.len() > 4 {
                            format!("[{}…+{}]", head.join(","), ids.len() - 4)
                        } else {
                            format!("[{}]", head.join(","))
                        }
                    };
                    if !s.is_empty() {
                        s.push(' ');
                    }
                    s.push_str(&format!("job:{job}"));
                }
                "run" => {
                    if let Some(cmds) = call.commands.as_ref().filter(|v| !v.is_empty()) {
                        if !s.is_empty() {
                            s.push(' ');
                        }
                        s.push_str(&format!("cmds:{}", cmds.len()));
                        if let Some(first) =
                            cmds.first().map(|v| v.trim()).filter(|v| !v.is_empty())
                        {
                            let first = first.replace('\n', " ⏎ ").replace('\t', " ");
                            s.push(' ');
                            s.push_str(&build_preview(&first, 80));
                        }
                    } else if !call.input.trim().is_empty() {
                        let cmd = call.input.trim().replace('\n', " ⏎ ").replace('\t', " ");
                        if !s.is_empty() {
                            s.push(' ');
                        }
                        s.push_str(&build_preview(&cmd, 110));
                    }
                    let mode = call.mode.as_deref().unwrap_or("").trim();
                    if !mode.is_empty() {
                        if !s.is_empty() {
                            s.push(' ');
                        }
                        s.push_str(&format!("mode:{mode}"));
                    }
                }
                "list" => {}
                _ => {}
            }
            if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
                if !s.is_empty() {
                    s.push_str(" in ");
                }
                s.push_str(&display_tool_path(cwd));
            }
            build_preview(&s, limit)
        }
        "search" => {
            let raw_pattern = call.pattern.as_deref().unwrap_or(call.input.trim());
            let (pattern2, inline_ctx_lines, inline_ctx_enabled) =
                extract_search_inline_ctx_marker(raw_pattern);
            let pattern = if pattern2.trim().is_empty() {
                raw_pattern.to_string()
            } else {
                pattern2
            };
            let root = call.root.as_deref().or(call.path.as_deref()).unwrap_or(".");
            let keywords = parse_search_keywords(&pattern);
            let kw_display = if keywords.is_empty() {
                build_preview(&pattern, 180)
            } else if keywords.len() <= 6 {
                keywords.join(" ")
            } else {
                format!(
                    "{} … +{}",
                    keywords[..6].join(" "),
                    keywords.len().saturating_sub(6)
                )
            };
            let ctx = call.context.unwrap_or(false)
                || call.context_lines.is_some()
                || call.context_files.is_some()
                || call.context_hits_per_file.is_some()
                || inline_ctx_enabled
                || inline_ctx_lines.is_some();
            let mut extra = String::new();
            if ctx {
                let n = call
                    .context_lines
                    .or(inline_ctx_lines)
                    .unwrap_or(SEARCH_CONTEXT_DEFAULT_LINES)
                    .clamp(1, SEARCH_CONTEXT_MAX_LINES);
                extra = format!(" ctx:±{n}");
            }
            let mut prefix = String::new();
            if call.memory.unwrap_or(false) {
                prefix.push_str("mem:");
            }
            if call.file.unwrap_or(false) {
                prefix.push_str("file:");
            }
            let in_part = if call.memory.unwrap_or(false) {
                let target = call
                    .root
                    .as_deref()
                    .or(call.path.as_deref())
                    .unwrap_or("")
                    .trim();
                if target.is_empty() {
                    "mem:default".to_string()
                } else {
                    format!("mem:{target}")
                }
            } else {
                display_tool_path(root)
            };
            build_preview(&format!("{prefix}{kw_display}{extra} in {in_part}"), limit)
        }
        "edit_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim());
            if call.memory.unwrap_or(false) {
                let raw = path.trim();
                if raw.is_empty() {
                    build_preview("mem:(missing path)", limit)
                } else {
                    build_preview(&format!("mem:{raw}"), limit)
                }
            } else {
                build_preview(&display_tool_path(path), limit)
            }
        }
        "apply_patch" => {
            let size = call
                .patch
                .as_ref()
                .map(|p| p.len())
                .or_else(|| call.content.as_ref().map(|c| c.len()))
                .unwrap_or(call.input.len());
            if let Some(name) = call
                .patch
                .as_deref()
                .or(call.content.as_deref())
                .and_then(extract_patch_target)
            {
                return build_preview(&display_tool_path(&name), limit);
            }
            build_preview(&format!("patch ({} bytes)", size), limit)
        }
        "memory_check" => {
            let pattern = call.pattern.as_deref().unwrap_or(call.input.trim());
            let keywords = dedup_keywords(&parse_memory_keywords(pattern), SEARCH_MAX_KEYWORDS);
            let mut preview = if keywords.is_empty() {
                pattern.to_string()
            } else {
                keywords.join("/")
            };
            let start = call.date_start.as_deref().and_then(parse_memory_date);
            let end = call.date_end.as_deref().and_then(parse_memory_date);
            if let Some(range) = format_date_range(start, end) {
                if !preview.is_empty() {
                    preview.push(' ');
                }
                preview.push_str(&range);
            }
            build_preview(&preview, limit)
        }
        "memory_read" => {
            let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            let label = resolve_memory_path_label(raw)
                .map(|(label, _)| label)
                .unwrap_or_else(|| raw.to_string());
            let mut display = label;
            let start = call.start_line.unwrap_or(0);
            let max = call.max_lines.unwrap_or(0);
            if start > 0 && max > 0 {
                let end = start.saturating_add(max.saturating_sub(1));
                display = format!("{display} (lines {start}-{end})");
            }
            let start_date = call.date_start.as_deref().and_then(parse_memory_date);
            let end_date = call.date_end.as_deref().and_then(parse_memory_date);
            if let Some(range) = format_date_range(start_date, end_date) {
                display = format!("{display} {range}");
            }
            build_preview(&display, limit)
        }
        "memory_edit" | "memory_add" => {
            let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            let label = resolve_memory_path_label(raw)
                .map(|(label, _)| label)
                .unwrap_or_else(|| raw.to_string());
            if call.tool == "memory_add" && label == "fastmemo" {
                let section = call.section.as_deref().unwrap_or("").trim();
                if !section.is_empty() {
                    let display = format!("{label}::{section}");
                    return build_preview(&display, limit);
                }
            }
            build_preview(&label, limit)
        }
        "mind_msg" => {
            let target = resolve_mind_target(call).unwrap_or_else(|| "unknown".to_string());
            build_preview(&format!("to:{target}"), limit)
        }
        "system_config" => {
            if let Some(v) = parse_heartbeat_minutes(call) {
                return build_preview(&format!("heartbeat:{v}m"), limit);
            }
            build_preview(call.input.trim(), limit)
        }
        "skills_mcp" => {
            let category = call.category.as_deref().unwrap_or(call.input.trim()).trim();
            let category = if category.is_empty() {
                "编程类"
            } else {
                category
            };
            build_preview(category, limit)
        }
        "list" => {
            let op = call.op.as_deref().unwrap_or("dir").trim().to_ascii_lowercase();
            let cwd = call.cwd.as_deref().unwrap_or(".").trim();
            let cwd = if cwd.is_empty() { "." } else { cwd };
            let depth = call.depth.unwrap_or(LIST_MAX_DEPTH_DEFAULT);
            let file_count = call
                .names
                .as_ref()
                .map(|v| v.iter().filter(|s| !s.trim().is_empty()).count())
                .unwrap_or(0);
            if op == "files" {
                build_preview(
                    &format!("files name:{} in {}", file_count, display_tool_path(cwd)),
                    limit,
                )
            } else {
                build_preview(
                    &format!("dir depth:{} in {}", depth, display_tool_path(cwd)),
                    limit,
                )
            }
        }
        _ => build_preview(&call.input, limit),
    }
}

pub fn tool_display_label(tool: &str) -> String {
    match tool.trim() {
        // UI 展示层：直接使用工具名（避免把 bash 显示成 “Run” 造成困惑）
        "bash" => "BASH",
        "adb" => "SHELL",
        "termux_api" => "TERMUX",
        "read_file" => "READ",
        "write_file" => "WRITE",
        "search" => "SEARCH",
        "edit_file" => "EDIT",
        "apply_patch" => "PATCH",
        "list" => "LIST",
        "memory_check" => "MFIND",
        "memory_read" => "MREAD",
        "memory_edit" => "MEDIT",
        "memory_add" => "MADD",
        "mind_msg" => "MIND",
        "system_config" => "SYS",
        "skills_mcp" => "SKILLS",
        "pty" => "Terminal",
        _ => tool.trim(),
    }
    .to_string()
}
pub(crate) fn shorten_path(raw: &str) -> String {
    short_tail_path(raw, 2)
}

// cwd 的展示规则：
// - 如果在 $HOME 下：显示为 `~` 或 `~/<rel>`（省 token，避免 Android 沙盒长路径）
// - 否则：显示真实的绝对路径（便于排查/定位）
fn short_cwd_display(raw: &str) -> String {
    let path = raw.trim();
    if path.is_empty() {
        return String::new();
    }
    let mut p = path.to_string();
    // 将相对路径先解析成绝对（避免 cwd:system 这种歧义）
    if !p.starts_with('/') && !p.starts_with('~') {
        if let Ok(base) = std::env::current_dir() {
            p = base.join(path).to_string_lossy().to_string();
        }
    }
    // 尝试 canonicalize（失败则直接用解析后的路径）
    let p = std::fs::canonicalize(&p)
        .ok()
        .and_then(|x| x.to_str().map(|s| s.to_string()))
        .unwrap_or(p);

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

fn short_tail_path(raw: &str, tail_parts: usize) -> String {
    let path = raw.trim();
    if path.is_empty() {
        return String::new();
    }
    let home = std::env::var("HOME").unwrap_or_default();
    let mut p = path.to_string();
    if !home.is_empty() && p.starts_with(&home) {
        p = p.replacen(&home, "~", 1);
    }
    let parts: Vec<&str> = p.split('/').filter(|s| !s.is_empty()).collect();
    if parts.len() <= tail_parts.max(1) {
        return p;
    }
    let start = parts.len().saturating_sub(tail_parts.max(1));
    let tail = parts[start..].join("/");
    if p.starts_with('/') {
        format!(".../{tail}")
    } else if p.starts_with("~") {
        format!("~/.../{tail}")
    } else {
        format!(".../{tail}")
    }
}

fn short_display_path(raw: &str) -> String {
    let path = raw.trim();
    if path.is_empty() {
        return String::new();
    }
    let mut trimmed = path.trim_start_matches("./");
    if trimmed == "." {
        trimmed = "";
    }
    let cwd = std::env::current_dir().ok();
    let cwd_name = cwd
        .as_ref()
        .and_then(|p| p.file_name())
        .and_then(|s| s.to_str())
        .unwrap_or("");
    if !trimmed.starts_with('/') && !trimmed.starts_with('~') {
        if !cwd_name.is_empty() {
            if trimmed == cwd_name || trimmed.starts_with(&format!("{cwd_name}/")) {
                return trimmed.to_string();
            }
            if trimmed.is_empty() {
                return cwd_name.to_string();
            }
            return format!("{cwd_name}/{trimmed}");
        }
        return trimmed.to_string();
    }
    if let Some(cwd) = cwd.as_ref().and_then(|p| p.to_str()) {
        let cwd_norm = cwd.trim_end_matches('/');
        if !cwd_norm.is_empty() && path.starts_with(cwd_norm) {
            let rel = path[cwd_norm.len()..].trim_start_matches('/');
            if !cwd_name.is_empty() {
                if rel.is_empty() {
                    return cwd_name.to_string();
                }
                return format!("{cwd_name}/{rel}");
            }
        }
    }
    shorten_path(path)
}

pub(crate) fn normalize_tool_path(raw: &str) -> String {
    let path = raw.trim();
    if path.is_empty() {
        return String::new();
    }
    let home = std::env::var("HOME").unwrap_or_default();
    // 工程根别名：home -> ~/AItermux
    if !home.is_empty() {
        let project = format!("{home}/AItermux");
        if path == "." {
            return ".".to_string();
        }
        if path == "~" || path.starts_with("~/") {
            let rest = path.trim_start_matches('~').trim_start_matches('/');
            if rest.is_empty() {
                return project;
            }
            return format!("{project}/{rest}");
        }
        if path == "home" || path.starts_with("home/") {
            let rest = path.trim_start_matches("home").trim_start_matches('/');
            if rest.is_empty() {
                return project;
            }
            return format!("{project}/{rest}");
        }
    }
    if path.starts_with('/') {
        return path.to_string();
    }
    let mut trimmed = path.trim_start_matches("./");
    if trimmed == "." {
        return ".".to_string();
    }
    let cwd_name = std::env::current_dir()
        .ok()
        .and_then(|p| {
            p.file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_default();
    if !cwd_name.is_empty() {
        if trimmed == cwd_name {
            return ".".to_string();
        }
        let prefix = format!("{cwd_name}/");
        if trimmed.starts_with(&prefix) {
            trimmed = &trimmed[prefix.len()..];
        }
    }
    trimmed.to_string()
}

fn extract_patch_target(patch: &str) -> Option<String> {
    for line in patch.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("*** Update File:") {
            let name = rest.trim();
            if !name.is_empty() {
                return Some(name.to_string());
            }
        }
        if let Some(rest) = trimmed.strip_prefix("+++ ") {
            let raw = rest
                .trim()
                .trim_start_matches("b/")
                .trim_start_matches("a/");
            if raw == "/dev/null" {
                continue;
            }
            if !raw.is_empty() {
                return Some(raw.to_string());
            }
        }
        if let Some(rest) = trimmed.strip_prefix("--- ") {
            let raw = rest
                .trim()
                .trim_start_matches("a/")
                .trim_start_matches("b/");
            if raw == "/dev/null" {
                continue;
            }
            if !raw.is_empty() {
                return Some(raw.to_string());
            }
        }
    }
    None
}

fn count_patch_changes(patch: &str) -> (usize, usize) {
    let mut add = 0usize;
    let mut del = 0usize;
    for line in patch.lines() {
        if line.starts_with("+++ ") || line.starts_with("--- ") || line.starts_with("@@") {
            continue;
        }
        if line.starts_with("diff ") || line.starts_with("index ") {
            continue;
        }
        if line.starts_with("new file mode")
            || line.starts_with("deleted file mode")
            || line.starts_with("rename ")
        {
            continue;
        }
        if line.starts_with('+') {
            add += 1;
        } else if line.starts_with('-') {
            del += 1;
        }
    }
    (add, del)
}

#[derive(Debug)]
struct PatchValidationError {
    line: usize,
    reason: String,
}

fn validate_unified_patch(patch: &str) -> Result<(), PatchValidationError> {
    let mut any_file = false;
    let mut have_old = false;
    let mut have_new = false;
    let mut have_hunk = false;
    let mut last_header_line = 0usize;
    for (idx, line) in patch.lines().enumerate() {
        let line_no = idx + 1;
        let trimmed = line.trim_end();
        if trimmed.starts_with("*** Begin Patch") || trimmed.starts_with("*** Update File:") {
            return Err(PatchValidationError {
                line: line_no,
                reason: "请使用 unified diff（含 ---/+++ 与 @@）".to_string(),
            });
        }
        if trimmed.starts_with("diff ") {
            continue;
        }
        if trimmed.starts_with("index ")
            || trimmed.starts_with("new file mode")
            || trimmed.starts_with("deleted file mode")
            || trimmed.starts_with("rename ")
            || trimmed.starts_with("similarity index")
            || trimmed.starts_with("old mode")
            || trimmed.starts_with("new mode")
            || trimmed.starts_with("copy ")
        {
            continue;
        }
        if trimmed.starts_with("--- ") {
            if any_file && !have_hunk {
                return Err(PatchValidationError {
                    line: last_header_line.max(1),
                    reason: "缺少 @@ hunk".to_string(),
                });
            }
            any_file = true;
            have_old = true;
            have_new = false;
            have_hunk = false;
            last_header_line = line_no;
            continue;
        }
        if trimmed.starts_with("+++ ") {
            if !have_old {
                return Err(PatchValidationError {
                    line: line_no,
                    reason: "缺少 --- 旧文件头".to_string(),
                });
            }
            have_new = true;
            last_header_line = line_no;
            continue;
        }
        if trimmed.starts_with("@@") {
            if !(have_old && have_new) {
                return Err(PatchValidationError {
                    line: line_no,
                    reason: "缺少文件头（---/+++）".to_string(),
                });
            }
            have_hunk = true;
            continue;
        }
    }
    if !any_file {
        return Err(PatchValidationError {
            line: 1,
            reason: "缺少文件头（---/+++）".to_string(),
        });
    }
    if have_old && have_new && !have_hunk {
        return Err(PatchValidationError {
            line: last_header_line.max(1),
            reason: "缺少 @@ hunk".to_string(),
        });
    }
    Ok(())
}
#[derive(Default)]
struct PatchReport {
    fuzz: bool,
    offset: bool,
    failed: bool,
}


fn parse_patch_report(body: &str) -> PatchReport {
    let mut report = PatchReport::default();
    for line in body.lines() {
        let lower = line.to_lowercase();
        if lower.contains("fuzz") {
            report.fuzz = true;
        }
        if lower.contains("offset") {
            report.offset = true;
        }
        if lower.contains("failed") || lower.contains("reject") {
            report.failed = true;
        }
    }
    report
}

fn run_patch_command(
    patch_text: &str,
    strip: i32,
    dry_run: bool,
    cwd_exec: &str,
) -> anyhow::Result<(i32, String, String, bool)> {
    let strip_arg = format!("-p{strip}");
    let mut args = vec![strip_arg.as_str(), "--forward", "--batch"];
    if dry_run {
        args.push("--dry-run");
    }
    let bytes = patch_text.len() as u64;
    let timeout_secs = if bytes >= 300_000 {
        PATCH_TIMEOUT_MAX_SECS
    } else if bytes >= 80_000 {
        PATCH_TIMEOUT_MID_SECS
    } else {
        PATCH_TIMEOUT_MIN_SECS
    };
    let (mut command, timeout_used) = build_command_with_timeout("patch", &args, timeout_secs);
    let mut child = command
        .current_dir(cwd_exec)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("启动 patch 失败")?;
    if let Some(stdin) = child.stdin.as_mut() {
        stdin
            .write_all(patch_text.as_bytes())
            .context("写入 patch 失败")?;
    }
    let out = child.wait_with_output().context("执行 patch 失败")?;
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    Ok((code, stdout, stderr, timed_out))
}

fn run_command_output_with_optional_timeout(
    program: &str,
    args: &[&str],
    timeout_secs: Option<u64>,
) -> anyhow::Result<(i32, String, String, bool)> {
    let (mut command, timeout_used) =
        build_command_with_optional_timeout(program, args, timeout_secs);
    let out = command
        .output()
        .with_context(|| format!("执行失败：{program}"))?;
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    // Termux 环境下可能存在“被覆盖到 PATH 的不可执行 rg”（例如被某些工具链注入的 musl 二进制）。
    // 这会导致 search 工具走 rg 时稳定返回 127（cannot execute / required file not found）。
    // 这里做一次兜底：检测到典型错误后改用 Termux 自带的 rg 路径重试。
    if program == "rg"
        && code == 127
        && (stderr.contains("cannot execute") || stderr.contains("required file not found"))
    {
        let termux_rg = "/data/data/com.termux/files/usr/bin/rg";
        if Path::new(termux_rg).is_file() {
            let (mut cmd2, timeout_used2) =
                build_command_with_optional_timeout(termux_rg, args, timeout_secs);
            if let Ok(out2) = cmd2.output() {
                let code2 = status_code(out2.status.code());
                let timed_out2 = timeout_used2 && is_timeout_status(code2);
                let stdout2 = String::from_utf8_lossy(&out2.stdout).to_string();
                let stderr2 = String::from_utf8_lossy(&out2.stderr).to_string();
                return Ok((code2, stdout2, stderr2, timed_out2));
            }
        }
    }
    Ok((code, stdout, stderr, timed_out))
}
