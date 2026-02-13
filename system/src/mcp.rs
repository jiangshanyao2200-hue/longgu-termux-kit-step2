use std::fs;
use std::io::{BufRead, Read, Seek, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::Instant;

use anyhow::{Context, anyhow};
use chrono::{Local, NaiveDate};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::memorydb::{DEFAULT_MEMO_DB_PATH, MemoDb, MemoKind, MemoRow};

const BASH_SHELL: &str = "/data/data/com.termux/files/usr/bin/bash";
const ADB_SERIAL: &str = "127.0.0.1:5555";
const OUTPUT_MAX_CHARS: usize = 20_000;
const OUTPUT_MAX_LINES: usize = 400;
const READ_MAX_BYTES: usize = 200_000;
const READ_FULL_MAX_BYTES: usize = 300_000;
const READ_FULL_MAX_LINES: usize = 20_000;
const READ_PEEK_HEAD_LINES: usize = 30;
const READ_PEEK_TAIL_LINES: usize = 10;
const READ_PEEK_TOKEN_BUDGET: usize = 10_000;
const READ_RANGE_TOKEN_BUDGET: usize = 20_000;
const READ_FULL_TOKEN_BUDGET: usize = 20_000;
const READ_MAX_LINE_CHARS: usize = 2000;
const READ_RANGE_MAX_LINES: usize = 1000;
const READ_SCAN_MAX_BYTES: usize = 20_000_000;
// read_file 的 raw 输出预算：避免被全局 20k chars 二次截断导致“读不够用”。
const READ_TOOL_RAW_MAX_LINES: usize = 1400;
const READ_TOOL_RAW_MAX_CHARS: usize = 80_000;
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
const SHELL_SAVE_THRESHOLD_BYTES: usize = 400_000;
const ADB_CACHE_DIR: &str = "log/adb-cache";
// adb 输出（尤其 logcat/dumpsys）可能更大：默认阈值略高，超出则落盘供后续按需读取。
const ADB_SAVE_THRESHOLD_BYTES: usize = 900_000;
const TERMUX_API_CACHE_DIR: &str = "log/termux-api-cache";
// termux-api 输出通常偏中等，但也可能一次返回大量 JSON（如 SAF/传感器/Wi-Fi 扫描等）。
const TERMUX_API_SAVE_THRESHOLD_BYTES: usize = 600_000;
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
struct ExportedOutputMeta {
    path: String,
    export_dir: String,
    bytes: usize,
    lines: usize,
}

fn export_dir_of(path: &str) -> String {
    Path::new(path)
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or("")
        .to_string()
}

fn exported_meta(path: String, bytes: usize, lines: usize) -> ExportedOutputMeta {
    ExportedOutputMeta {
        export_dir: export_dir_of(&path),
        path,
        bytes,
        lines,
    }
}

fn format_exported_notice(meta: &ExportedOutputMeta) -> String {
    let size = format_bytes(meta.bytes as u64);
    format!(
        "【输出已导出】dir:{} | saved:{} | size:{} | lines:{}",
        if meta.export_dir.trim().is_empty() {
            "(unknown)"
        } else {
            meta.export_dir.as_str()
        },
        meta.path,
        size,
        meta.lines
    )
}

fn truncate_export_preview(text: &str) -> String {
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
    out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
    if call.tool == "read_file" {
        return (READ_TOOL_RAW_MAX_LINES, READ_TOOL_RAW_MAX_CHARS);
    }
    (TOOL_OUTPUT_RAW_MAX_LINES, TOOL_OUTPUT_RAW_MAX_CHARS)
}
const WRITE_MAX_BYTES: usize = 50_000_000;
const WRITE_BACKUP_DIR: &str = "log/backupcache";
const FILE_MANAGER_RECYCLE_DIR: &str = "log/recycle";
const PATCH_MAX_BYTES: usize = 500_000;
const EDIT_MAX_FILE_BYTES: usize = 800_000;
const EDIT_MAX_MATCHES: usize = 400;
const LIST_MAX_DEPTH_CAP: usize = 4;
const LIST_MAX_ENTRIES_DEFAULT: usize = 300;
const LIST_MAX_ENTRIES_CAP: usize = 1200;
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
    RE.get_or_init(|| Regex::new(r"(?is)<tool>([\s\S]*?)</tool>").expect("tool regex"))
}

fn json_fence_re() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?is)```(?:json)?\s*(\{[\s\S]*?\})\s*```").expect("fence regex"))
}

fn parse_usize_value(v: &Value) -> Option<usize> {
    v.as_u64()
        .map(|n| n as usize)
        .or_else(|| v.as_str().and_then(|s| s.trim().parse::<usize>().ok()))
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
    pub strict: Option<bool>,
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

    // list_dir：递归深度（1=仅当前目录）。为避免 token/性能暴涨，默认仍为 1。
    #[serde(default)]
    pub depth: Option<usize>,
    // list_dir：最多返回多少条 entry（每条一行）。超出会截断并提示。
    #[serde(default)]
    pub max_entries: Option<usize>,
    // write_file：覆盖已存在的非空文件（需要显式确认）
    #[serde(default)]
    pub overwrite: Option<bool>,

    // file_manager
    #[serde(default)]
    pub op: Option<String>,
    #[serde(default)]
    pub src: Option<String>,
    #[serde(default)]
    pub dst: Option<String>,
    #[serde(default)]
    pub force: Option<bool>,
    #[serde(default)]
    pub recursive: Option<bool>,
    #[serde(default)]
    pub mode: Option<String>,
    #[serde(default)]
    pub trash_id: Option<String>,
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

fn parse_tool_call_payload(payload: &str) -> Option<ToolCall> {
    let mut v: Value = serde_json::from_str(payload).ok()?;
    if let Some(args_str) = v
        .get("arguments")
        .and_then(|x| x.as_str())
        .map(str::trim)
        .filter(|s| !s.is_empty())
        && let Ok(parsed) = serde_json::from_str::<Value>(args_str)
        && let Value::Object(mut m) = v
    {
        if let Value::Object(a) = parsed {
            for (k, val) in a {
                m.entry(k).or_insert(val);
            }
        }
        v = Value::Object(m);
    }
    normalize_tool_call(v).ok()
}

fn normalize_tool_call(v: Value) -> anyhow::Result<ToolCall> {
    let Value::Object(mut m) = v else {
        return Err(anyhow!("tool call 不是对象"));
    };

    let tool = m
        .remove("tool")
        .or_else(|| m.remove("name"))
        .or_else(|| m.remove("tool_name"))
        .and_then(|x| x.as_str().map(|s| s.trim().to_string()))
        .unwrap_or_default();

    let mut call = ToolCall {
        tool,
        ..Default::default()
    };

    if let Some(v) = m.remove("brief").or_else(|| m.remove("desc"))
        && let Some(s) = v.as_str().map(str::trim).filter(|s| !s.is_empty())
    {
        call.brief = Some(s.to_string());
    }

    if let Some(v) = m
        .remove("input")
        .or_else(|| m.remove("command"))
        .or_else(|| m.remove("cmd"))
    {
        match v {
            Value::String(s) => call.input = s,
            Value::Object(obj) => apply_args_object(&mut call, obj),
            Value::Array(arr) => {
                let joined = arr
                    .into_iter()
                    .filter_map(|x| x.as_str().map(|s| s.to_string()))
                    .collect::<Vec<_>>()
                    .join(" ");
                call.input = joined;
            }
            other => call.input = other.to_string(),
        }
    }

    if let Some(Value::Object(obj)) = m.remove("args").or_else(|| m.remove("arguments")) {
        apply_args_object(&mut call, obj);
    }

    apply_flat_fields(&mut call, &mut m);
    let raw_tool = call.tool.clone();
    call.tool = normalize_tool_name(&call.tool);
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
            "content" => call.content = s,
            "message" | "msg" => call.content = s,
            "pattern" => call.pattern = s,
            "root" => call.root = s,
            "patch" => call.patch = s,
            "find" => call.find = s,
            "replace" => call.replace = s,
            "time" | "时间" => call.time = s,
            "keywords" | "keyword" | "tags" | "关键词" => call.keywords = s,
            "diary" | "note" | "日记" => call.diary = s,
            "category" | "class" | "kind" | "group" | "类别" => call.category = s,
            "section" | "slot" | "area" | "heading" => call.section = s,
            "region" => call.region = s,
            "target" | "to" | "dest" | "receiver" | "recipient" | "目标" => call.target = s,
            "start_date" | "date_start" | "from_date" => call.date_start = s,
            "end_date" | "date_end" | "to_date" => call.date_end = s,
            "heartbeat_minutes" | "heartbeat_min" | "heartbeat" | "heartbeat_minute" => {
                if let Some(v) = parse_usize_value(&val) {
                    call.heartbeat_minutes = Some(v);
                }
            }
            "interactive" => {
                call.interactive = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "timeout_secs" | "timeout_s" | "timeout" => {
                call.timeout_secs = val
                    .as_u64()
                    .or_else(|| val.as_str().and_then(|x| x.trim().parse::<u64>().ok()));
            }
            "timeout_ms" => {
                call.timeout_ms = val
                    .as_u64()
                    .or_else(|| val.as_str().and_then(|x| x.trim().parse::<u64>().ok()));
            }
            "cwd" | "workdir" | "work_dir" | "dir" => {
                call.cwd = s;
            }
            "save" | "export" | "capture" => {
                call.save = s;
            }
            "ok_exit_codes" | "ok_codes" | "ok_exit" | "ok_exit_code" => {
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
            "memory" | "mem" | "memo" | "in_memory" => {
                call.memory = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "scope" | "bucket" | "zone" => {
                call.region = s;
            }
            "scan_limit" | "scan_max" | "scan_cap" | "max_scan" => {
                call.scan_limit = parse_usize_value(&val);
            }
            "full" => {
                call.full = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "output_level" | "out_level" | "level" | "output" => {
                call.output_level = s;
            }
            "depth" | "max_depth" => {
                call.depth = parse_usize_value(&val);
            }
            "max_entries" | "entries" | "entry_cap" => {
                call.max_entries = parse_usize_value(&val);
            }
            "context" | "ctx" | "fast_context" => {
                call.context = val
                    .as_bool()
                    .or_else(|| val.as_str().and_then(|x| parse_toggle_input(x)));
            }
            "context_lines" | "ctx_lines" | "lines_around" | "around_lines" => {
                call.context_lines = parse_usize_value(&val);
            }
            "context_files" | "ctx_files" | "max_files" => {
                call.context_files = parse_usize_value(&val);
            }
            "context_hits_per_file" | "ctx_hits_per_file" | "hits_per_file" => {
                call.context_hits_per_file = parse_usize_value(&val);
            }
            "include_glob" | "include_globs" | "include" => {
                call.include_glob = parse_string_list_value_max(&val, SEARCH_MAX_GLOBS);
            }
            "exclude_glob" | "exclude_globs" | "exclude" => {
                call.exclude_glob = parse_string_list_value_max(&val, SEARCH_MAX_GLOBS);
            }
            "exclude_dirs" | "exclude_dir" => {
                call.exclude_dirs = parse_string_list_value_max(&val, 32);
            }
            "count" => call.count = parse_usize_value(&val),
            "start_line" | "start" => {
                call.start_line = parse_usize_value(&val);
            }
            "max_lines" | "lines" => {
                call.max_lines = parse_usize_value(&val);
            }
            "max_chars" | "chars" | "max_char" => {
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
            "strict" => {
                call.strict = val
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

fn normalize_tool_name(name: &str) -> String {
    let n = name.trim();
    match n {
        "ls" | "list" | "list_files" | "listdir" | "list_directory" => "list_dir",
        "stat" | "info" | "file_info" => "stat_file",
        "read" | "readfile" => "read_file",
        "write" | "writefile" => "write_file",
        "grep" | "rg" | "ripgrep" => "search",
        "edit" => "edit_file",
        "patch" => "apply_patch",
        "memorycheck" | "memory_check" | "memorycheak" => "memory_check",
        "memoryread" | "memory_read" => "memory_read",
        "memoryedit" | "memory_edit" => "memory_edit",
        "memoryadd" | "memory_add" => "memory_add",
        "mind_msg" | "mindmsg" | "mind_message" | "mindmessage" | "mind" | "todog" | "to_dog"
        | "to-dog" | "tomain" | "to_main" | "to-main" => "mind_msg",
        "system_config" | "sys_config" | "systemcfg" | "system_setting" | "system_settings" => {
            "system_config"
        }
        "skills" | "skills_mcp" | "skill" | "skill_mcp" => "skills_mcp",
        _ => n,
    }
    .to_string()
}

fn apply_tool_defaults(call: &mut ToolCall) {
    match call.tool.as_str() {
        "list_dir" | "stat_file" => {
            if call.path.as_deref().unwrap_or("").trim().is_empty() && call.input.trim().is_empty()
            {
                call.input = ".".to_string();
            }
        }
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

fn sanitize_search_line(raw: &str) -> String {
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

fn is_probably_binary_file(path: &Path) -> bool {
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

    let fence_re = json_fence_re();
    for caps in fence_re.captures_iter(text) {
        let Some(block) = caps.get(0) else {
            continue;
        };
        if spans_overlap(&spans, block.start(), block.end()) {
            continue;
        }
        let Some(payload) = caps.get(1).map(|m| m.as_str()) else {
            continue;
        };
        if let Some(call) = parse_tool_call_payload(payload) {
            calls.push(call);
            spans.push((block.start(), block.end()));
        }
    }

    for (start, end, payload) in extract_json_objects(text) {
        if spans_overlap(&spans, start, end) {
            continue;
        }
        if let Some(call) = parse_tool_call_payload(&payload) {
            calls.push(call);
            spans.push((start, end));
        }
    }

    let cleaned = remove_spans(text, &spans);
    Ok((calls, cleaned.trim().to_string()))
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

fn spans_overlap(spans: &[(usize, usize)], start: usize, end: usize) -> bool {
    spans.iter().any(|(s, e)| start < *e && end > *s)
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

fn extract_json_objects(text: &str) -> Vec<(usize, usize, String)> {
    let mut out = Vec::new();
    let mut in_str = false;
    let mut escape = false;
    let mut depth = 0usize;
    let mut start: Option<usize> = None;

    for (idx, ch) in text.char_indices() {
        if start.is_none() {
            if ch == '{' {
                start = Some(idx);
                depth = 1;
                in_str = false;
                escape = false;
            }
            continue;
        }
        if in_str {
            if escape {
                escape = false;
            } else if ch == '\\' {
                escape = true;
            } else if ch == '"' {
                in_str = false;
            }
            continue;
        }
        match ch {
            '"' => in_str = true,
            '{' => depth = depth.saturating_add(1),
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let s = start.take().unwrap_or(0);
                    let e = idx + ch.len_utf8();
                    if e > s && e <= text.len() {
                        out.push((s, e, text[s..e].to_string()));
                    }
                }
            }
            _ => {}
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_tool_call_basic() {
        let call = parse_tool_call_payload(r#"{"tool":"list_dir","input":".","brief":"列目录"}"#)
            .expect("call");
        assert_eq!(call.tool, "list_dir");
        assert_eq!(call.input, ".");
        assert_eq!(call.brief.as_deref(), Some("列目录"));
    }

    #[test]
    fn parse_tool_call_name_arguments_string() {
        let call = parse_tool_call_payload(
            r#"{"name":"search","arguments":"{\"pattern\":\"foo\",\"root\":\".\"}"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "search");
        assert_eq!(call.pattern.as_deref(), Some("foo"));
        assert_eq!(call.root.as_deref(), Some("."));
    }

    #[test]
    fn parse_tool_call_input_object() {
        let call = parse_tool_call_payload(
            r#"{"tool":"search","input":{"pattern":"foo","root":"."},"brief":"搜文本"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "search");
        assert_eq!(call.pattern.as_deref(), Some("foo"));
        assert_eq!(call.root.as_deref(), Some("."));
        assert_eq!(call.brief.as_deref(), Some("搜文本"));
    }

    #[test]
    fn parse_tool_call_input_object_turing_modules() {
        let call = parse_tool_call_payload(
            r#"{"tool":"bash","brief":"模块化参数","input":{"input":"pwd","run":{"cwd":"AItermux/system","timeout_ms":12000},"out":{"save":"always"},"expect":{"ok_exit_codes":[0,1]}}}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "bash");
        assert_eq!(call.input.trim(), "pwd");
        assert_eq!(call.cwd.as_deref(), Some("AItermux/system"));
        assert_eq!(call.timeout_ms, Some(12000));
        assert_eq!(call.save.as_deref(), Some("always"));
        assert!(call.ok_exit_codes.as_ref().is_some_and(|v| v.contains(&1)));
    }

    #[test]
    fn parse_tool_call_memory_flag() {
        let call = parse_tool_call_payload(
            r#"{"tool":"search","pattern":"foo","root":"all","memory":true,"brief":"搜记忆"}"#,
        )
        .expect("call");
        assert_eq!(call.tool, "search");
        assert_eq!(call.memory, Some(true));
        assert_eq!(call.pattern.as_deref(), Some("foo"));
    }

    #[test]
    fn parse_tool_call_memory_region_and_scan_limit() {
        let call = parse_tool_call_payload(
            r#"{"tool":"search","pattern":"a b","root":"datememo","memory":true,"region":"past","scan_limit":12000,"brief":"搜记忆"}"#,
        )
        .expect("call");
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
        assert_eq!(call.tool, "list_dir");
        assert_eq!(call.input, ".");
    }

    #[test]
    fn apply_defaults_for_list_dir() {
        let call = parse_tool_call_payload(r#"{"tool":"list_dir"}"#).expect("call");
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
        assert!(out.contains("[输出已截断"));
        assert!(!out.contains("line5"));
    }

    #[test]
    fn truncate_tool_payload_truncates_by_chars_with_unicode() {
        let input = "你好世界你好世界你好世界"; // 12 chars
        let out = truncate_tool_payload(input, 10, 6);
        assert!(out.contains("\n...\n"));
        assert!(out.contains("[输出已截断"));
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

    #[test]
    fn validate_file_manager_requires_op() {
        let call = ToolCall {
            tool: "file_manager".to_string(),
            brief: Some("测试".to_string()),
            ..Default::default()
        };
        assert!(validate_tool_call(&call).is_err());
    }

    #[test]
    fn file_manager_trash_restore_roundtrip_file() {
        let pid = std::process::id();
        let src_rel = format!("log/_test_file_manager_{pid}.txt");
        let src = PathBuf::from(&src_rel);
        if let Some(parent) = src.parent() {
            fs::create_dir_all(parent).expect("mkdir log");
        }
        fs::write(&src, "hello").expect("write src");

        let trash_call = ToolCall {
            tool: "file_manager".to_string(),
            brief: Some("测试回收".to_string()),
            op: Some("trash".to_string()),
            path: Some(src_rel.clone()),
            ..Default::default()
        };
        let trashed = handle_tool_call(&trash_call).expect("trash ok");
        assert!(!src.exists(), "src should be removed by rename-trash");
        let meta_line = trashed
            .log_lines
            .iter()
            .find(|l| l.contains("trash_id:"))
            .cloned()
            .unwrap_or_default();
        let trash_id = meta_line
            .split('|')
            .map(|s| s.trim())
            .find_map(|part| part.strip_prefix("trash_id:"))
            .unwrap_or("")
            .trim()
            .to_string();
        assert!(!trash_id.is_empty(), "trash_id should be present");

        let restore_call = ToolCall {
            tool: "file_manager".to_string(),
            brief: Some("测试恢复".to_string()),
            op: Some("restore".to_string()),
            trash_id: Some(trash_id.clone()),
            force: Some(true),
            ..Default::default()
        };
        let _restored = handle_tool_call(&restore_call).expect("restore ok");
        assert!(src.exists(), "src should be restored");
        let content = fs::read_to_string(&src).expect("read restored");
        assert_eq!(content, "hello");

        let _ = fs::remove_file(&src);
        let _ = fs::remove_dir_all(file_manager_recycle_dir().join(&trash_id));
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

fn format_tool_message_with_limits(
    call: &ToolCall,
    outcome: &ToolOutcome,
    output_max_lines: usize,
    output_max_chars: usize,
    meta_max_lines: usize,
    meta_max_chars: usize,
) -> String {
    let mut msg = String::new();
    let label = tool_display_label(&call.tool);
    if !label.is_empty() {
        msg.push_str(&format!("操作: {label}\n"));
    }
    let mut input_preview = describe_tool_input(call, 400);
    if let Some((add, del, unit)) = extract_delta_from_log(&outcome.log_lines)
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
        msg.push_str(&format!("explain: {brief}\n"));
    }
    if !input_preview.is_empty() {
        msg.push_str(&format!("input: {input_preview}\n"));
    }
    msg.push_str("output:\n```text\n");
    let output = if outcome.user_message.trim().is_empty() {
        "(no output)".to_string()
    } else {
        truncate_tool_payload(
            outcome.user_message.trim_end(),
            output_max_lines,
            output_max_chars,
        )
    };
    msg.push_str(&output);
    msg.push_str("\n```\n");
    if !outcome.log_lines.is_empty() {
        msg.push_str("meta:\n```text\n");
        let meta_join = outcome.log_lines.join("\n");
        let meta = truncate_tool_payload(meta_join.trim_end(), meta_max_lines, meta_max_chars);
        msg.push_str(&meta);
        msg.push_str("\n```\n");
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
        "plan" | "work" => {
            r#"{"tool":"plan","section":"start","input":"修复 PTY 键位冲突","time":"~5min","content":"1) 复现问题\n2) 修改映射\n3) 跑测试验证","brief":"开始工作"}"#
        }
        "list_dir" => r#"{"tool":"list_dir","path":".","brief":"列出目录"}"#,
        "stat_file" => r#"{"tool":"stat_file","path":"Cargo.toml","brief":"查看文件信息"}"#,
        "read_file" => {
            r#"{"tool":"read_file","path":"src/main.rs","head":true,"max_lines":200,"brief":"读取文件开头"}"#
        }
        "write_file" => {
            r#"{"tool":"write_file","path":"notes/demo.txt","content":"hello","overwrite":false,"brief":"写入文件（默认不覆盖非空文件）"}"#
        }
        "search" => {
            r#"{"tool":"search","pattern":"TODO","root":"src","memory":false,"brief":"搜索内容（可选 file:true 搜文件名；memory:true 搜记忆）"}"#
        }
        "edit_file" => {
            r#"{"tool":"edit_file","path":"src/main.rs","find":"old","replace":"new","count":1,"brief":"替换片段"}"#
        }
        "apply_patch" => {
            r#"{"tool":"apply_patch","patch":"--- a/src/main.rs\n+++ b/src/main.rs\n@@\n- old\n+ new\n","strict":false,"brief":"应用补丁"}"#
        }
        "memory_check" => {
            r#"{"tool":"memory_check","pattern":"上次的工作","brief":"回忆最近的工作记录"}"#
        }
        "memory_read" => {
            r#"{"tool":"memory_read","path":"datememo","date_start":"2026-02-13","brief":"读取指定日期的日记（sqlite）"}"#
        }
        "memory_edit" => {
            r#"{"tool":"memory_edit","path":"fastmemo","find":"旧条目","replace":"新条目","count":1,"brief":"修正条目"}"#
        }
        "memory_add" => {
            r#"{"tool":"memory_add","path":"fastmemo","section":"事件感知","content":"新增一条","brief":"追加 fastmemo 条目"}"#
        }
        "mind_msg" => {
            r#"{"tool":"mind_msg","target":"dog","content":"需要你协助检查工具结果。","brief":"同步需求"}"#
        }
        "system_config" => {
            r#"{"tool":"system_config","heartbeat_minutes":10,"brief":"调整心跳间隔"}"#
        }
        "skills_mcp" => r#"{"tool":"skills_mcp","category":"编程类","brief":"获取编程类工具说明"}"#,
        "file_manager" => {
            r#"{"tool":"file_manager","op":"copy","src":"a.txt","dst":"backup/a.txt","force":false,"recursive":false,"brief":"Manage·Copy"}"#
        }
        _ => r#"{"tool":"<tool>","input":"...","brief":"一句话说明"}"#,
    }
}

fn tool_format_error(tool: &str, reason: &str) -> ToolOutcome {
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
    // 大多数工具必须提供 brief（审计/可视化用途）；但纯 UI 的 plan/work 允许省略。
    if !matches!(tool, "plan" | "work") && call.brief.as_deref().unwrap_or("").trim().is_empty() {
        return Err(tool_format_error(tool, "缺少 brief"));
    }
    match tool {
        "bash" | "adb" | "termux_api" => {
            if call.input.trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 input"));
            }
        }
        "work" | "plan" => {
            // 纯 UI 工具：允许空 input，但至少要有一些内容能渲染。
            let input = call.input.trim();
            let content = call.content.as_deref().unwrap_or("").trim();
            let time = call.time.as_deref().unwrap_or("").trim();
            if input.is_empty() && content.is_empty() && time.is_empty() {
                return Err(tool_format_error(
                    tool,
                    "缺少 input/content/time（至少提供其一）",
                ));
            }
        }
        "list_dir" | "stat_file" | "read_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            if path.is_empty() {
                return Err(tool_format_error(tool, "缺少 path"));
            }
        }
        "write_file" => {
            if call.path.as_deref().unwrap_or("").trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 path"));
            }
            let content = call.content.as_deref().unwrap_or(call.input.trim());
            if content.is_empty() {
                return Err(tool_format_error(tool, "缺少 content"));
            }
        }
        "search" => {
            let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
            if pattern.is_empty() {
                return Err(tool_format_error(tool, "缺少 pattern"));
            }
        }
        "edit_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            if path.is_empty() {
                return Err(tool_format_error(tool, "缺少 path"));
            }
            if call.find.as_deref().unwrap_or("").trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 find"));
            }
        }
        "apply_patch" => {
            let patch = call
                .patch
                .as_deref()
                .or(call.content.as_deref())
                .unwrap_or(call.input.trim())
                .trim();
            if patch.is_empty() {
                return Err(tool_format_error(tool, "缺少 patch"));
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
        "file_manager" => {
            let op = call.op.as_deref().unwrap_or("").trim();
            if op.is_empty() {
                return Err(tool_format_error(tool, "缺少 op"));
            }
            match op {
                "list" | "stat" => {
                    // path 可以为空：默认 "."；执行阶段兜底即可。
                }
                "search" => {
                    let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
                    if pattern.is_empty() {
                        return Err(tool_format_error(tool, "search 缺少 pattern"));
                    }
                }
                "copy" | "move" => {
                    let src = call.src.as_deref().unwrap_or("").trim();
                    let dst = call.dst.as_deref().unwrap_or("").trim();
                    if src.is_empty() {
                        return Err(tool_format_error(tool, "缺少 src"));
                    }
                    if dst.is_empty() {
                        return Err(tool_format_error(tool, "缺少 dst"));
                    }
                }
                "trash" => {
                    let path = call
                        .path
                        .as_deref()
                        .or(call.src.as_deref())
                        .unwrap_or(call.input.trim())
                        .trim();
                    if path.is_empty() {
                        return Err(tool_format_error(tool, "缺少 path/src"));
                    }
                }
                "restore" => {
                    let id = call.trash_id.as_deref().unwrap_or("").trim();
                    if id.is_empty() {
                        return Err(tool_format_error(tool, "缺少 trash_id"));
                    }
                }
                _ => {
                    return Err(tool_format_error(
                        tool,
                        "op 仅支持 list/stat/search/copy/move/trash/restore",
                    ));
                }
            }
        }
        _ => {
            return Err(tool_format_error(
                tool,
                "未知工具（可用：bash/adb/termux_api/plan/list_dir/stat_file/read_file/write_file/search/edit_file/apply_patch/file_manager/memory_check/memory_read/memory_edit/memory_add/system_config/skills_mcp）",
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
        "work" | "plan" => run_work(call),
        "read_file" => run_read_file(call),
        "write_file" => run_write_file(call),
        "list_dir" => run_list_dir(call),
        "stat_file" => run_stat_file(call),
        "search" => run_search(call),
        "edit_file" => run_edit_file(call),
        "apply_patch" => run_apply_patch(call),
        "file_manager" => run_file_manager(call),
        "memory_check" => run_memory_check(call),
        "memory_read" => run_memory_read(call),
        "memory_edit" => run_memory_edit(call),
        "memory_add" => run_memory_add(call),
        "mind_msg" => run_mind_msg(call),
        "system_config" => run_system_config(call),
        "skills_mcp" => run_skills_mcp(call),
        other => Ok(ToolOutcome {
            user_message: format!("未知工具：{other}"),
            log_lines: vec!["状态:fail".to_string()],
        }),
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
        matches!(
            tool,
            "read_file"
                | "list_dir"
                | "stat_file"
                | "search"
                | "memory_check"
                | "memory_read"
                | "skills_mcp"
        )
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
                    if !timeout_retry_safe(call.tool.as_str()) {
                        break;
                    }
                    continue;
                }
                return outcome;
            }
            Err(e) => {
                last = Some(ToolOutcome {
                    user_message: format!("工具执行失败：{e:#}"),
                    log_lines: vec!["状态:fail".to_string()],
                });
            }
        }
    }
    let mut out = last.unwrap_or_else(|| ToolOutcome {
        user_message: "工具执行失败".to_string(),
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
        .and_then(|p| p.to_str().map(short_display_path))
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "(unknown)".to_owned())
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

fn format_bytes(bytes: u64) -> String {
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
                cwd_display = short_display_path(cwd);
                PathBuf::from(cwd)
            } else {
                PathBuf::from(".")
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

fn try_write_shell_cache_impl(dir: &str, path: &str, stdout: &[u8], stderr: &[u8]) -> bool {
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
        total_bytes > SHELL_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
            body.push_str("\n\n提示：bash 工具每次在独立的非交互 shell 中执行，`kill %1` 这类 job control 不生效。\n如需终止正在运行的 Terminal（PTY），请在 Terminal 视图 350ms 内快速连按两次 Esc（结束）或按 Ctrl+Home（强制结束）。");
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
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(meta) = saved_meta.as_ref() {
        log_lines.push(format!("saved:{}", meta.path));
        log_lines.push(format!("saved_bytes:{}", meta.bytes));
        log_lines.push(format!("saved_lines:{}", meta.lines));
    }
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
        total_bytes > ADB_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(meta) = saved_meta.as_ref() {
        log_lines.push(format!("saved:{}", meta.path));
        log_lines.push(format!("saved_bytes:{}", meta.bytes));
        log_lines.push(format!("saved_lines:{}", meta.lines));
    }
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
                    "状态:adb_offline | 耗时:{}ms | cwd:{cwd_display}",
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
                cwd_display = short_display_path(cwd);
                PathBuf::from(cwd)
            } else {
                PathBuf::from(".")
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
        total_bytes > TERMUX_API_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(meta) = saved_meta.as_ref() {
        log_lines.push(format!("saved:{}", meta.path));
        log_lines.push(format!("saved_bytes:{}", meta.bytes));
        log_lines.push(format!("saved_lines:{}", meta.lines));
    }

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
    let mut cwd_display = current_dir_display();
    let input = call.input.trim();
    if let Err(e) = validate_termux_api(input) {
        return Ok(ToolOutcome {
            user_message: e.to_string(),
            log_lines: vec![format!(
                "termux_api rejected -> {}",
                build_preview(input, 160)
            )],
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
    command
        .env("TERM", "xterm-256color")
        .env("COLORTERM", "truecolor")
        .env("LANG", "C.UTF-8")
        .env("LC_ALL", "C.UTF-8")
        .env("TERM_PROGRAM", "AItermux")
        .current_dir({
            if let Some(cwd) = call.cwd.as_deref().map(str::trim).filter(|s| !s.is_empty()) {
                cwd_display = short_display_path(cwd);
                PathBuf::from(cwd)
            } else {
                PathBuf::from(".")
            }
        });
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

fn run_work(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    // 这是一个“纯 UI 工具”：用于在聊天区显示“开始工作/工作结束”的目标与汇报区块。
    // 不执行任何命令、无副作用。
    let started = Instant::now();
    let phase_raw = call
        .section
        .as_deref()
        .unwrap_or("start")
        .trim()
        .to_ascii_lowercase();
    let phase = if matches!(
        phase_raw.as_str(),
        "end" | "done" | "finish" | "stop" | "report"
    ) {
        "end"
    } else {
        "start"
    };

    let input = call.input.trim();
    let content = call.content.as_deref().unwrap_or("").trim();
    let time = call.time.as_deref().unwrap_or("").trim();

    let mut body = String::new();
    let has_structured = {
        let lower = content.to_ascii_lowercase();
        lower.contains("target:") || lower.contains("steps:") || lower.contains("time:")
    };

    if has_structured && !content.is_empty() {
        body.push_str(content);
    } else if phase == "start" {
        let target = if !input.is_empty() {
            input
        } else if !content.is_empty() {
            content
        } else {
            call.brief.as_deref().unwrap_or("").trim()
        };
        if !target.is_empty() {
            body.push_str(&format!("Target: {target}\n"));
        }
        if !content.is_empty() && content != target {
            body.push_str("Steps:\n");
            for (idx, line) in content.lines().enumerate() {
                let t = line.trim();
                if t.is_empty() {
                    continue;
                }
                body.push_str(&format!("  {}. {}\n", idx.saturating_add(1), t));
            }
        }
        if !time.is_empty() {
            body.push_str(&format!("Time: {time}\n"));
        }
    } else {
        // end/report
        if !input.is_empty() {
            body.push_str(input);
            body.push('\n');
        }
        if !content.is_empty() {
            body.push_str(content);
            body.push('\n');
        }
        if !time.is_empty() {
            body.push_str(&format!("Time: {time}\n"));
        }
    }

    let body = body.trim_end().to_string();
    Ok(ToolOutcome {
        user_message: if body.is_empty() {
            "(empty)".to_string()
        } else {
            body
        },
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | phase:{}",
            started.elapsed().as_millis(),
            phase
        )],
    })
}

fn count_text_lines(text: &str) -> usize {
    text.lines().count()
}

fn count_file_lines(path: &str) -> Option<usize> {
    let file = fs::File::open(path).ok()?;
    let mut reader = std::io::BufReader::new(file);
    let mut buf: Vec<u8> = Vec::new();
    let mut count = 0usize;
    loop {
        buf.clear();
        let n = reader.read_until(b'\n', &mut buf).ok()?;
        if n == 0 {
            break;
        }
        count = count.saturating_add(1);
    }
    Some(count)
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

fn run_read_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    if call.memory.unwrap_or(false) {
        return run_read_memory_like("read_file", call);
    }
    #[derive(Clone, Copy)]
    enum ReadMode {
        Peek,
        Range,
        Full,
    }

    fn estimate_tokens_for_text(text: &str) -> usize {
        let mut ascii = 0usize;
        let mut non_ascii = 0usize;
        for ch in text.chars() {
            if ch.is_ascii() {
                ascii = ascii.saturating_add(1);
            } else {
                non_ascii = non_ascii.saturating_add(1);
            }
        }
        non_ascii.saturating_add(ascii.saturating_add(3) / 4)
    }

    fn trim_line_for_read(raw: &str) -> (String, bool) {
        let clean = sanitize_search_line(raw.trim_end_matches(['\n', '\r']));
        let chars = clean.chars().count();
        if chars <= READ_MAX_LINE_CHARS {
            return (clean, false);
        }
        let mut out: String = clean.chars().take(READ_MAX_LINE_CHARS).collect();
        out.push_str(" …");
        (out, true)
    }

    fn count_file_lines_limited(path: &str, max_bytes: usize) -> (usize, bool) {
        let Ok(file) = fs::File::open(path) else {
            return (0, false);
        };
        let mut reader = std::io::BufReader::new(file);
        let mut buf: Vec<u8> = Vec::new();
        let mut bytes = 0usize;
        let mut lines = 0usize;
        loop {
            buf.clear();
            let Ok(n) = reader.read_until(b'\n', &mut buf) else {
                return (lines, false);
            };
            if n == 0 {
                return (lines, true);
            }
            bytes = bytes.saturating_add(n);
            lines = lines.saturating_add(1);
            if bytes >= max_bytes {
                return (lines, false);
            }
        }
    }

    fn read_head_lines(
        path: &str,
        max_lines: usize,
    ) -> anyhow::Result<(Vec<(usize, String)>, bool)> {
        let file = fs::File::open(path).with_context(|| format!("读取文件失败：{path}"))?;
        let mut reader = std::io::BufReader::new(file);
        let mut buf: Vec<u8> = Vec::new();
        let mut out: Vec<(usize, String)> = Vec::new();
        let mut truncated = false;
        let mut line_no = 0usize;
        while out.len() < max_lines {
            buf.clear();
            let n = reader
                .read_until(b'\n', &mut buf)
                .context("读取文件内容失败")?;
            if n == 0 {
                break;
            }
            line_no = line_no.saturating_add(1);
            let line = String::from_utf8_lossy(&buf);
            let (t, was_trunc) = trim_line_for_read(&line);
            if was_trunc {
                truncated = true;
            }
            out.push((line_no, t));
        }
        Ok((out, truncated))
    }

    fn read_tail_lines(
        path: &str,
        total_bytes: usize,
        tail_lines: usize,
    ) -> anyhow::Result<(Vec<String>, bool, bool)> {
        let mut file = fs::File::open(path).with_context(|| format!("读取文件失败：{path}"))?;
        let mut truncated_by_bytes = false;
        if total_bytes > READ_MAX_BYTES {
            truncated_by_bytes = true;
            let offset = total_bytes.saturating_sub(READ_MAX_BYTES) as u64;
            file.seek(std::io::SeekFrom::Start(offset))
                .context("尾部读取 seek 失败")?;
        }
        let mut buf: Vec<u8> = Vec::new();
        std::io::Read::take(&mut file, READ_MAX_BYTES as u64)
            .read_to_end(&mut buf)
            .context("读取文件内容失败")?;
        let text = String::from_utf8_lossy(&buf);
        let mut lines: Vec<&str> = text.lines().collect();
        if lines.len() > tail_lines {
            lines = lines[lines.len().saturating_sub(tail_lines)..].to_vec();
        }
        let mut out: Vec<String> = Vec::new();
        let mut truncated_line = false;
        for line in lines {
            let (t, was_trunc) = trim_line_for_read(line);
            if was_trunc {
                truncated_line = true;
            }
            out.push(t);
        }
        Ok((out, truncated_by_bytes, truncated_line))
    }

    let path = pick_path(call)?;
    let started = Instant::now();
    let head_mode = call.head.unwrap_or(false) && !call.tail.unwrap_or(false);
    let tail_mode = call.tail.unwrap_or(false);
    let full_requested = call.full.unwrap_or(false);
    let range_requested =
        head_mode || tail_mode || call.start_line.is_some() || call.max_lines.is_some();

    let meta0 = fs::metadata(&path).with_context(|| format!("读取文件失败：{path}"))?;
    let total_bytes = meta0.len() as usize;
    let modified = meta0.modified().ok();
    let modified_ms = modified
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as u64);
    let is_binary = is_probably_binary_file(Path::new(&path));

    let mut mode = if range_requested {
        ReadMode::Range
    } else if full_requested {
        ReadMode::Full
    } else {
        ReadMode::Peek
    };

    let mut full_note: Option<String> = None;
    if full_requested && range_requested {
        full_note = Some("ignored: range 参数已提供（按 range 处理）".to_string());
    }
    if matches!(mode, ReadMode::Full) && total_bytes > READ_FULL_MAX_BYTES {
        // full 仅允许小文件；大文件降级为 Peek（并给出更丰富 meta 信息）。
        full_note = Some(format!(
            "denied: full 仅允许 <= {READ_FULL_MAX_BYTES} bytes"
        ));
        mode = ReadMode::Peek;
    }

    let token_budget = match mode {
        ReadMode::Peek => READ_PEEK_TOKEN_BUDGET,
        ReadMode::Range => READ_RANGE_TOKEN_BUDGET,
        ReadMode::Full => READ_FULL_TOKEN_BUDGET,
    };

    // 额外的“读文件信息”：最多扫 2MB 估算行数，避免对超大文件全扫卡住。
    let (total_lines_est, total_lines_complete) = if total_bytes <= 2_000_000 {
        count_file_lines_limited(&path, 2_000_000)
    } else {
        (0, false)
    };

    let mut out_lines: Vec<String> = Vec::new();
    out_lines.push(format!("File: {}", shorten_path(&path)));
    out_lines.push(format!("Size: {total_bytes} bytes"));
    if let Some(ms) = modified_ms {
        out_lines.push(format!("MTime(ms): {ms}"));
    }
    if total_lines_est > 0 {
        if total_lines_complete {
            out_lines.push(format!("Total lines: {total_lines_est}"));
        } else {
            out_lines.push(format!("Total lines(estimated): >= {total_lines_est}"));
        }
    }
    out_lines.push(format!(
        "Binary: {}",
        if is_binary { "true" } else { "false" }
    ));
    if full_requested {
        if let Some(note) = full_note.as_deref() {
            out_lines.push(format!("Full requested: true ({note})"));
        } else {
            out_lines.push("Full requested: true".to_string());
        }
    }
    out_lines.push("---".to_string());

    let mut partial = false;
    let mut partial_reason: Option<&'static str> = None;
    let mut truncated_line = false;
    let mut shown_start: Option<usize> = None;
    let mut shown_end: Option<usize> = None;

    if is_binary {
        out_lines.push("(binary file: content omitted)".to_string());
        partial = true;
        partial_reason = Some("binary");
    } else if matches!(mode, ReadMode::Peek) {
        let (head, head_trunc) = read_head_lines(&path, READ_PEEK_HEAD_LINES)?;
        let (tail, tail_trunc_bytes, tail_trunc_line) =
            read_tail_lines(&path, total_bytes, READ_PEEK_TAIL_LINES)?;
        truncated_line = head_trunc || tail_trunc_line;
        let _tail_window_limited = tail_trunc_bytes;

        out_lines.push(format!("(peek) head {READ_PEEK_HEAD_LINES} lines:"));
        for (ln, text) in head {
            out_lines.push(format!("{ln} | {text}"));
        }
        out_lines.push("...".to_string());
        out_lines.push(format!("(peek) tail {READ_PEEK_TAIL_LINES} lines:"));
        for text in tail {
            out_lines.push(text);
        }
        partial = true;
        partial_reason = Some("peek");
    } else {
        // Range / Full（小文件 full 也走这里）：按行流式扫描，支持 start_line/max_lines。
        let mut start_line = if tail_mode {
            0usize
        } else if matches!(mode, ReadMode::Full) || head_mode {
            1usize
        } else {
            call.start_line.unwrap_or(1).max(1)
        };
        let mut max_lines = call.max_lines.unwrap_or(200).max(1);
        if matches!(mode, ReadMode::Full) {
            start_line = 1;
            max_lines = call
                .max_lines
                .unwrap_or(READ_FULL_MAX_LINES)
                .max(1)
                .min(READ_FULL_MAX_LINES);
        } else {
            max_lines = max_lines.min(READ_RANGE_MAX_LINES);
        }

        if tail_mode {
            let (tail, tail_trunc_bytes, tail_trunc_line) =
                read_tail_lines(&path, total_bytes, max_lines)?;
            truncated_line = tail_trunc_line;
            if tail_trunc_bytes {
                partial = true;
                partial_reason = Some("tail_bytes_window");
            }
            out_lines.push(format!(
                "(tail) last {max_lines} lines (line numbers unknown):"
            ));
            for text in tail {
                out_lines.push(text);
            }
        } else {
            let file = fs::File::open(&path).with_context(|| format!("读取文件失败：{path}"))?;
            let mut reader = std::io::BufReader::new(file);
            let mut buf: Vec<u8> = Vec::new();
            let mut bytes_scanned = 0usize;
            let mut line_no = 0usize;
            let mut collected = 0usize;
            let width = (start_line + max_lines).to_string().len().max(3);
            while collected < max_lines {
                buf.clear();
                let n = reader
                    .read_until(b'\n', &mut buf)
                    .context("读取文件内容失败")?;
                if n == 0 {
                    break;
                }
                bytes_scanned = bytes_scanned.saturating_add(n);
                line_no = line_no.saturating_add(1);
                if bytes_scanned >= READ_SCAN_MAX_BYTES && line_no < start_line {
                    partial = true;
                    partial_reason = Some("scan_bytes_cap");
                    break;
                }
                if line_no < start_line {
                    continue;
                }
                let line = String::from_utf8_lossy(&buf);
                let (t, was_trunc) = trim_line_for_read(&line);
                if was_trunc {
                    truncated_line = true;
                }
                shown_start.get_or_insert(line_no);
                shown_end = Some(line_no);
                out_lines.push(format!("{:>width$} | {}", line_no, t, width = width));
                collected = collected.saturating_add(1);
            }
            if line_no >= start_line && collected >= max_lines {
                // 触发了 max_lines 上限：仅当文件确实还有更多内容时才算 partial，避免“刚好读完整文件”的误判。
                buf.clear();
                let more = reader.read_until(b'\n', &mut buf).unwrap_or(0) > 0;
                if more {
                    partial = true;
                    partial_reason = Some("max_lines_cap");
                }
            }
        }
    }

    // 预算裁剪：token budget + char budget 双兜底（并明确 partial）。
    let mut budget_tokens_used = 0usize;
    let mut budget_chars_used = 0usize;
    let mut clipped: Vec<String> = Vec::new();
    for line in out_lines {
        let line_chars = line.chars().count();
        let line_tokens = estimate_tokens_for_text(&line);
        if !clipped.is_empty() {
            // newline cost
            budget_chars_used = budget_chars_used.saturating_add(1);
        }
        if budget_tokens_used.saturating_add(line_tokens) > token_budget
            || budget_chars_used.saturating_add(line_chars) > READ_TOOL_RAW_MAX_CHARS
            || clipped.len().saturating_add(1) > READ_TOOL_RAW_MAX_LINES
        {
            partial = true;
            partial_reason = Some("token_budget");
            break;
        }
        budget_tokens_used = budget_tokens_used.saturating_add(line_tokens);
        budget_chars_used = budget_chars_used.saturating_add(line_chars);
        clipped.push(line);
    }

    if partial {
        let reason = partial_reason.unwrap_or("partial");
        clipped.push("---".to_string());
        clipped.push(format!(
            "[partial:true reason:{reason} | token_budget:{token_budget} | est_tokens:{budget_tokens_used}]"
        ));
        if let (Some(a), Some(b)) = (shown_start, shown_end) {
            clipped.push(format!("[window: lines {a}-{b}]"));
        }
        if truncated_line {
            clipped.push(format!(
                "[note: some lines were truncated to {READ_MAX_LINE_CHARS} chars]"
            ));
        }
    }

    let elapsed = started.elapsed();
    let body = clipped.join("\n").trim_end().to_string();
    let line_count = body.lines().count();
    let char_count = body.chars().count();

    let mode_label = match mode {
        ReadMode::Peek => "peek",
        ReadMode::Range => "range",
        ReadMode::Full => "full",
    };
    let mut meta = format!(
        "状态:0 | 耗时:{}ms | path:{} | mode:{mode_label} | bytes:{total_bytes} | lines:{} | chars:{}",
        elapsed.as_millis(),
        shorten_path(&path),
        line_count,
        char_count
    );
    if total_lines_est > 0 {
        if total_lines_complete {
            meta.push_str(&format!(" | total_lines:{total_lines_est}"));
        } else {
            meta.push_str(&format!(" | total_lines:>={total_lines_est}"));
        }
    }
    if partial {
        meta.push_str(" | partial:true");
        if let Some(r) = partial_reason {
            meta.push_str(&format!(" | reason:{r}"));
        }
    }
    if is_binary {
        meta.push_str(" | binary:true");
    }
    if truncated_line {
        meta.push_str(" | line_truncated:true");
    }
    meta.push_str(&format!(" | est_tokens:{budget_tokens_used}"));
    if let Some(note) = full_note.as_deref() {
        meta.push_str(&format!(" | full_note:{note}"));
    }

    Ok(ToolOutcome {
        user_message: body,
        log_lines: vec![meta],
    })
}

fn run_write_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let (path, content) = pick_write_fields(call)?;
    if content.len() > WRITE_MAX_BYTES {
        return Ok(ToolOutcome {
            user_message: format!("安全限制：写入内容过大（>{WRITE_MAX_BYTES} bytes）。"),
            log_lines: vec![],
        });
    }

    fn atomic_write_text(
        path: &str,
        content: &str,
        preserve_mode: Option<u32>,
    ) -> anyhow::Result<()> {
        let p = Path::new(path);
        let parent = p.parent().unwrap_or_else(|| Path::new("."));
        let file_name = p
            .file_name()
            .and_then(|s| s.to_str())
            .filter(|s| !s.is_empty())
            .unwrap_or("file");
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let mut last_err: Option<anyhow::Error> = None;
        for seq in 0..10usize {
            let tmp_name = format!(".{file_name}.tmp_{ts}_{pid}_{seq}");
            let tmp_path = parent.join(tmp_name);
            let opened = fs::OpenOptions::new()
                .create_new(true)
                .write(true)
                .open(&tmp_path);
            let mut f = match opened {
                Ok(f) => f,
                Err(e) => {
                    last_err = Some(anyhow!(e).context("创建临时文件失败"));
                    continue;
                }
            };
            if let Err(e) = f.write_all(content.as_bytes()) {
                let _ = fs::remove_file(&tmp_path);
                return Err(anyhow!(e).context("写入临时文件失败"));
            }
            let _ = f.flush();
            let _ = f.sync_all();
            drop(f);

            #[cfg(unix)]
            if let Some(mode) = preserve_mode {
                let _ = fs::set_permissions(&tmp_path, fs::Permissions::from_mode(mode));
            }

            if let Err(e) = fs::rename(&tmp_path, p) {
                let _ = fs::remove_file(&tmp_path);
                return Err(anyhow!(e).context("原子替换失败（rename）"));
            }

            #[cfg(unix)]
            if let Ok(dirf) = fs::File::open(parent) {
                let _ = dirf.sync_all();
            }
            return Ok(());
        }
        Err(last_err.unwrap_or_else(|| anyhow!("创建临时文件失败（重试耗尽）")))
    }

    let started = Instant::now();
    let overwrite = call.overwrite.unwrap_or(false);
    let old_meta = fs::metadata(&path).ok().filter(|m| m.is_file());
    let old_bytes = old_meta.as_ref().map(|m| m.len() as usize).unwrap_or(0);
    #[cfg(unix)]
    let old_mode: Option<u32> = old_meta.as_ref().map(|m| m.permissions().mode());
    #[cfg(not(unix))]
    let old_mode: Option<u32> = None;
    let old_lines = count_file_lines(&path).unwrap_or(0);
    let old_chars = if old_bytes <= READ_MAX_BYTES {
        fs::read_to_string(&path)
            .ok()
            .map(|s| s.chars().count())
            .unwrap_or(old_bytes)
    } else {
        old_bytes
    };
    let mut backup_path: Option<String> = None;
    if old_meta.is_some() && old_bytes > 0 && !overwrite {
        return Ok(ToolOutcome {
            user_message: format!(
                "目标文件已存在且非空：{path}\n为避免误覆盖，请显式设置 overwrite:true。\n（覆盖前会自动备份旧文件到 {WRITE_BACKUP_DIR}/）"
            ),
            log_lines: vec![format!(
                "状态:ok_need_confirm | path:{} | bytes:{} | overwrite:0",
                shorten_path(&path),
                old_bytes
            )],
        });
    }
    // 非空覆盖：自动备份旧文件到 log/backupcache
    if old_meta.is_some() && old_bytes > 0 && overwrite {
        let _ = fs::create_dir_all(WRITE_BACKUP_DIR);
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let name = Path::new(&path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("file");
        let backup = format!("{WRITE_BACKUP_DIR}/write_{ts}_{pid}_{name}.bak");
        match fs::copy(&path, &backup) {
            Ok(_) => backup_path = Some(backup),
            Err(e) => {
                return Ok(ToolOutcome {
                    user_message: format!("备份失败，已取消写入：{path}\n原因：{e}"),
                    log_lines: vec![format!(
                        "状态:fail | phase:backup_failed | path:{} | bytes:{}",
                        shorten_path(&path),
                        old_bytes
                    )],
                });
            }
        }
    }
    if let Some(parent) = Path::new(&path).parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建目录失败：{}", parent.display()))?;
    }
    atomic_write_text(&path, &content, old_mode).with_context(|| format!("写入失败：{path}"))?;
    let elapsed = started.elapsed();
    let new_bytes = content.len();
    let new_lines = count_text_lines(&content);
    let new_chars = content.chars().count();

    let preview = build_preview(&content, 160);
    let mut msg = String::new();
    msg.push_str(&format!(
        "已写入 {path}（{new_lines} 行 / {new_chars} 字符）"
    ));
    if let Some(bk) = backup_path.as_deref() {
        msg.push_str(&format!("\nbackup: {bk}"));
    }
    msg.push_str(&format!("\n预览: {preview}"));

    let delta = if old_lines > 1 || new_lines > 1 {
        format!("delta_lines:+{} -{}", new_lines, old_lines)
    } else {
        format!("delta_chars:+{} -{}", new_chars, old_chars)
    };

    let mut log_lines = vec![format!(
        "状态:0 | 耗时:{}ms | path:{} | bytes:{} | lines:{} | chars:{} | overwrite:{} | {delta}",
        elapsed.as_millis(),
        shorten_path(&path),
        new_bytes,
        new_lines,
        new_chars,
        if overwrite { 1 } else { 0 }
    )];
    if let Some(bk) = backup_path.as_deref() {
        log_lines.push(format!("backup:{}", bk));
    }
    Ok(ToolOutcome {
        user_message: msg,
        log_lines,
    })
}

fn run_list_dir(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let path = pick_path(call)?;
    let started = Instant::now();
    let p = Path::new(&path);
    if !p.exists() {
        return Ok(ToolOutcome {
            user_message: format!("路径不存在：{path}"),
            log_lines: vec![format!(
                "状态:ok_not_found | 耗时:{}ms | path:{}",
                started.elapsed().as_millis(),
                shorten_path(&path)
            )],
        });
    }

    fn fmt_mtime(meta: &fs::Metadata) -> String {
        meta.modified()
            .ok()
            .and_then(|t| {
                let dt: chrono::DateTime<Local> = t.into();
                Some(dt.format("%Y-%m-%d %H:%M").to_string())
            })
            .unwrap_or_else(|| "unknown".to_string())
    }

    fn fmt_kind(meta: &fs::Metadata) -> &'static str {
        if meta.is_dir() {
            "dir"
        } else if meta.is_file() {
            "file"
        } else {
            "other"
        }
    }

    fn abs_path(cwd: &Path, raw: &str) -> PathBuf {
        let p = Path::new(raw);
        if p.is_absolute() {
            p.to_path_buf()
        } else {
            cwd.join(p)
        }
    }

    // 合并 stat_file 能力：path 是文件时，直接输出单条“增强版 list”。
    if p.is_file() {
        let meta = fs::metadata(p).with_context(|| format!("获取文件信息失败：{path}"))?;
        let elapsed = started.elapsed();
        let bytes = meta.len();
        let body = format!(
            "{} | path:{} | size:{} ({}) | mtime:{} | readonly:{}",
            fmt_kind(&meta),
            shorten_path(&path),
            bytes,
            format_bytes(bytes),
            fmt_mtime(&meta),
            meta.permissions().readonly()
        );
        return Ok(ToolOutcome {
            user_message: body,
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | type:file",
                elapsed.as_millis(),
                shorten_path(&path)
            )],
        });
    }

    let depth = call.depth.unwrap_or(1).clamp(1, LIST_MAX_DEPTH_CAP);
    let max_entries = call
        .max_entries
        .unwrap_or(LIST_MAX_ENTRIES_DEFAULT)
        .clamp(1, LIST_MAX_ENTRIES_CAP);

    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let abs = abs_path(&cwd, &path);
    let real = fs::canonicalize(&abs).unwrap_or(abs.clone());

    let mut out: Vec<String> = Vec::new();
    out.push(format!("cwd: {}", shorten_path(&cwd.to_string_lossy())));
    out.push(format!("path: {path}"));
    out.push(format!("real: {}", shorten_path(&real.to_string_lossy())));
    out.push(format!("depth: {depth} | entry_cap: {max_entries}"));
    out.push(String::new());

    let mut produced = 0usize;
    let mut truncated = false;

    fn join_rel(prefix: &str, name: &str) -> String {
        if prefix.is_empty() {
            name.to_string()
        } else {
            format!("{prefix}/{name}")
        }
    }

    fn list_one_dir(
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
                let here = if rel_prefix.is_empty() {
                    ".".to_string()
                } else {
                    format!("{rel_prefix}/")
                };
                out.push(format!(
                    "dir | size:- | mtime:unknown | readonly:? | {here} [unreadable]"
                ));
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
            let (kind, bytes, mtime, ro) = if let Some(m) = meta.as_ref() {
                (
                    fmt_kind(m),
                    m.len(),
                    fmt_mtime(m),
                    m.permissions().readonly(),
                )
            } else {
                ("other", 0u64, "unknown".to_string(), false)
            };
            let rel = join_rel(rel_prefix, &name);
            let display = if kind == "dir" {
                format!("{rel}/")
            } else {
                rel
            };
            let size_part = if kind == "file" {
                format!("size:{} ({})", bytes, format_bytes(bytes))
            } else {
                "size:-".to_string()
            };
            out.push(format!(
                "{kind} | {size_part} | mtime:{mtime} | readonly:{ro} | {display}"
            ));
            *produced = produced.saturating_add(1);

            if kind == "dir" && level < max_level && !*truncated {
                let next_prefix = join_rel(rel_prefix, &name);
                list_one_dir(
                    &ep,
                    &next_prefix,
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

    list_one_dir(
        p,
        "",
        1,
        depth,
        max_entries,
        &mut produced,
        &mut truncated,
        &mut out,
    )?;
    if truncated {
        out.push(String::new());
        out.push(format!(
            "[truncated:true entries:{produced} entry_cap:{max_entries} depth:{depth}]"
        ));
    }
    let elapsed = started.elapsed();
    let mut body = out.join("\n");
    if body.is_empty() {
        body = "(empty)".to_string();
    }
    let total_entries = produced;
    let truncated_by_lines = total_entries > OUTPUT_MAX_LINES;
    let truncated_by_chars = body.chars().count() > OUTPUT_MAX_CHARS;

    Ok(ToolOutcome {
        user_message: truncate_command_output(body),
        log_lines: vec![{
            let mut meta = format!(
                "状态:0 | 耗时:{}ms | path:{} | entries:{}",
                elapsed.as_millis(),
                shorten_path(&path),
                total_entries
            );
            if truncated_by_lines || truncated_by_chars {
                meta.push_str(&format!(
                    " | truncated:true | limit:{} lines/{} chars",
                    OUTPUT_MAX_LINES, OUTPUT_MAX_CHARS
                ));
            }
            meta
        }],
    })
}

fn run_stat_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    // stat_file 已并入“增强版 list_dir”（list_dir 对文件路径会输出单条信息）。
    run_list_dir(call)
}

fn run_search(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
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
            out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
            log.push_str(&format!(
                " | saved:{} | saved_bytes:{} | saved_lines:{}",
                meta.path, meta.bytes, meta.lines
            ));
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
        log.push_str(&format!(
            " | saved:{} | saved_bytes:{} | saved_lines:{}",
            meta.path, meta.bytes, meta.lines
        ));
    }

    Ok(ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    })
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
        if label == "fastmemo" {
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
                let (body2, saved) = maybe_export_text(
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
                if let Some(meta) = saved.as_ref() {
                    out.push_str(&format!("\n[saved:{}]\n", meta.path));
                }
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
        log.push_str(&format!(
            " | saved:{} | saved_bytes:{} | saved_lines:{}",
            meta.path, meta.bytes, meta.lines
        ));
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
        out.push_str(&format!("\n\n[saved:{}]", meta.path));
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
        log.push_str(&format!(
            " | saved:{} | saved_bytes:{} | saved_lines:{}",
            meta.path, meta.bytes, meta.lines
        ));
    }

    ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    }
}

fn run_edit_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
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

fn run_apply_patch(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
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
    if !patch_text.ends_with('\n') {
        patch_text.push('\n');
    }
    if let Err(err) = validate_unified_patch(&patch_text) {
        return Ok(ToolOutcome {
            user_message: format!("补丁格式错误：{}（第{}行）", err.reason, err.line),
            log_lines: vec![format!("状态:format_error | line:{}", err.line)],
        });
    }
    let started = Instant::now();
    let (add, del) = count_patch_changes(&patch_text);
    let strip = if patch_text.contains("\n--- a/") || patch_text.starts_with("--- a/") {
        1
    } else {
        0
    };
    let strict = call.strict.unwrap_or(false);
    if strict {
        let (code, stdout, stderr, timed_out) = run_patch_command(&patch_text, strip, true)?;
        let elapsed = started.elapsed();
        let body = annotate_timeout(
            truncate_command_output(collect_output(&stdout, &stderr)),
            timed_out,
            None,
        );
        let report = parse_patch_report(&body);
        if timed_out {
            return Ok(ToolOutcome {
                user_message: "补丁超时（严格模式未应用）".to_string(),
                log_lines: vec![format!(
                    "状态:timeout | 耗时:{}ms | strip:-p{strip} | delta_lines:+{} -{} | result:timeout",
                    elapsed.as_millis(),
                    add,
                    del
                )],
            });
        }
        if code != 0 || report.failed {
            return Ok(ToolOutcome {
                user_message: format!("补丁失败（严格模式未应用）\n{body}"),
                log_lines: vec![format!(
                    "状态:fail | 耗时:{}ms | strip:-p{strip} | delta_lines:+{} -{} | result:fail",
                    elapsed.as_millis(),
                    add,
                    del
                )],
            });
        }
        if report.fuzz || report.offset {
            return Ok(ToolOutcome {
                user_message: format!("补丁命中不精确（严格模式拒绝）\n{body}"),
                log_lines: vec![format!(
                    "状态:ok_fuzz | 耗时:{}ms | strip:-p{strip} | delta_lines:+{} -{} | result:ok_fuzz",
                    elapsed.as_millis(),
                    add,
                    del
                )],
            });
        }
    }

    let (code, stdout, stderr, timed_out) = run_patch_command(&patch_text, strip, false)?;
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
    } else if success && (report.fuzz || report.offset) {
        "ok_fuzz"
    } else if success && !report.failed {
        "ok"
    } else {
        "fail"
    };
    let summary = if timed_out {
        "补丁超时"
    } else if result_tag == "ok_fuzz" {
        "补丁成功（含差异）"
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
            "状态:{status} | 耗时:{}ms | strip:-p{strip} | delta_lines:+{} -{} | result:{}",
            elapsed.as_millis(),
            add,
            del,
            result_tag
        )],
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TrashMeta {
    trash_id: String,
    created_at: String,
    src: String,
    payload: String,
    kind: String,
    method: String,
    note: Option<String>,
}

fn file_manager_make_trash_id() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static SEQ: AtomicUsize = AtomicUsize::new(0);
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S_%3f");
    let pid = unsafe { libc::getpid() };
    let seq = SEQ.fetch_add(1, Ordering::Relaxed);
    format!("trash_{ts}_{pid}_{seq}")
}

fn file_manager_recycle_dir() -> PathBuf {
    PathBuf::from(FILE_MANAGER_RECYCLE_DIR)
}

fn file_manager_is_exdev(err: &std::io::Error) -> bool {
    #[cfg(unix)]
    {
        err.raw_os_error() == Some(libc::EXDEV)
    }
    #[cfg(not(unix))]
    {
        let _ = err;
        false
    }
}

fn file_manager_meta_path(trash_id: &str) -> PathBuf {
    file_manager_recycle_dir().join(trash_id).join("meta.json")
}

fn file_manager_payload_path(trash_id: &str) -> PathBuf {
    file_manager_recycle_dir().join(trash_id).join("payload")
}

fn file_manager_write_trash_meta(meta: &TrashMeta) -> anyhow::Result<()> {
    let meta_path = file_manager_meta_path(&meta.trash_id);
    if let Some(parent) = meta_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建回收站目录失败：{}", parent.display()))?;
    }
    let json = serde_json::to_string_pretty(meta).context("序列化回收站 meta 失败")?;
    fs::write(&meta_path, json)
        .with_context(|| format!("写入回收站 meta 失败：{}", meta_path.display()))?;
    Ok(())
}

fn file_manager_read_trash_meta(trash_id: &str) -> anyhow::Result<TrashMeta> {
    let meta_path = file_manager_meta_path(trash_id);
    let raw = fs::read_to_string(&meta_path)
        .with_context(|| format!("读取回收站 meta 失败：{}", meta_path.display()))?;
    let meta: TrashMeta = serde_json::from_str(&raw).context("解析回收站 meta 失败")?;
    Ok(meta)
}

#[derive(Debug, Default)]
struct CopyStats {
    files: usize,
    dirs: usize,
    bytes: u64,
}

fn copy_recursive(src: &Path, dst: &Path, stats: &mut CopyStats) -> anyhow::Result<()> {
    let meta = fs::symlink_metadata(src)
        .with_context(|| format!("读取源路径信息失败：{}", src.display()))?;
    if meta.is_dir() {
        fs::create_dir_all(dst).with_context(|| format!("创建目录失败：{}", dst.display()))?;
        stats.dirs = stats.dirs.saturating_add(1);
        for entry in
            fs::read_dir(src).with_context(|| format!("读取目录失败：{}", src.display()))?
        {
            let entry = entry?;
            let name = entry.file_name();
            let child_src = entry.path();
            let child_dst = dst.join(name);
            copy_recursive(&child_src, &child_dst, stats)?;
        }
    } else {
        if let Some(parent) = dst.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("创建目录失败：{}", parent.display()))?;
        }
        let copied = fs::copy(src, dst)
            .with_context(|| format!("复制失败：{} -> {}", src.display(), dst.display()))?;
        stats.files = stats.files.saturating_add(1);
        stats.bytes = stats.bytes.saturating_add(copied);
    }
    Ok(())
}

fn backup_existing_path_to_recycle(path: &Path) -> anyhow::Result<(String, String)> {
    // 返回 (trash_id, method)
    let trash_id = file_manager_make_trash_id();
    let recycle = file_manager_recycle_dir();
    fs::create_dir_all(&recycle)
        .with_context(|| format!("创建回收站目录失败：{}", recycle.display()))?;
    let id_dir = recycle.join(&trash_id);
    fs::create_dir_all(&id_dir)
        .with_context(|| format!("创建回收站子目录失败：{}", id_dir.display()))?;
    let payload = file_manager_payload_path(&trash_id);

    let kind = if fs::metadata(path).map(|m| m.is_dir()).unwrap_or(false) {
        "dir"
    } else {
        "file"
    };
    let created_at = chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    let src_display = path.to_string_lossy().to_string();
    let payload_display = payload.to_string_lossy().to_string();

    match fs::rename(path, &payload) {
        Ok(_) => {
            let meta = TrashMeta {
                trash_id: trash_id.clone(),
                created_at,
                src: src_display,
                payload: payload_display,
                kind: kind.to_string(),
                method: "rename".to_string(),
                note: None,
            };
            file_manager_write_trash_meta(&meta)?;
            return Ok((trash_id, "rename".to_string()));
        }
        Err(e) => {
            if !file_manager_is_exdev(&e) {
                return Err(anyhow!(e).context("回收站 rename 失败"));
            }
            // EXDEV：退化为 copy（不做删除）
            let mut stats = CopyStats::default();
            copy_recursive(path, &payload, &mut stats)?;
            let meta = TrashMeta {
                trash_id: trash_id.clone(),
                created_at,
                src: src_display,
                payload: payload_display,
                kind: kind.to_string(),
                method: "copy_only".to_string(),
                note: Some("EXDEV: source not removed".to_string()),
            };
            file_manager_write_trash_meta(&meta)?;
            Ok((trash_id, "copy_only".to_string()))
        }
    }
}

fn rename_to_local_backup(path: &Path) -> anyhow::Result<PathBuf> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("item");
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
    let pid = unsafe { libc::getpid() };
    for seq in 0..20usize {
        let backup_name = format!(".bak_{name}_{ts}_{pid}_{seq}");
        let backup_path = parent.join(&backup_name);
        if backup_path.exists() {
            continue;
        }
        fs::rename(path, &backup_path).with_context(|| {
            format!(
                "本地备份 rename 失败：{} -> {}",
                path.display(),
                backup_path.display()
            )
        })?;
        return Ok(backup_path);
    }
    Err(anyhow!("生成本地备份文件名失败（重试耗尽）"))
}

fn run_file_manager(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let started = Instant::now();
    let op = call.op.as_deref().unwrap_or("").trim().to_ascii_lowercase();

    fn pick_op_path(call: &ToolCall) -> String {
        call.path
            .as_deref()
            .or(call.src.as_deref())
            .unwrap_or(call.input.trim())
            .trim()
            .to_string()
    }

    match op.as_str() {
        "list" => {
            let mut inner = call.clone();
            inner.tool = "list_dir".to_string();
            let raw = pick_op_path(call);
            inner.path = Some(if raw.trim().is_empty() {
                ".".to_string()
            } else {
                raw
            });
            run_list_dir(&inner)
        }
        "stat" => {
            let mut inner = call.clone();
            inner.tool = "stat_file".to_string();
            let raw = pick_op_path(call);
            inner.path = Some(if raw.trim().is_empty() {
                ".".to_string()
            } else {
                raw
            });
            run_stat_file(&inner)
        }
        "search" => {
            let mode = call
                .mode
                .as_deref()
                .unwrap_or("content")
                .trim()
                .to_ascii_lowercase();
            let pattern = call
                .pattern
                .as_deref()
                .unwrap_or(call.input.trim())
                .trim()
                .to_string();
            let root = call
                .root
                .as_deref()
                .or(call.path.as_deref())
                .or(call.src.as_deref())
                .unwrap_or(".")
                .trim()
                .to_string();

            if mode == "both" {
                let mut a = call.clone();
                a.tool = "search".to_string();
                a.pattern = Some(pattern.clone());
                a.root = Some(root.clone());
                a.file = Some(false);
                let out_a = run_search(&a)?;

                let mut b = call.clone();
                b.tool = "search".to_string();
                b.pattern = Some(pattern);
                b.root = Some(root);
                b.file = Some(true);
                let out_b = run_search(&b)?;

                let mut body = String::new();
                body.push_str("[content]\n");
                body.push_str(out_a.user_message.trim_end());
                body.push_str("\n\n[name]\n");
                body.push_str(out_b.user_message.trim_end());

                let mut meta = format!(
                    "状态:0 | 耗时:{}ms | op:search | mode:both",
                    started.elapsed().as_millis()
                );
                // 附带子工具状态（便于排障，但不影响 UI “失败”判断）
                for line in out_a.log_lines.iter().take(2) {
                    if line.trim_start().starts_with("状态:") {
                        meta.push_str(&format!(" | content_{}", line.trim()));
                        break;
                    }
                }
                for line in out_b.log_lines.iter().take(2) {
                    if line.trim_start().starts_with("状态:") {
                        meta.push_str(&format!(" | name_{}", line.trim()));
                        break;
                    }
                }
                return Ok(ToolOutcome {
                    user_message: truncate_command_output(body),
                    log_lines: vec![meta],
                });
            }

            let mut inner = call.clone();
            inner.tool = "search".to_string();
            inner.pattern = Some(pattern);
            inner.root = Some(root);
            inner.file = Some(matches!(mode.as_str(), "name" | "file"));
            run_search(&inner)
        }
        "trash" => {
            let raw = pick_op_path(call);
            let path = if raw.trim().is_empty() {
                ".".to_string()
            } else {
                raw
            };
            let p = Path::new(&path);
            if !p.exists() {
                return Ok(ToolOutcome {
                    user_message: format!("路径不存在：{path}"),
                    log_lines: vec![format!(
                        "状态:ok_not_found | 耗时:{}ms | op:trash | path:{}",
                        started.elapsed().as_millis(),
                        shorten_path(&path)
                    )],
                });
            }
            let (trash_id, method) = backup_existing_path_to_recycle(p)?;
            let elapsed = started.elapsed();
            let payload = file_manager_payload_path(&trash_id);
            let mut status = "0".to_string();
            let mut note = String::new();
            if method == "copy_only" {
                status = "ok_exdev_copied".to_string();
                note = "\n注意：跨文件系统（EXDEV），已复制进回收站，但未删除原路径。".to_string();
            }
            Ok(ToolOutcome {
                user_message: format!(
                    "已回收：{path}\ntrash_id: {trash_id}\npayload: {}{note}",
                    shorten_path(&payload.to_string_lossy())
                ),
                log_lines: vec![format!(
                    "状态:{status} | 耗时:{}ms | op:trash | path:{} | trash_id:{} | method:{}",
                    elapsed.as_millis(),
                    shorten_path(&path),
                    trash_id,
                    method
                )],
            })
        }
        "restore" => {
            let id = call.trash_id.as_deref().unwrap_or("").trim().to_string();
            let force = call.force.unwrap_or(false);
            let recycle = file_manager_recycle_dir();
            if !recycle.exists() {
                return Ok(ToolOutcome {
                    user_message: "回收站不存在（无可恢复项）".to_string(),
                    log_lines: vec![format!(
                        "状态:ok_not_found | 耗时:{}ms | op:restore",
                        started.elapsed().as_millis()
                    )],
                });
            }
            let meta = match file_manager_read_trash_meta(&id) {
                Ok(m) => m,
                Err(_) => {
                    return Ok(ToolOutcome {
                        user_message: format!("未找到 trash_id：{id}"),
                        log_lines: vec![format!(
                            "状态:ok_not_found | 耗时:{}ms | op:restore | trash_id:{}",
                            started.elapsed().as_millis(),
                            id
                        )],
                    });
                }
            };
            let payload = PathBuf::from(&meta.payload);
            if !payload.exists() {
                return Ok(ToolOutcome {
                    user_message: format!("回收站 payload 不存在：{}", meta.payload),
                    log_lines: vec![format!(
                        "状态:ok_not_found | 耗时:{}ms | op:restore | trash_id:{}",
                        started.elapsed().as_millis(),
                        id
                    )],
                });
            }
            let target = PathBuf::from(&meta.src);
            if target.exists() && !force {
                return Ok(ToolOutcome {
                    user_message: format!(
                        "目标已存在：{}\n为避免覆盖，请显式设置 force:true（覆盖前会把旧目标备份到回收站/本地备份）。",
                        meta.src
                    ),
                    log_lines: vec![format!(
                        "状态:ok_need_confirm | 耗时:{}ms | op:restore | trash_id:{} | force:0 | target:{}",
                        started.elapsed().as_millis(),
                        id,
                        shorten_path(&meta.src)
                    )],
                });
            }
            let mut backup_note: Option<String> = None;
            if target.exists() && force {
                // 优先进回收站；EXDEV 退化为本地备份（同目录 rename）
                match backup_existing_path_to_recycle(&target) {
                    Ok((bid, method)) => {
                        backup_note = Some(format!("backup_trash_id:{bid} method:{method}"));
                    }
                    Err(e) => {
                        // 可能是 EXDEV（目标在别的 FS），尝试本地备份
                        let bak = rename_to_local_backup(&target).ok();
                        if let Some(bak) = bak.as_ref() {
                            backup_note = Some(format!(
                                "backup_local:{}",
                                shorten_path(&bak.to_string_lossy())
                            ));
                        } else {
                            return Ok(ToolOutcome {
                                user_message: format!(
                                    "备份旧目标失败，已取消恢复：{}\n原因：{e}",
                                    meta.src
                                ),
                                log_lines: vec![format!(
                                    "状态:fail | 耗时:{}ms | op:restore | phase:backup_failed | trash_id:{}",
                                    started.elapsed().as_millis(),
                                    id
                                )],
                            });
                        }
                    }
                }
            }
            if let Some(parent) = target.parent()
                && !parent.as_os_str().is_empty()
            {
                fs::create_dir_all(parent)
                    .with_context(|| format!("创建目录失败：{}", parent.display()))?;
            }
            let mut method = "rename".to_string();
            if let Err(e) = fs::rename(&payload, &target) {
                if !file_manager_is_exdev(&e) {
                    return Err(anyhow!(e).context("恢复 rename 失败"));
                }
                // EXDEV：copy + 删除回收站 payload（安全）
                let mut stats = CopyStats::default();
                copy_recursive(&payload, &target, &mut stats)?;
                method = "copy_then_remove".to_string();
                if payload.is_dir() {
                    let _ = fs::remove_dir_all(&payload);
                } else {
                    let _ = fs::remove_file(&payload);
                }
            }
            // 标记已恢复（仅更新 meta；保留目录作为审计记录）
            let mut meta2 = meta.clone();
            meta2.note = Some(format!(
                "restored_at:{}{}",
                chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                backup_note
                    .as_ref()
                    .map(|s| format!(" | {s}"))
                    .unwrap_or_default()
            ));
            let _ = file_manager_write_trash_meta(&meta2);
            let elapsed = started.elapsed();
            let mut user_message =
                format!("已恢复到：{}\ntrash_id: {id}\nmethod: {method}", meta2.src);
            if let Some(note) = backup_note.as_deref() {
                user_message.push_str(&format!("\n{note}"));
            }
            Ok(ToolOutcome {
                user_message,
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | op:restore | trash_id:{} | target:{} | method:{} | force:{}",
                    elapsed.as_millis(),
                    id,
                    shorten_path(&meta2.src),
                    method,
                    if force { 1 } else { 0 }
                )],
            })
        }
        "copy" | "move" => {
            let src_raw = call.src.as_deref().unwrap_or("").trim().to_string();
            let dst_raw = call.dst.as_deref().unwrap_or("").trim().to_string();
            let force = call.force.unwrap_or(false);
            let recursive = call.recursive.unwrap_or(false);
            if src_raw.is_empty() || dst_raw.is_empty() {
                return Ok(ToolOutcome {
                    user_message: "file_manager copy/move 需要 src/dst".to_string(),
                    log_lines: vec![format!(
                        "状态:fail | 耗时:{}ms | op:{}",
                        started.elapsed().as_millis(),
                        op
                    )],
                });
            }
            let src = PathBuf::from(&src_raw);
            let dst = PathBuf::from(&dst_raw);
            if !src.exists() {
                return Ok(ToolOutcome {
                    user_message: format!("源路径不存在：{src_raw}"),
                    log_lines: vec![format!(
                        "状态:ok_not_found | 耗时:{}ms | op:{} | src:{}",
                        started.elapsed().as_millis(),
                        op,
                        shorten_path(&src_raw)
                    )],
                });
            }
            let src_is_dir = fs::metadata(&src).map(|m| m.is_dir()).unwrap_or(false);
            if src_is_dir && !recursive {
                return Ok(ToolOutcome {
                    user_message: format!(
                        "源路径是目录：{src_raw}\n为避免误操作，请显式设置 recursive:true。",
                    ),
                    log_lines: vec![format!(
                        "状态:ok_need_confirm | 耗时:{}ms | op:{} | recursive:0 | src:{}",
                        started.elapsed().as_millis(),
                        op,
                        shorten_path(&src_raw)
                    )],
                });
            }
            // 目标已存在：默认需要确认；force:true 则备份旧目标再继续。
            let mut backup_info: Option<String> = None;
            if dst.exists() && !force {
                return Ok(ToolOutcome {
                    user_message: format!(
                        "目标已存在：{dst_raw}\n为避免覆盖，请显式设置 force:true（覆盖前会把旧目标备份到回收站/本地备份）。"
                    ),
                    log_lines: vec![format!(
                        "状态:ok_need_confirm | 耗时:{}ms | op:{} | force:0 | dst:{}",
                        started.elapsed().as_millis(),
                        op,
                        shorten_path(&dst_raw)
                    )],
                });
            }
            if dst.exists() && force {
                match backup_existing_path_to_recycle(&dst) {
                    Ok((bid, method)) => {
                        backup_info = Some(format!("backup_trash_id:{bid} method:{method}"));
                        // copy_only 代表原目标未移走，会导致覆盖失败；此时尝试本地备份移走。
                        if method == "copy_only" {
                            let bak = rename_to_local_backup(&dst)?;
                            backup_info = Some(format!(
                                "{} | backup_local:{}",
                                backup_info.unwrap_or_default(),
                                shorten_path(&bak.to_string_lossy())
                            ));
                        }
                    }
                    Err(_) => {
                        let bak = rename_to_local_backup(&dst)?;
                        backup_info = Some(format!(
                            "backup_local:{}",
                            shorten_path(&bak.to_string_lossy())
                        ));
                    }
                }
            }

            let mut method = "rename".to_string();
            let mut stats = CopyStats::default();
            if op == "move" {
                if let Err(e) = fs::rename(&src, &dst) {
                    if !file_manager_is_exdev(&e) {
                        return Err(anyhow!(e).context("move rename 失败"));
                    }
                    // EXDEV：退化为 copy（不删除源，避免真删）
                    copy_recursive(&src, &dst, &mut stats)?;
                    method = "exdev_copy_only".to_string();
                }
            } else {
                copy_recursive(&src, &dst, &mut stats)?;
                method = "copy".to_string();
            }

            let elapsed = started.elapsed();
            let mut status = "0".to_string();
            let mut note = String::new();
            if method == "exdev_copy_only" {
                status = "ok_exdev_copied".to_string();
                note = "\n注意：跨文件系统（EXDEV），已复制到目标，但未删除源路径。".to_string();
            }
            let mut msg = format!(
                "{op} 完成：\nsrc: {src_raw}\ndst: {dst_raw}\nmethod: {method}\nfiles:{} dirs:{} bytes:{} ({}){note}",
                stats.files,
                stats.dirs,
                stats.bytes,
                format_bytes(stats.bytes),
            );
            if let Some(bk) = backup_info.as_deref() {
                msg.push_str(&format!("\n{bk}"));
            }
            Ok(ToolOutcome {
                user_message: msg,
                log_lines: vec![format!(
                    "状态:{status} | 耗时:{}ms | op:{} | src:{} | dst:{} | method:{} | force:{} | recursive:{} | files:{} | dirs:{} | bytes:{}",
                    elapsed.as_millis(),
                    op,
                    shorten_path(&src_raw),
                    shorten_path(&dst_raw),
                    method,
                    if force { 1 } else { 0 },
                    if recursive { 1 } else { 0 },
                    stats.files,
                    stats.dirs,
                    stats.bytes
                )],
            })
        }
        _ => Ok(ToolOutcome {
            user_message: format!("file_manager 不支持的 op：{op}"),
            log_lines: vec![format!(
                "状态:fail | 耗时:{}ms | op:{}",
                started.elapsed().as_millis(),
                op
            )],
        }),
    }
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
            log.push_str(&format!(
                " | saved:{} | saved_bytes:{} | saved_lines:{}",
                meta.path, meta.bytes, meta.lines
            ));
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
        if label == "fastmemo" {
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
    if label != "fastmemo" {
        return Ok(tool_format_error("memory_edit", "仅支持 fastmemo"));
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
    if label != "fastmemo" {
        return Ok(tool_format_error("memory_add", "仅支持 fastmemo"));
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
    if label != "fastmemo" {
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
        "fastmemo" => {
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
    let path = Path::new("prompts/Skills index.md");
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

fn truncate_tool_payload(text: &str, max_lines: usize, max_chars: usize) -> String {
    if text.is_empty() {
        return String::new();
    }
    let (mut body, truncated_by_lines) = truncate_by_lines(text, max_lines);
    let (body2, truncated_by_chars) = truncate_owned_by_chars(body, max_chars);
    body = body2;
    if truncated_by_lines || truncated_by_chars {
        body.push_str(&format!(
            "\n\n[输出已截断：保留头尾，最多 {max_lines} 行 / {max_chars} 字符；建议缩小范围或分段读取以获取精确信息]"
        ));
    }
    body
}

fn build_preview(text: &str, limit: usize) -> String {
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
        "fast" | "fastmemo" => Some(normalize_tool_path("memory/fastmemo.jsonl")),
        _ => None,
    }
}

fn system_config_path() -> PathBuf {
    std::env::var("YING_SYSTEM_CONFIG")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("config/system.json"))
}

fn memory_label_for_path(path: &str) -> String {
    let lower = path.to_ascii_lowercase();
    if lower.contains("metamemo") {
        "metamemo".to_string()
    } else if lower.contains("datememo") {
        "datememo".to_string()
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
    if target.is_empty() {
        return vec![
            ("datememo".to_string(), "memory/datememo".to_string()),
            (
                "fastmemo".to_string(),
                normalize_tool_path("memory/fastmemo.jsonl"),
            ),
        ];
    }
    if target == "all" || target == "*" {
        return vec![
            ("datememo".to_string(), "memory/datememo".to_string()),
            ("metamemo".to_string(), "memory/metamemo".to_string()),
            (
                "fastmemo".to_string(),
                normalize_tool_path("memory/fastmemo.jsonl"),
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

fn pick_write_fields(call: &ToolCall) -> anyhow::Result<(String, String)> {
    let path = call
        .path
        .as_deref()
        .map(str::trim)
        .filter(|s| !s.is_empty());
    let content = call.content.as_ref().map(|s| s.to_string());
    if let Some(path) = path {
        let body = content.unwrap_or_else(|| call.input.to_string());
        return Ok((normalize_tool_path(path), body));
    }
    if content.is_some() {
        let input_path = call.input.trim();
        if input_path.is_empty() {
            return Err(anyhow!("write_file 需要 path"));
        }
        return Ok((normalize_tool_path(input_path), content.unwrap_or_default()));
    }
    Err(anyhow!("write_file 需要 path 与 content"))
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

    fn format_line_range(start: Option<usize>, max: Option<usize>) -> Option<String> {
        let start = start.unwrap_or(0);
        let max = max.unwrap_or(0);
        if start == 0 {
            return None;
        }
        let begin = start.max(1);
        if max > 0 {
            let end = begin.saturating_add(max.saturating_sub(1));
            Some(format!("{begin}-{end}"))
        } else {
            Some(format!("{begin}-..."))
        }
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
        "read_file" | "stat_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim());
            let mut display = if call.tool == "read_file" && call.memory.unwrap_or(false) {
                let raw = path.trim();
                if raw.is_empty() {
                    "mem:(missing path)".to_string()
                } else {
                    format!("mem:{raw}")
                }
            } else {
                display_tool_path(path)
            };
            if call.tool == "read_file" {
                let max_lines = call
                    .max_lines
                    .unwrap_or(TOOL_OUTPUT_MAX_LINES)
                    .clamp(1, READ_MAX_LINES_CAP);
                if call.tail.unwrap_or(false) {
                    display = format!("{display} tail:{max_lines}");
                } else if call.head.unwrap_or(false) && call.start_line.is_none() {
                    display = format!("{display} head:{max_lines}");
                } else if let Some(range) = format_line_range(call.start_line, call.max_lines) {
                    display = format!("{display} lines:{range}");
                } else if call.full.unwrap_or(false) {
                    display = format!("{display} full");
                } else {
                    display = format!("{display} peek");
                }
            }
            build_preview(&display, limit)
        }
        "list_dir" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim());
            build_preview(&display_tool_path(path), limit)
        }
        "write_file" => {
            let path = call
                .path
                .as_deref()
                .or_else(|| call.content.as_ref().map(|_| call.input.trim()))
                .unwrap_or("");
            let bytes = call
                .content
                .as_ref()
                .map(|c| c.len())
                .unwrap_or(call.input.len());
            if path.is_empty() {
                build_preview("(missing path)", limit)
            } else {
                let display = display_tool_path(path);
                build_preview(&format!("{display} ({bytes} bytes)"), limit)
            }
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
        "file_manager" => {
            let op = call.op.as_deref().unwrap_or("").trim().to_ascii_lowercase();
            match op.as_str() {
                "list" | "stat" | "trash" => {
                    let raw = call
                        .path
                        .as_deref()
                        .or(call.src.as_deref())
                        .unwrap_or(call.input.trim())
                        .trim();
                    let p = if raw.is_empty() { "." } else { raw };
                    build_preview(&format!("{op} {}", display_tool_path(p)), limit)
                }
                "restore" => {
                    let id = call.trash_id.as_deref().unwrap_or("").trim();
                    let force = call.force.unwrap_or(false);
                    let suffix = if force { " force" } else { "" };
                    build_preview(&format!("restore {id}{suffix}"), limit)
                }
                "copy" | "move" => {
                    let src = call.src.as_deref().unwrap_or("").trim();
                    let dst = call.dst.as_deref().unwrap_or("").trim();
                    let force = call.force.unwrap_or(false);
                    let recursive = call.recursive.unwrap_or(false);
                    let mut extra = String::new();
                    if force {
                        extra.push_str(" force");
                    }
                    if recursive {
                        extra.push_str(" recursive");
                    }
                    build_preview(
                        &format!(
                            "{op} {} -> {}{extra}",
                            display_tool_path(src),
                            display_tool_path(dst)
                        ),
                        limit,
                    )
                }
                "search" => {
                    let mode = call.mode.as_deref().unwrap_or("content").trim();
                    let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
                    let root = call
                        .root
                        .as_deref()
                        .or(call.path.as_deref())
                        .or(call.src.as_deref())
                        .unwrap_or(".")
                        .trim();
                    build_preview(
                        &format!("search[{mode}] {pattern} in {}", display_tool_path(root)),
                        limit,
                    )
                }
                _ => build_preview(&call.input, limit),
            }
        }
        _ => build_preview(&call.input, limit),
    }
}

pub fn tool_display_label(tool: &str) -> String {
    match tool.trim() {
        // UI 展示层：直接使用工具名（避免把 bash 显示成 “Run” 造成困惑）
        "bash" => "BASH",
        "adb" => "Shell",
        "termux_api" => "Termux",
        "plan" | "work" => "Plan",
        "read_file" => "READ",
        "write_file" => "Write",
        "list_dir" => "List",
        "stat_file" => "Info",
        "search" => "Search",
        "edit_file" => "Edit",
        "apply_patch" => "Patch",
        "file_manager" => "Manage",
        "memory_check" => "MFind",
        "memory_read" => "MRead",
        "memory_edit" => "MEdit",
        "memory_add" => "MAdd",
        "mind_msg" => "Mind",
        "system_config" => "Sys",
        "skills_mcp" => "Skills",
        _ => tool.trim(),
    }
    .to_string()
}
fn shorten_path(raw: &str) -> String {
    short_tail_path(raw, 2)
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

fn normalize_tool_path(raw: &str) -> String {
    let path = raw.trim();
    if path.is_empty() {
        return String::new();
    }
    let home = std::env::var("HOME").unwrap_or_default();
    if path == "~" || path.starts_with("~/") {
        if !home.is_empty() {
            let rest = path.trim_start_matches('~').trim_start_matches('/');
            if rest.is_empty() {
                return home;
            }
            return format!("{home}/{rest}");
        }
        return path.to_string();
    }
    if (path == "home" || path.starts_with("home/")) && !home.is_empty() {
        let rest = path.trim_start_matches("home").trim_start_matches('/');
        if rest.is_empty() {
            return home;
        }
        return format!("{home}/{rest}");
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
    Ok((code, stdout, stderr, timed_out))
}
