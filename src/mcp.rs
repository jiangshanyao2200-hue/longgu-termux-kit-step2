use std::fs;
use std::io::{BufRead, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::Instant;

use anyhow::{Context, anyhow};
use chrono::{Local, NaiveDate};
use regex::Regex;
use serde::Deserialize;
use serde_json::Value;

use crate::memorydb::{MemoDb, MemoKind, MemoRow};

const BASH_SHELL: &str = "/data/data/com.termux/files/usr/bin/bash";
const ADB_SERIAL: &str = "127.0.0.1:5555";
const OUTPUT_MAX_CHARS: usize = 20_000;
const OUTPUT_MAX_LINES: usize = 400;
const READ_MAX_BYTES: usize = 200_000;
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
const TOOL_OUTPUT_MAX_CHARS: usize = 12_000;
const TOOL_OUTPUT_MAX_LINES: usize = 240;
const TOOL_OUTPUT_RAW_MAX_CHARS: usize = 20_000;
const TOOL_OUTPUT_RAW_MAX_LINES: usize = 400;
const TOOL_META_RAW_MAX_CHARS: usize = 20_000;
const TOOL_META_RAW_MAX_LINES: usize = 200;

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
const WRITE_MAX_BYTES: usize = 400_000;
const PATCH_MAX_BYTES: usize = 500_000;
const EDIT_MAX_FILE_BYTES: usize = 800_000;
const EDIT_MAX_MATCHES: usize = 400;
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
        Value::Object(_) | Value::Bool(_) | Value::Number(_) => {
            value_to_nonempty_string(v).and_then(|s| {
                let tokens = parse_list_tokens(&s, max);
                (!tokens.is_empty()).then_some(tokens)
            })
        }
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
            "section" | "slot" | "area" | "region" | "heading" => call.section = s,
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
            "output_level" | "out_level" | "level" | "output" => {
                call.output_level = s;
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

fn normalize_search_pattern(raw_pattern: &str) -> (String, Vec<String>) {
    let raw_pattern = raw_pattern.trim();
    let keywords = parse_search_keywords(raw_pattern);
    if keywords.len() == 1 {
        return (keywords[0].clone(), keywords);
    }
    (raw_pattern.to_string(), keywords)
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
        assert!(call
            .exclude_dirs
            .as_ref()
            .is_some_and(|v| v.iter().any(|x| x == "target")));
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
    fn fastmemo_v1_migrates_to_v2_and_dedupes() {
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
        assert!(is_fastmemo_v2_struct(&out));
        assert!(!out.contains("淡化池"));
        assert!(out.contains("[自我感知]"));
        assert!(out.contains("[用户感知]"));
        assert!(out.contains("[环境感知]"));
        assert!(out.contains("[历史感知]"));
        assert!(out.contains("[动态context池]"));
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
        assert!(is_fastmemo_v2_struct(&repaired));
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
        "list_dir" => r#"{"tool":"list_dir","path":".","brief":"列出目录"}"#,
        "stat_file" => r#"{"tool":"stat_file","path":"Cargo.toml","brief":"查看文件信息"}"#,
        "read_file" => {
            r#"{"tool":"read_file","path":"src/main.rs","head":true,"max_lines":200,"brief":"读取文件开头"}"#
        }
        "write_file" => {
            r#"{"tool":"write_file","path":"notes/demo.txt","content":"hello","brief":"写入文件"}"#
        }
        "search" => {
            r#"{"tool":"search","pattern":"TODO","root":"src","brief":"搜索内容（可选 file:true 搜文件名）"}"#
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
            r#"{"tool":"memory_read","path":"datememo","start_line":1,"max_lines":120,"brief":"读取日记片段"}"#
        }
        "memory_edit" => {
            r#"{"tool":"memory_edit","path":"fastmemo","find":"旧条目","replace":"新条目","count":1,"brief":"修正条目"}"#
        }
        "memory_add" => {
            r#"{"tool":"memory_add","path":"datememo","content":"2026-01-20 20:00 | user | 记录内容","brief":"追加日记条目"}"#
        }
        "mind_msg" => {
            r#"{"tool":"mind_msg","target":"dog","content":"需要你协助检查工具结果。","brief":"同步需求"}"#
        }
        "system_config" => {
            r#"{"tool":"system_config","heartbeat_minutes":10,"brief":"调整心跳间隔"}"#
        }
        "skills_mcp" => r#"{"tool":"skills_mcp","category":"编程类","brief":"获取编程类工具说明"}"#,
        _ => r#"{"tool":"<tool>","input":"...","brief":"一句话说明"}"#,
    }
}

fn tool_format_error(tool: &str, reason: &str) -> ToolOutcome {
    let usage = tool_usage(tool);
    ToolOutcome {
        user_message: format!("格式错误：{reason}\n正确格式：{usage}"),
        log_lines: vec![],
    }
}

fn validate_tool_call(call: &ToolCall) -> Result<(), ToolOutcome> {
    let tool = call.tool.as_str();
    if call.brief.as_deref().unwrap_or("").trim().is_empty() {
        return Err(tool_format_error(tool, "缺少 brief"));
    }
    match tool {
        "bash" | "adb" | "termux_api" => {
            if call.input.trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 input"));
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
            if call.find.as_deref().unwrap_or("").trim().is_empty() {
                return Err(tool_format_error(tool, "缺少 find"));
            }
        }
        "memory_add" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim()).trim();
            let Some((label, _)) = resolve_memory_path_label(path) else {
                return Err(tool_format_error(tool, "缺少有效 path"));
            };
            if label != "datememo" && label != "fastmemo" {
                return Err(tool_format_error(tool, "仅支持 datememo/fastmemo"));
            }
            let content = call.content.as_deref().unwrap_or("").trim();
            if content.is_empty() {
                return Err(tool_format_error(tool, "缺少 content"));
            }
            if label == "fastmemo" {
                let section = call.section.as_deref().unwrap_or("").trim();
                if section.is_empty() {
                    return Err(tool_format_error(tool, "fastmemo 需要 section"));
                }
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
        _ => {
            return Err(tool_format_error(
                tool,
                "未知工具（可用：bash/adb/termux_api/list_dir/stat_file/read_file/write_file/search/edit_file/apply_patch/memory_check/memory_read/memory_edit/memory_add/system_config/skills_mcp）",
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
    let sensitive_paths = ["/system/", "/vendor/", "/product/", "/odm/", "/proc/", "/sys/", "/dev/"];
    let hit_path = sensitive_paths.iter().find(|p| lower.contains(**p)).copied();
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
            return Some(format!(
                "系统敏感路径写操作（path:{path} marker:{marker}）"
            ));
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
        return Ok(outcome);
    }
    match call.tool.as_str() {
        "bash" => run_bash(call),
        "adb" => run_adb(call),
        "termux_api" => run_termux_api(call),
        "read_file" => run_read_file(call),
        "write_file" => run_write_file(call),
        "list_dir" => run_list_dir(call),
        "stat_file" => run_stat_file(call),
        "search" => run_search(call),
        "edit_file" => run_edit_file(call),
        "apply_patch" => run_apply_patch(call),
        "memory_check" => run_memory_check(call),
        "memory_read" => run_memory_read(call),
        "memory_edit" => run_memory_edit(call),
        "memory_add" => run_memory_add(call),
        "mind_msg" => run_mind_msg(call),
        "system_config" => run_system_config(call),
        "skills_mcp" => run_skills_mcp(call),
        other => Ok(ToolOutcome {
            user_message: format!("未知工具：{other}"),
            log_lines: vec![],
        }),
    }
}

fn outcome_is_timeout(outcome: &ToolOutcome) -> bool {
    outcome
        .log_lines
        .iter()
        .any(|l| l.contains("状态:timeout") || l.contains("超时"))
}

pub fn handle_tool_call_with_retry(call: &ToolCall, retries: usize) -> ToolOutcome {
    let mut last = None;
    for _ in 0..retries.max(1) {
        match handle_tool_call(call) {
            Ok(outcome) => {
                if outcome_is_timeout(&outcome) {
                    last = Some(outcome);
                    continue;
                }
                return outcome;
            }
            Err(e) => {
                last = Some(ToolOutcome {
                    user_message: format!("工具执行失败：{e:#}"),
                    log_lines: vec![],
                });
            }
        }
    }
    last.unwrap_or_else(|| ToolOutcome {
        user_message: "工具执行失败".to_string(),
        log_lines: vec![],
    })
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
    let cwd_display = current_dir_display();
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
        .output()
        .with_context(|| format!("bash 执行失败：{cmd}"))?;
    let elapsed = started.elapsed();
    Ok(build_shell_outcome_bash(out, elapsed, timeout_used, &cwd_display, cmd, timeout_hint))
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
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let truncated_by_lines = combined.lines().count() > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let need_save = total_bytes > SHELL_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;

    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SHELL_CACHE_DIR}/bash_{ts}_{pid}.log");
        if try_write_shell_cache(&path, &out.stdout, &out.stderr) {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let body = annotate_timeout(truncate_command_output(combined), timed_out, timeout_hint_secs);
    let body = annotate_nonzero_exit(body, timed_out, code);
    let status = status_label(code, timed_out);
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(p) = saved_path.as_deref() {
        log_lines.push(format!("saved:{p}"));
    }
    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else if let Some(p) = saved_path {
            format!("{body}\n\n[saved:{p}]")
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
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let truncated_by_lines = combined.lines().count() > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let need_save = total_bytes > ADB_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;

    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{ADB_CACHE_DIR}/adb_{ts}_{pid}.log");
        if try_write_shell_cache_impl(ADB_CACHE_DIR, &path, &out.stdout, &out.stderr) {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let body = annotate_timeout(truncate_command_output(combined), timed_out, timeout_hint_secs);
    let body = annotate_nonzero_exit(body, timed_out, code);
    let status = status_label(code, timed_out);
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(p) = saved_path.as_deref() {
        log_lines.push(format!("saved:{p}"));
    }
    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else if let Some(p) = saved_path {
            format!("{body}\n\n[saved:{p}]")
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
    let cwd_display = current_dir_display();
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
) -> ToolOutcome {
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let combined = collect_output(&stdout, &stderr);

    let total_bytes = out.stdout.len().saturating_add(out.stderr.len());
    let truncated_by_lines = combined.lines().count() > OUTPUT_MAX_LINES;
    let truncated_by_chars = combined.chars().count() > OUTPUT_MAX_CHARS;
    let need_save =
        total_bytes > TERMUX_API_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;

    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{TERMUX_API_CACHE_DIR}/termux_api_{ts}_{pid}.log");
        if try_write_shell_cache_impl(TERMUX_API_CACHE_DIR, &path, &out.stdout, &out.stderr) {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let body = annotate_timeout(truncate_command_output(combined), timed_out, timeout_hint_secs);
    let body = annotate_nonzero_exit(body, timed_out, code);
    let status = status_label(code, timed_out);
    let mut log_lines = vec![format!(
        "状态:{status} | exit:{code} | 耗时:{}ms | cwd:{cwd_display} | cmd_len:{}",
        elapsed.as_millis(),
        cmd.chars().count()
    )];
    if let Some(p) = saved_path.as_deref() {
        log_lines.push(format!("saved:{p}"));
    }

    ToolOutcome {
        user_message: if body.is_empty() {
            "(no output)".to_string()
        } else if let Some(p) = saved_path {
            format!("{body}\n\n[saved:{p}]")
        } else {
            body
        },
        log_lines,
    }
}

fn run_termux_api(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let cwd_display = current_dir_display();
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
    ))
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
    let path = pick_path(call)?;
    let started = Instant::now();
    let tail_mode = call.tail.unwrap_or(false);
    let head_mode = call.head.unwrap_or(false) && !tail_mode;

    let mut file = fs::File::open(&path).with_context(|| format!("读取文件失败：{path}"))?;
    let total_bytes = file.metadata().map(|m| m.len() as usize).unwrap_or(0);
    let mut truncated_by_bytes = false;
    if tail_mode && total_bytes > READ_MAX_BYTES {
        truncated_by_bytes = true;
        let offset = total_bytes.saturating_sub(READ_MAX_BYTES) as u64;
        file.seek(std::io::SeekFrom::Start(offset))
            .context("尾部读取 seek 失败")?;
    }

    let mut buf: Vec<u8> = Vec::new();
    std::io::Read::take(&mut file, READ_MAX_BYTES as u64)
        .read_to_end(&mut buf)
        .context("读取文件内容失败")?;
    let elapsed = started.elapsed();

    if !tail_mode {
        truncated_by_bytes = total_bytes.max(buf.len()) > READ_MAX_BYTES;
    }
    let mut total_lines: Option<usize> = None;
    let mut total_chars: Option<usize> = None;
    let mut is_binary = false;
    let mut body = match String::from_utf8(buf.clone()) {
        Ok(s) => {
            if !truncated_by_bytes {
                total_lines = Some(s.lines().count());
                total_chars = Some(s.chars().count());
            }
            s
        }
        Err(_) => {
            is_binary = true;
            let hex = bytes_preview_hex(&buf, 256);
            format!(
                "(非 UTF-8 文本，大小 {} 字节)\n前 256 字节(hex):\n{}",
                buf.len(),
                hex
            )
        }
    };
    let mut line_count = body.lines().count();
    let mut char_count = body.chars().count();
    if !is_binary {
        let max_lines = call
            .max_lines
            .unwrap_or(TOOL_OUTPUT_MAX_LINES)
            .clamp(1, READ_MAX_LINES_CAP);
        if tail_mode {
            let lines: Vec<&str> = body.lines().collect();
            let total = lines.len();
            if total > max_lines {
                let start_idx = total.saturating_sub(max_lines);
                body = lines[start_idx..].join("\n");
            }
            let mut note = if truncated_by_bytes {
                format!(
                    "\n\n[仅展示末尾 {max_lines} 行（尾部读取，行号未知；尾部截取 {READ_MAX_BYTES} bytes）]"
                )
            } else {
                format!("\n\n[仅展示末尾 {max_lines} 行，共 {total} 行]")
            };
            if truncated_by_bytes {
                note.push_str("；可能缺少更早内容");
            }
            body.push_str(&note);
            line_count = body.lines().count();
            char_count = body.chars().count();
        } else {
            let needs_slice = head_mode || call.start_line.is_some() || call.max_lines.is_some();
            if needs_slice {
                let start = call.start_line.unwrap_or(1).max(1);
                let lines: Vec<&str> = body.lines().collect();
                let total = lines.len();
                if start > total {
                    body = format!("起始行超出范围：start_line={start}，总行数={total}");
                } else {
                    let start_idx = start.saturating_sub(1);
                    let end_idx = (start_idx + max_lines).min(total);
                    let slice = lines[start_idx..end_idx].join("\n");
                    let mut note =
                        format!("\n\n[仅展示第 {}-{} 行，共 {} 行]", start, end_idx, total);
                    if truncated_by_bytes {
                        note.push_str("；原文件已按字节截断");
                    }
                    body = format!("{slice}{note}");
                }
                line_count = body.lines().count();
                char_count = body.chars().count();
            } else if truncated_by_bytes {
                body.push_str(&format!("\n\n[仅展示前 {READ_MAX_BYTES} 字节]"));
                line_count = body.lines().count();
                char_count = body.chars().count();
            }
        }
    } else if truncated_by_bytes {
        body.push_str(&format!("\n\n[仅展示 {READ_MAX_BYTES} 字节]"));
        line_count = body.lines().count();
        char_count = body.chars().count();
    }

    Ok(ToolOutcome {
        user_message: truncate_command_output(body),
        log_lines: vec![{
            let mut meta = format!(
                "状态:0 | 耗时:{}ms | path:{} | lines:{} | chars:{} | bytes:{}",
                elapsed.as_millis(),
                shorten_path(&path),
                line_count,
                char_count,
                total_bytes.max(buf.len())
            );
            if let Some(total) = total_lines {
                meta.push_str(&format!(" | total_lines:{total}"));
            }
            if let Some(total) = total_chars {
                meta.push_str(&format!(" | total_chars:{total}"));
            }
            if truncated_by_bytes {
                meta.push_str(" | partial:true");
            }
            if is_binary {
                meta.push_str(" | binary:true");
            }
            meta
        }],
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
    let started = Instant::now();
    let old_bytes = fs::metadata(&path)
        .ok()
        .filter(|m| m.is_file())
        .map(|m| m.len() as usize)
        .unwrap_or(0);
    let old_lines = count_file_lines(&path).unwrap_or(0);
    let old_chars = if old_bytes <= READ_MAX_BYTES {
        fs::read_to_string(&path)
            .ok()
            .map(|s| s.chars().count())
            .unwrap_or(old_bytes)
    } else {
        old_bytes
    };
    if let Some(parent) = Path::new(&path).parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("创建目录失败：{}", parent.display()))?;
    }
    fs::write(&path, content.as_bytes()).with_context(|| format!("写入失败：{path}"))?;
    let elapsed = started.elapsed();
    let new_bytes = content.len();
    let new_lines = count_text_lines(&content);
    let new_chars = content.chars().count();

    let preview = build_preview(&content, 160);
    let mut msg = String::new();
    msg.push_str(&format!(
        "已写入 {path}（{new_lines} 行 / {new_chars} 字符）"
    ));
    msg.push_str(&format!("\n预览: {preview}"));

    let delta = if old_lines > 1 || new_lines > 1 {
        format!("delta_lines:+{} -{}", new_lines, old_lines)
    } else {
        format!("delta_chars:+{} -{}", new_chars, old_chars)
    };

    Ok(ToolOutcome {
        user_message: msg,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | path:{} | bytes:{} | lines:{} | chars:{} | {delta}",
            elapsed.as_millis(),
            shorten_path(&path),
            new_bytes,
            new_lines,
            new_chars
        )],
    })
}

fn run_list_dir(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let path = pick_path(call)?;
    let started = Instant::now();
    let mut entries: Vec<String> = Vec::new();
    for entry in fs::read_dir(&path).with_context(|| format!("列目录失败：{path}"))? {
        let entry = entry?;
        let name = entry.file_name().to_string_lossy().to_string();
        let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);
        let line = if is_dir { format!("{name}/") } else { name };
        entries.push(line);
    }
    entries.sort();
    let elapsed = started.elapsed();
    let mut body = entries.join("\n");
    if body.is_empty() {
        body = "(empty)".to_string();
    }
    let total_entries = entries.len();
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
    let path = pick_path(call)?;
    let started = Instant::now();
    let meta = fs::metadata(&path).with_context(|| format!("获取文件信息失败：{path}"))?;
    let elapsed = started.elapsed();
    let file_type = if meta.is_dir() {
        "dir"
    } else if meta.is_file() {
        "file"
    } else {
        "other"
    };
    let modified = meta
        .modified()
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| format!("{}", d.as_secs()))
        .unwrap_or_else(|| "unknown".to_string());
    let size_bytes = meta.len();
    let body = format!(
        "type: {file_type}\nsize: {size_bytes} ({})\nmodified(unix): {modified}\nreadonly: {}",
        format_bytes(size_bytes),
        meta.permissions().readonly()
    );

    Ok(ToolOutcome {
        user_message: body,
        log_lines: vec![format!(
            "状态:0 | 耗时:{}ms | path:{}",
            elapsed.as_millis(),
            shorten_path(&path)
        )],
    })
}

fn run_search(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let pattern = call
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
    let (pattern_effective, keywords) = normalize_search_pattern(&pattern);
    let root = call
        .root
        .as_deref()
        .or(call.path.as_deref())
        .unwrap_or(".")
        .trim()
        .to_string();
    if !Path::new(&root).exists() {
        return Ok(ToolOutcome {
            user_message: format!("search 根路径不存在：{root}"),
            log_lines: vec![],
        });
    }
    let search_files = call.file.unwrap_or(false);
    let want_context = call.context.unwrap_or(false)
        || call.context_lines.is_some()
        || call.context_files.is_some()
        || call.context_hits_per_file.is_some();
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
        let truncated_by_lines = body.lines().count() > out_lines;
        let truncated_by_chars = body.chars().count() > out_chars;
        let need_save =
            total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
        let saved_path = if need_save {
            let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
            let pid = unsafe { libc::getpid() };
            let path = format!("{SEARCH_CACHE_DIR}/search_files_{ts}_{pid}.log");
            if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
                Some(path)
            } else {
                None
            }
        } else {
            None
        };

        let mut body = truncate_tool_payload(&body, out_lines, out_chars);
        if let Some(p) = saved_path.as_deref() {
            body.push_str(&format!("\n\n[saved:{p}]"));
        }
        let mut log = format!(
            "状态:0 | 耗时:{}ms | root:{} | matches:{} | engine:find | file:true | pattern:{pattern_preview}",
            elapsed.as_millis(),
            shorten_path(&root),
            match_count
        );
        if let Some(p) = saved_path.as_deref() {
            log.push_str(&format!(" | saved:{p}"));
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
    if needles.len() >= 2 {
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
        "--binary-files=without-match".to_string(),
        "-m".to_string(),
        max_matches.to_string(),
    ];
    for d in SEARCH_EXCLUDE_DIRS {
        grep_args.push("--exclude-dir".to_string());
        grep_args.push((*d).to_string());
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
    let truncated_by_lines = raw_body.lines().count() > out_lines;
    let truncated_by_chars = raw_body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_{ts}_{pid}.log");
        if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, stdout.as_bytes(), stderr.as_bytes())
        {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let mut display_body = raw_body;
    if need_save && !no_match {
        display_body = format!(
            "【输出过大】已返回部分结果（输出已导出）。\n{}",
            display_body.trim_end()
        );
    }
    let mut body = annotate_timeout(display_body, timed_out, timeout_hint);
    body = truncate_tool_payload(&body, out_lines, out_chars);
    let status_code = if no_match { 0 } else { code };
    body = annotate_nonzero_exit(body, timed_out, status_code);
    if let Some(p) = saved_path.as_deref() {
        body.push_str(&format!("\n\n[saved:{p}]"));
    }
    let status = status_label(status_code, timed_out);
    let mut log = format!(
        "状态:{status} | 耗时:{}ms | root:{} | matches:{} | engine:{engine} | pattern:{pattern_preview}",
        elapsed.as_millis(),
        shorten_path(&root),
        match_count
    );
    if let Some(p) = saved_path.as_deref() {
        log.push_str(&format!(" | saved:{p}"));
    }

    Ok(ToolOutcome {
        user_message: body,
        log_lines: vec![log],
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
                hits.push(SearchHit {
                    path: path.clone(),
                    line_no,
                    line: line
                        .trim_end_matches(['\n', '\r'])
                        .to_string(),
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
            blocks.push(format!("=== {shown} ===\n(跳过：文件过大 > {SEARCH_MAX_FILE_BYTES} bytes)"));
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
            block.push_str(&format!("【命中过多】该文件仅展示前 {hits_per_file} 处命中上下文。\n"));
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
                let text = lines.get(ln.saturating_sub(1)).map(|s| s.as_str()).unwrap_or("");
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
    let truncated_by_lines = body.lines().count() > out_lines;
    let truncated_by_chars = body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_ctx_{ts}_{pid}.log");
        if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let mut body = truncate_tool_payload(&body, out_lines, out_chars);
    if let Some(p) = saved_path.as_deref() {
        body.push_str(&format!("\n\n[saved:{p}]"));
    }

    let status = status_label(0, timed_out);
    let mut log = format!(
        "状态:{status} | 耗时:{}ms | root:{} | engine:context | pattern:{pattern_preview} | ctx_lines:{context_lines}",
        elapsed.as_millis(),
        shorten_path(root),
    );
    if let Some(p) = saved_path.as_deref() {
        log.push_str(&format!(" | saved:{p}"));
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
        results.push(format!("{shown}:{}:{snippet}", h.line_no, snippet = snippet));
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
    if !notes.is_empty() {
        body = format!("{}\n{}", notes.join("\n"), body.trim_end());
    }
    if stats.timed_out {
        body = annotate_timeout(body, true, Some(timeout_secs));
    }

    let total_bytes = body.as_bytes().len();
    let truncated_by_lines = body.lines().count() > out_lines;
    let truncated_by_chars = body.chars().count() > out_chars;
    let need_save =
        total_bytes > SEARCH_SAVE_THRESHOLD_BYTES || truncated_by_lines || truncated_by_chars;
    let saved_path = if need_save {
        let ts = chrono::Local::now().format("%Y%m%d_%H%M%S");
        let pid = unsafe { libc::getpid() };
        let path = format!("{SEARCH_CACHE_DIR}/search_kw_{ts}_{pid}.log");
        if try_write_shell_cache_impl(SEARCH_CACHE_DIR, &path, body.as_bytes(), &[]) {
            Some(path)
        } else {
            None
        }
    } else {
        None
    };

    let mut body = truncate_tool_payload(&body, out_lines, out_chars);
    if let Some(p) = saved_path.as_deref() {
        body.push_str(&format!("\n\n[saved:{p}]"));
    }

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
    if stats.skipped_unreadable > 0 {
        log.push_str(&format!(" | skipped_unreadable:{}", stats.skipped_unreadable));
    }
    if let Some(p) = saved_path.as_deref() {
        log.push_str(&format!(" | saved:{p}"));
    }

    ToolOutcome {
        user_message: body,
        log_lines: vec![log],
    }
}

fn run_edit_file(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let path = pick_path(call)?;
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
    let pattern = call.pattern.as_deref().unwrap_or(call.input.trim()).trim();
    if pattern.is_empty() {
        return Ok(tool_format_error("memory_check", "缺少 pattern"));
    }
    let target = call
        .path
        .as_deref()
        .or(call.root.as_deref())
        .unwrap_or("")
        .trim();
    let files = memory_paths_for_check(target);
    if files.is_empty() {
        return Ok(tool_format_error("memory_check", "目标文件无效"));
    }
    let max_hits = call
        .count
        .unwrap_or(MEMORY_CHECK_DEFAULT_RESULTS)
        .clamp(1, MEMORY_CHECK_MAX_RESULTS);
    let started = Instant::now();
    let keywords = parse_memory_keywords(pattern);
    if keywords.is_empty() {
        return Ok(tool_format_error("memory_check", "缺少有效关键词"));
    }
    let (start_date, end_date) = match parse_memory_date_range(
        call.date_start.as_deref().unwrap_or(""),
        call.date_end.as_deref().unwrap_or(""),
    ) {
        Ok(range) => range,
        Err(reason) => return Ok(tool_format_error("memory_check", reason)),
    };
    let range_active = start_date.is_some() || end_date.is_some();
    let mut hits = 0usize;
    let mut total_hits = 0usize;
    let mut out = String::new();
    let mut memo_db: Option<MemoDb> = None;
    let mut global_min: Option<NaiveDate> = None;
    let mut global_max: Option<NaiveDate> = None;

    for (label, path) in files {
        if label == "fastmemo" {
            let text = fs::read_to_string(&path).unwrap_or_default();
            let mut block_hits = 0usize;
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
        let kind = if label == "datememo" {
            MemoKind::Date
        } else {
            MemoKind::Meta
        };
        let remaining = max_hits.saturating_sub(hits);
        let (rows, label_total, _stats, min_date, max_date) =
            db.check_by_keywords(kind, &keywords, start_date, end_date, remaining)?;
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
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str(&format!("[{label}]"));
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
            "状态:0 | 耗时:{}ms | hits:{} | total:{} | keywords:{}",
            started.elapsed().as_millis(),
            hits,
            total_hits,
            keywords.join("/")
        )],
    })
}

fn run_memory_read(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let Some((label, path)) = resolve_memory_path_label(raw) else {
        return Ok(tool_format_error("memory_read", "缺少有效 path"));
    };
    let started = Instant::now();
    let use_slice = call.start_line.is_some() || call.max_lines.is_some();
    let has_date = !call.date_start.as_deref().unwrap_or("").trim().is_empty()
        || !call.date_end.as_deref().unwrap_or("").trim().is_empty();
    if has_date && use_slice {
        return Ok(tool_format_error(
            "memory_read",
            "日期检索与行区间不可同时使用",
        ));
    }
    if label == "datememo" || label == "metamemo" {
        let kind = if label == "datememo" {
            MemoKind::Date
        } else {
            MemoKind::Meta
        };
        let db = MemoDb::open_default()?;
        let stats = db.table_stats(kind)?;
        if has_date {
            let (start_date, end_date) = match parse_memory_date_range(
                call.date_start.as_deref().unwrap_or(""),
                call.date_end.as_deref().unwrap_or(""),
            ) {
                Ok(range) => range,
                Err(reason) => return Ok(tool_format_error("memory_read", reason)),
            };
            let (rows, stats) = db.read_by_date(kind, start_date, end_date)?;
            if rows.is_empty() {
                return Ok(ToolOutcome {
                    user_message: "未找到匹配日期范围内的条目".to_string(),
                    log_lines: vec![format!(
                        "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{} | hits:0",
                        started.elapsed().as_millis(),
                        shorten_path(&path),
                        stats.total_rows,
                        stats.total_chars
                    )],
                });
            }
            let body = render_memo_rows(&rows);
            let clipped =
                truncate_tool_payload(&body, TOOL_OUTPUT_MAX_LINES, TOOL_OUTPUT_MAX_CHARS);
            return Ok(ToolOutcome {
                user_message: clipped,
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{} | hits:{}",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    stats.total_rows,
                    stats.total_chars,
                    rows.len()
                )],
            });
        }

        if use_slice {
            let start_line = call.start_line.unwrap_or(1).max(1);
            let max_lines = call
                .max_lines
                .unwrap_or(TOOL_OUTPUT_MAX_LINES)
                .clamp(1, READ_MAX_LINES_CAP);
            if stats.total_rows == 0 {
                return Ok(ToolOutcome {
                    user_message: "(空文件)".to_string(),
                    log_lines: vec![format!(
                        "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{}",
                        started.elapsed().as_millis(),
                        shorten_path(&path),
                        stats.total_rows,
                        stats.total_chars
                    )],
                });
            }
            if start_line > stats.total_rows.max(1) {
                return Ok(ToolOutcome {
                    user_message: format!(
                        "起始行超出范围：start_line={start_line}，总行数={}",
                        stats.total_rows
                    ),
                    log_lines: vec![format!(
                        "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{}",
                        started.elapsed().as_millis(),
                        shorten_path(&path),
                        stats.total_rows,
                        stats.total_chars
                    )],
                });
            }
            let (rows, stats) = db.read_by_index(kind, start_line, max_lines)?;
            let end = start_line
                .saturating_add(rows.len().saturating_sub(1))
                .min(stats.total_rows);
            let mut body = render_memo_rows(&rows);
            body.push_str(&format!(
                "\n\n[仅展示第 {start_line}-{end} 行，共 {} 行]",
                stats.total_rows
            ));
            return Ok(ToolOutcome {
                user_message: body,
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{} | range:{}-{}",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    stats.total_rows,
                    stats.total_chars,
                    start_line,
                    end
                )],
            });
        }

        if stats.total_rows == 0 {
            return Ok(ToolOutcome {
                user_message: "(空文件)".to_string(),
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{}",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    stats.total_rows,
                    stats.total_chars
                )],
            });
        }
        if stats.total_rows > TOOL_OUTPUT_MAX_LINES || stats.total_chars > TOOL_OUTPUT_MAX_CHARS {
            let msg = format!(
                "文件过大（{} 行 / {} 字符），请先 memory_check 后分段读取。",
                stats.total_rows, stats.total_chars
            );
            return Ok(ToolOutcome {
                user_message: msg,
                log_lines: vec![format!(
                    "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{}",
                    started.elapsed().as_millis(),
                    shorten_path(&path),
                    stats.total_rows,
                    stats.total_chars
                )],
            });
        }
        let (rows, stats) = db.read_by_index(kind, 1, stats.total_rows)?;
        let body = render_memo_rows(&rows);
        let clipped = truncate_tool_payload(&body, TOOL_OUTPUT_MAX_LINES, TOOL_OUTPUT_MAX_CHARS);
        return Ok(ToolOutcome {
            user_message: clipped,
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | rows:{} | chars:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                stats.total_rows,
                stats.total_chars
            )],
        });
    }

    ensure_memory_file(&label, &path)?;
    if has_date {
        if label != "datememo" && label != "metamemo" {
            return Ok(tool_format_error(
                "memory_read",
                "日期检索仅支持 datememo/metamemo",
            ));
        }
        let (start_date, end_date) = match parse_memory_date_range(
            call.date_start.as_deref().unwrap_or(""),
            call.date_end.as_deref().unwrap_or(""),
        ) {
            Ok(range) => range,
            Err(reason) => return Ok(tool_format_error("memory_read", reason)),
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
        // v2 canonical
        "自我感知" | "自我" | "人格" | "风格" | "style" | "self" => Some("自我感知"),
        "用户感知" | "用户" | "画像" | "user" => Some("用户感知"),
        "环境感知" | "环境" | "env" | "system" => Some("环境感知"),
        "历史感知" | "历史" | "里程碑" | "history" => Some("历史感知"),
        "动态context池" | "动态上下文池" | "动态摘要池" | "动态池" | "contextpool" | "ctx_pool"
        | "ctxpool" => Some("动态context池"),
        // v1 -> v2 mapping (best-effort)
        "动态成长人格" => Some("自我感知"),
        "用户感知画像" => Some("用户感知"),
        "人生旅程" => Some("历史感知"),
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

fn build_fastmemo_v2_template(
    now: &str,
    seed: &std::collections::HashMap<&'static str, Vec<String>>,
) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "fastmemo v2 | max_chars: 1800 | updated: {now}\n\n"
    ));
    for name in fastmemo_section_headers_v2() {
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
    for name in fastmemo_section_headers_v2() {
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
    build_fastmemo_v2_template(&now, &seed)
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
    for sec in fastmemo_section_headers_v2() {
        counts.entry(sec).or_insert(0);
    }
    counts
}

fn run_memory_add(call: &ToolCall) -> anyhow::Result<ToolOutcome> {
    let raw = call.path.as_deref().unwrap_or(call.input.trim()).trim();
    let Some((label, path)) = resolve_memory_path_label(raw) else {
        return Ok(tool_format_error("memory_add", "缺少有效 path"));
    };
    if label != "datememo" && label != "fastmemo" {
        return Ok(tool_format_error("memory_add", "仅支持 datememo/fastmemo"));
    }
    let content = call.content.as_deref().unwrap_or("").trim();
    if content.is_empty() {
        return Ok(tool_format_error("memory_add", "缺少 content"));
    }
    let started = Instant::now();
    if label == "datememo" {
        MemoDb::open_default()?.append_datememo_content(content)?;
        let msg = build_memory_add_message(&path, content);
        return Ok(ToolOutcome {
            user_message: msg,
            log_lines: vec![format!(
                "状态:0 | 耗时:{}ms | path:{} | chars:{}",
                started.elapsed().as_millis(),
                shorten_path(&path),
                content.chars().count()
            )],
        });
    }

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
                "fastmemo_counts | 自我:{} | 用户:{} | 环境:{} | 历史:{} | 动态:{}",
                counts.get("自我感知").copied().unwrap_or(0),
                counts.get("用户感知").copied().unwrap_or(0),
                counts.get("环境感知").copied().unwrap_or(0),
                counts.get("历史感知").copied().unwrap_or(0),
                counts.get("动态context池").copied().unwrap_or(0),
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
            let template = build_fastmemo_v2_template(&now, &std::collections::HashMap::new());
            fs::write(path, template).with_context(|| format!("初始化 fastmemo 失败：{path}"))?;
            return Ok(());
        }
        let looks_v1 =
            text.trim_start().starts_with("fastmemo v1") || text.contains("[动态成长人格]");
        let needs_repair =
            looks_v1 || !is_fastmemo_v2_struct(&text) || fastmemo_has_unknown_sections(&text);
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
            build_fastmemo_v2_template(&now.to_string(), &std::collections::HashMap::new())
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
            let snippet = build_preview(content.trim(), SNIP);
            out.push(format!("{p}:{ln}:{snippet}"));
        } else {
            out.push(build_preview(line.trim(), SNIP));
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
            ("metamemo".to_string(), "memory/metamemo".to_string()),
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
        "bash" | "adb" | "termux_api" => build_preview(&call.input, limit),
        "read_file" | "stat_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim());
            let mut display = display_tool_path(path);
            if call.tool == "read_file" {
                let max_lines = call
                    .max_lines
                    .unwrap_or(TOOL_OUTPUT_MAX_LINES)
                    .clamp(1, READ_MAX_LINES_CAP);
                if call.tail.unwrap_or(false) {
                    display = format!("{display} tail:{max_lines}");
                } else if call.head.unwrap_or(false) && call.start_line.is_none() {
                    display = format!("{display} 1-{max_lines}");
                } else if let Some(range) = format_line_range(call.start_line, call.max_lines) {
                    display = format!("{display} {range}");
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
            let pattern = call.pattern.as_deref().unwrap_or(call.input.trim());
            let root = call.root.as_deref().or(call.path.as_deref()).unwrap_or(".");
            let label = if call.file.unwrap_or(false) {
                "file"
            } else {
                "pattern"
            };
            let keywords = parse_search_keywords(pattern);
            let pattern = match keywords.len() {
                0 => pattern.to_string(),
                1 => keywords[0].clone(),
                _ => format!("\"{}\"", keywords.join(", ")),
            };
            let ctx = call.context.unwrap_or(false)
                || call.context_lines.is_some()
                || call.context_files.is_some()
                || call.context_hits_per_file.is_some();
            let mut extra = String::new();
            if ctx {
                let n = call
                    .context_lines
                    .unwrap_or(SEARCH_CONTEXT_DEFAULT_LINES)
                    .clamp(1, SEARCH_CONTEXT_MAX_LINES);
                extra = format!(" ctx:±{n}");
            }
            build_preview(
                &format!("{label}={pattern}{extra} in {}", display_tool_path(root)),
                limit,
            )
        }
        "edit_file" => {
            let path = call.path.as_deref().unwrap_or(call.input.trim());
            build_preview(&display_tool_path(path), limit)
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
            let keywords = parse_memory_keywords(pattern);
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
        _ => build_preview(&call.input, limit),
    }
}

pub fn tool_display_label(tool: &str) -> String {
    match tool.trim() {
        // UI 展示层：直接使用工具名（避免把 bash 显示成 “Run” 造成困惑）
        "bash" => "BASH",
        "adb" => "Shell",
        "termux_api" => "Termux",
        "read_file" => "Read",
        "write_file" => "Write",
        "list_dir" => "List",
        "stat_file" => "Info",
        "search" => "Search",
        "edit_file" => "Edit",
        "apply_patch" => "Patch",
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
    let (mut command, timeout_used) = build_command_with_optional_timeout(program, args, timeout_secs);
    let out = command
        .output()
        .with_context(|| format!("执行失败：{program}"))?;
    let code = status_code(out.status.code());
    let timed_out = timeout_used && is_timeout_status(code);
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    Ok((code, stdout, stderr, timed_out))
}
