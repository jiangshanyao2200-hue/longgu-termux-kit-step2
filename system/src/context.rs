use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;

use anyhow::Context;
use serde::Serialize;
use serde_json::json;

// DeepSeek chat/completions: some deployments reject requests unless the last message role is user.
// We keep tool results in `system` (so they are not misread as user input), then append this
// ephemeral user tail placeholder at request time (providers.rs) when needed.
//
// Important:
// - This must NEVER be written into long-term chat history/context (DogState/Core history).
// - It exists only to satisfy provider protocol constraints.
pub(crate) const ROLE_NEEDED_USER_PLACEHOLDER_BASE: &str =
    "[MCP:role协议占位]非用户输入，请忽略本条消息，基于上一轮工具回执继续响应。";

pub(crate) fn role_needed_user_placeholder_to(tool_code: Option<&str>) -> String {
    if let Some(code) = tool_code.map(str::trim).filter(|s| !s.is_empty()) {
        format!(
            "[MCP:role协议占位to {code}]非用户输入，请忽略本条消息，基于上一轮工具回执继续响应。"
        )
    } else {
        ROLE_NEEDED_USER_PLACEHOLDER_BASE.to_string()
    }
}

pub(crate) fn is_role_needed_user_placeholder(text: &str) -> bool {
    let t = text.trim();
    t.contains("[MCP:role协议占位")
}

pub(crate) fn extract_last_mcp_tool_code(messages: &[ApiMessage]) -> Option<String> {
    for m in messages.iter().rev() {
        let c = m.content.trim();
        if let Some(code) = extract_mcp_tool_code_from_text(c) {
            return Some(code);
        }
    }
    None
}

fn extract_mcp_tool_code_from_text(text: &str) -> Option<String> {
    // `[MCP:工具输出code001]...` -> `code001`
    let t = text.trim();
    let Some(pos) = t.find("[MCP:工具输出") else {
        return None;
    };
    let rest = &t[(pos + "[MCP:工具输出".len())..];
    let end = rest.find(']').unwrap_or(rest.len());
    let code = rest[..end].trim();
    (!code.is_empty()).then_some(code.to_string())
}

fn failure_reason_preview(result_text: &str) -> Option<String> {
    // 给模型用的“失败原因预览”：只取极短、可读的一段，避免撑爆上下文。
    // 不参与“状态推断”（状态只看 meta），因此即使包含 timeout/超时字样也不会误判状态。
    let mut lines = result_text.lines().map(str::trim).filter(|l| !l.is_empty());
    let first = lines.next()?;
    // 常见导出提示行本身就足够表达原因；否则就取第一行做预览。
    let mut s = first.to_string();
    // 若第一行是泛化标题（例如 exported notice），尝试拼一行补充信息。
    if (s.contains("已导出") || s.contains("exported") || s.contains("truncated"))
        && let Some(second) = lines.next()
    {
        if !second.is_empty() {
            s.push_str(" | ");
            s.push_str(second);
        }
    }
    // 压缩空白并截断。
    let s = s
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .trim()
        .to_string();
    if s.is_empty() {
        return None;
    }
    const MAX: usize = 120;
    let out = if s.chars().count() > MAX {
        let mut t = s.chars().take(MAX).collect::<String>();
        t.push('…');
        t
    } else {
        s
    };
    Some(out)
}

/// Provider-facing message type (protocol layer).
/// UI formatting must never be written back into this structure.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct ApiMessage {
    pub(crate) role: String,
    pub(crate) content: String,
}

pub(crate) fn strip_stamp_footer(text: &str) -> &str {
    // 兼容两种历史格式：
    // - 旧：末尾 "\n[[t:HHMMSS]]"
    // - 新：头部 "[t:HHMMSS]\n"
    let mut out = text;

    // strip new header
    if let Some(rest) = out.strip_prefix("[t:") {
        if rest.len() >= 7 {
            let hhmmss = &rest[..6];
            let end = rest.as_bytes().get(6).copied().unwrap_or(b'\0');
            if hhmmss.chars().all(|c| c.is_ascii_digit()) && end == b']' {
                let after = &rest[7..]; // skip "HHMMSS]"
                out = after.strip_prefix('\n').unwrap_or(after);
            }
        }
    }

    // strip old footer
    const MARK: &str = "\n[[t:";
    if let Some(pos) = out.rfind(MARK) {
        let tail = &out[pos + 1..]; // skip the leading '\n'
        if !tail.contains('\n') && tail.ends_with("]]") {
            return &out[..pos];
        }
    }
    out
}

pub(crate) fn with_stamp_footer(text: &str) -> String {
    // 兼容函数：历史上我们会把时间戳注入每条消息内容（如 `[t:HHMMSS]`）。
    // 但实践中这会被部分模型“学走并回显”，污染正文与工具调用判断，因此禁用内容级时间戳。
    //
    // 这里仅做“去除历史时间戳”，返回干净文本。
    strip_stamp_footer(text).trim_end().to_string()
}

/// DeepSeek rejects some sequences (e.g. consecutive assistant messages).
/// Keep this normalization strictly protocol-level: no UI summaries, no semantic rewrites.
pub(crate) fn normalize_messages_for_deepseek(messages: &[ApiMessage]) -> Vec<ApiMessage> {
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

/// Best-effort detector for "context pollution" where UI-rendered strings leak into model context.
/// Returns human-readable findings; callers should log, not mutate context here.
pub(crate) fn detect_context_contamination(messages: &[ApiMessage]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for (i, m) in messages.iter().enumerate() {
        let idx = i.saturating_add(1);
        let role = m.role.trim();
        let c = m.content.as_str();

        // UI glyphs / headings that should never be sent to providers.
        for needle in [
            "● Ran CMD",
            "● Brief",
            "tool call format",
            "to=shell",
            "TOOL: BASH / EXPLAIN / INPUT",
            "♡ · 系统信息",
            "[sys:工具调用（assistant 发起；非用户输入）]",
        ] {
            if c.contains(needle) {
                out.push(format!(
                    "#{idx} role={role} contains forbidden marker: {needle}"
                ));
            }
        }
    }
    out
}

#[derive(Debug, Clone)]
pub(crate) struct ContextHealth {
    pub(crate) score_0_100: u8,
    pub(crate) deductions: Vec<String>,
}

fn clamp_score(score: i32) -> u8 {
    score.clamp(0, 100) as u8
}

/// Heuristic context health score for debugging.
/// This must never mutate messages; it only reports issues.
pub(crate) fn score_context_health(provider: &str, messages: &[ApiMessage]) -> ContextHealth {
    let p = provider.trim().to_ascii_lowercase();
    let is_deepseek = p == "deepseek";

    let mut score: i32 = 100;
    let mut deductions: Vec<String> = Vec::new();

    if messages.is_empty() {
        deductions.push("-80: empty messages".to_string());
        return ContextHealth {
            score_0_100: clamp_score(20),
            deductions,
        };
    }

    let allowed_roles_deepseek = ["system", "user", "assistant"];
    for (i, m) in messages.iter().enumerate() {
        let idx = i.saturating_add(1);
        let role = m.role.trim();
        let content = m.content.trim();
        if role.is_empty() || content.is_empty() {
            score -= 5;
            deductions.push(format!("-5: empty role/content at message #{idx}"));
            continue;
        }
        if is_deepseek && !allowed_roles_deepseek.contains(&role) {
            score -= 25;
            deductions.push(format!(
                "-25: deepseek unsupported role at message #{idx}: {role}"
            ));
        }
    }

    if is_deepseek {
        if let Some(last) = messages.last() {
            let role = last.role.trim();
            if role != "user" {
                score -= 20;
                deductions.push("-20: deepseek last role is not user".to_string());
            }
            let tail = last.content.trim();
            if is_role_needed_user_placeholder(tail) {
                // ok: this tail exists only for protocol; do not treat as a user directive.
            } else if tail.contains("[sys:轮询占位]") {
                score -= 10;
                deductions.push("-10: sys poll tail uses legacy marker".to_string());
            } else if tail.contains("continue") || tail.contains("继续") {
                score -= 15;
                deductions.push("-15: last user tail contains directive-like words".to_string());
            }
        }
    }

    let mut last_role: Option<&str> = None;
    for m in messages {
        let role = m.role.trim();
        if last_role == Some("assistant") && role == "assistant" {
            score -= 8;
            deductions.push("-8: consecutive assistant messages".to_string());
            break;
        }
        last_role = Some(role);
    }

    let contam = detect_context_contamination(messages);
    if !contam.is_empty() {
        let n = contam.len();
        let penalty = (n as i32 * 12).clamp(12, 60);
        score -= penalty;
        deductions.push(format!("-{penalty}: context contamination markers ({n})"));
        for item in contam.into_iter().take(6) {
            deductions.push(format!("  - {item}"));
        }
        if n > 6 {
            deductions.push("  - ...".to_string());
        }
    }

    let est_tokens = crate::estimate_messages_in_tokens(messages) as i32;
    if est_tokens > 16_000 {
        score -= 20;
        deductions.push("-20: estimated_in_tokens > 16000".to_string());
    } else if est_tokens > 12_000 {
        score -= 12;
        deductions.push("-12: estimated_in_tokens > 12000".to_string());
    } else if est_tokens > 8_000 {
        score -= 6;
        deductions.push("-6: estimated_in_tokens > 8000".to_string());
    }
    if messages.len() > 80 {
        score -= 10;
        deductions.push("-10: messages_len > 80".to_string());
    } else if messages.len() > 50 {
        score -= 5;
        deductions.push("-5: messages_len > 50".to_string());
    }

    ContextHealth {
        score_0_100: clamp_score(score),
        deductions,
    }
}

// ===== fastmemo（上下文注入/压缩池）=====

pub(crate) const FASTMEMO_PATH: &str = "memory/fastmemory.jsonl";
const FASTMEMO_MAX_INJECT_CHARS: usize = 1800;

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

pub(crate) fn read_fastmemo_for_context() -> String {
    // fastmemo 文件由记忆工具维护；这里仅做只读注入。
    let path = Path::new(FASTMEMO_PATH);
    // 结构自愈：避免 fastmemo v1/脏结构进入上下文，导致后续 memory_add/edit 逻辑出现偏差。
    let _ = crate::mcp::ensure_memory_file("fastmemo", FASTMEMO_PATH);
    let text = std::fs::read_to_string(path).unwrap_or_default();
    truncate_chars_safe(text.trim(), FASTMEMO_MAX_INJECT_CHARS)
}


pub(crate) fn fastmemo_event_count_and_any_ge10(text: &str) -> (usize, bool) {
    let mut current: Option<&str> = None;
    let mut self_n = 0usize;
    let mut user_n = 0usize;
    let mut env_n = 0usize;
    let mut event_n = 0usize;
    for line in text.lines() {
        let t = line.trim();
        if t.starts_with('[') && t.contains(']') {
            current = match t {
                "[自我感知]" => Some("自我感知"),
                "[用户感知]" => Some("用户感知"),
                "[环境感知]" => Some("环境感知"),
                "[事件感知]" => Some("事件感知"),
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
            Some("事件感知") => event_n += 1,
            _ => {}
        }
    }
    let any_ge10 = self_n >= 10 || user_n >= 10 || env_n >= 10 || event_n >= 10;
    (event_n, any_ge10)
}

fn append_fastmemo_event_pool_item(raw: &str) -> anyhow::Result<(usize, bool)> {
    crate::mcp::ensure_memory_file("fastmemo", FASTMEMO_PATH)?;
    let mut lines: Vec<String> = std::fs::read_to_string(FASTMEMO_PATH)
        .unwrap_or_default()
        .lines()
        .map(|s| s.to_string())
        .collect();
    if lines.is_empty() {
        // 兜底：若文件异常为空，至少补全 v3 section 结构，避免后续写入失败。
        lines.push("fastmemo v3 | max_chars: 1800".to_string());
        lines.push(String::new());
        for sec in ["自我感知", "用户感知", "环境感知", "事件感知"] {
            lines.push(format!("[{sec}]"));
            lines.push(String::new());
        }
    }

    let header = "[事件感知]";
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
    let merged = crate::compact_ws_inline(raw.trim());
    if merged.is_empty() {
        let text = std::fs::read_to_string(FASTMEMO_PATH).unwrap_or_default();
        return Ok(fastmemo_event_count_and_any_ge10(&text));
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
    Ok(fastmemo_event_count_and_any_ge10(&out))
}

// ===== contextmemo（上下文审计日志）=====

#[derive(Debug, Default)]
pub(crate) struct ContextUsage {
    tokens: usize,
    dirty: bool,
}

impl ContextUsage {
    pub(crate) fn add_text(&mut self, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            return;
        }
        self.tokens = self.tokens.saturating_add(crate::estimate_tokens(clean));
        self.dirty = true;
    }

    pub(crate) fn reset(&mut self) {
        self.tokens = 0;
        self.dirty = true;
    }

    pub(crate) fn load_tokens(&mut self, tokens: usize) {
        self.tokens = tokens;
        self.dirty = false;
    }

    pub(crate) fn mark_clean(&mut self) {
        self.dirty = false;
    }

    pub(crate) fn dirty(&self) -> bool {
        self.dirty
    }

    pub(crate) fn tokens(&self) -> usize {
        self.tokens
    }
}

fn contextmemo_ts() -> String {
    chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

pub(crate) fn load_contextmemo_tokens(path: &str) -> usize {
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
        tokens = tokens.saturating_add(crate::estimate_tokens(clean));
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

pub(crate) fn read_contextmemo_text(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_default()
}

pub(crate) fn log_contextmemo(path: &str, usage: &mut ContextUsage, speaker: &str, text: &str) {
    if let Err(e) = append_contextmemo(path, usage, speaker, text) {
        eprintln!("contextmemo 写入失败: {e:#}");
    }
}

pub(crate) fn clear_contextmemo(path: &str) {
    if let Err(e) = fs::write(path, "") {
        eprintln!("contextmemo 清理失败: {e:#}");
    }
}

fn infer_mcp_exec_status_from_meta(meta: &[String]) -> &'static str {
    // 只依据 meta 推断状态：绝不能扫描 output 文本，否则会被 `ps` 等输出里的单词（如 `timeout` 进程名）
    // 误判为“超时/失败”，污染上下文与 UI 展示。
    let joined = meta
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
        .to_ascii_lowercase();

    // 超时：优先看 ok/result（工具层会在 meta 里写明）。
    if joined.contains("ok:timeout")
        || joined.contains("result:timeout")
        || joined.contains("状态:timeout")
        || joined.contains("状态:ok_timeout")
    {
        return "超时";
    }
    // 成功：优先 ok:true；兼容旧 meta。
    if joined.contains("ok:true") || joined.contains("状态:0") || joined.contains("result:0") {
        return "成功";
    }
    // 失败：明确 ok:false/状态:fail 等。
    if joined.contains("ok:false")
        || joined.contains("状态:fail")
        || joined.contains("tool failed")
        || joined.contains("工具执行失败")
    {
        return "失败";
    }
    // 不确定时默认失败（更安全）。
    "失败"
}

fn extract_meta_kv(meta: &[String], key: &str) -> Option<String> {
    let k = format!("{key}:");
    for line in meta {
        // 只解析 key:value 形式；meta 行通常为 `... | key:value | ...`
        for part in line.split('|') {
            let p = part.trim();
            if let Some(rest) = p.strip_prefix(&k) {
                let v = rest.trim();
                if !v.is_empty() {
                    return Some(v.to_string());
                }
            }
        }
    }
    None
}

// ===== DogState（上下文状态机）=====

const CTX_KEEP_RECENT_ROUNDS: usize = 3;

#[derive(Debug, Clone)]
pub(crate) struct DogState {
    messages: Vec<ApiMessage>,
    pub(crate) prompt: String,
    prompt_reinject_pct: u8,
    tool_result_assistant_template: String,
    tool_output_seq: u64,
    last_tool_output_code: Option<String>,
    last_prompt_inject_user: usize,
    user_count: usize,
    used_tokens_est: usize,
    last_usage_total_tokens: u64,
    fastmemo_idx: Option<usize>,
    ctx_compact_pending: bool,
    pub(crate) ctx_compact_inflight: bool,
    pub(crate) fastmemo_compact_pending: bool,
    include_tool_context: bool,
    date_injected: bool,
}

impl DogState {
    fn now_date_tz() -> String {
        // 只注入日期与时区，便于模型用 HHMM 构建“时间链”，同时减少 token。
        chrono::Local::now().format("%Y-%m-%d %:z").to_string()
    }

    fn now_time_tz() -> String {
        // 仅秒级：足够让模型感知时间流动，且 token 成本低；ms 级对模型决策通常无价值。
        chrono::Local::now().format("%H:%M:%S %:z").to_string()
    }

    fn strip_stamp_footer(text: &str) -> &str {
        strip_stamp_footer(text)
    }

    fn with_stamp_footer(text: &str) -> String {
        with_stamp_footer(text)
    }

    pub(crate) fn new(
        prompt: String,
        prompt_reinject_pct: u8,
        tool_result_assistant_template: String,
    ) -> Self {
        let mut s = Self {
            messages: Vec::new(),
            prompt,
            prompt_reinject_pct,
            tool_result_assistant_template,
            tool_output_seq: 0,
            last_tool_output_code: None,
            last_prompt_inject_user: 0,
            user_count: 0,
            used_tokens_est: 0,
            last_usage_total_tokens: 0,
            fastmemo_idx: None,
            ctx_compact_pending: false,
            ctx_compact_inflight: false,
            fastmemo_compact_pending: false,
            include_tool_context: true,
            date_injected: false,
        };
        s.inject_prompt();
        s
    }

    fn inject_prompt(&mut self) {
        // 日期只注入一次：避免 prompt reinject 时重复出现日期，造成上下文噪音与模型误判。
        if !self.date_injected {
            self.push_message("system", format!("[sys:日期]{}", Self::now_date_tz()));
            self.date_injected = true;
        }
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
            let mut merged = Self::strip_stamp_footer(&last.content)
                .trim_end()
                .to_string();
            let old = crate::estimate_tokens(&merged);
            if !merged.is_empty() {
                merged.push_str("\n\n");
            }
            merged.push_str(clean);
            merged = Self::with_stamp_footer(&merged);
            let new = crate::estimate_tokens(&merged);
            if new >= old {
                self.used_tokens_est = self.used_tokens_est.saturating_add(new - old);
            } else {
                self.used_tokens_est = self.used_tokens_est.saturating_sub(old - new);
            }
            last.content = merged;
            return;
        }

        let clean = Self::with_stamp_footer(clean);
        self.used_tokens_est = self
            .used_tokens_est
            .saturating_add(crate::estimate_tokens(&clean));
        self.messages.push(ApiMessage {
            role: role.to_string(),
            content: clean,
        });
    }

    pub(crate) fn push_user(&mut self, text: &str, limit: usize) -> bool {
        self.user_count = self.user_count.saturating_add(1);
        let added = crate::estimate_tokens(text);
        let projected = self.used_tokens_est.saturating_add(added);
        let reinject_now = !self.prompt.trim().is_empty()
            && crate::calc_pct(projected, limit) >= self.prompt_reinject_pct
            && self.last_prompt_inject_user != self.user_count;
        if reinject_now {
            self.inject_prompt();
            self.last_prompt_inject_user = self.user_count;
        }
        // 时间线：每条用户消息前注入一次时间（system role），避免在 assistant 内容中注入时间戳造成回显/污染。
        self.push_message("system", format!("[sys:时间]{}", Self::now_time_tz()));
        self.push_message("user", text.trim().to_string());
        reinject_now
    }

    pub(crate) fn push_assistant(&mut self, text: &str) {
        self.push_message("assistant", text.trim().to_string());
    }

    pub(crate) fn push_system(&mut self, text: &str) {
        self.push_message("system", text.trim().to_string());
    }

    fn upsert_system_message(&mut self, idx: &mut Option<usize>, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            return;
        }
        if let Some(pos) = *idx {
            if let Some(entry) = self.messages.get_mut(pos) {
                let old = crate::estimate_tokens(&entry.content);
                entry.content = Self::with_stamp_footer(clean);
                let new = crate::estimate_tokens(&entry.content);
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

    #[allow(dead_code)]
    pub(crate) fn push_tool(&mut self, text: &str) {
        if !self.include_tool_context {
            return;
        }
        let clean = text.trim();
        if clean.is_empty() {
            return;
        }
        // 兼容旧调用：无法得知 tool 名称与 meta，只能把整段内容作为“工具输出”写回上下文。
        self.push_mcp_tool_result("tool", clean, &[]);
    }

    pub(crate) fn last_tool_output_code(&self) -> Option<&str> {
        self.last_tool_output_code.as_deref()
    }

    pub(crate) fn push_mcp_tool_result(&mut self, tool: &str, result_text: &str, meta: &[String]) {
        if !self.include_tool_context {
            return;
        }
        let clean = result_text.trim();
        if clean.is_empty() {
            return;
        }
        self.tool_output_seq = self.tool_output_seq.saturating_add(1);
        let code = format!("code{:03}", self.tool_output_seq);
        self.last_tool_output_code = Some(code.clone());

        let status = infer_mcp_exec_status_from_meta(meta);
        let elapsed = extract_meta_kv(meta, "elapsed_ms")
            .map(|v| format!(" 耗时：{v}ms"))
            .or_else(|| extract_meta_kv(meta, "耗时").map(|v| format!(" 耗时：{v}")))
            .unwrap_or_default();
        let path = extract_meta_kv(meta, "cwd")
            .or_else(|| extract_meta_kv(meta, "path"))
            .map(|v| format!(" 路径：{v}"))
            .unwrap_or_default();
        let saved = meta
            .iter()
            .find_map(|l| l.trim().strip_prefix("saved:").map(str::trim))
            .filter(|s| !s.is_empty())
            .map(|v| format!(" 保存：{v}"))
            .unwrap_or_default();
        let reason = if status == "成功" {
            String::new()
        } else {
            failure_reason_preview(clean)
                .map(|r| format!(" 原因：{r}"))
                .unwrap_or_default()
        };

        let header = format!(
            "[MCP:工具输出{code}]工具名：{} 状态：{status}{reason}{elapsed}{path}{saved}",
            tool.trim()
        );
        // 工具回执（工具输出）写回上下文：
        // - 不能用 user：否则模型会把“工具输出”误判为“用户说的话/用户执行的结果”，进而误学工具协议；
        // - 不能用 tool role（DeepSeek/Codex 协议层不统一）；
        // - 这里使用 system，并用固定前缀显式声明“这是工具输出，不是系统指令/用户输入”。
        //
        // 注意：system 的优先级更高，因此 tool_result_assistant_template 必须足够明确、简短，
        // 且禁止混入任何 UI 渲染文本（●/Ran/TOOL: 等）。
        self.push_message(
            "system",
            crate::messages::render_template(
                &self.tool_result_assistant_template,
                &[("HEADER", &header), ("RESULT", clean)],
            ),
        );
    }

    pub(crate) fn set_last_usage_total(&mut self, tokens: u64) {
        if tokens > 0 {
            self.last_usage_total_tokens = tokens;
        }
    }

    pub(crate) fn reset_context(&mut self) {
        self.messages.clear();
        self.used_tokens_est = 0;
        self.user_count = 0;
        self.last_prompt_inject_user = 0;
        self.last_usage_total_tokens = 0;
        self.fastmemo_idx = None;
        self.ctx_compact_pending = false;
        self.ctx_compact_inflight = false;
        self.fastmemo_compact_pending = false;
        self.tool_output_seq = 0;
        self.last_tool_output_code = None;
        self.inject_prompt();
    }

    pub(crate) fn push_ctx_pool_item(
        &mut self,
        _sys_cfg: &crate::config::SystemConfig,
        item: &str,
    ) {
        if let Ok((_event_n, pending)) = append_fastmemo_event_pool_item(item) {
            if pending {
                self.fastmemo_compact_pending = true;
            }
        }
    }

    pub(crate) fn messages_clone(&self) -> Vec<ApiMessage> {
        self.messages.clone()
    }

    pub(crate) fn message_snapshot(&self, extra_system: Option<&str>) -> Vec<ApiMessage> {
        // Provider-facing: strip any historical `[t:HHMMSS]` stamps to avoid models echoing them.
        let mut out: Vec<ApiMessage> = self
            .messages
            .iter()
            .map(|m| ApiMessage {
                role: m.role.clone(),
                content: Self::strip_stamp_footer(&m.content).trim().to_string(),
            })
            .filter(|m| !m.role.trim().is_empty() && !m.content.trim().is_empty())
            .collect();
        if let Some(extra) = extra_system {
            let clean = extra.trim();
            if !clean.is_empty() {
                // 额外指令按 system 角色入列，保持上下文语义稳定。
                out.push(ApiMessage {
                    role: "system".to_string(),
                    content: Self::with_stamp_footer(clean),
                });
            }
        }
        normalize_messages_for_deepseek(&out)
    }

    pub(crate) fn set_include_tool_context(&mut self, enabled: bool) {
        self.include_tool_context = enabled;
    }

    fn estimate_total_tokens(&self) -> usize {
        self.messages
            .iter()
            .map(|m| crate::estimate_tokens(&m.content))
            .sum()
    }

    pub(crate) fn estimate_chat_tokens(&self) -> usize {
        self.messages
            .iter()
            .filter(|m| m.role == "user" || m.role == "assistant")
            .map(|m| crate::estimate_tokens(&m.content))
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

    pub(crate) fn refresh_fastmemo_system(&mut self, text: &str) {
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

    pub(crate) fn arm_context_compact_if_needed(
        &mut self,
        sys_cfg: &crate::config::SystemConfig,
    ) -> bool {
        if !sys_cfg.context_compact_enabled {
            self.ctx_compact_pending = false;
            self.ctx_compact_inflight = false;
            return false;
        }
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

    pub(crate) fn begin_context_compact(&mut self) -> bool {
        if self.ctx_compact_pending && !self.ctx_compact_inflight {
            self.ctx_compact_pending = false;
            self.ctx_compact_inflight = true;
            return true;
        }
        false
    }

    pub(crate) fn abort_context_compact(&mut self) {
        if self.ctx_compact_inflight {
            self.ctx_compact_inflight = false;
            self.ctx_compact_pending = true;
        }
    }

    pub(crate) fn apply_context_compact(
        &mut self,
        _sys_cfg: &crate::config::SystemConfig,
        summary: &str,
    ) -> (usize, usize) {
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
            if let Ok((event_n, pending)) = append_fastmemo_event_pool_item(clean) {
                pool_len = event_n;
                if pending {
                    self.fastmemo_compact_pending = true;
                }
            }
        }
        (cleared, pool_len)
    }
}
