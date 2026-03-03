use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;

use anyhow::Context;
use serde::Deserialize;
use serde::Serialize;
use serde_json::json;

// ===== 注释链（上下文层：协议清洗 + 外置回放 + DogState）=====
//
//（1）A provider 发送前链路（构造 messages）
//（2）core.rs:start_user_chat_request
//（3）-> DogState::message_snapshot_with_context_file
//（4）-> read_contextlog_messages_for_mind（回放外置 context JSONL）
//（5）-> normalize_messages_for_model_context（去空/去噪/合并）
//（6）-> providers.rs:normalize_messages_for_provider(provider
//（7）, messages)
//
//（1）B 外置 context 写入链路（本轮记录）
//（2）core.rs:start_user_chat_request /
//（3）handle_model_stream_end / drain_async_events(tool end)
//（4）-> log_dyncontext(mind, role=user|assistant|tool,
//（5）content, tool?, meta?)
//
//（1）C 外置 context 回放链路（下轮注入）
//（2）read_contextlog_messages_for_mind
//（3）role=user/assistant/system：原样回放
//（4）role=tool：render_tool_result_system_message（工具回执以 system
//（5）角色呈现，带 [MCP:工具输出code...] 头）
//
//（1）D DeepSeek role 约束兜底（仅 request time）
//（2）ROLE_NEEDED_USER_PLACEHOLDER_*：仅用于满足“最后一条必须是 user”的部署差异，
//（3）不写入长期上下文。

//（1）DeepSeek chat/completions: some deployments reject
//（2）requests unless the last message role is user.
//（3）We keep tool results in `system` (so they are not
//（4）misread as user input), then append this
//（5）ephemeral user tail placeholder at request time
//（6）(providers.rs) when needed.
//
//（1）Important:
//（2）This must NEVER be written into long-term chat
//（3）history/context (DogState/Core history).
//（4）It exists only to satisfy provider protocol constraints.
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
    //（1）`[MCP:工具输出code001]...` -> `code001`
    let t = text.trim();
    let pos = t.find("[MCP:工具输出")?;
    let rest = &t[(pos + "[MCP:工具输出".len())..];
    let end = rest.find(']').unwrap_or(rest.len());
    let code = rest[..end].trim();
    (!code.is_empty()).then_some(code.to_string())
}

fn failure_reason_preview(result_text: &str) -> Option<String> {
    //（1）给模型用的“失败原因预览”：只取极短、可读的一段，避免撑爆上下文。
    //（2）不参与“状态推断”（状态只看 meta），因此即使包含 timeout/超时字样也不会误判状态。
    let mut lines = result_text.lines().map(str::trim).filter(|l| !l.is_empty());
    let first = lines.next()?;
    //（1）常见导出提示行本身就足够表达原因；否则就取第一行做预览。
    let mut s = first.to_string();
    //（1）若第一行是泛化标题（例如 exported notice），尝试拼一行补充信息。
    if (s.contains("已导出") || s.contains("exported") || s.contains("truncated"))
        && let Some(second) = lines.next()
        && !second.is_empty()
    {
        s.push_str(" | ");
        s.push_str(second);
    }
    //（1）压缩空白并截断。
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
    //（1）兼容两种历史格式：
    //（2）旧：末尾 "\n[[t:HHMMSS]]"
    //（3）新：头部 "[t:HHMMSS]\n"
    let mut out = text;

    //（1）strip new header
    if let Some(rest) = out.strip_prefix("[t:")
        && rest.len() >= 7
    {
        let hhmmss = &rest[..6];
        let end = rest.as_bytes().get(6).copied().unwrap_or(b'\0');
        if hhmmss.chars().all(|c| c.is_ascii_digit()) && end == b']' {
            let after = &rest[7..]; // skip "HHMMSS]"
            out = after.strip_prefix('\n').unwrap_or(after);
        }
    }

    //（1）strip old footer
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
    //（1）兼容函数：历史上我们会把时间戳注入每条消息内容（如 `[t:HHMMSS]`）。
    //（2）但实践中这会被部分模型“学走并回显”，污染正文与工具调用判断，因此禁用内容级时间戳。
    //
    //（1）这里仅做“去除历史时间戳”，返回干净文本。
    strip_stamp_footer(text).trim_end().to_string()
}

fn normalize_messages_basic(messages: &[ApiMessage]) -> Vec<ApiMessage> {
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

/// Generic, provider-agnostic normalization for building model context snapshots:
/// - filter empty messages
/// - merge consecutive assistant messages
///   This is safe for all providers and all minds.
pub(crate) fn normalize_messages_for_model_context(messages: &[ApiMessage]) -> Vec<ApiMessage> {
    normalize_messages_basic(messages)
}

/// Provider-level normalization:
/// - Strictly protocol-level (never inject UI strings, never rewrite semantics).
/// - Keep behavior centralized per provider so Main/Dog/Memory share one rule set.
pub(crate) fn normalize_messages_for_provider(
    provider: &str,
    messages: &[ApiMessage],
) -> Vec<ApiMessage> {
    let p = crate::api::normalize_provider(provider);
    match p {
        //（1）DeepSeek rejects some sequences (e.g. consecutive
        //（2）assistant messages).
        //（3）We pre-merge assistant and strip empties for stability.
        "deepseek" => normalize_messages_basic(messages),
        //（1）Codex endpoints are generally tolerant, but we still
        //（2）apply the same basic cleanup.
        //（3）Keeping this consistent reduces edge cases across minds.
        "codex" => normalize_messages_basic(messages),
        _ => normalize_messages_basic(messages),
    }
}

/// Best-effort detector for "context pollution" where UI-rendered strings leak into model context.
/// Returns human-readable findings; callers should log, not mutate context here.
pub(crate) fn detect_context_contamination(messages: &[ApiMessage]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for (i, m) in messages.iter().enumerate() {
        let idx = i.saturating_add(1);
        let role = m.role.trim();
        let c = m.content.as_str();

        //（1）UI glyphs / headings that should never be sent to
        //（2）providers.
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

    if is_deepseek && let Some(last) = messages.last() {
        let role = last.role.trim();
        if role != "user" {
            score -= 20;
            deductions.push("-20: deepseek last role is not user".to_string());
        }
        let tail = last.content.trim();
        if is_role_needed_user_placeholder(tail) {
            //（1）ok: this tail exists only for protocol; do not treat as
            //（2）a user directive.
        } else if tail.contains("[sys:轮询占位]") {
            score -= 10;
            deductions.push("-10: sys poll tail uses legacy marker".to_string());
        } else if tail.contains("continue") || tail.contains("继续") {
            score -= 15;
            deductions.push("-15: last user tail contains directive-like words".to_string());
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
pub(crate) const DYNCONTEXT_PATH_DEFAULT: &str = "memory/context/context.jsonl";

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
    //（1）fastmemo 文件由记忆工具维护；这里仅做只读注入。
    let path = Path::new(FASTMEMO_PATH);
    //（1）结构自愈：避免 fastmemo v1/脏结构进入上下文，导致后续 memory_add/edit 逻辑出现偏差。
    let _ = crate::mcp::ensure_memory_file("fastmemo", FASTMEMO_PATH);
    let text = std::fs::read_to_string(path).unwrap_or_default();
    truncate_chars_safe(text.trim(), FASTMEMO_MAX_INJECT_CHARS)
}

pub(crate) fn fastmemo_event_count_and_any_ge10(text: &str) -> (usize, bool) {
    let mut current: Option<&str> = None;
    let mut self_n = 0usize;
    let mut user_n = 0usize;
    let mut env_n = 0usize;
    let mut dyn_n = 0usize;
    for line in text.lines() {
        let t = line.trim();
        if t.starts_with('[') && t.contains(']') {
            current = match t {
                "[自我感知]" => Some("自我感知"),
                "[用户感知]" => Some("用户感知"),
                "[环境感知]" => Some("环境感知"),
                "[动态上下文]" => Some("动态上下文"),
                //（1）兼容旧名
                "[事件感知]" => Some("动态上下文"),
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
            Some("动态上下文") => dyn_n += 1,
            _ => {}
        }
    }
    let any_ge10 = self_n >= 10 || user_n >= 10 || env_n >= 10 || dyn_n >= 10;
    (dyn_n, any_ge10)
}

pub(crate) fn fastmemo_max_section_usage(text: &str) -> Option<(&'static str, usize)> {
    //（1）返回“最接近 10 的那一栏”，并在并列时按固定优先级选择：
    //（2）自我感知 > 用户感知 > 环境感知 > 动态上下文
    let mut current: Option<&'static str> = None;
    let mut self_n = 0usize;
    let mut user_n = 0usize;
    let mut env_n = 0usize;
    let mut dyn_n = 0usize;
    for line in text.lines() {
        let t = line.trim();
        if t.starts_with('[') && t.contains(']') {
            current = match t {
                "[自我感知]" => Some("自我感知"),
                "[用户感知]" => Some("用户感知"),
                "[环境感知]" => Some("环境感知"),
                "[动态上下文]" => Some("动态上下文"),
                //（1）兼容旧名
                "[事件感知]" => Some("动态上下文"),
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
            Some("动态上下文") => dyn_n += 1,
            _ => {}
        }
    }
    let mut best_label = "自我感知";
    let mut best_n = self_n;
    for (label, n) in [
        ("用户感知", user_n),
        ("环境感知", env_n),
        ("动态上下文", dyn_n),
    ] {
        if n > best_n {
            best_label = label;
            best_n = n;
        }
    }
    if best_n == 0 {
        None
    } else {
        Some((best_label, best_n))
    }
}

fn dyncontext_ts() -> String {
    //（1）秒级即可；动态上下文注入本身会带入完整原文。
    chrono::Local::now()
        .format("%Y-%m-%d %H:%M:%S %:z")
        .to_string()
}

fn append_dyncontext(
    path: &str,
    mind: &str,
    role: &str,
    text: &str,
    tool: Option<&str>,
    meta: Option<&[String]>,
) -> anyhow::Result<()> {
    let clean = text.trim();
    if clean.is_empty() {
        return Ok(());
    }
    let mut entry = json!({
        "time": dyncontext_ts(),
        "mind": mind,
        "role": role,
        "content": clean,
    });
    if let Some(t) = tool.map(str::trim).filter(|s| !s.is_empty()) {
        entry["tool"] = json!(t);
    }
    if let Some(m) = meta
        && !m.is_empty()
    {
        entry["meta"] = json!(m);
    }
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
    Ok(())
}

pub(crate) fn log_dyncontext(
    path: &str,
    mind: &str,
    role: &str,
    text: &str,
    tool: Option<&str>,
    meta: Option<&[String]>,
) {
    if let Err(e) = append_dyncontext(path, mind, role, text, tool, meta) {
        eprintln!("dyncontext 写入失败: {e:#}");
    }
}

pub(crate) fn clear_dyncontext_file(path: &str) {
    let p = Path::new(path);
    if let Some(parent) = p.parent()
        && !parent.as_os_str().is_empty()
    {
        let _ = fs::create_dir_all(parent);
    }
    //（1）best-effort：重启清空会话上下文；失败不阻断启动
    let _ = fs::write(p, "");
}

#[derive(Debug, Clone, Deserialize)]
struct ContextLogEntry {
    #[serde(default)]
    #[allow(dead_code)]
    time: String,
    #[serde(default)]
    mind: String,
    #[serde(default)]
    role: String,
    #[serde(default)]
    content: String,
    #[serde(default)]
    tool: Option<String>,
    #[serde(default)]
    meta: Option<Vec<String>>,
}

fn render_tool_result_system_message(
    seq: u64,
    tool: &str,
    result_text: &str,
    meta: &[String],
    template: &str,
) -> ApiMessage {
    let clean = result_text.trim();
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
    let code = format!("code{:03}", seq);
    let header = format!(
        "[MCP:工具输出{code}]工具名：{} 状态：{status}{reason}{elapsed}{path}{saved}",
        tool.trim()
    );
    let body =
        crate::messages::render_template(template, &[("HEADER", &header), ("RESULT", clean)]);
    ApiMessage {
        role: "system".to_string(),
        content: body,
    }
}

pub(crate) fn read_contextlog_messages_for_mind(
    path: &str,
    mind: &str,
    tool_result_assistant_template: &str,
) -> Vec<ApiMessage> {
    let text = fs::read_to_string(path).unwrap_or_default();
    if text.trim().is_empty() {
        return Vec::new();
    }
    let want = mind.trim();
    let mut out: Vec<ApiMessage> = Vec::new();
    let mut tool_seq: u64 = 0;
    for line in text.lines() {
        let t = line.trim();
        if t.is_empty() {
            continue;
        }
        let Ok(entry) = serde_json::from_str::<ContextLogEntry>(t) else {
            continue;
        };
        if entry.mind.trim() != want {
            continue;
        }
        let role = entry.role.trim().to_ascii_lowercase();
        let content = entry.content.trim();
        if content.is_empty() {
            continue;
        }
        match role.as_str() {
            "user" => out.push(ApiMessage {
                role: "user".to_string(),
                content: content.to_string(),
            }),
            "assistant" => out.push(ApiMessage {
                role: "assistant".to_string(),
                content: content.to_string(),
            }),
            "system" => out.push(ApiMessage {
                role: "system".to_string(),
                content: content.to_string(),
            }),
            "tool" => {
                tool_seq = tool_seq.saturating_add(1);
                let tool = entry.tool.as_deref().unwrap_or("tool").trim();
                let meta = entry.meta.as_deref().unwrap_or(&[]);
                out.push(render_tool_result_system_message(
                    tool_seq,
                    tool,
                    content,
                    meta,
                    tool_result_assistant_template,
                ));
            }
            _ => {}
        }
    }
    out
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

pub(crate) fn contextmemo_size_kb(path: &str) -> usize {
    let bytes = fs::metadata(path).map(|m| m.len()).unwrap_or(0);
    //（1）0 bytes => 0KB；其余向上取整，便于阈值直观控制。
    bytes.div_ceil(1024) as usize
}

fn infer_mcp_exec_status_from_meta(meta: &[String]) -> &'static str {
    //（1）只依据 meta 推断状态：绝不能扫描 output 文本，
    //（2）否则会被 `ps` 等输出里的单词（如 `timeout` 进程名）
    //（3）误判为“超时/失败”，污染上下文与 UI 展示。
    let joined = meta
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
        .to_ascii_lowercase();

    //（1）超时：优先看 ok/result（工具层会在 meta 里写明）。
    if joined.contains("ok:timeout")
        || joined.contains("result:timeout")
        || joined.contains("状态:timeout")
        || joined.contains("状态:ok_timeout")
    {
        return "超时";
    }
    //（1）成功：优先 ok:true；兼容旧 meta。
    if joined.contains("ok:true") || joined.contains("状态:0") || joined.contains("result:0") {
        return "成功";
    }
    //（1）失败：明确 ok:false/状态:fail 等。
    if joined.contains("ok:false")
        || joined.contains("状态:fail")
        || joined.contains("tool failed")
        || joined.contains("工具执行失败")
    {
        return "失败";
    }
    //（1）不确定时默认失败（更安全）。
    "失败"
}

fn extract_meta_kv(meta: &[String], key: &str) -> Option<String> {
    let k = format!("{key}:");
    for line in meta {
        //（1）只解析 key:value 形式；meta 行通常为 `... | key:value | ...`
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

#[derive(Debug, Clone)]
pub(crate) struct DogState {
    messages: Vec<ApiMessage>,
    pub(crate) prompt: String,
    #[allow(dead_code)]
    prompt_reinject_pct: u8,
    tool_result_assistant_template: String,
    tool_output_seq: u64,
    last_tool_output_code: Option<String>,
    last_prompt_inject_user: usize,
    user_count: usize,
    used_tokens_est: usize,
    last_usage_total_tokens: u64,
    fastmemo_idx: Option<usize>,
    dyncontext_idx: Option<usize>,
    pub(crate) fastmemo_compact_pending: bool,
    include_tool_context: bool,
    date_injected: bool,
}

impl DogState {
    fn now_date_tz() -> String {
        //（1）只注入日期与时区，便于模型用 HHMM 构建“时间链”，同时减少 token。
        chrono::Local::now().format("%Y-%m-%d %:z").to_string()
    }

    #[allow(dead_code)]
    fn now_time_tz() -> String {
        //（1）仅秒级：足够让模型感知时间流动，且 token 成本低；ms 级对模型决策通常无价值。
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
            dyncontext_idx: None,
            fastmemo_compact_pending: false,
            include_tool_context: true,
            date_injected: false,
        };
        s.inject_prompt();
        s
    }

    fn remove_message_at(&mut self, pos: usize) -> Option<ApiMessage> {
        if pos >= self.messages.len() {
            return None;
        }
        let removed = self.messages.remove(pos);
        let old = crate::estimate_tokens(&removed.content);
        self.used_tokens_est = self.used_tokens_est.saturating_sub(old);
        Some(removed)
    }

    fn relocate_pinned_systems_after_prompt(&mut self) {
        //（1）确保“prompt → fastmemo → dyncontext”的相对顺序稳定：
        //（2）每次 inject_prompt() 发生时，
        //（3）把 pinned system blocks 重新移动到 prompt 之后（列表尾部）。
        let mut fast: Option<ApiMessage> = None;
        let mut dynctx: Option<ApiMessage> = None;

        let mut pins: Vec<(usize, &'static str)> = Vec::new();
        if let Some(i) = self.fastmemo_idx {
            if i < self.messages.len() {
                pins.push((i, "fast"));
            } else {
                self.fastmemo_idx = None;
            }
        }
        if let Some(i) = self.dyncontext_idx {
            if i < self.messages.len() {
                pins.push((i, "dyn"));
            } else {
                self.dyncontext_idx = None;
            }
        }
        if pins.is_empty() {
            return;
        }
        //（1）倒序删除，避免索引漂移。
        pins.sort_by_key(|(i, _)| *i);
        for (i, kind) in pins.into_iter().rev() {
            if let Some(msg) = self.remove_message_at(i) {
                match kind {
                    "fast" => fast = Some(msg),
                    "dyn" => dynctx = Some(msg),
                    _ => {}
                }
            }
        }
        self.fastmemo_idx = None;
        self.dyncontext_idx = None;

        if let Some(m) = fast {
            let pos = self.messages.len();
            self.push_message("system", Self::strip_stamp_footer(&m.content).to_string());
            self.fastmemo_idx = Some(pos);
        }
        if let Some(m) = dynctx {
            let pos = self.messages.len();
            self.push_message("system", Self::strip_stamp_footer(&m.content).to_string());
            self.dyncontext_idx = Some(pos);
        }
    }

    fn inject_prompt(&mut self) {
        //（1）日期只注入一次：避免 prompt reinject 时重复出现日期，造成上下文噪音与模型误判。
        if !self.date_injected {
            self.push_message("system", format!("[sys:日期]{}", Self::now_date_tz()));
            self.date_injected = true;
        }
        let prompt = self.prompt.trim();
        if prompt.is_empty() {
            return;
        }
        self.push_message("system", prompt.to_string());
        //（1）prompt reinject 之后，把 fastmemo/dyncontext 固定移动到 prompt 后面，
        //（2）避免顺序倒置。
        self.relocate_pinned_systems_after_prompt();
    }

    fn push_message(&mut self, role: &str, content: String) {
        let clean = content.trim();
        if clean.is_empty() {
            return;
        }
        //（1）DeepSeek 会拒绝连续 assistant 消息（400: Invalid consecutive
        //（2）assistant message）。
        //（3）工具链/自动续写等场景可能造成 assistant 连续出现；这里做最小合并，保持请求合法。
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

    #[allow(dead_code)]
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
        //（1）时间线：每条用户消息前注入一次时间（system role），
        //（2）避免在 assistant 内容中注入时间戳造成回显/污染。
        self.push_message("system", format!("[sys:时间]{}", Self::now_time_tz()));
        self.push_message("user", text.trim().to_string());
        reinject_now
    }

    #[allow(dead_code)]
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
        //（1）兼容旧调用：无法得知 tool 名称与 meta，只能把整段内容作为“工具输出”写回上下文。
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
        //（1）工具回执（工具输出）写回上下文：
        //（2）不能用 user：否则模型会把“工具输出”误判为“用户说的话/用户执行的结果”，进而误学工具协议；
        //（3）不能用 tool role（DeepSeek/Codex 协议层不统一）；
        //（4）这里使用 system，并用固定前缀显式声明“这是工具输出，不是系统指令/用户输入”。
        //
        //（1）注意：system 的优先级更高，因此 tool_result_assistant_template
        //（2）必须足够明确、简短，
        //（3）且禁止混入任何 UI 渲染文本（●/Ran/TOOL: 等）。
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
        self.dyncontext_idx = None;
        self.fastmemo_compact_pending = false;
        self.tool_output_seq = 0;
        self.last_tool_output_code = None;
        self.inject_prompt();
    }

    pub(crate) fn messages_clone(&self) -> Vec<ApiMessage> {
        self.messages.clone()
    }

    #[allow(dead_code)]
    pub(crate) fn message_snapshot(&self, extra_system: Option<&str>) -> Vec<ApiMessage> {
        //（1）Provider-facing: strip any historical `[t:HHMMSS]
        //（2）` stamps to avoid models echoing them.
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
                //（1）额外指令按 system 角色入列，保持上下文语义稳定。
                out.push(ApiMessage {
                    role: "system".to_string(),
                    content: Self::with_stamp_footer(clean),
                });
            }
        }
        normalize_messages_for_model_context(&out)
    }

    pub(crate) fn message_snapshot_with_context_file(
        &self,
        extra_system: Option<&str>,
        context_path: &str,
        mind: &str,
    ) -> Vec<ApiMessage> {
        //（1）Provider-facing: pinned blocks come from in-memory
        //（2）snapshot (prompt/fastmemo),
        //（3）while turn history & tool receipts are replayed from the
        //（4）external context JSONL.
        let mut out: Vec<ApiMessage> = self
            .messages
            .iter()
            .map(|m| ApiMessage {
                role: m.role.clone(),
                content: Self::strip_stamp_footer(&m.content).trim().to_string(),
            })
            .filter(|m| !m.role.trim().is_empty() && !m.content.trim().is_empty())
            .collect();
        let mut ctx = read_contextlog_messages_for_mind(
            context_path,
            mind,
            self.tool_result_assistant_template.as_str(),
        );
        //（1）strip any legacy stamps (should be rare, but keep
        //（2）consistent)
        for m in &mut ctx {
            m.content = Self::strip_stamp_footer(&m.content).trim().to_string();
        }
        out.append(&mut ctx);
        if let Some(extra) = extra_system {
            let clean = extra.trim();
            if !clean.is_empty() {
                out.push(ApiMessage {
                    role: "system".to_string(),
                    content: Self::with_stamp_footer(clean),
                });
            }
        }
        normalize_messages_for_model_context(&out)
    }

    pub(crate) fn set_include_tool_context(&mut self, enabled: bool) {
        self.include_tool_context = enabled;
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

    #[allow(dead_code)]
    pub(crate) fn refresh_dyncontext_system(&mut self, text: &str) {
        let clean = text.trim();
        if clean.is_empty() {
            self.dyncontext_idx = None;
            return;
        }
        let body = format!("【动态上下文 context】\n{clean}");
        let mut idx = self.dyncontext_idx;
        self.upsert_system_message(&mut idx, &body);
        self.dyncontext_idx = idx;
    }
}
