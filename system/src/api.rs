use serde_json::{Value, json};

use crate::ApiMessage;

#[derive(Debug, Clone, Copy)]
pub(crate) struct ProviderCapabilities {
    pub(crate) supports_reasoning_effort: bool,
    pub(crate) supports_temperature: bool,
    pub(crate) supports_max_tokens: bool,
}

const DEEPSEEK_MODELS: [&str; 2] = ["deepseek-chat", "deepseek-reasoner"];
const CODEX_MODELS: [&str; 3] = ["gpt-5.3-codex", "gpt-5.2", "gpt-5.1-codex-max"];
const CODEX_REASONING_EFFORTS: [&str; 4] = ["low", "medium", "high", "xhigh"];

pub(crate) fn capabilities(provider: &str) -> ProviderCapabilities {
    if normalize_provider(provider) == "codex" {
        ProviderCapabilities {
            supports_reasoning_effort: true,
            supports_temperature: false,
            supports_max_tokens: false,
        }
    } else {
        ProviderCapabilities {
            supports_reasoning_effort: false,
            supports_temperature: true,
            supports_max_tokens: true,
        }
    }
}

pub(crate) fn normalize_provider(value: &str) -> &str {
    let trimmed = value.trim();
    if trimmed.eq_ignore_ascii_case("deepseek") {
        "deepseek"
    } else if trimmed.eq_ignore_ascii_case("codex") {
        "codex"
    } else {
        "deepseek"
    }
}

pub(crate) fn provider_label(value: &str) -> &'static str {
    match normalize_provider(value) {
        "deepseek" => "DeepSeek",
        "codex" => "Codex",
        _ => "Unknown",
    }
}

pub(crate) fn model_label(value: &str) -> &'static str {
    let v = value.trim().to_ascii_lowercase();
    if v == "gpt-5.3-codex" {
        "GPT-5.3 Codex"
    } else if v == "gpt-5.2" {
        "GPT-5.2"
    } else if v == "gpt-5.1-codex-max" {
        "GPT-5.1 Codex Max"
    } else if v.contains("reasoner") {
        "DeepSeek Reasoner"
    } else if v.contains("chat") {
        "DeepSeek Chat"
    } else {
        "自定义"
    }
}

pub(crate) fn available_models_for_provider(provider: &str) -> &'static [&'static str] {
    match normalize_provider(provider) {
        "codex" => &CODEX_MODELS,
        _ => &DEEPSEEK_MODELS,
    }
}

pub(crate) fn default_model_for_provider(provider: &str) -> &'static str {
    available_models_for_provider(provider)
        .first()
        .copied()
        .unwrap_or("deepseek-chat")
}

pub(crate) fn normalize_reasoning_effort(value: Option<&str>) -> Option<&'static str> {
    let raw = value?;
    let t = raw.trim();
    if t.eq_ignore_ascii_case("low") {
        Some("low")
    } else if t.eq_ignore_ascii_case("medium") {
        Some("medium")
    } else if t.eq_ignore_ascii_case("high") {
        Some("high")
    } else if t.eq_ignore_ascii_case("xhigh") {
        Some("xhigh")
    } else {
        None
    }
}

pub(crate) fn normalize_reasoning_effort_or_default(value: Option<&str>) -> &'static str {
    normalize_reasoning_effort(value).unwrap_or("high")
}

pub(crate) fn format_reasoning_effort(provider: &str, value: Option<&str>) -> String {
    if !capabilities(provider).supports_reasoning_effort {
        return "N/A".to_string();
    }
    normalize_reasoning_effort_or_default(value).to_string()
}

pub(crate) fn next_cycle_value(current: &str, options: &[&str]) -> String {
    if options.is_empty() {
        return String::new();
    }
    let idx = options
        .iter()
        .position(|v| v.eq_ignore_ascii_case(current.trim()))
        .unwrap_or(usize::MAX);
    if idx == usize::MAX || idx + 1 >= options.len() {
        options[0].to_string()
    } else {
        options[idx + 1].to_string()
    }
}

pub(crate) fn next_model(current: &str, provider: &str) -> String {
    next_cycle_value(current, available_models_for_provider(provider))
}

pub(crate) fn next_reasoning_effort(current: &str) -> String {
    next_cycle_value(current, &CODEX_REASONING_EFFORTS)
}

fn normalize_role_for_codex(role: &str) -> &'static str {
    match role.trim().to_ascii_lowercase().as_str() {
        "system" => "system",
        "assistant" => "assistant",
        "user" => "user",
        _ => "user",
    }
}

pub(crate) fn build_codex_input_messages(messages: &[ApiMessage]) -> Vec<Value> {
    let mut out = Vec::with_capacity(messages.len());
    for msg in messages {
        let text = msg.content.trim();
        if text.is_empty() {
            continue;
        }
        out.push(json!({
            "role": normalize_role_for_codex(&msg.role),
            "content": [
                {
                    "type": "input_text",
                    "text": text,
                }
            ],
        }));
    }
    if out.is_empty() {
        out.push(json!({
            "role": "user",
            "content": [
                {
                    "type": "input_text",
                    "text": "continue"
                }
            ],
        }));
    }
    out
}
