use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::config::SystemConfig;
use crate::mcp::ToolCall;

const DEFAULT_MCP_MESSAGES_JSON: &str = include_str!("../prompts/mcpprompt/messages.json");

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct McpMessages {
    pub deepseek_tool_loop_tick_user: String,
    pub heartbeat_user: String,
    pub plan_mixed_batch_reject: String,
    pub tool_confirm_prompt: String,
}

impl Default for McpMessages {
    fn default() -> Self {
        serde_json::from_str::<McpMessages>(DEFAULT_MCP_MESSAGES_JSON).unwrap_or_else(|_| McpMessages {
            deepseek_tool_loop_tick_user: "[AITERMUX_INTERNAL_TOOL_LOOP] system injected (not user input). Continue based on previous tool result.".to_string(),
            heartbeat_user: "heartbeat: {STAMP} | idle".to_string(),
            plan_mixed_batch_reject: "plan mixed with other tools in the same batch; rejected.".to_string(),
            tool_confirm_prompt: "Confirm tool call? {REASON} {TOOL} {CALL_JSON}".to_string(),
        })
    }
}

pub fn load_mcp_messages(sys_cfg: &SystemConfig) -> (McpMessages, Option<String>, PathBuf) {
    let path = crate::resolve_config_path(&sys_cfg.mcp_messages_path, true);
    let (raw, err) =
        crate::load_prompt_file_with_default(&path, DEFAULT_MCP_MESSAGES_JSON, "MCP-MESSAGES");

    match serde_json::from_str::<McpMessages>(&raw) {
        Ok(msgs) => (msgs, err, path),
        Err(e) => {
            let mut msg = format!("MCP-MESSAGES JSON 解析失败：{e}");
            if let Some(prev) = err {
                msg = format!("{prev}; {msg}");
            }
            let _ = crate::store_prompt_file(&path, DEFAULT_MCP_MESSAGES_JSON);
            (McpMessages::default(), Some(msg), path)
        }
    }
}

pub fn render_template(template: &str, pairs: &[(&str, &str)]) -> String {
    let mut out = template.to_string();
    for (k, v) in pairs {
        out = out.replace(&format!("{{{k}}}"), v);
    }
    out
}

pub fn render_heartbeat_user(msgs: &McpMessages, stamp: &str) -> String {
    render_template(&msgs.heartbeat_user, &[("STAMP", stamp)])
}

pub fn render_tool_confirm_prompt(msgs: &McpMessages, reason: &str, call: &ToolCall) -> String {
    let tool_label = crate::mcp::tool_display_label(&call.tool);
    let call_json = serde_json::json!({
        "tool": call.tool.as_str(),
        "input": call.input.as_str(),
        "brief": call.brief.as_deref(),
    });
    let mut call_json =
        serde_json::to_string_pretty(&call_json).unwrap_or_else(|_| "{}".to_string());
    call_json = crate::truncate_with_suffix(&call_json, 12_000);

    render_template(
        &msgs.tool_confirm_prompt,
        &[
            ("REASON", reason),
            ("TOOL", &tool_label),
            ("CALL_JSON", &call_json),
        ],
    )
}
