use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::config::SystemConfig;
use crate::mcp::ToolCall;

const DEFAULT_MCP_MESSAGES_JSON: &str = include_str!("../config/prompt/mcp/mcp_messages.json");

fn default_deepseek_tool_loop_tick_user() -> String {
    "[AITERMUX_INTERNAL_TOOL_LOOP] 系统注入：非用户输入。请基于上一条 Tool result 继续当前任务。"
        .to_string()
}

fn default_heartbeat_user() -> String {
    "心跳信号：{STAMP} | idle\n请像正常消息一样回复（可简短）。若无需回应请只回复 [mainpass]。不要调用工具。"
        .to_string()
}

fn default_plan_mixed_batch_reject() -> String {
    "检测到 plan 与其它工具同批输出：已拒绝本轮工具执行。请将 plan:start / plan:end 单独一条回复输出；执行阶段每轮只调用一个工具。"
        .to_string()
}

fn default_tool_confirm_prompt() -> String {
    "系统：该工具需要用户确认。\n原因：{REASON}\n\n工具：{TOOL}\n调用：\n```json\n{CALL_JSON}\n```\n\n请在输入框回复 y/n（或 是/否）。"
        .to_string()
}

fn default_tool_result_assistant() -> String {
    "Tool result:\n{RESULT}".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpMessages {
    #[serde(default = "default_deepseek_tool_loop_tick_user")]
    pub deepseek_tool_loop_tick_user: String,
    #[serde(default = "default_heartbeat_user")]
    pub heartbeat_user: String,
    #[serde(default = "default_plan_mixed_batch_reject")]
    pub plan_mixed_batch_reject: String,
    #[serde(default = "default_tool_confirm_prompt")]
    pub tool_confirm_prompt: String,
    #[serde(default = "default_tool_result_assistant")]
    pub tool_result_assistant: String,
}

impl Default for McpMessages {
    fn default() -> Self {
        McpMessages {
            deepseek_tool_loop_tick_user: default_deepseek_tool_loop_tick_user(),
            heartbeat_user: default_heartbeat_user(),
            plan_mixed_batch_reject: default_plan_mixed_batch_reject(),
            tool_confirm_prompt: default_tool_confirm_prompt(),
            tool_result_assistant: default_tool_result_assistant(),
        }
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
