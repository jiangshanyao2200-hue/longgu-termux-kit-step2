use std::path::PathBuf;
use std::sync::OnceLock;

use serde::{Deserialize, Serialize};

use crate::config::SystemConfig;

const DEFAULT_TOOL_MESSAGES_JSON: &str =
    include_str!("../config/systemmessage/model/tool_messages.json");

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct ToolMessages {
    pub tool_line: String,
    pub explain_line: String,
    pub input_line: String,
    pub output_header: String,
    pub output_footer: String,
    pub meta_header: String,
    pub meta_footer: String,
    pub no_output: String,

    pub unknown_tool: String,
    pub tool_failed: String,
    pub tool_failed_generic: String,
}

impl Default for ToolMessages {
    fn default() -> Self {
        serde_json::from_str::<ToolMessages>(DEFAULT_TOOL_MESSAGES_JSON).unwrap_or_else(|_| {
            ToolMessages {
                tool_line: "操作: {TOOL}\n".to_string(),
                explain_line: "说明: {BRIEF}\n".to_string(),
                input_line: "输入: {INPUT}\n".to_string(),
                output_header: "输出:\n```text\n".to_string(),
                output_footer: "\n```\n".to_string(),
                meta_header: "元信息:\n```text\n".to_string(),
                meta_footer: "\n```\n".to_string(),
                no_output: "(无输出)".to_string(),
                unknown_tool: "未知工具：{TOOL}".to_string(),
                tool_failed: "工具执行失败：{ERR}".to_string(),
                tool_failed_generic: "工具执行失败".to_string(),
            }
        })
    }
}

static TOOL_MESSAGES: OnceLock<ToolMessages> = OnceLock::new();

pub fn set_tool_messages(msgs: ToolMessages) {
    let _ = TOOL_MESSAGES.set(msgs);
}

pub fn tool_messages() -> &'static ToolMessages {
    TOOL_MESSAGES.get_or_init(|| ToolMessages::default())
}

pub fn load_tool_messages(sys_cfg: &SystemConfig) -> (ToolMessages, Option<String>, PathBuf) {
    let path = crate::resolve_config_path(&sys_cfg.tool_messages_path, true);
    let (raw, err) =
        crate::load_prompt_file_with_default(&path, DEFAULT_TOOL_MESSAGES_JSON, "TOOL-MESSAGES");
    match serde_json::from_str::<ToolMessages>(&raw) {
        Ok(msgs) => (msgs, err, path),
        Err(e) => {
            let mut msg = format!("TOOL-MESSAGES JSON 解析失败：{e}");
            if let Some(prev) = err {
                msg = format!("{prev}; {msg}");
            }
            let _ = crate::store_prompt_file(&path, DEFAULT_TOOL_MESSAGES_JSON);
            (ToolMessages::default(), Some(msg), path)
        }
    }
}

pub fn render_tool_template(template: &str, pairs: &[(&str, &str)]) -> String {
    let mut out = template.to_string();
    for (k, v) in pairs {
        out = out.replace(&format!("{{{k}}}"), v);
    }
    out
}
