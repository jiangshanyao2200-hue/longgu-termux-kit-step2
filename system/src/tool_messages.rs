use std::path::PathBuf;
use std::sync::OnceLock;

use serde::{Deserialize, Serialize};

use crate::config::SystemConfig;

const DEFAULT_TOOL_MESSAGES_JSON: &str = include_str!("../config/prompt/mcp/tool_messages.json");

fn default_tool_line() -> String {
    "TOOL: {TOOL}\n".to_string()
}
fn default_explain_line() -> String {
    "EXPLAIN: {BRIEF}\n".to_string()
}
fn default_input_line() -> String {
    "INPUT: {INPUT}\n".to_string()
}
fn default_output_header() -> String {
    "OUTPUT:\n```text\n".to_string()
}
fn default_output_footer() -> String {
    "\n```\n".to_string()
}
fn default_meta_header() -> String {
    "META:\n```text\n".to_string()
}
fn default_meta_footer() -> String {
    "\n```\n".to_string()
}
fn default_no_output() -> String {
    "(NO OUTPUT)".to_string()
}
fn default_unknown_tool() -> String {
    "UNKNOWN TOOL: {TOOL}".to_string()
}
fn default_tool_failed() -> String {
    "TOOL FAILED: {ERR}".to_string()
}
fn default_tool_failed_generic() -> String {
    "TOOL FAILED".to_string()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolMessages {
    #[serde(default = "default_tool_line")]
    pub tool_line: String,
    #[serde(default = "default_explain_line")]
    pub explain_line: String,
    #[serde(default = "default_input_line")]
    pub input_line: String,
    #[serde(default = "default_output_header")]
    pub output_header: String,
    #[serde(default = "default_output_footer")]
    pub output_footer: String,
    #[serde(default = "default_meta_header")]
    pub meta_header: String,
    #[serde(default = "default_meta_footer")]
    pub meta_footer: String,
    #[serde(default = "default_no_output")]
    pub no_output: String,

    #[serde(default = "default_unknown_tool")]
    pub unknown_tool: String,
    #[serde(default = "default_tool_failed")]
    pub tool_failed: String,
    #[serde(default = "default_tool_failed_generic")]
    pub tool_failed_generic: String,
}

impl Default for ToolMessages {
    fn default() -> Self {
        ToolMessages {
            tool_line: default_tool_line(),
            explain_line: default_explain_line(),
            input_line: default_input_line(),
            output_header: default_output_header(),
            output_footer: default_output_footer(),
            meta_header: default_meta_header(),
            meta_footer: default_meta_footer(),
            no_output: default_no_output(),
            unknown_tool: default_unknown_tool(),
            tool_failed: default_tool_failed(),
            tool_failed_generic: default_tool_failed_generic(),
        }
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

#[cfg(test)]
mod tests {
    use super::ToolMessages;

    #[test]
    fn can_deserialize_empty_object() {
        let msgs: ToolMessages = serde_json::from_str("{}").expect("deserialize {}");
        assert!(msgs.tool_line.contains("{TOOL}"));
        assert!(msgs.input_line.contains("{INPUT}"));
        assert!(!msgs.tool_failed_generic.trim().is_empty());
    }
}
