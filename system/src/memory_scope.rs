use std::fs;
use std::path::Path;

pub(crate) const SCOPE_DIR: &str = "memory/scopes";
const PREFIX: &str = "scopememo_";

pub(crate) fn ensure_scope_dir() {
    let _ = fs::create_dir_all(SCOPE_DIR);
}

// 允许中文/空格等，但必须是“文件名安全”的单段标识：
// - 禁止路径分隔符，避免穿越
// - 会把空白折叠为 `_`
// - 限长，避免生成超长文件名
pub(crate) fn normalize_scope_name(raw: &str) -> Option<String> {
    let mut s = raw.trim().to_string();
    if s.is_empty() {
        return None;
    }
    if s.contains('/') || s.contains('\\') || s.contains('\0') || s.contains(':') {
        return None;
    }
    if s == "." || s == ".." || s.contains("..") {
        return None;
    }
    // 将空白折叠为 `_`，避免 scope 名在工具 JSON 中因换行/空格造成歧义。
    s = s.split_whitespace().collect::<Vec<_>>().join("_");
    // 避免隐藏文件（以 `.` 开头）
    s = s.trim_start_matches('.').to_string();
    if s.is_empty() {
        return None;
    }
    if s.chars().count() > 64 {
        s = s.chars().take(64).collect();
    }
    Some(s)
}

pub(crate) fn scopememo_rel_path(scope: &str) -> Option<String> {
    let name = normalize_scope_name(scope)?;
    Some(format!("{SCOPE_DIR}/{PREFIX}{name}.jsonl"))
}

#[allow(dead_code)]
pub(crate) fn is_scopememo_path(path: &str) -> bool {
    path.to_ascii_lowercase().contains("scopememo")
}

#[allow(dead_code)]
pub(crate) fn list_scopes() -> Vec<String> {
    let mut out = Vec::new();
    let dir = Path::new(SCOPE_DIR);
    let Ok(rd) = fs::read_dir(dir) else {
        return out;
    };
    for ent in rd.flatten() {
        let Ok(ft) = ent.file_type() else {
            continue;
        };
        if !ft.is_file() {
            continue;
        }
        let name = ent.file_name().to_string_lossy().to_string();
        if !name.starts_with(PREFIX) || !name.ends_with(".jsonl") {
            continue;
        }
        let stem = name
            .trim_start_matches(PREFIX)
            .trim_end_matches(".jsonl")
            .to_string();
        if !stem.is_empty() {
            out.push(stem);
        }
    }
    out.sort();
    out
}
