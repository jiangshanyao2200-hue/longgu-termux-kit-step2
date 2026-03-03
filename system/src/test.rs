use std::collections::{HashMap, VecDeque};
use std::fmt::Write as _;
use std::fs;
use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crossterm::event::{DisableBracketedPaste, DisableMouseCapture};
use crossterm::execute;
use crossterm::terminal::LeaveAlternateScreen;
use serde_json::json;

use crate::types::Core;
use crate::{ApiMessage, MindKind};

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct TestConfig {
    pub(crate) enabled: bool,
    pub(crate) show_system_bootstrap: bool,
    pub(crate) block_model_requests: bool,
}

static TEST_CONFIG: OnceLock<TestConfig> = OnceLock::new();
const TEST_MODE_FORCE_DISABLED: bool = true;

fn parse_bool_env(key: &str) -> Option<bool> {
    let raw = std::env::var(key).ok()?;
    let v = raw.trim().to_ascii_lowercase();
    match v.as_str() {
        "1" | "true" | "on" | "yes" | "y" => Some(true),
        "0" | "false" | "off" | "no" | "n" => Some(false),
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DebugTraceConfig {
    pub(crate) runtime_log_enabled: bool,
    pub(crate) runtime_log_stream_chunks: bool,
    pub(crate) contexttest_enabled: bool,
    pub(crate) startup_trace_enabled: bool,
    pub(crate) crash_log_enabled: bool,
    pub(crate) contexttest_path: PathBuf,
    pub(crate) startup_trace_path: PathBuf,
}

static DEBUG_TRACE_CONFIG: OnceLock<DebugTraceConfig> = OnceLock::new();

fn find_repo_root_from(mut dir: PathBuf) -> Option<PathBuf> {
    for _ in 0..10 {
        if dir.join("Cargo.toml").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            break;
        }
    }
    None
}

fn repo_root_best_effort() -> Option<PathBuf> {
    if let Ok(dir) = std::env::current_dir()
        && let Some(root) = find_repo_root_from(dir)
    {
        return Some(root);
    }
    if let Ok(exe) = std::env::current_exe()
        && let Some(parent) = exe.parent()
        && let Some(root) = find_repo_root_from(parent.to_path_buf())
    {
        return Some(root);
    }
    None
}

fn resolve_path_from_env(key: &str, default_rel: &str) -> PathBuf {
    let raw = std::env::var(key).ok().unwrap_or_default();
    let raw = raw.trim();
    if !raw.is_empty() {
        if raw.starts_with('/') {
            return PathBuf::from(raw);
        }
        if let Some(root) = repo_root_best_effort() {
            return root.join(raw);
        }
        return PathBuf::from(raw);
    }
    if let Some(root) = repo_root_best_effort() {
        return root.join(default_rel);
    }
    PathBuf::from(default_rel)
}

fn load_debug_trace_from_env() -> DebugTraceConfig {
    DebugTraceConfig {
        //（1）默认开启：便于追踪上下文工程与启动问题。
        runtime_log_enabled: parse_bool_env("YING_RUNTIME_LOG").unwrap_or(true),
        runtime_log_stream_chunks: parse_bool_env("YING_RUN_LOG_STREAM_CHUNKS")
            .or_else(|| parse_bool_env("YING_RUNLOG_STREAM_CHUNKS"))
            .unwrap_or(false),
        contexttest_enabled: parse_bool_env("YING_CONTEXTTEST").unwrap_or(true),
        startup_trace_enabled: parse_bool_env("YING_STARTUP_TRACE").unwrap_or(true),
        crash_log_enabled: parse_bool_env("YING_CRASH_LOG_ENABLED").unwrap_or(true),
        contexttest_path: resolve_path_from_env("YING_CONTEXTTEST_PATH", "log/contexttest.txt"),
        startup_trace_path: resolve_path_from_env(
            "YING_STARTUP_TRACE_PATH",
            "log/startup_trace.log",
        ),
    }
}

pub(crate) fn debug_trace_config() -> &'static DebugTraceConfig {
    DEBUG_TRACE_CONFIG.get_or_init(load_debug_trace_from_env)
}

fn now_ts() -> String {
    //（1）Debug timestamps need millisecond precision for interval
    //（2）analysis.
    //（3）Example: 2026-02-24 10:44:09.123 +08:00
    chrono::Local::now()
        .format("%Y-%m-%d %H:%M:%S%.3f %:z")
        .to_string()
}

pub(crate) fn trace_startup(msg: &str) {
    let cfg = debug_trace_config();
    if !cfg.startup_trace_enabled {
        return;
    }
    let path = &cfg.startup_trace_path;
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    if let Ok(mut f) = fs::OpenOptions::new().create(true).append(true).open(path) {
        let _ = writeln!(f, "[{}] {msg}", now_ts());
    }
}

pub(crate) fn recover_terminal_best_effort() {
    crossterm::terminal::disable_raw_mode().ok();
    execute!(
        io::stdout(),
        DisableBracketedPaste,
        DisableMouseCapture,
        LeaveAlternateScreen,
        crossterm::cursor::Show
    )
    .ok();
}

fn resolve_crash_log_path() -> PathBuf {
    //（1）兼容旧变量：该变量本来用于覆盖 crash log 路径；
    //（2）现在额外用 CRASH_LOG_ENABLED 控制开关。
    if let Ok(p) = std::env::var("YING_CRASH_LOG") {
        let p = p.trim();
        if !p.is_empty() {
            if p.starts_with('/') {
                return PathBuf::from(p);
            }
            if let Some(root) = repo_root_best_effort() {
                return root.join(p);
            }
            return PathBuf::from(p);
        }
    }
    if let Some(root) = repo_root_best_effort() {
        return root.join("log").join("crash.log");
    }
    let home = std::env::var("HOME").unwrap_or_else(|_| "/data/data/com.termux/files/home".into());
    PathBuf::from(home).join(".ying_crash.log")
}

pub(crate) fn install_crash_hook() {
    let cfg = debug_trace_config();
    if !cfg.crash_log_enabled {
        return;
    }
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        recover_terminal_best_effort();

        let path = resolve_crash_log_path();
        if let Some(parent) = path.parent() {
            let _ = fs::create_dir_all(parent);
        }
        if let Ok(mut f) = fs::OpenOptions::new().create(true).append(true).open(&path) {
            let _ = writeln!(f, "\n=== CRASH {} ===", now_ts());
            let _ = writeln!(f, "panic: {info}");
            let bt = std::backtrace::Backtrace::force_capture();
            let _ = writeln!(f, "backtrace:\n{bt}");
        }

        prev(info);
    }));
}

#[derive(Debug)]
struct RunLogger {
    writer: Mutex<io::BufWriter<fs::File>>,
    run_id: String,
    seq: AtomicU64,
    stream_chunks: bool,
}

static RUN_LOGGER: OnceLock<RunLogger> = OnceLock::new();

pub(crate) fn init_runtime_log(path: &str) {
    let cfg = debug_trace_config();
    if !cfg.runtime_log_enabled {
        return;
    }
    if path.trim().is_empty() {
        return;
    }
    if RUN_LOGGER.get().is_some() {
        return;
    }
    let p = Path::new(path);
    if let Some(parent) = p.parent()
        && !parent.as_os_str().is_empty()
    {
        let _ = fs::create_dir_all(parent);
    }
    let Ok(file) = fs::OpenOptions::new().create(true).append(true).open(p) else {
        return;
    };
    let run_id = format!(
        "{}:{}:{}",
        std::process::id(),
        now_ts(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(1)
    );
    let _ = RUN_LOGGER.set(RunLogger {
        writer: Mutex::new(io::BufWriter::new(file)),
        run_id,
        seq: AtomicU64::new(0),
        stream_chunks: cfg.runtime_log_stream_chunks,
    });
}

pub(crate) fn runtime_log_stream_chunks() -> bool {
    debug_trace_config().runtime_log_enabled
        && RUN_LOGGER.get().map(|l| l.stream_chunks).unwrap_or(false)
}

fn redact_log_value(value: serde_json::Value) -> serde_json::Value {
    fn is_sensitive_key(key: &str) -> bool {
        let k = key.trim().to_ascii_lowercase();
        matches!(
            k.as_str(),
            "api_key"
                | "apikey"
                | "authorization"
                | "auth"
                | "bearer"
                | "token"
                | "secret"
                | "password"
                | "pass"
        )
    }
    fn walk(v: &mut serde_json::Value) {
        match v {
            serde_json::Value::Object(map) => {
                for (k, child) in map.iter_mut() {
                    if is_sensitive_key(k) {
                        *child = serde_json::Value::String("<redacted>".to_string());
                    } else {
                        walk(child);
                    }
                }
            }
            serde_json::Value::Array(items) => {
                for item in items.iter_mut() {
                    walk(item);
                }
            }
            _ => {}
        }
    }
    let mut out = value;
    walk(&mut out);
    out
}

pub(crate) fn runlog_event(level: &str, event: &str, data: serde_json::Value) {
    let cfg = debug_trace_config();
    if !cfg.runtime_log_enabled {
        return;
    }
    let Some(logger) = RUN_LOGGER.get() else {
        return;
    };
    let seq = logger.seq.fetch_add(1, Ordering::Relaxed).saturating_add(1);
    let ts = now_ts();
    let thread = format!("{:?}", std::thread::current().id());
    let data_pretty = serde_json::to_string_pretty(&redact_log_value(data))
        .unwrap_or_else(|_| "<failed to pretty-print data>".to_string());
    if let Ok(mut w) = logger.writer.lock() {
        let _ = writeln!(
            w,
            "==== [{ts}] [{level}] {event} seq={seq} run_id={} thread={thread} ====",
            logger.run_id
        );
        let _ = w.write_all(data_pretty.as_bytes());
        let _ = w.write_all(b"\n");
        let _ = w.write_all(b"----\n");
        //（1）调试期宁可多 flush（便于实时观察），也不要“看起来像没启用日志”。
        let _ = w.flush();
    }
}

pub(crate) fn append_run_log(path: &str, level: &str, msg: &str) {
    let cfg = debug_trace_config();
    if !cfg.runtime_log_enabled {
        return;
    }
    init_runtime_log(path);
    runlog_event(level, "log", json!({ "msg": msg.trim() }));
}

#[derive(Debug)]
struct ContextTestLogger {
    path: PathBuf,
    seq: AtomicU64,
    latest_started_seq: AtomicU64,
    pending: Mutex<HashMap<ContextTestKey, PendingContextTest>>,
}

static CONTEXTTEST_LOGGER: OnceLock<ContextTestLogger> = OnceLock::new();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ContextTestKey {
    mind: u8,
    request_id: u64,
}

#[derive(Debug, Clone)]
struct PendingContextTest {
    seq: u64,
    request_block: String,
}

fn mind_key(mind: MindKind) -> u8 {
    match mind {
        MindKind::Main => 0,
        MindKind::Sub => 1,
        MindKind::Memory => 2,
    }
}

fn write_contexttest_snapshot(path: &Path, text: &str) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    //（1）只保留最后一轮：覆盖写（truncate），避免文件无限膨胀。
    let _ = fs::write(path, text);
}

fn init_contexttest_logger() -> Option<&'static ContextTestLogger> {
    let cfg = debug_trace_config();
    if !cfg.contexttest_enabled {
        return None;
    }
    if let Some(l) = CONTEXTTEST_LOGGER.get() {
        return Some(l);
    }
    let _ = CONTEXTTEST_LOGGER.set(ContextTestLogger {
        path: cfg.contexttest_path.clone(),
        seq: AtomicU64::new(0),
        latest_started_seq: AtomicU64::new(0),
        pending: Mutex::new(HashMap::new()),
    });
    CONTEXTTEST_LOGGER.get()
}

pub(crate) struct ContextTestRequestStartArgs<'a> {
    pub(crate) mind: MindKind,
    pub(crate) request_id: u64,
    pub(crate) stream: bool,
    pub(crate) provider: &'a str,
    pub(crate) base_url: &'a str,
    pub(crate) model: &'a str,
    pub(crate) reasoning_effort: Option<&'a str>,
    pub(crate) temperature: Option<f32>,
    pub(crate) max_tokens: Option<u32>,
    pub(crate) timeout_secs: u64,
    pub(crate) messages: &'a [ApiMessage],
}

pub(crate) struct ContextTestResponseEndArgs<'a> {
    pub(crate) mind: MindKind,
    pub(crate) request_id: u64,
    pub(crate) usage_total_tokens: u64,
    pub(crate) error: Option<&'a str>,
    pub(crate) brief: Option<&'a str>,
    pub(crate) thinking_full: Option<&'a str>,
    pub(crate) raw_text: Option<&'a str>,
    pub(crate) assistant_text: Option<&'a str>,
}

pub(crate) fn contexttest_log_request_start(args: ContextTestRequestStartArgs<'_>) {
    let ContextTestRequestStartArgs {
        mind,
        request_id,
        stream,
        provider,
        base_url,
        model,
        reasoning_effort,
        temperature,
        max_tokens,
        timeout_secs,
        messages,
    } = args;

    let Some(logger) = init_contexttest_logger() else {
        return;
    };
    let seq = logger.seq.fetch_add(1, Ordering::Relaxed).saturating_add(1);
    let mind_label = match mind {
        MindKind::Main => "main",
        MindKind::Sub => "dog",
        MindKind::Memory => "memory",
    };
    let ts = now_ts();
    //（1）只记录“模型实际收到的上下文”：
    //（2）DeepSeek：直接使用 messages
    //（3）Codex：按 responses API 的 input 规范做一次归一（role/trim/空消息过滤）
    let is_codex = crate::api::normalize_provider(provider) == "codex";
    let mut codex_normalized: Vec<ApiMessage> = Vec::new();
    let messages_for_log: &[ApiMessage] = if is_codex {
        for v in crate::api::build_codex_input_messages(messages) {
            let role = v
                .get("role")
                .and_then(|x| x.as_str())
                .unwrap_or("user")
                .to_string();
            let text = v
                .get("content")
                .and_then(|x| x.as_array())
                .and_then(|a| a.first())
                .and_then(|c| c.get("text"))
                .and_then(|t| t.as_str())
                .unwrap_or("")
                .to_string();
            codex_normalized.push(ApiMessage {
                role,
                content: text,
            });
        }
        &codex_normalized
    } else {
        messages
    };
    let messages_format = if is_codex {
        "codex_input"
    } else {
        "chat_messages"
    };
    let messages_len = messages_for_log.len();
    let estimated_in_tokens = crate::estimate_messages_in_tokens(messages_for_log);

    let health = crate::context::score_context_health(provider, messages_for_log);
    runlog_event(
        "INFO",
        "context.health",
        json!({
            "mind": mind_label,
            "request_id": request_id,
            "provider": provider,
            "model": model,
            "messages_format": messages_format,
            "messages_len": messages_len,
            "estimated_in_tokens": estimated_in_tokens,
            "health_score_0_100": health.score_0_100,
            "deductions": health.deductions,
        }),
    );

    let mut buf = String::new();
    buf.push_str(
        "================================================================================\n",
    );
    let _ = writeln!(
        &mut buf,
        "[CTXTEST REQUEST] ts={ts} seq={seq} request_id={request_id} mind={mind_label}"
    );
    let _ = writeln!(
        &mut buf,
        "provider={provider} base_url={base_url} model={model} stream={stream} timeout_secs={timeout_secs}"
    );
    let _ = writeln!(
        &mut buf,
        "reasoning_effort={} temperature={} max_tokens={} messages_format={messages_format} messages_len={messages_len} estimated_in_tokens={estimated_in_tokens} health_score_0_100={}",
        reasoning_effort.unwrap_or("N/A"),
        temperature
            .map(|v| v.to_string())
            .unwrap_or_else(|| "N/A".to_string()),
        max_tokens
            .map(|v| v.to_string())
            .unwrap_or_else(|| "N/A".to_string()),
        health.score_0_100,
    );
    if !health.deductions.is_empty() {
        buf.push_str("health_deductions:\n");
        for d in &health.deductions {
            let _ = writeln!(&mut buf, "{d}");
        }
    }
    buf.push_str(
        "--------------------------------------------------------------------------------\n",
    );
    for (i, m) in messages_for_log.iter().enumerate() {
        let idx = i.saturating_add(1);
        let len = m.content.chars().count();
        let _ = writeln!(&mut buf, "--- #{idx} role={} (chars={len}) ---", m.role);
        buf.push_str(&m.content);
        if !m.content.ends_with('\n') {
            buf.push('\n');
        }
        buf.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }
    buf.push('\n');

    //（1）只保留最后一轮：请求开始就覆盖写入（便于看“模型实际收到了什么”）。
    logger.latest_started_seq.store(seq, Ordering::Relaxed);
    let key = ContextTestKey {
        mind: mind_key(mind),
        request_id,
    };
    if let Ok(mut pending) = logger.pending.lock() {
        pending.insert(
            key,
            PendingContextTest {
                seq,
                request_block: buf.clone(),
            },
        );
    }
    let mut snapshot = buf;
    snapshot.push_str(
        "================================================================================\n",
    );
    snapshot.push_str("[CTXTEST RESPONSE] (pending)\n");
    write_contexttest_snapshot(&logger.path, &snapshot);
}

pub(crate) fn contexttest_log_response_end(args: ContextTestResponseEndArgs<'_>) {
    let ContextTestResponseEndArgs {
        mind,
        request_id,
        usage_total_tokens,
        error,
        brief,
        thinking_full,
        raw_text,
        assistant_text,
    } = args;

    let Some(logger) = init_contexttest_logger() else {
        return;
    };
    let seq = logger.seq.fetch_add(1, Ordering::Relaxed).saturating_add(1);
    let mind_label = match mind {
        MindKind::Main => "main",
        MindKind::Sub => "dog",
        MindKind::Memory => "memory",
    };
    let ts = now_ts();
    let mut response = String::new();
    response.push_str(
        "================================================================================\n",
    );
    let _ = writeln!(
        &mut response,
        "[CTXTEST RESPONSE] ts={ts} seq={seq} request_id={request_id} mind={mind_label} usage_total_tokens={usage_total_tokens}"
    );
    response.push_str(
        "--------------------------------------------------------------------------------\n",
    );

    if let Some(e) = error
        && !e.trim().is_empty()
    {
        response.push_str("--- error ---\n");
        response.push_str(e);
        if !e.ends_with('\n') {
            response.push('\n');
        }
        response.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }

    if let Some(b) = brief
        && !b.trim().is_empty()
    {
        response.push_str("--- brief ---\n");
        response.push_str(b);
        if !b.ends_with('\n') {
            response.push('\n');
        }
        response.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }

    if let Some(t) = thinking_full
        && !t.trim().is_empty()
    {
        response.push_str("--- thinking_full ---\n");
        response.push_str(t);
        if !t.ends_with('\n') {
            response.push('\n');
        }
        response.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }

    if let Some(r) = raw_text
        && !r.trim().is_empty()
    {
        response.push_str("--- raw_text ---\n");
        response.push_str(r);
        if !r.ends_with('\n') {
            response.push('\n');
        }
        response.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }

    if let Some(a) = assistant_text
        && !a.trim().is_empty()
    {
        response.push_str("--- assistant_text ---\n");
        response.push_str(a);
        if !a.ends_with('\n') {
            response.push('\n');
        }
        response.push_str(
            "--------------------------------------------------------------------------------\n",
        );
    }

    response.push('\n');

    //（1）只写“最后一轮快照”：仅当该 response 对应当前最新的 request 才覆盖 contexttest
    //（2）文件。
    let key = ContextTestKey {
        mind: mind_key(mind),
        request_id,
    };
    let pending_item = logger.pending.lock().ok().and_then(|mut p| p.remove(&key));
    let Some(pending_item) = pending_item else {
        return;
    };
    let latest = logger.latest_started_seq.load(Ordering::Relaxed);
    if pending_item.seq != latest {
        return;
    }
    let mut snapshot = pending_item.request_block;
    snapshot.push_str(&response);
    write_contexttest_snapshot(&logger.path, &snapshot);
}

fn load_from_env() -> TestConfig {
    if TEST_MODE_FORCE_DISABLED {
        return TestConfig::default();
    }
    let enabled = parse_bool_env("YING_TEST").unwrap_or(false);
    let show_system_bootstrap = parse_bool_env("YING_TEST_SHOW_SYSTEM_BOOT")
        .or_else(|| parse_bool_env("YING_TEST_SHOW_SYSTEM"))
        .unwrap_or(false);
    let block_model_requests = parse_bool_env("YING_TEST_BLOCK_MODEL").unwrap_or(false);
    TestConfig {
        enabled,
        show_system_bootstrap,
        block_model_requests,
    }
}

pub(crate) fn init_from_env() -> TestConfig {
    *TEST_CONFIG.get_or_init(load_from_env)
}

pub(crate) fn config() -> TestConfig {
    *TEST_CONFIG.get_or_init(load_from_env)
}

pub(crate) fn inject_boot_system_messages(core: &mut Core, sys_log: &VecDeque<String>) -> usize {
    let cfg = config();
    if !cfg.enabled || !cfg.show_system_bootstrap || sys_log.is_empty() {
        return 0;
    }
    core.push_system("[TEST] 系统信息调试已启用：以下为启动阶段系统日志");
    for line in sys_log {
        core.push_system(&format!("[TEST][SYS] {line}"));
    }
    sys_log.len().saturating_add(1)
}

#[allow(dead_code)]
pub(crate) fn should_block_model_requests() -> bool {
    let cfg = config();
    cfg.enabled && cfg.block_model_requests
}
