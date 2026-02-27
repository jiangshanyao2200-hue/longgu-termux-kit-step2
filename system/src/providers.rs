use std::io;
use std::io::BufRead;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::time::{Duration, Instant};

use anyhow::Context;
use serde::Serialize;
use serde_json::json;

use crate::ApiMessage;
use crate::AsyncEvent;
use crate::MindKind;
use crate::api::{normalize_provider, normalize_reasoning_effort};
use crate::config::DogApiConfig;

// 约束：单次模型响应不超过 7 分钟；否则流式会卡住 UI，非流式会长时间无响应。
const MODEL_RESPONSE_TIMEOUT_CAP_SECS: u64 = 7 * 60;

#[derive(Debug, Serialize)]
struct DeepseekRequest<'a> {
    model: &'a str,
    messages: &'a [ApiMessage],
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f32>,
    stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    stream_options: Option<StreamOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
}

#[derive(Debug, Serialize)]
struct StreamOptions {
    include_usage: bool,
}

fn build_api_url(base: &str, path: &str) -> String {
    let base = base.trim_end_matches('/');
    let path = path.trim_start_matches('/');
    format!("{base}/{path}")
}

#[derive(Debug, Clone)]
pub(crate) struct DogClient {
    http: reqwest::blocking::Client,
    cfg: DogApiConfig,
}

impl DogClient {
    pub(crate) fn new(cfg: DogApiConfig) -> anyhow::Result<Self> {
        let key = cfg.api_key.as_deref().unwrap_or("").trim().to_string();
        if key.is_empty() {
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        let http = reqwest::blocking::Client::builder()
            .build()
            .context("创建 HTTP 客户端失败")?;
        Ok(Self { http, cfg })
    }

    fn request_timeout_secs(&self, stream: bool) -> u64 {
        let cap = MODEL_RESPONSE_TIMEOUT_CAP_SECS;
        let base = self.cfg.timeout_secs.max(30).min(cap);
        if stream {
            base.max(120).min(cap)
        } else {
            base.max(120).min(cap)
        }
    }

    fn post_deepseek(
        &self,
        req: &DeepseekRequest<'_>,
        key: &str,
        timeout_secs: u64,
    ) -> anyhow::Result<reqwest::blocking::Response> {
        let url = build_api_url(&self.cfg.base_url, "chat/completions");
        self.http
            .post(&url)
            .bearer_auth(key)
            .timeout(Duration::from_secs(timeout_secs))
            .json(req)
            .send()
            .context("请求 DeepSeek 失败")
            .and_then(|r| {
                let status = r.status();
                if !status.is_success() {
                    let body = r.text().unwrap_or_default();
                    let body = body.trim();
                    if body.is_empty() {
                        return Err(anyhow::anyhow!("DeepSeek 返回异常状态: {status}"));
                    }
                    return Err(anyhow::anyhow!(
                        "DeepSeek 返回异常状态: {status} | {}",
                        body
                    ));
                }
                Ok(r)
            })
    }

    fn post_codex_responses(
        &self,
        req: &serde_json::Value,
        key: &str,
        timeout_secs: u64,
    ) -> anyhow::Result<reqwest::blocking::Response> {
        let url = build_api_url(&self.cfg.base_url, "responses");
        self.http
            .post(&url)
            .bearer_auth(key)
            .header("Accept", "text/event-stream")
            .timeout(Duration::from_secs(timeout_secs))
            .json(req)
            .send()
            .context("请求 Codex 失败")
            .and_then(|r| {
                let status = r.status();
                if !status.is_success() {
                    let body = r.text().unwrap_or_default();
                    let body = body.trim();
                    if body.is_empty() {
                        return Err(anyhow::anyhow!("Codex 返回异常状态: {status}"));
                    }
                    return Err(anyhow::anyhow!("Codex 返回异常状态: {status} | {}", body));
                }
                Ok(r)
            })
    }

    pub(crate) fn provider(&self) -> &str {
        self.cfg.provider.as_str()
    }

    pub(crate) fn is_codex_provider(&self) -> bool {
        normalize_provider(&self.cfg.provider) == "codex"
    }

    fn send_codex_stream(
        &self,
        messages: Vec<ApiMessage>,
        tx: mpsc::Sender<AsyncEvent>,
        kind: MindKind,
        request_id: u64,
        cancel: Arc<AtomicBool>,
    ) -> anyhow::Result<()> {
        let key = self.cfg.api_key.as_deref().unwrap_or("").trim();
        if key.is_empty() {
            crate::test::runlog_event(
                "ERROR",
                "model.request.aborted",
                json!({"mind": crate::mind_label(kind), "request_id": request_id, "reason": "missing_api_key"}),
            );
            return Err(anyhow::anyhow!("API Key 为空"));
        }

        let mut req = json!({
            "model": &self.cfg.model,
            "input": crate::api::build_codex_input_messages(&messages),
            "stream": true,
        });
        if let Some(effort) = normalize_reasoning_effort(self.cfg.reasoning_effort.as_deref()) {
            req["reasoning"] = json!({ "effort": effort, "summary": "detailed" });
        } else {
            req["reasoning"] = json!({ "summary": "detailed" });
        }

        crate::test::contexttest_log_request_start(
            kind,
            request_id,
            true,
            &self.cfg.provider,
            &self.cfg.base_url,
            &self.cfg.model,
            self.cfg.reasoning_effort.as_deref(),
            None,
            None,
            self.request_timeout_secs(true),
            &messages,
        );

        let max_retries = 3usize;
        let mut retries_done = 0usize;
        let mut started = false;
        let mut last_err: Option<anyhow::Error> = None;

        loop {
            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            let resp = match self.post_codex_responses(&req, key, self.request_timeout_secs(true)) {
                Ok(r) => r,
                Err(e) => {
                    if Self::is_non_retryable_request_error(&e) {
                        crate::test::runlog_event(
                            "ERROR",
                            "model.request.error",
                            json!({
                                "mind": crate::mind_label(kind),
                                "request_id": request_id,
                                "attempt": retries_done.saturating_add(1),
                                "error": format!("{e:#}"),
                                "retryable": false,
                            }),
                        );
                        let _ = last_err.replace(e);
                        break;
                    }
                    crate::test::runlog_event(
                        "WARN",
                        "model.request.retryable_error",
                        json!({
                            "mind": crate::mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            if !started {
                let _ = tx.send(AsyncEvent::ModelStreamStart {
                    kind,
                    expect_brief: true,
                    request_id,
                });
                started = true;
            }

            let mut resp = resp;
            let mut reader = io::BufReader::new(&mut resp);
            let mut line = String::new();
            let mut usage = 0u64;
            let mut emitted_any = false;
            let mut brief_seen_any = false;
            let mut brief_pending_sep = false;
            let mut brief_last_index: Option<i64> = None;
            let mut reasoning_seen_any = false;
            let mut reasoning_last_index: Option<i64> = None;
            let mut text_seen_any = false;
            let mut text_buf = String::new();
            let mut last_text_flush = Instant::now();
            let stream_result: anyhow::Result<()> = (|| {
                loop {
                    if cancel.load(Ordering::SeqCst) {
                        return Ok(());
                    }
                    line.clear();
                    let bytes = reader
                        .read_line(&mut line)
                        .context("读取 Codex 流式响应失败")?;
                    if bytes == 0 {
                        break;
                    }
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    if !trimmed.starts_with("data:") {
                        continue;
                    }
                    let payload = trimmed.trim_start_matches("data:").trim();
                    if payload == "[DONE]" {
                        break;
                    }
                    let value: serde_json::Value = match serde_json::from_str(payload) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    match value.get("type").and_then(|v| v.as_str()).unwrap_or("") {
                        "response.reasoning_summary_part.added" => {
                            // 某些实现会按 part 分段输出 summary；这里用分隔换行保持段落边界。
                            if brief_seen_any {
                                brief_pending_sep = true;
                            }
                        }
                        "response.output_text.delta" => {
                            let content = value
                                .get("delta")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if crate::test::runtime_log_stream_chunks() && !content.is_empty() {
                                crate::test::runlog_event(
                                    "DEBUG",
                                    "model.stream.chunk",
                                    json!({
                                        "mind": crate::mind_label(kind),
                                        "request_id": request_id,
                                        "content": &content,
                                        "reasoning": "",
                                        "brief": "",
                                    }),
                                );
                            }
                            if !content.is_empty() {
                                text_seen_any = true;
                                text_buf.push_str(&content);
                                // Codex 有时会“真流式分段”，也有时会一次性吐大量 delta；
                                // 这里做轻量 coalesce，既能早显示，也避免 UI/渲染被海量事件打爆。
                                let should_flush = text_buf.len() >= 1024
                                    || last_text_flush.elapsed() >= Duration::from_millis(120);
                                if should_flush && !text_buf.is_empty() {
                                    let content = std::mem::take(&mut text_buf);
                                    let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                        content,
                                        reasoning: String::new(),
                                        brief: String::new(),
                                        request_id,
                                    });
                                    emitted_any = true;
                                    last_text_flush = Instant::now();
                                }
                            }
                        }
                        "response.output_text.done" => {
                            let content = value
                                .get("text")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if !content.is_empty() && !text_seen_any && text_buf.trim().is_empty() {
                                text_buf.push_str(&content);
                            }
                        }
                        "response.reasoning.delta" | "response.reasoning_text.delta" => {
                            let idx = value.get("content_index").and_then(|v| v.as_i64());
                            let reasoning = value
                                .get("delta")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if crate::test::runtime_log_stream_chunks() && !reasoning.is_empty() {
                                crate::test::runlog_event(
                                    "DEBUG",
                                    "model.stream.chunk",
                                    json!({
                                        "mind": crate::mind_label(kind),
                                        "request_id": request_id,
                                        "content": "",
                                        "reasoning": &reasoning,
                                        "brief": "",
                                    }),
                                );
                            }
                            if !reasoning.is_empty() {
                                let need_sep = idx.is_some()
                                    && reasoning_last_index.is_some()
                                    && idx != reasoning_last_index;
                                if need_sep && reasoning_seen_any {
                                    let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                        content: String::new(),
                                        reasoning: "\n\n".to_string(),
                                        brief: String::new(),
                                        request_id,
                                    });
                                    emitted_any = true;
                                }
                                if idx.is_some() {
                                    reasoning_last_index = idx;
                                }
                                reasoning_seen_any = true;
                                emitted_any = true;
                                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                    content: String::new(),
                                    reasoning,
                                    brief: String::new(),
                                    request_id,
                                });
                            }
                        }
                        "response.reasoning.done" | "response.reasoning_text.done" => {
                            let reasoning = value
                                .get("text")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if !reasoning.is_empty() && !reasoning_seen_any {
                                reasoning_seen_any = true;
                                emitted_any = true;
                                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                    content: String::new(),
                                    reasoning,
                                    brief: String::new(),
                                    request_id,
                                });
                            }
                        }
                        "response.summary.delta"
                        | "response.reasoning_summary.delta"
                        | "response.reasoning_summary_text.delta" => {
                            let idx = value
                                .get("summary_index")
                                .or_else(|| value.get("content_index"))
                                .and_then(|v| v.as_i64());
                            let brief = value
                                .get("delta")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if crate::test::runtime_log_stream_chunks() && !brief.is_empty() {
                                crate::test::runlog_event(
                                    "DEBUG",
                                    "model.stream.chunk",
                                    json!({
                                        "mind": crate::mind_label(kind),
                                        "request_id": request_id,
                                        "content": "",
                                        "reasoning": "",
                                        "brief": &brief,
                                    }),
                                );
                            }
                            if !brief.is_empty() {
                                let need_sep = brief_pending_sep
                                    || (idx.is_some()
                                        && brief_last_index.is_some()
                                        && idx != brief_last_index);
                                if need_sep && brief_seen_any {
                                    let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                        content: String::new(),
                                        reasoning: String::new(),
                                        brief: "\n\n".to_string(),
                                        request_id,
                                    });
                                    emitted_any = true;
                                }
                                brief_pending_sep = false;
                                if idx.is_some() {
                                    brief_last_index = idx;
                                }
                                brief_seen_any = true;
                                emitted_any = true;
                                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                    content: String::new(),
                                    reasoning: String::new(),
                                    brief,
                                    request_id,
                                });
                            }
                        }
                        "response.summary.done"
                        | "response.reasoning_summary.done"
                        | "response.reasoning_summary_text.done" => {
                            let brief = value
                                .get("text")
                                .and_then(|v| v.as_str())
                                .unwrap_or("")
                                .to_string();
                            if !brief.is_empty() && !brief_seen_any {
                                brief_seen_any = true;
                                emitted_any = true;
                                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                    content: String::new(),
                                    reasoning: String::new(),
                                    brief,
                                    request_id,
                                });
                            }
                        }
                        "response.completed" => {
                            if let Some(total) = value
                                .get("response")
                                .and_then(|v| v.get("usage"))
                                .and_then(|u| u.get("total_tokens"))
                                .and_then(|v| v.as_u64())
                            {
                                usage = total;
                            }
                        }
                        "response.error" => {
                            let msg = value
                                .get("error")
                                .and_then(|e| e.get("message"))
                                .and_then(|v| v.as_str())
                                .unwrap_or("Codex 返回错误事件")
                                .to_string();
                            return Err(anyhow::anyhow!(msg));
                        }
                        _ => {}
                    }
                }
                Ok(())
            })();

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            let err_str = stream_result.as_ref().err().map(|e| format!("{e:#}"));

            if stream_result.is_ok() && !text_buf.is_empty() && !cancel.load(Ordering::SeqCst) {
                let content = std::mem::take(&mut text_buf);
                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                    content,
                    reasoning: String::new(),
                    brief: String::new(),
                    request_id,
                });
                emitted_any = true;
            }

            if !cancel.load(Ordering::SeqCst) {
                let _ = tx.send(AsyncEvent::ModelStreamEnd {
                    kind,
                    usage,
                    error: err_str.clone(),
                    request_id,
                });
            }

            if let Err(e) = stream_result {
                crate::test::runlog_event(
                    "ERROR",
                    "model.stream.error",
                    json!({
                        "mind": crate::mind_label(kind),
                        "request_id": request_id,
                        "attempt": retries_done.saturating_add(1),
                        "error": format!("{e:#}"),
                        "emitted_any_chunk": emitted_any,
                    }),
                );
                let _ = last_err.replace(e);
                if !emitted_any && retries_done < max_retries {
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
                break;
            }
            return Ok(());
        }

        if cancel.load(Ordering::SeqCst) {
            return Ok(());
        }
        let msg = last_err
            .map(|e| format!("{e:#}"))
            .unwrap_or_else(|| "Codex 请求失败".to_string());
        let _ = tx.send(AsyncEvent::ModelStreamEnd {
            kind,
            usage: 0,
            error: Some(msg),
            request_id,
        });
        Ok(())
    }

    fn is_non_retryable_request_error(err: &anyhow::Error) -> bool {
        let s = format!("{err:#}").to_ascii_lowercase();
        s.contains("invalid_request_error")
            || s.contains("400 bad request")
            || s.contains("invalid max_tokens value")
    }

    pub(crate) fn send_chat_stream(
        &self,
        messages: Vec<ApiMessage>,
        tx: mpsc::Sender<AsyncEvent>,
        kind: MindKind,
        request_id: u64,
        cancel: Arc<AtomicBool>,
    ) -> anyhow::Result<()> {
        let mut messages = messages;
        // DeepSeek（chat/completions）某些实现要求最后一条必须是 user。
        // 工具回执使用 system（避免模型误判为“用户输入”），因此只要最后一条不是 user，
        // 就补一个“零指令语义”的 user 占位，保证协议兼容。
        //
        // 注意：消息内容可能包含历史遗留的时间戳/前缀，因此不要用 `starts_with(...)` 做语义判断。
        if normalize_provider(&self.cfg.provider) == "deepseek"
            && !messages
                .last()
                .is_some_and(|m| m.role.trim().eq_ignore_ascii_case("user"))
        {
            let code = crate::context::extract_last_mcp_tool_code(&messages);
            messages.push(ApiMessage {
                role: "user".to_string(),
                content: crate::context::with_stamp_footer(
                    &crate::context::role_needed_user_placeholder_to(code.as_deref()),
                ),
            });
        }
        let contamination = crate::context::detect_context_contamination(&messages);
        if !contamination.is_empty() {
            crate::test::runlog_event(
                "WARN",
                "context.contamination",
                json!({
                    "mind": crate::mind_label(kind),
                    "request_id": request_id,
                    "provider": &self.cfg.provider,
                    "base_url": &self.cfg.base_url,
                    "model": &self.cfg.model,
                    "findings": contamination,
                }),
            );
        }
        crate::trace_startup("run_loop: runlog_event");
        crate::test::runlog_event(
            "INFO",
            "model.request.start",
            json!({
                "mind": crate::mind_label(kind),
                "request_id": request_id,
                "stream": true,
                "provider": &self.cfg.provider,
                "base_url": &self.cfg.base_url,
                "model": &self.cfg.model,
                "reasoning_effort": self.cfg.reasoning_effort.as_deref(),
                "temperature": self.cfg.temperature,
                "max_tokens": self.cfg.max_tokens,
                "timeout_secs": self.request_timeout_secs(true),
                "messages_len": messages.len(),
                "estimated_in_tokens": crate::estimate_messages_in_tokens(&messages),
                "messages": &messages,
            }),
        );
        let key = self.cfg.api_key.as_deref().unwrap_or("").trim();
        if key.is_empty() {
            crate::test::runlog_event(
                "ERROR",
                "model.request.aborted",
                json!({"mind": crate::mind_label(kind), "request_id": request_id, "reason": "missing_api_key"}),
            );
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        if self.is_codex_provider() {
            return self.send_codex_stream(messages, tx, kind, request_id, cancel);
        }
        let req = DeepseekRequest {
            model: &self.cfg.model,
            messages: &messages,
            temperature: self.cfg.temperature,
            stream: true,
            stream_options: Some(StreamOptions {
                include_usage: true,
            }),
            max_tokens: self.cfg.max_tokens,
        };
        crate::test::contexttest_log_request_start(
            kind,
            request_id,
            true,
            &self.cfg.provider,
            &self.cfg.base_url,
            &self.cfg.model,
            self.cfg.reasoning_effort.as_deref(),
            self.cfg.temperature,
            self.cfg.max_tokens,
            self.request_timeout_secs(true),
            &messages,
        );
        let max_retries = 3usize;
        let mut retries_done = 0usize;
        let mut started = false;
        let mut last_err: Option<anyhow::Error> = None;

        loop {
            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            let resp = match self.post_deepseek(&req, key, self.request_timeout_secs(true)) {
                Ok(r) => r,
                Err(e) => {
                    if Self::is_non_retryable_request_error(&e) {
                        crate::test::runlog_event(
                            "ERROR",
                            "model.request.error",
                            json!({
                                "mind": crate::mind_label(kind),
                                "request_id": request_id,
                                "attempt": retries_done.saturating_add(1),
                                "error": format!("{e:#}"),
                                "retryable": false,
                            }),
                        );
                        let _ = last_err.replace(e);
                        break;
                    }
                    crate::test::runlog_event(
                        "WARN",
                        "model.request.retryable_error",
                        json!({
                            "mind": crate::mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            if !started {
                let _ = tx.send(AsyncEvent::ModelStreamStart {
                    kind,
                    expect_brief: false,
                    request_id,
                });
                started = true;
            }

            let mut resp = resp;
            let mut reader = io::BufReader::new(&mut resp);
            let mut line = String::new();
            let mut usage = 0u64;
            let mut emitted = false;
            let stream_result: anyhow::Result<()> = (|| {
                loop {
                    if cancel.load(Ordering::SeqCst) {
                        return Ok(());
                    }
                    line.clear();
                    let bytes = reader.read_line(&mut line).context("读取流式响应失败")?;
                    if bytes == 0 {
                        break;
                    }
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    if !trimmed.starts_with("data:") {
                        continue;
                    }
                    let payload = trimmed.trim_start_matches("data:").trim();
                    if payload == "[DONE]" {
                        break;
                    }
                    let value: serde_json::Value = match serde_json::from_str(payload) {
                        Ok(v) => v,
                        Err(_) => continue,
                    };
                    if let Some(total) = value
                        .get("usage")
                        .and_then(|u| u.get("total_tokens"))
                        .and_then(|v| v.as_u64())
                    {
                        usage = total;
                    }
                    if let Some(delta) = value
                        .get("choices")
                        .and_then(|c| c.get(0))
                        .and_then(|c| c.get("delta"))
                    {
                        let content = delta
                            .get("content")
                            .and_then(|v| v.as_str())
                            .unwrap_or("")
                            .to_string();
                        let reasoning = delta
                            .get("reasoning_content")
                            .and_then(|v| v.as_str())
                            .unwrap_or("")
                            .to_string();
                        if crate::test::runtime_log_stream_chunks()
                            && (!content.is_empty() || !reasoning.is_empty())
                        {
                            crate::test::runlog_event(
                                "DEBUG",
                                "model.stream.chunk",
                                json!({
                                    "mind": crate::mind_label(kind),
                                    "request_id": request_id,
                                    "content": &content,
                                    "reasoning": &reasoning,
                                }),
                            );
                        }
                        if !content.is_empty() || !reasoning.is_empty() {
                            if cancel.load(Ordering::SeqCst) {
                                return Ok(());
                            }
                            emitted = true;
                            let _ = tx.send(AsyncEvent::ModelStreamChunk {
                                content,
                                reasoning,
                                brief: String::new(),
                                request_id,
                            });
                        }
                    }
                }
                Ok(())
            })();

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }

            if !cancel.load(Ordering::SeqCst) {
                let _ = tx.send(AsyncEvent::ModelStreamEnd {
                    kind,
                    usage,
                    error: stream_result.as_ref().err().map(|e| format!("{e:#}")),
                    request_id,
                });
            }

            if let Err(e) = stream_result {
                crate::test::runlog_event(
                    "ERROR",
                    "model.stream.error",
                    json!({
                        "mind": crate::mind_label(kind),
                        "request_id": request_id,
                        "attempt": retries_done.saturating_add(1),
                        "error": format!("{e:#}"),
                        "emitted_any_chunk": emitted,
                    }),
                );
                let _ = last_err.replace(e);
                if !emitted && retries_done < max_retries {
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
                break;
            }
            return Ok(());
        }

        if cancel.load(Ordering::SeqCst) {
            return Ok(());
        }
        let msg = last_err
            .map(|e| format!("{e:#}"))
            .unwrap_or_else(|| "DeepSeek 请求失败".to_string());
        let _ = tx.send(AsyncEvent::ModelStreamEnd {
            kind,
            usage: 0,
            error: Some(msg),
            request_id,
        });
        Ok(())
    }

    pub(crate) fn send_chat(
        &self,
        messages: Vec<ApiMessage>,
        tx: mpsc::Sender<AsyncEvent>,
        kind: MindKind,
        request_id: u64,
        cancel: Arc<AtomicBool>,
    ) -> anyhow::Result<()> {
        let mut messages = messages;
        // DeepSeek（chat/completions）某些实现要求最后一条必须是 user。
        // 工具回执使用 system（避免模型误判为“用户输入”），因此只要最后一条不是 user，
        // 就补一个“零指令语义”的 user 占位，保证协议兼容。
        if normalize_provider(&self.cfg.provider) == "deepseek"
            && !messages
                .last()
                .is_some_and(|m| m.role.trim().eq_ignore_ascii_case("user"))
        {
            let code = crate::context::extract_last_mcp_tool_code(&messages);
            messages.push(ApiMessage {
                role: "user".to_string(),
                content: crate::context::with_stamp_footer(
                    &crate::context::role_needed_user_placeholder_to(code.as_deref()),
                ),
            });
        }
        let contamination = crate::context::detect_context_contamination(&messages);
        if !contamination.is_empty() {
            crate::test::runlog_event(
                "WARN",
                "context.contamination",
                json!({
                    "mind": crate::mind_label(kind),
                    "request_id": request_id,
                    "provider": &self.cfg.provider,
                    "base_url": &self.cfg.base_url,
                    "model": &self.cfg.model,
                    "findings": contamination,
                }),
            );
        }
        crate::test::runlog_event(
            "INFO",
            "model.request.start",
            json!({
                "mind": crate::mind_label(kind),
                "request_id": request_id,
                "stream": false,
                "provider": &self.cfg.provider,
                "base_url": &self.cfg.base_url,
                "model": &self.cfg.model,
                "reasoning_effort": self.cfg.reasoning_effort.as_deref(),
                "temperature": self.cfg.temperature,
                "max_tokens": self.cfg.max_tokens,
                "timeout_secs": self.request_timeout_secs(false),
                "messages_len": messages.len(),
                "estimated_in_tokens": crate::estimate_messages_in_tokens(&messages),
                "messages": &messages,
            }),
        );
        let key = self.cfg.api_key.as_deref().unwrap_or("").trim();
        if key.is_empty() {
            crate::test::runlog_event(
                "ERROR",
                "model.request.aborted",
                json!({"mind": crate::mind_label(kind), "request_id": request_id, "reason": "missing_api_key"}),
            );
            return Err(anyhow::anyhow!("API Key 为空"));
        }
        if self.is_codex_provider() {
            // Codex responses 端点要求 stream=true；非流式模式回落到同一套流式处理。
            return self.send_chat_stream(messages, tx, kind, request_id, cancel);
        }
        let req = DeepseekRequest {
            model: &self.cfg.model,
            messages: &messages,
            temperature: self.cfg.temperature,
            stream: false,
            stream_options: None,
            max_tokens: self.cfg.max_tokens,
        };
        crate::test::contexttest_log_request_start(
            kind,
            request_id,
            false,
            &self.cfg.provider,
            &self.cfg.base_url,
            &self.cfg.model,
            self.cfg.reasoning_effort.as_deref(),
            self.cfg.temperature,
            self.cfg.max_tokens,
            self.request_timeout_secs(false),
            &messages,
        );

        let max_retries = 3usize;
        let mut retries_done = 0usize;
        let mut last_err: Option<anyhow::Error> = None;

        loop {
            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            let resp = match self.post_deepseek(&req, key, self.request_timeout_secs(false)) {
                Ok(r) => r,
                Err(e) => {
                    if Self::is_non_retryable_request_error(&e) {
                        crate::test::runlog_event(
                            "ERROR",
                            "model.request.error",
                            json!({
                                "mind": crate::mind_label(kind),
                                "request_id": request_id,
                                "attempt": retries_done.saturating_add(1),
                                "error": format!("{e:#}"),
                                "retryable": false,
                            }),
                        );
                        let _ = last_err.replace(e);
                        break;
                    }
                    crate::test::runlog_event(
                        "WARN",
                        "model.request.retryable_error",
                        json!({
                            "mind": crate::mind_label(kind),
                            "request_id": request_id,
                            "attempt": retries_done.saturating_add(1),
                            "max_retries": max_retries,
                            "error": format!("{e:#}"),
                        }),
                    );
                    let _ = last_err.replace(e);
                    if retries_done >= max_retries {
                        break;
                    }
                    retries_done = retries_done.saturating_add(1);
                    let _ = tx.send(AsyncEvent::ErrorRetry {
                        attempt: retries_done,
                        max: max_retries,
                        request_id,
                    });
                    std::thread::sleep(Duration::from_millis(240 * retries_done as u64));
                    continue;
                }
            };

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            let resp = resp;
            let value: serde_json::Value = match resp.json().context("解析 DeepSeek 响应失败")
            {
                Ok(v) => v,
                Err(e) => {
                    let _ = last_err.replace(e);
                    break;
                }
            };
            let usage = value
                .get("usage")
                .and_then(|u| u.get("total_tokens"))
                .and_then(|v| v.as_u64())
                .unwrap_or(0);
            let message = value
                .get("choices")
                .and_then(|c| c.get(0))
                .and_then(|c| c.get("message"))
                .cloned()
                .unwrap_or(serde_json::Value::Null);
            let content = message
                .get("content")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            let reasoning = message
                .get("reasoning_content")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();

            crate::test::runlog_event(
                "INFO",
                "model.response.json",
                json!({
                    "mind": crate::mind_label(kind),
                    "request_id": request_id,
                    "usage_total_tokens": usage,
                    "raw": value,
                    "parsed": { "content": &content, "reasoning": &reasoning },
                }),
            );

            if cancel.load(Ordering::SeqCst) {
                return Ok(());
            }
            let _ = tx.send(AsyncEvent::ModelStreamStart {
                kind,
                expect_brief: false,
                request_id,
            });
            if !reasoning.is_empty() {
                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                    content: String::new(),
                    reasoning,
                    brief: String::new(),
                    request_id,
                });
                std::thread::sleep(Duration::from_millis(120));
            }
            if !content.is_empty() {
                let _ = tx.send(AsyncEvent::ModelStreamChunk {
                    content,
                    reasoning: String::new(),
                    brief: String::new(),
                    request_id,
                });
            }
            if !cancel.load(Ordering::SeqCst) {
                let _ = tx.send(AsyncEvent::ModelStreamEnd {
                    kind,
                    usage,
                    error: None,
                    request_id,
                });
            }
            return Ok(());
        }

        if cancel.load(Ordering::SeqCst) {
            return Ok(());
        }
        let msg = last_err
            .map(|e| format!("{e:#}"))
            .unwrap_or_else(|| "DeepSeek 请求失败".to_string());
        let _ = tx.send(AsyncEvent::ModelStreamEnd {
            kind,
            usage: 0,
            error: Some(msg),
            request_id,
        });
        Ok(())
    }
}
