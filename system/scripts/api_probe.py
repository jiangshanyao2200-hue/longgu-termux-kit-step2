#!/usr/bin/env python3
import argparse
import json
import sys
import time
from pathlib import Path
from urllib.request import Request, urlopen
from urllib.error import HTTPError, URLError


def load_json(path: Path):
    return json.loads(path.read_text(encoding="utf-8"))


def merged_profile(cfg: dict, provider: str) -> dict:
    profiles = cfg.get("provider_profiles") or {}
    p = profiles.get(provider) or {}
    out = dict(cfg)
    out.update(p)
    out["provider"] = provider
    return out


def build_messages_case(case: str, text: str) -> list:
    sys_msg = {"role": "system", "content": "You are a test harness. Reply briefly."}
    user_msg = {"role": "user", "content": text}

    if case == "basic":
        return [sys_msg, user_msg]
    if case == "system_only":
        return [sys_msg]
    if case == "assistant_last":
        return [sys_msg, {"role": "assistant", "content": text}]
    if case == "double_assistant":
        return [sys_msg, {"role": "assistant", "content": "a1"}, {"role": "assistant", "content": "a2"}]
    if case == "system_then_tool":
        tool_like = "[MCP:tool] ok" if not text.strip() else text
        return [sys_msg, {"role": "system", "content": tool_like}]
    if case == "tool_role":
        return [
            sys_msg,
            user_msg,
            {"role": "assistant", "content": "calling tool"},
            {"role": "tool", "tool_call_id": "call_1", "content": "{\"ok\":true}"},
        ]
    raise ValueError(f"unknown case: {case}")


def build_deepseek_body(cfg: dict, stream: bool, messages: list) -> dict:
    body = {"model": cfg["model"], "messages": messages, "stream": bool(stream)}
    if cfg.get("temperature") is not None:
        body["temperature"] = cfg["temperature"]
    if cfg.get("max_tokens") is not None:
        body["max_tokens"] = cfg["max_tokens"]
    return body


def build_codex_body(cfg: dict, stream: bool, user_text: str) -> dict:
    body = {
        "model": cfg["model"],
        "input": [
            {
                "role": "system",
                "content": [{"type": "input_text", "text": "You are a test harness. Reply briefly."}],
            },
            {"role": "user", "content": [{"type": "input_text", "text": user_text}]},
        ],
        "stream": bool(stream),
        "reasoning": {"summary": "detailed"},
    }
    effort = (cfg.get("reasoning_effort") or "").strip().lower()
    if effort in ("low", "medium", "high", "xhigh"):
        body["reasoning"]["effort"] = effort
    return body


def post_json(url: str, api_key: str, body: dict, accept: str, timeout: float):
    data = json.dumps(body).encode("utf-8")
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
        "Accept": accept,
    }
    req = Request(url, data=data, headers=headers, method="POST")
    t0 = time.time()
    resp = urlopen(req, timeout=timeout)
    t1 = time.time()
    return resp, t0, t1


def _deepseek_sse_extract(v: dict):
    usage_total = None
    try:
        usage_total = v.get("usage", {}).get("total_tokens")
    except Exception:
        usage_total = None

    delta = None
    try:
        delta = v.get("choices", [None])[0].get("delta")
    except Exception:
        delta = None

    content = ""
    reasoning = ""
    finish_reason = None

    if isinstance(delta, dict):
        content = delta.get("content") or ""
        reasoning = delta.get("reasoning_content") or ""

    try:
        finish_reason = v.get("choices", [None])[0].get("finish_reason")
    except Exception:
        finish_reason = None

    return usage_total, content, reasoning, finish_reason


def run_stream(provider: str, url: str, api_key: str, body: dict, timeout: float, max_events: int):
    resp, t0, t1 = post_json(url, api_key, body, "text/event-stream", timeout)
    ct = resp.headers.get("Content-Type", "")
    print(f"HTTP 200 content_type={ct} ttfb_ms={int((t1 - t0) * 1000)}")

    shown = 0
    lines = 0
    t_first_data = None
    usage_total = None

    try:
        for raw in resp:
            lines += 1
            try:
                line = raw.decode("utf-8", errors="replace").strip()
            except Exception:
                continue
            if not line or not line.startswith("data:"):
                continue
            payload = line[5:].strip()
            if payload == "[DONE]":
                print("DONE")
                break

            if t_first_data is None:
                t_first_data = time.time()

            try:
                v = json.loads(payload)
            except Exception:
                if shown < max_events:
                    print(f"EVENT(nonjson): {payload[:200]}")
                    shown += 1
                continue

            if provider == "deepseek":
                ut, content, reasoning, fin = _deepseek_sse_extract(v)
                if ut is not None:
                    usage_total = ut
                if shown < max_events:
                    msg = ""
                    if isinstance(reasoning, str) and reasoning.strip():
                        rr = reasoning.strip().replace("\n", "\\n")
                        msg += f" reasoning={rr[:160]}"
                    if isinstance(content, str) and content.strip():
                        cc = content.strip().replace("\n", "\\n")
                        msg += f" content={cc[:160]}"
                    if fin:
                        msg += f" finish={fin}"
                    if msg:
                        print(f"EVENT{msg}")
                        shown += 1
            else:
                if shown < max_events:
                    print("EVENT keys=" + ",".join(sorted(list(v.keys()))))
                    shown += 1
    finally:
        resp.close()

    t2 = time.time()
    t_first = int(((t_first_data or t2) - t0) * 1000)
    print(
        "summary: "
        + " ".join(
            [
                f"lines={lines}",
                f"first_data_ms={t_first}",
                f"total_ms={int((t2 - t0) * 1000)}",
                f"usage_total={usage_total if usage_total is not None else N/A}",
            ]
        )
    )


def run_nonstream(provider: str, url: str, api_key: str, body: dict, timeout: float, show_text: bool):
    resp, t0, t1 = post_json(url, api_key, body, "application/json", timeout)
    ct = resp.headers.get("Content-Type", "")
    data = resp.read()
    resp.close()
    t2 = time.time()
    print(
        f"HTTP 200 content_type={ct} ttfb_ms={int((t1 - t0) * 1000)} total_ms={int((t2 - t0) * 1000)} bytes={len(data)}"
    )
    try:
        v = json.loads(data.decode("utf-8", errors="replace"))
    except Exception:
        print(data[:800].decode("utf-8", errors="replace"))
        return

    print("json_keys:", ",".join(sorted(list(v.keys()))))

    if show_text and provider == "deepseek":
        try:
            msg = v.get("choices", [None])[0].get("message")
            content = (msg.get("content") or "") if isinstance(msg, dict) else ""
            reasoning = (msg.get("reasoning_content") or "") if isinstance(msg, dict) else ""
            fin = v.get("choices", [None])[0].get("finish_reason")
            if reasoning.strip():
                print("reasoning:", reasoning.strip()[:400].replace("\n", "\\n"))
            if content.strip():
                print("content:", content.strip()[:400].replace("\n", "\\n"))
            if fin:
                print("finish_reason:", fin)
        except Exception:
            pass


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", choices=["main", "dog", "memory"], default="main")
    ap.add_argument("--provider", choices=["codex", "deepseek"], default=None)
    ap.add_argument("--stream", action="store_true")
    ap.add_argument("--no-stream", dest="stream", action="store_false")
    ap.set_defaults(stream=True)
    ap.add_argument("--timeout", type=float, default=None)
    ap.add_argument("--max-events", type=int, default=30)
    ap.add_argument("--text", default="Reply with exactly: ok")
    ap.add_argument(
        "--case",
        choices=["basic", "system_only", "assistant_last", "double_assistant", "system_then_tool", "tool_role"],
        default="basic",
        help="deepseek-only: probe role/sequence constraints",
    )
    ap.add_argument("--append-user-tail", action="store_true")
    ap.add_argument("--show-body", action="store_true")
    ap.add_argument("--show-text", action="store_true")
    ap.add_argument("--model", default=None)
    ap.add_argument("--effort", choices=["low", "medium", "high", "xhigh"], default=None)
    args = ap.parse_args()

    root = Path(__file__).resolve().parents[1]
    cfg_path = root / "config" / ("mainAPI.json" if args.profile == "main" else ("dogAPI.json" if args.profile == "dog" else "memoryAPI.json"))
    cfg0 = load_json(cfg_path)
    provider = args.provider or (cfg0.get("provider") or "deepseek")
    cfg = merged_profile(cfg0, provider)
    if args.model:
        cfg["model"] = args.model
    if args.effort:
        cfg["reasoning_effort"] = args.effort

    base_url = (cfg.get("base_url") or "").rstrip("/")
    api_key = (cfg.get("api_key") or "").strip()
    if not base_url or not api_key:
        print("missing base_url/api_key in config", file=sys.stderr)
        return 2

    timeout = float(args.timeout if args.timeout is not None else (cfg.get("timeout_secs") or 60))

    if provider == "codex":
        url = f"{base_url}/responses"
        body = build_codex_body(cfg, args.stream, args.text)
    else:
        url = f"{base_url}/chat/completions"
        messages = build_messages_case(args.case, args.text)
        if args.append_user_tail and not (messages and messages[-1].get("role") == "user"):
            messages.append({"role": "user", "content": "(compat)"})
        body = build_deepseek_body(cfg, args.stream, messages)

    if args.show_body:
        print(json.dumps({"url": url, "body": body}, ensure_ascii=True, indent=2)[:12000])

    try:
        if args.stream:
            run_stream(provider, url, api_key, body, timeout, args.max_events)
        else:
            run_nonstream(provider, url, api_key, body, timeout, args.show_text)
    except HTTPError as e:
        data = e.read() if hasattr(e, "read") else b""
        msg = data.decode("utf-8", errors="replace").strip()
        print(f"HTTP {getattr(e, 'code', 'ERR')} error_body={msg[:1200]}")
        return 1
    except URLError as e:
        print(f"URL error: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
