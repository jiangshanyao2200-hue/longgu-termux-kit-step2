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


def build_deepseek_body(cfg: dict, stream: bool, user_text: str) -> dict:
    body = {
        "model": cfg["model"],
        "messages": [
            {"role": "system", "content": "You are a test harness. Reply briefly."},
            {"role": "user", "content": user_text},
        ],
        "stream": bool(stream),
    }
    # DeepSeek supports temperature/max_tokens (in this codebase); keep optional.
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


def run_stream(url: str, api_key: str, body: dict, timeout: float, max_events: int):
    resp, t0, t1 = post_json(url, api_key, body, "text/event-stream", timeout)
    ct = resp.headers.get("Content-Type", "")
    print(f"HTTP 200 content_type={ct} ttfb_ms={int((t1 - t0) * 1000)}")

    types = {}
    shown = 0
    lines = 0
    t_first_data = None
    try:
        for raw in resp:
            lines += 1
            try:
                line = raw.decode("utf-8", errors="replace").strip()
            except Exception:
                continue
            if not line:
                continue
            if not line.startswith("data:"):
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
            typ = v.get("type", "") or ""
            types[typ] = types.get(typ, 0) + 1
            if shown < max_events:
                # Print minimal fields only; avoid dumping full objects.
                delta = v.get("delta")
                text = v.get("text")
                msg = ""
                if isinstance(delta, str) and delta.strip():
                    msg = delta.strip().replace("\n", "\\n")
                    msg = msg[:160]
                    msg = f" delta={msg}"
                elif isinstance(text, str) and text.strip():
                    msg = text.strip().replace("\n", "\\n")
                    msg = msg[:160]
                    msg = f" text={msg}"
                print(f"EVENT type={typ}{msg}")
                shown += 1
    finally:
        resp.close()
    t2 = time.time()
    t_first = int(((t_first_data or t2) - t0) * 1000)
    print(f"summary: lines={lines} first_data_ms={t_first} types={json.dumps(types, ensure_ascii=True)} total_ms={int((t2 - t0) * 1000)}")


def run_nonstream(url: str, api_key: str, body: dict, timeout: float):
    resp, t0, t1 = post_json(url, api_key, body, "application/json", timeout)
    ct = resp.headers.get("Content-Type", "")
    data = resp.read()
    resp.close()
    t2 = time.time()
    print(f"HTTP 200 content_type={ct} ttfb_ms={int((t1 - t0) * 1000)} total_ms={int((t2 - t0) * 1000)} bytes={len(data)}")
    try:
        v = json.loads(data.decode("utf-8", errors="replace"))
    except Exception:
        print(data[:800].decode("utf-8", errors="replace"))
        return
    # Print only top-level keys to avoid dumping large content.
    keys = sorted(list(v.keys()))
    print("json_keys:", ",".join(keys))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--profile", choices=["main", "dog"], default="main")
    ap.add_argument("--provider", choices=["codex", "deepseek"], default=None)
    ap.add_argument("--stream", action="store_true")
    ap.add_argument("--no-stream", dest="stream", action="store_false")
    ap.set_defaults(stream=True)
    ap.add_argument("--timeout", type=float, default=None)
    ap.add_argument("--max-events", type=int, default=30)
    ap.add_argument("--text", default="Reply with exactly: ok")
    ap.add_argument("--model", default=None)
    ap.add_argument("--effort", choices=["low", "medium", "high", "xhigh"], default=None)
    args = ap.parse_args()

    root = Path(__file__).resolve().parents[1]
    cfg_path = root / "config" / ("mainAPI.json" if args.profile == "main" else "dogAPI.json")
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
        body = build_deepseek_body(cfg, args.stream, args.text)

    try:
        if args.stream:
            run_stream(url, api_key, body, timeout, args.max_events)
        else:
            run_nonstream(url, api_key, body, timeout)
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
