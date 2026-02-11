#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if command -v pkg >/dev/null 2>&1; then
  exec "$ROOT/scripts/bootstrap-termux.sh" "$@"
fi

missing=()
for cmd in cargo rustc; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    missing+=("$cmd")
  fi
done

if ((${#missing[@]})); then
  echo "[bootstrap] 检测到缺少依赖：${missing[*]}" >&2
  echo "[bootstrap] 当前环境不是 Termux（或没有 pkg），无法自动安装。" >&2
  echo "[bootstrap] 请手动安装 Rust 工具链后重试（需包含 cargo/rustc）。" >&2
  exit 1
fi

echo "[bootstrap] ok (非 Termux 环境：仅做依赖存在性检查)"
