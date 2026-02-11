#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "[prepush-check] 扫描疑似密钥（sk-...）"

# 只针对“可能被提交”的内容扫描：排除构建产物与本地配置。
if grep -RIn "sk-" \
  --exclude-dir target \
  --exclude-dir memory \
  --exclude-dir log \
  --exclude-dir .git \
  --exclude "config/*.json" \
  -- .; then
  echo ""
  echo "[prepush-check] 检测到疑似密钥文本，请先清理再 push。" >&2
  exit 1
fi

echo "[prepush-check] ok"
