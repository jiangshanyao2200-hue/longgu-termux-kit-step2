#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)"
cd -- "$ROOT"

OUT="${1:-index.symbols.md}"

{
  echo "# 符号索引（自动生成）"
  echo ""
  echo "- 生成时间：$(date '+%Y-%m-%d %H:%M:%S')"
  echo "- 说明：本文件用于代码审查导航；为避免噪音，只列出顶层声明（文件级）与顶层函数（列首 fn / pub fn）。"
  echo ""
} >"$OUT"

for f in src/*.rs; do
  echo "## $f" >> "$OUT"
  echo "" >> "$OUT"
  echo "### 顶层类型/常量" >> "$OUT"
  if ! grep -nE '^(pub )?(struct|enum|type) |^const |^static ' "$f" >> "$OUT"; then
    echo "(none)" >> "$OUT"
  fi
  echo "" >> "$OUT"
  echo "### 顶层函数" >> "$OUT"
  if ! grep -nE '^(pub(\([^)]*\))?\s+)?fn [A-Za-z0-9_]+' "$f" >> "$OUT"; then
    echo "(none)" >> "$OUT"
  fi
  echo "" >> "$OUT"
done

echo "[gen_index] wrote $OUT"
