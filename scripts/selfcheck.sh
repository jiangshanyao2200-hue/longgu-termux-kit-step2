#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "[selfcheck] bootstrap"
"$ROOT/scripts/bootstrap.sh"

echo "[selfcheck] cargo test"
cargo test -q

echo "[selfcheck] cargo run -- --selfcheck"
cargo run -q -- --selfcheck

echo "[selfcheck] done"
