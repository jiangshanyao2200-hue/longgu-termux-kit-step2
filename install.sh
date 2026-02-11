#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

# Global installer entrypoint (repo root).
# - Source tree is this repo.
# - Target install root defaults to ~/AItermux (kit2 overlay behavior).

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export AITERMUX_HOME="${AITERMUX_HOME:-$HOME/AItermux}"
# Where to copy the ProjectYing (Rust) system tree from.
export SYSTEM_SRC="${SYSTEM_SRC:-$ROOT/system}"

exec "$ROOT/Quickinstall/install.sh" "$@"
