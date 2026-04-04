#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export AITERMUX_HOME="${AITERMUX_HOME:-$HOME/AItermux}"
export AITERMUX_PROJECTYING_REPO="${AITERMUX_PROJECTYING_REPO:-https://github.com/jiangshanyao2200-hue/projectying.git}"

exec "$ROOT/Quickinstall/install.sh" "$@"
