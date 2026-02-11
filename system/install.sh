#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AITERMUX_HOME="${AITERMUX_HOME:-$(cd "$ROOT/.." && pwd)}"
exec "$AITERMUX_HOME/install.sh" "$@"
