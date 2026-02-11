#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

# Global installer entrypoint (user workspace root).
# Delegates to Quickinstall templates and writes into Termux + ~/AItermux.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export AITERMUX_HOME="${AITERMUX_HOME:-$ROOT}"

exec "$ROOT/Quickinstall/install.sh" "$@"

