#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
XSTARTUP="${XSTARTUP:-startxfce4}"
USE_NODBUS=0
USE_NOGPU=0

show_help() {
  cat <<'EOF'
tx11start [--xstartup NAME] [--nodbus] [--nogpu]
EOF
}

while (($# >= 1)); do
  case "$1" in
    --xstartup)
      shift
      XSTARTUP="${1:-startxfce4}"
      ;;
    --nodbus|--no-dbus)
      USE_NODBUS=1
      ;;
    --nogpu|--no-gpu)
      USE_NOGPU=1
      ;;
    --help|-h)
      show_help
      exit 0
      ;;
    *)
      show_help
      exit 2
      ;;
  esac
  shift
done

export XDG_RUNTIME_DIR="$PREFIX_DIR/tmp"
mkdir -p "$XDG_RUNTIME_DIR"
export DISPLAY=:0

pkill -f com.termux.x11 >/dev/null 2>&1 || true

if command -v termux-wake-lock >/dev/null 2>&1; then
  termux-wake-lock >/dev/null 2>&1 || true
fi

pulseaudio --check >/dev/null 2>&1 || pulseaudio --start --exit-idle-time=-1 >/dev/null 2>&1 || true

if ((USE_NOGPU)); then
  LIBGL_ALWAYS_SOFTWARE=1 termux-x11 :0 >/dev/null 2>&1 &
else
  termux-x11 :0 >/dev/null 2>&1 &
fi

sleep 1
am start --user 0 -n com.termux.x11/com.termux.x11.MainActivity >/dev/null 2>&1 || true
sleep 1

if ((USE_NODBUS)); then
  env DISPLAY=:0 XDG_CONFIG_DIRS="$PREFIX_DIR/etc/xdg" "$XSTARTUP" >/dev/null 2>&1 &
else
  env DISPLAY=:0 XDG_CONFIG_DIRS="$PREFIX_DIR/etc/xdg" dbus-launch --exit-with-session "$XSTARTUP" >/dev/null 2>&1 &
fi
