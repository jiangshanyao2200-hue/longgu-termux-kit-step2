#!/data/data/com.termux/files/usr/bin/bash
set +e

[ "${AITERMUX_MOTD_DISABLE:-0}" = "1" ] && exit 0
tty >/dev/null 2>&1 || exit 0

ROOT_DIR="${AITERMUX_HOME:-$HOME/AItermux}"
START_DIR="$ROOT_DIR/startboot"
LOG_DIR="$ROOT_DIR/logs"
BASH_BIN="/data/data/com.termux/files/usr/bin/bash"

mkdir -p "$LOG_DIR" >/dev/null 2>&1 || true
[ -x "$BASH_BIN" ] || BASH_BIN="$(command -v bash 2>/dev/null || echo bash)"

cleanup() {
  stty sane >/dev/null 2>&1 || true
  tput cnorm >/dev/null 2>&1 || true
  if [ -t 1 ]; then
    printf '\033[0m\033[?25h\033[?7h\033[?1049l' >/dev/tty 2>/dev/null || true
    printf '\033[H\033[2J\033[3J' >/dev/tty 2>/dev/null || true
  fi
}
trap cleanup EXIT INT TERM

pick_random() {
  local -a pool=()
  mapfile -t pool < <(find "$START_DIR" -maxdepth 1 -type f -name '*.sh' | sort)
  [ "${#pool[@]}" -gt 0 ] || return 1
  printf '%s\n' "${pool[RANDOM % ${#pool[@]}]}"
}

[ -d "$START_DIR" ] || exit 0
anim="$(pick_random || true)"
[ -n "${anim:-}" ] && [ -f "$anim" ] || exit 0

args=(--altscr)
if [ "${AITERMUX_MOTD_COLOR:-1}" = "0" ]; then
  args+=(--no-color)
fi
if [ -n "${AITERMUX_MOTD_ARGS:-}" ]; then
  extra=(${AITERMUX_MOTD_ARGS})
  args+=("${extra[@]}")
fi

if ! printf '%s\n' "${args[@]}" | grep -qx -- '--size'; then
  cols="$(tput cols 2>/dev/null || echo 80)"
  rows="$(tput lines 2>/dev/null || echo 24)"
  if [ "$cols" -gt 2 ] 2>/dev/null; then
    cols=$((cols - 1))
  fi
  args+=(--size "${cols}x${rows}")
fi

timeout_raw="${AITERMUX_MOTD_TIMEOUT:-6}"
if [ "$timeout_raw" = "0" ]; then
  "$BASH_BIN" "$anim" "${args[@]}"
  exit 0
fi

timeout_value="$timeout_raw"
if ! printf '%s' "$timeout_value" | grep -Eq '[a-zA-Z]$'; then
  timeout_value="${timeout_value}s"
fi

if command -v timeout >/dev/null 2>&1; then
  timeout "$timeout_value" "$BASH_BIN" "$anim" "${args[@]}" >/dev/null 2>&1 || true
else
  "$BASH_BIN" "$anim" "${args[@]}" >/dev/null 2>&1 || true
fi

exit 0
