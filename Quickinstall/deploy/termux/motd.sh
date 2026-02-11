#!/data/data/com.termux/files/usr/bin/bash
set +e

[ "${AITERMUX_MOTD_DISABLE:-0}" = "1" ] && exit 0
tty >/dev/null 2>&1 || exit 0

PREFIX="/data/data/com.termux/files/usr"
export PATH="$PREFIX/bin:/system/bin:/system/xbin:${PATH:-}"

ROOT_DIR="${AITERMUX_HOME:-$HOME/AItermux}"
START_DIR="$ROOT_DIR/startboot"
LOG_DIR="$ROOT_DIR/logs"
BASH_BIN="/data/data/com.termux/files/usr/bin/bash"
DEBUG="${AITERMUX_MOTD_DEBUG:-0}"
ERR_LOG="$LOG_DIR/motd-last.err"
META_LOG="$LOG_DIR/motd-last.meta"
META_REASON=""
META_WRITTEN=0

mkdir -p "$LOG_DIR" >/dev/null 2>&1 || true
[ -x "$BASH_BIN" ] || BASH_BIN="$(command -v bash 2>/dev/null || echo bash)"

cleanup_old_guards() {
  local keep="${AITERMUX_MOTD_GUARD_KEEP:-8}"
  [[ "$keep" =~ ^[0-9]+$ ]] || return 0
  (( keep <= 0 )) && return 0

  local -a guards=()
  shopt -s nullglob
  guards=("$LOG_DIR"/motd-guard-*)
  shopt -u nullglob

  ((${#guards[@]} <= keep)) && return 0

  # Keep newest N by mtime; remove the rest.
  ls -1t "$LOG_DIR"/motd-guard-* 2>/dev/null | tail -n +$((keep + 1)) | xargs -r rm -f -- 2>/dev/null || true
}

prune_keep_newest_files() {
  local dir="$1"
  local keep="${2:-10}"
  [[ -d "$dir" ]] || return 0
  [[ "$keep" =~ ^[0-9]+$ ]] || return 0
  (( keep <= 0 )) && return 0

  shopt -s nullglob
  local -a files=("$dir"/*)
  shopt -u nullglob
  ((${#files[@]} <= keep)) && return 0

  ls -1t "$dir"/* 2>/dev/null | tail -n +$((keep + 1)) | xargs -r rm -f -- 2>/dev/null || true
}

trim_file_tail_bytes() {
  local path="$1"
  local max_kb="${2:-512}"
  [[ -f "$path" ]] || return 0
  [[ "$max_kb" =~ ^[0-9]+$ ]] || return 0
  (( max_kb <= 0 )) && return 0

  local max_bytes=$((max_kb * 1024))
  local size_bytes=""
  size_bytes="$(wc -c <"$path" 2>/dev/null || true)"
  [[ "$size_bytes" =~ ^[0-9]+$ ]] || return 0
  (( size_bytes <= max_bytes )) && return 0

  local tmp="$LOG_DIR/.trim.$$.$RANDOM"
  if tail -c "$max_bytes" "$path" >"$tmp" 2>/dev/null; then
    mv -f "$tmp" "$path" 2>/dev/null || rm -f "$tmp" 2>/dev/null || true
  else
    rm -f "$tmp" 2>/dev/null || true
  fi
}

cleanup_system_log_dir() {
  local enabled="${AITERMUX_MOTD_CLEAN_SYSTEM_LOG:-1}"
  [[ "$enabled" = "0" ]] && return 0

  local sys_root="$ROOT_DIR/system"
  local sys_log="$sys_root/log"
  [[ -d "$sys_log" ]] || return 0

  local keep="${AITERMUX_MOTD_SYSTEM_LOG_KEEP:-10}"
  local runtime_kb="${AITERMUX_MOTD_SYSTEM_RUNTIME_MAX_KB:-512}"

  # The main runtime log can grow quickly; trim it aggressively.
  trim_file_tail_bytes "$sys_log/runtime.log" "$runtime_kb" || true

  # Old/one-off install logs (safe to delete; kept tiny but noisy).
  rm -f "$sys_log/pkg_termux_api_install.log" 2>/dev/null || true

  # Cache dirs: keep only newest N cache files.
  prune_keep_newest_files "$sys_log/adb-cache" "$keep" || true
  prune_keep_newest_files "$sys_log/bash-cache" "$keep" || true
  prune_keep_newest_files "$sys_log/search-cache" "$keep" || true
  prune_keep_newest_files "$sys_log/termux-api-cache" "$keep" || true

  # Temp/debug snippets: remove all (they are reproducible).
  rm -f "$sys_log/tmp"/* 2>/dev/null || true
}

TTY_ID="$(tty 2>/dev/null | tr -c 'a-zA-Z0-9' '_' | tr -s '_' '_' | sed 's/^_*//;s/_*$//')"
if [ -n "${TTY_ID:-}" ]; then
  GUARD_FILE="$LOG_DIR/motd-guard-$TTY_ID"
  RUN_FILE="$LOG_DIR/motd-running-$TTY_ID"
  {
    printf 'pid=%s\n' "$$"
    printf 'ts=%s\n' "$(date '+%F %T' 2>/dev/null || echo unknown)"
  } >"$RUN_FILE" 2>/dev/null || true
  NOW_SEC="$(date +%s 2>/dev/null || true)"
  if [ -n "${NOW_SEC:-}" ]; then
    LAST_SEC="$(cat "$GUARD_FILE" 2>/dev/null || true)"
    # Avoid double-trigger within the same second.
    if [ "$LAST_SEC" = "$NOW_SEC" ]; then
      exit 0
    fi
    printf '%s\n' "$NOW_SEC" >"$GUARD_FILE" 2>/dev/null || true
  fi
fi

cleanup_old_guards || true
cleanup_system_log_dir || true

if [ "$DEBUG" != "1" ]; then
  : >"$ERR_LOG" 2>/dev/null || true
  exec 2>>"$ERR_LOG"
fi

write_meta_once() {
  [ "$META_WRITTEN" = "1" ] && return 0
  META_WRITTEN=1
  {
    printf 'ts=%s\n' "$(date '+%F %T' 2>/dev/null || echo unknown)"
    printf 'tty=%s\n' "${TTY_ID:-}"
    printf 'reason=%s\n' "${META_REASON:-}"
    [ -n "${anim:-}" ] && printf 'anim=%s\n' "${anim:-}"
    [ -n "${final_size:-}" ] && printf 'size=%s\n' "${final_size:-}"
    [ -n "${timeout_limit:-}" ] && printf 'timeout=%s\n' "${timeout_limit:-}"
    printf 'fps=%s duration=%s hold=%s speed=%s\n' "${FPS:-}" "${DURATION:-}" "${HOLD:-}" "${SPEED:-}"
    printf 'elapsed_ms=%s rc=%s\n' "${elapsed_ms:-0}" "${rc:-0}"
  } >"$META_LOG" 2>/dev/null || true
}

cleanup() {
  stty sane >/dev/null 2>&1 || true
  tput cnorm >/dev/null 2>&1 || true
  if [ -t 1 ]; then
    printf '\033[0m\033[?25h\033[?7h\033[?1049l' >/dev/tty 2>/dev/null || true
    printf '\033[H\033[2J\033[3J' >/dev/tty 2>/dev/null || true
  fi
  write_meta_once || true
  [ -n "${RUN_FILE:-}" ] && rm -f "$RUN_FILE" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

pick_random() {
  local -a pool=()
  # 只从“动画脚本”里选：1.sh..11.sh（避免把实验脚本/辅助脚本选进去）
  # 仅用 bash 内建 glob/regex，避免 PATH/外部命令缺失导致“直接跳过动画”。
  local f base
  shopt -s nullglob
  for f in "$START_DIR"/*.sh; do
    base="${f##*/}"
    [[ "$base" =~ ^(10|11|[1-9])\.sh$ ]] || continue
    [ -x "$f" ] || continue
    pool+=("$f")
  done
  shopt -u nullglob
  [ "${#pool[@]}" -gt 0 ] || return 1
  printf '%s\n' "${pool[RANDOM % ${#pool[@]}]}"
}

[ -d "$START_DIR" ] || { META_REASON="startboot_missing"; exit 0; }
anim="$(pick_random || true)"
[ -n "${anim:-}" ] && [ -f "$anim" ] || { META_REASON="no_anim"; exit 0; }

args=(--altscr)
if [ "${AITERMUX_MOTD_COLOR:-1}" = "0" ]; then
  args+=(--no-color)
fi
if [ -n "${AITERMUX_MOTD_ARGS:-}" ]; then
  extra=(${AITERMUX_MOTD_ARGS})
  args+=("${extra[@]}")
fi

get_term_size_safe() {
  local cols_raw="" rows_raw="" cols_safe=""
  if stty_out="$(stty size </dev/tty 2>/dev/null)"; then
    rows_raw="${stty_out%% *}"
    cols_raw="${stty_out##* }"
    if ! printf '%s' "${rows_raw:-}" | grep -Eq '^[0-9]+$' || [ "${rows_raw:-0}" -le 0 ] 2>/dev/null; then
      rows_raw=""
    fi
    if ! printf '%s' "${cols_raw:-}" | grep -Eq '^[0-9]+$' || [ "${cols_raw:-0}" -le 0 ] 2>/dev/null; then
      cols_raw=""
    fi
  fi

  if ! printf '%s' "${cols_raw:-}" | grep -Eq '^[0-9]+$'; then
    cols_raw="$(tput cols 2>/dev/null || echo 80)"
  fi
  if ! printf '%s' "${rows_raw:-}" | grep -Eq '^[0-9]+$'; then
    rows_raw="$(tput lines 2>/dev/null || echo 24)"
  fi

  cols_safe="$cols_raw"
  if [ -n "${cols_safe:-}" ] && [ "$cols_safe" -gt 2 ] 2>/dev/null; then
    cols_safe=$((cols_safe - 1))
  fi
  [ -n "${cols_safe:-}" ] || cols_safe=80
  [ -n "${rows_raw:-}" ] || rows_raw=24
  printf '%sx%s\n' "$cols_safe" "$rows_raw"
}

wait_term_size_stable() {
  local last="" same=0 key _i
  for _i in 1 2 3 4 5 6 7 8 9 10 11 12; do
    key="$(get_term_size_safe)"
    if [ "$key" = "$last" ]; then
      same=$((same + 1))
    else
      same=0
      last="$key"
    fi
    if [ "$same" -ge 2 ]; then
      printf '%s\n' "$key"
      return 0
    fi
    sleep 0.05
  done
  printf '%s\n' "${last:-80x24}"
}

cap_size_if_needed() {
  local size_in="$1"
  if ! printf '%s' "$size_in" | grep -Eq '^[0-9]+x[0-9]+$'; then
    printf '80x24\n'
    return 0
  fi
  local cols="${size_in%x*}"
  local rows="${size_in#*x}"
  if [ "${cols:-0}" -le 0 ] 2>/dev/null; then
    cols=80
  fi
  if [ "${rows:-0}" -le 0 ] 2>/dev/null; then
    rows=24
  fi
  local max_cols="${AITERMUX_MOTD_MAX_COLS:-0}"
  local max_rows="${AITERMUX_MOTD_MAX_ROWS:-0}"

  if [ "${AITERMUX_MOTD_LIGHT:-0}" = "1" ]; then
    [ "$max_cols" = "0" ] && max_cols=70
    [ "$max_rows" = "0" ] && max_rows=24
  fi

  if printf '%s' "$max_cols" | grep -Eq '^[0-9]+$' && [ "$max_cols" -gt 0 ] 2>/dev/null; then
    if [ "$cols" -gt "$max_cols" ] 2>/dev/null; then
      cols="$max_cols"
    fi
  fi
  if printf '%s' "$max_rows" | grep -Eq '^[0-9]+$' && [ "$max_rows" -gt 0 ] 2>/dev/null; then
    if [ "$rows" -gt "$max_rows" ] 2>/dev/null; then
      rows="$max_rows"
    fi
  fi
  printf '%sx%s\n' "$cols" "$rows"
}

has_size_arg=0
for a in "${args[@]}"; do
  if [ "$a" = "--size" ] || [ "$a" = "-s" ]; then
    has_size_arg=1
    break
  fi
done
if [ "$has_size_arg" = "0" ]; then
  stable_size="$(wait_term_size_stable)"
  final_size="$(cap_size_if_needed "$stable_size")"
  args+=(--size "$final_size")
else
  final_size=""
fi

# Global animation tuning (startboot scripts honor these env vars).
# 说明：只默认固定 FPS（保护性能），其它节奏参数默认留空，让各动画脚本使用自己的默认值。
export FPS="${AITERMUX_MOTD_FPS:-10}"
export DURATION="${AITERMUX_MOTD_DURATION:-}"
export HOLD="${AITERMUX_MOTD_HOLD:-}"
export SPEED="${AITERMUX_MOTD_SPEED:-}"

normalize_timeout() {
  local t="$1"
  if [ -z "$t" ]; then
    printf '0'
    return 0
  fi
  if printf '%s' "$t" | grep -Eq '^0+([.][0]+)?$'; then
    printf '0'
    return 0
  fi
  if printf '%s' "$t" | grep -Eq '[a-zA-Z]$'; then
    printf '%s' "$t"
  else
    printf '%ss' "$t"
  fi
}

# 默认超时要足够大：避免动画在弱机上被 timeout “腰斩”，导致抽帧/节奏错乱。
timeout_start="$(normalize_timeout "${AITERMUX_MOTD_TIMEOUT_START:-12}")"
timeout_limit="$(normalize_timeout "${AITERMUX_MOTD_TIMEOUT:-$timeout_start}")"

start_ns="$(date +%s%N 2>/dev/null || echo 0)"
rc=0
if command -v timeout >/dev/null 2>&1 && [ "$timeout_limit" != "0" ]; then
  timeout_opts=()
  timeout --help 2>/dev/null | grep -q -- '--foreground' && timeout_opts+=(-f)
  timeout "${timeout_opts[@]}" -k 0.6s "$timeout_limit" "$BASH_BIN" "$anim" "${args[@]}" || rc=$?
else
  "$BASH_BIN" "$anim" "${args[@]}" || rc=$?
fi

end_ns="$(date +%s%N 2>/dev/null || echo 0)"
elapsed_ms=0
if [ "$start_ns" != "0" ] && [ "$end_ns" != "0" ] 2>/dev/null; then
  elapsed_ms=$(( (end_ns - start_ns) / 1000000 ))
fi
META_REASON="done"
write_meta_once || true

exit 0
