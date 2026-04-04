#!/data/data/com.termux/files/usr/bin/bash
set +e

[ "${AITERMUX_MOTD_DISABLE:-0}" = "1" ] && exit 0
tty >/dev/null 2>&1 || exit 0

PREFIX="/data/data/com.termux/files/usr"
export PATH="$PREFIX/bin:/system/bin:/system/xbin:${PATH:-}"

# Ensure UTF-8 width math for banner centering (█ etc.).
export LANG="${LANG:-C.UTF-8}"
export LC_ALL="${LC_ALL:-C.UTF-8}"

TTY_DEV="/dev/tty"

tty_printf() {
  if [ -w "$TTY_DEV" ] 2>/dev/null; then
    printf "$@" >"$TTY_DEV"
  else
    printf "$@"
  fi
}

ROOT_DIR="${AITERMUX_HOME:-$HOME/AItermux}"
START_DIR="$ROOT_DIR/startboot"
LOG_DIR="$ROOT_DIR/logs"
STATE_DIR="$ROOT_DIR/.state/motd"
BASH_BIN="/data/data/com.termux/files/usr/bin/bash"
DEBUG="${AITERMUX_MOTD_DEBUG:-0}"
STARTUP_LOG="$LOG_DIR/startup.log"
GUARD_STATE_FILE="$STATE_DIR/guard.state"
META_REASON=""
META_WRITTEN=0
MOTD_KEEP_SCREEN=0

mkdir -p "$LOG_DIR" "$STATE_DIR" >/dev/null 2>&1 || true
[ -x "$BASH_BIN" ] || BASH_BIN="$(command -v bash 2>/dev/null || echo bash)"

startup_log() {
  local ts
  ts="$(date -u '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date '+%F %T' 2>/dev/null || echo unknown)"
  printf '%s %s\n' "$ts" "$*" >>"$STARTUP_LOG" 2>/dev/null || true
}

cleanup_state_dir() {
  rm -f \
    "$STATE_DIR"/motd-guard-* \
    "$STATE_DIR"/motd-running-* \
    2>/dev/null || true
}

prune_keep_newest_files() {
  local dir="$1"
  local keep="${2:-10}"
  [[ -d "$dir" ]] || return 0
  [[ "$keep" =~ ^[0-9]+$ ]] || return 0
  (( keep <= 0 )) && return 0

  # List newest-first without glob expansion; remove older files.
  local seen=0 name path
  while IFS= read -r name; do
    [[ -n "$name" ]] || continue
    path="$dir/$name"
    [[ -f "$path" ]] || continue
    seen=$((seen + 1))
    (( seen > keep )) || continue
    rm -f -- "$path" 2>/dev/null || true
  done < <(LC_ALL=C ls -1t -- "$dir" 2>/dev/null || true)
}

shrink_file_tail_if_over_kb() {
  local path="$1"
  local max_kb="${2:-1024}"
  local keep_kb="${3:-$max_kb}"
  [[ -f "$path" ]] || return 0
  [[ "$max_kb" =~ ^[0-9]+$ ]] || return 0
  [[ "$keep_kb" =~ ^[0-9]+$ ]] || return 0
  (( max_kb <= 0 )) && return 0
  (( keep_kb <= 0 )) && return 0

  local max_bytes=$((max_kb * 1024))
  local keep_bytes=$((keep_kb * 1024))
  local size_bytes=""
  size_bytes="$(wc -c <"$path" 2>/dev/null || true)"
  [[ "$size_bytes" =~ ^[0-9]+$ ]] || return 0
  (( size_bytes <= max_bytes )) && return 0
  (( keep_bytes > max_bytes )) && keep_bytes="$max_bytes"

  local tmp="$LOG_DIR/.trim.$$.$RANDOM"
  if tail -c "$keep_bytes" "$path" >"$tmp" 2>/dev/null; then
    mv -f "$tmp" "$path" 2>/dev/null || rm -f "$tmp" 2>/dev/null || true
  else
    rm -f "$tmp" 2>/dev/null || true
  fi
}

cleanup_project_log_dirs() {
  local enabled="${AITERMUX_MOTD_CLEAN_SYSTEM_LOG:-1}"
  [[ "$enabled" = "0" ]] && return 0

  local keep="${AITERMUX_MOTD_SYSTEM_LOG_KEEP:-10}"
  local runtime_kb="${AITERMUX_MOTD_SYSTEM_RUNTIME_MAX_KB:-512}"
  local sys_root="" sys_log=""

  for sys_root in "$ROOT_DIR/projectying"; do
    sys_log="$sys_root/log"
    [[ -d "$sys_log" ]] || continue

    shrink_file_tail_if_over_kb "$sys_log/runtime.log" "$runtime_kb" "$runtime_kb" || true
    rm -f "$sys_log/pkg_termux_api_install.log" 2>/dev/null || true
    prune_keep_newest_files "$sys_log/adb-cache" "$keep" || true
    prune_keep_newest_files "$sys_log/bash-cache" "$keep" || true
    prune_keep_newest_files "$sys_log/search-cache" "$keep" || true
    prune_keep_newest_files "$sys_log/termux-api-cache" "$keep" || true
    rm -f "$sys_log/tmp"/* 2>/dev/null || true
  done
}

cleanup_logs_dir() {
  local startup_max_kb="${AITERMUX_STARTUP_LOG_MAX_KB:-1024}"
  local startup_keep_kb="${AITERMUX_STARTUP_LOG_KEEP_KB:-512}"
  shrink_file_tail_if_over_kb "$STARTUP_LOG" "$startup_max_kb" "$startup_keep_kb" || true
  rm -f \
    "$LOG_DIR"/launcher.log \
    "$LOG_DIR"/motd-last.err \
    "$LOG_DIR"/motd-last.meta \
    "$LOG_DIR"/motd-guard-* \
    "$LOG_DIR"/motd-running-* \
    "$LOG_DIR"/ying-selfcheck.err \
    "$LOG_DIR"/ying-selfcheck.out \
    2>/dev/null || true
}

TTY_ID="$(tty 2>/dev/null | tr -c 'a-zA-Z0-9' '_' | tr -s '_' '_' | sed 's/^_*//;s/_*$//')"
cleanup_state_dir || true
cleanup_logs_dir || true
cleanup_project_log_dirs || true

if [ -n "${TTY_ID:-}" ]; then
  NOW_SEC="$(date +%s 2>/dev/null || true)"
  if [ -n "${NOW_SEC:-}" ]; then
    LAST_TTY="$(sed -n 's/^last_tty=//p' "$GUARD_STATE_FILE" 2>/dev/null | head -n 1)"
    LAST_SEC="$(sed -n 's/^last_sec=//p' "$GUARD_STATE_FILE" 2>/dev/null | head -n 1)"
    # Avoid double-trigger within the same second on the same tty.
    if [ "$LAST_TTY" = "$TTY_ID" ] && [ "$LAST_SEC" = "$NOW_SEC" ]; then
      exit 0
    fi
    GUARD_TMP="$STATE_DIR/.guard.$$.$RANDOM"
    {
      printf 'last_tty=%s\n' "$TTY_ID"
      printf 'last_sec=%s\n' "$NOW_SEC"
      printf 'updated_at=%s\n' "$(date '+%F %T' 2>/dev/null || echo unknown)"
    } >"$GUARD_TMP" 2>/dev/null && mv -f "$GUARD_TMP" "$GUARD_STATE_FILE" 2>/dev/null || rm -f "$GUARD_TMP" 2>/dev/null || true
  fi
fi

if [ "$DEBUG" != "1" ]; then
  exec 2>>"$STARTUP_LOG"
fi

write_meta_once() {
  [ "$META_WRITTEN" = "1" ] && return 0
  META_WRITTEN=1
  startup_log \
    "motd reason=${META_REASON:-} tty=${TTY_ID:-} anim=${anim##*/} size=${final_size:-na} timeout=${timeout_limit:-na} fps=${FPS:-na} duration=${DURATION:-script-default} hold=${HOLD:-script-default} speed=${SPEED:-script-default} elapsed_ms=${elapsed_ms:-0} rc=${rc:-0}"
  cleanup_logs_dir || true
}

cleanup() {
  stty sane >/dev/null 2>&1 || true
  tput cnorm >/dev/null 2>&1 || true
  if [ -t 1 ] && [ "${MOTD_KEEP_SCREEN:-0}" != "1" ]; then
    tty_printf '\033[0m\033[?25h\033[?7h\033[?1049l' || true
    tty_printf '\033[H\033[2J\033[3J' || true
  fi
  write_meta_once || true
}
trap cleanup EXIT INT TERM

pick_random() {
  local -a pool=()
  local f base
  shopt -s nullglob
  for f in "$START_DIR"/*.sh; do
    base="${f##*/}"
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
# 说明：默认提升到 15 FPS，减少拖拍和卡顿感；其它节奏参数默认留空，让各动画脚本使用自己的默认值。
export FPS="${AITERMUX_MOTD_FPS:-15}"
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

motd_term_cols() {
  local cols_raw="" stty_out=""

  if stty_out="$(stty size </dev/tty 2>/dev/null)"; then
    cols_raw="${stty_out##* }"
  fi

  if ! printf '%s' "${cols_raw:-}" | grep -Eq '^[0-9]+$'; then
    cols_raw="$(tput cols 2>/dev/null || echo 80)"
  fi

  if ! printf '%s' "${cols_raw:-}" | grep -Eq '^[0-9]+$' || [ "${cols_raw:-0}" -le 0 ] 2>/dev/null; then
    cols_raw=80
  fi

  if [ "$cols_raw" -le 20 ] 2>/dev/null; then
    cols_raw=80
  fi

  printf '%s' "$cols_raw"
}

motd_print_center_ascii() {
  local line="$1"
  local style="${2:-}"
  local reset='\033[0m'
  local cols pad
  cols="$(motd_term_cols)"
  pad=$(( (cols - ${#line}) / 2 ))
  if [ "$pad" -lt 0 ] 2>/dev/null; then
    pad=0
  fi
  if [ -n "$style" ]; then
    tty_printf '%*s%b%s%b\n' "$pad" '' "$style" "$line" "$reset"
  else
    tty_printf '%*s%s\n' "$pad" '' "$line"
  fi
}

motd_art_style() {
  local variant="$1"
  local line_no="$2"
  case "${variant}:${line_no}" in
    shadow:*)
      printf '\033[2;38;2;255;0;153m'
      ;;
    phase_a:1|phase_a:6)
      printf '\033[1;38;2;255;92;218m'
      ;;
    phase_a:2|phase_a:5)
      printf '\033[1;38;2;170;120;255m'
      ;;
    phase_a:3|phase_a:4)
      printf '\033[1;38;2;0;238;255m'
      ;;
    phase_b:1|phase_b:6)
      printf '\033[1;38;2;0;255;229m'
      ;;
    phase_b:2|phase_b:5)
      printf '\033[1;38;2;255;255;255m'
      ;;
    phase_b:3|phase_b:4)
      printf '\033[1;38;2;255;92;218m'
      ;;
    final:1)
      printf '\033[1;38;2;0;255;229m'
      ;;
    final:2)
      printf '\033[1;38;2;114;198;255m'
      ;;
    final:3)
      printf '\033[1;38;2;255;92;218m'
      ;;
    final:4)
      printf '\033[1;38;2;186;126;255m'
      ;;
    final:5)
      printf '\033[1;38;2;255;92;218m'
      ;;
    final:6)
      printf '\033[1;38;2;255;255;255m'
      ;;
    *)
      printf '\033[1;97m'
      ;;
  esac
}

motd_title_style() {
  local variant="$1"
  case "$variant" in
    shadow)
      printf '\033[2;38;2;255;0;153m'
      ;;
    phase_a)
      printf '\033[1;38;2;255;92;218m'
      ;;
    phase_b)
      printf '\033[1;38;2;0;255;229m'
      ;;
    final)
      printf '\033[48;2;22;14;41m\033[1;38;2;255;255;255m'
      ;;
    *)
      printf '\033[1;97m'
      ;;
  esac
}

motd_draw_launcher_art() {
  local variant="$1"
  local with_shadow="${2:-0}"
  local art style idx=0
  while IFS= read -r art; do
    idx=$((idx + 1))
    style="$(motd_art_style "$variant" "$idx")"
    if [ "$with_shadow" = "1" ]; then
      motd_print_center_ascii " ${art}" "$style"
    else
      motd_print_center_ascii "${art}" "$style"
    fi
  done <<'EOF'
████████╗███████╗██████╗ ███╗   ███╗██╗   ██╗██╗  ██╗
╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║   ██║╚██╗██╔╝
   ██║   █████╗  ██████╔╝██╔████╔██║██║   ██║ ╚███╔╝
   ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║   ██║ ██╔██╗
   ██║   ███████╗██║  ██║██║ ╚═╝ ██║╚██████╔╝██╔╝ ██╗
   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝
EOF
}

motd_draw_launcher_art_stack() {
  local variant="$1"
  motd_draw_launcher_art shadow 1
  tty_printf '\033[6A'
  motd_draw_launcher_art "$variant" 0
}

motd_print_launcher_title() {
  local variant="$1"
  local main_style reset='\033[0m'
  main_style="$(motd_title_style "$variant")"
  tty_printf '  %b● TERMUX LAUNCHER%b\n' "$main_style" "$reset"
}

motd_render_launcher_frame() {
  local variant="$1"
  local cols=80
  local top_pad=5
  local bottom_pad=3
  local _pad=0
  tty_printf '\033[?1049l' || true
  tty_printf '\033[H\033[2J'
  cols="$(motd_term_cols)"
  if [ "${cols:-0}" -ge 64 ] 2>/dev/null; then
    MOTD_MENU_TOP_ROW=$((1 + top_pad + 6 + bottom_pad + 2))
    _pad=0
    while [ "$_pad" -lt "$top_pad" ] 2>/dev/null; do
      tty_printf '\n'
      _pad=$(( _pad + 1 ))
    done
    motd_draw_launcher_art_stack "$variant"
    _pad=0
    while [ "$_pad" -lt "$bottom_pad" ] 2>/dev/null; do
      tty_printf '\n'
      _pad=$(( _pad + 1 ))
    done
  else
    MOTD_MENU_TOP_ROW=4
    tty_printf '\n'
  fi
  motd_print_launcher_title "$variant"
  tty_printf '\n'
}

motd_path_prepend_once() {
  local dir="$1"
  [ -n "${dir:-}" ] || return 0
  case ":${PATH:-}:" in
    *":$dir:"*) ;;
    *) PATH="$dir:${PATH:-}" ;;
  esac
}

motd_refresh_launcher_env() {
  motd_path_prepend_once "$HOME/.local/bin"
  if [ -f "$HOME/.codex/termux_env.sh" ]; then
    # 主要用于补 PATH；wrapper 本身仍优先走脚本文件，不依赖 shell function。
    . "$HOME/.codex/termux_env.sh" >/dev/null 2>&1 || true
  fi
}

motd_launcher_log() {
  startup_log "launcher $*"
  cleanup_logs_dir || true
}

motd_bootstrap_path() {
  printf '%s\n' "$ROOT_DIR/bin/aitermux-bootstrap"
}

motd_bootstrap_component_now() {
  local component="$1"
  local bootstrap=""

  [ -n "${component:-}" ] || return 1
  bootstrap="$(motd_bootstrap_path)"
  if [ ! -x "$bootstrap" ]; then
    motd_launcher_log "bootstrap_missing component=$component path=$bootstrap"
    return 1
  fi

  motd_launcher_log "bootstrap_run force=1 component=$component"
  if "$bootstrap" --quiet --force --component "$component"; then
    motd_refresh_launcher_env || true
    motd_launcher_log "bootstrap_ok component=$component"
    return 0
  fi

  motd_launcher_log "bootstrap_fail component=$component"
  return 1
}

motd_has_projectying() {
  [ -x "$ROOT_DIR/projectying/run.sh" ]
}

motd_has_codex() {
  [ -x "$HOME/.local/bin/codex" ] || return 1
  [ -f "$PREFIX/lib/node_modules/@openai/codex/bin/codex.js" ]
}

motd_has_gemini() {
  if [ ! -x "$HOME/.local/bin/gemini" ] && [ ! -x "$PREFIX/bin/gemini" ]; then
    return 1
  fi
  [ -f "$PREFIX/lib/node_modules/@google/gemini-cli/dist/index.js" ]
}

motd_projectying_cmd() {
  if [ -x "$ROOT_DIR/bin/aitermux" ]; then
    printf '%s\n' "$ROOT_DIR/bin/aitermux"
    return 0
  fi
  if [ -x "$ROOT_DIR/projectying/run.sh" ]; then
    printf '%s\n' "$ROOT_DIR/projectying/run.sh"
    return 0
  fi
  return 1
}

motd_codex_cmd() {
  if [ -x "$HOME/.local/bin/codex" ]; then
    printf '%s\n' "$HOME/.local/bin/codex"
    return 0
  fi
  if [ -x "$PREFIX/bin/codex" ]; then
    printf '%s\n' "$PREFIX/bin/codex"
    return 0
  fi
  return 1
}

motd_gemini_cmd() {
  if [ -x "$HOME/.local/bin/gemini" ]; then
    printf '%s\n' "$HOME/.local/bin/gemini"
    return 0
  fi
  if [ -x "$PREFIX/bin/gemini" ]; then
    printf '%s\n' "$PREFIX/bin/gemini"
    return 0
  fi
  return 1
}

motd_tty_run() {
  local cwd="$1"
  shift
  local rc=0

  motd_launcher_log "launch cwd=${cwd:-$PWD} cmd=$*"
  if [ -n "${cwd:-}" ]; then
    if [ -r "$TTY_DEV" ] && [ -w "$TTY_DEV" ]; then
      (cd "$cwd" && "$@" <"$TTY_DEV" >"$TTY_DEV" 2>"$TTY_DEV")
    else
      (cd "$cwd" && "$@")
    fi
  else
    if [ -r "$TTY_DEV" ] && [ -w "$TTY_DEV" ]; then
      ("$@" <"$TTY_DEV" >"$TTY_DEV" 2>"$TTY_DEV")
    else
      ("$@")
    fi
  fi
  rc=$?
  motd_launcher_log "exit rc=$rc cwd=${cwd:-$PWD} cmd=$*"
  return $rc
}

motd_launch_choice() {
  local choice="$1"
  local cmd=""
  local rc=0

  motd_refresh_launcher_env || true
  motd_launcher_log "choice=${choice:-invalid} source=motd-menu"

  case "$choice" in
    1)
      if ! motd_has_projectying; then
        motd_bootstrap_component_now projectying || true
      fi
      cmd="$(motd_projectying_cmd || true)"
      if [ -n "${cmd:-}" ]; then
        motd_tty_run "$HOME" "$cmd"
        rc=$?
      else
        tty_printf '[launcher] 未找到 Project 萤启动入口。\n'
        rc=127
      fi
      ;;
    2)
      if ! motd_has_codex; then
        motd_bootstrap_component_now codex || true
      fi
      cmd="$(motd_codex_cmd || true)"
      if [ -n "${cmd:-}" ] && motd_has_codex; then
        motd_tty_run "$HOME" "$cmd"
        rc=$?
      else
        tty_printf '[launcher] 未找到 codex 命令。\n'
        rc=127
      fi
      ;;
    3)
      if ! motd_has_gemini; then
        motd_bootstrap_component_now gemini || true
      fi
      cmd="$(motd_gemini_cmd || true)"
      if [ -n "${cmd:-}" ] && motd_has_gemini; then
        motd_tty_run "$HOME" "$cmd"
        rc=$?
      else
        tty_printf '[launcher] 未找到 gemini 命令。\n'
        rc=127
      fi
      ;;
    4)
      if command -v tx11start >/dev/null 2>&1; then
        motd_tty_run "$HOME" tx11start
        rc=$?
      else
        tty_printf '[launcher] 未找到 tx11start 命令。\n'
        rc=127
      fi
      ;;
    *)
      rc=1
      ;;
  esac

  return "$rc"
}

motd_print_launcher_menu_item() {
  local selected="$1"
  local idx="$2"
  local label="$3"
  local reset="$4"
  local bold="$5"
  local fg_white="$6"
  local fg_cyan="$7"
  local fg_cyan_dim="$8"
  local fg_magenta="$9"
  local pointer=' '
  local pointer_style="$fg_cyan_dim"
  local badge_style="$fg_magenta"
  local label_style="${bold}${fg_white}"

  if [ "$selected" = "$idx" ]; then
    pointer='›'
    pointer_style="$fg_cyan"
    badge_style="$fg_cyan"
    label_style="${bold}${fg_cyan}"
  fi

  tty_printf '  %b%s%b %b[%s]%b  %b%s%b' \
    "$pointer_style" "$pointer" "$reset" \
    "$badge_style" "$idx" "$reset" \
    "$label_style" "$label" "$reset"
}

motd_launcher_menu_styles_init() {
  local reset='' bold='' dim=''
  local fg_white='' fg_cyan='' fg_cyan_dim='' fg_magenta='' fg_violet='' fg_blue=''
  local cols=80
  local hint=''
  local label1='启动 PROJECT 萤'
  local label2='启动 CODEX'
  local label3='启动 Gemini'
  local label4='启动 Xfce 图形界面'
  local menu_top_row=4

  if [ "${AITERMUX_MOTD_COLOR:-1}" != "0" ]; then
    reset=$'\033[0m'
    bold=$'\033[1m'
    dim=$'\033[2m'
    fg_white=$'\033[97m'
    fg_cyan=$'\033[38;2;0;255;229m'
    fg_cyan_dim=$'\033[38;2;105;190;210m'
    fg_magenta=$'\033[38;2;255;92;218m'
    fg_violet=$'\033[38;2;170;120;255m'
    fg_blue=$'\033[38;2;114;198;255m'
  fi

  cols="$(motd_term_cols)"
  if [ "${cols:-0}" -lt 34 ] 2>/dev/null; then
    label1='PROJECT 萤'
    label2='CODEX'
    label3='Gemini'
    label4='Xfce'
  fi
  if [ "${cols:-0}" -lt 26 ] 2>/dev/null; then
    label1='萤'
  fi
  hint="  ${fg_cyan_dim}↑↓ 选择  ·  Enter 进入  ·  Esc 退出到 Shell${reset}"
  if [ "${cols:-0}" -lt 40 ] 2>/dev/null; then
    hint="  ${fg_cyan_dim}↑↓ 选择  ·  Enter 进入  ·  Esc 退出${reset}"
  fi
  if [ "${cols:-0}" -lt 28 ] 2>/dev/null; then
    hint=''
  fi

  MOTD_MENU_RESET="$reset"
  MOTD_MENU_BOLD="$bold"
  MOTD_MENU_FG_WHITE="$fg_white"
  MOTD_MENU_FG_CYAN="$fg_cyan"
  MOTD_MENU_FG_CYAN_DIM="$fg_cyan_dim"
  MOTD_MENU_FG_MAGENTA="$fg_magenta"
  MOTD_MENU_DECO="  ${fg_violet}▪▪▪${fg_magenta}───────── ${fg_blue}───── ${fg_cyan}───${dim}${fg_white}  - - · ·${reset}"
  if [ "${cols:-0}" -lt 40 ] 2>/dev/null; then
    MOTD_MENU_DECO="  ${fg_violet}▪▪▪${fg_magenta}──── ${fg_blue}─── ${fg_cyan}──${dim}${fg_white}  · ·${reset}"
  fi
  if [ "${cols:-0}" -lt 28 ] 2>/dev/null; then
    MOTD_MENU_DECO="  ${fg_violet}▪▪▪${fg_blue}── ${fg_cyan}──${reset}"
  fi
  MOTD_MENU_HINT="$hint"
  MOTD_MENU_LABEL_1="$label1"
  MOTD_MENU_LABEL_2="$label2"
  MOTD_MENU_LABEL_3="$label3"
  MOTD_MENU_LABEL_4="$label4"
  if [ -n "${MOTD_MENU_TOP_ROW:-}" ] && [ "${MOTD_MENU_TOP_ROW:-0}" -gt 0 ] 2>/dev/null; then
    menu_top_row="$MOTD_MENU_TOP_ROW"
  fi
  MOTD_MENU_FIRST_ITEM_ROW=$((menu_top_row + 2))
  if [ -n "$hint" ]; then
    MOTD_MENU_CURSOR_ROW=$((menu_top_row + 11))
  else
    MOTD_MENU_CURSOR_ROW=$((menu_top_row + 9))
  fi
}

motd_move_cursor_to() {
  local row="$1"
  local col="${2:-1}"
  [ -n "${row:-}" ] || return 0
  [ -n "${col:-}" ] || col=1
  tty_printf '\033[%s;%sH' "$row" "$col"
}

motd_clear_row_at() {
  local row="$1"
  motd_move_cursor_to "$row" 1
  tty_printf '\033[2K\r'
}

motd_render_launcher_menu_text_row() {
  local row="$1"
  local text="$2"
  motd_move_cursor_to "$row" 1
  tty_printf '\033[2K\r%s' "$text"
}

motd_render_launcher_menu_item_row() {
  local selected="$1"
  local idx="$2"
  local row=0

  case "$idx" in
    1) row="${MOTD_MENU_FIRST_ITEM_ROW:-0}" ;;
    2) row=$(( ${MOTD_MENU_FIRST_ITEM_ROW:-0} + 1 )) ;;
    3) row=$(( ${MOTD_MENU_FIRST_ITEM_ROW:-0} + 2 )) ;;
    4) row=$(( ${MOTD_MENU_FIRST_ITEM_ROW:-0} + 3 )) ;;
    *) row=0 ;;
  esac
  [ "$row" -gt 0 ] 2>/dev/null && motd_move_cursor_to "$row" 1
  tty_printf '\033[2K\r'
  case "$idx" in
    1)
      motd_print_launcher_menu_item "$selected" 1 "$MOTD_MENU_LABEL_1" "$MOTD_MENU_RESET" "$MOTD_MENU_BOLD" "$MOTD_MENU_FG_WHITE" "$MOTD_MENU_FG_CYAN" "$MOTD_MENU_FG_CYAN_DIM" "$MOTD_MENU_FG_MAGENTA"
      ;;
    2)
      motd_print_launcher_menu_item "$selected" 2 "$MOTD_MENU_LABEL_2" "$MOTD_MENU_RESET" "$MOTD_MENU_BOLD" "$MOTD_MENU_FG_WHITE" "$MOTD_MENU_FG_CYAN" "$MOTD_MENU_FG_CYAN_DIM" "$MOTD_MENU_FG_MAGENTA"
      ;;
    3)
      motd_print_launcher_menu_item "$selected" 3 "$MOTD_MENU_LABEL_3" "$MOTD_MENU_RESET" "$MOTD_MENU_BOLD" "$MOTD_MENU_FG_WHITE" "$MOTD_MENU_FG_CYAN" "$MOTD_MENU_FG_CYAN_DIM" "$MOTD_MENU_FG_MAGENTA"
      ;;
    4)
      motd_print_launcher_menu_item "$selected" 4 "$MOTD_MENU_LABEL_4" "$MOTD_MENU_RESET" "$MOTD_MENU_BOLD" "$MOTD_MENU_FG_WHITE" "$MOTD_MENU_FG_CYAN" "$MOTD_MENU_FG_CYAN_DIM" "$MOTD_MENU_FG_MAGENTA"
      ;;
  esac
}

motd_render_launcher_menu_items() {
  local selected="${1:-1}"
  motd_render_launcher_menu_item_row "$selected" 1
  motd_render_launcher_menu_item_row "$selected" 2
  motd_render_launcher_menu_item_row "$selected" 3
  motd_render_launcher_menu_item_row "$selected" 4
}

motd_render_launcher_menu_static() {
  local selected="${1:-1}"
  local top_row=4
  motd_launcher_menu_styles_init

  if [ -n "${MOTD_MENU_TOP_ROW:-}" ] && [ "${MOTD_MENU_TOP_ROW:-0}" -gt 0 ] 2>/dev/null; then
    top_row="$MOTD_MENU_TOP_ROW"
  fi
  motd_render_launcher_menu_text_row "$top_row" "$MOTD_MENU_DECO"
  motd_clear_row_at $((top_row + 1))
  motd_render_launcher_menu_items "$selected"
  motd_clear_row_at $((top_row + 6))
  motd_render_launcher_menu_text_row $((top_row + 7)) "$MOTD_MENU_DECO"
  motd_clear_row_at $((top_row + 8))
  if [ -n "$MOTD_MENU_HINT" ]; then
    motd_render_launcher_menu_text_row $((top_row + 9)) "$MOTD_MENU_HINT"
    motd_clear_row_at $((top_row + 10))
  else
    motd_clear_row_at $((top_row + 9))
    motd_clear_row_at $((top_row + 10))
  fi
  motd_move_cursor_to "${MOTD_MENU_CURSOR_ROW:-$((top_row + 11))}" 1
}

motd_update_launcher_menu_selection() {
  local old_selected="$1"
  local new_selected="$2"

  [ "$old_selected" = "$new_selected" ] && return 0

  motd_render_launcher_menu_items "$new_selected"
  motd_move_cursor_to "${MOTD_MENU_CURSOR_ROW:-1}" 1
}

motd_move_cursor_below_menu() {
  return 0
}

motd_flush_tty_input() {
  local _ch=''
  local _i=0
  local _idle=0
  while [ "$_i" -lt 4096 ] 2>/dev/null; do
    if IFS= read -rsn1 -t 0.01 _ch <"$TTY_DEV"; then
      _i=$(( _i + 1 ))
      _idle=0
      continue
    fi
    _idle=$(( _idle + 1 ))
    [ "$_idle" -ge 2 ] && break
  done
}

motd_read_launcher_key() {
  local key='' seq=''

  IFS= read -rsn1 key <"$TTY_DEV" || return 1

  case "$key" in
    '')
      printf 'enter\n'
      return 0
      ;;
    $'\n'|$'\r')
      printf 'enter\n'
      return 0
      ;;
    $'\033')
      if IFS= read -rsn1 -t 0.02 seq <"$TTY_DEV"; then
        if [ "$seq" = "[" ] || [ "$seq" = "O" ]; then
          if IFS= read -rsn1 -t 0.02 seq <"$TTY_DEV"; then
            case "$seq" in
              A) printf 'up\n'; return 0 ;;
              B) printf 'down\n'; return 0 ;;
            esac
          fi
        fi
      fi
      printf 'esc\n'
      return 0
      ;;
    [1-4])
      printf 'digit:%s\n' "$key"
      return 0
      ;;
    k|K)
      printf 'up\n'
      return 0
      ;;
    j|J)
      printf 'down\n'
      return 0
      ;;
    q|Q)
      printf 'esc\n'
      return 0
      ;;
  esac

  printf 'noop\n'
  return 0
}

motd_show_launcher_screen() {
  local selected="${1:-1}"
  motd_render_launcher_frame final
  motd_render_launcher_menu_static "$selected"
}

motd_run_launcher_menu() {
  local selected="${AITERMUX_MOTD_DEFAULT_MENU_INDEX:-1}"
  local action=''
  local choice=''
  local last_size=''
  local current_size=''
  local next_selected=''
  local redraw_needed=1
  local input_armed=0

  case "$selected" in
    1|2|3|4) ;;
    *) selected=1 ;;
  esac

  MOTD_KEEP_SCREEN=1
  motd_refresh_launcher_env || true
  wait_term_size_stable >/dev/null 2>&1 || true
  motd_flush_tty_input || true
  tput civis >/dev/null 2>&1 || true

  while :; do
    current_size="$(get_term_size_safe)"
    if [ "$redraw_needed" = "1" ] || [ "$current_size" != "$last_size" ]; then
      motd_show_launcher_screen "$selected"
      last_size="$current_size"
      redraw_needed=0
      if [ "$input_armed" = "0" ]; then
        # 动画结束后，Termux/IME 可能还有晚到的按键，先隔离一个极短窗口再接管输入。
        motd_flush_tty_input || true
        sleep 0.08
        motd_flush_tty_input || true
        input_armed=1
        continue
      fi
    fi
    action="$(motd_read_launcher_key || true)"
    next_selected="$selected"

    case "$action" in
      up)
        if [ "$selected" -le 1 ] 2>/dev/null; then
          next_selected=4
        else
          next_selected=$((selected - 1))
        fi
        ;;
      down)
        if [ "$selected" -ge 4 ] 2>/dev/null; then
          next_selected=1
        else
          next_selected=$((selected + 1))
        fi
        ;;
      digit:1|digit:2|digit:3|digit:4)
        next_selected="${action#digit:}"
        ;;
      enter)
        choice="$selected"
        break
        ;;
      esc)
        motd_launcher_log 'menu_shell source=esc'
        tput cnorm >/dev/null 2>&1 || true
        return 0
        ;;
      *)
        ;;
    esac

    if [ "$next_selected" != "$selected" ]; then
      motd_update_launcher_menu_selection "$selected" "$next_selected"
      selected="$next_selected"
    fi
  done

  motd_launcher_log "menu_enter choice=$choice"
  motd_move_cursor_below_menu
  tty_printf '\n'
  tput cnorm >/dev/null 2>&1 || true
  motd_launch_choice "$choice" || true
  return 0
}

motd_prompt_after_anim() {
  motd_run_launcher_menu || true
  return 0
}

META_REASON="done"
write_meta_once || true
motd_prompt_after_anim || true

exit 0
