#!/data/data/com.termux/files/usr/bin/env bash
set -euo pipefail

AITERMUX_HOME="${AITERMUX_HOME:-$HOME/AItermux}"
PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
PROJECTYING_DIR="$AITERMUX_HOME/projectying"
PROJECTYING_REPO="${AITERMUX_PROJECTYING_REPO:-https://github.com/jiangshanyao2200-hue/projectying.git}"
STATE_DIR="$AITERMUX_HOME/.state/bootstrap"
LOG_DIR="$AITERMUX_HOME/logs"
STARTUP_LOG="$LOG_DIR/startup.log"
RETRY_SECS="${AITERMUX_BOOTSTRAP_RETRY_SECS:-600}"
QUIET=0
FORCE=0

declare -a COMPONENTS=()

usage() {
  cat <<'EOF'
AITermux bootstrap

用法：
  aitermux-bootstrap [--quiet] [--force] [--component projectying|codex|gemini]

说明：
  默认会检查并补装 projectying、codex、gemini。
  失败会写入 ~/AItermux/logs/startup.log，并做短暂退避，避免每次登录都重复阻塞。
EOF
}

timestamp_utc() {
  date -u '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date '+%F %T' 2>/dev/null || echo unknown
}

log() {
  local msg="$*"
  mkdir -p "$LOG_DIR" "$STATE_DIR" >/dev/null 2>&1 || true
  printf '%s bootstrap %s\n' "$(timestamp_utc)" "$msg" >>"$STARTUP_LOG" 2>/dev/null || true
  if (( QUIET == 0 )); then
    printf '[aitermux-bootstrap] %s\n' "$msg" >&2
  fi
}

state_file_for() {
  printf '%s/%s.state\n' "$STATE_DIR" "$1"
}

state_get() {
  local file="$1"
  local key="$2"
  sed -n "s/^${key}=//p" "$file" 2>/dev/null | head -n 1
}

state_set() {
  local component="$1"
  local status="$2"
  local reason="${3:-}"
  local file tmp

  mkdir -p "$STATE_DIR" >/dev/null 2>&1 || true
  file="$(state_file_for "$component")"
  tmp="${file}.tmp.$$.$RANDOM"
  {
    printf 'status=%s\n' "$status"
    printf 'ts=%s\n' "$(date +%s 2>/dev/null || echo 0)"
    printf 'updated_at=%s\n' "$(timestamp_utc)"
    printf 'reason=%s\n' "$reason"
  } >"$tmp"
  mv -f "$tmp" "$file"
}

skip_due_to_backoff() {
  local component="$1"
  local file status ts now

  (( FORCE == 0 )) || return 1
  [[ "$RETRY_SECS" =~ ^[0-9]+$ ]] || return 1
  (( RETRY_SECS > 0 )) || return 1

  file="$(state_file_for "$component")"
  [[ -f "$file" ]] || return 1

  status="$(state_get "$file" status)"
  ts="$(state_get "$file" ts)"
  now="$(date +%s 2>/dev/null || echo 0)"

  [[ "$status" == "fail" ]] || return 1
  [[ "$ts" =~ ^[0-9]+$ ]] || return 1
  [[ "$now" =~ ^[0-9]+$ ]] || return 1
  (( now > ts )) || return 1

  if (( now - ts < RETRY_SECS )); then
    log "skip component=${component} reason=recent-failure retry_after=${RETRY_SECS}s"
    return 0
  fi

  return 1
}

append_line_if_missing() {
  local file="$1"
  local line="$2"

  mkdir -p "$(dirname "$file")" >/dev/null 2>&1 || true
  touch "$file"
  grep -qxF "$line" "$file" 2>/dev/null || printf '%s\n' "$line" >>"$file"
}

write_file_atomically() {
  local target="$1"
  local mode="$2"
  local tmp

  if [ -L "$target" ]; then
    target="$(readlink "$target" 2>/dev/null || printf '%s' "$target")"
  fi
  mkdir -p "$(dirname "$target")" >/dev/null 2>&1 || true
  tmp="${target}.tmp.$$.$RANDOM"
  cat >"$tmp"
  chmod "$mode" "$tmp" 2>/dev/null || true
  mv -f "$tmp" "$target"
}

ensure_pkg() {
  local cmd_name="$1"
  local pkg_name="$2"

  if command -v "$cmd_name" >/dev/null 2>&1; then
    return 0
  fi

  if ! command -v pkg >/dev/null 2>&1; then
    log "missing pkg while installing package=${pkg_name}"
    return 1
  fi

  log "pkg install package=${pkg_name}"
  pkg install -y "$pkg_name"
}

ensure_node_stack() {
  ensure_pkg node nodejs-lts || return 1
  command -v npm >/dev/null 2>&1 || return 1
}

ensure_projectying_build_stack() {
  ensure_pkg git git || return 1
  ensure_pkg cargo rust || return 1
  ensure_pkg clang clang || return 1
  ensure_pkg pkg-config pkg-config || return 1
  ensure_pkg make make || return 1
}

ensure_codex_stack() {
  ensure_node_stack || return 1
  ensure_pkg proot proot || return 1
}

write_codex_wrapper() {
  write_file_atomically "$HOME/.local/bin/codex" 0755 <<'EOF'
#!/data/data/com.termux/files/usr/bin/env bash

set -e

PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
REAL_NODE="${PREFIX_DIR}/bin/node"
REAL_CODEX="${PREFIX_DIR}/lib/node_modules/@openai/codex/bin/codex.js"
REAL_BWRAP="${PREFIX_DIR}/bin/bwrap"
CODEX_CERT_FILE_DEFAULT="${PREFIX_DIR}/etc/tls/cert.pem"
CODEX_PROXY_URL_DEFAULT="http://127.0.0.1:7890"
CODEX_ROUTE_MODE_FILE="${HOME}/.codex/route_mode"

read_route_mode() {
  if [ -n "${CODEX_ROUTE_MODE:-}" ]; then
    case "${CODEX_ROUTE_MODE}" in
      official|relay)
        printf '%s\n' "${CODEX_ROUTE_MODE}"
        return 0
        ;;
    esac
  fi

  if [ -f "${CODEX_ROUTE_MODE_FILE}" ]; then
    mode="$(tr -d '[:space:]' < "${CODEX_ROUTE_MODE_FILE}")"
  else
    mode="relay"
  fi

  case "${mode}" in
    official|relay) printf '%s\n' "${mode}" ;;
    *) printf 'relay\n' ;;
  esac
}

exec_canonical() {
  mode="$(read_route_mode)"
  if [ "${mode}" = "official" ]; then
    exec env \
      SSL_CERT_FILE="${SSL_CERT_FILE-${CODEX_CERT_FILE_DEFAULT}}" \
      HTTP_PROXY="${HTTP_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
      HTTPS_PROXY="${HTTPS_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
      ALL_PROXY="${ALL_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
      NO_PROXY="${NO_PROXY-127.0.0.1,localhost}" \
      "$@"
  fi

  exec env \
    -u HTTP_PROXY \
    -u HTTPS_PROXY \
    -u ALL_PROXY \
    -u NO_PROXY \
    -u http_proxy \
    -u https_proxy \
    -u all_proxy \
    -u no_proxy \
    SSL_CERT_FILE="${SSL_CERT_FILE-${CODEX_CERT_FILE_DEFAULT}}" \
    "$@"
}

if [ -x /usr/bin/bwrap ] || [ ! -x "$REAL_BWRAP" ] || ! command -v proot >/dev/null 2>&1; then
  exec_canonical "$REAL_NODE" "$REAL_CODEX" "$@"
fi

exec_canonical \
  proot \
  -b "$REAL_BWRAP:/usr/bin/bwrap" \
  -w "$PWD" \
  "$REAL_NODE" "$REAL_CODEX" "$@"
EOF
}

write_codex_env() {
  write_file_atomically "$HOME/.codex/termux_env.sh" 0644 <<'EOF'
# Termux/Android wrapper for Codex with switchable transport modes.
#
# Modes:
#   relay    -> use current config/auth directly, explicitly disable proxy vars
#   official -> force local Clash proxy + cert for OpenAI official login flow

case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) export PATH="$HOME/.local/bin:$PATH" ;;
esac

CODEX_CERT_FILE_DEFAULT="/data/data/com.termux/files/usr/etc/tls/cert.pem"
CODEX_PROXY_URL_DEFAULT="http://127.0.0.1:7890"
CODEX_ROUTE_MODE_FILE="${HOME}/.codex/route_mode"

_codex_read_route_mode() {
  if [ -n "${CODEX_ROUTE_MODE:-}" ]; then
    case "${CODEX_ROUTE_MODE}" in
      official|relay)
        printf '%s\n' "${CODEX_ROUTE_MODE}"
        return 0
        ;;
    esac
  fi

  if [ -f "${CODEX_ROUTE_MODE_FILE}" ]; then
    mode="$(tr -d '[:space:]' < "${CODEX_ROUTE_MODE_FILE}")"
  else
    mode="relay"
  fi

  case "${mode}" in
    official|relay) printf '%s\n' "${mode}" ;;
    *) printf 'relay\n' ;;
  esac
}

_codex_write_route_mode() {
  mode="$1"
  case "${mode}" in
    official|relay)
      printf '%s\n' "${mode}" > "${CODEX_ROUTE_MODE_FILE}"
      ;;
    *)
      printf 'invalid codex mode: %s\n' "${mode}" >&2
      return 1
      ;;
  esac
}

codex_mode() {
  if [ $# -eq 0 ]; then
    _codex_read_route_mode
    return 0
  fi

  _codex_write_route_mode "$1" || return 1
  codex_status
}

codex_official_on() {
  codex_mode official
}

codex_official_off() {
  codex_mode relay
}

codex_status() {
  mode="$(_codex_read_route_mode)"
  printf 'codex mode: %s\n' "${mode}"
  if [ "${mode}" = "official" ]; then
    printf 'proxy: %s\n' "${CODEX_PROXY_URL_DEFAULT}"
    printf 'cert: %s\n' "${CODEX_CERT_FILE_DEFAULT}"
  else
    printf 'proxy: disabled for codex\n'
    printf 'cert: %s\n' "${CODEX_CERT_FILE_DEFAULT}"
  fi
}

_codex_run_relay() {
  env \
    -u HTTP_PROXY \
    -u HTTPS_PROXY \
    -u ALL_PROXY \
    -u NO_PROXY \
    -u http_proxy \
    -u https_proxy \
    -u all_proxy \
    -u no_proxy \
    SSL_CERT_FILE="${SSL_CERT_FILE-${CODEX_CERT_FILE_DEFAULT}}" \
    codex "$@"
}

_codex_run_official() {
  SSL_CERT_FILE="${SSL_CERT_FILE-${CODEX_CERT_FILE_DEFAULT}}" \
  HTTP_PROXY="${HTTP_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
  HTTPS_PROXY="${HTTPS_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
  ALL_PROXY="${ALL_PROXY-${CODEX_PROXY_URL_DEFAULT}}" \
  NO_PROXY="${NO_PROXY-127.0.0.1,localhost}" \
  command codex "$@"
}

codex() {
  mode="$(_codex_read_route_mode)"
  if [ "${mode}" = "official" ]; then
    _codex_run_official "$@"
  else
    _codex_run_relay "$@"
  fi
}

codex_official() {
  _codex_run_official "$@"
}

codex_relay() {
  _codex_run_relay "$@"
}

codex_raw() {
  command codex "$@"
}
EOF
}

write_gemini_wrapper() {
  write_file_atomically "$HOME/.local/bin/gemini" 0755 <<'EOF'
#!/data/data/com.termux/files/usr/bin/bash

PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
REAL_GEMINI="${PREFIX_DIR}/bin/gemini"
REAL_NODE="${PREFIX_DIR}/bin/node"
REAL_GEMINI_JS="${PREFIX_DIR}/lib/node_modules/@google/gemini-cli/dist/index.js"

if [ -x "$REAL_GEMINI" ]; then
  exec "$REAL_GEMINI" "$@"
fi

exec "$REAL_NODE" "$REAL_GEMINI_JS" "$@"
EOF
}

ensure_projectying() {
  local component="projectying"

  if [[ -f "$PROJECTYING_DIR/run.sh" ]]; then
    chmod u+x "$PROJECTYING_DIR/run.sh" >/dev/null 2>&1 || true
    state_set "$component" ok present
    return 0
  fi

  skip_due_to_backoff "$component" && return 0
  ensure_projectying_build_stack || {
    state_set "$component" fail missing-build-stack
    return 1
  }

  if [[ -e "$PROJECTYING_DIR" && ! -d "$PROJECTYING_DIR/.git" ]]; then
    log "component=${component} path-exists-but-invalid path=$PROJECTYING_DIR"
    state_set "$component" fail path-exists-but-invalid
    return 1
  fi

  if [[ ! -d "$PROJECTYING_DIR" ]]; then
    mkdir -p "$AITERMUX_HOME" >/dev/null 2>&1 || true
    log "git clone component=${component} repo=${PROJECTYING_REPO}"
    if ! git clone "$PROJECTYING_REPO" "$PROJECTYING_DIR"; then
      state_set "$component" fail git-clone-failed
      return 1
    fi
  fi

  if [[ ! -f "$PROJECTYING_DIR/run.sh" ]]; then
    log "component=${component} missing run.sh after clone"
    state_set "$component" fail missing-runsh
    return 1
  fi

  chmod u+x "$PROJECTYING_DIR/run.sh" >/dev/null 2>&1 || true
  state_set "$component" ok ready
  return 0
}

ensure_codex() {
  local component="codex"
  local codex_js="$PREFIX_DIR/lib/node_modules/@openai/codex/bin/codex.js"

  if [[ -f "$codex_js" && -x "$HOME/.local/bin/codex" ]]; then
    state_set "$component" ok present
    return 0
  fi

  skip_due_to_backoff "$component" && return 0
  ensure_codex_stack || {
    state_set "$component" fail missing-codex-stack
    return 1
  }

  append_line_if_missing "$HOME/.npmrc" "foreground-scripts=true"

  if [[ ! -f "$codex_js" ]]; then
    log "npm install component=${component} package=@openai/codex"
    if ! npm install -g @openai/codex; then
      state_set "$component" fail npm-install-failed
      return 1
    fi
  fi

  write_codex_wrapper || {
    state_set "$component" fail wrapper-write-failed
    return 1
  }
  write_codex_env || {
    state_set "$component" fail env-write-failed
    return 1
  }

  state_set "$component" ok ready
  return 0
}

ensure_gemini() {
  local component="gemini"
  local gemini_js="$PREFIX_DIR/lib/node_modules/@google/gemini-cli/dist/index.js"

  if [[ -f "$gemini_js" && ( -x "$HOME/.local/bin/gemini" || -x "$PREFIX_DIR/bin/gemini" ) ]]; then
    state_set "$component" ok present
    return 0
  fi

  skip_due_to_backoff "$component" && return 0
  ensure_node_stack || {
    state_set "$component" fail missing-node-stack
    return 1
  }

  if [[ ! -f "$gemini_js" ]]; then
    log "npm install component=${component} package=@google/gemini-cli"
    if ! npm install -g @google/gemini-cli; then
      state_set "$component" fail npm-install-failed
      return 1
    fi
  fi

  write_gemini_wrapper || {
    state_set "$component" fail wrapper-write-failed
    return 1
  }

  state_set "$component" ok ready
  return 0
}

run_component() {
  case "$1" in
    projectying) ensure_projectying ;;
    codex) ensure_codex ;;
    gemini) ensure_gemini ;;
    *)
      log "unknown component=$1"
      return 1
      ;;
  esac
}

while (($#)); do
  case "$1" in
    --quiet)
      QUIET=1
      ;;
    --force)
      FORCE=1
      ;;
    --component)
      shift
      (($#)) || {
        usage >&2
        exit 2
      }
      COMPONENTS+=("$1")
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
  shift
done

if ((${#COMPONENTS[@]} == 0)); then
  COMPONENTS=(projectying codex gemini)
fi

rc=0
for component in "${COMPONENTS[@]}"; do
  if ! run_component "$component"; then
    rc=1
  fi
done

exit "$rc"
