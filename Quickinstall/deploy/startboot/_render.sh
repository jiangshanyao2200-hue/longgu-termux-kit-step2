#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

aitermux_anim_get_lines() {
  local fallback="${1:-24}"
  local n=""
  n="$(stty size </dev/tty 2>/dev/null || true)"
  n="${n%% *}"
  if [[ "${n:-}" =~ ^[0-9]+$ ]] && (( n > 0 )); then
    echo "$n"
    return 0
  fi
  n="$(tput lines 2>/dev/null || true)"
  if [[ "${n:-}" =~ ^[0-9]+$ ]] && (( n > 0 )); then
    echo "$n"
    return 0
  fi
  echo "$fallback"
}

aitermux_anim_print_frame() {
  local frame="${1-}"
  local virtual_h="${2:-24}"
  local cur_h
  cur_h="$(aitermux_anim_get_lines "$virtual_h")"
  if [[ ! "$cur_h" =~ ^[0-9]+$ ]] || (( cur_h <= 0 )); then
    cur_h="$virtual_h"
  fi
  mapfile -t -n "$cur_h" _aitermux_lines <<<"$frame"
  (IFS=$'\n'; printf '%s' "${_aitermux_lines[*]}")
  printf '\033[0m\033[J'
}
