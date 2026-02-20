if [[ -o interactive ]] && [[ -n "${AITERMUX_CMD_SHELL-}" ]]; then
  function _aitermux_cmd_accept_line() {
    local line="${BUFFER}"
    if [[ -n "$line" ]] && [[ "${line:l}" == "/ai" ]]; then
      print
      exit 0
    fi
    zle .accept-line
  }
  zle -N accept-line _aitermux_cmd_accept_line
fi

if [[ -o interactive ]] && [[ -z "${AITERMUX_DISABLE-}" ]] && [[ -z "${AITERMUX_STARTED-}" ]]; then
  export AITERMUX_STARTED=1
  if [[ -x "$HOME/AItermux/bin/aitermux" ]]; then
    _aitermux_tty_id="$(tty 2>/dev/null | tr -c 'a-zA-Z0-9' '_' | tr -s '_' '_' | sed 's/^_*//;s/_*$//' )"
    if [[ -n "${_aitermux_tty_id:-}" ]]; then
      _aitermux_motd_runfile="$HOME/AItermux/logs/motd-running-${_aitermux_tty_id}"
      if [[ -f "$_aitermux_motd_runfile" ]]; then
        for _ in {1..40}; do
          [[ -f "$_aitermux_motd_runfile" ]] || break
          sleep 0.05
        done
      fi
    fi
    "$HOME/AItermux/bin/aitermux"
    _aitermux_rc=$?
    if [[ $_aitermux_rc -eq 200 ]]; then
      exit
    fi
  fi
  unset _aitermux_rc _aitermux_tty_id _aitermux_motd_runfile
fi
