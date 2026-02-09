#!/data/data/com.termux/files/usr/bin/sh

if tty >/dev/null 2>&1 && [ ! -f "$HOME/.hushlogin" ]; then
  if [ -f "$HOME/.termux/motd.sh" ]; then
    [ ! -x "$HOME/.termux/motd.sh" ] && chmod u+x "$HOME/.termux/motd.sh" >/dev/null 2>&1 || true
    "$HOME/.termux/motd.sh" || true
  fi
fi
