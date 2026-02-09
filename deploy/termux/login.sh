#!/data/data/com.termux/files/usr/bin/sh

unset TERMUX_HUSHLOGIN

if [ -G ~/.termux/shell ]; then
  export SHELL="`realpath ~/.termux/shell`"
else
  for file in /data/data/com.termux/files/usr/bin/bash /data/data/com.termux/files/usr/bin/sh /system/bin/sh; do
    if [ -x "$file" ]; then
      export SHELL="$file"
      break
    fi
  done
fi

if [ -f "/data/data/com.termux/files/usr/lib/libtermux-exec-ld-preload.so" ]; then
  export LD_PRELOAD="/data/data/com.termux/files/usr/lib/libtermux-exec-ld-preload.so"
  "$SHELL" -c "coreutils --coreutils-prog=true" >/dev/null 2>&1 || unset LD_PRELOAD
elif [ -f "/data/data/com.termux/files/usr/lib/libtermux-exec.so" ]; then
  export LD_PRELOAD="/data/data/com.termux/files/usr/lib/libtermux-exec.so"
  "$SHELL" -c "coreutils --coreutils-prog=true" >/dev/null 2>&1 || unset LD_PRELOAD
fi

if tty >/dev/null 2>&1 && [ ! -f "$HOME/.hushlogin" ]; then
  if [ -f "$HOME/.termux/motd.sh" ]; then
    [ ! -x "$HOME/.termux/motd.sh" ] && chmod u+x "$HOME/.termux/motd.sh"
    "$HOME/.termux/motd.sh" || true
  fi
fi

if [ -n "$TERM" ]; then
  exec "$SHELL" -l "$@"
else
  exec "$SHELL" "$@"
fi
