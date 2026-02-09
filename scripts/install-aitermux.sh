#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
AITERMUX_HOME="${AITERMUX_HOME:-$HOME/AItermux}"
SYSTEM_DST="$AITERMUX_HOME/system"
BACKUP_ROOT="$AITERMUX_HOME/backups"
STAMP="$(date +%Y%m%d-%H%M%S)"
BACKUP_DIR="$BACKUP_ROOT/upgrade-$STAMP"

DRY_RUN=0
SKIP_PREVIEW=0

usage() {
  cat <<'EOF'
AITermux 一键覆盖部署（Termux）

用法：
  bash scripts/install-aitermux.sh [--dry-run] [--skip-preview]

参数：
  --dry-run       仅打印操作，不写入文件
  --skip-preview  安装完成后不预览随机启动动画

环境变量：
  AITERMUX_HOME   默认：$HOME/AItermux
EOF
}

log() {
  echo "[install-aitermux] $*"
}

run_cmd() {
  if ((DRY_RUN)); then
    printf '+ '
    printf '%q ' "$@"
    printf '\n'
    return 0
  fi
  "$@"
}

backup_file() {
  local target="$1"
  [ -e "$target" ] || return 0
  local rel="${target#/}"
  local dest="$BACKUP_DIR/$rel"
  run_cmd mkdir -p "$(dirname "$dest")"
  run_cmd cp -a "$target" "$dest"
}

install_file() {
  local src="$1"
  local dst="$2"
  local mode="$3"
  backup_file "$dst"
  run_cmd mkdir -p "$(dirname "$dst")"
  run_cmd install -m "$mode" "$src" "$dst"
}

sync_system_tree() {
  local src_real=""
  local dst_real=""

  src_real="$(cd "$ROOT" && pwd -P)"
  if [ -d "$SYSTEM_DST" ]; then
    dst_real="$(cd "$SYSTEM_DST" && pwd -P)"
  fi

  if [ -n "$dst_real" ] && [ "$src_real" = "$dst_real" ]; then
    log "检测到源目录与目标目录一致，跳过代码同步。"
    return 0
  fi

  run_cmd mkdir -p "$SYSTEM_DST"

  if command -v rsync >/dev/null 2>&1; then
    local -a cmd=(
      rsync -a --delete
      --exclude .git
      --exclude target
      --exclude log
      --exclude memory
      --exclude deploy
      --exclude 'config/*.json'
      "$ROOT/" "$SYSTEM_DST/"
    )
    run_cmd "${cmd[@]}"
    return 0
  fi

  if ((DRY_RUN)); then
    log "未检测到 rsync，将使用 tar 同步（dry-run 不执行）。"
    return 0
  fi

  (
    cd "$ROOT"
    tar \
      --exclude=.git \
      --exclude=target \
      --exclude=log \
      --exclude=memory \
      --exclude=deploy \
      --exclude='config/*.json' \
      -cf - .
  ) | (
    cd "$SYSTEM_DST"
    tar -xf -
  )
}

ensure_zshrc_block() {
  local zshrc="$HOME/.zshrc"
  local marker_begin="# >>> AITERMUX AUTOSTART >>>"
  local marker_end="# <<< AITERMUX AUTOSTART <<<"

  if ((DRY_RUN)); then
    log "将更新 $zshrc 的 AITERMUX AUTOSTART 段。"
    return 0
  fi

  mkdir -p "$(dirname "$zshrc")"
  [ -f "$zshrc" ] || touch "$zshrc"
  backup_file "$zshrc"

  local tmp="${TMPDIR:-/data/data/com.termux/files/usr/tmp}/zshrc.aitermux.$$"
  awk -v b="$marker_begin" -v e="$marker_end" '
    BEGIN { skip=0 }
    $0==b { skip=1; next }
    $0==e { skip=0; next }
    !skip { print }
  ' "$zshrc" > "$tmp"

  {
    cat "$tmp"
    printf '\n%s\n' "$marker_begin"
    cat "$ROOT/deploy/aitermux/zshrc.autostart.zsh"
    printf '%s\n' "$marker_end"
  } > "$tmp.new"

  mv "$tmp.new" "$zshrc"
  rm -f "$tmp"
}

for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    --skip-preview) SKIP_PREVIEW=1 ;;
    --help|-h) usage; exit 0 ;;
    *)
      echo "未知参数：$arg" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [ ! -d "$PREFIX_DIR/bin" ] || [ ! -d "$PREFIX_DIR/etc" ]; then
  echo "[install-aitermux] 未检测到有效的 Termux PREFIX：$PREFIX_DIR" >&2
  exit 1
fi

log "备份目录：$BACKUP_DIR"
run_cmd mkdir -p "$BACKUP_DIR"
run_cmd mkdir -p "$AITERMUX_HOME" "$AITERMUX_HOME/bin" "$AITERMUX_HOME/startboot" "$HOME/.termux"

log "同步 ProjectYing 代码到 $SYSTEM_DST"
sync_system_tree

log "覆盖 Termux 启动链路文件"
install_file "$ROOT/deploy/termux/motd.sh" "$HOME/.termux/motd.sh" 0755
install_file "$ROOT/deploy/termux/login.sh" "$PREFIX_DIR/bin/login" 0755
install_file "$ROOT/deploy/termux/etc-motd.sh" "$PREFIX_DIR/etc/motd.sh" 0755
install_file "$ROOT/deploy/termux/termux-login.sh" "$PREFIX_DIR/etc/termux-login.sh" 0755
install_file "$ROOT/deploy/termux/tx11start.sh" "$PREFIX_DIR/bin/tx11start" 0755

log "安装 AITermux 启动器"
install_file "$ROOT/deploy/aitermux/aitermux" "$AITERMUX_HOME/bin/aitermux" 0755

log "安装随机开屏动画"
for src in "$ROOT"/deploy/startboot/*; do
  [ -f "$src" ] || continue
  base="$(basename "$src")"
  mode=0755
  if [ "$base" = "_render.sh" ]; then
    mode=0644
  fi
  install_file "$src" "$AITERMUX_HOME/startboot/$base" "$mode"
done

log "写入 zsh 自动启动段"
ensure_zshrc_block

if ((SKIP_PREVIEW == 0)); then
  if [ -t 1 ]; then
    log "预览一次随机开屏动画"
    if ((DRY_RUN)); then
      printf '+ %q %q\n' "$HOME/.termux/motd.sh" ""
    else
      AITERMUX_MOTD_DEBUG=1 "$HOME/.termux/motd.sh" || true
    fi
  else
    log "当前无 TTY，跳过动画预览。"
  fi
fi

log "完成。"
log "下次新开 Termux 会话将自动进入 AITermux。"
log "如需回滚，请从 $BACKUP_DIR 取回被覆盖文件。"
