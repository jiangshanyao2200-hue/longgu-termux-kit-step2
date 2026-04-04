#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

QUICK_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PREFIX_DIR="${PREFIX:-/data/data/com.termux/files/usr}"
AITERMUX_HOME="${AITERMUX_HOME:-$HOME/AItermux}"
PROJECTYING_REPO="${AITERMUX_PROJECTYING_REPO:-https://github.com/jiangshanyao2200-hue/projectying.git}"
BACKUP_ROOT="$AITERMUX_HOME/backups"
STAMP="$(date +%Y%m%d-%H%M%S)"
BACKUP_DIR="$BACKUP_ROOT/upgrade-$STAMP"
DRY_RUN=0
SKIP_PREVIEW=0
QUIET=0

usage() {
  cat <<'EOF'
AITermux 一键覆盖部署（Termux）

用法：
  bash ~/AItermux/install.sh [--dry-run] [--skip-preview] [--quiet]
  # 或者：cd ~/AItermux/Quickinstall && bash install.sh [args]

参数：
  --dry-run       仅打印操作，不写入文件
  --skip-preview  安装完成后不预览随机启动动画
  --quiet         减少输出（不打印每条命令）

环境变量：
  AITERMUX_HOME            默认：$HOME/AItermux
  AITERMUX_PROJECTYING_REPO 默认：https://github.com/jiangshanyao2200-hue/projectying.git
EOF
}

log() {
  printf '[%s] [install-aitermux] %s\n' "$(date '+%F %T' 2>/dev/null || echo unknown)" "$*"
}

run_cmd() {
  if (( QUIET == 0 )); then
    printf '+ '
    printf '%q ' "$@"
    printf '\n'
  fi
  (( DRY_RUN )) && return 0
  "$@"
}

backup_file() {
  local target="$1"
  [ -e "$target" ] || return 0
  local rel="${target#/}"
  local dest="$BACKUP_DIR/$rel"
  run_cmd mkdir -p "$(dirname "$dest")"
  log "备份：$target -> $dest"
  run_cmd cp -a "$target" "$dest"
}

install_file() {
  local src="$1"
  local dst="$2"
  local mode="$3"
  local sha_src="" sha_dst=""
  backup_file "$dst"
  run_cmd mkdir -p "$(dirname "$dst")"
  log "写入：$dst (mode=$mode) <- $src"
  run_cmd install -m "$mode" "$src" "$dst"
  if (( DRY_RUN == 0 )); then
    sha_src="$(sha256sum "$src" 2>/dev/null | awk '{print $1}' || true)"
    sha_dst="$(sha256sum "$dst" 2>/dev/null | awk '{print $1}' || true)"
    printf '%s\t%s\t%s\t%s\n' "$mode" "$dst" "${sha_dst:-}" "${sha_src:-}" >>"$MANIFEST_FILE" 2>/dev/null || true
  fi
}

ensure_termux_shell() {
  local zsh_bin="$PREFIX_DIR/bin/zsh"
  local shell_link="$HOME/.termux/shell"

  if [ ! -x "$zsh_bin" ]; then
    echo "[install-aitermux] 未检测到 zsh：$zsh_bin" >&2
    echo "[install-aitermux] 请先执行：pkg install zsh" >&2
    exit 1
  fi

  backup_file "$shell_link"
  run_cmd mkdir -p "$HOME/.termux"
  log "设置登录 shell：$shell_link -> $zsh_bin"
  run_cmd ln -sfn "$zsh_bin" "$shell_link"
  printf 'shell\t%s\t%s\n' "$shell_link" "$zsh_bin" >>"$MANIFEST_FILE" 2>/dev/null || true
}

ensure_zshrc_block() {
  local zshrc="$HOME/.zshrc"
  local marker_begin="# >>> AITERMUX AUTOSTART >>>"
  local marker_end="# <<< AITERMUX AUTOSTART <<<"

  if (( DRY_RUN )); then
    log "将更新 $zshrc 的 AITERMUX AUTOSTART 段。"
    return 0
  fi

  mkdir -p "$(dirname "$zshrc")"
  [ -f "$zshrc" ] || touch "$zshrc"
  backup_file "$zshrc"
  log "写入：$zshrc（注入 AITERMUX AUTOSTART 段）"

  local tmp="${TMPDIR:-/data/data/com.termux/files/usr/tmp}/zshrc.aitermux.$$"
  awk -v b="$marker_begin" -v e="$marker_end" '
    BEGIN { skip=0 }
    $0==b { skip=1; next }
    $0==e { skip=0; next }
    !skip { print }
  ' "$zshrc" >"$tmp"

  local tmp2="${tmp}.legacy"
  awk '
    BEGIN { in_legacy=0; seen_unset=0 }
    {
      if (!in_legacy && $0 ~ /^# CMD 模式：从 AItermux 切到原生 zsh/) {
        in_legacy=1
        next
      }
      if (in_legacy) {
        if ($0 ~ /^\s*unset _aitermux_rc _aitermux_tty_id _aitermux_motd_runfile\s*$/) {
          seen_unset=1
          next
        }
        if (seen_unset && $0 ~ /^\s*fi\s*$/) {
          in_legacy=0
          seen_unset=0
          next
        }
        next
      }
      print
    }
  ' "$tmp" >"$tmp2"

  {
    cat "$tmp2"
    printf '\n%s\n' "$marker_begin"
    cat "$QUICK_ROOT/deploy/aitermux/zshrc.autostart.zsh"
    printf '%s\n' "$marker_end"
  } >"$tmp.new"

  mv "$tmp.new" "$zshrc"
  rm -f "$tmp" "$tmp2"
  printf 'zshrc\t%s\n' "$zshrc" >>"$MANIFEST_FILE" 2>/dev/null || true
}

install_startboot_pool() {
  local src base mode

  log "安装开屏动画脚本池"
  for src in "$QUICK_ROOT"/deploy/startboot/*; do
    [ -f "$src" ] || continue
    base="$(basename "$src")"
    mode=0644
    case "$base" in
      *.sh) mode=0755 ;;
    esac
    install_file "$src" "$AITERMUX_HOME/startboot/$base" "$mode"
  done
}

for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    --skip-preview) SKIP_PREVIEW=1 ;;
    --quiet) QUIET=1 ;;
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

if [ ! -f "$QUICK_ROOT/deploy/termux/motd.sh" ]; then
  echo "[install-aitermux] 部署模板缺失：$QUICK_ROOT/deploy/termux/motd.sh" >&2
  exit 1
fi

log "备份目录：$BACKUP_DIR"
run_cmd mkdir -p "$BACKUP_DIR"

MANIFEST_FILE="$BACKUP_DIR/manifest.tsv"
if (( DRY_RUN == 0 )); then
  : >"$MANIFEST_FILE" 2>/dev/null || true
  printf 'mode\tpath\tsha_dst\tsha_src\n' >>"$MANIFEST_FILE" 2>/dev/null || true

  if command -v tee >/dev/null 2>&1; then
    exec > >(tee -a "$BACKUP_DIR/install.log") 2>&1
    log "安装日志：$BACKUP_DIR/install.log"
  fi
fi

trap 'log "错误：安装中断（line=$LINENO）。可用备份目录回滚：$BACKUP_DIR"' ERR

run_cmd mkdir -p "$AITERMUX_HOME" "$AITERMUX_HOME/bin" "$AITERMUX_HOME/startboot" "$HOME/.termux"

log "校验 zsh 登录链"
ensure_termux_shell

log "覆盖 Termux 启动链路文件"
install_file "$QUICK_ROOT/deploy/termux/motd.sh" "$HOME/.termux/motd.sh" 0755
install_file "$QUICK_ROOT/deploy/termux/termux.properties" "$HOME/.termux/termux.properties" 0644
install_file "$QUICK_ROOT/deploy/termux/login.sh" "$PREFIX_DIR/bin/login" 0755
install_file "$QUICK_ROOT/deploy/termux/etc-motd.sh" "$PREFIX_DIR/etc/motd.sh" 0755
install_file "$QUICK_ROOT/deploy/termux/termux-login.sh" "$PREFIX_DIR/etc/termux-login.sh" 0755
install_file "$QUICK_ROOT/deploy/termux/tx11start.sh" "$PREFIX_DIR/bin/tx11start" 0755

log "安装 AITermux 启动器"
install_file "$QUICK_ROOT/deploy/aitermux/aitermux" "$AITERMUX_HOME/bin/aitermux" 0755
install_file "$QUICK_ROOT/deploy/aitermux/bootstrap.sh" "$AITERMUX_HOME/bin/aitermux-bootstrap" 0755

install_startboot_pool

log "写入 zsh 自动启动段"
ensure_zshrc_block

if (( DRY_RUN == 0 )); then
  log "生成回滚脚本：$BACKUP_DIR/rollback.sh"
  cat >"$BACKUP_DIR/rollback.sh" <<'EOF'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

BACKUP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
cd "$BACKUP_DIR"

echo "[rollback] from: $BACKUP_DIR"
find . -type f -print0 | while IFS= read -r -d '' f; do
  target="/${f#./}"
  mkdir -p "$(dirname "$target")"
  cp -a "$f" "$target"
  echo "[rollback] restore: $target"
done
echo "[rollback] done."
EOF
  chmod 0755 "$BACKUP_DIR/rollback.sh" 2>/dev/null || true
fi

if (( SKIP_PREVIEW == 0 )); then
  if [ -t 1 ]; then
    log "预览一次随机开屏动画"
    if (( DRY_RUN )); then
      printf '+ %q %q\n' "$HOME/.termux/motd.sh" ""
    else
      AITERMUX_MOTD_DEBUG=1 "$HOME/.termux/motd.sh" || true
    fi
  else
    log "当前无 TTY，跳过动画预览。"
  fi
fi

log "完成。"
log "projectying 仓库地址：$PROJECTYING_REPO"
log "本次只部署 AITermux 启动链/样式层，不主动安装 projectying/codex/gemini。"
log "下次新开 Termux 会话时，如运行时缺失，将由启动链自动补装。"
log "下次新开 Termux 会话将自动进入 AITermux。"
log "如需回滚，请从 $BACKUP_DIR 取回被覆盖文件。"
