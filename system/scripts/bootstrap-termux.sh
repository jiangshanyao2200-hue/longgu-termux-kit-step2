#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if ! command -v pkg >/dev/null 2>&1; then
  echo "[bootstrap-termux] 未检测到 pkg（这看起来不像 Termux 环境）。" >&2
  exit 1
fi

declare -a need_pkgs=()
declare -A seen_pkg=()

need_pkg() {
  local pkg="$1"
  if [[ -z "${seen_pkg[$pkg]:-}" ]]; then
    need_pkgs+=("$pkg")
    seen_pkg[$pkg]=1
  fi
}

need_cmd_pkg() {
  local cmd="$1"
  local pkg="$2"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    need_pkg "$pkg"
  fi
}

# Rust 构建 + rusqlite(bundled) 需要 C 编译器
need_cmd_pkg cargo rust
need_cmd_pkg rustc rust
need_cmd_pkg clang clang

# 运行时/工具链常用
need_cmd_pkg git git
need_cmd_pkg timeout coreutils
need_cmd_pkg adb android-tools
need_cmd_pkg rg ripgrep

# Termux:API（用于 wake-lock 与 termux_api 工具）
need_cmd_pkg termux-wake-lock termux-api
need_cmd_pkg termux-wake-unlock termux-api
need_cmd_pkg termux-battery-status termux-api

if ((${#need_pkgs[@]})); then
  echo "[bootstrap-termux] 将自动安装缺失依赖：${need_pkgs[*]}"
  pkg update -y
  pkg install -y "${need_pkgs[@]}"
else
  echo "[bootstrap-termux] 依赖已齐全"
fi

termux_api_check="${PROJECTYING_TERMUX_API_CHECK:-1}"
termux_api_timeout="${PROJECTYING_TERMUX_API_TIMEOUT:-2}"
termux_api_fix="${PROJECTYING_TERMUX_API_FIX:-1}"
pkg_timeout="${PROJECTYING_PKG_TIMEOUT:-30}"
termux_api_url="https://github.com/termux/termux-api"

termux_api_hint() {
  cat >&2 <<EOF
[bootstrap-termux] 需要安装 Termux:API（手机端应用）并确保权限已授予。
[bootstrap-termux] Termux:API Git: $termux_api_url
EOF
}

try_install_termux_api_pkg() {
  if [[ "$termux_api_fix" != "1" ]]; then
    return 0
  fi
  echo "[bootstrap-termux] 尝试执行：pkg update -y && pkg install -y termux-api"
  if command -v timeout >/dev/null 2>&1; then
    timeout "$pkg_timeout" pkg update -y || true
    timeout "$pkg_timeout" pkg install -y termux-api || true
  else
    pkg update -y || true
    pkg install -y termux-api || true
  fi
}

if [[ "$termux_api_check" == "1" ]] && command -v termux-battery-status >/dev/null 2>&1; then
  if command -v timeout >/dev/null 2>&1; then
    rc=0
    timeout "$termux_api_timeout" termux-battery-status >/dev/null 2>&1 || rc=$?
    if (( rc != 0 )); then
      if (( rc == 124 )); then
        echo "[bootstrap-termux] termux-api 检测超时（可能卡住），将提示并尝试修复后跳过。" >&2
        termux_api_hint
        try_install_termux_api_pkg
      else
        termux_api_hint
        try_install_termux_api_pkg
      fi
      echo "[bootstrap-termux] 已跳过 termux-api 检测，继续启动。" >&2
    fi
  else
    if ! termux-battery-status >/dev/null 2>&1; then
      termux_api_hint
      try_install_termux_api_pkg
    fi
  fi
fi

echo "[bootstrap-termux] ok"
