首先记录教程
1 安装termux，termuxapi，termuxtx11，termuxstyling
2 安装git
`pkg update -y && pkg install -y git`
3 clone 脚本
`git clone https://github.com/jiangshanyao2200-hue/longgu-termux-kit-step1`
4 进入目录
`cd ~/longgu-termux-kit-step1`
5 运行脚本
`bash stage1-prereqs.sh`

Termux Desktop安装器选项；

1 安装类型 选2 Generic With Hardware 

2 桌面环境 选1 xfce

3 桌面风格 选5 modern style（现代风格）

---

## 启动链路（调用关系 / 文件调用问题）
Termux 任意进入（含新建 tab/session）时，开屏动画的主链路是：
1 `/data/data/com.termux/files/usr/bin/login`
2 `login` 内部先设置 `LD_PRELOAD(termux-exec)`（Termux 常规行为；用于整体路径兼容）
3 `login` 调用 `~/.termux/motd.sh`
4 `~/.termux/motd.sh` 随机执行 `~/AItermux/startboot/*.sh`（带 `timeout -f/--foreground`（若支持）超时兜底；并通过 `bash script.sh` 执行，避免 shebang 依赖）
5 动画结束清屏 → `login` `exec $SHELL -l`（进入 zshrc）

说明：
- `/data/data/com.termux/files/usr/etc/motd.sh` 不在主链路中；它现在只是“兼容 wrapper”，若第三方脚本仍调用它，会转到 `~/.termux/motd.sh`。
- `/data/data/com.termux/files/usr/etc/termux-login.sh` 为 termux-tools 的 conffile，保留但不走主链路（同样委托到 `~/.termux/motd.sh`，避免 missing/self-check 报错）。

---

## 当前改动清单（用于后续打包脚本覆盖同步）
目标：公开发布 AITermux 时，除 `~/AItermux/` 目录外，需要在用户侧“覆盖/同步”的改动文件清单（确保启动链路一致、避免回退到 Termux 默认逻辑）。

### A. 必须覆盖（AITermux 运行依赖）
- `~/.termux/motd.sh`：唯一开屏入口（随机播动画、超时兜底、日志、清屏、容错）
- `~/.zshrc`：AITermux TUI 自启动（含逃生开关 `AITERMUX_DISABLE=1`、递归保护 `AITERMUX_STARTED=1`）
- `$PREFIX/bin/login`：强制走 `~/.termux/motd.sh`，并避免多套 motd/hook 二次输出覆盖
- `$PREFIX/etc/motd.sh`：兼容 wrapper（委托执行 `~/.termux/motd.sh`，防第三方直接调用它时覆盖开屏）
- `$PREFIX/etc/termux-login.sh`：保留 termux-tools 的 login hook 文件，但委托执行 `~/.termux/motd.sh`（避免 missing/self-check 报错）
- `$PREFIX/bin/tx11start`：图形/桌面启动命令（`/xfce` 依赖）；此文件不属于 dpkg 包，需随项目脚本覆盖

### B. 可选覆盖（体验/偏好）
- `~/.termux/termux.properties`：键位/软键盘等体验优化（不影响 AITermux 正常运行）

说明：
- `~/AItermux/` 目录内的所有文件属于项目仓库内容，不在此“系统覆盖清单”里重复列出。

---

## 覆盖同步脚本（草案）
未来做“一键同步/覆盖”脚本时建议：
1 先对每个被覆盖文件做备份（例如加时间戳 `.bak-YYYYMMDD-HHMMSS`）
2 用户态文件（`~/.termux/*`、`~/AItermux/*`）可直接覆盖/更新
3 `$PREFIX` 内文件（`$PREFIX/bin/login`、`$PREFIX/bin/tx11start`、`$PREFIX/etc/motd.sh`、`$PREFIX/etc/termux-login.sh`）一般不需要 root：只要脚本在 Termux 内运行即可写入（这些文件属于 Termux App 用户）
   - 只有在“从 Termux 外部执行安装器（如 adb shell）”或“权限异常/只读”时，才可能需要 `su` 或其它方式进入 Termux 用户上下文
4 永远保证：即使动画脚本坏了，也不会阻断进入 shell（靠 `~/.termux/motd.sh` 的容错/超时/日志兜底）
