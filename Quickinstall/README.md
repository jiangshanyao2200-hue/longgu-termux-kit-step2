# AITermux Quickinstall

此目录用于放置“一键覆盖安装”相关脚本与模板。

## 用法

首次安装（从 GitHub 克隆到默认目录）：

```bash
git clone https://github.com/jiangshanyao2200-hue/longgu-termux-kit-step2.git ~/AItermux
cd ~/AItermux
bash install.sh
```

已存在 `~/AItermux` 时，直接覆盖部署：

```bash
cd ~/AItermux
bash install.sh
```

也可以从子目录运行（等价）：

```bash
cd ~/AItermux/Quickinstall
bash install.sh
```

## 可选参数

```bash
bash install.sh --dry-run
bash install.sh --skip-preview
bash install.sh --quiet
```

## 说明

- `deploy/termux/`：覆盖到 Termux 启动链路文件（`motd.sh` / `login` / `etc/motd.sh` / `etc/termux-login.sh` / `tx11start`）与配置（`termux.properties`）；安装时还会把 `~/.termux/shell` 设为 `$PREFIX/bin/zsh`，确保 `motd` 菜单结束后进入正常 zsh。
- `deploy/aitermux/`：安装启动器、bootstrap，以及保留给 zsh 的占位片段；Launcher 菜单本体已经回收到 `motd.sh`。
- `deploy/startboot/`：开屏动画脚本池；所有可执行 `*.sh` 都会进入随机播放池。

安装器职责边界：

- 安装阶段只部署 AITermux 外壳层，不主动拉取 `projectying`
- 安装阶段不主动安装 `codex / gemini`
- 缺失运行时的自动补装，只在用户点击对应 Launcher 入口时触发

前提：

- 需要已安装 `zsh`；安装器会校验 `$PREFIX/bin/zsh`，缺失时直接报错，避免菜单结束后落到 bash。
- `projectying` 不再要求随 kit2 完整提交；默认缺失时会在点击 `PROJECT 萤` 时从 `https://github.com/jiangshanyao2200-hue/projectying.git` 自动 clone。
- `codex / gemini` 缺失时会在点击对应入口时由 bootstrap 自动安装 npm CLI 和 Termux 包装层。

## 启动菜单（登录后）

安装完成后，重新打开 Termux（或新建 session）会先播放随机 `startboot` 动画，再落到 `motd` 里的交互式 `TERMUX LAUNCHER` 菜单：

- `↑↓`：上下选择
- `Enter`：启动当前选中项
- `Esc`：跳过菜单，直接进入 shell

入口仍然是：

- `1`：启动 `PROJECT 萤`（`~/AItermux/projectying/run.sh`）
- `2`：启动 `CODEX`（`codex`）
- `3`：启动 `Gemini`（`gemini`）
- `4`：启动 `Xfce 图形界面`（`tx11start`）

启动项会在当前 `motd` 页面下方继续执行；程序退出后，再落到正常 zsh。

如果入口缺失，选中后按 Enter 时才会触发安装：

- 点击 `PROJECT 萤`：自动 clone 独立仓库
- 点击 `CODEX`：自动安装 `@openai/codex`，并写入 `~/.local/bin/codex` 与 `~/.codex/termux_env.sh`
- 点击 `Gemini`：自动安装 `@google/gemini-cli`，并写入 `~/.local/bin/gemini`

安装失败不会阻塞进入菜单；错误会写进 `~/AItermux/logs/startup.log`，后续按退避策略重试。

## 运行时可调参数（开屏动画）

默认策略：尽量全屏渲染，但用较低负载参数减少卡顿；动画结束后固定落到一帧静态赛博欢迎头，不再额外 flicker。

- 轻量模式（必要时再开）：`AITERMUX_MOTD_LIGHT=1`（会把画布限制为 `70x24`）
- 自定义画布上限：`AITERMUX_MOTD_MAX_COLS=70`、`AITERMUX_MOTD_MAX_ROWS=24`（仅当 `AITERMUX_MOTD_LIGHT=1` 或手动设置时生效）
- 速度相关示例（偏快）：`AITERMUX_MOTD_FPS=12`、`AITERMUX_MOTD_DURATION=1.1`、`AITERMUX_MOTD_HOLD=0`、`AITERMUX_MOTD_SPEED=1.2`
- 速度相关示例（偏稳）：`AITERMUX_MOTD_FPS=12`、`AITERMUX_MOTD_DURATION=1.7`、`AITERMUX_MOTD_HOLD=0.4`、`AITERMUX_MOTD_SPEED=1.0`
- 超时兜底：`AITERMUX_MOTD_TIMEOUT=4`（支持 `4`/`4s`/`200ms`/`1m` 等；`0` 表示不超时）
- 关闭颜色：`AITERMUX_MOTD_COLOR=0`（欢迎头和菜单都会退回无彩色）

## startboot 约定

- `startboot/` 里所有带执行权限的 `*.sh` 都会被随机抽取。
- 脚本报错、超时或异常退出时，`motd` 会直接跳过，继续进入 Launcher。
- 如果你只想放素材、说明或辅助文件，不要给它们可执行权限，也不要命名成 `*.sh`。

## 安装可视化与回滚

- 每次安装会创建备份目录：`~/AItermux/backups/upgrade-YYYYMMDD-HHMMSS/`
- 安装过程会输出每一步，并写入日志：`install.log`
- 回滚脚本：`rollback.sh`（把备份文件复制回原路径）
