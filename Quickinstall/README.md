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
cd ~/AItermux/system
bash install.sh
```

## 可选参数

```bash
bash install.sh --dry-run
bash install.sh --skip-preview
bash install.sh --quiet
```

## 说明

- `deploy/termux/`：覆盖到 Termux 启动链路文件（motd/login/tx11start）。
- `deploy/aitermux/`：安装 `~/AItermux/bin/aitermux` 与 zsh 自动启动片段。
- `deploy/startboot/`：开屏动画脚本池（默认随机播放，忽略 `_*.sh` 辅助脚本）。

## 运行时可调参数（开屏动画）

默认策略：尽量全屏渲染，但用较低负载参数减少卡顿。

- 轻量模式（必要时再开）：`AITERMUX_MOTD_LIGHT=1`（会把画布限制为 `70x24`）
- 自定义画布上限：`AITERMUX_MOTD_MAX_COLS=70`、`AITERMUX_MOTD_MAX_ROWS=24`（仅当 `AITERMUX_MOTD_LIGHT=1` 或手动设置时生效）
- 速度相关：`AITERMUX_MOTD_FPS=12`、`AITERMUX_MOTD_DURATION=1.1`、`AITERMUX_MOTD_HOLD=0`、`AITERMUX_MOTD_SPEED=1.2`
- 速度相关：`AITERMUX_MOTD_FPS=12`、`AITERMUX_MOTD_DURATION=1.7`、`AITERMUX_MOTD_HOLD=0.4`、`AITERMUX_MOTD_SPEED=1.0`
- 超时兜底：`AITERMUX_MOTD_TIMEOUT=4`（支持 `4`/`4s`/`200ms`/`1m` 等；`0` 表示不超时）

## 安装可视化与回滚

- 每次安装会创建备份目录：`~/AItermux/backups/upgrade-YYYYMMDD-HHMMSS/`
- 安装过程会输出每一步，并写入日志：`install.log`
- 回滚脚本：`rollback.sh`（把备份文件复制回原路径）
