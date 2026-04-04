AItermux（Termux kit2 by longgu）

这个仓库负责维护 Termux 的启动链路、开屏动画、欢迎页/菜单和安装脚本本体。

`projectying` 已按独立仓库部署，不再要求随 kit2 一起完整提交源码。

`kit2` 安装阶段现在只负责把 Termux 同步成 AITermux 的启动链、动画、菜单和样式层，不会在安装时主动拉取 `projectying` 或安装 `codex / gemini`。

真正的依赖补装只发生在 Launcher 入口点击时：

- 点击 `PROJECT 萤`，若缺少 `projectying`，就先自动安装
- 点击 `CODEX`，若缺少 `codex`，就先自动安装
- 点击 `Gemini`，若缺少 `gemini`，就先自动安装

## 快速开始（推荐）

```bash
git clone https://github.com/jiangshanyao2200-hue/longgu-termux-kit-step2.git ~/AItermux
cd ~/AItermux
bash install.sh
```

前提：需要 `zsh`；安装脚本会校验 `$PREFIX/bin/zsh`，并把 `~/.termux/shell` 链到它，保证 `motd` 菜单结束后回到正常的 zsh。

默认的 `projectying` 仓库地址：

```text
https://github.com/jiangshanyao2200-hue/projectying.git
```

如需替换，可在安装前设置：

```bash
export AITERMUX_PROJECTYING_REPO='你的仓库地址'
```

## 登录后菜单

安装完成后，重新打开 Termux（或新建 session）会先播放随机开屏动画，然后在 `motd` 里显示交互式 `TERMUX LAUNCHER` 菜单：

- `↑↓`：上下选择
- `Enter`：启动当前选中项
- `Esc`：跳过菜单，直接进入 shell

入口仍然是：

- `1`：启动 `PROJECT 萤`
- `2`：启动 `CODEX`
- `3`：启动 `Gemini`
- `4`：启动 `Xfce 图形界面`

启动项会直接在当前 `motd` 页面下方继续执行；程序退出后，再落到正常 zsh。若入口缺失，会先尝试自动补装；补装失败会写日志，然后继续放行到 shell，不会把启动链卡死。

## 目录结构

- `Quickinstall/`：一键覆盖部署脚本与模板（会写入 `~/.termux/` 和 `$PREFIX/...`）
- `install.sh`：根目录安装入口（转调 `Quickinstall/install.sh`）
- `bin/aitermux-bootstrap`：运行时缺失依赖补装器
- `bin/aitermux`：启动器（由安装脚本写入/更新）
- `startboot/`：本机动画脚本池；所有可执行 `*.sh` 都可能被随机播放

## 启动依赖补装

`bin/aitermux-bootstrap` 当前会处理三类依赖：

- `projectying`：若 `~/AItermux/projectying` 不存在，则 clone 独立仓库
- `codex`：若缺失，则通过 npm 全局安装 `@openai/codex`，并写入 Termux 包装器与环境文件
- `gemini`：若缺失，则通过 npm 全局安装 `@google/gemini-cli`，并写入 `~/.local/bin/gemini` 包装器

失败会写入 `~/AItermux/logs/startup.log`，并做短暂退避，避免每次登录都重复卡在网络安装上。
