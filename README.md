AItermux（Termux kit2 by longgu）

这个仓库用于把 Termux 的启动链路、开屏动画、以及 `system/` 目录下的 Rust 程序（Project Ying/萤）组合成一套“可覆盖部署”的工作环境。

## 快速开始（推荐）

```bash
git clone https://github.com/jiangshanyao2200-hue/longgu-termux-kit-step2.git ~/AItermux
cd ~/AItermux
bash install.sh
```

## 目录结构

- `Quickinstall/`：一键覆盖部署脚本与模板（会写入 `~/.termux/` 和 `$PREFIX/...`）
- `install.sh`：根目录安装入口（转调 `Quickinstall/install.sh`）
- `system/`：Project Ying（萤）Rust 工程源码（详见 `system/README.md`）
- `bin/aitermux`：启动器（由安装脚本写入/更新）

