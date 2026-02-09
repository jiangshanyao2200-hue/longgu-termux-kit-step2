Project Ying（萤）

一个在 Termux/终端中运行的 AI 聊天程序（测试中），基于 Rust TUI（ratatui）框架，支持 MAIN/DOG 双通道与工具调用链。（当前为半成品状态）

---

🧠 它能做什么？

· 聊天 —— 与 AI 对话
· 执行工具 —— 通过 MCP 调用外部工具
· 玩 —— 探索交互与趣味功能

---

🚀 功能特点

· 双 agent 协同 —— MAIN 与 DOG 双通道协作
· MCP 工具 —— 支持 Model Context Protocol 工具调用
· Agent Skills —— 多种技能模块
· 动态上下文 —— 智能上下文管理
· 长期记忆方案 —— 本地化记忆存储
· 动态成长 —— AI 能力持续演进
· 细节技术实现 —— 多项底层优化（半成品测试阶段）

---

🔬 技术探索方向

1. 永久的本地化记忆方案
2. 动态人格塑造解决方案
3. 长期动态上下文解决方案
4. Agent 技术协同解决方案
5. AI 协同解决方案
6. AI 主动交流解决方案
7. 手机 Agent 执行解决方案

---

🎯 未来愿景

我们正在设计和开发一款能够从出生就陪伴用户直到生命终结的 AI 助理（Agent 助理 / AGI）。通过 Termux 环境、Shell 环境，让 AI 有潜力发展为 AGI：

· 通过传感器感知环境
· 构建与人类的动态情感连接
· 通过实时心跳机制让 AI 具备主动权
· 通过动态压缩记忆实现永久记忆
· 通过 AI 协同，构建主意识与潜意识的交互

理论上，它的开发与升级永无止境。在完善本项目之前，我们需先将精力放在完善 Termux 环境上，让 Termux 本身更加实用。我们计划先将 AI/AGI 技术植入 Termux，再继续 Ying 项目的开发。

通过探索 Project Ying 的技术，我们对 AGI 有了更深刻的理解与方向。然而遗憾的是，未经长期升级与细节打磨的 Ying 目前更像一个玩具。因此，我们希望将这些技术应用到 Termux 中，先打造一个有用、实用的 AI Termux。

---

🛠️ 快速开始

Termux 一键运行（自动自检并安装依赖）

```bash
cd ~/AItermux/system
bash Quickinstall/install.sh
```

安装脚本位于仓库内 `Quickinstall/`，安装到用户目录 `~/AItermux/`，会执行：
- 备份并覆盖启动链路文件：`~/.termux/motd.sh`、`$PREFIX/bin/login`、`$PREFIX/etc/motd.sh`、`$PREFIX/etc/termux-login.sh`、`$PREFIX/bin/tx11start`
- 同步 `~/AItermux/system` 工程代码（保留本地 `config/*.json` 与 `memory/`）
- 安装 `~/AItermux/bin/aitermux` 启动器
- 安装随机开屏动画到 `~/AItermux/startboot`
- 写入 `~/.zshrc` 的 AITermux 自动启动段

可选参数：

```bash
bash Quickinstall/install.sh --dry-run
bash Quickinstall/install.sh --skip-preview
bash Quickinstall/install.sh --quiet
```

如有错误，运行自检脚本：

```bash
./selfcheck.sh
```

直接使用 cargo run 也会在启动前尝试执行 scripts/bootstrap.sh 自动补齐依赖；如需跳过：cargo run -- --no-bootstrap。

---

📦 依赖管理

自动安装脚本

· 通用入口：./scripts/bootstrap.sh
· Termux 专用：./scripts/bootstrap-termux.sh（使用 pkg install -y 自动安装依赖）

完整自检

自检脚本（先执行 bootstrap，再运行测试与内置 --selfcheck）：

```bash
./scripts/selfcheck.sh
```

---

⚙️ 配置说明

系统设置项说明；
仓库不包含任何 `api_key`，请自行填写 `config/*_api.json`。

输入 `/settings` 进入设置界面。

常用命令：
- `/turn dog` / `/turn main`：切换潜意识/主意识（不共享上下文、不使用同一份提示词）
- `/think mcp open|close`：开启/关闭思考工具（模型侧能力开关）
- `/sse open|close`：流式开启/关闭

聊天页快捷键（小屏优先：默认只展示必要信息）：
- `PgUp/PgDn`：滚动聊天
- `Tab`：全局切换简约/详情模式（工具解析层级）
- `↑/↓`：选择消息（输入框为空时，高亮一条消息）
- `←`：展开/收起思考（选中 AI 正文时生效）
- `→`：展开/收起工具详情（选中工具消息时生效）
- `Enter`：复制选中消息到剪贴板（输入框为空时，依赖 `termux-clipboard-set`）
- `Home`：退出选择模式（取消高亮）

上下文；20k，触发上下文压缩的统计，20k推荐

心跳；5/10/30/60 分钟，推荐30分钟。自动发送消息
动效；开启（我也不知道是什么）

轮窗口：n轮对话触发fastcontext（动态context池）

轮token：触发动态context的总token

摘要池：动态context的摘要条数


测试阶段，任何bug都有可能产生。


项目运行时默认读取以下配置文件：

· config/dog_api.json
· config/main_api.json
· config/system.json

仓库默认提供一套用于联调/演示的测试 key（已做限额）。如需使用自己的 key，可用示例文件覆盖后再填写：

```bash
cp -n config/dog_api.example.json  config/dog_api.json
cp -n config/main_api.example.json config/main_api.json
cp -n config/system.example.json   config/system.json
```

---

⚠️ 注意事项

1. Termux API：Termux 的 termux-api 包只能提供命令；若命令调用失败，还需要在手机上安装 Termux:API APP 并授予权限。
2. ADB 工具：未 root 且未建立 TCP ADB 协议的设备，可能导致 AI 执行 adb 命令失败。未 root 的设备请在 dogprompt 中删除 adb 工具说明。
3. 心跳功能：心跳机制会主动触发聊天，目前处于测试阶段，后台运行需谨慎观察 AI 行为动向。
4. 若启动卡在“依赖已齐全”，脚本会提示安装 Termux:API（Git：`https://github.com/termux/termux-api`）并尝试执行 `pkg update -y && pkg install -y termux-api` 后跳过；也可手动跳过检测：`PROJECTYING_TERMUX_API_CHECK=0 ./run.sh`，或设置超时：`PROJECTYING_TERMUX_API_TIMEOUT=2 ./run.sh`

---

📁 项目结构

```
.
├── config/                 # 配置文件目录
│   ├── dog_api.example.json
│   ├── main_api.example.json
│   └── system.example.json
├── scripts/               # 脚本目录
│   ├── bootstrap.sh
│   ├── bootstrap-termux.sh
│   └── selfcheck.sh
├── src/                   # 源代码目录
├── Cargo.toml            # Rust 项目配置
├── run.sh                # 一键运行脚本
└── selfcheck.sh          # 自检脚本
```

---

🔄 开发状态

当前项目处于 半成品测试阶段，功能尚在完善中。欢迎参与测试与反馈！

---

📄 许可证

待定

---

🤝 贡献指南

欢迎提交 Issue 和 Pull Request！
联系在抖音：625433532

---

提示：本项目为实验性项目，功能可能随时调整，请勿用于生产环境。
