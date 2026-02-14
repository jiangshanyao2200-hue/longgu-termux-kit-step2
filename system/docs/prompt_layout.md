# Prompt 目录规范（config/prompt）

目标：把所有“模型提示词/注入模板/工具回注模板/系统欢迎语”等集中到 `config/prompt/`，便于版本化、对齐与后续图灵化迭代。

## 目录结构

- `config/prompt/agent/`
  - `main.txt`：MAIN 智能体主提示词
  - `dog.txt`：DOG（调度/压缩）提示词

- `config/prompt/memory/`
  - `context_main.txt`：日记/主上下文模板（用于写日记等记忆任务）
  - `context_compact.txt`：上下文压缩提示词
  - `fastmemo_compact.txt`：fastmemo 压缩提示词

- `config/prompt/mcp/`
  - `mcp_messages.json`：工具链注入/确认/心跳等模板（含变量占位符）
  - `tool_messages.json`：工具回注给智能体的格式模板（TOOL/OUTPUT/META 等）
  - `pty_audit.txt`：PTY 定时审计注入模板
  - `pty_help.txt`：PTY 启动快捷键说明模板
  - `pty_started_notice.txt`：PTY 启动后 tool output 模板

- `config/prompt/system/`
  - `welcome_shortcuts.txt`：启动欢迎语/快捷键（系统消息）

- `config/prompt/skills/`
  - `index.md`：Skills 索引（供 skills_mcp 工具读取）

## 命名约定

- 全部使用 `snake_case` 文件名。
- 目录名表达“用途域”（agent/memory/mcp/system/skills），文件名表达“具体职责”。
- 含变量的模板优先使用 `.json`（例如 `*_messages.json`），纯文本模板用 `.txt`。
