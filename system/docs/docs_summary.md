# AItermux System 文档总览（非计划类）

## 范围

本文件汇总 `system/docs` 下的非计划类文档。

未纳入（计划类）：
- `mcp_refactor_roadmap.md`
- `pty_queue_plan.md`
- `计划.md`

已汇总来源：
- `file_manager.md`
- `prompt_layout.md`
- `pty_manager.md`
- `sys_compass.md`
- `turing_memory_tools.md`
- `turing_output_budget.md`
- `turing_shell_tools.md`
- `turing_tool_status.md`

## 一致性原则

- 所有工具输出都要受预算控制（字符/行数）。
- 超预算优先“落盘 + 指针返回”，而不是一次性塞满上下文。
- 工具状态统一用 `log_lines` 的 `状态:*` 表达。
- 只有 `状态:fail` 才算执行失败，其余状态属于信息/预期态。
- 与运行协议强耦合的提示模板应内置，避免外置后路径错配。

## 系统注入协议（`[sys:*]`）

所有非用户意图、但进入模型上下文的系统文本，统一 `[sys:...]` 前缀，核心包括：
- `[sys:工具回执（对应上一轮assistant工具调用）]`
- `[sys:工具确认]...`
- `[sys:PTYDone]...`
- `[sys:PTY审计]...`
- `[sys:计划]...`
- `[sys:心跳]...`
- `[sys:轮询占位]`（历史兼容标记）

时间线约定：
- 会话注入一次：`[sys:日期]YYYY-MM-DD +TZ`
- 每条上下文消息附加尾注：`[[t:HHMMSS]]`

## Prompt 布局

外置提示词集中在 `system/prompt/`：
- `mainprompt.txt`、`dogprompt.txt`
- `Agentskills.md`
- `Systemwelcome.txt`、`Terminalwelcome.txt`
- `datesummary.txt`、`contextsummary.txt`、`fastmemorycompact.txt`

MCP 协议文案、PTY 审计/启动通知等高耦合内容收回内置，避免“改了文件但运行未读取”这类漂移。

## 工具状态规范

工具返回分两层：
- `user_message`：用户可读简述。
- `log_lines`：模型/UI 侧结构化状态。

判定规则：
- 失败：仅 `状态:fail`
- 成功/预期：`状态:0`、`状态:ok`、`状态:ok_*`、`状态:timeout`、`状态:ok_timeout`、`状态:ok_not_found`

建议附加字段：
- `saved:<path>`：大输出导出路径
- `log:<path>`：日志路径
- `meta:<...>`：关键统计（bytes/lines/job_id 等）

## 输出预算与落盘

超预算时：
- 返回摘要预览并明确“已截断”。
- 全量内容导出到 `system/log/*`。
- 在日志中返回 `saved:` 和元信息。

智能体使用建议：
- 先看预览，再按 `saved:` 二次读取细节。
- 命中过大时先缩小检索范围（路径/关键词/类型/深度）。

## Shell 工具图灵化（`bash` / `adb` / `termux_api`）

统一模块字段：
- `run`：`cwd`、`timeout_ms/timeout_secs`
- `out`：`save=auto|always|never`
- `expect`：`ok_exit_codes`

目标：低歧义、可审计、跨工具一致。

## 文件系统工具（`file_manager`）

定位：对文件系统操作做统一封装，并提供可回滚路径。

核心操作：
- 浏览：`list`、`stat`
- 检索：`search`
- 管理：`copy`、`move`
- 安全删除闭环：`trash`、`restore`

安全策略：
- 默认避免真实删除。
- 覆盖/移动前保留旧版本到回收站。
- 同文件系统优先原子 `rename`。
- 跨文件系统 `EXDEV` 时安全退化为 copy-only，并清晰回执状态。

当前设计建议回收站位置：
- `system/log/recycle/`

## PTY 管理（`pty`）

生命周期约束：
- 启动提示给用户可读。
- 默认不把“PTY 启动回执”注入给模型，避免模型把它当下一步触发点。
- 后台长任务通过 `[sys:PTY审计]` 让模型只做判断，不继续拉工具。
- 完成汇总走简约态；过长批量结果导出到 `log/bash-cache/` 并返回 `batch_saved:`。

模型调用入口（MCP）：
- `op: run | list | kill`
- `run` 支持批量 `commands[]`（最多 5 个）
- 支持会话模式 `mode:session/persist/keep/shell`

## 记忆工具收敛

推荐统一调用：
- `search` + `memory:true`：检索记忆
- `read_file` + `memory:true`：读取记忆
- `edit_file` + `memory:true`：编辑 fastmemo（find/replace）
- `memory_add`：追加一条 fastmemo

兼容保留：
- `memory_check`、`memory_read`、`memory_edit`、`memory_add` 仍可用，但内部应尽量复用同一套实现。

数据源规则：
- `memory:true` 代表切到记忆数据源（MemoDb + memory 文件），不是普通文件系统检索。
- `metamemo` 约束更严格：需单日精确日期与 day 区域语义。
- `datememo` 支持按时间区域策略检索。

## 运维基线

- 每个工具必须显式返回 `状态:*`。
- 路径、配置、提示词的加载链路必须与实际文件一致。
- 大输出统一按“预览 + 落盘 + 指针”模式处理。
- 优先安全退化，不做静默行为改变。
