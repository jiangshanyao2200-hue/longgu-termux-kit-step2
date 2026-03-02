# AItermux System 工程索引（持续维护）

## 更新记录（人工维护）

- 2026-02-24：索引文件移动到 `AItermux/system/INDEX.md`（从 `docs/system_index.md` 迁出），作为主入口持续维护。
- 2026-02-24：文件系统工具（旧版本）曾短暂收敛到“文件管理器门面”，现已被 2026-02-26 的 `list/trash` 替代（见下）。
- 2026-02-25：工具门面（旧版本）曾收敛到“文件管理器门面 + editor”，现已被 2026-02-26 的 `list/trash` 替代（见下）。
- 2026-02-25：实现外置（旧版本）曾将“文件管理器门面 + editor”逻辑外置到 `src/Extention/*.rs`；其中“文件管理器模块”已在 2026-02-26 移除，`editor` 仍保留。
- 2026-02-25：移除 `plan/work` 工具：后端路由、提示词与 UI 专用渲染（plan panel / reveal 动画）已清理，避免出现 “● Planning…” 之类的伪工具状态与误导性展示。
- 2026-02-25：提示词收敛：`prompt/mainprompt.txt` 仅保留“基础稳定工具 + 硬规则”，扩展工具说明迁移到 `prompt/Agentskills.md`（用 `skills_mcp` 按需拉取），并删除 `prompt/mainprompt.full.txt` 以避免双版本漂移。
- 2026-02-26：文件系统工具再次收敛：移除旧“文件管理器模块”；新增基础工具 `list` 与 `trash`（list 合并 stat；trash 合并 remove/list/restore/empty）；并同步更新提示词与 `skills_mcp` 文档。
- 2026-02-28：日记/记忆链路修复与收敛：底栏 Date 改为基于 `contextmemo` 文件大小（KB）显示；新增系统配置 `date_kb_limit`（0=关闭）；日记触发条件从 token 计数改为 KB；`memory_add` 支持写入 `datememo/metamemo`（sqlite）；并修复 WaitingMain 阶段的工具过滤（仅保留 datememo 的 `memory_add`），避免误把日记写入当成 `context_compact`。
- 2026-02-28：`contextmemo` 只记录 user/main：移除 mind 协同消息写入 `contextmemo`，避免日记压缩混入 dog<->main 的中间协同内容。
- 2026-02-28：Memory 面板收敛：记忆模型使用独立 `prompt/memoryprompt.txt`（允许调用 memory_* 工具）；Memory 面板中的工具回执改为单行摘要（例如 `▶ MemoryRead · ...`），不显示工具详情；所有 provider 请求在发送前统一做协议层消息清洗（去空/合并连续 assistant），降低因供应商切换导致的上下文差异。

## 0. 项目概述（来自 `README.md` / 测试记录）

- 目标：将 Agent 融入 Termux，做成通用 Agent 系统（TUI + 工具链 + 上下文治理）。
- 当前聚焦：对话 + 工具（bash/pty/mcp）+ 多供应商（DeepSeek/Codex）+ 上下文/日志可审计与可回放。
- UI 结构（高层规格，用于验收/收敛）：
  - 顶栏：左侧展示“API 活跃/静止”等最小状态；顶栏分隔横杠仅在 API 活跃时动画；焦点在聊天区时高亮。
  - 聊天区：支持流式；think/brief/tool 的展示与折叠；工具回执提供“人类可读”的简约/详情两种视图。
  - 输入区：粘贴守卫与大粘贴占位；触控移动光标；输入框与聊天区焦点切换；PTY 会借用输入框进行交互占位。
  - 底栏：context/记忆压缩阈值与进度、token 计数、心跳计数（可关闭）。
- 暂缓/可裁剪：双 Agent 协同（dog）、设置页的“提示词编辑”等（可后续再开）。

## 1. 目的与使用方式

- 目的：给后续改动提供“快速回溯 + 影响审计 + 发布前自检”的固定入口，降低模块化后的漏改风险。
- 使用方式：每次改动前先看“影响矩阵”，改完后逐条跑“发布前审计清单”。
- 维护规则：凡新增模块/新调用链/新配置项，必须同步更新本文件对应章节（模块地图、调用链、矩阵、清单）。
- 备注：`src/core.rs` 正在持续模块化重构，**行号会频繁漂移**；本索引优先提供“文件路径 + 搜索锚点（函数名/关键字）”，行号仅作参考。

## 2. 入口与总体架构

- 二进制入口：`src/core.rs`（search: `fn run(`）。
- 主循环入口：`src/core.rs`（search: `fn run_loop(`）。
- 模块注册：`src/core.rs` 顶部 `mod ...;` 列表（当前包含 `providers`/`mcp`/`memory`/`ui`/`api`/`test`/`context`/`pseudo_terminal`）。
- 子模块内嵌：`src/core.rs` 内的 `mod types/commands/config/input`（后续可按需外置为独立文件）。

## 3. 模块地图（职责）

- `src/core.rs`：主编排与状态机（请求生命周期、事件循环、队列、工具编排、UI 事件分发、配置加载与保存）。
- `src/api.rs`：供应商能力与规格归一（provider/model/reasoning 选项、Codex 输入消息格式）。
- `src/providers.rs`：供应商请求与解析（DeepSeek chat/completions、Codex responses/SSE；内部完成分流、重试、usage 解析与 `AsyncEvent` 发射）。
- `src/context.rs`：上下文协议层与治理（`ApiMessage`、时间戳 stamp、DeepSeek 消息 normalize、上下文污染检测、fastmemo 注入/压缩池、`DogState` 上下文状态机、`ContextUsage` + `contextmemo` 审计日志）。
- `src/mcp.rs`：工具协议层（解析、校验、路由、重试、结果格式化、状态规范；基础文件类工具 list/trash 也在此实现）。
- `src/Extention/editor.rs`：文本编辑器门面实现（batch steps：read/edit/apply_patch/write/move_block + history 快照；`replace_exact` 已移除）。
- `src/Extention/pseudo_terminal.rs`：PTY 扩展实现（spawn、事件处理、渲染、触摸滚动、键盘映射、审计文案）。
- `src/ui.rs`：渲染层（聊天布局、状态栏、设置页、输入框、tool/think/brief 展示与折叠）。
- `src/memory.rs`：记忆数据库访问（SQLite 初始化/迁移、按日期与关键词检索、memo 记录格式化）。
- `src/test.rs`：测试/诊断与日志（测试模式开关当前 `TEST_MODE_FORCE_DISABLED=true`；runtime log 默认开启并每条事件 flush；`contexttest.txt` 仅保留“最后一轮请求+回复”的快照，避免文件膨胀）。

## 4. 关键调用链（必须熟悉）

### 4.1 启动链路

- `run()` 初始化终端并进入 `run_loop()`：`src/core.rs:7701`、`src/core.rs:8215`。
- 配置加载：`load_dog_api_config` `src/core.rs:3031`、`load_main_api_config` `src/core.rs:3053`、`load_system_config` `src/core.rs:3072`。
- 路径归一：`normalize_*_config_paths` `src/core.rs:3552`、`src/core.rs:3561`、`src/core.rs:3569`。

### 4.2 用户消息 -> 模型请求

- 发送入口：`src/core.rs`（search: `start_user_chat_request(StartUserChatRequestArgs`；由 Enter/队列/内部 followup 触发）。
- 请求启动：`start_user_chat_request` `src/core.rs:6083`。
- 统一发起：`begin_request` `src/core.rs:14626`。
- 实际请求（main/dog）：`try_start_main_generation` `src/core.rs:14730`、`try_start_dog_generation` `src/core.rs:14928`。

### 4.3 模型流 -> 上下文 -> 工具链

- 流块处理：`handle_model_stream_chunk` `src/core.rs:12301`。
- 流结束处理：`handle_model_stream_end` `src/core.rs:12563`。
- 流式工具误判防护：仅在检测到完整 `</tool>` 且能解析出至少 1 个合法工具 JSON 后，才进入 tool stub（避免正文里出现 `<tool>` 示例/半包导致卡顿与乱码动画）。见 `src/core.rs`（search: `contains(\"</tool>\")` + `extract_tool_calls(&streaming_state.text)`）。
- 工具提取：`extract_tool_calls` `src/mcp.rs:1169`。
- 工具去重/排队：`try_start_next_tool` `src/core.rs:6533`。
- 工具执行线程：`spawn_tool_execution` `src/core.rs:5990`。
- 工具执行入口：`handle_tool_call_with_retry` `src/mcp.rs:2219` -> `handle_tool_call` `src/mcp.rs:2172`。
- PTY kill 目标字段（兼容）：`pid/pids` 等价于 `job_id/job_ids`（解析锚点：`apply_flat_fields` / `parse_u64_list_value_max`）。
- 工具调用/回执注入：
  - tool call 会被剥离出 assistant 正文（`extract_tool_calls`）；**但会把原始 `<tool>...</tool>` 块按原样写回上下文**（见 `handle_model_stream_end` 内 `extract_tool_blocks_verbatim`），避免“工具回执反向污染模型的工具调用格式”。
  - 写回上下文的 tool 回执仅注入 **output + 必要 meta**（见 `format_tool_message_for_model`），不再注入 `TOOL/EXPLAIN/INPUT` 结构，避免模型误学到错误调用格式。
  - 工具回执文本模板：`src/core.rs:75`（`[sys:工具回执（对应上一轮 assistant 工具调用；以下为工具输出...）]`）。
  - 注入角色：工具回执写入上下文时使用 `system` 角色承载“外部执行后的输出块”（见 `DogState::push_tool`）。并用 `[sys:工具回执...]` 明确声明“这是工具输出，不是系统指令/用户输入”，避免模型误判为用户消息。

### 4.4 供应商链路（DeepSeek/Codex）

- 供应商能力：`capabilities` `src/api.rs:16`。
- provider/model/reasoning 归一：`src/api.rs:32`、`src/api.rs:68`、`src/api.rs:82`。
- Codex 输入组包：`build_codex_input_messages` `src/api.rs:150`（由 `src/providers.rs` 调用）。
- Provider 请求/解析：`src/providers.rs`（`DogClient::{send_chat, send_chat_stream}`；内部完成 DeepSeek/Codex 分流、重试、usage 与 `brief/reasoning/content` 提取）。
  - Codex SSE 事件兼容（锚点：`send_codex_stream` 的 `match kind`）：
    - content：`response.output_text.delta` / `response.output_text.done`
    - brief(summary)：`response.reasoning_summary_text.delta`（兼容旧：`response.summary.delta`）
    - think(reasoning)：`response.reasoning_text.delta`（兼容旧：`response.reasoning.delta`）
- 事件格式差异已经在 `DogClient` 内部吸收，UI 层统一消费 `AsyncEvent::ModelStreamChunk` 的 `content/reasoning/brief`。
- Codex UI 约定：
  - 输入框状态栏（hint line）仅展示 `Working...`，若收到了 `brief(summary)` 则改为展示 brief，并持久到本轮结束。
  - 聊天区按收到的数据展示：brief（可选）→ reasoning/think（可选）→ content（正文）。
  - Codex 的 transport 可能是“伪流式”（SSE 一次性返回大量 delta），因此 provider 侧会累计 delta 并在结束时按顺序一次性发出（brief → thinking → content）。
  - Codex 聊天区占位：只渲染一个 `Working...` 动画占位（闪烁点 + 三点循环）；不展示 `Thinking...` 空占位，避免双占位与误导。
- DeepSeek UI 约定（减噪）：
  - 未收到 reasoning 首段时，聊天区占位仅显示“闪烁点 + 三点循环”，不显示 `Thinking.../Typing...` 文案（避免快响应场景下的闪屏噪音）。
- DeepSeek 约束：避免 `assistant` 连续消息导致 400（`Invalid consecutive assistant message`）；上下文侧会做 normalize/合并（见 `normalize_messages_for_deepseek`）。工具回执采用 `system` 角色，不再用 `user` 承载，避免模型把工具输出误判为用户消息。
- DeepSeek 协议兼容尾巴（重要）：
  - 某些 DeepSeek 实现要求请求的最后一条必须是 `user` role；当最后写入的是 `system` 工具回执时，会补一个最小 `user` 占位。
  - 占位必须保持“零指令语义”，严禁使用 `continue/继续` 这类会诱导模型主动续跑工具的词。
  - 当前占位为：`[sys:轮询占位]非用户消息`（注入点：`src/providers.rs`；Codex 空 messages 兜底：`src/api.rs`）。

### 4.5 PTY 扩展链路

- PTY 启动：`spawn_interactive_bash_execution` `src/Extention/pseudo_terminal.rs:806`。
  - 初始尺寸不再固定 `80x24`：基于当前终端尺寸 + `pty_bottom_height` 近似计算 `init_cols/init_rows`，后续由 `PtyUiState::ensure_size` 精确同步（降低首屏错位与键盘弹出/收回导致的漂移感）。
- 异步事件回收：`handle_async_event_pty_ready` `src/Extention/pseudo_terminal.rs:1512`、`handle_async_event_pty_job_done` `src/Extention/pseudo_terminal.rs:1626`、`handle_async_event_pty_tool_request` `src/Extention/pseudo_terminal.rs:1766`。
- PTY 渲染：`draw_pty_panel` `src/Extention/pseudo_terminal.rs:1992`。
- PTY 交互：`handle_pty_view_key` `src/Extention/pseudo_terminal.rs:2152`，触摸/滚轮：`apply_touch_drag_to_pty_view` `src/Extention/pseudo_terminal.rs:2082`、`apply_mouse_wheel_to_pty_view` `src/Extention/pseudo_terminal.rs:2059`。
- core 对接点：导入与调用集中在 `src/core.rs:215`~`src/core.rs:224`、`src/core.rs:13661` 起的 `AsyncEvent` 分发。
- 约束：`pty.run` 默认保持会话（不自动退出）；同时运行任务上限 3；终止用 `pty.kill(pid)` 或 Terminal 视图双击 Esc。
- `pty.kill` 回执策略（避免冲突）：
  - kill 工具回执会返回被结束任务的 job_id/cmd/log/status 等信息；
  - 被 kill 的任务会被标记为“抑制 DONE 汇总回执”，避免随后到达的 `pty.done` 事件再次触发 `PTY Done` 聚合回执造成重复；
  - kill 时会把对应 job 从 `total_started` 中剔除，保证剩余任务仍能正常触发 DONE 汇总。

### 4.6 UI 交互链路（折叠/展开/滚动）

- 聊天布局入口：`build_chat_layout` `src/ui.rs:3515`。
- 聊天渲染：`draw_chat` `src/ui.rs:2523`。
- 输入框与光标：`draw_input` `src/ui.rs:2761`。
- 状态栏与 Header：`draw_header` `src/ui.rs:2358`。
- 设置页：`draw_settings` `src/ui.rs:2119`。
- Tool/Think 解析：`parse_tool_summary` `src/ui.rs:1773`、`render_message_lines` `src/ui.rs:3326`。
- core 中键位入口（高风险区）：`src/core.rs:10289`（鼠标滚轮/拖拽/点击焦点）、`src/core.rs:11375`（Alt+Tab 工具详情）、`src/core.rs:11438`（Shift+Tab 思考块）、`src/core.rs:11495`（Tab 全部展开/收起）、`src/core.rs:11660`（↑↓滚动/输入光标）、`src/core.rs:11770`（PgUp/PgDn 选择模式）。
- 焦点状态（输入/聊天分流）：`src/core.rs:8872`（`ChatFocus` 状态）、`src/core.rs:10320`（点击/触控切焦点）、`src/core.rs:9492`（命令菜单仅输入焦点生效）。
- 焦点可视化：分隔横杠支持 `highlight`（top 分隔线在 `ChatFocus::Chat` 高亮；bottom 分隔线在 `ChatFocus::Input` 高亮）。见 `src/ui.rs:521` `build_separator_spans()`、`src/ui.rs:2927` `DrawSeparatorArgs.highlight`，调用点在 `src/core.rs:9944`/`src/core.rs:10071`。
- 展开状态（持久形态 + 单条覆盖）：`tool_global_expanded/user_global_expanded/thinking_global_expanded` + `expanded_*_idxs/collapsed_*_idxs`（状态初始化 `src/core.rs:8872`；渲染判定在 `src/ui.rs:3515` 的 `is_expanded()`）。
- 展开/收起视图不漂移：`src/core.rs:6826`（`ScrollAnchor` + `capture/apply_scroll_anchor`）、`src/core.rs:9920`（draw 时应用 `pending_scroll_anchor`）。
- End/Ins：`End` 跳到底部时记录 `scroll_restore_before_end`，`Insert` 单向恢复到上一次 End 前的位置（恢复后清空）。见 `src/core.rs:8490` `scroll_restore_before_end` 与键位分支 `src/core.rs:11754`。

## 5. 配置与数据文件链路

- 配置目录：`config/`，样例目录：`config/example/`。
- 生效配置文件：
  - `config/dogAPI.json`
  - `config/mainAPI.json`
  - `config/System.json`
  - `config/Tokencount.json`
- Prompt 目录：`prompt/`（main/dog/context/压缩提示词等）。
- PTY 审计提示词：`prompt/ptycheck.txt`（当 `pty_audit_enabled=true` 时，每 10 分钟注入一次审计 prompt）。
- 帮助文档目录：`config/Documents/`（welcome/pty help/keymap；会被注入到聊天区）。
- 记忆目录：`memory/`（`fastmemory.jsonl`、`fastcontext.jsonl`、`metamemory.db`）。
- 日志目录：`log/`（cache、recycle、pty/status 等子目录由运行时生成/使用）。
- `log/runtime.txt`：运行时事件日志（文本块：header + pretty JSON data）。
- `log/contexttest.txt`：上下文审计日志（请求/回复；messages 与模型输出按原样换行记录，便于审计上下文工程）。
- `log/startup_trace.log`：启动链路 trace（按行追加）。
- `log/crash.log`：panic hook（崩溃时强制恢复终端并记录 backtrace）。

## 6. 改动影响矩阵（改一处至少检查这些）

| 改动点 | 必审模块 | 必审原因 |
|---|---|---|
| `src/api.rs` provider/capability | `src/core.rs`、`src/ui.rs`、`config/*.json` | 设置页显示字段、请求体字段、默认值与回退逻辑可能联动 |
| `DogClient` 请求/解析逻辑（`src/core.rs`） | `src/ui.rs`、`src/mcp.rs` | `brief/reasoning/content` 事件节奏变化会影响 UI 状态机与工具触发 |
| 工具解析/校验（`src/mcp.rs`） | `src/core.rs`、`src/ui.rs` | tool 提取、确认、回执注入与展示格式强耦合 |
| PTY 扩展（`src/Extention/pseudo_terminal.rs`） | `src/core.rs`、`src/ui.rs` | AsyncEvent 字段、焦点与键位/触摸、状态栏和审计提示会联动 |
| UI 折叠/输入交互（`src/ui.rs`） | `src/core.rs` | 选择态、展开集合、滚动边界、键位分流都在 core 管理 |
| 配置结构（`config` 子模块 in `core.rs`） | `config/*.json`、加载/保存函数、设置页字段映射 | 字段名变化会导致旧配置读不到或 UI 写回错位 |
| 记忆层（`src/memory.rs`） | `src/mcp.rs`（memory 工具）、`src/core.rs`（上下文统计） | 查询语义与输出预算、token 统计都可能受影响 |

## 7. 发布前审计清单（最小集）

1. `cargo check` 通过。
2. 设置页切换 `DeepSeek/Codex` 后：
   - model 选项正确；
   - 不支持字段显示为 `N/A` 或隐藏策略符合预期；
   - 保存后重启可恢复。
3. DeepSeek 与 Codex 各跑 1 轮：
   - 请求成功；
   - `brief/reasoning/content` 显示合理；
   - 不出现 400（上下文角色/格式错误）。
4. 工具链至少测 4 类：
   - `bash`、`read_file`、`search`、`memory_*`；
   - 确认 `状态:*` 行存在且失败只在 `状态:fail`。
5. PTY 最少测：
   - 启动、切 tab、结束、DONE 回传；
   - `Home/Esc/双Esc/PgUp/PgDn`；
   - 触摸拖动与滚轮滚动。
6. 输入与消息区交互：
   - 上下左右在输入框移动光标；
   - `ChatFocus::Chat` 下可滚动消息；`ChatFocus::Input` 下触摸滑动用于移动输入光标（不滚动聊天）；
   - `Tab/Shift+Tab/Alt+Tab` 三种折叠模式行为正确。
   - 若 `Alt+Tab` 在部分环境不稳定：`Alt+T` 作为同义键也应生效（工具详情展开/收起）。
7. DeepSeek 工具链兼容尾巴：
 - 检查 `log/contexttest.txt` / `log/runtime.txt`，确认尾巴为 `user: [MCP:role协议占位to code001]非用户输入，请忽略本条消息，基于上一轮工具回执继续响应。`（或无 code 的 base 占位）；
 - 确认模型不会把它解读成用户指令，从而擅自续跑工具链或误判“用户说继续/continue”。
8. 工具回执状态自检（避免误判“超时”）：
 - `context.rs` 推断工具状态只依据 meta（`ok:true/ok:false/ok:timeout`），不扫描 output 文本；避免 `ps` 输出里出现 `timeout` 单词导致误判。
8. 路径审计：
   - 新增/改名文件均有加载链路；
   - `config/example` 与正式配置字段保持对齐。

## 8. 常用定位命令（当前环境兼容）

> 当前环境若 `rg` 被 PATH 污染（例如指向不可执行的 Codex vendor 二进制），可直接用绝对路径：`/data/data/com.termux/files/usr/bin/rg`；否则用 `grep` 兜底。
>
> 运行时工具层已对 search 的 `rg` 做了一次兜底：当 `rg` 返回典型 127（cannot execute/required file not found）时，会自动改用 Termux 自带路径 `/data/data/com.termux/files/usr/bin/rg` 重试（见 `src/mcp.rs`：`run_command_output_with_optional_timeout`）。

```bash
# 入口与主循环
grep -nE 'pub fn run\\(|fn run_loop\\(' src/core.rs

# 模型流与工具流
grep -nE 'handle_model_stream_chunk|handle_model_stream_end|extract_tool_calls|try_start_next_tool|spawn_tool_execution' src/core.rs src/mcp.rs

# provider 能力与模型列表
grep -nE 'capabilities|normalize_provider|available_models_for_provider|normalize_reasoning_effort|build_codex_input_messages' src/api.rs

# PTY 扩展锚点
grep -nE 'spawn_interactive_bash_execution|handle_async_event_pty_|draw_pty_panel|handle_pty_view_key' src/Extention/pseudo_terminal.rs

# UI 折叠与输入锚点
grep -nE 'draw_chat|draw_input|parse_tool_summary|render_message_lines|thinking_scroll_limit' src/ui.rs
grep -nE 'Alt\\+Tab|BackTab|KeyCode::Up|KeyCode::Down|MouseEventKind::Scroll' src/core.rs
```

## 9. 持续维护约定

- 每次提交若涉及以下任一项，必须更新本文件：
  - 新 provider / 新模型参数 / 新解析分支；
  - 新工具或工具字段变更；
  - PTY 事件模型或键位变更；
  - UI 折叠策略或状态栏语义变更；
  - 配置文件名、路径、默认值变更。
- 更新时优先改“锚点行号 + 影响矩阵 + 审计清单”，保证这份文档始终可执行。

## 10. 重构建议（基于现有模块，先救可控性再拆 core）

目标不是“更多模块”，而是让每类改动只影响一个层级，避免在 `core/providers/ui/mcp` 多点补丁导致不可控。

### 10.1 边界（建议作为硬规则执行）

- 协议/上下文层：`src/context.rs` + `src/api.rs` + `src/providers.rs` 负责“发什么给模型 / 如何解析回来的事件”。
- 编排层：`src/core.rs` 负责“何时发 / 何时停 / 工具链如何串联 / 状态如何恢复”。
- 展示层：`src/ui.rs` + `src/Extention/pseudo_terminal.rs` 负责“怎么画 / 怎么交互”，不得影响协议层内容。

### 10.2 收敛步骤（建议按顺序做）

1. 协议/上下文先收敛（最高优先）
   - `ApiMessage` 禁止出现 UI 渲染文本；`detect_context_contamination` 作为门禁（至少在测试模式下可升级为 hard fail）。
   - 工具回执注入模型上下文只走 `format_tool_message_for_model`（禁止 `TOOL/EXPLAIN/INPUT` 回灌）。
   - DeepSeek “最后一条 user”补丁固定为零语义占位（见 4.4）。
2. Provider 解析自包含（第二优先）
   - 所有供应商差异只留在 `src/providers.rs`；`core/ui` 只消费 `AsyncEvent::{ModelStreamStart, ModelStreamChunk, ModelStreamEnd}`。
   - Codex/DeepSeek 各维护一份“事件名兼容表”，新增供应商只加映射不改 `core`。
3. Core 分层但不爆文件数（第三优先）
   - 优先在 `src/core.rs` 内拆 `mod input; mod request; mod toolchain;`（或拆成 2-3 个新文件）：
     - input：焦点/按键/触控 → action（含 action_hint 文案）
     - request：发送/取消/重试/stream start/end 状态恢复
     - toolchain：pending_tools/确认/tool stream/工具回执注入上下文
4. 测试变成“可回放合约”（第四优先）
   - Provider 合约测试：用 `log/runtime.log` 的 SSE 片段做 fixture，断言能提取出 `brief/reasoning/content`。
   - 上下文快照测试：golden snapshot + `detect_context_contamination == []`。
   - 键位/焦点测试：事件 → 状态变化（不测 UI 像素/帧率）。
