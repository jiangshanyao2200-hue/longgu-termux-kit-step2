# MCP 工具整改路线图（图灵化 / 可验收）

> 目的：在“上下文压缩”频繁发生的情况下，仍能稳定推进 MCP 工具整改。  
> 用法：每次开始新一轮改动前，先读本文件；完成一个里程碑后更新本文件的“进度/验收/决策记录”。

## 0. 范围与原则（必须稳定）

### 0.1 工具职责拆分（OS 观）
- **File Manager**：只管“文件/目录对象”的管理（list/stat/search/create/copy/move/trash/restore 等）。
- **Editor**：只管“文本内容”的定位、变更、校验、提交（open/index/slice → dry_run(ops) → apply）。
- **Shell/PTY**：只管“命令执行与作业生命周期”（普通/交互/并发），输出与日志落盘遵循预算。
- **Memory Manager**：只管“记忆数据源（fastmemo/sqlite）”的读/检索/追加/压缩策略。

> 说明：我们做的是“AI 操作系统”，不是单纯 agent 工具箱；因此**协议稳定性、可解释性、可回滚**优先。

### 0.2 失败判定（硬约束）
- **只有 `状态:fail` 才算失败。**  
  其他 `状态:*`（如 `状态:0`、`状态:ok_*`、`状态:ok_timeout`、`状态:timeout`）都属于“情况汇报”，UI 不应渲染为“执行失败”。
- 参见：`docs/turing_tool_status.md`

### 0.3 输出预算与落盘（硬约束）
- 所有工具必须遵守“总字符/总行数预算”；超限必须落盘并回传 `saved:` 指针 + `meta:size/lines`。
- 参见：`docs/turing_output_budget.md`

### 0.4 安全与可恢复（硬约束）
- 任何删除/覆写都必须可恢复（回收站/备份）；副作用工具超时不得自动重试。
- 原子写优先（写临时文件 → rename 替换）。

## 1. 当前进度（2026-02-13）

### 已完成
- `write_file`：原子写、覆写确认、备份到 `log/backupcache/`（后续会调整“仅写已存在文件”语义）。
- `file_manager`：list/stat/search/copy/move/trash/restore + 回收站闭环（后续补批量与预算/meta）。
- shell 工具图灵化：`bash/adb/termux_api` 支持模块化参数与 `ok_exit_codes`。
- memory 工具对齐：`memory:true` + fastmemo v3（4 区）+ sqlite memo 读取单日硬约束 + 超限导出 `log/memory-cache/`。
- 状态兜底误判收口：避免把“情况汇报”误判失败；新增规范文档（状态/预算）。
- PTY 多 tab：支持同时存在多个后台终端任务；终端视图内 `PgUp/PgDn` 快速切换；PTY 输出事件按 `job_id` 路由到对应 tab。
- UI 动画：当 PTY 视图占用导致聊天区未渲染时，切回聊天会自动将“乱码/打字机”等动画快速推进到尾部，避免重播历史（仅在流式尾部保留动画）。

### 进行中
- bash/PTY 作业化（job 模型 + 多 PTY 并发 + UI 焦点/快捷键状态机 + DONE 清理一致性）。

## 2. 里程碑（逐个落实 / 逐个验收）

### M1：状态与预算规范落地（已完成）
**验收：**
- 任一工具输出都包含 `状态:*`（或至少不被误判为 fail）。
- 超限时必落盘并返回 `saved:` 指针。

### M2：Shell/PTY 作业化（下一优先）
**目标：**
- 统一把长任务/交互任务表达为 `job_id`（可轮询、可终止、可聚合多作业状态）。
- PTY 自然结束或被终止后：**UI 不残留**，Home 不可再呼出已结束 PTY。

**需要定死的“状态机”（建议写到实现与文档里）：**
- 模式：`Chat` / `MsgSelect` / `PTY` / `PTYTabs`
- 全局键优先级：`Esc×2(350ms 内终止作业)` > `Home(显示/隐藏 PTY)` > `Esc(透传终端)` > `PgUp/PgDn(按模式切换)` > 其他
- 用户输入/退出必须拥有最高优先级；后台日志推送最低优先级（可订阅/可暂停）。

**验收：**
- `start` 返回：`job_id + log_path + 初始状态 + meta`。
- `poll(job_id)`：返回状态变化摘要 + 尾部日志预览（受预算控制）。
- `stop(job_id)`：返回最终状态；若已结束返回 `状态:ok_not_found`（情况汇报）。
- DONE 以 system 消息或事件队列推送（而非同一次工具调用“二次返回”）。

### M3：File Manager 扩展（批量 + 搜索预算 + meta）
**目标：**
- 批量 create/rename/copy/move/trash/restore（同目录批量 rename 优先）。
- 搜索返回“树形/按路径聚合”的摘要；超限落盘 `log/fm-search-cache/`。
- `stat/list/search` 返回关键 meta：`size_bytes/mtime/lines(文本)/type`。

**验收：**
- 大目录搜索不会炸 UI；必有预算截断与落盘指针。
- 同名冲突策略明确（默认不覆盖；需要显式策略）。

### M4：Editor（事务式批量编辑器）
**目标：**
- 面向智能体的“真正编辑器”：大文件不整读；通过 index/slice 定位；批量 ops 一次提交。

**核心接口（建议）：**
- `editor_open(path, mode=index|slice, selector?, budget?) -> base_rev + index/slices`
- `editor_dry_run(path, base_rev, ops[], asserts[], budget?) -> preview_diff + violations? + saved?`
- `editor_apply(path, base_rev, ops[], policy?) -> 状态 + saved_backup?/trash_id?`

**定位（不依赖行号）：**
- selector：regex / unique-snippet / context-sentinel / hash-anchor（命中次数必须可断言）。

**验收：**
- “段落打散重组”压力用例通过：一次 dry_run 给出可解释 diff；apply 原子提交；失败不落盘。
- 文件漂移（base_rev 不匹配）会拒绝提交并提示重新 open。

### M5：`write_file` 语义调整（按计划）
**目标：**
- `write_file` 仅负责写入/覆写“已存在文件”；不存在则保存 draft 并回传指针，避免智能体重写内容。
- 覆写策略统一：覆写非空必须确认；备份/回收站策略明确为 policy。

**验收：**
- 目标不存在：`状态:ok_draft_saved` + `saved:` 指针 + meta。
- 目标存在：原子写；覆写非空需确认；备份失败则拒绝写入（防丢）。

### M6：Memory Manager（统一门面与区域策略）
**目标：**
- memotools 去重：除 `memory_add` 外尽量复用同一套 search/read 逻辑，通过 `memory:true/region/scan_limit` 扩展。
- fastmemo v3：4 区 + 10 条阈值触发压缩链 + 工具后即时重载。
- datememo/metamemo：日期/区域硬约束、输出预算落盘、scan_limit 解释清晰。

**验收：**
- 误用参数会给出“格式错误 + usage”，而不是 silent fail。
- 大结果必落盘，且 saved 可复现读取。

## 3. 决策记录（简短）

- 2026-02-13：file manager 与 editor 必须分离（职责与协议清晰，避免无限膨胀）。
- 2026-02-13：失败判定仅认 `状态:fail`；其余均为“情况汇报”。
- 2026-02-15：可配置 JSON（如 `mcp_messages.json`）的反序列化不要放进 `Default::default()`，避免与 `#[serde(default)]` 形成递归导致栈溢出；推荐做法是“字段级默认函数 + Default 纯构造”。

## 4. 上下文压缩后的“恢复流程”（给智能体/维护者）

当发现对话被压缩或记忆缺失时：

1) 先读：`AItermux/system/docs/mcp_refactor_roadmap.md`
2) 再读：`.codex/agentmemory.md`（最新约定与已完成变更）
3) 进入当前里程碑（M2/M3/M4…）对应章节，按“目标→验收”推进
