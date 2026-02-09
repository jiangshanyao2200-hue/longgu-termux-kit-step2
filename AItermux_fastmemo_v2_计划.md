# AItermux FastMemo v2（四感知 + 动态context池 + 自动压缩）实施计划

> 目标：把 fastmemo 从“自由文本堆叠”升级为可控的“**四感知 + 动态context池**”结构，保证：**可读、可写、可压缩、可审计**。  
> 核心：每个区域固定最多 10 条；任意区域达到 `>= 10` 即触发 `fastmemo_compact`（由 DOG 执行压缩，静默维护），把 **5 个区域**分别迭代压缩为 1 条（里程碑摘要），长期保持稳定大小并增强推理连续性。

---

## 1. 现状盘点（v1）

- fastmemo 文件：`memory/fastmemo.jsonl`（当前虽然叫 jsonl，但内容是“带 [section] 的文本”）。
- 注入方式：`read_fastmemo_for_context()` 读取文件文本并截断到 1800 chars 注入到模型上下文。
- 写入工具：`memory_add(path=fastmemo, section=..., content=...)` 以“[section]”为锚点向该区块末尾插入内容。

---

## 2. v2 规范（必须定死）

### 2.1 固定区域（Four Senses + Dynamic Pool）

1. `[自我感知]`：输出风格/行为准则/价值观/边界与安全规则（与用户无关的“我是谁/怎么做”）。
2. `[用户感知]`：对用户偏好、设备环境、目标的稳定认识（可长期复用）。
3. `[环境感知]`：对当前运行环境/系统状态的稳定认识（Termux、路径、默认 shell、关键限制等）。
4. `[历史感知]`：长期项目/关键里程碑/重大结论（“发生过什么且未来仍重要”）。
5. `[动态context池]`：系统自动写入的“阶段性摘要/协同记录”等动态信息（由 DOG 统一压缩回收）。

### 2.2 固定条数与格式

- 每个区域 **最多 10 条**（固定上限）。
- 每条必须是单行 bullet：`- ...`（避免多行膨胀与难解析）。
- 允许在一条里包含少量结构（例如 `key: value` / `tag`），但仍保持单行。

### 2.3 写入策略（Append-only + 稳定排序）

- `memory_add fastmemo` 默认追加到该 section 的末尾。
- 不允许写入“提示词注入/大段工具输出原文/整段代码墙”；需要引用时只写“摘要 + 指针（artifact_id/path）”。

---

## 3. 溢出触发：fastmemo_compact（关键逻辑）

### 3.1 触发条件

- 任意一个区域条数 `>= 10` 即触发（软触发，不中断当前 AI/工具链）。

### 3.2 压缩目标（强约束）

- 由 DOG 执行压缩：对 **5 个区域**分别做“迭代压缩”为 **1 条**：
  - 输出：五条 bullet（分别对应五区），每条总结该区过去内容的“稳定结论/可长期复用信息”。
- 压缩时不得引入幻觉性事实；不确定的内容要标注“待确认/推测”或直接丢弃。

### 3.3 压缩时机（软启动，不打断）

- 溢出发生后，只设置 `fastmemo_compact_pending=true`。
- 只有当系统处于空闲（无流式输出、无工具队列、mode=Idle）时才发起 DOG 压缩请求。

---

## 4. 工具与提示词（先写规范，再动代码）

### 4.1 新增提示词文件

- `prompts/fastmemo_compact.txt`（DOG 专用）
  - 输入：完整 fastmemo 文件（五区现有内容）。
  - 任务：将每个区域的内容迭代压缩为 1 条 bullet。
  - 输出：只允许输出工具 JSON（禁止正文）。
  - 推荐流程：1 次 `memory_read` + 对 5 个区域分别 `memory_edit(section=..., content=...)` 覆盖为 1 条 bullet。

### 4.2 强化 memory_add（fastmemo）

需要做到：
- `section` 只允许五种（支持别名映射，比如 “自我/我/人格” → 自我感知；“ctx_pool/动态池” → 动态context池）。
- 自动把多行 content 合并/截断为单行 bullet（必要时截断并提示）。
- 写入完成后统计五区条数：若任一区达到阈值，标记 `fastmemo_compact_pending`。

### 4.3 强化 memory_edit（fastmemo）（用于压缩回写）

当前的压缩回写需要一个“覆盖式写入”能力：
- 采用 `memory_edit(path=fastmemo, section=..., content=...)` 覆盖单个区域（每次 1 区），DOG 依次覆盖 5 区即可完成压缩回写。

---

## 5. 模型侧“自由度”与“系统侧安全阀”

你希望 todo/笔记/重要标记由模型自控，这是可行的，但 fastmemo 属于“系统稳定记忆层”，必须有安全阀：

- fastmemo 不接受大段原文（工具输出/代码/日志），只能写“结论 + 指针”。
- fastmemo 每区永远 ≤10 条：溢出必压缩。
- 压缩由 DOG 执行，但系统必须保证：DOG 输出为空/格式错误时，不要破坏 fastmemo 文件（失败回滚 + 下次再试）。

---

## 6. 分阶段实现（按你指挥一步步做）

### Phase A：只做格式与写入（不做自动压缩）
1. 定义 fastmemo v2 文件模板（五区标题固定、每区空 0 条）。
2. 修改 `memory_add fastmemo`：
   - section 只允许五区 + 别名映射
   - content 强制单行 bullet
   - 统计每区条数（先只记录/日志提示，不触发压缩）

验收：
- 任何写入都能落到正确区域；
- fastmemo 文件结构稳定，且注入到上下文时可读。

### Phase B：加入溢出检测与 pending（仍不触发 DOG）
1. 写入后若任一区 >=10：设置 `fastmemo_compact_pending=true`（只打标，不发请求）。
2. UI/日志提示：已进入“待压缩”。

验收：
- 溢出时不会阻断用户对话/工具调用；
- pending 状态可观察。

### Phase C：实现 DOG 压缩与回写（完整闭环）
1. 新增 `prompts/fastmemo_compact.txt`（DOG 专用）。
2. 在空闲时机触发 DOG 生成（软启动）。
3. DOG 输出工具调用（复用 `memory_edit` 或新增专用 apply）。
4. 系统应用压缩结果：五区各 1 条；更新 `fastmemo.jsonl`；清除 pending。
5. 失败处理：格式错误/空输出 → 不改文件、保持 pending、写日志并延迟重试。

验收：
- 任一区达到阈值后最终都会被压回五区各 1 条；
- fastmemo 大小长期稳定；
- 失败不会破坏文件、不会卡 UI。

---

## 7. 与“交叉上下文记忆”整合（后续，不在本阶段强推）

你提出的整体上下文配方：
- Chat：`prompt + fastmemo + AI笔记 + 历史压缩区 + recent3`
- Tool-loop：在 Chat 基础上追加 `todo + 重要标记 + AI toolcall 完整内容 + 工具返回完整内容`

本阶段仅先把 fastmemo 做成“稳定、可控、可压缩”的底座；后续再把“动态池/历史压缩区”与 fastmemo 做合并（它们本质相近，但合并需要额外的策略与迁移步骤）。

---

## 8. 验收指标（量化）

1. fastmemo 每区条数永远 ≤10（压缩后每区=1）。
2. fastmemo 注入文本 ≤1800 chars（当前实现截断），且结构可读。
3. 溢出触发压缩不打断当前任务（软启动）。
4. DOG 压缩失败不破坏文件（可回滚、可重试）。

---

## 9. 已落地（用于后续打包/同步）

- 修改：`AItermux/system/src/mcp.rs`（fastmemo v2 模板/迁移、`memory_add/memory_edit` 适配五区、并将 `ensure_memory_file` 提升为 `pub(crate)` 供 core 使用）。
- 修改：`AItermux/system/src/main.rs`（把 `context_compact`/`mind_msg` 的动态信息写入 `fastmemo[动态context池]`；任一区 `>=10` 时调度 DOG 执行 `fastmemo_compact`；完成判定为 5 区均被 `memory_edit(section=...)` 覆盖）。
- 新增：`AItermux/system/prompts/fastmemo_compact.txt`（DOG 静默压缩提示词）。

元反思：这份计划故意把“自由文本的智能”与“系统稳定记忆层”拆开；前者靠模型自控，后者靠规则与压缩闭环保证长期稳定，否则记忆系统迟早会被自己写爆。
