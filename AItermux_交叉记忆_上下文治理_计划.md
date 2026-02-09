# AItermux 交叉记忆与工具上下文治理（v1 计划）

> 目标：在不牺牲“关键记忆/推理能力”的前提下，把 token 消耗严格收敛到可控阈值，尤其是抑制“工具输出滚雪球”导致的上下文污染与费用爆炸。

## 0. 总体原则（必须坚持）

1. **工具输出默认不进入模型上下文原文**：工具原始输出进入 `Artifacts(证据库)`，上下文只注入“摘要 + 指针（artifact_id/path）”。
2. **记忆延续靠系统编排，不靠模型长记忆**：每一次模型请求都由 AItermux 重新拼装“必要上下文”。上下文可裁剪，但必须可追溯。
3. **“重要信息”用 Pins（可审计/可回滚/有限额）而不是自然语言堆砌**：避免 todo/笔记本膨胀成新的 token 杀手。
4. **上下文分流**：Chat（面向用户）与 Tool-loop（面向执行）使用不同的上下文配方，互相隔离噪声。

---

## 1. 核心对象（四件套）

为避免“区域太多导致人和 AI 都难以消化”，只保留四个“系统级对象”，其余都视为派生视图：

1. **Plan（TODO）**：只记录“要做什么/下一步/完成条件”，短、可执行、可勾选。  
2. **Notebook（工作笔记）**：记录“为什么这么做/做到了什么/关键决策/下一步依据”，允许压缩合并。  
3. **Pins（重要信息）**：跨回合保留的关键事实/约束/路径/代码片段摘要，严格限额 + 证据引用 + TTL。  
4. **Artifacts（证据库）**：工具原始输出、生成的代码、文件 diff、日志等，按 id/路径存储；默认不注入上下文。

> 解释：Plan 解决“我现在要干什么”；Notebook 解决“我为什么这么干”；Pins 解决“哪些必须永远记住”；Artifacts 解决“证据原文在哪里，需要时可取证”。

---

## 2. 上下文配方（两条链路）

### 2.1 Chat 回合（用户对话）

用户发消息时，模型输入建议固定为以下顺序（并带清晰分区标题）：

1. **基础提示词（Base Prompt）**
2. **表层记忆（Fastmemo：你的表层记忆）**
3. **过去总结（Context Pool：过去一段时间聊天总结）**
4. **重要信息（Pins：关键事实/约束/指针）**
5. **AI 笔记本（Notebook：本次做的笔记）**
6. **历史聊天记录（Chat Window：你们最近的聊天记录，已过滤工具原文）**
7. **最新三轮（Recent-3：实时最新发言）**

Chat 回合规则：
- **不附带详细工具返回原文**（只允许出现“简短工具摘要/指针”）。
- 允许包含 AI 写的代码，但建议在 Chat Window 中只保留“必要片段/摘要”，大段代码应进入 Artifacts 并用指针引用（由 Pins/Notebook 引导）。

### 2.2 Tool-loop 回合（执行轮询）

当模型发起 tool call 后，进入“工具轮询回合”，下一次模型请求的上下文建议为：

1. **基础提示词（Base Prompt）+ 工具契约（Tool Contract）**
2. **表层记忆（Fastmemo）**
3. **过去总结（Context Pool）**
4. **重要信息（Pins）**
5. **Plan（TODO）**
6. **Notebook（工作笔记）**
7. **最新三轮（Recent-3，包含刚才的 tool call 摘要）**
8. **本次工具返回（ToolResult）**：默认“截断 + 结构化摘要 + artifact_id”，必要时可按需加载局部原文。

Tool-loop 回合规则：
- **不回放历史工具原文**：历史工具输出只以 Pins/Notebook 的“指针 + 摘要”形式出现。
- **工具输出的“重要性升级”必须显式发生**（见第 3 节）。

---

## 3. Pins（重要信息）机制（Pin/Unpin）

### 3.1 什么能被 Pin

允许被 Pin 的典型类型：
- 关键约束（版本、环境、不能做的事、必须做的事）
- 关键路径/文件（例如配置文件位置、生成物位置）
- 关键命令/流程（可复现、可一键执行）
- 关键代码片段（仅摘要/接口签名/关键差异，不放整段）
- 关键结论（例如“某 bug 根因是 X”）

### 3.2 Pin 的硬规则（防失控）

- **必须带证据引用**：`source = chat:<id> | tool:<id> | file:<path>#Lx`  
- **必须短**：默认上限（例如 600 字符），长内容强制改为“摘要 + artifact_id/path”  
- **必须限额**：例如最多 20 条，满了则拒绝或要求先 Unpin  
- **必须有 TTL**：例如 7 天或 30 次会话，过期自动降级为 Notebook/动态池摘要（可续期）

### 3.3 Pin/Unpin 的表达方式（不要用自然语言猜）

落地时建议做成“结构化动作”（优先是内部 tool，而不是靠模型输出特殊文本）：

- `pin_add(type, title, summary, source, artifact_ref?, ttl?)`
- `pin_remove(ids[])`
- `pin_update(id, ...)`
- `pin_list()`

> 说明：你提到的 `!todo remove #1 #3` 可以作为 UI 快捷命令，但最终落地仍应走结构化动作，保证可审计/可回滚。

---

## 4. Artifacts（证据库）与“按需加载”

### 4.1 工具输出入库

每次工具执行（无论成功失败）生成：
- `tool_id`（递增或 uuid）
- `owner`（main/dog）
- `tool_name` + `input`（参数）
- `stdout/stderr/exit_code`
- `created_at`

存储：
- 小输出可直接进 SQLite；
- 大输出写文件（如 `memory/artifacts/<tool_id>.txt`），DB 只存索引与摘要。

### 4.2 模型上下文只拿“摘要 + 指针”

默认注入：
- `ToolResultSummary`: 1~6 行（关键字段 + 结果）  
- `artifact_ref`: `tool:<id>` 或文件路径

当模型需要证据原文时，走“按需加载”工具（未来实现）：
- `artifact_get(tool_id, range?)`
- `file_snippet(path, start, len)`
- `grep(path, pattern, limit)`

> 关键：按需加载必须是“点对点拉取”，不能把历史工具原文整包回灌。

---

## 5. Token 预算与统计

### 5.1 Sc（聊天 tokens）作为软触发

- `Sc` 只统计 `user + assistant` 的聊天 token（不含 prompt/fastmemo/动态池 system）。
- 达到 `ctx_recent_max_tokens`（例如 3000/5000）后：进入“待压缩”（软触发），等待当前生成/工具链结束再压缩。

### 5.2 压缩策略（你们已实现的方向）

- 压缩对象：可压缩聊天窗口（不含 Recent-3）。
- 压缩产物：写入 **fastmemo 的 `[动态context池]`**（计入 fastmemo 的 10 条阈值治理）。
- 规则：任一区 `>= 10` 时触发 `fastmemo_compact`（DOG 静默压缩整份 fastmemo），**不再使用“最旧挤出”**。
- 压缩后：保留 Recent-3 以维持连贯性。

---

## 6. 整合策略（减少区域、降低实现复杂度）

你担心“区域太多”对 AI 也会产生额外负担，这个判断成立。建议把“固定每次都要发送的东西”整合为一个**可版本化的 Context State**，由系统负责管理与注入。

### 6.1 推荐落地：`context_state.json`（单文件，结构化）

建议在 `AItermux/system/memory/` 下新增：
- `context_state.json`：权威状态（可读、可 diff、可备份）

示例结构（示意）：

```json
{
  "version": 1,
  "base_prompt_ref": "prompts/main.txt",
  "tool_contract_ref": "prompts/tool_contract.txt",
  "fastmemo_ref": "memory/fastmemo.txt",
  "fastmemo_dynamic_pool_ref": "fastmemo[动态context池]",
  "pins": [{"id": 1, "type": "constraint", "summary": "...", "source": "tool:12"}],
  "plan": [{"id": 1, "text": "...", "done": false}],
  "notebook": [{"ts": "...", "summary": "...", "source": "tool:12"}],
  "recent_window_policy": {"keep_rounds": 3},
  "limits": {"pins_max": 20, "fastmemo_section_max": 10}
}
```

说明：
- prompt 仍保留文本文件便于编辑；`context_state.json` 只存引用与“系统管理状态”（fastmemo/pins/plan/notebook）。
- 这样每次拼装上下文时，只需加载一个状态文件 + 少量引用文件，避免散落一堆逻辑。

### 6.2 与 SQLite 共存（后续可选）

当 notebook/工具 artifacts 变大后：
- `context_state.json` 保留“摘要/索引/活跃集合”
- `memo.db` 存原始条目与索引

---

## 7. 分阶段实施计划（逐步上线，避免一次性大爆炸）

### Phase 1（最小闭环）
- Artifacts 入库：每次工具执行写 `tool_id` + 输出文件
- Notebook：每次工具后自动追加 1 条“极短笔记”（先用规则生成，后续再交给 dog 记忆管家）
- Pins：实现 pin_add/pin_remove（先不做 TTL）
- Context packer：Chat 与 Tool-loop 两套配方切换（只解决“历史工具原文不回灌”）

### Phase 2（可靠性增强）
- Pins 限额 + TTL + 证据引用强制
- Notebook 压缩合并（滚动压缩，保留里程碑）
- 按需加载：artifact_get/file_snippet/grep（点对点）

### Phase 3（记忆管家 dog 上线）
- dog 专职：pins/notebook/fastmemo（含 `[动态context池]`）的抽取、压缩、归档（main 只执行与决策）
- 交叉确认：dog 写入 Pins/日记前与 main 进行确认（避免误记）

---

## 11. FastMemo v2（四感知 + 自动压缩）

FastMemo 的 v2 重构与“记忆管家化”是交叉记忆治理的底座之一，单独拆分为实施计划：
- `AItermux_fastmemo_v2_计划.md`

---

## 8. 风险清单（必须提前规避）

- Pins 滥用 → 变成新的上下文污染源：用“证据引用 + 限额 + TTL + 短摘要”硬约束。
- Notebook 变大 → 变成 token 杀手：必须滚动压缩（里程碑化）。
- 过度裁剪 → 模型找不到证据：必须保留 artifact 指针，并提供按需加载工具。
- tool-loop 反复回放历史 → token 雪崩：禁止历史工具原文回灌，只注入摘要/指针。

---

## 9. 验收标准（可量化）

- tool-loop 连续 30 次工具调用，单次请求 token 不随时间线性增长（稳定预算）。
- Chat 回合不出现“工具原文墙”，只出现摘要/指针。
- Pins/Notebook/fastmemo 都有上限与可追溯来源；能解释“为什么记住它”。

---

## 10. 约定（便于后续协作）

- 所有区块注入到模型时，都必须有清晰标题（避免 AI 混淆来源）。
- 所有“重要信息”写入必须走结构化动作（可审计/可回滚），不要靠自然语言猜。

元反思：这份计划的关键不是“多做几个文件”，而是把信息流变成可控、可追溯、可压缩的系统工程；否则再强的模型也会被上下文噪声拖垮。
