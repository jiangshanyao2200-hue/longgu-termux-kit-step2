# MCP Skills Index

本文件按类别组织，供 `skills_mcp` 工具按需返回。当前包含【编程类】【协同类】与【记忆管理类】。

## 使用方式

- 调用 `skills_mcp` 并提供 `category`。
- 仅返回对应类别内容，避免一次性加载全部。

示例：

```json
{"tool":"skills_mcp","category":"编程类","brief":"获取编程类工具说明"}
```

```json
{"tool":"skills_mcp","category":"记忆管理类","brief":"获取记忆工具说明"}
```

```json
{"tool":"skills_mcp","category":"协同类","brief":"获取协同工具说明"}
```

## 编程类

### 工具总览

- list_dir
- stat_file
- read_file
- search
- write_file
- edit_file
- apply_patch
- bash
- adb
- termux_api
- system_config

### 通用 Agent 提示词

1) 先确认目标与约束，再动手。
2) 先搜索定位，再读取必要片段，避免整文件直读。
3) 先写小计划，再分步执行。
4) 改动前后对比，避免多余修改。
5) 能用小改就不做大改，保持可回滚。
6) 执行后自检关键信息（路径、结果、返回码）。
7) 解释结论要简洁，重点是风险与验证。

### 安全与截断规则

- 高风险命令（如 rm -rf/mkfs/dd/reboot 等）会触发确认；默认先避免执行。
- 工具输出有行数/字符数上限，超过会截断；遇到截断应缩小范围或分段读取。
- read_file 默认会裁剪大文件；建议先 search 定位，再分段读取。
- edit/write/apply_patch 前先 read/定位目标片段，避免盲写。
- apply_patch 建议提供足够上下文，避免 fuzz/偏移风险。

### 工具调用通则

- 工具调用必须是 JSON，包含 `tool`。
- `brief` 一句话说明目的。
- 一次只调用一个工具。
- 路径优先用相对路径。
- 输出过长时，先 `search` 再分段 `read_file`。

推荐格式（实际调用时包裹在 `<tool>...</tool>` 中）：

```json
{"tool":"read_file","path":"src/main.rs","start_line":1,"max_lines":80,"brief":"读取入口逻辑"}
```

### 路径规则

- `./` 表示项目根目录（AItermux/system）。
- `home/` 表示 Termux HOME（等同于 `~`）。
- `/` 表示绝对路径。

### 工具清单与示例

#### list_dir

用途：列出目录内容。

字段：
- `tool`: "list_dir"
- `path`: 目录路径（或用 `input`）
- `brief`: 一句话目的

```json
{"tool":"list_dir","path":".","brief":"列出当前目录"}
```

#### stat_file

用途：查看文件/目录基本信息。

字段：
- `tool`: "stat_file"
- `path`: 目标路径（或用 `input`）
- `brief`: 一句话目的

```json
{"tool":"stat_file","path":"Cargo.toml","brief":"查看配置文件信息"}
```

#### read_file

用途：安全读取文件内容，支持分段读取。

字段：
- `tool`: "read_file"
- `path`: 文件路径
- `start_line`: 起始行（1-based，可选）
- `max_lines`: 读取行数（可选）
- `brief`: 一句话目的

推荐流程：先 `search` 定位，再分段读取。

```json
{"tool":"read_file","path":"src/main.rs","brief":"读取入口函数附近内容"}
```

```json
{"tool":"read_file","path":"src/main.rs","start_line":200,"max_lines":120,"brief":"读取 200-319 行"}
```

#### search

用途：全文搜索（优先使用）。

字段：
- `tool`: "search"
- `pattern`: 搜索关键字/正则
- `root`: 搜索根目录（可选，默认 ".")
- `count`: 最大匹配数（可选）
- `brief`: 一句话目的

```json
{"tool":"search","pattern":"draw_header","root":"src","brief":"定位 header 绘制逻辑"}
```

#### write_file

用途：写入/创建文件（会覆盖）。

字段：
- `tool`: "write_file"
- `path`: 目标文件
- `content`: 写入内容
- `brief`: 一句话目的

```json
{"tool":"write_file","path":"notes/demo.txt","content":"hello","brief":"写入演示文件"}
```

#### edit_file

用途：按字符串替换（适合小范围编辑）。

字段：
- `tool`: "edit_file"
- `path`: 目标文件
- `find`: 匹配内容
- `replace`: 替换内容
- `count`: 替换次数（`0` 表示全替换）
- `brief`: 一句话目的

```json
{"tool":"edit_file","path":"src/main.rs","find":"old","replace":"new","count":1,"brief":"替换片段"}
```

#### apply_patch

用途：应用 unified diff 补丁（适合多处修改）。

字段：
- `tool`: "apply_patch"
- `patch`: 统一 diff 文本
- `strict`: `true` 时拒绝 fuzz/offset（更安全）
- `brief`: 一句话目的

```json
{"tool":"apply_patch","patch":"--- a/src/main.rs\n+++ b/src/main.rs\n@@\n- old\n+ new\n","strict":false,"brief":"应用补丁"}
```

#### bash

用途：执行 bash 命令。

字段：
- `tool`: "bash"
- `input`: 命令文本
- `brief`: 一句话目的

```json
{"tool":"bash","input":"ls -la","brief":"查看目录"}
```

#### adb

用途：执行 adb 命令（已默认连接 127.0.0.1:5555）。

字段：
- `tool`: "adb"
- `input`: 命令文本
- `brief`: 一句话目的

```json
{"tool":"adb","input":"shell getprop ro.product.model","brief":"读取设备型号"}
```

#### termux_api

用途：调用 termux-api 命令。

字段：
- `tool`: "termux_api"
- `input`: termux-api 子命令
- `brief`: 一句话目的

```json
{"tool":"termux_api","input":"termux-battery-status","brief":"读取电池状态"}
```

#### system_config

用途：调整系统配置（当前仅心跳间隔）。

字段：
- `tool`: "system_config"
- `heartbeat_minutes`: 心跳间隔（仅支持 5/10/30/60）
- `brief`: 一句话目的

```json
{"tool":"system_config","heartbeat_minutes":10,"brief":"调整心跳间隔"}
```

## 协同类

### 工具总览

- mind_msg

### mind_msg

用途：在 MAIN/DOG 之间传递消息（不等待回传）。

字段：
- `tool`: "mind_msg"
- `target`: "main" 或 "dog"
- `content`: 传递内容
- `brief`: 一句话目的

```json
{"tool":"mind_msg","target":"dog","content":"需要你协助检查工具结果。","brief":"同步进度"}
```

## 记忆管理类

### 工具总览

- memory_check
- memory_read
- memory_edit
- memory_add

### 记忆工具规则

- memo 仅作用于记忆存储（memory/ 目录）：
  datememo/metamemo 已迁移至 SQLite（memory/memo.db），fastmemo 仍为 memory/fastmemo.jsonl
- 默认只检索/读取必要片段，避免整文件直读。
- `memory_check` 支持多关键词（空格/逗号/斜线分隔），为“全部命中”（AND）；找不到时先用单关键词或日期区间再缩小范围。
  默认返回前 10 条；匹配过多仍会返回前 N 条并提示日期范围，可用 `start_date/end_date` 进行日期区域检索。
- `memory_read` 默认可读 `fastmemo` 全文；`metamemo/datememo` 过大时需要先 `memory_check` 再分段读取；支持日期范围或行区间读取（不可同时使用）。
- `memory_edit` 仅支持 `fastmemo`，必须提供 `find/replace` 且 `count` 不宜过大（建议 1）。
- `memory_add` 仅支持 `datememo/fastmemo`：  
  - `datememo` 直接追加一条文本。  
  - `fastmemo` 必须提供 `section`，会追加到对应段落末尾（可选值：动态成长人格 / 用户感知画像 / 人生旅程 / 淡化池）。
- `metamemo` 为深层元记忆，只读（read/check），不使用 add/edit。

### memory_check

用途：回忆检索，快速定位记忆片段。

字段：
- `tool`: "memory_check"
- `pattern`: 关键词（可多关键词）
- `start_date`: 区域检索开始日期（可选）
- `end_date`: 区域检索结束日期（可选）
- `count`: 最大返回条数（可选）
- `brief`: 一句话目的

```json
{"tool":"memory_check","pattern":"工作 计划","brief":"查找最近的工作计划记录"}
```

```json
{"tool":"memory_check","pattern":"升级","brief":"回忆电影《升级》相关记录"}
```

```json
{"tool":"memory_check","pattern":"工作","start_date":"2026-01-01","end_date":"2026-03-12","brief":"限定日期范围检索"}
```

### memory_read

用途：读取记忆文件（metamemo/datememo 过大时需分段）。

字段：
- `tool`: "memory_read"
- `path`: 记忆文件（datememo/fastmemo/metamemo）
- `start_line`: 起始行（行区间）
- `max_lines`: 读取行数（行区间）
- `start_date`: 日期范围开始（仅 datememo/metamemo）
- `end_date`: 日期范围结束（仅 datememo/metamemo）
- `brief`: 一句话目的

```json
{"tool":"memory_read","path":"datememo","start_line":1,"max_lines":120,"brief":"读取日记片段"}
```

```json
{"tool":"memory_read","path":"datememo","start_date":"2026-01-01","end_date":"2026-03-12","brief":"读取指定日期范围"}
```

### memory_edit

用途：精确编辑记忆条目（仅 fastmemo）。

字段：
- `tool`: "memory_edit"
- `path`: fastmemo
- `find`: 查找内容
- `replace`: 替换内容
- `count`: 替换次数（建议 1）
- `brief`: 一句话目的

```json
{"tool":"memory_edit","path":"fastmemo","find":"旧条目","replace":"新条目","count":1,"brief":"修正记忆条目"}
```

### memory_add

用途：快速追加记忆。

字段：
- `tool`: "memory_add"
- `path`: datememo / fastmemo
- `content`: 追加内容
- `section`: fastmemo 需要指定段落（动态成长人格 / 用户感知画像 / 人生旅程 / 淡化池）
- `brief`: 一句话目的

```json
{"tool":"memory_add","path":"datememo","content":"2026-01-20 20:00 | user | 记录内容","brief":"追加日记"}
```

```json
{"tool":"memory_add","path":"fastmemo","section":"动态成长人格","content":"- 更坚定的表达风格","brief":"补充人格条目"}
```
