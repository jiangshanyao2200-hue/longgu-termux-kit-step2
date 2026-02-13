# 记忆工具图灵化对齐（memory_* ↔ search/read/edit）

目标：减少 `memory_check/memory_read/memory_edit/memory_add` 与 `search/read_file/edit_file` 的重复心智负担，并把“数据源”差异收敛为一个开关。

结论：记忆相关能力优先用 **对应工具 + `memory:true`** 表达；`memory_*` 作为兼容入口保留，但内部尽量复用同一实现。

## 总规则

- `memory:true` 表示把工具的“数据源”切换到记忆（MemoDb + memory 文件），而不是文件系统目录。
- `brief` 仍建议写（审计/可视化）。
- 旧工具名仍可用（兼容），但 TA 可以优先学习“统一后的四件套”：
  - `search` + `memory:true`：检索记忆
  - `read_file` + `memory:true`：读取记忆
  - `edit_file` + `memory:true`：编辑 fastmemo（find/replace）
  - `memory_add`：仅用于追加 fastmemo（每次 1 条）

## 1) 检索：`search (memory:true)`

```json
{"tool":"search","memory":true,"pattern":"快捷键 PTY","root":"all","count":10,"brief":"在记忆中搜索"}
```

- `pattern`：关键词（AND 语义）
- `root`（可选）：记忆目标，支持 `all/*/datememo/metamemo/fastmemo`（为空则默认检索 datememo+fastmemo；metamemo 需显式指定）
- `date_start/date_end`（可选）：仅对 datememo/metamemo 生效
  - `metamemo`：必须是精确日期（单日），且要求 `region:"day"`（当天）（兼容 `section:"day"`）
  - `datememo`：可用 `region:"recent|past|older"` 选择区域（兼容 `section`）；`recent` 支持单关键词并直接返回记录内容
  - `scan_limit`：仅对 `past/older` 生效（sqlite 候选行拉取上限）

兼容：`memory_check` 内部复用同一逻辑。

示例（datememo 过去区域，多关键词 + 提升扫描上限）：
```json
{"tool":"search","memory":true,"root":"datememo","region":"past","pattern":"PTY Esc","scan_limit":12000,"brief":"检索过去一年内同日多关键词命中"}
```

## 2) 读取：`read_file (memory:true)`

```json
{"tool":"read_file","memory":true,"path":"datememo","date_start":"2026-02-13","brief":"读取 datememo 当天全量"}
```

- `path`：记忆目标（`datememo/metamemo/fastmemo` 或具体 jsonl 路径）
- `datememo/metamemo`：读取必须使用 `date_start`（精确到单日）；`metamemo` 还要求 `region:"day"`（兼容 `section:"day"`）
- `fastmemo`：可按行区间读取（或直接读取全量，工具会自动做预算裁剪）

兼容：`memory_read` 内部复用同一逻辑。

## 3) 编辑：`edit_file (memory:true)`

```json
{"tool":"edit_file","memory":true,"path":"fastmemo","find":"旧条目","replace":"新条目","count":1,"brief":"修正 fastmemo"}
```

约束：`edit_file(memory:true)` 仅支持 `fastmemo`（find/replace）。

兼容：`memory_edit` 仍保留（并继续支持 section+content 的“结构化写区块”能力）。

## 4) 追加：`memory_add`（仅 fastmemo）

`memory_add` 只用于 fastmemo 的结构化追加（`section` 必填，每次只加 1 条 bullet）；达到阈值会触发 fastmemo 自动压缩链。
