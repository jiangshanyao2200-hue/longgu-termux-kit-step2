# File Manager（MCP）设计稿（第一步）

目标：提供一个稳定、可审计、可回滚的“文件系统操作层”，减少模型直接拼 bash 导致的误操作与不可控输出；同时保留 bash 作为通用能力，不强制替代。

## 总体分层

### 1) 编辑类（文本/内容编辑）
- `write_file`：创建/写入（全量写）。非空覆盖需要显式 `overwrite:true`，并备份。
- `edit_file`：find/replace（局部替换）。
- `apply_patch`：补丁应用（可 strict）。
- `read_file`：读取（peek/head/tail/range/full 限制）。

编辑类不纳入 File Manager（避免语义混杂）。

### 2) File Manager（文件系统操作）
一个统一工具：`file_manager`。

覆盖：
- 浏览：`list` / `stat`
- 检索：`search`（文件名/内容/both）
- 管理：`copy` / `move`
- 安全删除：`trash` / `restore`（回收站闭环）

原则：默认不做“真实删除”，删除/覆盖/移动前的旧版本进入回收站。

## 回收站（Recycle Bin）

位置：项目 `system` 目录内（建议：`log/recycle/`）。

每次回收生成一个 `trash_id`，同时落盘一份元信息（meta）：
- 原始路径 `src`
- 回收目标路径 `dst`
- 时间戳、PID、操作方式（rename / copy）
- 文件统计（bytes/entries 可选）

### 重要约束
- 对同一文件系统内路径：优先 `rename`（快且原子）。
- 跨文件系统（EXDEV）：退化为 `copy` 到回收站，并汇报“原文件未删除”（避免真删）。

## 状态与 UI 呈现规则

工具返回 `meta` 中必须包含 `状态:<token>`。

约定：
- `0` / `ok` / `ok_*`：成功或“可预期状态”（不显示为错误）
- `ok_need_confirm`：需要用户/模型显式确认（不应显示为失败）
- `ok_not_found`：路径不存在（汇报情况）
- `fail`：明确失败（才显示“执行失败”）

## 输出预算与落盘策略

search/list 等可能命中较多：
- 允许调用方传 `max_chars/max_lines/limit` 等参数；
- 工具端仍有 HARD CAP（最终兜底）；
- 超出时：截断 + `partial:true reason:budget`，并建议/自动落盘返回 `saved:<path>`。

## `file_manager` 工具草案（JSON）

```json
{"tool":"file_manager","op":"copy","src":"a.txt","dst":"backup/a.txt","force":false,"recursive":false,"brief":"Copy"}
```

常见 op：
- `list`：`path`/`input`
- `stat`：`path`
- `search`：`mode=name|content|both` + `pattern` + `root`
- `copy`：`src` + `dst`（目录需 `recursive:true`）
- `move`：`src` + `dst`（`force:true` 可覆盖；覆盖前会进入回收站）
- `trash`：`path`（或 `src`）
- `restore`：`trash_id`（可 `force:true` 覆盖目标）

## 实施顺序（建议）
1) 落文档（本文件）
2) `file_manager` v0：`list/stat/search` 先透传到现有实现（统一 UI label + preview）
3) `trash/restore`：回收站闭环
4) `copy/move`：先支持文件/目录（递归），跨 FS 做安全退化
5) 后续再考虑：分页 cursor、follow_symlink、purge（真删除）等增强

