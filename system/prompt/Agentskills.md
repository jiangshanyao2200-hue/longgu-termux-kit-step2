# MCP Skills Index

本文件供 `skills_mcp` 按 `category` 返回对应工具说明，避免一次性加载全部。

示例：
```json
{"tool":"skills_mcp","category":"编程类","brief":"获取编程类概览"}
```
```json
{"tool":"skills_mcp","category":"list","brief":"获取 list 用法"}
```

## 编程类

### 工具总览（对外门面）

- list
- search
- bash
- pty
- adb
- termux_api
- system_config
- skills_mcp

建议流程：
1) 先用 list / search 了解结构与定位。
2) 再用 apply_patch 做小步改动（unified diff）。
3) 执行/验证用 bash；长任务或需要交互用 pty。
4) 大输出会截断；必要时再按提示去读 cache 文件。


## list

用途：列目录 + 文件信息（list 与 stat 合并）。

核心原则：
- 只做“查看”，不做改动。
- 目录条目默认最多展示300条，超过会自动导出并只回显160条（中间截断），避免上下文炸掉。

示例：
```json
{"tool":"list","op":"dir","cwd":".","depth":1,"brief":"列目录"}
```
```json
{"tool":"list","op":"files","cwd":".","name":["Cargo.toml"],"brief":"看文件信息"}
```
```json
{"tool":"list","op":"files","cwd":"src","name":["main.rs","lib.rs"],"brief":"看同目录多个文件信息"}
```

字段：
- `op`：`dir` 或 `files`
- `cwd`：可选。基准目录。不填则默认工作目录~/AItermux/system
- `op=dir`：列目录（列的是 cwd 目录；`depth` 仅对 op=dir 生效）
- `op=files`：查看文件信息（`name` 为 cwd 目录下的文件名数组）
- `name`：文件名数组（同目录下纯文件名，不含 `/`）。单文件也用 name
- `depth`：目录递归深度，仅对 op=dir 生效，1-3，默认 1

## search

用途：文本/文件名搜索（rg/grep 门面）。

示例：
```json
{"tool":"search","pattern":"TODO","root":"src","file":false,"timeout_secs":30,"brief":"搜内容"}
```
```json
{"tool":"search","pattern":"Cargo","root":".","file":true,"timeout_secs":30,"brief":"按文件名搜"}
```

## 协同类

- mind_msg：向主意识同步信息（`target:"main"`）
- skills_mcp：按类别取工具说明

## 记忆管理类

- memory_check / memory_read / memory_add / memory_edit
