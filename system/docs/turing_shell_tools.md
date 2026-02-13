# Shell 工具图灵化规范（bash / adb / termux_api）

目的：让智能体（TA）以**最少字段**稳定调用 shell 类工具；参数“模块化拼接”，减少重复提示词与学习成本，同时保持标准、可审计、可预测。

## 总体原则

1) **标准优先**：不做模糊推断；字段缺失就走默认；默认不改变命令语义。
2) **模块化**：允许把参数按模块写在子对象里（工具端会展开合并）。
3) **一致性**：`bash/adb/termux_api` 共用同一套可选模块字段。
4) **审计友好**：`brief` 仍建议提供；meta 中用稳定键值输出（`状态/exit/cwd/saved/...`）。

## 推荐写法（模块化）

支持把参数按模块写在 `input` 对象里：

```json
{
  "tool": "bash",
  "brief": "查目录并容忍 grep 无命中",
  "input": {
    "input": "ls -la; rg -n \"TODO\" -S . || true",
    "run": { "cwd": "AItermux/system", "timeout_ms": 12000 },
    "out": { "save": "auto" },
    "expect": { "ok_exit_codes": [0, 1] }
  }
}
```

也可以用扁平写法（等价）：

```json
{
  "tool": "bash",
  "brief": "同上（扁平）",
  "input": "ls -la; rg -n \"TODO\" -S . || true",
  "cwd": "AItermux/system",
  "timeout_ms": 12000,
  "save": "auto",
  "ok_exit_codes": [0, 1]
}
```

## 模块字段（可选）

### 1) `run` 模块（执行环境）
- `cwd`：工作目录（字符串）。不存在会导致执行失败（属于明确失败）。
- `timeout_ms` / `timeout_secs`：超时；`timeout_ms` 优先。

### 2) `out` 模块（输出/落盘策略）
- `save`：`"auto" | "always" | "never"`
  - `auto`：按内置阈值（大输出/截断时）自动落盘
  - `always`：总是落盘（返回“导出提示 + 头尾预览”）
  - `never`：不落盘（仍会按 UI/工具输出预算截断）

### 3) `expect` 模块（结果判定）
- `ok_exit_codes`：可接受的退出码数组（除 0 以外也可声明为“成功/可预期”）。
  - 例如：`grep/rg` 无命中通常是 `1`，可设 `[0,1]` 避免被 UI 渲染成错误。

## 状态约定（meta 的 `状态:`）

- `0` / `ok` / `ok_*`：成功或可预期状态（UI 不应渲染为错误）
- `timeout`：超时（UI 显示状态提示，不是“执行失败”）
- `adb_offline`：ADB 不可用（UI 显示状态提示）
- `fail`：明确失败（才渲染“执行失败”）

