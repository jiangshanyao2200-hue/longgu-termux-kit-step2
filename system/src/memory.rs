use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Local, NaiveDate, NaiveDateTime, TimeZone};
use rusqlite::{Connection, OptionalExtension, params, params_from_iter, types::Value};
use std::time::Duration;

pub const DEFAULT_MEMO_DB_PATH: &str = "memory/metamemory.db";
pub const DEFAULT_METAMEMO_PATH: &str = "memory/metamemo.jsonl";
pub const DEFAULT_DATEMEMO_PATH: &str = "memory/datememo.jsonl";

type MemoKeywordCheckResult = (
    Vec<MemoRow>,
    usize,
    MemoStats,
    Option<String>,
    Option<String>,
);

#[derive(Clone, Copy, Debug)]
pub enum MemoKind {
    Meta,
    Date,
}

impl MemoKind {
    pub fn table(self) -> &'static str {
        match self {
            MemoKind::Meta => "metamemo",
            MemoKind::Date => "datememo",
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemoRow {
    pub rownum: usize,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct DateMemoDayAgg {
    pub date: String,       // YYYY-MM-DD (来自 DB 字段)
    pub latest_ts: String,  // 该日最新一条记录的 ts（来自 DB 字段）
    pub entries: usize,     // 该日命中条目数量（DB 行数）
    pub kw_hits: usize,     // 该日命中“不同关键词”的数量（按日聚合）
}

#[derive(Debug, Clone, Default)]
pub struct MemoStats {
    pub total_rows: usize,
    pub total_chars: usize,
}

#[derive(Clone, Debug)]
pub struct MemoDb {
    db_path: PathBuf,
}

impl MemoDb {
    pub fn open(db_path: PathBuf, meta_jsonl: PathBuf, date_jsonl: PathBuf) -> Result<Self> {
        let db = Self { db_path };
        db.ensure_ready(&meta_jsonl, &date_jsonl)?;
        Ok(db)
    }

    pub fn open_default() -> Result<Self> {
        let db_path =
            std::env::var("YING_MEMO_DB_PATH").unwrap_or_else(|_| DEFAULT_MEMO_DB_PATH.to_string());
        let meta_path = std::env::var("YING_METAMEMO_PATH")
            .unwrap_or_else(|_| DEFAULT_METAMEMO_PATH.to_string());
        let date_path = std::env::var("YING_DATEMEMO_PATH")
            .unwrap_or_else(|_| DEFAULT_DATEMEMO_PATH.to_string());
        Self::open(
            PathBuf::from(db_path),
            PathBuf::from(meta_path),
            PathBuf::from(date_path),
        )
    }

    pub fn append_entry(
        &self,
        kind: MemoKind,
        ts: &str,
        speaker: &str,
        content: &str,
    ) -> Result<()> {
        let normalized_ts = normalize_memo_ts(ts);
        let date = extract_memo_date(&normalized_ts);
        let conn = self.connect()?;
        insert_entry(&conn, kind.table(), &normalized_ts, &date, speaker, content)?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn append_datememo_content(&self, content: &str) -> Result<()> {
        let trimmed = content.trim();
        if trimmed.is_empty() {
            return Ok(());
        }
        let (ts, speaker) = parse_header_ts_speaker(trimmed.lines().next().unwrap_or(""));
        let normalized_ts = normalize_memo_ts(&ts);
        let date = extract_memo_date(&normalized_ts);
        let conn = self.connect()?;
        insert_entry(
            &conn,
            MemoKind::Date.table(),
            &normalized_ts,
            &date,
            &speaker,
            trimmed,
        )?;
        Ok(())
    }

    pub fn read_last_entry(&self, kind: MemoKind) -> Result<Option<String>> {
        let conn = self.connect()?;
        let sql = format!(
            "SELECT content FROM {} ORDER BY id DESC LIMIT 1",
            kind.table()
        );
        let mut stmt = conn.prepare(&sql)?;
        let row = stmt.query_row([], |r| r.get::<_, String>(0)).optional()?;
        Ok(row)
    }

    #[allow(dead_code)]
    pub fn table_stats(&self, kind: MemoKind) -> Result<MemoStats> {
        let conn = self.connect()?;
        table_stats(&conn, kind.table())
    }

    #[allow(dead_code)]
    pub fn read_by_index(
        &self,
        kind: MemoKind,
        start_line: usize,
        max_lines: usize,
    ) -> Result<(Vec<MemoRow>, MemoStats)> {
        let conn = self.connect()?;
        let stats = table_stats(&conn, kind.table())?;
        if stats.total_rows == 0 {
            return Ok((Vec::new(), stats));
        }
        let start_line = start_line.max(1);
        let end_line = start_line.saturating_add(max_lines.saturating_sub(1));
        let sql = format!(
            "SELECT rownum, content FROM (\
                SELECT row_number() OVER (ORDER BY id) AS rownum, content \
                FROM {}\
            ) WHERE rownum BETWEEN ?1 AND ?2 ORDER BY rownum",
            kind.table()
        );
        let mut stmt = conn.prepare(&sql)?;
        let mut rows = Vec::new();
        let iter = stmt.query_map(params![start_line as i64, end_line as i64], |r| {
            Ok(MemoRow {
                rownum: r.get::<_, i64>(0)? as usize,
                content: r.get(1)?,
            })
        })?;
        for row in iter {
            rows.push(row?);
        }
        Ok((rows, stats))
    }

    pub fn read_by_date(
        &self,
        kind: MemoKind,
        start: Option<NaiveDate>,
        end: Option<NaiveDate>,
    ) -> Result<(Vec<MemoRow>, MemoStats)> {
        let conn = self.connect()?;
        let stats = table_stats(&conn, kind.table())?;
        let (clauses, params) = build_date_clause(start, end);
        let where_sql = if clauses.is_empty() {
            "1=1".to_string()
        } else {
            clauses.join(" AND ")
        };
        let sql = format!(
            "SELECT rownum, content FROM (\
                SELECT row_number() OVER (ORDER BY id) AS rownum, content, date \
                FROM {}\
            ) WHERE {} ORDER BY rownum",
            kind.table(),
            where_sql
        );
        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params), |r| {
            Ok(MemoRow {
                rownum: r.get::<_, i64>(0)? as usize,
                content: r.get(1)?,
            })
        })?;
        let mut rows = Vec::new();
        for row in iter {
            rows.push(row?);
        }
        Ok((rows, stats))
    }

    pub fn check_by_keywords(
        &self,
        kind: MemoKind,
        keywords: &[String],
        start: Option<NaiveDate>,
        end: Option<NaiveDate>,
        limit: usize,
    ) -> Result<MemoKeywordCheckResult> {
        let conn = self.connect()?;
        let stats = table_stats(&conn, kind.table())?;
        let (mut clauses, mut params) = build_date_clause(start, end);
        for kw in keywords {
            clauses.push("LOWER(content) LIKE ?".to_string());
            params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
        }
        let where_sql = if clauses.is_empty() {
            "1=1".to_string()
        } else {
            clauses.join(" AND ")
        };
        let count_sql = format!("SELECT COUNT(*) FROM {} WHERE {}", kind.table(), where_sql);
        let total_hits: i64 =
            conn.query_row(&count_sql, params_from_iter(params.clone()), |r| r.get(0))?;
        let range_sql = format!(
            "SELECT MIN(date), MAX(date) FROM {} WHERE {}",
            kind.table(),
            where_sql
        );
        let (min_date, max_date): (Option<String>, Option<String>) =
            conn.query_row(&range_sql, params_from_iter(params.clone()), |r| {
                Ok((r.get(0)?, r.get(1)?))
            })?;
        let mut params_with_limit = params;
        params_with_limit.push(Value::from(limit as i64));
        let sql = format!(
            "SELECT rownum, content FROM (\
                SELECT row_number() OVER (ORDER BY id) AS rownum, content, date \
                FROM {}\
            ) WHERE {} ORDER BY rownum LIMIT ?",
            kind.table(),
            where_sql
        );
        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params_with_limit), |r| {
            Ok(MemoRow {
                rownum: r.get::<_, i64>(0)? as usize,
                content: r.get(1)?,
            })
        })?;
        let mut rows = Vec::new();
        for row in iter {
            rows.push(row?);
        }
        Ok((rows, total_hits.max(0) as usize, stats, min_date, max_date))
    }

    /// 以 OR 语义拉取候选行（用于“按天聚合/多关键词”策略），返回 (date, content)。
    /// 注意：这是低层查询，调用方需自行做按天聚合与输出预算控制。
    pub fn fetch_entries_by_keywords_or(
        &self,
        kind: MemoKind,
        keywords: &[String],
        start: Option<NaiveDate>,
        end: Option<NaiveDate>,
        limit_rows: usize,
    ) -> Result<Vec<(String, String)>> {
        if limit_rows == 0 {
            return Ok(Vec::new());
        }
        let conn = self.connect()?;
        let (mut clauses, mut params) = build_date_clause(start, end);
        if !keywords.is_empty() {
            let mut ors: Vec<String> = Vec::new();
            for kw in keywords {
                ors.push("LOWER(content) LIKE ?".to_string());
                params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
            }
            clauses.push(format!("({})", ors.join(" OR ")));
        }
        let where_sql = if clauses.is_empty() {
            "1=1".to_string()
        } else {
            clauses.join(" AND ")
        };
        let mut params_with_limit = params;
        params_with_limit.push(Value::from(limit_rows as i64));
        let sql = format!(
            "SELECT date, content FROM {} WHERE {} ORDER BY id LIMIT ?",
            kind.table(),
            where_sql
        );
        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params_with_limit), |r| {
            Ok((r.get::<_, String>(0)?, r.get::<_, String>(1)?))
        })?;
        let mut rows = Vec::new();
        for row in iter {
            rows.push(row?);
        }
        Ok(rows)
    }

    /// 以 OR 语义拉取候选行（用于“按天聚合/多关键词”策略），返回 (date, ts, content)。
    /// - 这里的 ts/date 来自数据库字段（权威时间线），不依赖 content 内的时间格式。
    /// - 默认按最新优先（id DESC），便于检索近期日记。
    #[allow(dead_code)]
    pub fn fetch_entries_by_keywords_or_with_ts(
        &self,
        kind: MemoKind,
        keywords: &[String],
        start: Option<NaiveDate>,
        end: Option<NaiveDate>,
        limit_rows: usize,
    ) -> Result<Vec<(String, String, String)>> {
        if limit_rows == 0 {
            return Ok(Vec::new());
        }
        let conn = self.connect()?;
        let (mut clauses, mut params) = build_date_clause(start, end);
        if !keywords.is_empty() {
            let mut ors: Vec<String> = Vec::new();
            for kw in keywords {
                ors.push("LOWER(content) LIKE ?".to_string());
                params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
            }
            clauses.push(format!("({})", ors.join(" OR ")));
        }
        let where_sql = if clauses.is_empty() {
            "1=1".to_string()
        } else {
            clauses.join(" AND ")
        };
        let mut params_with_limit = params;
        params_with_limit.push(Value::from(limit_rows as i64));
        let sql = format!(
            "SELECT date, ts, content FROM {} WHERE {} ORDER BY id DESC LIMIT ?",
            kind.table(),
            where_sql
        );
        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params_with_limit), |r| {
            Ok((
                r.get::<_, String>(0)?,
                r.get::<_, String>(1)?,
                r.get::<_, String>(2)?,
            ))
        })?;
        let mut rows = Vec::new();
        for row in iter {
            rows.push(row?);
        }
        Ok(rows)
    }

    /// datememo 专用：按“日”聚合 OR 关键词命中，返回每一天的：
    /// - DB 字段 date（权威）
    /// - 命中条目数（entries）
    /// - 命中不同关键词数（kw_hits）
    ///
    /// 注意：kw_hits 依据“同日内是否出现该关键词”计算，而不是单条记录内。
    pub fn datememo_aggregate_days_by_keywords_or(
        &self,
        keywords: &[String],
        start: Option<NaiveDate>,
        end: Option<NaiveDate>,
    ) -> Result<Vec<DateMemoDayAgg>> {
        let conn = self.connect()?;
        let (mut clauses, date_params) = build_date_clause(start, end);

        // 若无关键词，直接返回空（调用方应当先校验）。
        if keywords.is_empty() {
            return Ok(Vec::new());
        }

        // WHERE：OR 关键词（仅拉取命中过至少 1 个关键词的日记条目）。
        let mut ors: Vec<String> = Vec::new();
        for _ in keywords {
            ors.push("LOWER(content) LIKE ?".to_string());
        }
        clauses.push(format!("({})", ors.join(" OR ")));

        // SELECT：对每个关键词做一个按日的存在性标记（0/1）。
        // 这里也用参数占位，避免拼接 kw 到 SQL 中。
        let mut kw_cols: Vec<String> = Vec::new();
        for (i, _) in keywords.iter().enumerate() {
            kw_cols.push(format!(
                "MAX(CASE WHEN LOWER(content) LIKE ? THEN 1 ELSE 0 END) AS k{i}"
            ));
        }
        let where_sql = if clauses.is_empty() {
            "1=1".to_string()
        } else {
            clauses.join(" AND ")
        };
        let sql = format!(
            "SELECT date, MAX(ts) AS latest_ts, COUNT(*) AS entries, {} \
             FROM datememo \
             WHERE {} \
             GROUP BY date \
             ORDER BY date DESC",
            kw_cols.join(", "),
            where_sql
        );

        // 参数顺序必须与 SQL 中 `?` 的出现顺序一致：
        // 1) SELECT: 每个 kw 的 CASE WHEN ... LIKE ?
        // 2) WHERE: date_clause 的参数（start/end）
        // 3) WHERE: OR 关键词参数（LOWER(content) LIKE ?）
        let mut params: Vec<Value> = Vec::new();
        for kw in keywords {
            params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
        }
        params.extend(date_params);
        for kw in keywords {
            params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
        }

        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params), |r| {
            let date: String = r.get(0)?;
            let latest_ts: String = r.get(1)?;
            let entries: i64 = r.get(2)?;
            let mut kw_hits: usize = 0;
            // k0..kN
            for i in 0..keywords.len() {
                let v: i64 = r.get(3 + i)?;
                if v != 0 {
                    kw_hits = kw_hits.saturating_add(1);
                }
            }
            Ok(DateMemoDayAgg {
                date,
                latest_ts,
                entries: entries.max(0) as usize,
                kw_hits,
            })
        })?;

        let mut out: Vec<DateMemoDayAgg> = Vec::new();
        for row in iter {
            out.push(row?);
        }
        Ok(out)
    }

    /// datememo 专用：取某一天的最新若干条命中记录（用于 UI/检索预览）。
    pub fn datememo_fetch_samples_for_day(
        &self,
        day: &str, // YYYY-MM-DD
        keywords: &[String],
        limit: usize,
    ) -> Result<Vec<(String, String)>> {
        let limit = limit.clamp(1, 10);
        let conn = self.connect()?;
        let mut clauses: Vec<String> = vec!["date = ?".to_string()];
        let mut params: Vec<Value> = vec![Value::from(day.to_string())];
        if !keywords.is_empty() {
            let mut ors: Vec<String> = Vec::new();
            for kw in keywords {
                ors.push("LOWER(content) LIKE ?".to_string());
                params.push(Value::from(format!("%{}%", kw.to_ascii_lowercase())));
            }
            clauses.push(format!("({})", ors.join(" OR ")));
        }
        let where_sql = clauses.join(" AND ");
        params.push(Value::from(limit as i64));
        let sql = format!(
            "SELECT ts, content FROM datememo WHERE {} ORDER BY id DESC LIMIT ?",
            where_sql
        );
        let mut stmt = conn.prepare(&sql)?;
        let iter = stmt.query_map(params_from_iter(params), |r| {
            Ok((r.get::<_, String>(0)?, r.get::<_, String>(1)?))
        })?;
        let mut rows: Vec<(String, String)> = Vec::new();
        for row in iter {
            rows.push(row?);
        }
        Ok(rows)
    }

    fn connect(&self) -> Result<Connection> {
        if let Some(dir) = self.db_path.parent() {
            fs::create_dir_all(dir).ok();
        }
        let conn = Connection::open(&self.db_path)
            .with_context(|| format!("打开记忆数据库失败：{}", self.db_path.display()))?;
        // 可靠性：避免并发写入/索引迁移时短暂锁定导致工具直接失败。
        let _ = conn.busy_timeout(Duration::from_millis(1200));
        init_schema(&conn)?;
        Ok(conn)
    }

    fn ensure_ready(&self, meta_jsonl: &Path, date_jsonl: &Path) -> Result<()> {
        let mut conn = self.connect()?;
        migrate_if_needed(&mut conn, meta_jsonl, date_jsonl)?;
        Ok(())
    }
}

fn build_date_clause(
    start: Option<NaiveDate>,
    end: Option<NaiveDate>,
) -> (Vec<String>, Vec<Value>) {
    let mut clauses = Vec::new();
    let mut params: Vec<Value> = Vec::new();
    if let Some(start) = start {
        clauses.push("date >= ?".to_string());
        params.push(Value::from(start.format("%Y-%m-%d").to_string()));
    }
    if let Some(end) = end {
        clauses.push("date <= ?".to_string());
        params.push(Value::from(end.format("%Y-%m-%d").to_string()));
    }
    (clauses, params)
}

pub fn build_memo_entry(ts: &str, speaker: &str, text: &str) -> String {
    let mut out = String::new();
    if speaker.trim().is_empty() {
        out.push_str(ts.trim());
    } else {
        out.push_str(ts.trim());
        out.push_str(" | ");
        out.push_str(speaker.trim());
    }
    out.push('\n');
    for line in text.lines() {
        out.push_str("  ");
        out.push_str(line);
        out.push('\n');
    }
    out.trim_end().to_string()
}

pub fn normalize_memo_ts(raw: &str) -> String {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
    }
    if let Ok(dt) = DateTime::parse_from_rfc3339(trimmed) {
        return dt
            .with_timezone(&Local)
            .format("%Y-%m-%d %H:%M:%S")
            .to_string();
    }
    if let Ok(dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%d %H:%M:%S") {
        return Local
            .from_local_datetime(&dt)
            .single()
            .unwrap_or_else(Local::now)
            .format("%Y-%m-%d %H:%M:%S")
            .to_string();
    }
    if let Ok(dt) = NaiveDateTime::parse_from_str(trimmed, "%Y-%m-%d %H:%M") {
        return Local
            .from_local_datetime(&dt)
            .single()
            .unwrap_or_else(Local::now)
            .format("%Y-%m-%d %H:%M:%S")
            .to_string();
    }
    if let Ok(date) = NaiveDate::parse_from_str(trimmed, "%Y-%m-%d") {
        let dt = date.and_hms_opt(0, 0, 0).unwrap();
        return Local
            .from_local_datetime(&dt)
            .single()
            .unwrap_or_else(Local::now)
            .format("%Y-%m-%d %H:%M:%S")
            .to_string();
    }
    let digits: String = trimmed.chars().filter(|c| c.is_ascii_digit()).collect();
    if digits.len() >= 8 {
        let y = digits.get(0..4).and_then(|s| s.parse::<i32>().ok());
        let m = digits.get(4..6).and_then(|s| s.parse::<u32>().ok());
        let d = digits.get(6..8).and_then(|s| s.parse::<u32>().ok());
        if let (Some(y), Some(m), Some(d)) = (y, m, d) {
            let h = digits
                .get(8..10)
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(0);
            let min = digits
                .get(10..12)
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(0);
            let sec = digits
                .get(12..14)
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(0);
            if let Some(date) = NaiveDate::from_ymd_opt(y, m, d) {
                if let Some(dt) = date.and_hms_opt(h, min, sec) {
                    return Local
                        .from_local_datetime(&dt)
                        .single()
                        .unwrap_or_else(Local::now)
                        .format("%Y-%m-%d %H:%M:%S")
                        .to_string();
                }
                let dt = date.and_hms_opt(0, 0, 0).unwrap();
                return Local
                    .from_local_datetime(&dt)
                    .single()
                    .unwrap_or_else(Local::now)
                    .format("%Y-%m-%d %H:%M:%S")
                    .to_string();
            }
        }
    }
    Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
}

fn extract_memo_date(ts: &str) -> String {
    let trimmed = ts.trim();
    if trimmed.len() >= 10 {
        return trimmed[..10].to_string();
    }
    Local::now().format("%Y-%m-%d").to_string()
}

fn init_schema(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS metamemo (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            ts TEXT NOT NULL,
            date TEXT NOT NULL,
            speaker TEXT,
            content TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS datememo (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            ts TEXT NOT NULL,
            date TEXT NOT NULL,
            speaker TEXT,
            content TEXT NOT NULL
        );
        CREATE INDEX IF NOT EXISTS idx_metamemo_date ON metamemo(date);
        CREATE INDEX IF NOT EXISTS idx_metamemo_ts ON metamemo(ts);
        CREATE INDEX IF NOT EXISTS idx_datememo_date ON datememo(date);
        CREATE INDEX IF NOT EXISTS idx_datememo_ts ON datememo(ts);",
    )?;
    Ok(())
}

fn table_stats(conn: &Connection, table: &str) -> Result<MemoStats> {
    let sql = format!("SELECT COUNT(*), SUM(LENGTH(content)) FROM {table}");
    let (count, sum): (i64, Option<i64>) =
        conn.query_row(&sql, [], |r| Ok((r.get(0)?, r.get(1)?)))?;
    Ok(MemoStats {
        total_rows: count.max(0) as usize,
        total_chars: sum.unwrap_or(0).max(0) as usize,
    })
}

fn insert_entry(
    conn: &Connection,
    table: &str,
    ts: &str,
    date: &str,
    speaker: &str,
    content: &str,
) -> Result<()> {
    let sql = format!("INSERT INTO {table} (ts, date, speaker, content) VALUES (?1, ?2, ?3, ?4)");
    conn.execute(&sql, params![ts, date, speaker, content])?;
    Ok(())
}

fn migrate_if_needed(conn: &mut Connection, meta_jsonl: &Path, date_jsonl: &Path) -> Result<()> {
    migrate_table_if_empty(conn, MemoKind::Meta.table(), meta_jsonl)?;
    migrate_table_if_empty(conn, MemoKind::Date.table(), date_jsonl)?;
    Ok(())
}

fn migrate_table_if_empty(conn: &mut Connection, table: &str, jsonl_path: &Path) -> Result<()> {
    if !jsonl_path.exists() {
        return Ok(());
    }
    let count: i64 = conn.query_row(&format!("SELECT COUNT(*) FROM {table}"), [], |r| r.get(0))?;
    if count > 0 {
        return Ok(());
    }
    let raw = fs::read_to_string(jsonl_path)
        .with_context(|| format!("读取记忆文件失败：{}", jsonl_path.display()))?;
    let blocks = collect_blocks(&raw);
    if blocks.is_empty() {
        return Ok(());
    }
    let tx = conn.transaction()?;
    for block in blocks {
        let Some(entry) = parse_block(&block) else {
            continue;
        };
        insert_entry(
            &tx,
            table,
            &entry.ts,
            &entry.date,
            &entry.speaker,
            &entry.content,
        )?;
    }
    tx.commit()?;
    Ok(())
}

#[derive(Debug)]
struct ParsedEntry {
    ts: String,
    date: String,
    speaker: String,
    content: String,
}

fn parse_block(lines: &[String]) -> Option<ParsedEntry> {
    let mut filtered: Vec<String> = Vec::new();
    for line in lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed.starts_with('#') {
            continue;
        }
        filtered.push(line.to_string());
    }
    let first = filtered.first()?.trim().to_string();
    let (ts_raw, speaker) = parse_header_ts_speaker(&first);
    let ts = normalize_memo_ts(&ts_raw);
    let date = extract_memo_date(&ts);
    let content = filtered.join("\n").trim_end().to_string();
    Some(ParsedEntry {
        ts,
        date,
        speaker,
        content,
    })
}

fn parse_header_ts_speaker(line: &str) -> (String, String) {
    let mut parts = line.split('|').map(|s| s.trim());
    let ts = parts.next().unwrap_or("").to_string();
    let speaker = parts.next().unwrap_or("").to_string();
    (ts, speaker)
}

fn collect_blocks(text: &str) -> Vec<Vec<String>> {
    let mut blocks = Vec::new();
    let mut current: Vec<String> = Vec::new();
    for line in text.lines() {
        if line.trim().is_empty() {
            if !current.is_empty() {
                blocks.push(std::mem::take(&mut current));
            }
            continue;
        }
        current.push(line.to_string());
    }
    if !current.is_empty() {
        blocks.push(current);
    }
    blocks
}
