use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Local, NaiveDate, NaiveDateTime, TimeZone};
use rusqlite::{Connection, OptionalExtension, params, params_from_iter, types::Value};

pub const DEFAULT_MEMO_DB_PATH: &str = "memory/memo.db";
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

    pub fn table_stats(&self, kind: MemoKind) -> Result<MemoStats> {
        let conn = self.connect()?;
        table_stats(&conn, kind.table())
    }

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

    fn connect(&self) -> Result<Connection> {
        if let Some(dir) = self.db_path.parent() {
            fs::create_dir_all(dir).ok();
        }
        let conn = Connection::open(&self.db_path)
            .with_context(|| format!("打开记忆数据库失败：{}", self.db_path.display()))?;
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
