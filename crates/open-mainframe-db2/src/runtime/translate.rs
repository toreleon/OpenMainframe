//! SQL translation from DB2 to PostgreSQL dialect.
//!
//! Handles syntax differences between DB2 and PostgreSQL.

use std::collections::HashMap;

/// SQL translator for DB2 to PostgreSQL conversion.
pub struct SqlTranslator {
    /// Function mappings
    function_map: HashMap<String, String>,
}

impl SqlTranslator {
    /// Create a new translator.
    pub fn new() -> Self {
        let mut function_map = HashMap::new();

        // DB2 to PostgreSQL function mappings
        function_map.insert("VALUE".to_string(), "COALESCE".to_string());
        function_map.insert("LOCATE".to_string(), "POSITION".to_string());
        function_map.insert("POSSTR".to_string(), "POSITION".to_string());
        function_map.insert("LENGTH".to_string(), "LENGTH".to_string());
        function_map.insert("STRIP".to_string(), "TRIM".to_string());
        function_map.insert("CHAR".to_string(), "CAST".to_string());
        function_map.insert("INTEGER".to_string(), "CAST".to_string());
        function_map.insert("DECIMAL".to_string(), "CAST".to_string());
        function_map.insert("DIGITS".to_string(), "TO_CHAR".to_string());
        function_map.insert("HEX".to_string(), "ENCODE".to_string());
        function_map.insert("DAYOFWEEK".to_string(), "EXTRACT(DOW FROM".to_string());
        function_map.insert("DAYOFYEAR".to_string(), "EXTRACT(DOY FROM".to_string());
        function_map.insert("DAYS".to_string(), "DATE".to_string());

        Self { function_map }
    }

    /// Translate DB2 SQL to PostgreSQL SQL.
    pub fn translate(&self, sql: &str) -> String {
        let mut result = sql.to_string();

        // Translate FETCH FIRST n ROWS ONLY -> LIMIT n
        result = self.translate_fetch_first(&result);

        // Translate OPTIMIZE FOR n ROWS -> (remove, PostgreSQL doesn't use this)
        result = self.translate_optimize_for(&result);

        // Translate WITH UR/CS/RS/RR -> (remove, handled at connection level)
        result = self.translate_isolation_clause(&result);

        // Translate CONCAT operator
        result = self.translate_concat(&result);

        // Translate SUBSTR
        result = self.translate_substr(&result);

        // Translate CURRENT TIMESTAMP
        result = self.translate_current_timestamp(&result);

        // Translate CURRENT DATE
        result = self.translate_current_date(&result);

        // Translate functions
        result = self.translate_functions(&result);

        // Translate data types in CAST expressions
        result = self.translate_data_types(&result);

        // Translate FOR UPDATE OF col1,col2 → FOR UPDATE
        result = self.translate_for_update_of(&result);

        // Translate SET CURRENT SCHEMA → SET search_path TO
        result = self.translate_set_current_schema(&result);

        // Translate special registers
        result = self.translate_special_registers(&result);

        // Translate MERGE INTO to INSERT ... ON CONFLICT
        result = self.translate_merge(&result);

        result
    }

    /// Translate DB2 MERGE to PostgreSQL INSERT ... ON CONFLICT.
    ///
    /// DB2 syntax:
    /// ```sql
    /// MERGE INTO target AS t
    /// USING source AS s ON t.key = s.key
    /// WHEN MATCHED THEN UPDATE SET col1 = s.col1
    /// WHEN NOT MATCHED THEN INSERT (col1) VALUES (s.col1)
    /// ```
    ///
    /// PostgreSQL equivalent:
    /// ```sql
    /// INSERT INTO target (col1) VALUES (...)
    /// ON CONFLICT (key) DO UPDATE SET col1 = EXCLUDED.col1
    /// ```
    ///
    /// This is a simplified translation that rewrites the basic MERGE pattern.
    /// Complex MERGE statements are passed through as-is for `EXECUTE IMMEDIATE`
    /// or manual handling.
    fn translate_merge(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if !upper.starts_with("MERGE") {
            return sql.to_string();
        }

        // Extract target table
        let target = match extract_between(&upper, "MERGE INTO ", " ") {
            Some(t) => t,
            None => return sql.to_string(),
        };

        // Extract ON condition to find the conflict key
        let on_clause = match extract_between(&upper, " ON ", " WHEN") {
            Some(c) => c,
            None => return sql.to_string(),
        };

        // Try to extract the conflict column from ON clause (e.g., "T.KEY = S.KEY" -> "KEY")
        let conflict_col = extract_conflict_column(&on_clause);

        // Extract WHEN NOT MATCHED THEN INSERT columns and values
        let insert_part = if let Some(pos) = upper.find("WHEN NOT MATCHED THEN INSERT") {
            let after = &sql[pos + 28..];
            after.trim().to_string()
        } else {
            String::new()
        };

        // Extract WHEN MATCHED THEN UPDATE SET ...
        let update_set = if let Some(pos) = upper.find("WHEN MATCHED THEN UPDATE SET") {
            let after = &sql[pos + 28..];
            // Take until "WHEN NOT MATCHED" or end
            let end = after.to_uppercase().find("WHEN NOT MATCHED")
                .unwrap_or(after.len());
            after[..end].trim().to_string()
        } else {
            String::new()
        };

        // Build PostgreSQL INSERT ... ON CONFLICT
        if !insert_part.is_empty() {
            let mut pg_sql = format!("INSERT INTO {} {}", target, insert_part.trim_end());

            if let Some(ref col) = conflict_col {
                if !update_set.is_empty() {
                    pg_sql.push_str(&format!(" ON CONFLICT ({}) DO UPDATE SET {}", col, update_set));
                } else {
                    pg_sql.push_str(&format!(" ON CONFLICT ({}) DO NOTHING", col));
                }
            }

            return pg_sql;
        }

        sql.to_string()
    }

    /// Translate FETCH FIRST n ROWS ONLY to LIMIT n.
    fn translate_fetch_first(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("FETCH FIRST") {
            // Find the number of rows
            let after_fetch = &sql[pos + 11..];
            let mut chars = after_fetch.trim_start().chars().peekable();
            let mut num = String::new();

            while let Some(&c) = chars.peek() {
                if c.is_ascii_digit() {
                    num.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            if !num.is_empty() {
                // Find "ROWS ONLY" or "ROW ONLY"
                let remaining: String = chars.collect();
                let remaining_upper = remaining.to_uppercase();

                if remaining_upper.contains("ROW") {
                    // Find the end of "ROWS ONLY" or "ROW ONLY"
                    let end_pos = if let Some(p) = remaining_upper.find("ONLY") {
                        p + 4
                    } else {
                        remaining.len()
                    };

                    // Build the result
                    let before = &sql[..pos];
                    let after = &remaining[end_pos..];
                    return format!("{} LIMIT {} {}", before.trim(), num, after.trim());
                }
            }
        }

        sql.to_string()
    }

    /// Remove OPTIMIZE FOR clause (PostgreSQL doesn't use it).
    fn translate_optimize_for(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("OPTIMIZE FOR") {
            // Find "ROWS" after OPTIMIZE FOR
            let after = &upper[pos..];
            if let Some(rows_pos) = after.find("ROWS") {
                let end = pos + rows_pos + 4;
                let before = &sql[..pos];
                let after = &sql[end..];
                return format!("{}{}", before.trim(), after);
            }
        }

        sql.to_string()
    }

    /// Remove isolation level clauses (WITH UR, CS, RS, RR).
    fn translate_isolation_clause(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        for clause in &["WITH UR", "WITH CS", "WITH RS", "WITH RR"] {
            if let Some(pos) = upper.find(clause) {
                let before = &sql[..pos];
                let after = &sql[pos + clause.len()..];
                return format!("{}{}", before.trim(), after);
            }
        }

        sql.to_string()
    }

    /// Translate CONCAT function to || operator.
    fn translate_concat(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();
        let mut result = sql.to_string();

        // Simple CONCAT(a, b) -> a || b
        // This is a simplified implementation
        let mut search_pos = 0;
        while let Some(pos) = upper[search_pos..].find("CONCAT(") {
            let actual_pos = search_pos + pos;

            // Find matching parentheses
            if let Some((args, end_pos)) = self.extract_function_args(&sql[actual_pos + 7..]) {
                // Split args by comma (simplified - doesn't handle nested functions well)
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() == 2 {
                    let replacement = format!("({} || {})", parts[0], parts[1]);
                    result = format!(
                        "{}{}{}",
                        &result[..actual_pos],
                        replacement,
                        &result[actual_pos + 7 + end_pos + 1..]
                    );
                }
            }

            search_pos = actual_pos + 1;
            if search_pos >= upper.len() {
                break;
            }
        }

        result
    }

    /// Translate SUBSTR(str, pos, len) to SUBSTRING(str FROM pos FOR len).
    fn translate_substr(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();
        let mut result = sql.to_string();

        let mut search_pos = 0;
        while let Some(pos) = upper[search_pos..].find("SUBSTR(") {
            let actual_pos = search_pos + pos;

            if let Some((args, end_pos)) = self.extract_function_args(&sql[actual_pos + 7..]) {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    let replacement = if parts.len() == 3 {
                        format!(
                            "SUBSTRING({} FROM {} FOR {})",
                            parts[0], parts[1], parts[2]
                        )
                    } else {
                        format!("SUBSTRING({} FROM {})", parts[0], parts[1])
                    };
                    result = format!(
                        "{}{}{}",
                        &result[..actual_pos],
                        replacement,
                        &result[actual_pos + 7 + end_pos + 1..]
                    );
                }
            }

            search_pos = actual_pos + 1;
            if search_pos >= upper.len() {
                break;
            }
        }

        result
    }

    /// Translate CURRENT TIMESTAMP to CURRENT_TIMESTAMP.
    fn translate_current_timestamp(&self, sql: &str) -> String {
        sql.replace("CURRENT TIMESTAMP", "CURRENT_TIMESTAMP")
            .replace("current timestamp", "CURRENT_TIMESTAMP")
    }

    /// Translate CURRENT DATE to CURRENT_DATE.
    fn translate_current_date(&self, sql: &str) -> String {
        sql.replace("CURRENT DATE", "CURRENT_DATE")
            .replace("current date", "CURRENT_DATE")
    }

    /// Translate DB2 functions to PostgreSQL equivalents.
    fn translate_functions(&self, sql: &str) -> String {
        let mut result = sql.to_string();

        for (db2_func, pg_func) in &self.function_map {
            // Case-insensitive replacement
            let pattern = format!("{}(", db2_func);
            let replacement = format!("{}(", pg_func);

            // Simple case-insensitive find and replace
            let upper = result.to_uppercase();
            let pattern_upper = pattern.to_uppercase();

            if let Some(pos) = upper.find(&pattern_upper) {
                result = format!(
                    "{}{}{}",
                    &result[..pos],
                    replacement,
                    &result[pos + pattern.len()..]
                );
            }
        }

        result
    }

    /// Translate DB2 data types to PostgreSQL.
    fn translate_data_types(&self, sql: &str) -> String {
        sql.replace("VARCHAR FOR BIT DATA", "BYTEA")
            .replace("varchar for bit data", "BYTEA")
            .replace("GRAPHIC", "TEXT")
            .replace("VARGRAPHIC", "TEXT")
            .replace("LONG VARCHAR", "TEXT")
            .replace("CLOB", "TEXT")
            .replace("BLOB", "BYTEA")
            .replace("DBCLOB", "TEXT")
    }

    /// Translate FOR UPDATE OF col1, col2 → FOR UPDATE.
    ///
    /// PostgreSQL FOR UPDATE doesn't support column-level specification.
    fn translate_for_update_of(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("FOR UPDATE OF ") {
            let before = &sql[..pos];
            let after = &sql[pos + 14..]; // skip "FOR UPDATE OF "

            // Skip column names until we hit a keyword or end of string
            let end_keywords = ["WHERE", "ORDER", "FETCH", "LIMIT", "WITH", "FOR", "OPTIMIZE"];
            let remaining_upper = after.to_uppercase();
            let mut end = after.len();

            for kw in &end_keywords {
                if let Some(kw_pos) = remaining_upper.find(kw) {
                    if kw_pos < end {
                        end = kw_pos;
                    }
                }
            }

            let after_cols = &after[end..];
            return format!("{}FOR UPDATE {}", before, after_cols);
        }

        sql.to_string()
    }

    /// Translate SET CURRENT SCHEMA = 'X' → SET search_path TO 'X'.
    fn translate_set_current_schema(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("SET CURRENT SCHEMA") {
            let after = &sql[pos + 18..]; // skip "SET CURRENT SCHEMA"
            // Expect optional spaces, then '=' or whitespace, then the value
            let trimmed = after.trim_start();
            let value_start = trimmed.strip_prefix('=').unwrap_or(trimmed);
            let schema_value = value_start.trim();
            return format!("SET search_path TO {schema_value}");
        }

        sql.to_string()
    }

    /// Translate DB2 special registers to PostgreSQL equivalents.
    fn translate_special_registers(&self, sql: &str) -> String {
        let mut result = sql.to_string();

        // CURRENT SQLID → current_schema
        let upper = result.to_uppercase();
        if let Some(pos) = upper.find("CURRENT SQLID") {
            result = format!(
                "{}current_schema{}",
                &result[..pos],
                &result[pos + 13..]
            );
        }

        // CURRENT SCHEMA (as a value, not SET) → current_schema
        let upper = result.to_uppercase();
        if let Some(pos) = upper.find("CURRENT SCHEMA") {
            // Only if not preceded by SET (which is handled separately)
            if pos < 4 || result[..pos].to_uppercase().trim_end() != "SET" {
                result = format!(
                    "{}current_schema{}",
                    &result[..pos],
                    &result[pos + 14..]
                );
            }
        }

        // CURRENT DEGREE → '1' (PostgreSQL is single-node)
        let upper = result.to_uppercase();
        if let Some(pos) = upper.find("CURRENT DEGREE") {
            result = format!("{}'{}'{}",
                &result[..pos],
                "1",
                &result[pos + 14..]
            );
        }

        // CURRENT SERVER → current_database()
        let upper = result.to_uppercase();
        if let Some(pos) = upper.find("CURRENT SERVER") {
            result = format!(
                "{}current_database(){}",
                &result[..pos],
                &result[pos + 14..]
            );
        }

        result
    }

    /// Extract function arguments (finds matching closing parenthesis).
    fn extract_function_args(&self, sql: &str) -> Option<(String, usize)> {
        let mut depth = 1;
        let mut end_pos = 0;

        for (i, c) in sql.chars().enumerate() {
            match c {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end_pos = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        if depth == 0 {
            Some((sql[..end_pos].to_string(), end_pos))
        } else {
            None
        }
    }
}

impl Default for SqlTranslator {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract the text between two delimiters in `haystack`.
///
/// Returns the first occurrence of text between `start_delim` and `end_delim`,
/// trimmed of whitespace.
fn extract_between(haystack: &str, start_delim: &str, end_delim: &str) -> Option<String> {
    let start_pos = haystack.find(start_delim)?;
    let after_start = &haystack[start_pos + start_delim.len()..];
    let end_pos = after_start.find(end_delim)?;
    let value = after_start[..end_pos].trim();
    if value.is_empty() {
        None
    } else {
        Some(value.to_string())
    }
}

/// Extract the conflict/key column from an ON clause.
///
/// Given something like `T.KEY = S.KEY` or `TARGET.ID = SOURCE.ID`,
/// extracts the unqualified column name (`KEY` or `ID`).
fn extract_conflict_column(on_clause: &str) -> Option<String> {
    // Split on '=' and take the left side
    let parts: Vec<&str> = on_clause.split('=').collect();
    if parts.is_empty() {
        return None;
    }
    let left = parts[0].trim();
    // If qualified (e.g., "T.KEY"), take the part after the dot
    let col = if let Some(dot_pos) = left.rfind('.') {
        &left[dot_pos + 1..]
    } else {
        left
    };
    let col = col.trim();
    if col.is_empty() {
        None
    } else {
        Some(col.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fetch_first_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T FETCH FIRST 10 ROWS ONLY";
        let result = translator.translate(sql);
        assert!(result.contains("LIMIT 10"));
        assert!(!result.contains("FETCH FIRST"));
    }

    #[test]
    fn test_fetch_first_single_row() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T FETCH FIRST 1 ROW ONLY";
        let result = translator.translate(sql);
        assert!(result.contains("LIMIT 1"));
    }

    #[test]
    fn test_current_timestamp() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT TIMESTAMP FROM SYSIBM.SYSDUMMY1";
        let result = translator.translate(sql);
        assert!(result.contains("CURRENT_TIMESTAMP"));
    }

    #[test]
    fn test_current_date() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT DATE FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("CURRENT_DATE"));
    }

    #[test]
    fn test_concat_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CONCAT(A, B) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("||"));
    }

    #[test]
    fn test_substr_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT SUBSTR(NAME, 1, 5) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("SUBSTRING"));
        assert!(result.contains("FROM 1 FOR 5"));
    }

    #[test]
    fn test_isolation_removal() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T WITH UR";
        let result = translator.translate(sql);
        assert!(!result.contains("WITH UR"));
    }

    #[test]
    fn test_data_type_translation() {
        let translator = SqlTranslator::new();

        let sql = "CAST(X AS VARCHAR FOR BIT DATA)";
        let result = translator.translate(sql);
        assert!(result.contains("BYTEA"));
    }

    #[test]
    fn test_value_to_coalesce() {
        let translator = SqlTranslator::new();

        let sql = "SELECT VALUE(A, B) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("COALESCE"));
    }

    // --- Epic 308: SQL Dialect Translation Expansion ---

    #[test]
    fn test_for_update_of_single_column() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM EMP FOR UPDATE OF SALARY";
        let result = translator.translate(sql);
        assert!(result.contains("FOR UPDATE"));
        assert!(!result.contains("FOR UPDATE OF"));
    }

    #[test]
    fn test_for_update_of_multiple_columns() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM EMP FOR UPDATE OF SALARY, NAME, DEPT";
        let result = translator.translate(sql);
        assert!(result.contains("FOR UPDATE"));
        assert!(!result.contains("FOR UPDATE OF"));
    }

    #[test]
    fn test_for_update_of_no_change_plain() {
        let translator = SqlTranslator::new();

        // Plain FOR UPDATE (no OF) should stay unchanged
        let sql = "SELECT * FROM EMP FOR UPDATE";
        let result = translator.translate(sql);
        assert!(result.contains("FOR UPDATE"));
    }

    #[test]
    fn test_set_current_schema_with_equals() {
        let translator = SqlTranslator::new();

        let sql = "SET CURRENT SCHEMA = 'MYSCHEMA'";
        let result = translator.translate(sql);
        assert_eq!(result, "SET search_path TO 'MYSCHEMA'");
    }

    #[test]
    fn test_set_current_schema_without_equals() {
        let translator = SqlTranslator::new();

        let sql = "SET CURRENT SCHEMA MYSCHEMA";
        let result = translator.translate(sql);
        assert_eq!(result, "SET search_path TO MYSCHEMA");
    }

    #[test]
    fn test_current_sqlid_register() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT SQLID FROM SYSIBM.SYSDUMMY1";
        let result = translator.translate(sql);
        assert!(result.contains("current_schema"));
        assert!(!result.contains("CURRENT SQLID"));
    }

    #[test]
    fn test_current_degree_register() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT DEGREE FROM SYSIBM.SYSDUMMY1";
        let result = translator.translate(sql);
        assert!(result.contains("'1'"));
        assert!(!result.contains("CURRENT DEGREE"));
    }

    #[test]
    fn test_current_server_register() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT SERVER FROM SYSIBM.SYSDUMMY1";
        let result = translator.translate(sql);
        assert!(result.contains("current_database()"));
        assert!(!result.contains("CURRENT SERVER"));
    }

    #[test]
    fn test_optimize_for_removal() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T OPTIMIZE FOR 100 ROWS";
        let result = translator.translate(sql);
        assert!(!result.contains("OPTIMIZE FOR"));
        assert!(!result.contains("100 ROWS"));
    }

    // --- Epic 310: MERGE Translation ---

    #[test]
    fn test_merge_basic_upsert() {
        let translator = SqlTranslator::new();

        let sql = "MERGE INTO TARGET AS T USING SOURCE AS S ON T.KEY = S.KEY WHEN MATCHED THEN UPDATE SET COL1 = S.COL1 WHEN NOT MATCHED THEN INSERT (COL1) VALUES (S.COL1)";
        let result = translator.translate(sql);
        assert!(result.starts_with("INSERT INTO TARGET"), "got: {}", result);
        assert!(result.contains("ON CONFLICT (KEY) DO UPDATE SET"));
    }

    #[test]
    fn test_merge_insert_only() {
        let translator = SqlTranslator::new();

        // MERGE with only NOT MATCHED (insert-only upsert with conflict ignore)
        let sql = "MERGE INTO ORDERS AS T USING NEW_ORDERS AS S ON T.ID = S.ID WHEN NOT MATCHED THEN INSERT (ID, AMOUNT) VALUES (S.ID, S.AMOUNT)";
        let result = translator.translate(sql);
        assert!(result.starts_with("INSERT INTO ORDERS"), "got: {}", result);
        assert!(result.contains("ON CONFLICT (ID) DO NOTHING"), "got: {}", result);
    }

    #[test]
    fn test_merge_non_merge_passthrough() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T";
        let result = translator.translate(sql);
        // Non-MERGE statements should not be affected
        assert!(!result.contains("ON CONFLICT"));
    }

    #[test]
    fn test_extract_between_basic() {
        let result = extract_between("MERGE INTO TARGET USING", "MERGE INTO ", " ");
        assert_eq!(result, Some("TARGET".to_string()));
    }

    #[test]
    fn test_extract_between_missing() {
        let result = extract_between("SELECT FROM T", "MERGE INTO ", " ");
        assert_eq!(result, None);
    }

    #[test]
    fn test_extract_conflict_column_qualified() {
        let result = extract_conflict_column("T.KEY = S.KEY");
        assert_eq!(result, Some("KEY".to_string()));
    }

    #[test]
    fn test_extract_conflict_column_unqualified() {
        let result = extract_conflict_column("ID = ID");
        assert_eq!(result, Some("ID".to_string()));
    }

    #[test]
    fn test_combined_translation() {
        let translator = SqlTranslator::new();

        // Multiple DB2-isms in one statement
        let sql = "SELECT CURRENT TIMESTAMP, VALUE(A, B) FROM T FETCH FIRST 5 ROWS ONLY";
        let result = translator.translate(sql);
        assert!(result.contains("CURRENT_TIMESTAMP"));
        assert!(result.contains("COALESCE"));
        assert!(result.contains("LIMIT 5"));
        assert!(!result.contains("FETCH FIRST"));
    }
}
