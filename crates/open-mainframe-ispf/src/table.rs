//! ISPF table services — in-memory tables with row operations, search, sort, and persistence.
//!
//! Services:
//! - **Lifecycle**: TBCREATE, TBOPEN, TBCLOSE, TBEND, TBSAVE
//! - **Row ops**: TBADD, TBPUT, TBMOD, TBDELETE, TBTOP, TBBOT, TBSKIP
//! - **Search**: TBSCAN, TBSARG, TBSORT
//! - **Query**: TBQUERY, TBSTATS, TBEXIST

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Table definition
// ---------------------------------------------------------------------------

/// An ISPF in-memory table.
#[derive(Debug, Clone)]
pub struct IspfTable {
    /// Table name.
    pub name: String,
    /// Key column names.
    pub keys: Vec<String>,
    /// Non-key column names (NAMES).
    pub names: Vec<String>,
    /// Rows: each row is a map from column name → value.
    pub rows: Vec<HashMap<String, String>>,
    /// Current Row Pointer (1-based, 0 = "before top").
    pub crp: usize,
    /// Whether the table is writable.
    pub writable: bool,
    /// Whether the table has been modified since last save.
    pub modified: bool,
    /// Sort specification (if any): list of (column, ascending).
    sort_spec: Vec<(String, bool)>,
    /// Search arguments set by TBSARG.
    search_args: Vec<(String, SearchCond, String)>,
}

/// Search condition for TBSCAN/TBSARG.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchCond {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

impl SearchCond {
    fn from_str(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "NE" => SearchCond::Ne,
            "GT" => SearchCond::Gt,
            "LT" => SearchCond::Lt,
            "GE" => SearchCond::Ge,
            "LE" => SearchCond::Le,
            _ => SearchCond::Eq,
        }
    }

    fn eval(&self, left: &str, right: &str) -> bool {
        match self {
            SearchCond::Eq => left == right,
            SearchCond::Ne => left != right,
            SearchCond::Gt => left > right,
            SearchCond::Lt => left < right,
            SearchCond::Ge => left >= right,
            SearchCond::Le => left <= right,
        }
    }
}

// ---------------------------------------------------------------------------
//  Table manager
// ---------------------------------------------------------------------------

/// Manages a collection of ISPF tables.
#[derive(Debug, Default)]
pub struct TableManager {
    /// Open tables: name → table.
    tables: HashMap<String, IspfTable>,
    /// Persisted tables (simulated ISPTLIB/ISPTABL): name → serialised rows.
    persisted: HashMap<String, PersistedTable>,
}

/// Serialised table for persistence.
#[derive(Debug, Clone)]
struct PersistedTable {
    keys: Vec<String>,
    names: Vec<String>,
    rows: Vec<HashMap<String, String>>,
}

impl TableManager {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a reference to an open table.
    pub fn get(&self, name: &str) -> Option<&IspfTable> {
        self.tables.get(&name.to_uppercase())
    }

    /// Get a mutable reference to an open table.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut IspfTable> {
        self.tables.get_mut(&name.to_uppercase())
    }

    // -----------------------------------------------------------------------
    //  Lifecycle
    // -----------------------------------------------------------------------

    /// TBCREATE — create a new in-memory table.
    pub fn tbcreate(
        &mut self,
        name: &str,
        keys: &[String],
        names: &[String],
        writable: bool,
    ) -> i32 {
        let upper = name.to_uppercase();
        if self.tables.contains_key(&upper) {
            return 12; // Already exists.
        }
        let table = IspfTable {
            name: upper.clone(),
            keys: keys.iter().map(|s| s.to_uppercase()).collect(),
            names: names.iter().map(|s| s.to_uppercase()).collect(),
            rows: Vec::new(),
            crp: 0,
            writable,
            modified: false,
            sort_spec: Vec::new(),
            search_args: Vec::new(),
        };
        self.tables.insert(upper, table);
        0
    }

    /// TBOPEN — open a persisted table into memory.
    pub fn tbopen(&mut self, name: &str, writable: bool) -> i32 {
        let upper = name.to_uppercase();
        if self.tables.contains_key(&upper) {
            return 12; // Already open.
        }
        if let Some(persisted) = self.persisted.get(&upper) {
            let table = IspfTable {
                name: upper.clone(),
                keys: persisted.keys.clone(),
                names: persisted.names.clone(),
                rows: persisted.rows.clone(),
                crp: 0,
                writable,
                modified: false,
                sort_spec: Vec::new(),
                search_args: Vec::new(),
            };
            self.tables.insert(upper, table);
            0
        } else {
            8 // Table not found in library.
        }
    }

    /// TBCLOSE — close a table, saving it if writable.
    pub fn tbclose(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.remove(&upper) {
            if table.writable {
                self.persisted.insert(upper, PersistedTable {
                    keys: table.keys,
                    names: table.names,
                    rows: table.rows,
                });
            }
            0
        } else {
            12
        }
    }

    /// TBEND — close a table without saving changes.
    pub fn tbend(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if self.tables.remove(&upper).is_some() {
            0
        } else {
            12
        }
    }

    /// TBSAVE — save the table to persistent storage without closing.
    pub fn tbsave(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            if !table.writable {
                return 12;
            }
            self.persisted.insert(upper, PersistedTable {
                keys: table.keys.clone(),
                names: table.names.clone(),
                rows: table.rows.clone(),
            });
            table.modified = false;
            0
        } else {
            12
        }
    }

    // -----------------------------------------------------------------------
    //  Row operations
    // -----------------------------------------------------------------------

    /// TBADD — add a row to the table from the provided variable map.
    pub fn tbadd(&mut self, name: &str, vars: &HashMap<String, String>) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            let row = table.build_row(vars);

            // Check for duplicate key.
            if !table.keys.is_empty() {
                let dup = table.rows.iter().any(|r| {
                    table.keys.iter().all(|k| r.get(k) == row.get(k))
                });
                if dup {
                    return 8; // Duplicate key.
                }
            }

            // Insert after CRP position (or at end if CRP is 0).
            let pos = if table.crp == 0 || table.crp >= table.rows.len() {
                table.rows.len()
            } else {
                table.crp
            };
            table.rows.insert(pos, row);
            table.crp = pos + 1;
            table.modified = true;
            0
        } else {
            12
        }
    }

    /// TBPUT — update the row at CRP with current variable values.
    pub fn tbput(&mut self, name: &str, vars: &HashMap<String, String>) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            if table.crp == 0 || table.crp > table.rows.len() {
                return 8; // No current row.
            }
            let row = table.build_row(vars);
            table.rows[table.crp - 1] = row;
            table.modified = true;
            0
        } else {
            12
        }
    }

    /// TBMOD — modify (upsert): update existing row by key or add new row.
    pub fn tbmod(&mut self, name: &str, vars: &HashMap<String, String>) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            let row = table.build_row(vars);

            if !table.keys.is_empty() {
                if let Some(idx) = table.rows.iter().position(|r| {
                    table.keys.iter().all(|k| r.get(k) == row.get(k))
                }) {
                    table.rows[idx] = row;
                    table.crp = idx + 1;
                    table.modified = true;
                    return 0;
                }
            }

            // Not found — add.
            table.rows.push(row);
            table.crp = table.rows.len();
            table.modified = true;
            0
        } else {
            12
        }
    }

    /// TBDELETE — delete the row at CRP.
    pub fn tbdelete(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            if table.crp == 0 || table.crp > table.rows.len() {
                return 8;
            }
            table.rows.remove(table.crp - 1);
            table.modified = true;
            // CRP stays at same position (now points to next row, or past end).
            if table.crp > table.rows.len() {
                table.crp = table.rows.len();
            }
            0
        } else {
            12
        }
    }

    /// TBTOP — position CRP before the first row.
    pub fn tbtop(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            table.crp = 0;
            0
        } else {
            12
        }
    }

    /// TBBOT — position CRP to the last row.
    pub fn tbbot(&mut self, name: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            table.crp = table.rows.len();
            0
        } else {
            12
        }
    }

    /// TBSKIP — advance CRP by `count` rows. Returns the row data or 8 if past end.
    pub fn tbskip(&mut self, name: &str, count: i32) -> (i32, Option<HashMap<String, String>>) {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            let new_crp = table.crp as i32 + count;
            if new_crp < 1 || new_crp as usize > table.rows.len() {
                return (8, None);
            }
            table.crp = new_crp as usize;
            let row = table.rows[table.crp - 1].clone();
            (0, Some(row))
        } else {
            (12, None)
        }
    }

    // -----------------------------------------------------------------------
    //  Search
    // -----------------------------------------------------------------------

    /// TBSARG — set search arguments for subsequent TBSCAN calls.
    pub fn tbsarg(
        &mut self,
        name: &str,
        arglist: &[String],
        condlist: &[String],
        vars: &HashMap<String, String>,
    ) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            table.search_args.clear();
            for (i, col) in arglist.iter().enumerate() {
                let col_upper = col.to_uppercase();
                let cond = condlist.get(i)
                    .map(|s| SearchCond::from_str(s))
                    .unwrap_or(SearchCond::Eq);
                let val = vars.get(&col_upper).cloned().unwrap_or_default();
                table.search_args.push((col_upper, cond, val));
            }
            0
        } else {
            12
        }
    }

    /// TBSCAN — search for the next matching row starting after CRP.
    /// Uses either provided arglist or previously set TBSARG arguments.
    pub fn tbscan(
        &mut self,
        name: &str,
        arglist: Option<(&[String], &[String])>,
        vars: &HashMap<String, String>,
    ) -> (i32, Option<HashMap<String, String>>) {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            // Build search criteria.
            let criteria: Vec<(String, SearchCond, String)> = if let Some((cols, conds)) = arglist {
                cols.iter().enumerate().map(|(i, col)| {
                    let col_upper = col.to_uppercase();
                    let cond = conds.get(i)
                        .map(|s| SearchCond::from_str(s))
                        .unwrap_or(SearchCond::Eq);
                    let val = vars.get(&col_upper).cloned().unwrap_or_default();
                    (col_upper, cond, val)
                }).collect()
            } else {
                table.search_args.clone()
            };

            if criteria.is_empty() {
                return (20, None); // No search criteria.
            }

            // Search from CRP+1 forward.
            let start = table.crp;
            for i in start..table.rows.len() {
                let row = &table.rows[i];
                let matches = criteria.iter().all(|(col, cond, val)| {
                    let row_val = row.get(col).map(|s| s.as_str()).unwrap_or("");
                    cond.eval(row_val, val)
                });
                if matches {
                    table.crp = i + 1;
                    return (0, Some(row.clone()));
                }
            }
            (8, None) // Not found.
        } else {
            (12, None)
        }
    }

    /// TBSORT — sort the table by the given field specification.
    /// Spec format: `"COL1,C,A,COL2,C,D"` — column, type, order triplets.
    pub fn tbsort(&mut self, name: &str, spec: &str) -> i32 {
        let upper = name.to_uppercase();
        if let Some(table) = self.tables.get_mut(&upper) {
            // Parse spec: "COL1,C,A,COL2,C,D"
            let parts: Vec<&str> = spec.split(',').map(|s| s.trim()).collect();
            let mut sort_fields: Vec<(String, bool)> = Vec::new();

            let mut i = 0;
            while i + 2 < parts.len() {
                let col = parts[i].to_uppercase();
                // parts[i+1] is type (C/N), parts[i+2] is order (A/D)
                let ascending = !parts[i + 2].eq_ignore_ascii_case("D");
                sort_fields.push((col, ascending));
                i += 3;
            }

            // Handle partial triplets: just column name with defaults.
            if i < parts.len() {
                let col = parts[i].to_uppercase();
                sort_fields.push((col, true));
            }

            table.sort_spec = sort_fields.clone();

            table.rows.sort_by(|a, b| {
                for (col, ascending) in &sort_fields {
                    let av = a.get(col).map(|s| s.as_str()).unwrap_or("");
                    let bv = b.get(col).map(|s| s.as_str()).unwrap_or("");
                    let cmp = av.cmp(bv);
                    if cmp != std::cmp::Ordering::Equal {
                        return if *ascending { cmp } else { cmp.reverse() };
                    }
                }
                std::cmp::Ordering::Equal
            });

            table.crp = 0;
            0
        } else {
            12
        }
    }

    // -----------------------------------------------------------------------
    //  Query
    // -----------------------------------------------------------------------

    /// TBQUERY — return table statistics.
    pub fn tbquery(&self, name: &str) -> Option<TableStats> {
        let upper = name.to_uppercase();
        self.tables.get(&upper).map(|t| TableStats {
            name: t.name.clone(),
            key_count: t.keys.len(),
            name_count: t.names.len(),
            row_count: t.rows.len(),
            crp: t.crp,
            sort_fields: t.sort_spec.clone(),
        })
    }

    /// TBEXIST — check if a table exists (is open).
    pub fn tbexist(&self, name: &str) -> bool {
        self.tables.contains_key(&name.to_uppercase())
    }
}

/// Table statistics returned by TBQUERY.
#[derive(Debug, Clone)]
pub struct TableStats {
    pub name: String,
    pub key_count: usize,
    pub name_count: usize,
    pub row_count: usize,
    pub crp: usize,
    pub sort_fields: Vec<(String, bool)>,
}

impl IspfTable {
    /// Build a row from a variable map, extracting only the table's columns.
    fn build_row(&self, vars: &HashMap<String, String>) -> HashMap<String, String> {
        let mut row = HashMap::new();
        for col in self.keys.iter().chain(self.names.iter()) {
            if let Some(val) = vars.get(col) {
                row.insert(col.clone(), val.clone());
            } else {
                row.insert(col.clone(), String::new());
            }
        }
        row
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_vars(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs.iter().map(|(k, v)| (k.to_uppercase(), v.to_string())).collect()
    }

    #[test]
    fn test_tbcreate_and_tbadd() {
        let mut tm = TableManager::new();
        let rc = tm.tbcreate("MYTAB", &["KEY1".into()], &["COL1".into(), "COL2".into()], true);
        assert_eq!(rc, 0);

        let vars = make_vars(&[("KEY1", "A001"), ("COL1", "Smith"), ("COL2", "NY")]);
        let rc = tm.tbadd("MYTAB", &vars);
        assert_eq!(rc, 0);
        assert_eq!(tm.get("MYTAB").unwrap().rows.len(), 1);
    }

    #[test]
    fn test_tbadd_duplicate_key() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &["K".into()], &["V".into()], true);

        let vars = make_vars(&[("K", "1"), ("V", "first")]);
        assert_eq!(tm.tbadd("T", &vars), 0);

        let vars2 = make_vars(&[("K", "1"), ("V", "second")]);
        assert_eq!(tm.tbadd("T", &vars2), 8); // Duplicate.
    }

    #[test]
    fn test_tbput_updates_crp_row() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["NAME".into()], true);

        let vars = make_vars(&[("NAME", "Alice")]);
        tm.tbadd("T", &vars);

        let vars2 = make_vars(&[("NAME", "Bob")]);
        let rc = tm.tbput("T", &vars2);
        assert_eq!(rc, 0);
        assert_eq!(tm.get("T").unwrap().rows[0].get("NAME").unwrap(), "Bob");
    }

    #[test]
    fn test_tbmod_upsert() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &["K".into()], &["V".into()], true);

        // Insert via tbmod.
        let vars = make_vars(&[("K", "X"), ("V", "one")]);
        assert_eq!(tm.tbmod("T", &vars), 0);
        assert_eq!(tm.get("T").unwrap().rows.len(), 1);

        // Update via tbmod (same key).
        let vars2 = make_vars(&[("K", "X"), ("V", "two")]);
        assert_eq!(tm.tbmod("T", &vars2), 0);
        assert_eq!(tm.get("T").unwrap().rows.len(), 1);
        assert_eq!(tm.get("T").unwrap().rows[0].get("V").unwrap(), "two");
    }

    #[test]
    fn test_tbdelete() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["N".into()], true);
        tm.tbadd("T", &make_vars(&[("N", "a")]));
        tm.tbadd("T", &make_vars(&[("N", "b")]));
        assert_eq!(tm.get("T").unwrap().rows.len(), 2);

        tm.tbtop("T");
        tm.tbskip("T", 1);
        assert_eq!(tm.tbdelete("T"), 0);
        assert_eq!(tm.get("T").unwrap().rows.len(), 1);
        assert_eq!(tm.get("T").unwrap().rows[0].get("N").unwrap(), "b");
    }

    #[test]
    fn test_tbtop_tbbot_tbskip() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["V".into()], true);
        for i in 1..=5 {
            tm.tbadd("T", &make_vars(&[("V", &i.to_string())]));
        }

        tm.tbtop("T");
        assert_eq!(tm.get("T").unwrap().crp, 0);

        let (rc, row) = tm.tbskip("T", 1);
        assert_eq!(rc, 0);
        assert_eq!(row.unwrap().get("V").unwrap(), "1");

        tm.tbbot("T");
        assert_eq!(tm.get("T").unwrap().crp, 5);
    }

    #[test]
    fn test_tbscan() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["NAME".into(), "CITY".into()], true);
        tm.tbadd("T", &make_vars(&[("NAME", "Alice"), ("CITY", "NY")]));
        tm.tbadd("T", &make_vars(&[("NAME", "Bob"), ("CITY", "LA")]));
        tm.tbadd("T", &make_vars(&[("NAME", "Carol"), ("CITY", "NY")]));

        tm.tbtop("T");
        let search_vars = make_vars(&[("CITY", "NY")]);
        let (rc, row) = tm.tbscan("T",
            Some((&["CITY".into()], &["EQ".into()])),
            &search_vars,
        );
        assert_eq!(rc, 0);
        assert_eq!(row.unwrap().get("NAME").unwrap(), "Alice");

        // Next scan finds Carol.
        let (rc, row) = tm.tbscan("T",
            Some((&["CITY".into()], &["EQ".into()])),
            &search_vars,
        );
        assert_eq!(rc, 0);
        assert_eq!(row.unwrap().get("NAME").unwrap(), "Carol");

        // No more.
        let (rc, _) = tm.tbscan("T",
            Some((&["CITY".into()], &["EQ".into()])),
            &search_vars,
        );
        assert_eq!(rc, 8);
    }

    #[test]
    fn test_tbsarg_tbscan() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["X".into()], true);
        tm.tbadd("T", &make_vars(&[("X", "10")]));
        tm.tbadd("T", &make_vars(&[("X", "20")]));
        tm.tbadd("T", &make_vars(&[("X", "30")]));

        let vars = make_vars(&[("X", "20")]);
        tm.tbsarg("T", &["X".into()], &["GE".into()], &vars);

        tm.tbtop("T");
        let (rc, row) = tm.tbscan("T", None, &HashMap::new());
        assert_eq!(rc, 0);
        assert_eq!(row.unwrap().get("X").unwrap(), "20");
    }

    #[test]
    fn test_tbsort() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["NAME".into(), "AGE".into()], true);
        tm.tbadd("T", &make_vars(&[("NAME", "Carol"), ("AGE", "30")]));
        tm.tbadd("T", &make_vars(&[("NAME", "Alice"), ("AGE", "25")]));
        tm.tbadd("T", &make_vars(&[("NAME", "Bob"), ("AGE", "35")]));

        let rc = tm.tbsort("T", "NAME,C,A");
        assert_eq!(rc, 0);

        let table = tm.get("T").unwrap();
        assert_eq!(table.rows[0].get("NAME").unwrap(), "Alice");
        assert_eq!(table.rows[1].get("NAME").unwrap(), "Bob");
        assert_eq!(table.rows[2].get("NAME").unwrap(), "Carol");
    }

    #[test]
    fn test_tbsort_descending() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["V".into()], true);
        tm.tbadd("T", &make_vars(&[("V", "A")]));
        tm.tbadd("T", &make_vars(&[("V", "C")]));
        tm.tbadd("T", &make_vars(&[("V", "B")]));

        tm.tbsort("T", "V,C,D");
        let table = tm.get("T").unwrap();
        assert_eq!(table.rows[0].get("V").unwrap(), "C");
        assert_eq!(table.rows[1].get("V").unwrap(), "B");
        assert_eq!(table.rows[2].get("V").unwrap(), "A");
    }

    #[test]
    fn test_tbsave_and_tbopen() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &["K".into()], &["V".into()], true);
        tm.tbadd("T", &make_vars(&[("K", "1"), ("V", "hello")]));
        tm.tbsave("T");
        tm.tbclose("T");

        // Reopen.
        let rc = tm.tbopen("T", false);
        assert_eq!(rc, 0);
        let table = tm.get("T").unwrap();
        assert_eq!(table.rows.len(), 1);
        assert_eq!(table.rows[0].get("V").unwrap(), "hello");
    }

    #[test]
    fn test_tbend_discards_changes() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["V".into()], true);
        tm.tbadd("T", &make_vars(&[("V", "data")]));
        tm.tbend("T"); // Discard.

        // Can't reopen — never saved.
        assert_eq!(tm.tbopen("T", false), 8);
    }

    #[test]
    fn test_tbclose_saves_writable() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["V".into()], true);
        tm.tbadd("T", &make_vars(&[("V", "saved")]));
        tm.tbclose("T");

        let rc = tm.tbopen("T", false);
        assert_eq!(rc, 0);
        assert_eq!(tm.get("T").unwrap().rows.len(), 1);
    }

    #[test]
    fn test_tbclose_nowrite_not_persisted() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &["V".into()], false); // NOWRITE.
        tm.tbadd("T", &make_vars(&[("V", "temp")]));
        tm.tbclose("T");

        assert_eq!(tm.tbopen("T", false), 8); // Not persisted.
    }

    #[test]
    fn test_tbquery() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &["K".into()], &["A".into(), "B".into()], true);
        tm.tbadd("T", &make_vars(&[("K", "1"), ("A", "x"), ("B", "y")]));
        tm.tbadd("T", &make_vars(&[("K", "2"), ("A", "m"), ("B", "n")]));

        let stats = tm.tbquery("T").unwrap();
        assert_eq!(stats.key_count, 1);
        assert_eq!(stats.name_count, 2);
        assert_eq!(stats.row_count, 2);
    }

    #[test]
    fn test_tbexist() {
        let mut tm = TableManager::new();
        assert!(!tm.tbexist("T"));
        tm.tbcreate("T", &[], &[], true);
        assert!(tm.tbexist("T"));
    }

    #[test]
    fn test_tbcreate_duplicate() {
        let mut tm = TableManager::new();
        tm.tbcreate("T", &[], &[], true);
        assert_eq!(tm.tbcreate("T", &[], &[], true), 12); // Duplicate.
    }
}
