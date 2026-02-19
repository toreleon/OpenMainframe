//! ISPF dialog services — DISPLAY, SELECT, TBDISPL, SETMSG, CONTROL, ADDPOP/REMPOP.
//!
//! Provides the ISPF dialog manager that processes ISPEXEC commands:
//! - DISPLAY PANEL(name) — display a panel and collect input
//! - SELECT PGM(name)/PANEL(name)/CMD(cmd) — invoke a dialog function
//! - TBDISPL table PANEL(name) — display table data with scrolling
//! - SETMSG MSG(id) — set a message for the next panel display
//! - GETMSG MSG(id) — retrieve a message definition
//! - CONTROL ERRORS RETURN — control error handling mode
//! - ADDPOP/REMPOP — manage pop-up windows

use std::collections::HashMap;

use crate::panel::{Panel, PanelStmt, PanelExpr, PanelCond, CmpOp, VerCheck, VarPool as PanelVarPool};

// ---------------------------------------------------------------------------
//  Dialog manager
// ---------------------------------------------------------------------------

/// The ISPF dialog manager — manages panels, variables, messages, and services.
#[derive(Debug)]
pub struct DialogManager {
    /// Panel library: name → parsed panel.
    panels: HashMap<String, Panel>,
    /// Variable pools.
    pub vars: IspfVarPools,
    /// Message library: id → message definition.
    messages: HashMap<String, MessageDef>,
    /// Pending message (set by SETMSG, displayed on next DISPLAY).
    pending_msg: Option<String>,
    /// Control mode: whether errors return to dialog (vs terminate).
    errors_return: bool,
    /// Pop-up stack: (row, col) positions.
    popup_stack: Vec<(u16, u16)>,
    /// Captured output for testing (simulates screen display).
    pub display_log: Vec<DisplayEvent>,
    /// Last return code from a service call.
    pub last_rc: i32,
}

/// A display event captured for testing/logging.
#[derive(Debug, Clone)]
pub enum DisplayEvent {
    /// Panel was displayed.
    PanelDisplay { name: String, fields: HashMap<String, String> },
    /// Message was shown.
    Message { id: String, short: String },
    /// SELECT was invoked.
    Select { target: String },
    /// Pop-up was added.
    PopupAdd { row: u16, col: u16 },
    /// Pop-up was removed.
    PopupRemove,
}

/// ISPF message definition.
#[derive(Debug, Clone)]
pub struct MessageDef {
    /// Message ID (e.g., "ISRZ001").
    pub id: String,
    /// Short message text (shown in message area).
    pub short_msg: String,
    /// Long message text (shown on Help).
    pub long_msg: String,
    /// Alarm: whether to sound the terminal bell.
    pub alarm: bool,
}

// ---------------------------------------------------------------------------
//  ISPF variable pools
// ---------------------------------------------------------------------------

/// The four-pool ISPF variable model.
#[derive(Debug, Default)]
pub struct IspfVarPools {
    /// Function pool stack (one per SELECT level).
    function_stack: Vec<HashMap<String, String>>,
    /// Shared pool (visible across split screens within a session).
    shared: HashMap<String, String>,
    /// Profile pool (persisted across sessions).
    profile: HashMap<String, String>,
    /// System variables (read-only).
    system: HashMap<String, String>,
}

impl IspfVarPools {
    fn new() -> Self {
        let mut sys = HashMap::new();
        sys.insert("ZUSER".to_string(), "USER01".to_string());
        sys.insert("ZPREFIX".to_string(), "USER01".to_string());
        sys.insert("ZDATE".to_string(), "2025/01/01".to_string());
        sys.insert("ZTIME".to_string(), "12:00".to_string());
        sys.insert("ZSCREEN".to_string(), "1".to_string());
        sys.insert("ZSCRMAXD".to_string(), "24".to_string());
        sys.insert("ZSCRMAXW".to_string(), "80".to_string());

        Self {
            function_stack: vec![HashMap::new()],
            shared: HashMap::new(),
            profile: HashMap::new(),
            system: sys,
        }
    }

    /// Get a variable value, searching pools in ISPF order:
    /// function → shared → profile → system.
    pub fn get(&self, name: &str) -> Option<String> {
        let upper = name.to_uppercase();
        // Function pool (current level).
        if let Some(func) = self.function_stack.last() {
            if let Some(v) = func.get(&upper) {
                return Some(v.clone());
            }
        }
        // Shared pool.
        if let Some(v) = self.shared.get(&upper) {
            return Some(v.clone());
        }
        // Profile pool.
        if let Some(v) = self.profile.get(&upper) {
            return Some(v.clone());
        }
        // System pool.
        if let Some(v) = self.system.get(&upper) {
            return Some(v.clone());
        }
        None
    }

    /// Set a variable in the function pool.
    pub fn set(&mut self, name: &str, value: String) {
        let upper = name.to_uppercase();
        if let Some(func) = self.function_stack.last_mut() {
            func.insert(upper, value);
        }
    }

    /// VGET — copy variables from the specified pool to the function pool.
    pub fn vget(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            let value = match pool {
                PanelVarPool::Shared => self.shared.get(&upper).cloned(),
                PanelVarPool::Profile => self.profile.get(&upper).cloned(),
                PanelVarPool::Asis => self.shared.get(&upper).cloned()
                    .or_else(|| self.profile.get(&upper).cloned()),
            };
            // Also check system variables.
            let value = value.or_else(|| self.system.get(&upper).cloned());
            if let Some(v) = value {
                self.set(&upper, v);
            }
        }
    }

    /// VPUT — copy variables from the function pool to the specified pool.
    pub fn vput(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            let value = self.function_stack.last()
                .and_then(|f| f.get(&upper).cloned());
            if let Some(v) = value {
                match pool {
                    PanelVarPool::Shared => { self.shared.insert(upper, v); }
                    PanelVarPool::Profile => { self.profile.insert(upper, v); }
                    PanelVarPool::Asis => { self.shared.insert(upper, v); }
                }
            }
        }
    }

    /// VERASE — remove a variable from the specified pool.
    pub fn verase(&mut self, vars: &[String], pool: PanelVarPool) {
        for var in vars {
            let upper = var.to_uppercase();
            match pool {
                PanelVarPool::Shared => { self.shared.remove(&upper); }
                PanelVarPool::Profile => { self.profile.remove(&upper); }
                PanelVarPool::Asis => {
                    self.shared.remove(&upper);
                    self.profile.remove(&upper);
                }
            }
        }
    }

    /// Push a new function pool (for SELECT).
    pub fn push_function(&mut self) {
        self.function_stack.push(HashMap::new());
    }

    /// Pop the current function pool (returning from SELECT).
    pub fn pop_function(&mut self) {
        if self.function_stack.len() > 1 {
            self.function_stack.pop();
        }
    }
}

// ---------------------------------------------------------------------------
//  Dialog manager implementation
// ---------------------------------------------------------------------------

impl DialogManager {
    /// Create a new dialog manager.
    pub fn new() -> Self {
        Self {
            panels: HashMap::new(),
            vars: IspfVarPools::new(),
            messages: HashMap::new(),
            pending_msg: None,
            errors_return: false,
            popup_stack: Vec::new(),
            display_log: Vec::new(),
            last_rc: 0,
        }
    }

    /// Load a panel into the panel library.
    pub fn load_panel(&mut self, panel: Panel) {
        self.panels.insert(panel.name.to_uppercase(), panel);
    }

    /// Register a message definition.
    pub fn register_message(&mut self, msg: MessageDef) {
        self.messages.insert(msg.id.to_uppercase(), msg);
    }

    /// Execute an ISPEXEC command string.
    pub fn exec(&mut self, cmd: &str) -> i32 {
        let trimmed = cmd.trim();
        let upper = trimmed.to_uppercase();
        let words: Vec<&str> = upper.split_whitespace().collect();

        match words.first().copied() {
            Some("DISPLAY") => self.exec_display(&upper),
            Some("SELECT") => self.exec_select(&upper),
            Some("TBDISPL") => self.exec_tbdispl(&upper),
            Some("SETMSG") => self.exec_setmsg(&upper),
            Some("GETMSG") => self.exec_getmsg(&upper),
            Some("CONTROL") => self.exec_control(&upper),
            Some("ADDPOP") => self.exec_addpop(&upper),
            Some("REMPOP") => self.exec_rempop(),
            Some("VGET") => self.exec_vget(&upper),
            Some("VPUT") => self.exec_vput(&upper),
            Some("VERASE") => self.exec_verase(&upper),
            _ => {
                self.last_rc = 12;
                12
            }
        }
    }

    // -----------------------------------------------------------------------
    //  DISPLAY
    // -----------------------------------------------------------------------

    fn exec_display(&mut self, cmd: &str) -> i32 {
        let panel_name = extract_paren(cmd, "PANEL").unwrap_or_default();

        if let Some(panel) = self.panels.get(&panel_name).cloned() {
            // Execute )INIT section.
            self.exec_panel_stmts(&panel.init);

            // Show pending message if any.
            if let Some(msg_id) = self.pending_msg.take() {
                if let Some(msg) = self.messages.get(&msg_id) {
                    self.display_log.push(DisplayEvent::Message {
                        id: msg_id,
                        short: msg.short_msg.clone(),
                    });
                }
            }

            // Collect field values from variables.
            let fields: HashMap<String, String> = crate::extract_fields(&panel)
                .iter()
                .filter(|f| !f.name.is_empty())
                .map(|f| {
                    let val = self.vars.get(&f.name).unwrap_or_default();
                    (f.name.clone(), val)
                })
                .collect();

            self.display_log.push(DisplayEvent::PanelDisplay {
                name: panel_name,
                fields,
            });

            // Execute )PROC section.
            let proc_rc = self.exec_panel_stmts(&panel.proc_section);
            self.last_rc = proc_rc;
            proc_rc
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  SELECT
    // -----------------------------------------------------------------------

    fn exec_select(&mut self, cmd: &str) -> i32 {
        let target = if let Some(pgm) = extract_paren(cmd, "PGM") {
            format!("PGM({pgm})")
        } else if let Some(panel) = extract_paren(cmd, "PANEL") {
            format!("PANEL({panel})")
        } else if let Some(c) = extract_paren(cmd, "CMD") {
            format!("CMD({c})")
        } else {
            "UNKNOWN".to_string()
        };

        self.vars.push_function();
        self.display_log.push(DisplayEvent::Select { target: target.clone() });

        // If it's a PANEL select, display the panel.
        if let Some(panel_name) = extract_paren(cmd, "PANEL") {
            self.exec_display(&format!("DISPLAY PANEL({panel_name})"));
        }

        self.vars.pop_function();
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  TBDISPL
    // -----------------------------------------------------------------------

    fn exec_tbdispl(&mut self, cmd: &str) -> i32 {
        let _table_name = cmd.split_whitespace().nth(1).unwrap_or("");
        let panel_name = extract_paren(cmd, "PANEL").unwrap_or_default();

        if self.panels.contains_key(&panel_name) || panel_name.is_empty() {
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  SETMSG / GETMSG
    // -----------------------------------------------------------------------

    fn exec_setmsg(&mut self, cmd: &str) -> i32 {
        if let Some(msg_id) = extract_paren(cmd, "MSG") {
            self.pending_msg = Some(msg_id);
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 12;
            12
        }
    }

    fn exec_getmsg(&mut self, cmd: &str) -> i32 {
        if let Some(msg_id) = extract_paren(cmd, "MSG") {
            if let Some(msg) = self.messages.get(&msg_id) {
                self.vars.set("ZERRSM", msg.short_msg.clone());
                self.vars.set("ZERRLM", msg.long_msg.clone());
                self.last_rc = 0;
                0
            } else {
                self.last_rc = 12;
                12
            }
        } else {
            self.last_rc = 12;
            12
        }
    }

    // -----------------------------------------------------------------------
    //  CONTROL
    // -----------------------------------------------------------------------

    fn exec_control(&mut self, cmd: &str) -> i32 {
        if cmd.contains("ERRORS") && cmd.contains("RETURN") {
            self.errors_return = true;
        } else if cmd.contains("ERRORS") && cmd.contains("CANCEL") {
            self.errors_return = false;
        }
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  ADDPOP / REMPOP
    // -----------------------------------------------------------------------

    fn exec_addpop(&mut self, cmd: &str) -> i32 {
        let row = extract_paren(cmd, "ROW")
            .and_then(|v| v.parse::<u16>().ok())
            .unwrap_or(1);
        let col = extract_paren(cmd, "COLUMN")
            .and_then(|v| v.parse::<u16>().ok())
            .unwrap_or(1);
        self.popup_stack.push((row, col));
        self.display_log.push(DisplayEvent::PopupAdd { row, col });
        self.last_rc = 0;
        0
    }

    fn exec_rempop(&mut self) -> i32 {
        if self.popup_stack.pop().is_some() {
            self.display_log.push(DisplayEvent::PopupRemove);
            self.last_rc = 0;
            0
        } else {
            self.last_rc = 8;
            8
        }
    }

    // -----------------------------------------------------------------------
    //  VGET / VPUT / VERASE passthrough
    // -----------------------------------------------------------------------

    fn exec_vget(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VGET");
        self.vars.vget(&vars, pool);
        self.last_rc = 0;
        0
    }

    fn exec_vput(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VPUT");
        self.vars.vput(&vars, pool);
        self.last_rc = 0;
        0
    }

    fn exec_verase(&mut self, cmd: &str) -> i32 {
        let (vars, pool) = parse_var_list_and_pool(cmd, "VERASE");
        self.vars.verase(&vars, pool);
        self.last_rc = 0;
        0
    }

    // -----------------------------------------------------------------------
    //  Panel statement execution
    // -----------------------------------------------------------------------

    fn exec_panel_stmts(&mut self, stmts: &[PanelStmt]) -> i32 {
        for stmt in stmts {
            match stmt {
                PanelStmt::Assign { var, value } => {
                    let val = self.eval_panel_expr(value);
                    self.vars.set(var, val);
                }
                PanelStmt::If { cond, then_stmts, else_stmts } => {
                    if self.eval_panel_cond(cond) {
                        self.exec_panel_stmts(then_stmts);
                    } else {
                        self.exec_panel_stmts(else_stmts);
                    }
                }
                PanelStmt::Ver { field, checks, msg } => {
                    let val = self.vars.get(field).unwrap_or_default();
                    for check in checks {
                        let ok = match check {
                            VerCheck::NonBlank => !val.trim().is_empty(),
                            VerCheck::Numeric => val.chars().all(|c| c.is_ascii_digit() || c == '.' || c == '-'),
                            VerCheck::Alpha => val.chars().all(|c| c.is_ascii_alphabetic()),
                            VerCheck::Dsname => !val.is_empty() && val.len() <= 44,
                            VerCheck::List(items) => items.contains(&val),
                            VerCheck::Range(lo, hi) => val >= *lo && val <= *hi,
                        };
                        if !ok {
                            if let Some(msg_id) = msg {
                                self.pending_msg = Some(msg_id.clone());
                            }
                            return 8; // Verification failed.
                        }
                    }
                }
                PanelStmt::VGet { vars, pool } => {
                    self.vars.vget(vars, *pool);
                }
                PanelStmt::VPut { vars, pool } => {
                    self.vars.vput(vars, *pool);
                }
                PanelStmt::Refresh(_) | PanelStmt::Label(_) | PanelStmt::Goto(_) => {
                    // Stubs for now.
                }
            }
        }
        0
    }

    fn eval_panel_expr(&self, expr: &PanelExpr) -> String {
        match expr {
            PanelExpr::Literal(s) => {
                // Substitute &var references.
                substitute_vars(s, &self.vars)
            }
            PanelExpr::Trans { var, pairs, default } => {
                let val = self.vars.get(var).unwrap_or_default();
                for (key, result) in pairs {
                    if val == *key {
                        return result.clone();
                    }
                }
                default.clone().unwrap_or(val)
            }
            PanelExpr::Trunc { var, delim } => {
                let val = self.vars.get(var).unwrap_or_default();
                if let Some(pos) = val.find(*delim) {
                    val[..pos].to_string()
                } else {
                    val
                }
            }
        }
    }

    fn eval_panel_cond(&self, cond: &PanelCond) -> bool {
        match cond {
            PanelCond::Compare { var, op, value } => {
                let val = self.vars.get(var).unwrap_or_default();
                let rhs = substitute_vars(value, &self.vars);
                match op {
                    CmpOp::Eq => val == rhs,
                    CmpOp::Ne => val != rhs,
                    CmpOp::Gt => val > rhs,
                    CmpOp::Lt => val < rhs,
                    CmpOp::Ge => val >= rhs,
                    CmpOp::Le => val <= rhs,
                }
            }
            PanelCond::Not(inner) => !self.eval_panel_cond(inner),
            PanelCond::And(a, b) => self.eval_panel_cond(a) && self.eval_panel_cond(b),
            PanelCond::Or(a, b) => self.eval_panel_cond(a) || self.eval_panel_cond(b),
        }
    }
}

impl Default for DialogManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Extract parenthesized value: `KEY(VALUE)` → `"VALUE"`.
fn extract_paren(text: &str, key: &str) -> Option<String> {
    let pat = format!("{key}(");
    if let Some(start) = text.find(&pat) {
        let after = &text[start + pat.len()..];
        if let Some(end) = after.find(')') {
            return Some(after[..end].to_string());
        }
    }
    None
}

/// Parse variable list and pool from VGET/VPUT/VERASE command.
fn parse_var_list_and_pool(cmd: &str, keyword: &str) -> (Vec<String>, PanelVarPool) {
    let after = cmd.find(keyword)
        .map(|i| &cmd[i + keyword.len()..])
        .unwrap_or("");

    let vars_str = if let (Some(start), Some(end)) = (after.find('('), after.find(')')) {
        &after[start + 1..end]
    } else {
        ""
    };

    let vars: Vec<String> = vars_str
        .split_whitespace()
        .map(|s| s.trim_start_matches('&').to_uppercase())
        .filter(|s| !s.is_empty())
        .collect();

    let remainder = after.rfind(')').map(|i| &after[i + 1..]).unwrap_or("");
    let pool = if remainder.contains("PROFILE") {
        PanelVarPool::Profile
    } else if remainder.contains("ASIS") {
        PanelVarPool::Asis
    } else {
        PanelVarPool::Shared
    };

    (vars, pool)
}

/// Substitute `&var` references in a string with variable values.
fn substitute_vars(text: &str, vars: &IspfVarPools) -> String {
    let mut result = String::new();
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '&' {
            let start = i + 1;
            let mut end = start;
            while end < chars.len() && (chars[end].is_ascii_alphanumeric() || chars[end] == '_') {
                end += 1;
            }
            if end > start {
                let var_name: String = chars[start..end].iter().collect();
                let val = vars.get(&var_name).unwrap_or_default();
                result.push_str(&val);
                // Skip trailing period if present (ISPF concatenation).
                if end < chars.len() && chars[end] == '.' {
                    end += 1;
                }
                i = end;
            } else {
                result.push('&');
                i += 1;
            }
        } else if chars[i] == '\'' {
            // Skip quotes.
            i += 1;
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::panel::parse_panel;

    #[test]
    fn test_dialog_manager_display() {
        let source = r#")ATTR DEFAULT(%+_)
% TYPE(TEXT) INTENS(HIGH)
_ TYPE(INPUT) INTENS(LOW)
)BODY
%COMMAND ===>_ZCMD
)INIT
  &ZCMD = ''
)PROC
)END
"#;
        let panel = parse_panel("MYPANEL", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        let rc = dm.exec("DISPLAY PANEL(MYPANEL)");
        assert_eq!(rc, 0);
        assert!(!dm.display_log.is_empty());
    }

    #[test]
    fn test_dialog_manager_setmsg() {
        let mut dm = DialogManager::new();
        dm.register_message(MessageDef {
            id: "ISRZ001".to_string(),
            short_msg: "Error".to_string(),
            long_msg: "An error occurred".to_string(),
            alarm: false,
        });

        let source = r#")ATTR DEFAULT(%+_)
)BODY
%Test Panel
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        dm.load_panel(panel);

        dm.exec("SETMSG MSG(ISRZ001)");
        dm.exec("DISPLAY PANEL(TEST)");

        let has_msg = dm.display_log.iter().any(|e| matches!(e, DisplayEvent::Message { id, .. } if id == "ISRZ001"));
        assert!(has_msg);
    }

    #[test]
    fn test_dialog_manager_select() {
        let mut dm = DialogManager::new();
        let rc = dm.exec("SELECT PGM(ISRBRO)");
        assert_eq!(rc, 0);
        let has_select = dm.display_log.iter().any(|e| matches!(e, DisplayEvent::Select { target } if target.contains("ISRBRO")));
        assert!(has_select);
    }

    #[test]
    fn test_dialog_manager_control() {
        let mut dm = DialogManager::new();
        dm.exec("CONTROL ERRORS RETURN");
        assert!(dm.errors_return);
        dm.exec("CONTROL ERRORS CANCEL");
        assert!(!dm.errors_return);
    }

    #[test]
    fn test_dialog_manager_addpop_rempop() {
        let mut dm = DialogManager::new();
        dm.exec("ADDPOP ROW(5) COLUMN(10)");
        assert_eq!(dm.popup_stack.len(), 1);
        assert_eq!(dm.popup_stack[0], (5, 10));
        dm.exec("REMPOP");
        assert_eq!(dm.popup_stack.len(), 0);
    }

    #[test]
    fn test_var_pools_function_scope() {
        let mut vars = IspfVarPools::new();
        vars.set("MYVAR", "hello".into());
        assert_eq!(vars.get("MYVAR"), Some("hello".to_string()));

        vars.push_function();
        // New function pool doesn't see parent.
        assert_eq!(vars.get("MYVAR"), None);
        vars.set("MYVAR", "world".into());
        assert_eq!(vars.get("MYVAR"), Some("world".to_string()));

        vars.pop_function();
        // Back to original.
        assert_eq!(vars.get("MYVAR"), Some("hello".to_string()));
    }

    #[test]
    fn test_var_pools_vget_vput() {
        let mut vars = IspfVarPools::new();
        vars.set("DSN", "MY.DATA".into());
        vars.vput(&["DSN".into()], PanelVarPool::Shared);

        vars.push_function();
        assert_eq!(vars.get("DSN"), Some("MY.DATA".to_string())); // Found in shared.
        vars.vget(&["DSN".into()], PanelVarPool::Shared);
        // Now it's also in the function pool.
        assert_eq!(vars.function_stack.last().unwrap().get("DSN"), Some(&"MY.DATA".to_string()));

        vars.pop_function();
    }

    #[test]
    fn test_var_pools_system_vars() {
        let vars = IspfVarPools::new();
        assert_eq!(vars.get("ZUSER"), Some("USER01".to_string()));
        assert_eq!(vars.get("ZPREFIX"), Some("USER01".to_string()));
    }

    #[test]
    fn test_var_pools_verase() {
        let mut vars = IspfVarPools::new();
        vars.shared.insert("TEMPVAR".into(), "temp".into());
        assert_eq!(vars.get("TEMPVAR"), Some("temp".to_string()));
        vars.verase(&["TEMPVAR".into()], PanelVarPool::Shared);
        assert_eq!(vars.get("TEMPVAR"), None);
    }

    #[test]
    fn test_substitute_vars() {
        let mut vars = IspfVarPools::new();
        vars.set("PREFIX", "USER01".into());
        let result = substitute_vars("&PREFIX..DATA", &vars);
        assert_eq!(result, "USER01.DATA");
    }

    #[test]
    fn test_panel_init_vget() {
        let source = r#")ATTR DEFAULT(%+_)
)BODY
%Test
)INIT
  VGET (ZUSER) SHARED
  &WHO = &ZUSER
)PROC
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        dm.exec("DISPLAY PANEL(TEST)");
        assert_eq!(dm.vars.get("WHO"), Some("USER01".to_string()));
    }

    #[test]
    fn test_getmsg() {
        let mut dm = DialogManager::new();
        dm.register_message(MessageDef {
            id: "TST001".to_string(),
            short_msg: "Short".to_string(),
            long_msg: "Long message".to_string(),
            alarm: false,
        });

        let rc = dm.exec("GETMSG MSG(TST001)");
        assert_eq!(rc, 0);
        assert_eq!(dm.vars.get("ZERRSM"), Some("Short".to_string()));
        assert_eq!(dm.vars.get("ZERRLM"), Some("Long message".to_string()));
    }

    #[test]
    fn test_panel_ver_nonblank() {
        let source = r#")ATTR DEFAULT(%+_)
_ TYPE(INPUT) INTENS(LOW)
)BODY
_DSN
)PROC
  VER (&DSN,NB,MSG=ISRZ002)
)END
"#;
        let panel = parse_panel("TEST", source).unwrap();
        let mut dm = DialogManager::new();
        dm.load_panel(panel);
        // DSN is empty by default — VER should fail.
        let rc = dm.exec("DISPLAY PANEL(TEST)");
        assert_eq!(rc, 8);
    }
}
