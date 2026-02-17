//! CICS command inventory and classification.
//!
//! Scans COBOL source for EXEC CICS commands, classifies them by category,
//! and reports support status against the open-mainframe-cics implementation.

use std::collections::HashMap;

/// CICS command category.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
pub enum CicsCategory {
    /// File control: READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, READPREV, ENDBR, UNLOCK
    FileControl,
    /// Terminal control: SEND MAP, RECEIVE MAP, SEND TEXT, SEND CONTROL
    TerminalControl,
    /// Program control: LINK, XCTL, RETURN, LOAD, RELEASE
    ProgramControl,
    /// Queue: WRITEQ TS, READQ TS, DELETEQ TS, WRITEQ TD, READQ TD
    QueueControl,
    /// Interval control: START, CANCEL, DELAY, ASKTIME, FORMATTIME
    IntervalControl,
    /// Task control: SUSPEND, ENQ, DEQ, ASSIGN
    TaskControl,
    /// Storage: GETMAIN, FREEMAIN
    StorageControl,
    /// BMS: SEND MAP, RECEIVE MAP, SEND PAGE, PURGE MESSAGE
    BmsControl,
    /// Journaling: WRITE JOURNALNAME
    JournalControl,
    /// Syncpoint: SYNCPOINT, SYNCPOINT ROLLBACK
    SyncpointControl,
    /// Exception handling: HANDLE CONDITION, HANDLE AID, HANDLE ABEND, IGNORE CONDITION, PUSH, POP
    ExceptionHandling,
    /// Other/Unknown
    Other,
}

impl CicsCategory {
    /// Display name for the category.
    pub fn name(&self) -> &'static str {
        match self {
            CicsCategory::FileControl => "File Control",
            CicsCategory::TerminalControl => "Terminal Control",
            CicsCategory::ProgramControl => "Program Control",
            CicsCategory::QueueControl => "Queue Control",
            CicsCategory::IntervalControl => "Interval Control",
            CicsCategory::TaskControl => "Task Control",
            CicsCategory::StorageControl => "Storage Control",
            CicsCategory::BmsControl => "BMS Control",
            CicsCategory::JournalControl => "Journal Control",
            CicsCategory::SyncpointControl => "Syncpoint Control",
            CicsCategory::ExceptionHandling => "Exception Handling",
            CicsCategory::Other => "Other",
        }
    }
}

/// Support status for a CICS command.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum SupportStatus {
    /// Fully supported by open-mainframe-cics.
    Supported,
    /// Partially supported (some options may not work).
    Partial,
    /// Not yet implemented.
    Unsupported,
}

impl SupportStatus {
    /// Display label.
    pub fn label(&self) -> &'static str {
        match self {
            SupportStatus::Supported => "Supported",
            SupportStatus::Partial => "Partial",
            SupportStatus::Unsupported => "Unsupported",
        }
    }
}

/// A classified CICS command occurrence.
#[derive(Debug, Clone, serde::Serialize)]
pub struct CicsCommand {
    /// The command verb (e.g., "READ", "SEND MAP").
    pub verb: String,
    /// Category classification.
    pub category: CicsCategory,
    /// Number of occurrences.
    pub count: usize,
    /// Source lines where the command appears.
    pub lines: Vec<usize>,
    /// Support status in open-mainframe-cics.
    pub support: SupportStatus,
}

/// CICS command inventory for a program.
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct CicsInventory {
    /// All commands found, keyed by verb.
    pub commands: HashMap<String, CicsCommand>,
    /// Total command count.
    pub total_count: usize,
}

impl CicsInventory {
    /// Create a new empty inventory.
    pub fn new() -> Self {
        Self::default()
    }

    /// Scan COBOL source and classify all EXEC CICS commands.
    pub fn from_source(source: &str) -> Self {
        let mut inv = CicsInventory::new();
        let upper = source.to_uppercase();

        for (line_num, line) in upper.lines().enumerate() {
            let line_num = line_num + 1; // 1-based

            // Skip comments
            if line.len() > 6 && line.as_bytes().get(6) == Some(&b'*') {
                continue;
            }

            let trimmed = line.trim();
            if !trimmed.contains("EXEC CICS") {
                continue;
            }

            // Extract the verb(s) after EXEC CICS
            if let Some(pos) = trimmed.find("EXEC CICS") {
                let after = trimmed[pos + 9..].trim();
                let verb = extract_verb(after);
                if !verb.is_empty() {
                    let category = classify_verb(&verb);
                    let support = support_status(&verb);

                    let entry = inv.commands.entry(verb.clone()).or_insert_with(|| {
                        CicsCommand {
                            verb: verb.clone(),
                            category,
                            count: 0,
                            lines: Vec::new(),
                            support,
                        }
                    });
                    entry.count += 1;
                    entry.lines.push(line_num);
                    inv.total_count += 1;
                }
            }
        }

        inv
    }

    /// Get commands grouped by category.
    pub fn by_category(&self) -> HashMap<CicsCategory, Vec<&CicsCommand>> {
        let mut result: HashMap<CicsCategory, Vec<&CicsCommand>> = HashMap::new();
        for cmd in self.commands.values() {
            result.entry(cmd.category).or_default().push(cmd);
        }
        result
    }

    /// Count of supported commands.
    pub fn supported_count(&self) -> usize {
        self.commands
            .values()
            .filter(|c| c.support == SupportStatus::Supported)
            .map(|c| c.count)
            .sum()
    }

    /// Count of unsupported commands.
    pub fn unsupported_count(&self) -> usize {
        self.commands
            .values()
            .filter(|c| c.support == SupportStatus::Unsupported)
            .map(|c| c.count)
            .sum()
    }
}

/// Extract the CICS verb from the text after "EXEC CICS".
fn extract_verb(text: &str) -> String {
    let words: Vec<&str> = text.split_whitespace().collect();
    if words.is_empty() {
        return String::new();
    }

    // Strip parentheses and quotes from each word for matching purposes
    let clean = |w: &str| -> String {
        w.split('(').next().unwrap_or(w).to_string()
    };

    let w0 = clean(words[0]);

    // Two-word verbs first
    if words.len() >= 2 {
        let w1 = clean(words[1]);
        let two = format!("{} {}", w0, w1);
        match two.as_str() {
            "SEND MAP" | "RECEIVE MAP" | "SEND TEXT" | "SEND CONTROL" | "SEND PAGE"
            | "WRITEQ TS" | "READQ TS" | "DELETEQ TS" | "WRITEQ TD" | "READQ TD"
            | "HANDLE CONDITION" | "HANDLE AID" | "HANDLE ABEND"
            | "IGNORE CONDITION" | "SYNCPOINT ROLLBACK" | "PURGE MESSAGE"
            | "WRITE JOURNALNAME" => return two,
            _ => {}
        }
    }

    // Single-word verb
    w0
}

/// Classify a CICS verb into a category.
fn classify_verb(verb: &str) -> CicsCategory {
    match verb {
        "READ" | "WRITE" | "REWRITE" | "DELETE" | "STARTBR" | "READNEXT" | "READPREV"
        | "ENDBR" | "UNLOCK" | "RESETBR" => CicsCategory::FileControl,

        "SEND MAP" | "RECEIVE MAP" | "SEND TEXT" | "SEND CONTROL" | "SEND PAGE"
        | "PURGE MESSAGE" => CicsCategory::BmsControl,

        "SEND" | "RECEIVE" => CicsCategory::TerminalControl,

        "LINK" | "XCTL" | "RETURN" | "LOAD" | "RELEASE" => CicsCategory::ProgramControl,

        "WRITEQ TS" | "READQ TS" | "DELETEQ TS" | "WRITEQ TD" | "READQ TD" => {
            CicsCategory::QueueControl
        }

        "START" | "CANCEL" | "DELAY" | "ASKTIME" | "FORMATTIME" => {
            CicsCategory::IntervalControl
        }

        "SUSPEND" | "ENQ" | "DEQ" | "ASSIGN" => CicsCategory::TaskControl,

        "GETMAIN" | "FREEMAIN" => CicsCategory::StorageControl,

        "WRITE JOURNALNAME" => CicsCategory::JournalControl,

        "SYNCPOINT" | "SYNCPOINT ROLLBACK" => CicsCategory::SyncpointControl,

        "HANDLE CONDITION" | "HANDLE AID" | "HANDLE ABEND" | "IGNORE CONDITION" | "PUSH"
        | "POP" => CicsCategory::ExceptionHandling,

        _ => CicsCategory::Other,
    }
}

/// Determine support status for a CICS verb in open-mainframe-cics.
fn support_status(verb: &str) -> SupportStatus {
    match verb {
        // Fully supported
        "READ" | "WRITE" | "REWRITE" | "DELETE" | "STARTBR" | "READNEXT" | "READPREV"
        | "ENDBR" | "UNLOCK" | "RESETBR" => SupportStatus::Supported,

        "SEND MAP" | "RECEIVE MAP" | "SEND TEXT" => SupportStatus::Supported,

        "LINK" | "XCTL" | "RETURN" => SupportStatus::Supported,

        "WRITEQ TS" | "READQ TS" | "DELETEQ TS" | "WRITEQ TD" | "READQ TD" => {
            SupportStatus::Supported
        }

        "ASSIGN" | "HANDLE AID" | "HANDLE CONDITION" | "HANDLE ABEND"
        | "IGNORE CONDITION" | "PUSH" | "POP" => SupportStatus::Supported,

        "ASKTIME" | "FORMATTIME" => SupportStatus::Supported,

        "SYNCPOINT" => SupportStatus::Supported,

        // Partially supported
        "SEND CONTROL" | "START" | "CANCEL" | "GETMAIN" | "FREEMAIN" | "ENQ" | "DEQ" => {
            SupportStatus::Partial
        }

        // Not yet supported
        "LOAD" | "RELEASE" | "SUSPEND" | "DELAY" | "SEND PAGE" | "PURGE MESSAGE"
        | "SYNCPOINT ROLLBACK" | "WRITE JOURNALNAME" | "SEND" | "RECEIVE" => {
            SupportStatus::Unsupported
        }

        _ => SupportStatus::Unsupported,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify_commands() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
           EXEC CICS READ FILE('CUSTFILE') INTO(WS-REC) END-EXEC.
           EXEC CICS WRITE FILE('CUSTFILE') FROM(WS-REC) END-EXEC.
           EXEC CICS SEND MAP('MAP1') MAPSET('SET1') END-EXEC.
           EXEC CICS LINK PROGRAM('SUB1') END-EXEC.
           EXEC CICS WRITEQ TS QUEUE('MYQUEUE') FROM(WS-DATA) END-EXEC.
"#;
        let inv = CicsInventory::from_source(source);
        assert_eq!(inv.total_count, 5);
        assert_eq!(inv.commands.len(), 5);

        let read = inv.commands.get("READ").unwrap();
        assert_eq!(read.category, CicsCategory::FileControl);
        assert_eq!(read.count, 1);
        assert_eq!(read.support, SupportStatus::Supported);

        let send_map = inv.commands.get("SEND MAP").unwrap();
        assert_eq!(send_map.category, CicsCategory::BmsControl);

        let link = inv.commands.get("LINK").unwrap();
        assert_eq!(link.category, CicsCategory::ProgramControl);

        let writeq = inv.commands.get("WRITEQ TS").unwrap();
        assert_eq!(writeq.category, CicsCategory::QueueControl);
    }

    #[test]
    fn test_support_status_tracking() {
        let source = r#"
       EXEC CICS READ FILE('F') INTO(R) END-EXEC.
       EXEC CICS LOAD PROGRAM('P') END-EXEC.
       EXEC CICS SEND CONTROL ERASE END-EXEC.
"#;
        let inv = CicsInventory::from_source(source);
        assert_eq!(inv.supported_count(), 1);  // READ
        assert_eq!(inv.unsupported_count(), 1); // LOAD

        let send_ctl = inv.commands.get("SEND CONTROL").unwrap();
        assert_eq!(send_ctl.support, SupportStatus::Partial);
    }

    #[test]
    fn test_by_category_grouping() {
        let source = r#"
       EXEC CICS READ FILE('A') INTO(X) END-EXEC.
       EXEC CICS WRITE FILE('A') FROM(X) END-EXEC.
       EXEC CICS SEND MAP('M') END-EXEC.
       EXEC CICS LINK PROGRAM('P') END-EXEC.
"#;
        let inv = CicsInventory::from_source(source);
        let by_cat = inv.by_category();

        assert_eq!(by_cat.get(&CicsCategory::FileControl).unwrap().len(), 2);
        assert_eq!(by_cat.get(&CicsCategory::BmsControl).unwrap().len(), 1);
        assert_eq!(by_cat.get(&CicsCategory::ProgramControl).unwrap().len(), 1);
    }

    #[test]
    fn test_empty_source() {
        let inv = CicsInventory::from_source("");
        assert_eq!(inv.total_count, 0);
        assert!(inv.commands.is_empty());
    }

    #[test]
    fn test_comment_lines_skipped() {
        let source = "      * EXEC CICS READ FILE('X') END-EXEC.\n";
        let inv = CicsInventory::from_source(source);
        assert_eq!(inv.total_count, 0);
    }

    #[test]
    fn test_multiple_occurrences() {
        let source = r#"
       EXEC CICS READ FILE('A') INTO(X) END-EXEC.
       EXEC CICS READ FILE('B') INTO(Y) END-EXEC.
       EXEC CICS READ FILE('C') INTO(Z) END-EXEC.
"#;
        let inv = CicsInventory::from_source(source);
        let read = inv.commands.get("READ").unwrap();
        assert_eq!(read.count, 3);
        assert_eq!(read.lines.len(), 3);
    }

    #[test]
    fn test_category_names() {
        assert_eq!(CicsCategory::FileControl.name(), "File Control");
        assert_eq!(CicsCategory::ProgramControl.name(), "Program Control");
        assert_eq!(CicsCategory::QueueControl.name(), "Queue Control");
    }

    #[test]
    fn test_support_status_labels() {
        assert_eq!(SupportStatus::Supported.label(), "Supported");
        assert_eq!(SupportStatus::Partial.label(), "Partial");
        assert_eq!(SupportStatus::Unsupported.label(), "Unsupported");
    }
}
