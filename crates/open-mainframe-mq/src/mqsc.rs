//! MQSC Command Engine.
//!
//! Parses and executes MQSC commands:
//! - DEFINE QLOCAL / QALIAS / QREMOTE / QMODEL
//! - ALTER QLOCAL
//! - DELETE QLOCAL
//! - DISPLAY QLOCAL / QUEUE
//! - CLEAR QLOCAL

use crate::core::{MqError, QueueManager, QueueType};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  MQSC command types
// ---------------------------------------------------------------------------

/// An MQSC command.
#[derive(Debug, Clone)]
pub enum MqscCommand {
    /// DEFINE QLOCAL(name) [options].
    DefineQlocal {
        name: String,
        options: HashMap<String, String>,
    },
    /// DEFINE QALIAS(name) TARGET(target) [options].
    DefineQalias {
        name: String,
        target: String,
        options: HashMap<String, String>,
    },
    /// DEFINE QREMOTE(name) [options].
    DefineQremote {
        name: String,
        options: HashMap<String, String>,
    },
    /// ALTER QLOCAL(name) [options].
    AlterQlocal {
        name: String,
        options: HashMap<String, String>,
    },
    /// DELETE QLOCAL(name) [PURGE|NOPURGE].
    DeleteQlocal { name: String, purge: bool },
    /// DISPLAY QLOCAL(name) or DISPLAY QUEUE(name).
    DisplayQueue { name: String },
    /// CLEAR QLOCAL(name).
    ClearQlocal { name: String },
    /// DISPLAY QMGR.
    DisplayQmgr,
}

/// Result of an MQSC command execution.
#[derive(Debug, Clone)]
pub struct MqscResult {
    /// Whether the command succeeded.
    pub success: bool,
    /// Output text lines.
    pub output: Vec<String>,
}

// ---------------------------------------------------------------------------
//  MQSC engine
// ---------------------------------------------------------------------------

/// The MQSC command engine.
#[derive(Debug)]
pub struct MqscEngine;

impl Default for MqscEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl MqscEngine {
    pub fn new() -> Self {
        Self
    }

    /// Parse an MQSC command string into a command enum.
    pub fn parse(&self, input: &str) -> Result<MqscCommand, MqError> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Err(MqError::MqscError("Empty command".to_string()));
        }

        let upper = trimmed.to_uppercase();
        let tokens = tokenize_mqsc(trimmed);

        if tokens.is_empty() {
            return Err(MqError::MqscError("Empty command".to_string()));
        }

        let verb = tokens[0].to_uppercase();

        match verb.as_str() {
            "DEFINE" | "DEF" => self.parse_define(&tokens, &upper),
            "ALTER" | "ALT" => self.parse_alter(&tokens),
            "DELETE" | "DEL" => self.parse_delete(&tokens, &upper),
            "DISPLAY" | "DIS" => self.parse_display(&tokens),
            "CLEAR" => self.parse_clear(&tokens),
            _ => Err(MqError::MqscError(format!("Unknown verb: {verb}"))),
        }
    }

    fn parse_define(&self, tokens: &[String], upper: &str) -> Result<MqscCommand, MqError> {
        if tokens.len() < 2 {
            return Err(MqError::MqscError("DEFINE requires object type".to_string()));
        }
        let obj_type = tokens[1].to_uppercase();

        match obj_type.as_str() {
            t if t.starts_with("QLOCAL") || t.starts_with("QL") => {
                let name = extract_paren_value(&obj_type)
                    .ok_or_else(|| MqError::MqscError("DEFINE QLOCAL requires (name)".to_string()))?;
                let options = parse_options(&tokens[2..]);
                Ok(MqscCommand::DefineQlocal { name, options })
            }
            t if t.starts_with("QALIAS") || t.starts_with("QA") => {
                let name = extract_paren_value(&obj_type)
                    .ok_or_else(|| MqError::MqscError("DEFINE QALIAS requires (name)".to_string()))?;
                let options = parse_options(&tokens[2..]);
                let target = options.get("TARGET").cloned().or_else(|| {
                    // Check in upper for TARGET(x)
                    extract_keyword_value(upper, "TARGET")
                }).unwrap_or_default();
                Ok(MqscCommand::DefineQalias {
                    name,
                    target,
                    options,
                })
            }
            t if t.starts_with("QREMOTE") || t.starts_with("QR") => {
                let name = extract_paren_value(&obj_type)
                    .ok_or_else(|| MqError::MqscError("DEFINE QREMOTE requires (name)".to_string()))?;
                let options = parse_options(&tokens[2..]);
                Ok(MqscCommand::DefineQremote { name, options })
            }
            _ => Err(MqError::MqscError(format!("Unknown object type: {obj_type}"))),
        }
    }

    fn parse_alter(&self, tokens: &[String]) -> Result<MqscCommand, MqError> {
        if tokens.len() < 2 {
            return Err(MqError::MqscError("ALTER requires object type".to_string()));
        }
        let obj_type = tokens[1].to_uppercase();
        if obj_type.starts_with("QLOCAL") || obj_type.starts_with("QL") {
            let name = extract_paren_value(&obj_type)
                .ok_or_else(|| MqError::MqscError("ALTER QLOCAL requires (name)".to_string()))?;
            let options = parse_options(&tokens[2..]);
            Ok(MqscCommand::AlterQlocal { name, options })
        } else {
            Err(MqError::MqscError(format!("Unsupported ALTER target: {obj_type}")))
        }
    }

    fn parse_delete(&self, tokens: &[String], upper: &str) -> Result<MqscCommand, MqError> {
        if tokens.len() < 2 {
            return Err(MqError::MqscError("DELETE requires object type".to_string()));
        }
        let obj_type = tokens[1].to_uppercase();
        if obj_type.starts_with("QLOCAL") || obj_type.starts_with("QL") {
            let name = extract_paren_value(&obj_type)
                .ok_or_else(|| MqError::MqscError("DELETE QLOCAL requires (name)".to_string()))?;
            let purge = upper.contains("PURGE") && !upper.contains("NOPURGE");
            Ok(MqscCommand::DeleteQlocal { name, purge })
        } else {
            Err(MqError::MqscError(format!("Unsupported DELETE target: {obj_type}")))
        }
    }

    fn parse_display(&self, tokens: &[String]) -> Result<MqscCommand, MqError> {
        if tokens.len() < 2 {
            return Err(MqError::MqscError("DISPLAY requires object type".to_string()));
        }
        let obj_type = tokens[1].to_uppercase();
        if obj_type.starts_with("QLOCAL") || obj_type.starts_with("QL")
            || obj_type.starts_with("QUEUE") || obj_type.starts_with("Q(")
        {
            let name = extract_paren_value(&obj_type).unwrap_or_else(|| "*".to_string());
            Ok(MqscCommand::DisplayQueue { name })
        } else if obj_type == "QMGR" {
            Ok(MqscCommand::DisplayQmgr)
        } else {
            Err(MqError::MqscError(format!("Unsupported DISPLAY target: {obj_type}")))
        }
    }

    fn parse_clear(&self, tokens: &[String]) -> Result<MqscCommand, MqError> {
        if tokens.len() < 2 {
            return Err(MqError::MqscError("CLEAR requires object type".to_string()));
        }
        let obj_type = tokens[1].to_uppercase();
        if obj_type.starts_with("QLOCAL") || obj_type.starts_with("QL") {
            let name = extract_paren_value(&obj_type)
                .ok_or_else(|| MqError::MqscError("CLEAR QLOCAL requires (name)".to_string()))?;
            Ok(MqscCommand::ClearQlocal { name })
        } else {
            Err(MqError::MqscError(format!("Unsupported CLEAR target: {obj_type}")))
        }
    }

    /// Execute a parsed MQSC command against a queue manager.
    pub fn execute(&self, cmd: &MqscCommand, qm: &mut QueueManager) -> MqscResult {
        match cmd {
            MqscCommand::DefineQlocal { name, options } => {
                match qm.define_queue(name, QueueType::Local) {
                    Ok(()) => {
                        // Apply options.
                        if let Ok(queue) = qm.get_queue_mut(name) {
                            apply_queue_options(queue, options);
                        }
                        MqscResult {
                            success: true,
                            output: vec![format!("AMQ8006: Queue created: {name}")],
                        }
                    }
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8150: {e}")],
                    },
                }
            }
            MqscCommand::DefineQalias { name, target, .. } => {
                match qm.define_queue(name, QueueType::Alias) {
                    Ok(()) => {
                        if let Ok(queue) = qm.get_queue_mut(name) {
                            queue.target_queue = Some(target.clone());
                        }
                        MqscResult {
                            success: true,
                            output: vec![format!("AMQ8006: Queue created: {name}")],
                        }
                    }
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8150: {e}")],
                    },
                }
            }
            MqscCommand::DefineQremote { name, options } => {
                match qm.define_queue(name, QueueType::Remote) {
                    Ok(()) => {
                        if let Ok(queue) = qm.get_queue_mut(name) {
                            if let Some(rqm) = options.get("RQMNAME") {
                                queue.remote_qmgr = Some(rqm.clone());
                            }
                            if let Some(rq) = options.get("RNAME") {
                                queue.remote_queue = Some(rq.clone());
                            }
                        }
                        MqscResult {
                            success: true,
                            output: vec![format!("AMQ8006: Queue created: {name}")],
                        }
                    }
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8150: {e}")],
                    },
                }
            }
            MqscCommand::AlterQlocal { name, options } => {
                match qm.get_queue_mut(name) {
                    Ok(queue) => {
                        apply_queue_options(queue, options);
                        MqscResult {
                            success: true,
                            output: vec![format!("AMQ8008: Queue altered: {name}")],
                        }
                    }
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8147: {e}")],
                    },
                }
            }
            MqscCommand::DeleteQlocal { name, purge } => {
                let result = if *purge {
                    qm.delete_queue_purge(name)
                } else {
                    qm.delete_queue(name)
                };
                match result {
                    Ok(()) => MqscResult {
                        success: true,
                        output: vec![format!("AMQ8007: Queue deleted: {name}")],
                    },
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8147: {e}")],
                    },
                }
            }
            MqscCommand::DisplayQueue { name } => {
                if name == "*" {
                    let names = qm.list_queues();
                    let mut output = vec![format!("AMQ8409: {} queues found", names.len())];
                    for n in &names {
                        if let Ok(q) = qm.get_queue(n) {
                            output.push(format!(
                                "QUEUE({n}) TYPE({:?}) CURDEPTH({}) MAXDEPTH({})",
                                q.queue_type,
                                q.depth(),
                                q.max_depth
                            ));
                        }
                    }
                    MqscResult { success: true, output }
                } else {
                    match qm.get_queue(name) {
                        Ok(q) => MqscResult {
                            success: true,
                            output: vec![
                                format!("AMQ8409: Display queue details"),
                                format!("QUEUE({}) TYPE({:?})", q.name, q.queue_type),
                                format!("CURDEPTH({}) MAXDEPTH({})", q.depth(), q.max_depth),
                                format!("PUT({}) GET({})",
                                    if q.put_inhibited { "DISABLED" } else { "ENABLED" },
                                    if q.get_inhibited { "DISABLED" } else { "ENABLED" }
                                ),
                                format!("DESCR({})", q.description),
                            ],
                        },
                        Err(e) => MqscResult {
                            success: false,
                            output: vec![format!("AMQ8147: {e}")],
                        },
                    }
                }
            }
            MqscCommand::ClearQlocal { name } => {
                match qm.get_queue_mut(name) {
                    Ok(queue) => {
                        queue.clear();
                        MqscResult {
                            success: true,
                            output: vec![format!("AMQ8022: Queue cleared: {name}")],
                        }
                    }
                    Err(e) => MqscResult {
                        success: false,
                        output: vec![format!("AMQ8147: {e}")],
                    },
                }
            }
            MqscCommand::DisplayQmgr => MqscResult {
                success: true,
                output: vec![
                    "AMQ8408: Display queue manager details".to_string(),
                    format!("QMNAME({})", qm.name),
                    format!("DESCR({})", qm.description),
                    format!("DEADQ({})", qm.dead_letter_queue.as_deref().unwrap_or("")),
                ],
            },
        }
    }

    /// Parse and execute a command in one step.
    pub fn run(&self, input: &str, qm: &mut QueueManager) -> MqscResult {
        match self.parse(input) {
            Ok(cmd) => self.execute(&cmd, qm),
            Err(e) => MqscResult {
                success: false,
                output: vec![format!("AMQ8405: {e}")],
            },
        }
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Tokenize MQSC input, keeping parenthesized values together.
fn tokenize_mqsc(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut paren_depth: i32 = 0;

    for ch in input.chars() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                paren_depth -= 1;
                current.push(ch);
            }
            ' ' | '\t' if paren_depth == 0 => {
                if !current.is_empty() {
                    tokens.push(current.clone());
                    current.clear();
                }
            }
            _ => {
                current.push(ch);
            }
        }
    }
    if !current.is_empty() {
        tokens.push(current);
    }
    tokens
}

/// Extract value from KEYWORD(VALUE) syntax.
fn extract_paren_value(s: &str) -> Option<String> {
    let open = s.find('(')?;
    let close = s.rfind(')')?;
    if close > open + 1 {
        Some(s[open + 1..close].to_string())
    } else {
        None
    }
}

/// Extract a keyword value from the full command string.
fn extract_keyword_value(upper: &str, keyword: &str) -> Option<String> {
    let pat = format!("{keyword}(");
    let start = upper.find(&pat)?;
    let rest = &upper[start + pat.len()..];
    let end = rest.find(')')?;
    Some(rest[..end].to_string())
}

/// Parse KEYWORD(VALUE) options from tokens.
fn parse_options(tokens: &[String]) -> HashMap<String, String> {
    let mut options = HashMap::new();
    for token in tokens {
        let upper = token.to_uppercase();
        if let Some(value) = extract_paren_value(&upper) {
            let key_end = upper.find('(').unwrap();
            let key = upper[..key_end].to_string();
            options.insert(key, value);
        }
    }
    options
}

/// Apply parsed options to a queue.
fn apply_queue_options(queue: &mut crate::core::Queue, options: &HashMap<String, String>) {
    if let Some(desc) = options.get("DESCR") {
        queue.description = desc.clone();
    }
    if let Some(maxdepth) = options.get("MAXDEPTH") {
        if let Ok(d) = maxdepth.parse::<u32>() {
            queue.max_depth = d;
        }
    }
    if let Some(put) = options.get("PUT") {
        queue.put_inhibited = put == "DISABLED";
    }
    if let Some(get) = options.get("GET") {
        queue.get_inhibited = get == "DISABLED";
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (MqscEngine, QueueManager) {
        (MqscEngine::new(), QueueManager::new("TESTQM"))
    }

    #[test]
    fn test_define_qlocal() {
        let (engine, mut qm) = setup();
        let result = engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        assert!(result.success);
        assert!(qm.queue_exists("MY.Q"));
    }

    #[test]
    fn test_define_qlocal_with_options() {
        let (engine, mut qm) = setup();
        let result = engine.run("DEFINE QLOCAL(MY.Q) MAXDEPTH(1000) DESCR(Test queue)", &mut qm);
        assert!(result.success);
        let q = qm.get_queue("MY.Q").unwrap();
        assert_eq!(q.max_depth, 1000);
        assert_eq!(q.description, "TEST QUEUE");
    }

    #[test]
    fn test_define_duplicate() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        let result = engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        assert!(!result.success);
    }

    #[test]
    fn test_alter_qlocal() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        let result = engine.run("ALTER QLOCAL(MY.Q) MAXDEPTH(2000)", &mut qm);
        assert!(result.success);
        let q = qm.get_queue("MY.Q").unwrap();
        assert_eq!(q.max_depth, 2000);
    }

    #[test]
    fn test_delete_qlocal() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        let result = engine.run("DELETE QLOCAL(MY.Q)", &mut qm);
        assert!(result.success);
        assert!(!qm.queue_exists("MY.Q"));
    }

    #[test]
    fn test_display_queue() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q) DESCR(Hello)", &mut qm);
        let result = engine.run("DISPLAY QLOCAL(MY.Q)", &mut qm);
        assert!(result.success);
        assert!(result.output.len() > 1);
    }

    #[test]
    fn test_display_all_queues() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(Q1)", &mut qm);
        engine.run("DEFINE QLOCAL(Q2)", &mut qm);
        let result = engine.run("DISPLAY QUEUE(*)", &mut qm);
        assert!(result.success);
        assert!(result.output[0].contains("2 queues found"));
    }

    #[test]
    fn test_clear_qlocal() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        {
            let q = qm.get_queue_mut("MY.Q").unwrap();
            q.put(crate::structures::Mqmd::default(), b"test".to_vec()).unwrap();
        }
        let result = engine.run("CLEAR QLOCAL(MY.Q)", &mut qm);
        assert!(result.success);
        assert_eq!(qm.get_queue("MY.Q").unwrap().depth(), 0);
    }

    #[test]
    fn test_display_qmgr() {
        let (engine, mut qm) = setup();
        let result = engine.run("DISPLAY QMGR", &mut qm);
        assert!(result.success);
        assert!(result.output.iter().any(|l| l.contains("TESTQM")));
    }

    #[test]
    fn test_unknown_command() {
        let (engine, mut qm) = setup();
        let result = engine.run("FROBNICATE QUEUE(X)", &mut qm);
        assert!(!result.success);
    }

    #[test]
    fn test_define_qalias() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(TARGET.Q)", &mut qm);
        let result = engine.run("DEFINE QALIAS(ALIAS.Q) TARGET(TARGET.Q)", &mut qm);
        assert!(result.success);
        let q = qm.get_queue("ALIAS.Q").unwrap();
        assert_eq!(q.queue_type, QueueType::Alias);
    }

    #[test]
    fn test_define_qremote() {
        let (engine, mut qm) = setup();
        let result = engine.run("DEFINE QREMOTE(REMOTE.Q) RQMNAME(REMQM) RNAME(DEST.Q)", &mut qm);
        assert!(result.success);
        let q = qm.get_queue("REMOTE.Q").unwrap();
        assert_eq!(q.queue_type, QueueType::Remote);
    }

    #[test]
    fn test_delete_purge() {
        let (engine, mut qm) = setup();
        engine.run("DEFINE QLOCAL(MY.Q)", &mut qm);
        {
            let q = qm.get_queue_mut("MY.Q").unwrap();
            q.put(crate::structures::Mqmd::default(), b"test".to_vec()).unwrap();
        }
        // Without PURGE should fail.
        let result = engine.run("DELETE QLOCAL(MY.Q)", &mut qm);
        assert!(!result.success);
        // With PURGE.
        let result = engine.run("DELETE QLOCAL(MY.Q) PURGE", &mut qm);
        assert!(result.success);
    }

    #[test]
    fn test_abbreviated_commands() {
        let (engine, mut qm) = setup();
        let result = engine.run("DEF QL(MY.Q)", &mut qm);
        assert!(result.success);

        let result = engine.run("DIS QL(MY.Q)", &mut qm);
        assert!(result.success);

        let result = engine.run("DEL QL(MY.Q)", &mut qm);
        assert!(result.success);
    }
}
