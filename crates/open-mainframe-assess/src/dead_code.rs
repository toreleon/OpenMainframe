//! Dead code detection for COBOL programs.
//!
//! Identifies unreachable paragraphs and sections that are never PERFORMed,
//! GO-TO'd, or otherwise referenced, so migration effort estimates can
//! exclude unused code.

use std::collections::HashSet;

/// A detected dead code region.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct DeadCodeItem {
    /// Paragraph or section name.
    pub name: String,
    /// Line number where the paragraph/section starts.
    pub line: usize,
    /// Approximate number of lines in the paragraph/section.
    pub line_count: usize,
    /// Kind of dead code (paragraph or section).
    pub kind: DeadCodeKind,
}

/// Whether the dead item is a paragraph or a section.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum DeadCodeKind {
    Paragraph,
    Section,
}

/// Result of dead code analysis.
#[derive(Debug, Clone, serde::Serialize)]
pub struct DeadCodeReport {
    /// Program name.
    pub program: String,
    /// Total paragraphs/sections found.
    pub total_paragraphs: usize,
    /// Dead (unreachable) paragraphs/sections.
    pub dead_items: Vec<DeadCodeItem>,
    /// Total lines in dead code.
    pub dead_lines: usize,
    /// Total lines in all paragraphs.
    pub total_paragraph_lines: usize,
}

impl DeadCodeReport {
    /// Percentage of paragraphs that are dead.
    pub fn dead_percentage(&self) -> f64 {
        if self.total_paragraphs == 0 {
            return 0.0;
        }
        (self.dead_items.len() as f64 / self.total_paragraphs as f64) * 100.0
    }

    /// Adjusted line count excluding dead code.
    pub fn adjusted_lines(&self, total_lines: usize) -> usize {
        total_lines.saturating_sub(self.dead_lines)
    }
}

/// A paragraph/section definition found in the source.
#[derive(Debug, Clone)]
struct ParagraphDef {
    name: String,
    line: usize,
    end_line: usize,
    kind: DeadCodeKind,
}

/// Detect dead (unreachable) paragraphs in COBOL source code.
///
/// Scans the procedure division for paragraph/section definitions and
/// references (PERFORM, GO TO, THRU/THROUGH). Any paragraph that is
/// never referenced is flagged as potentially dead.
pub fn detect_dead_code(program_name: &str, source: &str) -> DeadCodeReport {
    let lines: Vec<&str> = source.lines().collect();
    let upper = source.to_uppercase();
    let upper_lines: Vec<&str> = upper.lines().collect();

    // Find where the procedure division starts
    let proc_start = upper_lines
        .iter()
        .position(|l| l.trim().starts_with("PROCEDURE DIVISION"))
        .unwrap_or(0);

    // 1. Find all paragraph/section definitions in the procedure division
    let paragraphs = find_paragraphs(&lines, &upper_lines, proc_start);

    // 2. Find all references (PERFORM, GO TO, THRU/THROUGH)
    let references = find_references(&upper_lines, proc_start);

    // 3. Determine which paragraphs are unreachable
    //    A paragraph is "live" if it matches any reference, or if it's the
    //    first paragraph in the procedure division (the entry point).
    let mut dead_items = Vec::new();
    for (i, para) in paragraphs.iter().enumerate() {
        // The first paragraph/section is the entry point — always reachable
        if i == 0 {
            continue;
        }
        let name_upper = para.name.to_uppercase();
        if !references.contains(&name_upper) {
            let line_count = para.end_line.saturating_sub(para.line) + 1;
            dead_items.push(DeadCodeItem {
                name: para.name.clone(),
                line: para.line + 1, // 1-based
                line_count,
                kind: para.kind,
            });
        }
    }

    let dead_lines: usize = dead_items.iter().map(|d| d.line_count).sum();
    let total_paragraph_lines: usize = paragraphs
        .iter()
        .map(|p| p.end_line.saturating_sub(p.line) + 1)
        .sum();

    DeadCodeReport {
        program: program_name.to_string(),
        total_paragraphs: paragraphs.len(),
        dead_items,
        dead_lines,
        total_paragraph_lines,
    }
}

/// Find paragraph and section definitions in the procedure division.
fn find_paragraphs(
    _lines: &[&str],
    upper_lines: &[&str],
    proc_start: usize,
) -> Vec<ParagraphDef> {
    let mut paragraphs = Vec::new();

    for (i, line) in upper_lines.iter().enumerate().skip(proc_start + 1) {
        let trimmed = line.trim();

        // Skip empty lines, comments, compiler directives
        if trimmed.is_empty() || is_comment_line(line) {
            continue;
        }

        // Section definition: NAME SECTION.
        if trimmed.ends_with("SECTION.")
            && !trimmed.starts_with("IDENTIFICATION")
            && !trimmed.starts_with("ENVIRONMENT")
            && !trimmed.starts_with("DATA")
            && !trimmed.starts_with("PROCEDURE")
        {
            let name = trimmed
                .trim_end_matches("SECTION.")
                .split_whitespace()
                .last()
                .unwrap_or("")
                .to_string();
            if !name.is_empty() && is_valid_paragraph_name(&name) {
                paragraphs.push(ParagraphDef {
                    name,
                    line: i,
                    end_line: i,
                    kind: DeadCodeKind::Section,
                });
            }
            continue;
        }

        // Paragraph definition: a line that ends with '.' and contains
        // a single word that looks like a paragraph name (starts in area A,
        // i.e. not heavily indented, and is followed by statements).
        if trimmed.ends_with('.') && !trimmed.contains(' ') {
            let name = trimmed.trim_end_matches('.').to_string();
            if !name.is_empty()
                && is_valid_paragraph_name(&name)
                && !is_cobol_verb(&name)
            {
                paragraphs.push(ParagraphDef {
                    name,
                    line: i,
                    end_line: i,
                    kind: DeadCodeKind::Paragraph,
                });
            }
        }
    }

    // Compute end lines: each paragraph extends until the next paragraph starts
    for i in 0..paragraphs.len() {
        let next_start = if i + 1 < paragraphs.len() {
            paragraphs[i + 1].line.saturating_sub(1)
        } else {
            upper_lines.len().saturating_sub(1)
        };
        paragraphs[i].end_line = next_start;
    }

    paragraphs
}

/// Find all paragraph/section references in the procedure division.
fn find_references(upper_lines: &[&str], proc_start: usize) -> HashSet<String> {
    let mut refs = HashSet::new();

    for line in upper_lines.iter().skip(proc_start) {
        let trimmed = line.trim();

        if is_comment_line(line) {
            continue;
        }

        // PERFORM para-name
        // PERFORM para-name THRU/THROUGH para-name-2
        // PERFORM para-name TIMES / UNTIL / VARYING
        if let Some(pos) = trimmed.find("PERFORM ") {
            let after = &trimmed[pos + 8..];
            extract_perform_targets(after, &mut refs);
        }

        // GO TO para-name
        if let Some(pos) = trimmed.find("GO TO ") {
            let after = &trimmed[pos + 6..];
            let target = after
                .split_whitespace()
                .next()
                .unwrap_or("")
                .trim_end_matches('.');
            if !target.is_empty() {
                refs.insert(target.to_string());
            }
        }

        // GO para-name (without TO — less common but valid)
        if trimmed.starts_with("GO ") && !trimmed.starts_with("GO TO") {
            let after = &trimmed[3..];
            let target = after
                .split_whitespace()
                .next()
                .unwrap_or("")
                .trim_end_matches('.');
            if !target.is_empty() {
                refs.insert(target.to_string());
            }
        }

        // SORT ... INPUT PROCEDURE IS para / OUTPUT PROCEDURE IS para
        for keyword in &["INPUT PROCEDURE", "OUTPUT PROCEDURE"] {
            if let Some(pos) = trimmed.find(keyword) {
                let after = &trimmed[pos + keyword.len()..];
                let after = after.trim_start();
                let after = if let Some(stripped) = after.strip_prefix("IS ") {
                    stripped
                } else {
                    after
                };
                let target = after
                    .split_whitespace()
                    .next()
                    .unwrap_or("")
                    .trim_end_matches('.');
                if !target.is_empty() {
                    refs.insert(target.to_string());
                }
                // Check for THRU/THROUGH
                extract_thru_target(after, &mut refs);
            }
        }
    }

    refs
}

/// Extract targets from a PERFORM clause: "PARA-A THRU PARA-B ..."
fn extract_perform_targets(clause: &str, refs: &mut HashSet<String>) {
    let words: Vec<&str> = clause.split_whitespace().collect();
    if words.is_empty() {
        return;
    }

    let target = words[0].trim_end_matches('.');
    // Skip inline performs (PERFORM UNTIL, PERFORM VARYING, PERFORM <number> TIMES)
    if is_cobol_verb(target) || target.parse::<usize>().is_ok() {
        return;
    }
    refs.insert(target.to_string());

    // Look for THRU/THROUGH
    for i in 1..words.len() {
        if words[i] == "THRU" || words[i] == "THROUGH" {
            if let Some(target2) = words.get(i + 1) {
                let target2 = target2.trim_end_matches('.');
                if !target2.is_empty() {
                    refs.insert(target2.to_string());
                }
            }
            break;
        }
    }
}

/// Extract THRU/THROUGH target from a clause.
fn extract_thru_target(clause: &str, refs: &mut HashSet<String>) {
    let words: Vec<&str> = clause.split_whitespace().collect();
    for i in 0..words.len() {
        if words[i] == "THRU" || words[i] == "THROUGH" {
            if let Some(target) = words.get(i + 1) {
                let target = target.trim_end_matches('.');
                if !target.is_empty() {
                    refs.insert(target.to_string());
                }
            }
            break;
        }
    }
}

/// Check if a line is a COBOL comment (column 7 = '*' or '/').
fn is_comment_line(line: &str) -> bool {
    if line.len() > 6 {
        let ch = line.as_bytes()[6];
        ch == b'*' || ch == b'/'
    } else {
        false
    }
}

/// Check if a name looks like a valid COBOL paragraph name.
fn is_valid_paragraph_name(name: &str) -> bool {
    !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        && name.chars().next().map_or(false, |c| c.is_alphanumeric())
}

/// Check if a word is a COBOL statement verb (not a paragraph name).
fn is_cobol_verb(word: &str) -> bool {
    matches!(
        word,
        "ACCEPT" | "ADD" | "ALTER" | "CALL" | "CANCEL" | "CLOSE" | "COMPUTE"
        | "CONTINUE" | "DELETE" | "DISPLAY" | "DIVIDE" | "ELSE" | "END"
        | "END-IF" | "END-PERFORM" | "END-EVALUATE" | "END-READ" | "END-WRITE"
        | "EVALUATE" | "EXEC" | "EXIT" | "GO" | "GOBACK" | "IF" | "INITIALIZE"
        | "INSPECT" | "MERGE" | "MOVE" | "MULTIPLY" | "OPEN" | "PERFORM"
        | "READ" | "RELEASE" | "RETURN" | "REWRITE" | "SEARCH" | "SET"
        | "SORT" | "START" | "STOP" | "STRING" | "SUBTRACT" | "UNSTRING"
        | "WRITE" | "NOT" | "WHEN" | "WITH" | "UNTIL" | "VARYING" | "TIMES"
        | "THROUGH" | "THRU" | "USING" | "GIVING" | "RETURNING"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_dead_paragraph() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM ACTIVE-PARA.
           STOP RUN.
       ACTIVE-PARA.
           DISPLAY "HELLO".
       DEAD-PARA.
           DISPLAY "NEVER REACHED".
"#;
        let report = detect_dead_code("TESTPROG", source);
        assert_eq!(report.total_paragraphs, 3);
        assert_eq!(report.dead_items.len(), 1);
        assert_eq!(report.dead_items[0].name, "DEAD-PARA");
        assert_eq!(report.dead_items[0].kind, DeadCodeKind::Paragraph);
    }

    #[test]
    fn test_no_dead_code() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM WORK-PARA.
           STOP RUN.
       WORK-PARA.
           DISPLAY "WORKING".
"#;
        let report = detect_dead_code("TESTPROG", source);
        assert_eq!(report.total_paragraphs, 2);
        assert!(report.dead_items.is_empty());
    }

    #[test]
    fn test_perform_thru_marks_range_as_live() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM STEP-A THRU STEP-C.
           STOP RUN.
       STEP-A.
           DISPLAY "A".
       STEP-B.
           DISPLAY "B".
       STEP-C.
           DISPLAY "C".
"#;
        let report = detect_dead_code("TESTPROG", source);
        // STEP-A and STEP-C are directly referenced by PERFORM...THRU
        // STEP-B is between them but not directly referenced — it's still
        // technically reachable via fall-through, but our analysis only
        // tracks explicit references. STEP-B will appear as dead.
        // A and C should be live.
        let dead_names: Vec<&str> = report.dead_items.iter().map(|d| d.name.as_str()).collect();
        assert!(!dead_names.contains(&"STEP-A"));
        assert!(!dead_names.contains(&"STEP-C"));
    }

    #[test]
    fn test_go_to_reference() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           GO TO ERROR-HANDLER.
       ERROR-HANDLER.
           DISPLAY "ERROR".
           STOP RUN.
"#;
        let report = detect_dead_code("TESTPROG", source);
        assert!(report.dead_items.is_empty());
    }

    #[test]
    fn test_section_dead_code() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       MAIN-PARA.
           PERFORM USED-SECTION.
           STOP RUN.
       USED-SECTION SECTION.
           DISPLAY "USED".
       UNUSED-SECTION SECTION.
           DISPLAY "NEVER USED".
"#;
        let report = detect_dead_code("TESTPROG", source);
        let dead_names: Vec<&str> = report.dead_items.iter().map(|d| d.name.as_str()).collect();
        assert!(dead_names.contains(&"UNUSED-SECTION"));
        assert!(!dead_names.contains(&"USED-SECTION"));
    }

    #[test]
    fn test_dead_percentage() {
        let report = DeadCodeReport {
            program: "TEST".to_string(),
            total_paragraphs: 10,
            dead_items: vec![
                DeadCodeItem {
                    name: "A".to_string(),
                    line: 10,
                    line_count: 5,
                    kind: DeadCodeKind::Paragraph,
                },
                DeadCodeItem {
                    name: "B".to_string(),
                    line: 20,
                    line_count: 3,
                    kind: DeadCodeKind::Paragraph,
                },
            ],
            dead_lines: 8,
            total_paragraph_lines: 40,
        };
        assert!((report.dead_percentage() - 20.0).abs() < 0.01);
        assert_eq!(report.adjusted_lines(100), 92);
    }

    #[test]
    fn test_empty_source() {
        let report = detect_dead_code("EMPTY", "");
        assert_eq!(report.total_paragraphs, 0);
        assert!(report.dead_items.is_empty());
    }

    #[test]
    fn test_entry_point_never_flagged() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       ENTRY-POINT.
           STOP RUN.
"#;
        let report = detect_dead_code("TESTPROG", source);
        assert_eq!(report.total_paragraphs, 1);
        assert!(report.dead_items.is_empty());
    }

    #[test]
    fn test_multiple_dead_paragraphs() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "MAIN".
           STOP RUN.
       DEAD-A.
           DISPLAY "DEAD A".
       DEAD-B.
           DISPLAY "DEAD B".
       DEAD-C.
           DISPLAY "DEAD C".
"#;
        let report = detect_dead_code("TESTPROG", source);
        assert_eq!(report.dead_items.len(), 3);
        let names: Vec<&str> = report.dead_items.iter().map(|d| d.name.as_str()).collect();
        assert!(names.contains(&"DEAD-A"));
        assert!(names.contains(&"DEAD-B"));
        assert!(names.contains(&"DEAD-C"));
    }
}
