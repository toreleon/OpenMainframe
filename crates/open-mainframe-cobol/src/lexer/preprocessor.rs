//! COBOL Source Preprocessor.
//!
//! Expands COPY statements by inlining copybook content.
//! Handles:
//! - Basic COPY statement: COPY copybook-name.
//! - COPY with library: COPY copybook-name IN library.
//! - COPY with REPLACING: COPY copybook-name REPLACING ==old== BY ==new==.
//! - Nested copybook inclusion
//! - Circular inclusion detection

use crate::error::CobolError;
use crate::lexer::copybook::{apply_replacements, CopybookConfig, CopybookResolver, Replacement, ReplacementMode};
use crate::lexer::source::SourceFormat;

/// Result type for preprocessor operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// COBOL source preprocessor.
///
/// Expands COPY statements by resolving and inlining copybook content.
#[derive(Debug)]
pub struct Preprocessor {
    /// Copybook resolver.
    resolver: CopybookResolver,
    /// Maximum nesting depth for copybooks.
    max_depth: usize,
}

impl Preprocessor {
    /// Create a new preprocessor.
    pub fn new(config: CopybookConfig, format: SourceFormat) -> Self {
        Self {
            resolver: CopybookResolver::new(config, format),
            max_depth: 10,
        }
    }

    /// Set the maximum copybook nesting depth.
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Preprocess COBOL source, expanding all COPY statements.
    ///
    /// Returns the expanded source with all copybooks inlined.
    pub fn preprocess(&mut self, source: &str) -> Result<String> {
        let expanded = self.preprocess_recursive(source, 0)?;
        // Post-process: expand CICS DFHRESP() pseudo-function calls
        Ok(Self::expand_dfhresp(&expanded))
    }

    /// Expand DFHRESP(condition) pseudo-function calls to numeric CICS response codes.
    fn expand_dfhresp(source: &str) -> String {
        let mut result = String::with_capacity(source.len());
        let mut remaining = source.as_bytes();

        while !remaining.is_empty() {
            // Find next DFHRESP( occurrence (case-insensitive)
            if let Some(pos) = find_dfhresp(remaining) {
                // Copy everything before the match
                result.push_str(&source[source.len() - remaining.len()..source.len() - remaining.len() + pos]);
                remaining = &remaining[pos..];

                // Find the matching closing paren
                let prefix_len = b"DFHRESP(".len();
                if remaining.len() > prefix_len {
                    if let Some(close) = remaining[prefix_len..].iter().position(|&b| b == b')') {
                        let condition = std::str::from_utf8(&remaining[prefix_len..prefix_len + close])
                            .unwrap_or("")
                            .trim();
                        let code = dfhresp_code(condition);
                        result.push_str(&code.to_string());
                        remaining = &remaining[prefix_len + close + 1..];
                        continue;
                    }
                }
                // No closing paren found, copy the DFHRESP literally
                result.push_str(&source[source.len() - remaining.len()..source.len() - remaining.len() + 1]);
                remaining = &remaining[1..];
            } else {
                // No more DFHRESP found, copy the rest
                result.push_str(&source[source.len() - remaining.len()..]);
                break;
            }
        }

        result
    }

    /// Recursively preprocess source, tracking depth.
    fn preprocess_recursive(&mut self, source: &str, depth: usize) -> Result<String> {
        if depth > self.max_depth {
            return Err(CobolError::ParseError {
                message: format!(
                    "Maximum copybook nesting depth ({}) exceeded",
                    self.max_depth
                ),
            });
        }

        let mut result = String::with_capacity(source.len());
        let mut copy_buffer = String::new();
        let mut in_copy = false;

        for line in source.lines() {
            // Check if this is a comment line (column 7 = '*' or '/')
            let is_comment = line.len() > 6 && {
                let c7 = line.chars().nth(6).unwrap_or(' ');
                c7 == '*' || c7 == '/'
            };

            if is_comment {
                result.push_str(line);
                result.push('\n');
                continue;
            }

            // Get content starting from column 8 (index 7)
            let content = if line.len() > 7 { &line[7..] } else { "" };

            // Check if continuing a multi-line COPY
            if in_copy {
                copy_buffer.push(' ');
                copy_buffer.push_str(content.trim());

                // Check if COPY statement ends with period
                if copy_buffer.trim().ends_with('.') {
                    let expanded = self.expand_copy(&copy_buffer, depth)?;
                    result.push_str(&expanded);
                    copy_buffer.clear();
                    in_copy = false;
                }
                continue;
            }

            // Check for COPY statement start
            let trimmed = content.trim();
            if trimmed.to_uppercase().starts_with("COPY ") {
                copy_buffer = trimmed.to_string();

                // Check if COPY statement ends on this line
                if trimmed.ends_with('.') {
                    let expanded = self.expand_copy(&copy_buffer, depth)?;
                    result.push_str(&expanded);
                    copy_buffer.clear();
                } else {
                    in_copy = true;
                }
                continue;
            }

            // Regular line - pass through
            result.push_str(line);
            result.push('\n');
        }

        // Handle unclosed COPY (error)
        if in_copy {
            return Err(CobolError::ParseError {
                message: format!("Unclosed COPY statement: {}", copy_buffer),
            });
        }

        Ok(result)
    }

    /// Expand a COPY statement into the copybook content.
    fn expand_copy(&mut self, stmt: &str, depth: usize) -> Result<String> {
        let parsed = self.parse_copy_statement(stmt)?;

        // Resolve and load the copybook
        let path = self.resolver.resolve(&parsed.copybook_name)?;
        let content = std::fs::read_to_string(&path).map_err(|e| CobolError::ParseError {
            message: format!("Failed to read copybook '{}': {}", parsed.copybook_name, e),
        })?;

        // Apply REPLACING if present
        let content = if parsed.replacements.is_empty() {
            content
        } else {
            apply_replacements(&content, &parsed.replacements)
        };

        // Track nesting for circular detection
        self.resolver.push_include(&parsed.copybook_name)?;

        // Recursively preprocess (handles nested COPY)
        let expanded = self.preprocess_recursive(&content, depth + 1)?;

        self.resolver.pop_include();

        // Add comment markers for debugging
        Ok(format!(
            "      *>>> COPY {} from {}\n{}\n      *<<< END COPY {}\n",
            parsed.copybook_name,
            path.display(),
            expanded.trim_end(),
            parsed.copybook_name
        ))
    }

    /// Parse a COPY statement to extract copybook name and REPLACING clause.
    fn parse_copy_statement(&self, stmt: &str) -> Result<CopyStatement> {
        // Expected format: COPY name [IN library] [REPLACING ...].
        let stmt = stmt.trim().trim_end_matches('.');
        let parts: Vec<&str> = stmt.split_whitespace().collect();

        if parts.is_empty() || !parts[0].eq_ignore_ascii_case("COPY") {
            return Err(CobolError::ParseError {
                message: format!("Invalid COPY statement: {}", stmt),
            });
        }

        if parts.len() < 2 {
            return Err(CobolError::ParseError {
                message: "COPY statement missing copybook name".to_string(),
            });
        }

        // Get copybook name (strip quotes if present)
        let copybook_name = parts[1]
            .trim_matches('\'')
            .trim_matches('"')
            .to_string();
        let mut idx = 2;

        // Check for IN/OF library
        if idx < parts.len()
            && (parts[idx].eq_ignore_ascii_case("IN") || parts[idx].eq_ignore_ascii_case("OF"))
        {
            idx += 2; // Skip "IN" and library name
        }

        // Check for REPLACING clause
        let mut replacements = Vec::new();
        if idx < parts.len() && parts[idx].eq_ignore_ascii_case("REPLACING") {
            idx += 1;
            // Collect the rest of the REPLACING clause
            let replacing_text: String = parts[idx..].join(" ");
            replacements = self.parse_replacing(&replacing_text);
        }

        Ok(CopyStatement {
            copybook_name,
            replacements,
        })
    }

    /// Parse REPLACING clause into replacements.
    fn parse_replacing(&self, text: &str) -> Vec<Replacement> {
        let mut replacements = Vec::new();
        let mut current_from = String::new();
        let mut current_to = String::new();
        let mut collecting_to = false;
        let mut current_mode = ReplacementMode::Full;

        for part in text.split_whitespace() {
            if part.eq_ignore_ascii_case("BY") {
                collecting_to = true;
                continue;
            }

            // Check for LEADING/TRAILING before a new pattern
            if !collecting_to && current_from.is_empty() {
                if part.eq_ignore_ascii_case("LEADING") {
                    current_mode = ReplacementMode::Leading;
                    continue;
                } else if part.eq_ignore_ascii_case("TRAILING") {
                    current_mode = ReplacementMode::Trailing;
                    continue;
                }
            }

            if collecting_to {
                // Check if this starts a new pattern or LEADING/TRAILING
                let is_new_pattern = part.starts_with("==") && !current_to.is_empty();
                let is_mode_keyword = (part.eq_ignore_ascii_case("LEADING")
                    || part.eq_ignore_ascii_case("TRAILING"))
                    && !current_to.is_empty();

                if is_new_pattern || is_mode_keyword {
                    // Finish current replacement
                    if !current_from.is_empty() {
                        replacements.push(Replacement::with_mode(
                            Self::clean_delimiters(&current_from),
                            Self::clean_delimiters(&current_to),
                            current_mode,
                        ));
                    }
                    current_mode = ReplacementMode::Full;
                    if is_mode_keyword {
                        if part.eq_ignore_ascii_case("LEADING") {
                            current_mode = ReplacementMode::Leading;
                        } else {
                            current_mode = ReplacementMode::Trailing;
                        }
                        current_from.clear();
                    } else {
                        current_from = part.to_string();
                    }
                    current_to.clear();
                    collecting_to = false;
                } else {
                    if !current_to.is_empty() {
                        current_to.push(' ');
                    }
                    current_to.push_str(part);
                }
            } else {
                if !current_from.is_empty() {
                    current_from.push(' ');
                }
                current_from.push_str(part);
            }
        }

        // Handle last replacement
        if !current_from.is_empty() {
            replacements.push(Replacement::with_mode(
                Self::clean_delimiters(&current_from),
                Self::clean_delimiters(&current_to),
                current_mode,
            ));
        }

        replacements
    }

    /// Remove == delimiters from pattern.
    fn clean_delimiters(s: &str) -> String {
        s.trim_start_matches("==")
            .trim_end_matches("==")
            .to_string()
    }
}

/// Find position of case-insensitive "DFHRESP(" in byte slice.
fn find_dfhresp(haystack: &[u8]) -> Option<usize> {
    let needle = b"DFHRESP(";
    if haystack.len() < needle.len() {
        return None;
    }
    for i in 0..=haystack.len() - needle.len() {
        if haystack[i..i + needle.len()]
            .iter()
            .zip(needle.iter())
            .all(|(a, b)| a.to_ascii_uppercase() == b.to_ascii_uppercase())
        {
            return Some(i);
        }
    }
    None
}

/// Map a CICS condition name to its numeric response code.
fn dfhresp_code(condition: &str) -> i32 {
    match condition.to_uppercase().as_str() {
        "NORMAL" => 0,
        "ERROR" => 1,
        "RDATT" => 2,
        "WRBRK" => 3,
        "EOF" => 4,
        "EODS" => 5,
        "EOC" => 6,
        "INBFMH" => 7,
        "ENDINPT" => 8,
        "NONVAL" => 9,
        "NOSTART" => 10,
        "TERMIDERR" => 11,
        "FILENOTFOUND" | "DSIDERR" => 12,
        "NOTFND" => 13,
        "DUPREC" => 14,
        "DUPKEY" => 15,
        "INVREQ" => 16,
        "IOERR" => 17,
        "NOSPACE" => 18,
        "NOTOPEN" => 19,
        "ENDFILE" => 20,
        "ILLOGIC" => 21,
        "LENGERR" => 22,
        "QZERO" => 23,
        "SIGNAL" => 24,
        "QBUSY" => 25,
        "ITEMERR" => 26,
        "PGMIDERR" => 27,
        "TRANSIDERR" => 28,
        "ENDDATA" => 29,
        "INVTSREQ" => 30,
        "EXPIRED" => 31,
        "MAPFAIL" => 36,
        "INVMPSZ" => 38,
        "OVERFLOW" => 40,
        "INVLDC" => 41,
        "NOSTG" => 42,
        "JIDERR" => 43,
        "QIDERR" => 44,
        "NOJBUFSP" => 45,
        "DSSTAT" => 46,
        "SELNERR" => 47,
        "FUNCERR" => 48,
        "UNEXPIN" => 49,
        "NOPASSBKRD" => 50,
        "NOPASSBKWR" => 51,
        "SYSIDERR" => 53,
        "ISCINVREQ" => 54,
        "ENQBUSY" => 55,
        "ENVDEFERR" => 56,
        "IGREQCD" => 57,
        "SESSIONERR" => 58,
        "SYSBUSY" => 59,
        "SESSBUSY" => 60,
        "NOTALLOC" => 61,
        "CBIDERR" => 62,
        "INVEXITREQ" => 63,
        "INVPARTNSET" => 64,
        "INVPARTN" => 65,
        "PARTNFAIL" => 66,
        "USERIDERR" => 69,
        "NOTAUTH" => 70,
        "VOLIDERR" => 71,
        "SUPPRESSED" => 72,
        "RESIDERR" => 76,
        "NOSPOOL" => 80,
        "TERMERR" => 81,
        "ROLLEDBACK" => 82,
        "END" => 83,
        "DISABLED" => 84,
        "ALLOCERR" => 85,
        "STRELERR" => 86,
        "OPENERR" => 87,
        "SPOLBUSY" => 88,
        "SPOLDERR" => 89,
        "LOADING" => 94,
        "MODELIDERR" => 95,
        "OUTDESCRERR" => 96,
        "PARTNERIDERR" => 97,
        "PROFILEIDERR" => 98,
        "NETNAMEIDERR" => 99,
        "LOCKED" => 100,
        "RECORDBUSY" => 101,
        "UOWNOTFOUND" => 102,
        "UOWLNOTFOUND" => 103,
        "LSRPOOLFULL" => 104,
        "CHANNELERR" => 122,
        "CCSIDERR" => 123,
        "TIMEDOUT" => 124,
        "CODEPAGEERR" => 125,
        "INCOMPLETE" => 126,
        "CONTAINERERR" => 127,
        "TOKENERR" => 128,
        _ => {
            eprintln!("WARNING: Unknown DFHRESP condition: {}", condition);
            -1
        }
    }
}

/// Parsed COPY statement.
#[derive(Debug)]
struct CopyStatement {
    /// Name of the copybook.
    copybook_name: String,
    /// REPLACING substitutions.
    replacements: Vec<Replacement>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_config() -> CopybookConfig {
        CopybookConfig::new()
    }

    #[test]
    fn test_parse_simple_copy() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
        assert!(parsed.replacements.is_empty());
    }

    #[test]
    fn test_parse_copy_with_in() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK IN MYLIB.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
    }

    #[test]
    fn test_parse_copy_with_replacing() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let stmt = "COPY MYBOOK REPLACING ==:PREFIX:== BY ==WS-==.";
        let parsed = preprocessor.parse_copy_statement(stmt).unwrap();
        assert_eq!(parsed.copybook_name, "MYBOOK");
        assert_eq!(parsed.replacements.len(), 1);
        assert_eq!(parsed.replacements[0].from, ":PREFIX:");
        assert_eq!(parsed.replacements[0].to, "WS-");
    }

    #[test]
    fn test_parse_replacing_multiple() {
        let preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let text = "==:A:== BY ==X== ==:B:== BY ==Y==";
        let replacements = preprocessor.parse_replacing(text);
        assert_eq!(replacements.len(), 2);
        assert_eq!(replacements[0].from, ":A:");
        assert_eq!(replacements[0].to, "X");
        assert_eq!(replacements[1].from, ":B:");
        assert_eq!(replacements[1].to, "Y");
    }

    #[test]
    fn test_preprocess_no_copy() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let source = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_comment_passthrough() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        let source = "      * This is a comment\n       DATA DIVISION.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_commented_copy_ignored() {
        let mut preprocessor = Preprocessor::new(test_config(), SourceFormat::Fixed);
        // Commented COPY statement should be passed through unchanged
        let source = "      *COPY DFHATTR.\n";
        let result = preprocessor.preprocess(source).unwrap();
        assert_eq!(result, source);
    }
}
