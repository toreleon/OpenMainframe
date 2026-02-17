use miette::Diagnostic;
use thiserror::Error;

/// Source location for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    /// 1-based line number in the JCL source.
    pub line: usize,
    /// 1-based column number.
    pub column: usize,
    /// The source line text (for display).
    pub source_line: String,
}

impl SourceLocation {
    /// Create a new source location.
    pub fn new(line: usize, column: usize, source_line: impl Into<String>) -> Self {
        Self {
            line,
            column,
            source_line: source_line.into(),
        }
    }

    /// Compute a `SourceLocation` from a byte offset into the full source text.
    ///
    /// Returns `None` if the offset is beyond the source length.
    pub fn from_offset(source: &str, offset: usize) -> Option<Self> {
        if offset > source.len() {
            return None;
        }

        let before = &source[..offset];
        let line = before.matches('\n').count() + 1;
        let last_newline = before.rfind('\n').map(|p| p + 1).unwrap_or(0);
        let column = offset - last_newline + 1;

        let line_start = last_newline;
        let line_end = source[offset..]
            .find('\n')
            .map(|p| offset + p)
            .unwrap_or(source.len());
        let source_line = source[line_start..line_end].to_string();

        Some(Self {
            line,
            column,
            source_line,
        })
    }

    /// Format the error location with a caret pointing to the error position.
    pub fn format_with_caret(&self) -> String {
        let caret = format!("{:>width$}", "^", width = self.column);
        format!(
            "line {}:\n  {}\n  {}",
            self.line, self.source_line, caret
        )
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum JclError {
    #[error("JCL parse error: {message}")]
    #[diagnostic(code(jcl::parse_error))]
    ParseError { message: String },

    #[error("JCL parse error at {location}: {message}")]
    #[diagnostic(code(jcl::parse_error_at))]
    ParseErrorAt {
        message: String,
        location: SourceLocation,
    },

    #[error("Invalid JCL statement: {0}")]
    #[diagnostic(code(jcl::invalid_statement))]
    InvalidStatement(String),

    #[error("Job execution failed: {message}")]
    #[diagnostic(code(jcl::execution_failed))]
    ExecutionFailed { message: String },
}

impl JclError {
    /// Create a parse error with source location from a byte offset.
    pub fn parse_error_at(message: impl Into<String>, source: &str, offset: usize) -> Self {
        if let Some(location) = SourceLocation::from_offset(source, offset) {
            JclError::ParseErrorAt {
                message: message.into(),
                location,
            }
        } else {
            JclError::ParseError {
                message: message.into(),
            }
        }
    }

    /// Get the source location if available.
    pub fn location(&self) -> Option<&SourceLocation> {
        match self {
            JclError::ParseErrorAt { location, .. } => Some(location),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_location_from_offset() {
        let source = "//MYJOB  JOB CLASS=A\n//STEP1  EXEC PGM=IEFBR14\n//";
        // Offset 0 = first char of line 1
        let loc = SourceLocation::from_offset(source, 0).unwrap();
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 1);
        assert_eq!(loc.source_line, "//MYJOB  JOB CLASS=A");

        // Offset at start of line 2
        let loc = SourceLocation::from_offset(source, 21).unwrap();
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 1);
        assert_eq!(loc.source_line, "//STEP1  EXEC PGM=IEFBR14");
    }

    #[test]
    fn test_source_location_from_offset_mid_line() {
        let source = "//JOB1   JOB CLASS=A\n//BAD    XYZ OOPS";
        // Point at 'X' in XYZ (offset = 21 + 9 = 30)
        let loc = SourceLocation::from_offset(source, 30).unwrap();
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 10);
        assert!(loc.source_line.contains("XYZ"));
    }

    #[test]
    fn test_source_location_display() {
        let loc = SourceLocation::new(5, 12, "//STEP1  EXEC PGM=BAD");
        assert_eq!(format!("{}", loc), "line 5, column 12");
    }

    #[test]
    fn test_source_location_format_with_caret() {
        let loc = SourceLocation::new(3, 5, "//STEP1  EXEC");
        let formatted = loc.format_with_caret();
        assert!(formatted.contains("line 3:"));
        assert!(formatted.contains("//STEP1  EXEC"));
        assert!(formatted.contains("    ^")); // 4 spaces + caret at col 5
    }

    #[test]
    fn test_parse_error_at_helper() {
        let source = "//JOB1   JOB CLASS=A\n//BAD    INVALID";
        let err = JclError::parse_error_at("Unexpected operation", source, 21);
        if let JclError::ParseErrorAt { message, location } = &err {
            assert_eq!(message, "Unexpected operation");
            assert_eq!(location.line, 2);
            assert_eq!(location.column, 1);
        } else {
            panic!("Expected ParseErrorAt");
        }
        assert!(err.location().is_some());
    }

    #[test]
    fn test_parse_error_at_beyond_source() {
        let source = "//JOB1   JOB CLASS=A";
        let err = JclError::parse_error_at("Truncated", source, 9999);
        // Should fall back to ParseError without location
        assert!(matches!(err, JclError::ParseError { .. }));
        assert!(err.location().is_none());
    }

    #[test]
    fn test_error_display_with_location() {
        let err = JclError::ParseErrorAt {
            message: "Unknown operation XYZ".to_string(),
            location: SourceLocation::new(5, 10, "//STEP1  XYZ"),
        };
        let msg = format!("{}", err);
        assert!(msg.contains("line 5"));
        assert!(msg.contains("Unknown operation XYZ"));
    }

    #[test]
    fn test_error_variants_location() {
        let parse_err = JclError::ParseError {
            message: "test".to_string(),
        };
        assert!(parse_err.location().is_none());

        let exec_err = JclError::ExecutionFailed {
            message: "test".to_string(),
        };
        assert!(exec_err.location().is_none());
    }
}
