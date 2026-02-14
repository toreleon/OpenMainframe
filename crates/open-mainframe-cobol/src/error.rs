//! Error types for the COBOL compiler.

use miette::Diagnostic;
use thiserror::Error;

/// Errors that can occur during COBOL compilation.
#[derive(Debug, Error, Diagnostic)]
pub enum CobolError {
    /// I/O error when reading source files.
    #[error("I/O error: {message}")]
    #[diagnostic(code(cobol::io_error))]
    IoError {
        /// Description of the I/O error.
        message: String,
    },

    /// Lexical error (invalid token).
    #[error("Lexical error at line {line}, column {column}: {message}")]
    #[diagnostic(code(cobol::lexical_error), help("{help}"))]
    LexicalError {
        /// Line number (1-indexed).
        line: u32,
        /// Column number (1-indexed).
        column: u32,
        /// Description of the error.
        message: String,
        /// Suggestion for fixing the error.
        help: String,
    },

    /// Parse error (invalid syntax).
    #[error("Parse error: {message}")]
    #[diagnostic(code(cobol::parse_error))]
    ParseError {
        /// Description of the parse error.
        message: String,
    },

    /// Compilation failed.
    #[error("Compilation failed: {message}")]
    #[diagnostic(code(cobol::compilation_failed))]
    CompilationFailed {
        /// Description of the failure.
        message: String,
    },

    /// Invalid syntax at a specific location.
    #[error("Invalid syntax at line {line}: {message}")]
    #[diagnostic(code(cobol::invalid_syntax))]
    InvalidSyntax {
        /// Line number (1-indexed).
        line: usize,
        /// Description of the syntax error.
        message: String,
    },

    /// Copybook not found.
    #[error("Copybook not found: {name}")]
    #[diagnostic(
        code(cobol::copybook_not_found),
        help("Searched paths: {searched_paths}")
    )]
    CopybookNotFound {
        /// Name of the copybook.
        name: String,
        /// Paths that were searched.
        searched_paths: String,
    },

    /// Circular copybook inclusion detected.
    #[error("Circular copybook inclusion: {chain}")]
    #[diagnostic(
        code(cobol::circular_copybook),
        help("Check your COPY statements for circular dependencies")
    )]
    CircularCopybook {
        /// The chain of inclusions that form the cycle.
        chain: String,
    },

    /// Unterminated string literal.
    #[error("Unterminated string literal starting at line {line}")]
    #[diagnostic(code(cobol::unterminated_string))]
    UnterminatedString {
        /// Line where the string started.
        line: u32,
    },

    /// Invalid character in source.
    #[error("Invalid character '{character}' at line {line}, column {column}")]
    #[diagnostic(code(cobol::invalid_character))]
    InvalidCharacter {
        /// The invalid character.
        character: char,
        /// Line number.
        line: u32,
        /// Column number.
        column: u32,
    },

    /// Code generation error.
    #[error("Code generation error: {message}")]
    #[diagnostic(code(cobol::codegen_error))]
    CodegenError {
        /// Description of the code generation error.
        message: String,
    },
}
