//! Error types for the sort utility.

use thiserror::Error;

/// Errors that can occur during sort operations.
#[derive(Error, Debug)]
pub enum SortError {
    /// Invalid control statement syntax.
    #[error("Parse error at line {line}: {message}")]
    ParseError { line: usize, message: String },

    /// Invalid field specification.
    #[error("Invalid field specification: {0}")]
    InvalidField(String),

    /// Invalid data type.
    #[error("Invalid data type: {0}")]
    InvalidDataType(String),

    /// I/O error during sort.
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Record too large for buffer.
    #[error("Record exceeds maximum length: {0}")]
    RecordTooLarge(usize),

    /// Field position exceeds record length.
    #[error("Field position {pos} + length {len} exceeds record length {record_len}")]
    FieldOutOfBounds {
        pos: usize,
        len: usize,
        record_len: usize,
    },

    /// Invalid condition in INCLUDE/OMIT.
    #[error("Invalid condition: {0}")]
    InvalidCondition(String),

    /// Missing required input.
    #[error("Missing required input: {0}")]
    MissingInput(String),
}
