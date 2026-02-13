use miette::Diagnostic;
use thiserror::Error;

/// Runtime errors for COBOL program execution.
#[derive(Debug, Error, Diagnostic)]
pub enum RuntimeError {
    /// General execution failure.
    #[error("Runtime execution failed: {message}")]
    #[diagnostic(code(runtime::execution_failed))]
    ExecutionFailed { message: String },

    /// Invalid operation attempted.
    #[error("Invalid operation: {0}")]
    #[diagnostic(code(runtime::invalid_operation))]
    InvalidOperation(String),

    /// Arithmetic overflow (ON SIZE ERROR).
    #[error("Arithmetic overflow in {operation}")]
    #[diagnostic(code(runtime::size_error))]
    SizeError { operation: String },

    /// Division by zero.
    #[error("Division by zero")]
    #[diagnostic(code(runtime::divide_by_zero))]
    DivideByZero,

    /// Subscript out of range.
    #[error("Subscript out of range: index {index}, table size {size}")]
    #[diagnostic(code(runtime::subscript_out_of_range))]
    SubscriptOutOfRange { index: usize, size: usize },

    /// Invalid data (S0C7 equivalent).
    #[error("Invalid numeric data: {field}")]
    #[diagnostic(code(runtime::data_exception))]
    DataException { field: String },

    /// File I/O error.
    #[error("File I/O error: {message}")]
    #[diagnostic(code(runtime::file_io))]
    FileIoError { message: String },

    /// ABEND condition.
    #[error("ABEND {code}: {message}")]
    #[diagnostic(code(runtime::abend))]
    Abend { code: String, message: String },
}
