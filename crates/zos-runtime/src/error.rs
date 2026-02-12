use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum RuntimeError {
    #[error("Runtime execution failed: {message}")]
    #[diagnostic(code(runtime::execution_failed))]
    ExecutionFailed { message: String },

    #[error("Invalid operation: {0}")]
    #[diagnostic(code(runtime::invalid_operation))]
    InvalidOperation(String),
}
