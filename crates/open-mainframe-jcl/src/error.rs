use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum JclError {
    #[error("JCL parse error: {message}")]
    #[diagnostic(code(jcl::parse_error))]
    ParseError { message: String },

    #[error("Invalid JCL statement: {0}")]
    #[diagnostic(code(jcl::invalid_statement))]
    InvalidStatement(String),

    #[error("Job execution failed: {message}")]
    #[diagnostic(code(jcl::execution_failed))]
    ExecutionFailed { message: String },
}
