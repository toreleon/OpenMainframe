use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum DatasetError {
    #[error("Dataset not found: {name}")]
    #[diagnostic(code(dataset::not_found))]
    NotFound { name: String },

    #[error("I/O error: {message}")]
    #[diagnostic(code(dataset::io_error))]
    IoError { message: String },

    #[error("Invalid record format: {0}")]
    #[diagnostic(code(dataset::invalid_record_format))]
    InvalidRecordFormat(String),
}
