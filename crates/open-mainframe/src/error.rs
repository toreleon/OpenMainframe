use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum CliError {
    #[error("Configuration error: {message}")]
    #[diagnostic(code(cli::config_error))]
    ConfigError { message: String },

    #[error("Command failed: {message}")]
    #[diagnostic(code(cli::command_failed))]
    CommandFailed { message: String },
}
