//! z/OSMF TSO REST API types.

use serde::{Deserialize, Serialize};

/// Response for TSO address space start.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TsoStartResponse {
    /// Unique servlet key for the session.
    pub servlet_key: String,
    /// Queue ID for the session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub queue_id: Option<String>,
    /// Initial output data.
    #[serde(default)]
    pub tso_data: Vec<TsoData>,
}

/// A TSO data entry in responses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TsoData {
    /// TSO message content.
    #[serde(rename = "TSO MESSAGE", skip_serializing_if = "Option::is_none")]
    pub tso_message: Option<TsoMessage>,
}

/// TSO message with data payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TsoMessage {
    /// Message data text.
    #[serde(rename = "DATA")]
    pub data: String,
}

/// Response for TSO command execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TsoCommandResponse {
    /// Servlet key.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub servlet_key: Option<String>,
    /// Output data.
    #[serde(default)]
    pub tso_data: Vec<TsoData>,
}

/// Request body for stateless TSO command.
#[derive(Debug, Clone, Deserialize)]
pub struct TsoCommandRequest {
    /// TSO command text.
    #[serde(rename = "TSO COMMAND")]
    pub tso_command: String,
}

/// Request body for starting a TSO address space.
#[derive(Debug, Clone, Deserialize)]
pub struct TsoStartRequest {
    /// Start parameters.
    #[serde(rename = "startTso", default)]
    pub start_tso: Option<TsoStartParams>,
    /// Stateless command (alternative to startTso).
    #[serde(rename = "TSO COMMAND", default)]
    pub tso_command: Option<String>,
}

/// TSO address space start parameters.
#[derive(Debug, Clone, Deserialize)]
pub struct TsoStartParams {
    /// Logon procedure name.
    #[serde(default)]
    pub proc: Option<String>,
    /// Account number.
    #[serde(default)]
    pub acct: Option<String>,
    /// Screen rows.
    #[serde(default = "default_rows")]
    pub rows: u32,
    /// Screen columns.
    #[serde(default = "default_cols")]
    pub cols: u32,
}

/// Query parameters for TSO start (Zowe CLI sends params in URL).
#[derive(Debug, Clone, Default, Deserialize)]
pub struct TsoStartQuery {
    /// Account number.
    #[serde(default)]
    pub acct: Option<String>,
    /// Logon procedure.
    #[serde(default)]
    pub proc: Option<String>,
    /// Character set.
    #[serde(default)]
    pub chset: Option<String>,
    /// Code page.
    #[serde(default)]
    pub cpage: Option<String>,
    /// Screen rows.
    #[serde(default)]
    pub rows: Option<u32>,
    /// Screen columns.
    #[serde(default)]
    pub cols: Option<u32>,
    /// Region size.
    #[serde(default)]
    pub rsize: Option<u32>,
}

fn default_rows() -> u32 {
    24
}

fn default_cols() -> u32 {
    80
}
