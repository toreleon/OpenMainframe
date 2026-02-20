//! z/OSMF console REST API types.

use serde::{Deserialize, Serialize};

/// Console command request body.
#[derive(Debug, Clone, Deserialize)]
pub struct ConsoleRequest {
    /// MVS command text.
    pub cmd: String,
    /// Solicitation key for response matching.
    #[serde(rename = "sol-key", default)]
    pub sol_key: Option<String>,
    /// Async mode flag ("Y" for async).
    #[serde(rename = "async", default)]
    pub async_mode: Option<String>,
    /// Target system name.
    #[serde(default)]
    pub system: Option<String>,
}

/// Console command response body.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsoleResponse {
    /// Command response text.
    #[serde(rename = "cmd-response")]
    pub cmd_response: String,
    /// Solicitation key detected in response.
    #[serde(rename = "sol-key-detected", skip_serializing_if = "Option::is_none")]
    pub sol_key_detected: Option<String>,
}
