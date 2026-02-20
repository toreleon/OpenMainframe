//! z/OSMF console REST API types.

use serde::{Deserialize, Serialize};

/// Console command request body.
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct ConsoleRequest {
    /// MVS command text.
    pub cmd: String,
    /// Solicitation key for response matching.
    #[serde(rename = "sol-key", default)]
    pub sol_key: Option<String>,
    /// Solicitation key regex pattern.
    #[serde(rename = "sol-key-reg", default)]
    pub sol_key_reg: Option<String>,
    /// Unsolicited message key.
    #[serde(rename = "unsol-key", default)]
    pub unsol_key: Option<String>,
    /// Unsolicited key regex pattern.
    #[serde(rename = "unsol-key-reg", default)]
    pub unsol_key_reg: Option<String>,
    /// Detection time in seconds.
    #[serde(rename = "detect-time", default)]
    pub detect_time: Option<String>,
    /// Unsolicited detection sync mode.
    #[serde(rename = "unsol-detect-sync", default)]
    pub unsol_detect_sync: Option<String>,
    /// Unsolicited detection timeout.
    #[serde(rename = "unsol-detect-timeout", default)]
    pub unsol_detect_timeout: Option<String>,
    /// Route code.
    #[serde(default)]
    pub routcode: Option<String>,
    /// Authorization level.
    #[serde(default)]
    pub auth: Option<String>,
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
    /// Command response URL for follow-up.
    #[serde(rename = "cmd-response-url", skip_serializing_if = "Option::is_none")]
    pub cmd_response_url: Option<String>,
    /// Command response text.
    #[serde(rename = "cmd-response")]
    pub cmd_response: String,
    /// Command response key for follow-up requests.
    #[serde(rename = "cmd-response-key", skip_serializing_if = "Option::is_none")]
    pub cmd_response_key: Option<String>,
    /// Whether the solicitation keyword was detected in the response.
    #[serde(rename = "sol-key-detected", skip_serializing_if = "Option::is_none")]
    pub sol_key_detected: Option<bool>,
    /// Command response URI (same as cmd-response-url, IBM provides both).
    #[serde(rename = "cmd-response-uri", skip_serializing_if = "Option::is_none")]
    pub cmd_response_uri: Option<String>,
}
