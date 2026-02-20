//! z/OSMF info endpoint types.

use serde::{Deserialize, Serialize};

/// Response body for `GET /zosmf/info`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZosmfInfo {
    /// z/OSMF API version string.
    pub api_version: String,
    /// z/OSMF version string.
    pub zosmf_version: String,
    /// Hostname of the z/OSMF server.
    pub zosmf_hostname: String,
    /// z/OS version string.
    pub zos_version: String,
    /// SAF realm name.
    pub zosmf_saf_realm: String,
    /// List of installed plugins.
    pub plugins: Vec<ZosmfPlugin>,
}

/// A z/OSMF plugin descriptor.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZosmfPlugin {
    /// Plugin name.
    #[serde(rename = "pluginVersion")]
    pub plugin_version: String,
    /// Plugin status.
    #[serde(rename = "pluginDefaultName")]
    pub plugin_default_name: String,
    /// Plugin status.
    #[serde(rename = "pluginStatus")]
    pub plugin_status: String,
}
