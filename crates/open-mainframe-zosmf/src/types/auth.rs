//! Authentication and session types.

use serde::{Deserialize, Serialize};

/// Response body for successful authentication.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoginResponse {
    /// JWT token string.
    pub token: String,
}

/// Authenticated user stored in session map.
#[derive(Debug, Clone)]
pub struct AuthenticatedUser {
    /// RACF user ID (uppercase).
    pub userid: String,
    /// JWT token value.
    pub token: String,
    /// Expiry timestamp (seconds since epoch).
    pub expires_at: u64,
}

/// Authentication context extracted from requests by the auth middleware.
#[derive(Debug, Clone)]
pub struct AuthContext {
    /// Authenticated RACF user ID (uppercase).
    pub userid: String,
}
