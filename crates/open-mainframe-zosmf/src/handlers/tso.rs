//! /zosmf/tsoApp/* — TSO command execution REST API endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register TSO routes (implemented in ZOW-104).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
