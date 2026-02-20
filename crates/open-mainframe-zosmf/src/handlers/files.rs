//! /zosmf/restfiles/fs/* — USS file operations REST API endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register USS file routes (implemented in ZOW-106).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
