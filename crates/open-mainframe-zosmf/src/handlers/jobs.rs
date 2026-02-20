//! /zosmf/restjobs/jobs/* — job management REST API endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register job routes (implemented in ZOW-103).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
