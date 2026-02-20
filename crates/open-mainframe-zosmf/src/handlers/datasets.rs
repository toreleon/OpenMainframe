//! /zosmf/restfiles/ds/* — dataset REST API endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register dataset routes (implemented in ZOW-102).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
