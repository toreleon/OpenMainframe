//! POST/DELETE /zosmf/services/authenticate — login and logout endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register authentication routes (implemented in ZOW-101).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
