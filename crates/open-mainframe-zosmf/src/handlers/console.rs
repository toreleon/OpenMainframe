//! /zosmf/restconsoles/* — console command REST API endpoints.

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Register console routes (implemented in ZOW-105).
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
}
