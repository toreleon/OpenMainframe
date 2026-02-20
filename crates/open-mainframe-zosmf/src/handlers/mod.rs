//! z/OSMF REST API route handlers — assembles all endpoint routes into a single Axum router.

pub mod authenticate;
pub mod console;
pub mod datasets;
pub mod files;
pub mod info;
pub mod jobs;
pub mod tso;

use std::sync::Arc;

use axum::Router;

use crate::state::AppState;

/// Build the complete z/OSMF REST API router with all routes.
pub fn build_router(state: Arc<AppState>) -> Router {
    Router::new()
        .merge(info::routes())
        .merge(authenticate::routes())
        .merge(datasets::routes())
        .merge(jobs::routes())
        .merge(tso::routes())
        .merge(console::routes())
        .merge(files::routes())
        .with_state(state)
}
