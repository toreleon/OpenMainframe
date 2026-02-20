//! /zosmf/tsoApp/* — TSO command execution REST API endpoints.
//!
//! Implements the z/OSMF TSO REST services:
//! - `POST   /zosmf/tsoApp/tso` — start TSO address space or issue stateless command
//! - `PUT    /zosmf/tsoApp/tso/:servletKey` — send command to TSO session
//! - `GET    /zosmf/tsoApp/tso/:servletKey` — receive TSO session response
//! - `DELETE /zosmf/tsoApp/tso/:servletKey` — stop TSO address space

use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use open_mainframe_tso::{execute, parse_command, TsoSession};

use crate::state::{AppState, TsoSessionHandle};
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;
use crate::types::tso::{
    TsoCommandRequest, TsoCommandResponse, TsoData, TsoMessage, TsoStartRequest,
    TsoStartResponse,
};

/// Register TSO routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/tsoApp/tso", post(start_or_command))
        .route("/zosmf/tsoApp/tso/{servlet_key}", put(send_command))
        .route("/zosmf/tsoApp/tso/{servlet_key}", get(receive_response))
        .route("/zosmf/tsoApp/tso/{servlet_key}", delete(stop_session))
}

/// Generate a unique servlet key.
fn generate_servlet_key() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("TSO-{:016X}", ts)
}

/// Convert output lines to TsoData entries.
fn lines_to_tso_data(lines: &[String]) -> Vec<TsoData> {
    lines
        .iter()
        .map(|line| TsoData {
            tso_message: Some(TsoMessage {
                data: line.clone(),
            }),
        })
        .collect()
}

/// POST /zosmf/tsoApp/tso — start TSO address space or issue stateless command.
async fn start_or_command(
    State(state): State<Arc<AppState>>,
    auth: AuthContext,
    Json(request): Json<TsoStartRequest>,
) -> std::result::Result<(StatusCode, Json<TsoStartResponse>), ZosmfErrorResponse> {
    // Stateless command — execute immediately and return.
    if let Some(ref cmd_text) = request.tso_command {
        let dataset_dir = PathBuf::from(&state.config.uss.root_directory);
        let mut session = TsoSession::new(&auth.userid, dataset_dir);
        let parsed = parse_command(cmd_text);
        let result = execute(&mut session, &parsed);

        return Ok((
            StatusCode::OK,
            Json(TsoStartResponse {
                servlet_key: String::new(),
                queue_id: None,
                tso_data: lines_to_tso_data(&result.output),
            }),
        ));
    }

    // Start a new TSO session.
    let servlet_key = generate_servlet_key();
    let dataset_dir = PathBuf::from(&state.config.uss.root_directory);
    let session = TsoSession::new(&auth.userid, dataset_dir);

    let welcome = vec![
        format!("IKJ56455I {} LOGON IN PROGRESS AT HH:MM:SS ON MM/DD/YYYY", auth.userid),
        "IKJ56951I NO BROADCAST MESSAGES".to_string(),
        "READY ".to_string(),
    ];

    let tso_data = lines_to_tso_data(&welcome);

    state.tso_sessions.insert(
        servlet_key.clone(),
        TsoSessionHandle {
            session,
            output_buffer: Vec::new(),
            userid: auth.userid,
        },
    );

    Ok((
        StatusCode::CREATED,
        Json(TsoStartResponse {
            servlet_key,
            queue_id: Some("0".to_string()),
            tso_data,
        }),
    ))
}

/// PUT /zosmf/tsoApp/tso/:servletKey — send command to TSO session.
async fn send_command(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(servlet_key): Path<String>,
    Json(request): Json<TsoCommandRequest>,
) -> std::result::Result<Json<TsoCommandResponse>, ZosmfErrorResponse> {
    let mut handle = state
        .tso_sessions
        .get_mut(&servlet_key)
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("TSO session '{}' not found", servlet_key))
        })?;

    let parsed = parse_command(&request.tso_command);
    let result = execute(&mut handle.session, &parsed);

    // Store output in buffer for GET retrieval.
    handle.output_buffer.extend(result.output.clone());

    Ok(Json(TsoCommandResponse {
        servlet_key: Some(servlet_key),
        tso_data: lines_to_tso_data(&result.output),
    }))
}

/// GET /zosmf/tsoApp/tso/:servletKey — receive TSO session response.
async fn receive_response(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(servlet_key): Path<String>,
) -> std::result::Result<Json<TsoCommandResponse>, ZosmfErrorResponse> {
    let mut handle = state
        .tso_sessions
        .get_mut(&servlet_key)
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("TSO session '{}' not found", servlet_key))
        })?;

    let buffered: Vec<String> = handle.output_buffer.drain(..).collect();

    Ok(Json(TsoCommandResponse {
        servlet_key: Some(servlet_key),
        tso_data: lines_to_tso_data(&buffered),
    }))
}

/// DELETE /zosmf/tsoApp/tso/:servletKey — stop TSO address space.
async fn stop_session(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(servlet_key): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    state
        .tso_sessions
        .remove(&servlet_key)
        .ok_or_else(|| {
            ZosmfErrorResponse::not_found(format!("TSO session '{}' not found", servlet_key))
        })?;

    Ok(StatusCode::OK)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_servlet_key() {
        let key1 = generate_servlet_key();
        let key2 = generate_servlet_key();
        assert!(key1.starts_with("TSO-"));
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_lines_to_tso_data() {
        let lines = vec!["HELLO".to_string(), "WORLD".to_string()];
        let data = lines_to_tso_data(&lines);
        assert_eq!(data.len(), 2);
        assert_eq!(
            data[0].tso_message.as_ref().unwrap().data,
            "HELLO"
        );
    }
}
