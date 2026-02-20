//! /zosmf/restconsoles/* — console command REST API endpoints.
//!
//! Implements the z/OSMF console REST services:
//! - `PUT /zosmf/restconsoles/consoles/:name` — issue MVS console command

use std::sync::Arc;

use axum::extract::{Path, State};
use axum::routing::put;
use axum::{Json, Router};
use open_mainframe_jes2::commands::{execute_command, parse_command, Initiator};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::console::{ConsoleRequest, ConsoleResponse};
use crate::types::error::ZosmfErrorResponse;

/// Register console routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new().route(
        "/zosmf/restconsoles/consoles/{console_name}",
        put(issue_command),
    )
}

/// PUT /zosmf/restconsoles/consoles/:name — issue MVS console command.
async fn issue_command(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(_console_name): Path<String>,
    Json(request): Json<ConsoleRequest>,
) -> std::result::Result<Json<ConsoleResponse>, ZosmfErrorResponse> {
    let mut jes = state
        .jes2
        .write()
        .map_err(|_| ZosmfErrorResponse::internal("JES2 lock poisoned"))?;

    // Parse and execute the command through JES2 command processor.
    let cmd = parse_command(&request.cmd);

    let response_text = if let Some(cmd) = cmd {
        let mut initiators: Vec<Initiator> = Vec::new();
        let resp = execute_command(&mut jes, &mut initiators, &cmd);
        resp.lines.join("\n")
    } else {
        format!("IEE305I COMMAND '{}' NOT RECOGNIZED", request.cmd)
    };

    // Check for solicitation key in response.
    let sol_key_detected = request.sol_key.as_ref().map(|key| response_text.contains(key));

    // Generate a response key for follow-up requests.
    let cmd_response_key = Some(format!("C{}", std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() % 10_000_000));

    let cmd_response_url = cmd_response_key.as_ref().map(|key| {
        format!("/zosmf/restconsoles/consoles/{}/solmsgs/{}", _console_name, key)
    });

    Ok(Json(ConsoleResponse {
        cmd_response_url,
        cmd_response: response_text,
        cmd_response_key,
        sol_key_detected,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_console_response_serialization() {
        let resp = ConsoleResponse {
            cmd_response_url: None,
            cmd_response: "IEE114I ACTIVE SYSTEM".to_string(),
            cmd_response_key: Some("C1234567".to_string()),
            sol_key_detected: None,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"cmd-response\":\"IEE114I ACTIVE SYSTEM\""));
        assert!(json.contains("\"cmd-response-key\":\"C1234567\""));
    }

    #[test]
    fn test_console_response_with_sol_key() {
        let resp = ConsoleResponse {
            cmd_response_url: None,
            cmd_response: "response data".to_string(),
            cmd_response_key: None,
            sol_key_detected: Some(true),
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"sol-key-detected\":true"));
    }
}
