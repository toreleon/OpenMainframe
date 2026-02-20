//! /zosmf/restfiles/fs/* — USS file operations REST API endpoints.
//!
//! Implements the z/OSMF USS file REST services:
//! - `GET    /zosmf/restfiles/fs/:path` — list directory or read file
//! - `PUT    /zosmf/restfiles/fs/:path` — write file content
//! - `POST   /zosmf/restfiles/fs/:path` — create directory
//! - `DELETE /zosmf/restfiles/fs/:path` — delete file or directory

use std::path::PathBuf;
use std::sync::Arc;

use axum::body::Body;
use axum::extract::{Path, State};
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::{delete, get, post, put};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};

use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Register USS file routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/restfiles/fs/{*path}", get(read_or_list))
        .route("/zosmf/restfiles/fs/{*path}", put(write_file))
        .route("/zosmf/restfiles/fs/{*path}", post(create_dir))
        .route("/zosmf/restfiles/fs/{*path}", delete(delete_path))
}

/// USS directory entry in list responses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UssEntry {
    /// File or directory name.
    pub name: String,
    /// Type: "file" or "directory".
    pub mode: String,
    /// Size in bytes.
    pub size: u64,
}

/// USS directory listing response.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UssListResponse {
    /// Directory entries.
    pub items: Vec<UssEntry>,
    /// Number of items returned.
    #[serde(rename = "returnedRows")]
    pub returned_rows: usize,
    /// Total items.
    #[serde(rename = "totalRows")]
    pub total_rows: usize,
}

/// Resolve the USS path to the configured root.
fn resolve_uss_path(state: &AppState, uss_path: &str) -> PathBuf {
    let root = PathBuf::from(&state.config.uss.root_directory);
    let cleaned = uss_path.trim_start_matches('/');
    root.join(cleaned)
}

/// GET /zosmf/restfiles/fs/:path — list directory contents or read file.
async fn read_or_list(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<axum::response::Response, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(&state, &uss_path);

    if !full_path.exists() {
        return Err(ZosmfErrorResponse::not_found(format!(
            "Path '{}' not found",
            uss_path
        )));
    }

    if full_path.is_dir() {
        // List directory contents.
        let entries = std::fs::read_dir(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to read directory: {}", e))
        })?;

        let mut items = Vec::new();
        for entry in entries {
            let entry = entry.map_err(|e| {
                ZosmfErrorResponse::internal(format!("Directory entry error: {}", e))
            })?;

            let metadata = entry.metadata().map_err(|e| {
                ZosmfErrorResponse::internal(format!("Metadata error: {}", e))
            })?;

            items.push(UssEntry {
                name: entry.file_name().to_string_lossy().to_string(),
                mode: if metadata.is_dir() {
                    "directory".to_string()
                } else {
                    "file".to_string()
                },
                size: metadata.len(),
            });
        }

        items.sort_by(|a, b| a.name.cmp(&b.name));
        let total = items.len();

        let resp = UssListResponse {
            items,
            returned_rows: total,
            total_rows: total,
        };

        Ok(Json(resp).into_response())
    } else {
        // Read file content.
        let content = std::fs::read(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to read file: {}", e))
        })?;

        let text = String::from_utf8_lossy(&content).to_string();
        Ok((
            StatusCode::OK,
            [("content-type", "text/plain; charset=UTF-8")],
            text,
        )
            .into_response())
    }
}

/// PUT /zosmf/restfiles/fs/:path — write file content.
async fn write_file(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(uss_path): Path<String>,
    body: Body,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(&state, &uss_path);

    let bytes = axum::body::to_bytes(body, 10 * 1024 * 1024)
        .await
        .map_err(|_| ZosmfErrorResponse::bad_request("Failed to read request body"))?;

    // Ensure parent directory exists.
    if let Some(parent) = full_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to create parent directory: {}", e))
        })?;
    }

    std::fs::write(&full_path, bytes.as_ref()).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to write file: {}", e))
    })?;

    Ok(StatusCode::NO_CONTENT)
}

/// POST /zosmf/restfiles/fs/:path — create directory.
async fn create_dir(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(&state, &uss_path);

    std::fs::create_dir_all(&full_path).map_err(|e| {
        ZosmfErrorResponse::internal(format!("Failed to create directory: {}", e))
    })?;

    Ok(StatusCode::CREATED)
}

/// DELETE /zosmf/restfiles/fs/:path — delete file or directory.
async fn delete_path(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    Path(uss_path): Path<String>,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    let full_path = resolve_uss_path(&state, &uss_path);

    if !full_path.exists() {
        return Err(ZosmfErrorResponse::not_found(format!(
            "Path '{}' not found",
            uss_path
        )));
    }

    if full_path.is_dir() {
        std::fs::remove_dir_all(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete directory: {}", e))
        })?;
    } else {
        std::fs::remove_file(&full_path).map_err(|e| {
            ZosmfErrorResponse::internal(format!("Failed to delete file: {}", e))
        })?;
    }

    Ok(StatusCode::NO_CONTENT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uss_entry_serialization() {
        let entry = UssEntry {
            name: "hello.txt".to_string(),
            mode: "file".to_string(),
            size: 1024,
        };

        let json = serde_json::to_string(&entry).unwrap();
        assert!(json.contains("\"name\":\"hello.txt\""));
        assert!(json.contains("\"mode\":\"file\""));
        assert!(json.contains("\"size\":1024"));
    }

    #[test]
    fn test_uss_list_response_serialization() {
        let resp = UssListResponse {
            items: vec![
                UssEntry {
                    name: "dir1".to_string(),
                    mode: "directory".to_string(),
                    size: 4096,
                },
                UssEntry {
                    name: "file1.txt".to_string(),
                    mode: "file".to_string(),
                    size: 100,
                },
            ],
            returned_rows: 2,
            total_rows: 2,
        };

        let json = serde_json::to_string(&resp).unwrap();
        assert!(json.contains("\"returnedRows\":2"));
        assert!(json.contains("\"totalRows\":2"));
    }
}
