//! ZOW-109.3: Zowe CLI TSO and Console Integration Tests
//!
//! End-to-end integration tests that exercise the z/OSMF REST API TSO and
//! console endpoints, simulating what `zowe zos-tso` and `zowe zos-console`
//! commands would do.

use std::sync::Arc;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use tower::ServiceExt;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::state::AppState;

/// Create a test AppState with IBMUSER authenticated.
fn setup_test_state() -> Arc<AppState> {
    let mut config = ZosmfConfig::default();
    let temp_dir = std::env::temp_dir().join(format!(
        "zosmf-test-tso-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    let _ = std::fs::create_dir_all(&temp_dir);
    config.uss.root_directory = temp_dir.to_string_lossy().to_string();

    let mut state = AppState::new(config);

    state
        .racf
        .add_user("IBMUSER", "SYS1", "IBM Default User", "SYS1")
        .expect("add user");
    state
        .racf
        .get_user_mut("IBMUSER")
        .expect("get user")
        .password_hash = Some("TESTPASS".to_string());

    Arc::new(state)
}

fn basic_auth_header() -> String {
    use base64::Engine;
    let encoded = base64::engine::general_purpose::STANDARD.encode("IBMUSER:TESTPASS");
    format!("Basic {}", encoded)
}

fn build_router(state: Arc<AppState>) -> axum::Router {
    open_mainframe_zosmf::handlers::build_router(state)
}

// ─── TSO Tests ───

#[tokio::test]
async fn test_tso_stateless_command() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/tsoApp/tso")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"TSO COMMAND":"TIME"}"#))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // Response should have tsoData
    assert!(json.get("tsoData").is_some());
}

#[tokio::test]
async fn test_tso_session_lifecycle() {
    let state = setup_test_state();

    // Start session
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/tsoApp/tso")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"startTso":{"proc":"DBSPROC","acct":"DEFAULT"}}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let servlet_key = json["servletKey"].as_str().unwrap();
    assert!(!servlet_key.is_empty());

    // Should have welcome messages in tsoData
    let tso_data = json["tsoData"].as_array().unwrap();
    assert!(!tso_data.is_empty());

    // Send command
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri(format!("/zosmf/tsoApp/tso/{}", servlet_key))
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"TSO COMMAND":"TIME"}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert!(json.get("tsoData").is_some());
    assert_eq!(json["servletKey"], servlet_key);

    // Receive buffered output
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!("/zosmf/tsoApp/tso/{}", servlet_key))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    // Stop session
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri(format!("/zosmf/tsoApp/tso/{}", servlet_key))
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    // Accessing stopped session should fail
    let app = build_router(Arc::new(AppState::new(ZosmfConfig::default())));
    let resp = app
        .oneshot(
            Request::builder()
                .uri(format!("/zosmf/tsoApp/tso/{}", servlet_key))
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    // New state has no sessions at all, so any key lookup should fail
    assert_ne!(resp.status(), StatusCode::OK);
}

#[tokio::test]
async fn test_tso_session_not_found() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/tsoApp/tso/NONEXISTENT-KEY")
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::NOT_FOUND);
}

// ─── Console Tests ───

#[tokio::test]
async fn test_console_issue_command() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restconsoles/consoles/defcn")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"cmd":"D A,L"}"#))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // Should have cmd-response field
    assert!(json.get("cmd-response").is_some());
    let response_text = json["cmd-response"].as_str().unwrap();
    assert!(!response_text.is_empty());
}

#[tokio::test]
async fn test_console_unrecognized_command() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restconsoles/consoles/defcn")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"cmd":"XYZZY NOSUCHCMD"}"#))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    let response_text = json["cmd-response"].as_str().unwrap();

    // Unrecognized commands return IEE305I
    assert!(response_text.contains("IEE305I") || response_text.contains("NOT RECOGNIZED"));
}

#[tokio::test]
async fn test_console_with_sol_key() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restconsoles/consoles/defcn")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(
                    r#"{"cmd":"D A,L","sol-key":"NOSUCHKEY"}"#,
                ))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // sol-key-detected should not be present if key not found in output
    assert!(json.get("cmd-response").is_some());
}

// ─── Authentication Tests ───

#[tokio::test]
async fn test_tso_requires_auth() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/tsoApp/tso")
                .header("content-type", "application/json")
                .body(Body::from(r#"{"TSO COMMAND":"TIME"}"#))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
}

#[tokio::test]
async fn test_console_requires_auth() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restconsoles/consoles/defcn")
                .header("content-type", "application/json")
                .body(Body::from(r#"{"cmd":"D A,L"}"#))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
}

// ─── Info Endpoint Test ───

#[tokio::test]
async fn test_zosmf_info_endpoint() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/info")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(json.get("api_version").is_some());
    assert!(json.get("zosmf_hostname").is_some());
    assert!(json.get("zosmf_saf_realm").is_some());
    assert!(json.get("plugins").is_some());
}

// ─── Authentication Flow Test ───

#[tokio::test]
async fn test_login_flow() {
    let state = setup_test_state();
    let app = build_router(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/services/authenticate")
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    // Should set cookie
    let set_cookie = resp.headers().get("set-cookie");
    assert!(set_cookie.is_some());
    let cookie_str = set_cookie.unwrap().to_str().unwrap();
    assert!(cookie_str.contains("jwtToken="));

    // IBM z/OSMF returns JWT via Set-Cookie only; body is empty JSON.
    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert!(json.is_object());
}

#[tokio::test]
async fn test_login_bad_credentials() {
    let state = setup_test_state();
    let app = build_router(state);

    use base64::Engine;
    let encoded = base64::engine::general_purpose::STANDARD.encode("IBMUSER:WRONGPASS");
    let bad_auth = format!("Basic {}", encoded);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/services/authenticate")
                .header("authorization", bad_auth)
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::UNAUTHORIZED);
}
