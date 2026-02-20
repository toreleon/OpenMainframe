//! ZOW-109.1: Zowe CLI Dataset Integration Tests
//!
//! End-to-end integration tests that exercise the z/OSMF REST API dataset
//! endpoints using actual HTTP requests against a running Axum router,
//! simulating what `zowe zos-files` commands would do.

use std::sync::Arc;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use tower::ServiceExt;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::state::AppState;

/// Create a test AppState with IBMUSER authenticated and a temp USS/dataset dir.
fn setup_test_state() -> Arc<AppState> {
    let mut config = ZosmfConfig::default();
    let temp_dir = std::env::temp_dir().join(format!(
        "zosmf-test-ds-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    let _ = std::fs::create_dir_all(&temp_dir);
    config.uss.root_directory = temp_dir.to_string_lossy().to_string();

    let mut state = AppState::new(config);

    // Override catalog base directory to isolated temp dir.
    let ds_dir = temp_dir.join("datasets");
    let _ = std::fs::create_dir_all(&ds_dir);
    *state.catalog.get_mut().unwrap() = open_mainframe_dataset::Catalog::new(&ds_dir);

    // Add IBMUSER to RACF with password.
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

// ─── Test: List Datasets (zowe zos-files ls ds) ───

#[tokio::test]
async fn test_list_datasets_empty() {
    let state = setup_test_state();
    let app = build_router(state);

    let response = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.*")
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(json["returnedRows"], 0);
    assert_eq!(json["totalRows"], 0);
    assert!(json["items"].as_array().unwrap().is_empty());
}

// ─── Test: Create Dataset (zowe zos-files create ds) ───

#[tokio::test]
async fn test_create_sequential_dataset() {
    let state = setup_test_state();
    let app = build_router(state);

    let response = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.TEST.SEQ")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(
                    serde_json::to_string(&serde_json::json!({
                        "dsorg": "PS",
                        "recfm": "FB",
                        "lrecl": 80,
                        "blksz": 800
                    }))
                    .unwrap(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::CREATED);
}

// ─── Test: Create + List Datasets roundtrip ───

#[tokio::test]
async fn test_create_then_list_dataset() {
    let state = setup_test_state();

    // Create dataset
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.MYDATA")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"dsorg":"PS","recfm":"FB","lrecl":80}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // List datasets
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.*")
                .header("authorization", basic_auth_header())
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
    assert!(json["returnedRows"].as_u64().unwrap() >= 1);

    let items = json["items"].as_array().unwrap();
    let names: Vec<&str> = items
        .iter()
        .filter_map(|i| i["dsname"].as_str())
        .collect();
    assert!(names.contains(&"IBMUSER.MYDATA"));
}

// ─── Test: Write + Read Dataset Content (zowe zos-files upload/download) ───

#[tokio::test]
async fn test_write_then_read_dataset() {
    let state = setup_test_state();

    // Create
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.UPLOAD.TEST")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"dsorg":"PS","recfm":"FB","lrecl":80}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // Write content
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restfiles/ds/IBMUSER.UPLOAD.TEST")
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from("HELLO FROM ZOWE CLI"))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::NO_CONTENT);

    // Read content back
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds/IBMUSER.UPLOAD.TEST")
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let content = String::from_utf8(body.to_vec()).unwrap();
    assert_eq!(content, "HELLO FROM ZOWE CLI");
}

// ─── Test: Create PDS + Add/List Members ───

#[tokio::test]
async fn test_pds_member_operations() {
    let state = setup_test_state();

    // Create PDS
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.PDS.TEST")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"dsorg":"PO","recfm":"FB","lrecl":80}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // Write member
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("PUT")
                .uri("/zosmf/restfiles/ds/IBMUSER.PDS.TEST(MEMBER1)")
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from("MEMBER CONTENT LINE 1"))
                .unwrap(),
        )
        .await
        .unwrap();
    // New member returns 201 Created per z/OSMF spec.
    assert_eq!(resp.status(), StatusCode::CREATED);

    // List members
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds/IBMUSER.PDS.TEST/member")
                .header("authorization", basic_auth_header())
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
    assert!(json["returnedRows"].as_u64().unwrap() >= 1);

    // Read member
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds/IBMUSER.PDS.TEST(MEMBER1)")
                .header("authorization", basic_auth_header())
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);

    let body = axum::body::to_bytes(resp.into_body(), 1024 * 1024)
        .await
        .unwrap();
    let content = String::from_utf8(body.to_vec()).unwrap();
    assert!(content.contains("MEMBER CONTENT LINE 1"));
}

// ─── Test: Delete Dataset (zowe zos-files delete ds) ───

#[tokio::test]
async fn test_delete_dataset() {
    let state = setup_test_state();

    // Create
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.DELETE.ME")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(r#"{"dsorg":"PS","recfm":"FB","lrecl":80}"#))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // Delete
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("DELETE")
                .uri("/zosmf/restfiles/ds/IBMUSER.DELETE.ME")
                .header("authorization", basic_auth_header())
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::NO_CONTENT);

    // Verify deleted — listing should not contain it
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.DELETE.*")
                .header("authorization", basic_auth_header())
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
    assert_eq!(json["returnedRows"], 0);
}

// ─── Test: Authentication required for dataset operations ───

#[tokio::test]
async fn test_dataset_requires_auth() {
    let state = setup_test_state();
    let app = build_router(state);

    let response = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.*")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(response.status(), StatusCode::UNAUTHORIZED);
}

// ─── Test: X-IBM-Max-Items header ───

#[tokio::test]
async fn test_list_datasets_max_items() {
    let state = setup_test_state();

    // Create 3 datasets
    for i in 1..=3 {
        let app = build_router(state.clone());
        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri(format!("/zosmf/restfiles/ds/IBMUSER.MULTI.DS{}", i))
                    .header("authorization", basic_auth_header())
                    .header("content-type", "application/json")
                    .header("X-CSRF-ZOSMF-HEADER", "true")
                    .body(Body::from(r#"{"dsorg":"PS","recfm":"FB","lrecl":80}"#))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::CREATED);
    }

    // List with max-items=2
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.MULTI.*")
                .header("authorization", basic_auth_header())
                .header("x-ibm-max-items", "2")
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
    assert_eq!(json["returnedRows"], 2);
    assert_eq!(json["totalRows"], 3);
}

// ─── Test: z/OSMF JSON format compliance ───

#[tokio::test]
async fn test_dataset_json_format_compliance() {
    let state = setup_test_state();

    // Create dataset
    let app = build_router(state.clone());
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/zosmf/restfiles/ds/IBMUSER.FORMAT.CHK")
                .header("authorization", basic_auth_header())
                .header("content-type", "application/json")
                .header("X-CSRF-ZOSMF-HEADER", "true")
                .body(Body::from(
                    r#"{"dsorg":"PS","recfm":"FB","lrecl":80,"blksz":800}"#,
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::CREATED);

    // List with extended attributes
    let app = build_router(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/zosmf/restfiles/ds?dslevel=IBMUSER.FORMAT.*")
                .header("authorization", basic_auth_header())
                .header("x-ibm-attributes", "vol")
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

    // Verify z/OSMF JSON fields present
    assert!(json.get("JSONversion").is_some());
    assert!(json.get("returnedRows").is_some());
    assert!(json.get("totalRows").is_some());

    let item = &json["items"][0];
    assert!(item.get("dsname").is_some());
    assert!(item.get("dsorg").is_some());
}
