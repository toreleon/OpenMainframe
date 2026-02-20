//! ZOW-109.4: Performance Benchmark Suite
//!
//! Performance tests that validate response time and concurrency requirements:
//! - Dataset list benchmark: p95 response time < 200ms for 100-item result set
//! - Job submit benchmark: p95 response time < 500ms for simple JCL
//! - Concurrency test: 50 simultaneous sessions complete successfully

use std::sync::Arc;
use std::time::Instant;

use axum::body::Body;
use axum::http::{Request, StatusCode};
use tower::ServiceExt;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::state::AppState;

fn setup_test_state() -> Arc<AppState> {
    let mut config = ZosmfConfig::default();
    let temp_dir = std::env::temp_dir().join(format!(
        "zosmf-bench-{}-{}",
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

/// Calculate p95 from a sorted list of durations.
fn p95(durations: &mut [u128]) -> u128 {
    durations.sort();
    let idx = (durations.len() as f64 * 0.95) as usize;
    let idx = idx.min(durations.len() - 1);
    durations[idx]
}

// ─── Benchmark: Dataset list response time ───

#[tokio::test]
async fn bench_dataset_list_response_time() {
    let state = setup_test_state();

    // Create 100 datasets for realistic benchmark
    for i in 1..=100 {
        let app = build_router(state.clone());
        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri(format!("/zosmf/restfiles/ds/BENCH.DS{:04}", i))
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

    // Benchmark list operation — run 20 iterations
    let mut durations = Vec::new();
    for _ in 0..20 {
        let app = build_router(state.clone());
        let start = Instant::now();

        let resp = app
            .oneshot(
                Request::builder()
                    .uri("/zosmf/restfiles/ds?dslevel=BENCH.*")
                    .header("authorization", basic_auth_header())
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        let elapsed = start.elapsed().as_millis();
        assert_eq!(resp.status(), StatusCode::OK);

        // Consume body to include serialization time
        let body = axum::body::to_bytes(resp.into_body(), 10 * 1024 * 1024)
            .await
            .unwrap();
        let json: serde_json::Value = serde_json::from_slice(&body).unwrap();
        assert_eq!(json["returnedRows"], 100);

        durations.push(elapsed);
    }

    let p95_ms = p95(&mut durations);
    eprintln!(
        "Dataset list p95 response time: {}ms (target: <200ms, {} items)",
        p95_ms,
        100
    );
    assert!(
        p95_ms < 200,
        "Dataset list p95 ({}ms) exceeds 200ms target",
        p95_ms
    );
}

// ─── Benchmark: Job submit response time ───

#[tokio::test]
async fn bench_job_submit_response_time() {
    let state = setup_test_state();

    let mut durations = Vec::new();
    for i in 0..20 {
        let app = build_router(state.clone());
        let jcl = format!(
            "//BENCH{:03} JOB (ACCT),'BENCH',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n",
            i
        );

        let start = Instant::now();

        let resp = app
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/zosmf/restjobs/jobs")
                    .header("authorization", basic_auth_header())
                    .header("content-type", "text/plain")
                    .header("X-CSRF-ZOSMF-HEADER", "true")
                    .body(Body::from(jcl))
                    .unwrap(),
            )
            .await
            .unwrap();

        let elapsed = start.elapsed().as_millis();
        assert_eq!(resp.status(), StatusCode::CREATED);

        // Consume body
        let _ = axum::body::to_bytes(resp.into_body(), 1024 * 1024).await;

        durations.push(elapsed);
    }

    let p95_ms = p95(&mut durations);
    eprintln!("Job submit p95 response time: {}ms (target: <500ms)", p95_ms);
    assert!(
        p95_ms < 500,
        "Job submit p95 ({}ms) exceeds 500ms target",
        p95_ms
    );
}

// ─── Benchmark: Concurrent sessions ───

#[tokio::test]
async fn bench_concurrent_sessions() {
    let state = setup_test_state();

    // Launch 50 concurrent requests
    let mut handles = Vec::new();
    for i in 0..50 {
        let state = state.clone();
        let handle = tokio::spawn(async move {
            let app = build_router(state);
            let jcl = format!(
                "//CONC{:04} JOB (ACCT),'CONCURRENT',CLASS=A\n//STEP1 EXEC PGM=IEFBR14\n",
                i
            );

            let resp = app
                .oneshot(
                    Request::builder()
                        .method("PUT")
                        .uri("/zosmf/restjobs/jobs")
                        .header("authorization", basic_auth_header())
                        .header("content-type", "text/plain")
                        .header("X-CSRF-ZOSMF-HEADER", "true")
                        .body(Body::from(jcl))
                        .unwrap(),
                )
                .await
                .unwrap();

            resp.status()
        });
        handles.push(handle);
    }

    // Wait for all to complete
    let mut success_count = 0;
    for handle in handles {
        let status = handle.await.unwrap();
        if status == StatusCode::CREATED {
            success_count += 1;
        }
    }

    eprintln!(
        "Concurrent sessions: {}/50 succeeded (target: 50/50)",
        success_count
    );
    assert_eq!(
        success_count, 50,
        "Only {}/50 concurrent sessions succeeded",
        success_count
    );
}

// ─── Benchmark: Info endpoint latency ───

#[tokio::test]
async fn bench_info_endpoint_latency() {
    let state = setup_test_state();

    let mut durations = Vec::new();
    for _ in 0..50 {
        let app = build_router(state.clone());
        let start = Instant::now();

        let resp = app
            .oneshot(
                Request::builder()
                    .uri("/zosmf/info")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        let elapsed = start.elapsed().as_millis();
        assert_eq!(resp.status(), StatusCode::OK);

        let _ = axum::body::to_bytes(resp.into_body(), 1024 * 1024).await;
        durations.push(elapsed);
    }

    let p95_ms = p95(&mut durations);
    eprintln!("Info endpoint p95 response time: {}ms (target: <50ms)", p95_ms);
    assert!(
        p95_ms < 50,
        "Info endpoint p95 ({}ms) exceeds 50ms target",
        p95_ms
    );
}
