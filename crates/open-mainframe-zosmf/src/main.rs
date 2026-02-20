//! z/OSMF server binary — starts the z/OSMF-compatible REST API server.

use std::sync::Arc;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::handlers::build_router;
use open_mainframe_zosmf::state::AppState;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let port: u16 = std::env::var("ZOSMF_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(10443);

    let mut config = ZosmfConfig::default();
    config.server.port = port;
    config.server.host = "127.0.0.1".to_string();

    // Set up USS root in a temp directory.
    let uss_dir = std::env::temp_dir().join("openmainframe-uss");
    let _ = std::fs::create_dir_all(&uss_dir);
    config.uss.root_directory = uss_dir.to_string_lossy().to_string();

    let mut state = AppState::new(config);

    // Set up dataset catalog in a persistent directory.
    let ds_dir = std::env::temp_dir().join("openmainframe-datasets");
    let _ = std::fs::create_dir_all(&ds_dir);
    *state.catalog.get_mut().unwrap() = open_mainframe_dataset::Catalog::new(&ds_dir);

    // Create default IBMUSER with password SYS1.
    state
        .racf
        .add_user("IBMUSER", "SYS1", "IBM Default User", "SYS1")
        .expect("Failed to create IBMUSER");
    state
        .racf
        .get_user_mut("IBMUSER")
        .expect("IBMUSER not found")
        .password_hash = Some("SYS1".to_string());

    let state = Arc::new(state);
    let router = build_router(state);

    let bind_addr = format!("127.0.0.1:{}", port);
    eprintln!("z/OSMF server starting on http://{}", bind_addr);
    eprintln!("  Default user: IBMUSER / SYS1");
    eprintln!("  Datasets dir: {}", ds_dir.display());
    eprintln!("  USS root:     {}", uss_dir.display());

    let listener = tokio::net::TcpListener::bind(&bind_addr)
        .await
        .expect("Failed to bind");

    axum::serve(listener, router)
        .await
        .expect("Server error");
}
