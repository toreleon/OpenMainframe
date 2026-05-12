//! Task-based evaluation harness for mainframe development agents.
//!
//! `open-mainframe-gym` wraps the in-process z/OSMF router as a deterministic
//! Gym-style environment. Tasks define HTTP actions and checks; each action
//! returns an observation that an agent can use to decide the next step.

#![forbid(unsafe_code)]

use std::path::PathBuf;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use axum::body::{to_bytes, Body};
use axum::http::{header, Method, Request, StatusCode};
use base64::Engine;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower::ServiceExt;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::state::AppState;

/// Default RACF user provisioned in each environment.
pub const DEFAULT_USER: &str = "IBMUSER";

/// Default password for the provisioned RACF user.
pub const DEFAULT_PASSWORD: &str = "SYS1";

/// Configuration for an isolated Gym environment instance.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GymConfig {
    /// User ID used by generated Basic Auth headers.
    pub user: String,
    /// Password used by generated Basic Auth headers.
    pub password: String,
    /// Host directory backing USS file APIs.
    pub uss_root: PathBuf,
    /// Host directory backing the dataset catalog.
    pub dataset_root: PathBuf,
    /// z/OSMF server configuration used by the in-process router.
    pub zosmf: ZosmfConfig,
}

impl GymConfig {
    /// Create an isolated config rooted under the system temp directory.
    pub fn isolated() -> Self {
        let base = std::env::temp_dir().join(format!(
            "openmainframe-gym-{}-{}",
            std::process::id(),
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos()
        ));
        let uss_root = base.join("uss");
        let dataset_root = base.join("datasets");

        let mut zosmf = ZosmfConfig::default();
        zosmf.uss.root_directory = uss_root.to_string_lossy().to_string();

        Self {
            user: DEFAULT_USER.to_string(),
            password: DEFAULT_PASSWORD.to_string(),
            uss_root,
            dataset_root,
            zosmf,
        }
    }
}

impl Default for GymConfig {
    fn default() -> Self {
        Self::isolated()
    }
}

/// A single agent/environment action.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MainframeAction {
    /// HTTP method, for example `GET`, `POST`, or `PUT`.
    pub method: String,
    /// z/OSMF URI path, including query string if needed.
    pub uri: String,
    /// Optional request body.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
    /// Optional content type.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,
    /// Whether to include the z/OSMF CSRF header expected by mutating APIs.
    #[serde(default)]
    pub csrf: bool,
}

impl MainframeAction {
    /// Construct a request action.
    pub fn new(method: impl Into<String>, uri: impl Into<String>) -> Self {
        Self {
            method: method.into(),
            uri: uri.into(),
            body: None,
            content_type: None,
            csrf: false,
        }
    }

    /// Attach a request body and content type.
    pub fn with_body(mut self, body: impl Into<String>, content_type: impl Into<String>) -> Self {
        self.body = Some(body.into());
        self.content_type = Some(content_type.into());
        self
    }

    /// Include the z/OSMF CSRF marker header.
    pub fn with_csrf(mut self) -> Self {
        self.csrf = true;
        self
    }
}

/// A response returned by the environment after one action.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Observation {
    /// HTTP status code.
    pub status: u16,
    /// Raw response body as UTF-8 text.
    pub body: String,
    /// Parsed JSON response body, when the response is JSON.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub json: Option<Value>,
}

/// A declarative assertion over an observation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Check {
    /// Assert an exact HTTP status code.
    Status { expected: u16 },
    /// Assert a JSON Pointer value equals the expected JSON value.
    JsonPointerEquals { pointer: String, expected: Value },
    /// Assert the raw body contains a substring.
    BodyContains { text: String },
}

impl Check {
    fn evaluate(&self, observation: &Observation) -> Result<(), String> {
        match self {
            Self::Status { expected } => {
                if observation.status == *expected {
                    Ok(())
                } else {
                    Err(format!(
                        "expected HTTP status {expected}, got {}",
                        observation.status
                    ))
                }
            }
            Self::JsonPointerEquals { pointer, expected } => {
                let json = observation
                    .json
                    .as_ref()
                    .ok_or_else(|| "response body is not JSON".to_string())?;
                let actual = json
                    .pointer(pointer)
                    .ok_or_else(|| format!("JSON pointer not found: {pointer}"))?;
                if actual == expected {
                    Ok(())
                } else {
                    Err(format!(
                        "expected JSON pointer {pointer} to equal {expected}, got {actual}"
                    ))
                }
            }
            Self::BodyContains { text } => {
                if observation.body.contains(text) {
                    Ok(())
                } else {
                    Err(format!("response body did not contain {text:?}"))
                }
            }
        }
    }
}

/// A complete benchmark task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GymTask {
    /// Stable task ID.
    pub id: String,
    /// Human-readable title.
    pub title: String,
    /// Optional setup actions applied before the scored steps.
    #[serde(default)]
    pub setup: Vec<MainframeAction>,
    /// Agent actions to execute.
    pub steps: Vec<MainframeAction>,
    /// Checks evaluated against the final observation.
    pub checks: Vec<Check>,
}

/// Result of running a task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskReport {
    /// Task ID.
    pub task_id: String,
    /// Observations from setup and scored steps in execution order.
    pub observations: Vec<Observation>,
    /// Check failures. Empty means the task passed.
    pub failures: Vec<String>,
}

impl TaskReport {
    /// Whether all checks passed.
    pub fn passed(&self) -> bool {
        self.failures.is_empty()
    }
}

/// Isolated in-process OpenMainframe environment.
#[derive(Clone)]
pub struct MainframeGymEnv {
    state: Arc<AppState>,
    router: axum::Router,
    auth_header: String,
}

impl MainframeGymEnv {
    /// Create a new environment with isolated filesystem-backed state.
    pub fn new(config: GymConfig) -> Result<Self, String> {
        std::fs::create_dir_all(&config.uss_root)
            .map_err(|e| format!("failed to create USS root: {e}"))?;
        std::fs::create_dir_all(&config.dataset_root)
            .map_err(|e| format!("failed to create dataset root: {e}"))?;

        let mut state = AppState::new(config.zosmf);
        *state
            .catalog
            .get_mut()
            .map_err(|_| "catalog lock poisoned")? =
            open_mainframe_dataset::Catalog::new(&config.dataset_root);

        state
            .racf
            .add_user(&config.user, "SYS1", "Gym User", "SYS1")
            .map_err(|e| format!("failed to create RACF user: {e}"))?;
        state
            .racf
            .get_user_mut(&config.user)
            .ok_or_else(|| "created RACF user was not found".to_string())?
            .password_hash = Some(config.password.clone());

        let credentials = format!("{}:{}", config.user, config.password);
        let encoded = base64::engine::general_purpose::STANDARD.encode(credentials);
        let auth_header = format!("Basic {encoded}");

        let state = Arc::new(state);
        let router = open_mainframe_zosmf::handlers::build_router(Arc::clone(&state));

        Ok(Self {
            state,
            router,
            auth_header,
        })
    }

    /// Access the underlying z/OSMF state for advanced tests and fixtures.
    pub fn state(&self) -> &Arc<AppState> {
        &self.state
    }

    /// Reset by creating a fresh environment with the supplied config.
    pub fn reset(&mut self, config: GymConfig) -> Result<(), String> {
        *self = Self::new(config)?;
        Ok(())
    }

    /// Execute one action and return the resulting observation.
    pub async fn step(&self, action: MainframeAction) -> Result<Observation, String> {
        let method: Method = action
            .method
            .parse()
            .map_err(|e| format!("invalid HTTP method {}: {e}", action.method))?;

        let mut request = Request::builder()
            .method(method)
            .uri(action.uri)
            .header(header::AUTHORIZATION, self.auth_header.as_str());

        if let Some(content_type) = action.content_type {
            request = request.header(header::CONTENT_TYPE, content_type);
        }
        if action.csrf {
            request = request.header("X-CSRF-ZOSMF-HEADER", "true");
        }

        let response = self
            .router
            .clone()
            .oneshot(
                request
                    .body(Body::from(action.body.unwrap_or_default()))
                    .map_err(|e| format!("failed to build request: {e}"))?,
            )
            .await
            .map_err(|e| format!("router request failed: {e}"))?;

        let status = response.status();
        let bytes = to_bytes(response.into_body(), 1024 * 1024)
            .await
            .map_err(|e| format!("failed to read response body: {e}"))?;
        let body = String::from_utf8_lossy(&bytes).to_string();
        let json = serde_json::from_slice(&bytes).ok();

        Ok(Observation {
            status: status.as_u16(),
            body,
            json,
        })
    }

    /// Run a full task and evaluate its checks against the final observation.
    pub async fn run_task(&self, task: GymTask) -> Result<TaskReport, String> {
        let mut observations = Vec::new();
        for action in task.setup.iter().chain(task.steps.iter()) {
            observations.push(self.step(action.clone()).await?);
        }

        let mut failures = Vec::new();
        let final_observation = observations
            .last()
            .ok_or_else(|| format!("task {} has no actions", task.id))?;
        for check in &task.checks {
            if let Err(err) = check.evaluate(final_observation) {
                failures.push(err);
            }
        }

        Ok(TaskReport {
            task_id: task.id,
            observations,
            failures,
        })
    }
}

/// Convenience constructor for a common dataset creation task.
pub fn dataset_create_task(dataset: impl Into<String>) -> GymTask {
    let dataset = dataset.into();
    GymTask {
        id: format!(
            "dataset-create-{}",
            dataset.replace('.', "-").to_lowercase()
        ),
        title: format!("Create sequential dataset {dataset}"),
        setup: Vec::new(),
        steps: vec![
            MainframeAction::new("POST", format!("/zosmf/restfiles/ds/{dataset}"))
                .with_body(
                    serde_json::json!({
                        "dsorg": "PS",
                        "recfm": "FB",
                        "lrecl": 80,
                        "blksz": 800
                    })
                    .to_string(),
                    "application/json",
                )
                .with_csrf(),
        ],
        checks: vec![Check::Status {
            expected: StatusCode::CREATED.as_u16(),
        }],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn creates_dataset_from_task() {
        let env = MainframeGymEnv::new(GymConfig::isolated()).unwrap();
        let report = env
            .run_task(dataset_create_task("IBMUSER.GYM.SEQ"))
            .await
            .unwrap();

        assert!(report.passed(), "{:?}", report.failures);
        assert_eq!(report.observations.len(), 1);
    }

    #[tokio::test]
    async fn submits_jcl_and_checks_jobname() {
        let env = MainframeGymEnv::new(GymConfig::isolated()).unwrap();
        let jcl =
            "//GYMJOB   JOB (ACCT),'GYM JOB',CLASS=A,MSGCLASS=X\n//STEP1    EXEC PGM=IEFBR14\n";
        let task = GymTask {
            id: "submit-jcl".to_string(),
            title: "Submit a simple JCL job".to_string(),
            setup: Vec::new(),
            steps: vec![MainframeAction::new("PUT", "/zosmf/restjobs/jobs")
                .with_body(jcl, "text/plain")
                .with_csrf()],
            checks: vec![
                Check::Status { expected: 201 },
                Check::JsonPointerEquals {
                    pointer: "/jobname".to_string(),
                    expected: Value::String("GYMJOB".to_string()),
                },
            ],
        };

        let report = env.run_task(task).await.unwrap();
        assert!(report.passed(), "{:?}", report.failures);
    }
}
