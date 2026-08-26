# open-mainframe-gym

Task-based agent evaluation and benchmarking harness for mainframe workloads — in-process execution of the z/OSMF REST API for SWE-bench and reinforcement learning workflows in the OpenMainframe project.

## Purpose

`open-mainframe-gym` provides a reproducible, sandboxed Gym-style test environment for evaluating AI coding assistants, autonomous agents, and automated test pipelines against mainframe operations. It runs the full `open-mainframe-zosmf` REST routing stack in-process via Tower `oneshot` requests without binding to network ports, enabling fast, isolated, deterministic benchmark execution.

## Capabilities

- **In-Process Environment** (`MainframeGymEnv`):
  - Wraps the Axum REST API router directly via Tower `ServiceExt::oneshot`.
  - Completely self-contained execution without socket allocation or port collisions.
- **Sandboxed Configuration** (`GymConfig`):
  - `GymConfig::isolated()` generates isolated temporary directory catalogs, mock RACF user profiles, and scratch dataset allocations per environment instance.
- **Action & Observation Model**:
  - `MainframeAction`: Structured REST actions (`HttpGet`, `HttpPost`, `HttpPut`, `HttpDelete`, `CreateDataset`, `SubmitJob`, `ExecuteTso`).
  - `Observation`: Structured response payload capturing HTTP status code, parsed JSON response body, response headers, and execution latency.
- **Declarative Task Verification** (`Check`, `GymTask`):
  - `Check` validators: `Status(u16)`, `JsonPointerEquals(pointer, value)`, `BodyContains(substring)`.
  - Multi-step `GymTask` workflows producing detailed `TaskReport` and `StepResult` records.
  - Built-in task constructors such as `dataset_create_task(dsn)`.

## Architecture

```
    ┌────────────────────────────────────────────────────────┐
    │                AI Agent / Test Harness                 │
    │  - Submits GymTask or step-by-step MainframeAction     │
    └────────────────────────┬───────────────────────────────┘
                             │
                             ▼
    ┌────────────────────────────────────────────────────────┐
    │                    MainframeGymEnv                     │
    │  - Manages sandboxed GymConfig & temp dataset paths    │
    │  - Dispatches actions via Tower ServiceExt::oneshot    │
    │  - Evaluates Observation against Check assertions      │
    └────────────────────────┬───────────────────────────────┘
                             │ (In-Process HTTP Requests)
                             ▼
    ┌────────────────────────────────────────────────────────┐
    │                  open-mainframe-zosmf                  │
    │  - Axum Router (Datasets, Jobs, TSO, CICS, Console)    │
    │  - AppState & Subsystem Managers                       │
    └────────────────────────────────────────────────────────┘
```

## Public API

### Primary Types and Functions

- `MainframeGymEnv`: Core Gym environment instance (`new()`, `step()`, `run_task()`).
- `GymConfig`: Environment configuration builder (`isolated()`, `with_temp_dir()`).
- `GymTask`: Multi-step benchmark task definition (`new()`, `add_step()`, `dataset_create_task()`).
- `MainframeAction`: Action variants representing z/OSMF operations (`HttpGet`, `HttpPost`, `HttpPut`, `HttpDelete`).
- `Observation`: Result of an action step (`status`, `body`, `headers`, `duration`).
- `Check`: Declarative validation checks (`Status`, `JsonPointerEquals`, `BodyContains`).
- `TaskReport`: Comprehensive task outcome (`passed()`, `total_steps`, `passed_steps`, `step_results`).
- `StepResult`: Individual step outcome with observation and failure diagnostic messages.
- `dataset_create_task(dsn: &str) -> GymTask`: Pre-built benchmark task for sequential dataset creation.

## Integration

- **Internal workspace dependencies**:
  - `open-mainframe-dataset`: Dataset models and catalog management.
  - `open-mainframe-zosmf`: z/OSMF router assembly (`build_router`) and `ZosmfConfig`.
- **Consumers**: External evaluation harnesses, automated test suites, and SWE-bench agent benchmarks.

## Examples

### Running a Declarative Benchmark Task

```rust
use open_mainframe_gym::{dataset_create_task, GymConfig, MainframeGymEnv};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env = MainframeGymEnv::new(GymConfig::isolated())?;

    // Run pre-built dataset creation benchmark task
    let report = env
        .run_task(dataset_create_task("IBMUSER.TEST.SEQ"))
        .await?;

    assert!(report.passed());
    println!(
        "Task completed: {}/{} steps passed",
        report.passed_steps, report.total_steps
    );
    Ok(())
}
```

### Executing Step-by-Step Actions

```rust
use open_mainframe_gym::{
    GymConfig, MainframeAction, MainframeGymEnv,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let env = MainframeGymEnv::new(GymConfig::isolated())?;

    // Query z/OSMF server info endpoint
    let action = MainframeAction::HttpGet {
        path: "/zosmf/info".to_string(),
        headers: vec![("X-CSRF-ZOSMF-HEADER".to_string(), "".to_string())],
    };

    let obs = env.step(action).await?;
    assert_eq!(obs.status, 200);
    assert!(obs.body.contains("zosmf_saf_mode"));
    Ok(())
}
```

## Testing

Run unit and integration tests:

```bash
cargo test -p open-mainframe-gym
```

The test suite contains 2 integration tests in `tests/integration_tests.rs`:
- End-to-end task execution verifying dataset creation via the in-process z/OSMF router.
- Step-by-step action dispatch and observation verification.

## Limitations

- **In-Process Lifecycle**: Environment state is bound to the lifespan of the `MainframeGymEnv` instance; dropping the environment cleans up temporary catalog directories and state.
- **In-Memory Concurrency**: Tests execute against an in-memory Axum router; high-concurrency multi-client stress tests should use the standalone `zosmf-server` binary over TCP.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md)
- [open-mainframe-dataset](../open-mainframe-dataset/README.md)
