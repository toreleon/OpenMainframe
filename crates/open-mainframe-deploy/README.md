# open-mainframe-deploy

Deployment, health checks, metrics, and observability infrastructure for running OpenMainframe workloads in cloud-native Docker and Kubernetes environments.

## Purpose

The `open-mainframe-deploy` crate bridges mainframe subsystems (COBOL, CICS, IMS, JCL, databases) and modern cloud-native operational platforms. It provides automated multi-stage Dockerfile generation, Kubernetes manifest templating with health probes and Prometheus scrape annotations, three-tier secret resolution, RAII-based workload metrics collection, Grafana dashboard and Prometheus alerting generation, and distributed W3C trace propagation.

## Capabilities

- **Containerization & Kubernetes** (`container`, `k8s_manifest`):
  - Generates multi-stage Dockerfiles with unprivileged `omf` user (UID 1000), healthcheck commands, and OCI image metadata labels.
  - Generates Kubernetes Deployment, Service, and ConfigMap YAML manifests with resource requests/limits, liveness/readiness probes, and Secret volume mounts.
- **Configuration & Secret Resolution** (`config`, `secrets`):
  - YAML configuration loader (`Config`) with environment variable overrides.
  - Three-tier credential resolver (`SecretsResolver`): Kubernetes Secret volume mount (`/etc/open-mainframe/secrets`) → environment variables (`OPEN_MAINFRAME_DB_*`) → config fallback.
- **Health Checks & Probes** (`health`, `server`):
  - Thread-safe atomic liveness (`/health`) and readiness (`/ready`) endpoints with per-subsystem status flags (database, CICS, IMS) and uptime tracking.
  - Built-in lightweight async HTTP server for probe handling and Prometheus metric scraping.
- **Prometheus Metrics** (`metrics`, `batch_metrics`):
  - `MetricsRegistry`: Counters, gauges, and histograms for HTTP requests, active connections, database queries and connection pools, COBOL program compilations/executions, CICS transactions and queues, and IMS DL/I calls.
  - `BatchMetricsCollector` & `JobExecutionTracker`: Per-job and per-step duration averages, success rates, return codes, and I/O record statistics exported in Prometheus text format.
- **RAII Instrumentation Guards** (`instrumentation`):
  - `CobolInstrumentation`, `CicsInstrumentation`, `ImsInstrumentation` returning `ProgramExecGuard` and `TransactionGuard` that decrement active counters and record execution durations on drop (panic-safe).
- **Observability Assets & Tracing** (`dashboards`, `trace_context`, `tracing_setup`):
  - Automated Grafana dashboard JSON generator and Prometheus alerting rule YAML generator (with pre-built alerts for latency, error rate, and pool exhaustion).
  - W3C trace context generation with trace IDs mapped from CICS `EIBTASKN` task numbers and hierarchical span trees.
  - Structured logging (JSON, text, compact) and optional OpenTelemetry OTLP trace exporter (`otel` feature).

## Architecture

```
    ┌─────────────────────────────────────────────────────────────────┐
    │                      Public API (lib.rs)                        │
    ├──────────────────┬─────────────────────┬────────────────────────┤
    │   Deployment     │   Observability     │   Runtime Integration  │
    ├──────────────────┼─────────────────────┼────────────────────────┤
    │ config           │ metrics             │ health                 │
    │ container        │ batch_metrics       │ instrumentation        │
    │ k8s_manifest     │ dashboards          │ server                 │
    │ secrets          │ trace_context       │                        │
    │                  │ tracing_setup       │                        │
    ├──────────────────┴─────────────────────┴────────────────────────┤
    │  prometheus  │  tracing  │  opentelemetry  │  tokio  │  serde   │
    └──────────────┴───────────┴─────────────────┴─────────┴──────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `config` | YAML and environment-variable configuration loading with priority chain |
| `container` | Multi-stage Dockerfile and `.dockerignore` generation |
| `k8s_manifest` | Kubernetes Deployment, Service, and ConfigMap YAML generation |
| `secrets` | Three-tier credential resolution (Secret mount > env > config) |
| `server` | Lightweight HTTP probe and `/metrics` exposition server |
| `health` | Liveness and readiness probe management with atomic component flags |
| `metrics` | Prometheus metric definitions for COBOL, CICS, IMS, DB, and HTTP |
| `batch_metrics` | Batch job execution metrics, step I/O stats, and text export |
| `instrumentation` | RAII guards for automatic runtime duration and active gauge tracking |
| `dashboards` | Grafana dashboard JSON and Prometheus alert rule generation |
| `trace_context` | Distributed W3C trace context with EIBTASKN-derived trace IDs |
| `tracing_setup` | Structured logging formatting and OpenTelemetry OTLP initialization |

## Public API

### Primary Types and Functions

- **Configuration & Secrets**: `Config`, `ServerConfig`, `DatabaseConfig`, `ObservabilityConfig`, `SecretsResolver`, `DatabaseCredentials`, `CredentialSource`.
- **Containers & Manifests**: `DockerConfig`, `generate_dockerfile()`, `generate_dockerignore()`, `ManifestOverrides`, `GeneratedManifests`, `generate_manifests()`.
- **Health & Server**: `HealthChecker`, `HealthStatus`, `ReadinessStatus`, `start_servers()`, `ServerHandle`.
- **Metrics & Instrumentation**: `MetricsRegistry`, `CobolInstrumentation`, `CicsInstrumentation`, `ImsInstrumentation`, `ProgramExecGuard`, `TransactionGuard`, `JobExecutionTracker`, `BatchMetricsCollector`, `InstrumentedRuntime`.
- **Dashboards & Alerts**: `DashboardConfig`, `generate_dashboard()`, `AlertConfig`, `generate_alert_rules()`.
- **Tracing**: `TransactionTrace`, `TraceSpan`, `SpanKind`, `TraceId`, `SpanId`, `TracingConfig`, `init_tracing()`.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `prometheus`, `tracing`, `tracing-subscriber`, `serde`, `serde_yaml`, `tokio`, `thiserror`, `opentelemetry`, etc.).
- **Consumers**: Standalone deployment and observability crate; provides runtime monitoring wrappers, health probe responders, and container generators for OpenMainframe components.

## Examples

### Server Startup and Health Check

```rust
use open_mainframe_deploy::{
    Config, HealthChecker, MetricsRegistry, start_servers,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = Config::default();
    let health = HealthChecker::new();
    let registry = Arc::new(MetricsRegistry::new(&config.observability.metrics_prefix)?);

    let handle = start_servers(&config.server, health.clone(), registry).await?;
    health.set_database_ready(true);
    health.set_cics_ready(true);

    // Shutdown when complete
    handle.shutdown();
    handle.wait().await;
    Ok(())
}
```

### RAII Instrumentation Guards

```rust
use open_mainframe_deploy::{CobolInstrumentation, MetricsRegistry};
use std::sync::Arc;

let registry = Arc::new(MetricsRegistry::new("open_mainframe").unwrap());
let cobol = CobolInstrumentation::new(registry);

// Guard increments active executions and records duration on finish / drop
let guard = cobol.begin_execution("COSGN00C");
// ... execute program ...
guard.finish(true);
```

### Generating Kubernetes Manifests and Dockerfiles

```rust
use open_mainframe_deploy::{
    generate_dockerfile, generate_manifests, Config, DockerConfig, ManifestOverrides,
};

let config = Config::default();
let dockerfile = generate_dockerfile(&DockerConfig::default());
let manifests = generate_manifests(&config, &ManifestOverrides::default());

assert!(dockerfile.contains("FROM debian:bookworm-slim"));
assert!(manifests.deployment.contains("apiVersion: apps/v1"));
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-deploy
```

The test suite contains 89 unit tests covering:
- Dockerfile generation (unprivileged user, healthcheck commands, multi-stage builds).
- Kubernetes manifest generation (probes, secret volume mounts, resource limits, Prometheus annotations).
- Three-tier credential resolution priority.
- Health checker atomic flags and liveness/readiness responses.
- Prometheus metric registration and counter/histogram recording.
- Batch job tracking and Prometheus text format export.
- RAII execution guards and drop safety under concurrent execution.
- Grafana dashboard JSON and alert rule generation.
- W3C trace context format and span hierarchy.

## Limitations

- **Atomic Health Flag Probes**: `HealthChecker` checks atomic boolean flags set by application components rather than executing live database network pings on every probe request.
- **Built-in HTTP Server**: The built-in metrics and health HTTP server implements lightweight raw TCP request parsing for GET requests; HTTP/2 and TLS termination are expected to be handled by Kubernetes ingress or sidecars.
- **Static Manifest Generation**: Kubernetes manifests are emitted as static YAML files; Helm chart package structures are not generated directly.
- **In-Memory Batch Metrics**: Batch execution history is aggregated in-memory; long-term historical storage relies on external Prometheus scraping.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
