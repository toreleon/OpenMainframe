# Deploy Crate — Product Requirements

## Overview

The `open-mainframe-deploy` crate provides deployment infrastructure and observability for the OpenMainframe platform. It includes configuration management (YAML + env overrides), Kubernetes-compatible health/readiness probes, Prometheus metrics collection (COBOL, CICS, IMS, DB2), and structured logging with optional OpenTelemetry tracing. Unlike other crates, Deploy is a tooling crate — it supports running the platform in production environments.

## Current State Assessment

- **Lines of code:** 1,363
- **Test count:** 19 (all passing)
- **Maturity:** Moderate (core observability pipeline works, no actual server or runtime integration)
- **Files:** 5 Rust source files (lib, config, health, metrics, tracing_setup)

### What Works Well

**Configuration Management:**
- Hierarchical config with ServerConfig, DatabaseConfig, CobolConfig, CicsConfig, ImsConfig, ObservabilityConfig
- YAML file loading with serde deserialization
- Environment variable overrides (OPEN_MAINFRAME_* prefix)
- Sensible defaults for all fields

**Health Checks:**
- Kubernetes liveness probe (always healthy if process is running)
- Kubernetes readiness probe (checks database, CICS, IMS components)
- Thread-safe atomic bool flags for component status
- Arc-based sharing for status updates across threads
- HealthStatus and ReadinessStatus JSON-serializable responses

**Prometheus Metrics:**
- MetricsRegistry with 5 metric groups:
  - General: requests_total, request_duration, active_connections, errors_total
  - COBOL: programs_executed, execution_duration, compilation_time, active_programs
  - CICS: transactions_total, transaction_duration, active_tasks, queue_depth
  - IMS: dli_calls_total, dli_duration, segments_retrieved, active_psbs
  - Database: query_duration, pool_connections, connection_errors
- Appropriate histogram buckets per metric type
- Prometheus text format export via gather()

**Structured Logging:**
- Three log formats: JSON (production), Text (human), Compact (dev)
- Configurable log levels via environment
- OpenTelemetry OTLP integration (behind `otel` feature flag)
- tracing_subscriber registry pattern for composable layers

### What Does NOT Work

- No HTTP server for metrics and health endpoints
- No actual integration with COBOL/CICS/IMS runtimes
- No graceful shutdown coordination
- No container image building or Dockerfile generation
- No Kubernetes manifest generation (Deployment, Service, ConfigMap)
- No Helm chart or operator
- No secrets management (database credentials in plaintext config)
- No TLS/mTLS support for service endpoints
- No distributed tracing context propagation through CICS transactions
- No log correlation with CICS transaction IDs (EIBTASKN)
- No JCL batch job metrics
- No alerting rules or dashboard definitions
- No CI/CD pipeline integration
- No canary/blue-green deployment support

## Functional Requirements

### FR-v3.0-1100: HTTP Server for Metrics and Health
Implement an HTTP server exposing `/metrics` (Prometheus), `/health` (liveness), and `/ready` (readiness) endpoints. Use the already-configured server host/port and metrics_port from Config.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature (config exists but no server)
- **Reference:** Kubernetes probes require HTTP endpoints. Prometheus scrapes `/metrics` over HTTP. The config defines ports but nothing listens on them.

### FR-v3.0-1101: Runtime Integration Hooks
Provide integration points for the COBOL runtime, CICS runtime, and IMS runtime to report metrics and health status automatically. Instrument CALL dispatch, EXEC CICS commands, and DL/I calls.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **Reference:** Metrics are currently only recordable via manual method calls. Runtimes should auto-instrument without manual integration in every program.

### FR-v3.0-1102: Graceful Shutdown Coordination
Implement graceful shutdown that drains active CICS transactions, completes in-flight COBOL programs, closes database connections, flushes metrics, and exports final traces before process exit.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** Production systems require graceful shutdown for zero-downtime deployments. Kubernetes sends SIGTERM and expects clean exit within terminationGracePeriodSeconds.

### FR-v3.0-1103: Transaction-Level Distributed Tracing
Propagate tracing context through CICS LINK/XCTL calls and DB2 queries so that a single trace spans the entire transaction lifecycle. Include EIBTASKN as a trace attribute.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** OpenTelemetry distributed tracing enables end-to-end request visibility. CICS transaction IDs provide natural trace correlation.

### FR-v3.0-1104: Kubernetes Manifest Generation
Generate Kubernetes Deployment, Service, ConfigMap, and HorizontalPodAutoscaler manifests from the Config struct. Support customization via templates.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** Kubernetes deployment requires YAML manifests. Auto-generation from config reduces errors and ensures consistency.

### FR-v3.0-1105: Secrets Management
Integrate with Kubernetes Secrets or external secret stores (HashiCorp Vault, AWS Secrets Manager) for database credentials and TLS certificates. Remove plaintext secrets from config files.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** Production deployments must not store credentials in plaintext config. Kubernetes Secrets or external vaults are the standard approach.

### FR-v3.0-1106: Batch Job Metrics
Add metrics for JCL batch job execution: job_steps_total, step_duration, return_codes, job_completion_time. Support batch-specific health checks (job queue depth, backlog age).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** Mainframe workloads are often batch-heavy. Batch observability is essential for operations monitoring.

### FR-v3.0-1107: Dashboard and Alert Definitions
Provide Grafana dashboard JSON and Prometheus alerting rules for common operational scenarios: high transaction latency, connection pool exhaustion, health check failures, batch job failures.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** Dashboards and alerts are the primary interface for operations teams. Pre-built definitions accelerate production readiness.

### FR-v3.0-1108: Container Image Building
Provide Dockerfile and build script for creating an optimized container image with the OpenMainframe runtime. Support multi-stage builds and distroless base images.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** Container images are the deployment unit for Kubernetes. Optimized images reduce attack surface and startup time.
