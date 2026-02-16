# Deploy Crate — Architecture Decisions

## AD-3.0-01: Axum HTTP Server for Endpoints

**Context:** The crate defines configuration for server host/port and metrics_port but has no HTTP server implementation. Health checks and Prometheus metrics require HTTP endpoints.

**Decision:** Use `axum` as the HTTP framework for serving health, readiness, and metrics endpoints. Two servers run concurrently: (1) the application server on `config.server.port` serving health/readiness probes; (2) the metrics server on `config.server.metrics_port` serving Prometheus `/metrics`. Both are async via tokio. The server module provides `start_servers()` which spawns both as background tokio tasks and returns handles for shutdown coordination.

**Consequences:**
- Adds `axum` and `tokio` (already present) as dependencies
- Two-port architecture separates application traffic from monitoring
- Health endpoints can be exposed to Kubernetes without exposing metrics to the public
- Graceful shutdown uses tokio::signal for SIGTERM handling

## AD-3.0-02: Runtime Instrumentation via Middleware Traits

**Context:** Metrics recording currently requires manual method calls. Runtimes (COBOL, CICS, IMS) should automatically emit metrics without each program explicitly calling record methods.

**Decision:** Define an `Instrumented` trait that wraps runtime operations with metrics recording. The COBOL runtime's CALL dispatch, CICS runtime's command execution, and IMS DL/I calls gain middleware-style instrumentation. Each operation is wrapped: record start time, execute, record duration/status. The `MetricsRegistry` is passed as an `Arc<MetricsRegistry>` to runtime constructors. Instrumentation is opt-in via a `with_metrics()` builder method on each runtime.

**Consequences:**
- Zero-overhead when metrics are not enabled (builder pattern)
- No code changes needed in COBOL programs
- Each runtime crate gains an optional dependency on `open-mainframe-deploy`
- Metric names and labels are standardized across runtimes
- Hot path instrumentation adds ~microseconds of overhead per operation

## AD-3.0-03: Trace Context Propagation via CICS Task Context

**Context:** CICS transactions invoke multiple programs via LINK/XCTL. Each program invocation should be a span within a single trace. The CICS EIB (Execute Interface Block) already carries EIBTASKN (task number) which is a natural trace correlation ID.

**Decision:** Attach an OpenTelemetry `SpanContext` to the CICS task context. When LINK or XCTL transfers control, the current span is ended and a new child span is started for the called program. The trace ID is derived from EIBTASKN to ensure all spans in a transaction share the same trace. DB2 queries within the transaction create child spans with SQL text as span attributes.

**Consequences:**
- Full transaction visibility in Jaeger/Grafana Tempo
- EIBTASKN provides deterministic trace ID (no random generation needed)
- Span hierarchy matches program call stack
- DB2 query visibility within transaction context
- Requires CICS runtime to carry span context (minor API change)

## AD-3.0-04: Kubernetes Manifest Templates via Tera

**Context:** Deploying to Kubernetes requires YAML manifests (Deployment, Service, ConfigMap, HPA). Manual creation is error-prone and inconsistent.

**Decision:** Use the `tera` template engine to generate Kubernetes manifests from the `Config` struct. Templates are embedded in the binary via `include_str!`. A `generate_manifests()` function takes a `Config` and produces a map of filename → YAML content. Templates expose all config fields and support custom overrides via a supplementary values map. Output includes: deployment.yaml, service.yaml, configmap.yaml, hpa.yaml.

**Consequences:**
- Single command to generate all deployment manifests
- Templates are versioned with the crate (no external dependencies)
- Custom overrides handle site-specific requirements
- Helm chart can be built on top of the generated manifests
- Template changes require crate rebuild (embedded, not external files)
