# Deploy Crate — Epics & Stories

## Epic 1100: HTTP Server for Metrics and Health

**Goal:** Serve health, readiness, and Prometheus metrics over HTTP for Kubernetes integration.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1100

### Story 1100.1: Health and Metrics HTTP Endpoints

As a **platform operator**,
I want **HTTP endpoints for /health, /ready, and /metrics**,
So that **Kubernetes probes and Prometheus can monitor the application**.

**Acceptance Criteria:**

**Given** the server is started with config.server.port=8080
**When** GET /health is requested
**Then** a 200 response with HealthStatus JSON is returned

**Given** the metrics server on config.server.metrics_port=9090
**When** GET /metrics is requested
**Then** Prometheus text format metrics are returned

**Given** database and CICS are not ready
**When** GET /ready is requested
**Then** a 503 response with ReadinessStatus showing failed components is returned

**Complexity:** M

### Story 1100.2: Graceful Shutdown

As a **platform operator**,
I want **graceful shutdown on SIGTERM that drains connections and flushes telemetry**,
So that **zero-downtime deployments are possible**.

**Acceptance Criteria:**

**Given** active CICS transactions in progress
**When** SIGTERM is received
**Then** no new transactions are accepted; existing ones complete before shutdown

**Given** the shutdown signal
**When** the server stops
**Then** final metrics are flushed and OpenTelemetry traces are exported

**Complexity:** L

---

## Epic 1101: Runtime Integration Hooks

**Goal:** Automatically instrument COBOL, CICS, and IMS runtimes with metrics and tracing.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1101

### Story 1101.1: COBOL Runtime Instrumentation

As a **platform operator**,
I want **COBOL program executions automatically recorded as metrics**,
So that **I can monitor program performance without code changes**.

**Acceptance Criteria:**

**Given** a COBOL runtime created with `.with_metrics(registry)`
**When** a program is executed via CALL
**Then** programs_executed counter increments and execution_duration histogram records the duration

**Given** a COBOL compilation
**When** it completes
**Then** compilation_time histogram records the duration

**Complexity:** M

### Story 1101.2: CICS Transaction Instrumentation

As a **platform operator**,
I want **CICS transactions automatically recorded with command-level metrics**,
So that **I can identify slow transactions and hot commands**.

**Acceptance Criteria:**

**Given** a CICS runtime with metrics enabled
**When** a transaction executes LINK, READ, SEND MAP
**Then** each command type is recorded with duration and result code

**Given** queue operations (WRITEQ TS, READQ TD)
**When** executed
**Then** queue_depth gauge is updated

**Complexity:** M

---

## Epic 1102: Transaction-Level Distributed Tracing

**Goal:** Propagate OpenTelemetry trace context through CICS transaction lifecycles.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1103

### Story 1102.1: CICS Trace Context Propagation

As a **platform operator**,
I want **each CICS transaction to be a single distributed trace with per-program spans**,
So that **I can see end-to-end transaction flow in Jaeger or Grafana Tempo**.

**Acceptance Criteria:**

**Given** a transaction that LINKs from PROG-A to PROG-B to PROG-C
**When** viewed in a trace viewer
**Then** one trace with 3 child spans (PROG-A → PROG-B → PROG-C) is visible

**Given** PROG-B executes EXEC SQL SELECT
**When** the trace is viewed
**Then** a child span under PROG-B shows the SQL query text and duration

**Given** EIBTASKN=12345 for the transaction
**When** the trace is exported
**Then** EIBTASKN appears as a trace attribute for correlation with CICS logs

**Complexity:** L

---

## Epic 1103: Kubernetes Manifest Generation

**Goal:** Auto-generate Kubernetes deployment manifests from configuration.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1104

### Story 1103.1: Manifest Templates

As a **platform operator**,
I want **Kubernetes YAML manifests generated from my Config**,
So that **I can deploy to Kubernetes without hand-writing manifests**.

**Acceptance Criteria:**

**Given** a Config with server port 8080, metrics port 9090, database URL
**When** generate_manifests() is called
**Then** deployment.yaml, service.yaml, configmap.yaml are produced with correct values

**Given** a Deployment manifest
**When** inspected
**Then** liveness probe points to /health:8080, readiness probe to /ready:8080, metrics port annotation to 9090

**Given** custom overrides (replica count=3, memory limit=512Mi)
**When** provided
**Then** the generated manifests include the overridden values

**Complexity:** M

---

## Epic 1104: Secrets Management

**Goal:** Secure credential handling for database connections and TLS certificates.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1105

### Story 1104.1: Kubernetes Secrets Integration

As a **platform operator**,
I want **database credentials loaded from Kubernetes Secrets instead of plaintext config**,
So that **credentials are not exposed in ConfigMaps or environment dumps**.

**Acceptance Criteria:**

**Given** a Kubernetes Secret `open-mainframe-db-credentials` with keys `url`, `username`, `password`
**When** the application starts
**Then** database config is populated from the mounted secret files

**Given** no secret mount present
**When** the application starts
**Then** fallback to OPEN_MAINFRAME_DB_URL environment variable or config file

**Complexity:** M

---

## Epic 1105: Batch Job Metrics

**Goal:** Add observability for JCL batch job execution alongside online CICS metrics.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1106

### Story 1105.1: JCL Job Execution Metrics

As a **platform operator**,
I want **batch job metrics (steps executed, duration, return codes, completion time)**,
So that **I can monitor batch workload health alongside online transactions**.

**Acceptance Criteria:**

**Given** a JCL job with 3 steps executes successfully
**When** metrics are recorded
**Then** job_steps_total=3, step_duration for each step, job_return_code=0

**Given** a step returns RC=12 (error)
**When** metrics are recorded
**Then** job_return_code=12 and errors_total increments with label job_name

**Complexity:** M

---

## Epic 1106: Dashboard and Alert Definitions

**Goal:** Provide pre-built Grafana dashboards and Prometheus alerting rules.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1107

### Story 1106.1: Grafana Dashboard JSON

As a **platform operator**,
I want **pre-built Grafana dashboards for COBOL, CICS, IMS, and database metrics**,
So that **I can deploy monitoring immediately without designing dashboards from scratch**.

**Acceptance Criteria:**

**Given** the exported dashboard JSON
**When** imported into Grafana
**Then** panels show: transaction rate, latency percentiles, error rate, active connections, queue depth

**Complexity:** M

### Story 1106.2: Prometheus Alert Rules

As a **platform operator**,
I want **alert rules for high latency, connection pool exhaustion, and health failures**,
So that **I am notified before service degradation impacts users**.

**Acceptance Criteria:**

**Given** the alerting rules YAML
**When** loaded by Prometheus
**Then** alerts fire for: p99 latency > 1s for 5m, connection pool utilization > 90%, readiness check failing for 1m

**Complexity:** S

---

## Epic 1107: Container Image Building

**Goal:** Provide Dockerfile and build scripts for optimized container images.

**Crate:** `open-mainframe-deploy`
**FRs:** FR-v3.0-1108

### Story 1107.1: Multi-Stage Dockerfile

As a **platform operator**,
I want **a multi-stage Dockerfile that builds the OpenMainframe runtime and produces a minimal container image**,
So that **deployment is simple and the image is small and secure**.

**Acceptance Criteria:**

**Given** the Dockerfile
**When** built with `docker build`
**Then** a container image is produced with the compiled binary and minimal runtime dependencies

**Given** the produced image
**When** inspected
**Then** the image size is < 100MB and contains no build tools or source code

**Complexity:** S
