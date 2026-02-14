---
version: 'v1.3'
baseVersion: 'v1.2'
date: '2026-02-13'
status: 'draft'
---

# Architecture Document - OpenMainframe v1.3: Production Ready

## Overview

v1.3 extends the OpenMainframe architecture with IMS/DB hierarchical database support, advanced CICS services, COBOL-2014 language features, and production deployment infrastructure.

## New Crates

### open-mainframe-ims

IMS/DB hierarchical database support with DL/I call interface.

```
open-mainframe-ims/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── dbd/              # Database Definition
│   │   ├── mod.rs
│   │   ├── parser.rs     # DBD macro parser
│   │   └── schema.rs     # Segment/field definitions
│   ├── psb/              # Program Specification Block
│   │   ├── mod.rs
│   │   ├── parser.rs     # PSB macro parser
│   │   └── pcb.rs        # PCB structures
│   ├── dli/              # DL/I Call Interface
│   │   ├── mod.rs
│   │   ├── calls.rs      # GU, GN, GNP, ISRT, DLET, REPL
│   │   ├── ssa.rs        # Segment Search Arguments
│   │   └── status.rs     # Status codes
│   ├── runtime/          # Runtime Support
│   │   ├── mod.rs
│   │   ├── database.rs   # PostgreSQL backend
│   │   ├── navigation.rs # Hierarchical path navigation
│   │   └── transaction.rs
│   └── preprocess/       # EXEC DLI preprocessing
│       ├── mod.rs
│       └── scanner.rs
```

### open-mainframe-deploy

Kubernetes and cloud deployment support.

```
open-mainframe-deploy/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── health.rs         # Health check handlers
│   ├── metrics.rs        # Prometheus metrics
│   ├── config.rs         # Environment configuration
│   └── tracing.rs        # OpenTelemetry integration
├── kubernetes/
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── configmap.yaml
│   └── hpa.yaml
└── helm/
    └── open-mainframe/
        ├── Chart.yaml
        ├── values.yaml
        └── templates/
```

## Extended Crates

### open-mainframe-cics (Extended)

New modules for advanced CICS functionality:

```
open-mainframe-cics/src/
├── queues/               # NEW: Queue Services
│   ├── mod.rs
│   ├── ts.rs            # Temporary Storage queues
│   └── td.rs            # Transient Data queues
├── interval/            # NEW: Interval Control
│   ├── mod.rs
│   ├── start.rs         # START command
│   ├── retrieve.rs      # RETRIEVE command
│   └── delay.rs         # DELAY command
├── time/                # NEW: Time Services
│   ├── mod.rs
│   ├── asktime.rs       # ASKTIME command
│   └── formattime.rs    # FORMATTIME command
├── journal/             # NEW: Journal Services
│   └── mod.rs
├── sync/                # NEW: Synchronization
│   ├── mod.rs
│   ├── syncpoint.rs     # SYNCPOINT command
│   └── enqdeq.rs        # ENQ/DEQ commands
└── inquiry/             # NEW: Resource Inquiry
    └── mod.rs           # INQUIRE/SET commands
```

### open-mainframe-cobol (Extended)

COBOL-2014 features:

```
open-mainframe-cobol/src/
├── intrinsics/          # Extended intrinsic functions
│   ├── string.rs        # TRIM, SUBSTITUTE, CONCATENATE (NEW)
│   └── numeric.rs       # Extended numeric functions
├── types/               # Extended data types
│   ├── boolean.rs       # NEW: BOOLEAN type
│   ├── typedef.rs       # NEW: TYPEDEF support
│   └── pointer.rs       # NEW: FUNCTION-POINTER
├── xml/                 # NEW: XML Support
│   ├── mod.rs
│   ├── generate.rs      # XML GENERATE
│   └── parse.rs         # XML PARSE
├── json/                # NEW: JSON Support
│   ├── mod.rs
│   ├── generate.rs      # JSON GENERATE
│   └── parse.rs         # JSON PARSE
└── unicode/             # NEW: Unicode Support
    └── mod.rs           # UTF-8 handling
```

## Data Flow

### IMS Hierarchical Access

```
┌─────────────────┐
│  COBOL Program  │
│  EXEC DLI GU... │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ DL/I Preprocessor│
│ (open-mainframe-ims/preproc)│
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  DL/I Runtime   │
│ (open-mainframe-ims/runtime)│
└────────┬────────┘
         │ SSA parsing
         │ Path navigation
         ▼
┌─────────────────┐
│   PostgreSQL    │
│ (Hierarchical   │
│  tables + JSON) │
└─────────────────┘
```

### CICS Queue Operations

```
┌─────────────────┐
│  CICS Program   │
│ WRITEQ TS/TD    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────┐
│  Queue Manager  │────▶│   Redis     │ (optional fast path)
│ (open-mainframe-cics/queues)│    └─────────────┘
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   PostgreSQL    │ (durable storage)
│  (queue tables) │
└─────────────────┘
```

## IMS Database Schema

### Hierarchical to Relational Mapping

IMS hierarchical structure:
```
          ROOT
         /    \
      CHILD1  CHILD2
       |
    GRANDCHILD
```

PostgreSQL representation:
```sql
CREATE TABLE ims_segments (
    segment_id BIGSERIAL PRIMARY KEY,
    parent_id BIGINT REFERENCES ims_segments(segment_id),
    db_name VARCHAR(8) NOT NULL,
    segment_name VARCHAR(8) NOT NULL,
    level_number INT NOT NULL,
    sequence_field VARCHAR(255),
    segment_data JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX idx_ims_parent ON ims_segments(parent_id);
CREATE INDEX idx_ims_db_segment ON ims_segments(db_name, segment_name);
CREATE INDEX idx_ims_sequence ON ims_segments(db_name, segment_name, sequence_field);
```

### SSA (Segment Search Argument) Processing

```rust
pub struct SegmentSearchArg {
    pub segment_name: String,
    pub command_codes: Vec<CommandCode>,
    pub qualification: Option<Qualification>,
}

pub enum CommandCode {
    D,      // Path call
    F,      // First occurrence
    L,      // Last occurrence
    N,      // Path call ignore
    P,      // Set parentage
    U,      // Maintain position
    V,      // Maintain position at this level
}

pub struct Qualification {
    pub field_name: String,
    pub operator: QualOp,
    pub value: Vec<u8>,
}

pub enum QualOp {
    Eq,     // =
    Ne,     // !=
    Lt,     // <
    Le,     // <=
    Gt,     // >
    Ge,     // >=
}
```

## CICS Queue Implementation

### Temporary Storage (TS) Queue

```rust
pub struct TsQueue {
    pub name: String,
    pub queue_type: TsType,
    pub items: Vec<TsItem>,
}

pub enum TsType {
    Main,       // Main storage (in-memory)
    Auxiliary,  // Auxiliary storage (disk)
}

pub struct TsItem {
    pub item_number: u32,
    pub data: Vec<u8>,
    pub length: usize,
}

impl TsQueueManager {
    pub fn writeq(&mut self, queue: &str, data: &[u8], item: Option<u32>) -> CicsResult<u32>;
    pub fn readq(&self, queue: &str, item: u32) -> CicsResult<Vec<u8>>;
    pub fn deleteq(&mut self, queue: &str) -> CicsResult<()>;
}
```

### Transient Data (TD) Queue

```rust
pub struct TdQueue {
    pub name: String,
    pub dest_type: TdDestType,
    pub trigger_level: Option<u32>,
}

pub enum TdDestType {
    Intrapartition,   // Internal queue
    Extrapartition,   // External file
}
```

## COBOL-2014 Type System

### Boolean Type

```rust
pub enum CobolBoolean {
    True,
    False,
}

// COBOL syntax:
// 01 WS-FLAG PIC 1 TYPE BOOLEAN.
// SET WS-FLAG TO B"1".  (true)
// SET WS-FLAG TO B"0".  (false)
```

### TYPEDEF Support

```rust
pub struct TypeDef {
    pub name: String,
    pub base_type: DataType,
    pub constraints: Vec<TypeConstraint>,
}

// COBOL syntax:
// TYPEDEF CUSTOMER-ID AS PIC X(10).
// 01 WS-CUST-ID TYPE CUSTOMER-ID.
```

## Kubernetes Architecture

```yaml
# Deployment topology
┌─────────────────────────────────────────────────┐
│                  Kubernetes Cluster              │
│  ┌──────────────────────────────────────────┐   │
│  │            open-mainframe Namespace            │   │
│  │                                           │   │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  │   │
│  │  │ Pod 1   │  │ Pod 2   │  │ Pod 3   │  │   │
│  │  │open-mainframe│  │open-mainframe│  │open-mainframe│  │   │
│  │  └────┬────┘  └────┬────┘  └────┬────┘  │   │
│  │       │            │            │        │   │
│  │  ┌────┴────────────┴────────────┴────┐  │   │
│  │  │           Service (ClusterIP)      │  │   │
│  │  └───────────────────┬───────────────┘  │   │
│  │                      │                   │   │
│  │  ┌───────────────────┴───────────────┐  │   │
│  │  │        Ingress Controller          │  │   │
│  │  └───────────────────────────────────┘  │   │
│  └──────────────────────────────────────────┘   │
│                                                  │
│  ┌────────────┐  ┌────────────┐                 │
│  │ PostgreSQL │  │   Redis    │                 │
│  │  (StatefulSet) (optional)  │                 │
│  └────────────┘  └────────────┘                 │
└─────────────────────────────────────────────────┘
```

## Observability Stack

### Prometheus Metrics

```rust
pub struct OpenMainframeMetrics {
    // COBOL execution
    cobol_programs_executed: Counter,
    cobol_execution_duration: Histogram,

    // CICS transactions
    cics_transactions_total: Counter,
    cics_transaction_duration: Histogram,
    cics_queue_depth: Gauge,

    // IMS operations
    ims_dli_calls_total: Counter,
    ims_dli_call_duration: Histogram,

    // Database
    db_connections_active: Gauge,
    db_query_duration: Histogram,
}
```

### Health Endpoints

```rust
// GET /health - Liveness probe
pub async fn health() -> impl Responder {
    HttpResponse::Ok().json(json!({"status": "ok"}))
}

// GET /ready - Readiness probe
pub async fn ready(db: &DbPool, redis: Option<&RedisPool>) -> impl Responder {
    let db_ok = db.check().await.is_ok();
    let redis_ok = redis.map(|r| r.check().await.is_ok()).unwrap_or(true);

    if db_ok && redis_ok {
        HttpResponse::Ok().json(json!({"status": "ready"}))
    } else {
        HttpResponse::ServiceUnavailable().json(json!({"status": "not ready"}))
    }
}
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `OPEN_MAINFRAME_DB_URL` | PostgreSQL connection string | required |
| `OPEN_MAINFRAME_REDIS_URL` | Redis connection string | optional |
| `OPEN_MAINFRAME_LOG_LEVEL` | Log level (trace/debug/info/warn/error) | info |
| `OPEN_MAINFRAME_METRICS_PORT` | Prometheus metrics port | 9090 |
| `OPEN_MAINFRAME_HEALTH_PORT` | Health check port | 8080 |
| `OPEN_MAINFRAME_CICS_TS_BACKEND` | TS queue backend (memory/redis/postgres) | memory |
| `OPEN_MAINFRAME_IMS_SCHEMA` | PostgreSQL schema for IMS data | ims |

## Migration Path

### From v1.2 to v1.3

1. **Database migrations**: Run IMS schema setup
2. **Configuration**: Add new environment variables
3. **CICS programs**: Update to use new queue APIs (backward compatible)
4. **COBOL programs**: Can use COBOL-2014 features (optional)

### Breaking Changes

None - v1.3 is fully backward compatible with v1.2.

## Testing Strategy

### IMS Testing

- Unit tests for SSA parsing
- Integration tests with PostgreSQL hierarchical data
- Compatibility tests with real IMS DBD/PSB definitions

### CICS Queue Testing

- Unit tests for queue operations
- Stress tests for queue throughput
- Redis failover tests

### Kubernetes Testing

- Helm chart linting
- Kind cluster integration tests
- Chaos testing (pod failures)
