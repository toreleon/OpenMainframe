# open-mainframe-smf

A high-performance Rust implementation of **SMF (System Management Facilities)** for the OpenMainframe project — providing the central auditing, accounting, and performance recording subsystem: binary record serialization, SMFWTM/SMFEWTM writer engine, SMFPRMxx configuration parsing, exit processing, Type 30/80/70+ records, IFASMFDP dump extraction, and Prometheus/OpenTelemetry bridges.

## Purpose

SMF is the primary instrumentation and audit subsystem on IBM z/OS mainframes, capturing structured binary records for every significant system event, batch job step, dataset I/O, security access check, and hardware performance metric. `open-mainframe-smf` models this subsystem within OpenMainframe:
1. **SMF Writer & Buffering**: Implements the `SMFWTM` and `SMFEWTM` writer pipeline with in-memory buffering, auto-flush thresholds, and 4-byte length-prefixed dataset dumping.
2. **Standard Record Format**: Encodes standard 18-byte/24-byte headers and self-defining section triplets (`SmfTriplet`: offset, entry length, entry count).
3. **Configuration & Filtering**: Parses `SMFPRMxx` PARMLIB parameters to enforce `TYPE(...)` and `NOTYPE(...)` record filtering.
4. **Subsystem Records & Observability**: Implements core records (Type 30 Job Accounting, Type 80 RACF Audit, Types 14/15/17/18 Dataset Activity, Types 70–74 RMF Performance) and bridges them to Prometheus metrics and OpenTelemetry spans.

## Capabilities

- **SMF Writer Engine (`SmfWriter`, `SmfWriterConfig`)**:
  - `smfwtm`: Standard record write respecting `NOTYPE` suppression rules.
  - `smfewtm`: Extended record write stamping subsystem identification into headers.
  - In-memory circular buffer with automatic flushing on buffer threshold or manual flush.
  - `to_dataset`: Generates standard z/OS SMF dump datasets formatted with 4-byte big-endian record length prefixes.
- **Record Structure Engine (`SmfRecord`, `SmfHeader`, `SmfTriplet`)**:
  - Binary-exact serialization and deserialization of standard SMF headers.
  - Triplet offset and length calculation for variable-length repeating sections.
- **SMFPRMxx Configuration (`SmfPrmConfig`)**:
  - Parses PARMLIB syntax: `TYPE(...)`, `NOTYPE(...)`, `STATUS(...)`, `NOPROMPT`, `EXITS(...)`, `RECORDING(DATASET|LOGSTREAM)`.
- **Exit Framework (`SmfExitRegistry`, `SmfExit`)**:
  - Standard exit invocation pipeline: `IEFU83` (record suppression and filtering), `IEFU84` (subsystem record handling), `IEFU85`, and `IEFU86`.
- **Core Record Implementations**:
  - **Type 30 (`Type30Record`, `SmfType30`)**: Job and step accounting across subtypes 1 (Job Start), 2/3 (Step Termination), 4 (Step Total), 5 (Job Termination).
  - **Type 80 (`Type80Record`)**: Security audit events, resource authorization checks, RACF command logging, logon/logoff records.
  - **Types 14/15/17/18 (`dataset`)**: Dataset open/close, I/O block counts, scratch, and rename tracking.
  - **Types 70–74 (`performance`)**: CPU utilization, paging, workload manager service class metrics, and DASD device activity.
  - **Types 100–120 (`subsystem`)**: DB2, CICS, MQ, and TCP/IP connection statistics.
- **Dump Program (`SmfDumpProgram`)**:
  - IFASMFDP-compatible utility for extracting, filtering by date/time/type range, and formatting SMF datasets.
- **Observability Bridges (`SmfToPrometheus`, `SmfToOtel`)**:
  - Maps SMF performance counters to Prometheus gauges/counters.
  - Converts SMF execution records to distributed OpenTelemetry spans.

## Architecture

```text
    Application / Subsystem               SMF Recording Environment
    ┌────────────────────┐                ┌────────────────────────┐
    │  Record Event      │    SMFWTM      │    SMF Writer          │
    │  Job, I/O, RACF    │ ─────────────> │    (SmfWriter)         │
    └────────────────────┘    Writer      │  Buffer Mgmt / Dataset │
                                          └────────────────────────┘
                                                       │
    ┌────────────────────┐                ┌────────────────────────┐
    │  Observability     │    Bridge      │    Exit Framework      │
    │  Prometheus / Otel │ <───────────── │    IEFU83 / IEFU84     │
    └────────────────────┘    Bridge      │  Filtering, Routing    │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Dump Utility      │    Extraction  │    SMF Record Types    │
    │  IFASMFDP Equiv    │ <───────────── │    Types 30, 80, 70-74 │
    └────────────────────┘    Dump        └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `record` | Core format: `SmfHeader`, `SmfRecord`, `SmfRecordType`, `SmfType4`, `SmfType5`, `SmfType30`. |
| `writer` | Writer engine: `SmfWriter`, `SmfWriterConfig`, `SmfTriplet`, `SmfWriterError`. |
| `config` | Configuration: `SmfPrmConfig`, `RecordingMode`, `LsnameConfig`, `SubsysConfig`. |
| `exits` | Exit pipeline: `SmfExitRegistry`, `Iefu83Exit`, `Iefu84Exit`, `SmfExitAction`. |
| `type30` | Job accounting: `Type30Record`, `Type30Subtype`, `JobLifecycleCollector`. |
| `type80` | Security audit: `Type80Record`, `SecurityEventType`, `EventSeverity`, `SecurityEventFilter`. |
| `dataset` | Dataset activity: `DatasetIoRecord`, `DatasetScratchRecord`, `DatasetRenameRecord`. |
| `performance`| RMF performance: `Type70Record` (CPU), `Type71Record` (Paging), `Type72Record` (WLM), `Type74Record` (Device). |
| `subsystem`| Subsystem records: `CicsTransactionRecord`, `Db2AccountingRecord`, `MqStatisticsRecord`, `TcpIpConnectionRecord`. |
| `dump` | Dump utilities: `SmfDumpProgram`, `DumpFilter`, `DumpOutputFormat`. |
| `bridge` | Modern observability: `SmfToPrometheus`, `SmfToOtel`, `PrometheusMetric`, `OtelSpan`. |

## Public API

### Core Types and Services

```rust
use open_mainframe_smf::{
    SmfWriter, SmfWriterConfig, SmfTriplet, SmfWriterError,
    record::{SmfHeader, SmfRecord, SmfRecordType, SmfType30, SmfSubtype30},
    config::SmfPrmConfig,
    exits::{SmfExitRegistry, Iefu83Exit, Iefu84Exit, SmfExitAction},
    type30::Type30Record,
    type80::Type80Record,
    dump::{SmfDumpProgram, DumpFilter},
    bridge::{SmfToPrometheus, SmfToOtel},
};
```

- `SmfWriter`: Central recording buffer engine supporting `write`, `smfwtm`, and `smfewtm`.
- `SmfTriplet`: Encapsulates self-defining section offsets, entry lengths, and entry counts.
- `SmfPrmConfig`: Parser for `SMFPRMxx` PARMLIB filtering rules.
- `SmfDumpProgram`: Program for filtering and exporting recorded SMF data.

## Integration

### Workspace Dependencies

- None (pure Rust library using standard workspace crates: `miette`, `thiserror`, `serde`, `tracing`).

### Known Consumers

- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Powers `/zosmf/resttopology/smf` endpoints for querying SMF system metrics.

## Examples

### Writing an SMF Record with NOTYPE Filtering

```rust
use open_mainframe_smf::{
    SmfWriter,
    record::{SmfType30, SmfSubtype30},
    config::SmfPrmConfig,
};

let mut writer = SmfWriter::with_defaults();

// Parse SMFPRMxx configuration
let prm = SmfPrmConfig::parse("TYPE(30,80), NOTYPE(0:29)").unwrap();

// Create Type 30 Job Start record
let type30 = SmfType30 {
    subtype: SmfSubtype30::JobStart,
    job_name: "BATCH01".to_string(),
    job_id: "JOB00100".to_string(),
    service_class: "PRODBTCH".to_string(),
    ..Default::default()
};
let record = type30.to_record();

// Write record through SMFWTM with configuration check
let seq = writer.smfwtm(&record, Some(&prm)).unwrap();
assert_eq!(seq, 1);
```

### Self-Defining Section Triplet Encoding

```rust
use open_mainframe_smf::SmfTriplet;

// Define section starting at offset 128, entry length 64 bytes, 3 entries
let triplet = SmfTriplet::new(128, 64, 3);
let bytes = triplet.to_bytes();

let restored = SmfTriplet::from_bytes(&bytes);
assert_eq!(restored.offset, 128);
assert_eq!(restored.entry_length, 64);
assert_eq!(restored.entry_count, 3);
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-smf
```

The test suite covers:
- **`record::*`**: Header byte alignment, self-defining triplet serialization/deserialization, and record type stamping.
- **`writer::*`**: Buffer auto-flush thresholds, sequence numbering, `smfwtm` suppression checks, `smfewtm` subsystem identification, and dataset length prefix formatting.
- **`config::*`**: `SMFPRMxx` PARMLIB syntax parsing, interval definitions, and active type calculation.
- **`exits::*`**: `IEFU83` record suppression closures and `IEFU84` subsystem ID verification.
- **`dump::*`**: IFASMFDP record range extraction and JSON/CSV conversion.
- **`bridge::*`**: Metric mapping from Type 70/72 records to Prometheus metrics and OpenTelemetry spans.

## Limitations

- **SYS1.MANx Datasets**: Recording operates on in-memory buffers and local disk files rather than real z/OS DASD MANx datasets with automatic HALT/SWITCH signals.
- **Coupling Facility Logstreams**: Emulates logstream mode locally without multi-system Coupling Facility logstream structures.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [RACF Subsystem (`open-mainframe-racf`)](../open-mainframe-racf/README.md)
- [WLM Subsystem (`open-mainframe-wlm`)](../open-mainframe-wlm/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
