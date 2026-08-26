# open-mainframe-idms

z/OS IDMS (Integrated Database Management System) — CODASYL network database model, navigational DML, DMCL storage, IDMS-DC transaction processing, and SQL option for the OpenMainframe project.

## Purpose

CA IDMS (formerly Cullinet IDMS) is a high-performance database management system based on the CODASYL network database model, featuring navigational Data Manipulation Language (DML), the IDMS-DC online teleprocessing monitor, and the ADS/Online 4GL development environment. The `open-mainframe-idms` crate provides a complete Rust implementation of IDMS, modeling schema/subschema DDL compilation, record and set currency tracking, navigational DML operations, CALC/VIA page placement, lock management with deadlock detection, transaction journaling/recovery, and SQL access over network records.

## Capabilities

- **CODASYL Network Data Model (`codasyl`)**:
  - Database structure modeling: Areas (`AreaDef`), Record Types (`RecordType`), and Set Types (`SetType`).
  - Set relationships: Owner and member records with `SetOrder` (First, Last, Next, Prior, Sorted), `SetMode` (Chain, Pointer Array), and `SetMembership` (Mandatory/Optional Automatic/Manual).
  - Record location modes (`LocationMode`): `Calc` (hashed by key), `Direct` (direct dbkey), and `Via` (clustered within set owner).
- **Schema & Subschema DDL (`schema`)**:
  - DDL parser (`SchemaParser`, `SubschemaParser`) parsing schema definitions, area definitions, record layouts, and subschema views with schema validation.
- **Navigational DML Engine (`dml`)**:
  - Execution engine (`DmlEngine`) supporting all standard DML statements: `BIND RUN-UNIT`, `READY` (area usage modes: Retrieval, Update, Exclusive, Protected), `FIND`, `GET`, `OBTAIN`, `STORE`, `MODIFY`, `ERASE`, `CONNECT`, `DISCONNECT`.
  - Navigation modes (`FindMode`): `First`, `Last`, `Next`, `Prior`, `Calc`, `WithinArea`, and `Dbkey`.
  - Four-digit IDMS status codes (`0000` Success, `0307` EndOfSet, `0326` RecordNotFound, `1207` DuplicateRecord, `0306` NoCurrency, `0069` AreaNotOpen).
- **Currency Indicators (`currency`)**:
  - Complete currency table (`CurrencyTable`) tracking current of run-unit, current of record type, current of set type, and current of area, with selective suppression on navigation.
- **COBOL DML Precompiler (`precompiler`)**:
  - Source transformer (`DmlPrecompiler`) translating `EXEC IDMS ... END-EXEC` blocks into `CALL 'IDMS'` statements and inserting the `SUBSCHEMA-CTRL` communication area.
- **Storage Management & DMCL (`storage`)**:
  - Device Media Control Language (`DmclConfig`) and physical page management (`PageManager`).
  - Hashing algorithm (`CalcRoutine`) for CALC-key target page distribution and overflow chaining.
  - Clustered record placement (`ViaPlacement`) targeting set owner pages.
- **IDMS-DC Transaction Monitor (`dc`)**:
  - Online environment simulation (`DcRuntime`, `IdmsDcTask`, `TaskScheduler`) supporting prioritized task dispatching, pseudo-conversational transactions, `ScratchArea` temporary storage, `QueueArea` FIFO queues, and 3270 terminal map binding (`MapSupport`).
- **Lock Management & Concurrency (`lock`)**:
  - Resource locking (`LockManager`) supporting record-level and area-level locks (`Shared`, `Exclusive`).
  - Deadlock detection (`DeadlockDetector`) via cycle detection on wait-for dependency graphs.
- **Recovery & Journaling (`recovery`)**:
  - Transaction journaling (`JournalManager`) capturing before/after images and checkpoints.
  - Transaction rollback (`RollbackManager`), warm start recovery (`WarmStart`), and cold start initialization (`ColdStart`).
- **Logical Record Facility & SQL Option (`lrf`, `sql_option`)**:
  - LRF engine (`LrfEngine`, `LogicalRecord`) evaluating path directions and retrieval conditions.
  - Relational SQL engine (`IdmsSqlEngine`, `IdmsSqlParser`) executing `SELECT`, `INSERT`, `UPDATE`, and `DELETE` queries over CODASYL record types and views.
- **ADS/Online 4GL (`ads`)**:
  - Dialog definitions (`AdsDialog`, `AdsMap`, `AdsProcess`) for dialog-driven application development.

## Architecture

```
                      Application Source (COBOL / ADS)
                                     │
                                     ▼
                      ┌─────────────────────────────┐
                      │       DmlPrecompiler        │
                      └──────────────┬──────────────┘
                                     │
                                     ▼
        ┌─────────────────────────────────────────────────────────┐
        │                        DmlEngine                        │
        │                                                         │
        │  ┌────────────────────┐         ┌────────────────────┐  │
        │  │   CurrencyTable    │         │    LockManager     │  │
        │  │ (Run/Rec/Set/Area) │         │ (Deadlock Detect)  │  │
        │  └─────────┬──────────┘         └─────────┬──────────┘  │
        │            │                              │             │
        │            ▼                              ▼             │
        │  ┌────────────────────┐         ┌────────────────────┐  │
        │  │   PageManager      │ <─────> │   JournalManager   │  │
        │  │ (CALC / VIA / DMCL)│         │ (Rollback/Recovery)│  │
        │  └────────────────────┘         └────────────────────┘  │
        └─────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `codasyl` | CODASYL network model definitions (`CodasylSchema`, `RecordType`, `SetType`, `AreaDef`, `LocationMode`, `SetOrder`) |
| `schema` | Schema and Subschema DDL parsers (`SchemaParser`, `SubschemaParser`, `Subschema`) |
| `dml` | Navigational DML engine (`DmlEngine`, `DmlResult`, `RecordInstance`, `UsageMode`, `FindMode`, `StatusCode`) |
| `currency` | Currency indicator management (`CurrencyTable`, `CurrencyUpdate`) |
| `dc` | IDMS-DC transaction manager (`DcRuntime`, `IdmsDcTask`, `TaskScheduler`, `ScratchArea`, `QueueArea`, `MapSupport`) |
| `precompiler` | COBOL DML precompiler (`DmlPrecompiler`) for `EXEC IDMS` source transformation |
| `storage` | DMCL physical storage (`PageManager`, `CalcRoutine`, `ViaPlacement`, `DmclConfig`) |
| `lock` | Locking and concurrency control (`LockManager`, `DeadlockDetector`, `LockMode`) |
| `sql_option` | Relational SQL interface over CODASYL data (`IdmsSqlEngine`, `IdmsSqlParser`, `SqlCursor`, `SqlView`) |
| `recovery` | Journaling and crash recovery (`JournalManager`, `RollbackManager`, `WarmStart`, `ColdStart`) |
| `ads` | ADS/Online dialog management (`AdsDialog`, `AdsMap`, `AdsProcess`) |
| `lrf` | Logical Record Facility (`LrfEngine`, `LogicalRecord`, `PathDirection`) |

## Public API

### Primary Types and Functions

- `DmlEngine`: Navigational database engine:
  - `new(schema: &CodasylSchema) -> Self`: Initialize DML engine for a given schema.
  - `bind_run_unit(&mut self, subschema: &str) -> DmlResult`: Establish run-unit binding.
  - `ready(&mut self, area: &str, mode: UsageMode) -> DmlResult`: Open an area with specified locking/usage mode.
  - `store(&mut self, record_type: &str, fields: HashMap<String, FieldValue>) -> DmlResult`: Insert a new record instance.
  - `find(&mut self, mode: FindMode, target: &str) -> DmlResult`: Navigate to a record instance.
  - `get(&mut self, record_type: &str) -> DmlResult`: Retrieve current record content.
  - `modify(&mut self, record_type: &str, fields: HashMap<String, FieldValue>) -> DmlResult`: Update current record.
  - `erase(&mut self, record_type: &str) -> DmlResult`: Delete current record.
- `CodasylSchema`: In-memory metadata container defining areas, records, fields, and sets.
- `DmlPrecompiler`: Source transformer converting `EXEC IDMS` blocks to `CALL 'IDMS'`.
- `IdmsSqlEngine`: Relational query processor executing SQL statements against CODASYL tables.
- `TaskScheduler`: IDMS-DC task dispatcher managing prioritized task queues and pseudo-conversational cycles.

## Integration

### Internal Workspace Dependencies

- `miette`: Diagnostic error reporting.
- `thiserror`: Error type derive macros.
- `serde` / `serde_json`: Persistence and schema serialization.
- `tracing`: Structured diagnostics logging.

### Workspace Consumers

- `open-mainframe-wiki`: Documented in the language and database catalog.
- Root workspace member in `Cargo.toml`.

## Examples

### Navigational DML Record Creation and Retrieval

```rust
use std::collections::HashMap;
use open_mainframe_idms::codasyl::{AreaDef, CodasylSchema, LocationMode, RecordType};
use open_mainframe_idms::dml::{DmlEngine, FindMode, UsageMode};
use open_mainframe_idms::dml::FieldValue;

fn main() {
    let mut schema = CodasylSchema::new("CUSTSCHEMA");
    schema.add_area(AreaDef::new("CUST-AREA", 1, 100));
    schema.add_record(RecordType::new("CUSTOMER", 1001, LocationMode::Direct, "CUST-AREA"));

    let mut engine = DmlEngine::new(&schema);
    engine.bind_run_unit("CUSTSS").expect("bind failed");
    engine.ready("CUST-AREA", UsageMode::Update).expect("ready failed");

    let mut fields = HashMap::new();
    fields.insert("NAME".to_string(), FieldValue::Text("ACME CORP".to_string()));
    let store_res = engine.store("CUSTOMER", fields).expect("store failed");
    let dbkey = store_res.dbkey.expect("missing dbkey");

    let find_res = engine.find(FindMode::Dbkey(dbkey), "CUSTOMER").expect("find failed");
    assert!(find_res.status == open_mainframe_idms::dml::StatusCode::Success);

    let get_res = engine.get("CUSTOMER").expect("get failed");
    let record = get_res.record.expect("record missing");
    assert_eq!(record.get_field("NAME"), Some(&FieldValue::Text("ACME CORP".to_string())));
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-idms
```

The crate contains 100 unit tests verifying:
- `codasyl` & `schema`: Schema and subschema DDL parsing, validation of record fields and set definitions.
- `dml`: `STORE`, `FIND` (First/Last/Next/Prior/Calc/Dbkey), `GET`, `MODIFY`, `ERASE`, `CONNECT`, and `DISCONNECT` logic.
- `currency`: Accurate tracking and reset of run-unit, record, set, and area currencies.
- `lock`: Shared/exclusive lock compatibility, reentrant locking, and deadlock cycle detection.
- `dc`: Task creation, priority scheduling, pseudo-conversational state, and queue/scratch operations.
- `storage`: CALC hash routine determinism, direct store, and VIA clustered page targeting.
- `recovery`: Journal record logging, checkpoint writing, and before-image rollback restoration.
- `sql_option`: SQL `SELECT`, `INSERT`, `UPDATE`, `DELETE` parsing and execution against CODASYL records.
- `precompiler`: Single-line and multi-line `EXEC IDMS` block translation into `CALL 'IDMS'` statements.

## Limitations

- **In-Memory Storage**: `PageManager` and `DmlEngine` operate on memory-backed structures rather than direct disk-based BDAM/VSAM dataset files.
- **Single-Threaded Task Scheduling**: `TaskScheduler` simulates multi-tasking sequentially without true OS thread concurrency.
- **SQL Optimization**: The SQL Option translates simple single-table filters; complex relational join queries over arbitrary network sets are not supported.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-ims](../open-mainframe-ims/README.md)
- [open-mainframe-db2](../open-mainframe-db2/README.md)
