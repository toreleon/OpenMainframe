# open-mainframe-cics

A comprehensive Rust implementation of **IBM CICS (Customer Information Control System)** for the OpenMainframe project — providing the transaction-processing runtime, EXEC CICS command preprocessing, BMS screen mapping with 3270 data stream rendering, queue management, channel/container inter-program data passing, interval control, and system programming services.

## Purpose

CICS is the dominant online transaction processing (OLTP) system on IBM mainframes, managing thousands of concurrent terminal users executing business transactions. `open-mainframe-cics` models the full CICS application environment within OpenMainframe:
1. **Source Preprocessing**: Scans COBOL source for `EXEC CICS ... END-EXEC` blocks and replaces them with `CALL` statements referencing structured command parameter blocks.
2. **Runtime Services**: Routes calls to a central `CicsRuntime` executing program control (LINK/XCTL/RETURN), file control, queues, timers, and inter-program data sharing.
3. **BMS & 3270 Presentation**: Compiles Basic Mapping Support (BMS) macro source definitions into structured maps, renders them to 3270 data streams, and generates COBOL symbolic copybooks.

## Capabilities

- **EXEC CICS Preprocessing**:
  - `CicsScanner` scans fixed-format COBOL source (columns 8–72), respecting column-7 comment indicators (`*`).
  - `CicsPreprocessor` parses 37 command types with nested option parenthesis extraction, generating `CALL "CICSxxxx" USING CICS-CMD-nnn DFHEIBLK` invocations in reverse order to preserve source line mapping.
- **Program Control & Data Passing**:
  - `LINK`, `XCTL`, and `RETURN` (with `TRANSID`, `COMMAREA`, or `CHANNEL`).
  - Dual communication models: legacy `COMMAREA` (up to 32 KB) and modern `Channel` / `Container` infrastructure (unlimited size, case-insensitive container names).
- **Basic Mapping Support (BMS)**:
  - Parses `DFHMSD`, `DFHMDI`, and `DFHMDF` macros with continuation support and multi-value `ATTRB` lists (PROT, UNPROT, NUM, BRT, DRK, ASKIP, IC, FSET).
  - 3270 data stream generation: SBA address calculation (12-bit and 14-bit), SF, SFE (extended color and highlight attribute pairs), WCC (keyboard restore, alarm, MDT reset), and EBCDIC CP037 character translation.
  - Generates COBOL symbolic copybooks with standard 12-byte `TIOAPFX` and length/flag/attribute/color field structures, including `decompose_from_buffer` and `compose_to_display_string`.
  - Supports 3270 display sizes: Model 2 (24x80), Model 3 (32x80), Model 4 (43x80), Model 5 (27x132).
- **Queues (TS and TD)**:
  - **Temporary Storage (TS)**: Main (in-memory) and Auxiliary (disk-persisted JSON) storage, supporting indexed `READQ TS`, sequential `READQ TS NEXT`, rewrite, and `DELETEQ TS`.
  - **Transient Data (TD)**: FIFO destructive read queues with Destination Control Table (DCT) definitions, trigger thresholds triggering asynchronous transids, and extrapartition file flushing.
- **Terminal Control**:
  - Multi-terminal session manager supporting `SEND MAP`, `RECEIVE MAP`, `SEND TEXT`, `SEND PAGE` with `ACCUM` page building, `CONVERSE`, and `PURGE MESSAGE`.
- **System & Support Services**:
  - **Interval Control**: `START`, `CANCEL`, `DELAY`, and `RETRIEVE` with HHMMSS calculation.
  - **Time Services**: `ASKTIME` and `FORMATTIME` with standard formatting options.
  - **Synchronization**: `ENQ` and `DEQ` resource locking.
  - **System Programming Interface (SPI)**: `INQUIRE` and `SET` for programs, transactions, and files (including `NEWCOPY`).
  - **Document & Web Services**: Template registration, symbol substitution, URIMAP pattern matching, and JSON pipeline transforms.

## Architecture

```text
    COBOL Source                         Runtime Execution
    ┌─────────────┐                      ┌──────────────────┐
    │ EXEC CICS   │    Preprocessing     │   CicsRuntime    │
    │ LINK ...    │ ──────────────────>  │                  │
    │ END-EXEC    │    CicsPreprocessor  │ ┌──────────────┐ │
    └─────────────┘    CicsScanner       │ │ProgramControl│ │
           │                             │ │LINK/XCTL/RTN │ │
           ▼                             │ └──────────────┘ │
    ┌─────────────┐    Dispatch          │ ┌──────────────┐ │
    │ CALL        │ ──────────────────>  │ │ FileControl   │ │
    │ "CICSLINK"  │    CicsDispatcher    │ │READ/WRITE/DEL│ │
    │ USING ...   │                      │ └──────────────┘ │
    └─────────────┘                      │ ┌──────────────┐ │
                                         │ │ Terminal I/O  │ │
    ┌───────────────────────────┐        │ │SEND/RECV MAP │ │
    │        BMS Subsystem      │        │ └──────────────┘ │
    │ Parser → Map → Renderer   │ <───>  │ ┌──────────────┐ │
    │ SymbolicMapGenerator      │        │ │ Queue Svcs    │ │
    │ FROM/INTO buffer decompose│        │ │ TS / TD      │ │
    └───────────────────────────┘        │ └──────────────┘ │
                                         │ ┌──────────────┐ │
    ┌───────────────────────────┐        │ │ Channels      │ │
    │      Support Services     │        │ │ PUT/GET/MOVE  │ │
    │ Document / Interval / SPI │        │ └──────────────┘ │
    │ Sync / Time / Web / ENQ   │        │ ┌──────────────┐ │
    └───────────────────────────┘        │ │ Web / SPI    │ │
                                         │ └──────────────┘ │
                                         └──────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `lib` | Crate root defining `CicsError`, `CicsResponse` (50+ condition codes), and `CicsResult<T>`. |
| `preprocess` | COBOL preprocessor: `CicsPreprocessor`, `CicsScanner`, `CicsBlock`, `CicsCommandType`, and `CicsOption`. |
| `runtime::commands` | Execution runtime: `CicsRuntime`, `ProgramRegistry`, `ProgramResult`, `Commarea`, and core command methods. |
| `runtime::dispatcher` | Bridge mapping `CALL "CICSxxxx"` invocations and `CommandParamBlock` payloads to `CicsRuntime`. |
| `runtime::eib` | EXEC Interface Block (`Eib`) maintaining `EIBRESP`, `EIBRESP2`, `EIBFN`, `EIBTRNID`, and execution metadata. |
| `runtime::files` | In-memory and dataset-backed file control (`FileManager`, `FileRecord`, `FileStatus`). |
| `bms::parser` | BMS macro parser: `BmsParser`, `BmsMapset`, `BmsMap`, `BmsField` from DFHMSD/DFHMDI/DFHMDF source. |
| `bms::field` | BMS field definitions: `BmsField`, `FieldAttribute`, `FieldType`, and buffer position calculation. |
| `bms::render` | 3270 data stream renderer: `MapRenderer`, `Wcc`, SBA/SF/SFE orders, and CP037 EBCDIC conversion. |
| `bms::symbolic` | COBOL copybook generator: `SymbolicMapGenerator`, `decompose_from_buffer`, `compose_to_display_string`. |
| `bms` (mod) | Screen models (`ScreenSize::Model2` through `Model5`), `AttributeByte`, colors, and highlight types. |
| `channels` | Channel and container manager: `Channel`, `Container`, `ChannelManager` for large inter-program payloads. |
| `queues::ts` | Temporary Storage queues: `TsQueue`, `TsQueueManager`, `TsItem`, and auxiliary disk persistence. |
| `queues::td` | Transient Data queues: `TdQueue`, `TdQueueManager`, `DctEntry`, `TdDestType`, triggers, and file export. |
| `terminal` | 3270 terminal handling: `Terminal`, `TerminalManager`, `ScreenBuffer`, `SendMapOptions`, and page accumulation. |
| `interval` | Interval control: `IntervalManager`, `ScheduledTransaction`, `START`, `CANCEL`, `DELAY`, `RETRIEVE`. |
| `time` | Mainframe time services: `TimeManager`, `Abstime`, `FormatTimeOptions`, `days_to_ymd`. |
| `sync` | Resource locking and synchronization: `SyncManager`, `EnqResource`, `LockState`. |
| `syspr` | System Programming Interface: `SystemProgrammingInterface`, `ProgramDef`, `TransactionDef`, `FileDef`. |
| `document` | Document services: `Document`, `DocumentManager`, template insertion, symbol substitution. |
| `web` | Web services client: `WebClient`, `WebSession`, `WebRequest`, `WebResponse`, `UriRouter`, `Pipeline`. |

## Public API

### Core Types and Error Handling

```rust
use open_mainframe_cics::{CicsError, CicsResponse, CicsResult};

let resp = CicsResponse::Normal;
assert_eq!(resp.condition_name(), "NORMAL");
```

### Preprocessor and Dispatcher

- `CicsPreprocessor`: Transforms raw COBOL source code containing `EXEC CICS` blocks into `CALL` statements.
- `CicsDispatcher`: Bridges CALL parameter blocks (`CommandParamBlock`) to runtime handler execution.

### Runtime and Program Management

- `CicsRuntime`: Main execution engine managing transaction state, EIB, queues, files, channels, and terminals.
- `ProgramRegistry`: Registers and looks up executable program entrypoints (`ProgramResult`).

## Integration

### Workspace Dependencies

- [`open-mainframe-encoding`](../open-mainframe-encoding/README.md) — EBCDIC CP037 character set encoding for 3270 byte streams.
- [`open-mainframe-dataset`](../open-mainframe-dataset/README.md) — Underlying dataset structures.

### Known Consumers

- [`open-mainframe-precompilers`](../open-mainframe-precompilers/README.md) — Integrates CICS preprocessing into the compilation toolchain.
- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Implements `/zosmf/cicsApp/terminal` REST session runners.
- [`open-mainframe-tui`](../open-mainframe-tui/README.md) — Interactive terminal UI for CICS 3270 screens.
- [`open-mainframe`](../open-mainframe/README.md) — CLI runner for headless and interactive CICS execution.
- [`open-mainframe-wiki`](../open-mainframe-wiki/README.md) — Automated documentation and syntax diagrams.

## Examples

### Preprocessing EXEC CICS Statements

```rust
use open_mainframe_cics::preprocess::CicsPreprocessor;

let cobol_source = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           EXEC CICS
               SEND TEXT FROM('HELLO WORLD') ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
"#;

let mut preprocessor = CicsPreprocessor::new();
let result = preprocessor.process(cobol_source).unwrap();
assert!(result.cobol_source.contains("CALL \"CICSTEXT\""));
```

### Running CICS Programs with Channels and Containers

```rust
use open_mainframe_cics::runtime::commands::{CicsRuntime, ProgramResult};

let mut runtime = CicsRuntime::new("TRN1");

runtime.register_program("SUBPGM", |rt| {
    let payload = rt.get_container("MY-CHANNEL", "INPUT-DATA")?;
    assert_eq!(payload, b"HELLO FROM CALLER");
    rt.put_container("MY-CHANNEL", "OUTPUT-DATA", b"PROCESSED")?;
    Ok(ProgramResult::Return)
});

runtime.put_container("MY-CHANNEL", "INPUT-DATA", b"HELLO FROM CALLER").unwrap();
runtime.link_with_channel("SUBPGM", "MY-CHANNEL").unwrap();

let response = runtime.get_container("MY-CHANNEL", "OUTPUT-DATA").unwrap();
assert_eq!(response, b"PROCESSED");
```

### Parsing BMS Mapsets and Rendering 3270 Streams

```rust
use open_mainframe_cics::bms::parser::BmsParser;
use open_mainframe_cics::bms::render::MapRenderer;
use open_mainframe_cics::bms::{ScreenSize, SymbolicMapGenerator};

let bms_source = r#"
MAPSET   DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,STORAGE=AUTO
MAP1     DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,1),LENGTH=20,INITIAL='ACCOUNT INQUIRY',ATTRB=PROT
ACCTNO   DFHMDF POS=(3,1),LENGTH=8,ATTRB=(UNPROT,IC),COLOR=TURQUOISE
         DFHMSD TYPE=FINAL
"#;

let mut parser = BmsParser::new();
let mapset = parser.parse(bms_source).unwrap();
let map = &mapset.maps[0];

let mut renderer = MapRenderer::new(ScreenSize::Model2);
renderer.set_field_string("ACCTNO", "12345678");
let stream = renderer.render(map, true);

let copybook = SymbolicMapGenerator::new().generate(&mapset);
assert!(copybook.contains("01  MAP1I."));
```

### Transient Data Queues with DCT Trigger

```rust
use open_mainframe_cics::queues::td::{DctEntry, TdQueueManager};

let mut td_mgr = TdQueueManager::new();
let entries = vec![
    DctEntry::intra("LOGQ").with_trigger(3, "LOGP"),
];
td_mgr.load_dct(&entries);

td_mgr.writeq("LOGQ", b"Record 1".to_vec()).unwrap();
td_mgr.writeq("LOGQ", b"Record 2".to_vec()).unwrap();
assert!(td_mgr.get_pending_triggers().is_empty());

td_mgr.writeq("LOGQ", b"Record 3".to_vec()).unwrap();
let triggers = td_mgr.get_pending_triggers();
assert_eq!(triggers, vec!["LOGP"]);
```

## Testing

Run the full test suite:

```bash
cargo test -p open-mainframe-cics
```

The test suite covers:
- **`bms::*`**: Mapset macro parsing, continuation handling, attribute byte calculation, 3270 stream SBA/SF/SFE generation, EBCDIC CP037 roundtrips, symbolic copybook generation, and buffer decomposition.
- **`preprocess::*`**: 37 command types, option nesting, multiline command extraction, line-number preservation.
- **`runtime::commands`**: LINK/XCTL/RETURN nesting, COMMAREA and channel passing, abend recovery, storage allocation (`GETMAIN`/`FREEMAIN`).
- **`queues::*`**: TS Main/Auxiliary persistence, browse cursor indexing, TD trigger thresholds, and extrapartition file output.
- **`terminal::*`**: Page accumulation (`ACCUM`), `CONVERSE`, multi-terminal routing.

## Limitations

- **VSAM Integration**: While runtime file operations (`READ`, `WRITE`, `REWRITE`, `DELETE`) operate correctly through `FileManager`, full VSAM key-sequenced dataset indexing in execution bridges relies on the external `open-mainframe-dataset` catalog.
- **Quasi-Reentrancy**: Each `CicsRuntime` session runs synchronously on a dedicated thread; multi-tasking is achieved via session thread pools rather than internal quasi-reentrant task slicing.
- **Attention Identifiers (AID)**: `HANDLE AID` commands are parsed, but automatic runtime paragraph branching requires orchestration by the calling interpreter.
- **Web Services**: Outbound `WEB OPEN`/`WEB CONVERSE` manages session state and headers but relies on simulated backend responses.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [EBCDIC Encoding Engine (`open-mainframe-encoding`)](../open-mainframe-encoding/README.md)
- [Dataset Storage Engine (`open-mainframe-dataset`)](../open-mainframe-dataset/README.md)
- [z/OSMF REST Server (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
- [Interactive 3270 Terminal UI (`open-mainframe-tui`)](../open-mainframe-tui/README.md)
