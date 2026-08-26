# open-mainframe-ims

IMS (Information Management System) — hierarchical database (IMS/DB) and transaction manager (IMS/TM) runtime for the OpenMainframe project.

## Purpose

IBM IMS is a dual-subsystem environment composed of IMS Database Manager (IMS/DB), a hierarchical database system accessed through DL/I (Data Language/I) calls, and IMS Transaction Manager (IMS/TM), a high-throughput message queuing and transaction scheduling system. The `open-mainframe-ims` crate provides a complete Rust implementation of both IMS/DB and IMS/TM, including DBD/PSB definition parsing, DL/I hierarchical navigation and update, secondary indexing, GSAM sequential access, MFS (Message Format Service) terminal formatting, MPP/BMP region scheduling, conversational SPA transactions, and operator console command processing.

## Capabilities

- **Hierarchical Database Engine (IMS/DB)**:
  - Supports full DL/I database calls: `GU` (Get Unique), `GHU` (Get Hold Unique), `GN` (Get Next), `GHN` (Get Hold Next), `GNP` (Get Next within Parent), `GHNP` (Get Hold Next within Parent), `ISRT` (Insert), `REPL` (Replace), `DLET` (Delete), and `STAT` (Statistics).
  - Segment Search Arguments (`SSA`): Qualified and unqualified search arguments with boolean operators, comparison operators (`=`, `!=`, `<`, `<=`, `>`, `>=`), and command codes (`-`, `*`, `D`, `F`, `L`, `N`, `P`, `Q`, `U`, `V`).
  - Database Definitions (`dbd`): Hierarchical tree structure parsing (`DBD`, `SEGM`, `FIELD`, `LCHILD`, `XDFLD`) supporting hierarchical parent/child and logical relationships.
  - Program Specification Blocks (`psb`): `PSB`, `PCB`, and `SENSEG` definition parsing with processing options (`PROCOPT` = `G`, `I`, `R`, `D`, `A`, `L`, `P`).
  - Secondary Indexing (`PROCSEQ`): Traversal through secondary index target segments.
  - GSAM Sequential Access: Sequential file reading and writing through DL/I calls.
  - Transaction recovery: `CHKP` (Checkpoint), `XRST` (Extended Restart), `ROLB` (Rollback to last checkpoint), and `ROLS` (Rollback to named savepoint).
- **Transaction Manager (IMS/TM)**:
  - Region simulation (`regions`): Message Processing Programs (`MppRegion`), Batch Message Processing (`BmpRegion`), and Fast Path (`IfpRegion`).
  - Transaction scheduler (`TransactionScheduler`) with priority queues and transaction class filtering.
  - Conversational processing: Scratch Pad Area (`ScratchPadArea`) persistence across conversational turns.
  - Alternate PCBs (`AltPcb`): Program-to-program switching and logical terminal message output (`CHNG`, `ISRT`, `PURG`).
  - Open Transaction Manager Access (`otma`): Asynchronous message prefixes, transactional pipes (`Tpipe`), and XCF group mapping.
  - Fast Path (`fastpath`): Expedited Message Handler (`EmhQueue`) and Data Entry Database (`DedbIntegration`) simulation.
  - Multiple Systems Coupling (`msc`): Remote transaction routing and shared message queues.
- **Message Format Service (MFS)**:
  - Macro compilation (`mfs_parser`, `mfs_compiler`): Parses `FMT`, `DEV`, `DIV`, `DPAGE`, `DFLD`, `MSG`, `SEG`, `MFLD` definitions into compiled `DIF`, `DOF`, `MID`, `MOD` blocks.
  - Runtime message formatting (`mfs_runtime`): Converts raw 3270 input streams to application message formats (`format_input_message`) and formats output segments for device presentation (`format_output_message`).
- **Operator Command Processor (`operator`)**:
  - Full implementation of console commands: `/DISPLAY` (active regions, transactions, databases, queues), `/START` and `/STOP` (databases, transactions, regions), `/CHECKPOINT`, and `/ASSIGN`.
- **Status Codes (`StatusCode`)**:
  - Full classification of 30+ two-character DL/I status codes (`Ok`, `GA`, `GB`, `GD`, `GE`, `GK`, `GP`, `II`, `IX`, `DJ`, `DX`, `RX`, `AA`, `AB`, `AC`, `AD`, `AF`, `AH`, `AI`, `AJ`, `AK`, `AL`, `AM`, `AO`, `AP`, `BA`, `DA`, `FD`, `FR`, `NI`, `QC`, `QD`, `TG`, `XD`) across 6 categories (`Success`, `Informational`, `NotFound`, `ProgramError`, `SystemError`, `MessageQueue`).

## Architecture

```
                       COBOL Source / Terminal Input
                                     │
                 ┌───────────────────┴───────────────────┐
                 ▼                                       ▼
        ┌──────────────────┐                   ┌──────────────────┐
        │  EXEC DLI / MFS  │                   │   3270 / OTMA    │
        │  (Preprocessing) │                   │  Message Queues  │
        └────────┬─────────┘                   └────────┬─────────┘
                 │                                       │
                 ▼                                       ▼
        ┌─────────────────────────────────────────────────────────┐
        │                       ImsRuntime                        │
        │                                                         │
        │  ┌────────────────────┐         ┌────────────────────┐  │
        │  │     DL/I Engine    │         │ Transaction Manager│  │
        │  │ (GU/GN/ISRT/DLET)  │         │ (MPP / BMP / SPA)  │  │
        │  └─────────┬──────────┘         └─────────┬──────────┘  │
        │            │                              │             │
        │            ▼                              ▼             │
        │  ┌────────────────────┐         ┌────────────────────┐  │
        │  │ Hierarchical Store │         │ MFS Compiler / FMT │  │
        │  │ (DBD / PSB / Keys) │         │ (DIF/DOF/MID/MOD)  │  │
        │  └────────────────────┘         └────────────────────┘  │
        └─────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `dli` | DL/I call processor (`extract.rs`), Segment Search Argument parsing (`ssa.rs`), in-memory hierarchical store (`store.rs`) |
| `dbd` | Database Definition parser (`DatabaseDefinition`, `SegmentDef`, `FieldDef`, `LogicalChildDef`, `XdfldDef`) |
| `psb` | Program Specification Block parser (`ProgramSpecBlock`, `ProgramCommBlock`, `SensitiveSegment`) |
| `runtime` | Central IMS runtime coordinator (`ImsRuntime`, `GsamDataset`, `ImsMessage`, `RuntimeStats`) |
| `preprocess` | COBOL source preprocessor for `EXEC DLI` statements and CBLTDLI call generation |
| `schema` | Export of hierarchical DBD definitions to relational SQL DDL |
| `persist` | Snapshot state serialization and transactional state management |
| `tm` | Transaction manager: `AltPcb`, `ConversationalTransaction`, `ScratchPadArea`, `SavepointManager` |
| `regions` | Region execution and job dispatching: `MppRegion`, `BmpRegion`, `TransactionScheduler` |
| `operator` | Operator console command processing (`ImsCommandProcessor`, `/DISPLAY`, `/START`, `/STOP`, `/CHECKPOINT`) |
| `otma` | Open Transaction Manager Access: `Tpipe`, `XcfGroup`, asynchronous prefixes |
| `connect` | IMS Connect configuration, TCP/IP simulation, and connection pooling |
| `fastpath` | Fast Path data structures: DEDB integration and EMH priority queues |
| `msc` | Multiple Systems Coupling: shared message queues and remote links |
| `codegen` | Copybook and DL/I call wrapper generators (AIB, DIB, CBLTDLI) |
| `mfs_parser` | MFS macro syntax parser (`MfsParser`, `MfsStatement`, `MfsFmtDef`, `MfsMsgDef`) |
| `mfs_compiler` | MFS compiler generating runtime format blocks (`MfsCompiler`, `Dif`, `Dof`, `Mid`, `Mod`) |
| `mfs_runtime` | MFS 3270 message transformation engine (`format_input_message`, `format_output_message`) |

## Public API

### Primary Types and Functions

- `ImsRuntime`: Main execution engine managing registered DBDs, scheduled PSBs, GSAM datasets, message queues, and DL/I dispatch.
- `StatusCode`: DL/I status code enumeration (`StatusCode::Ok`, `StatusCode::GE`, `StatusCode::GB`, etc.) with helper methods `is_ok()`, `is_not_found()`, `is_error()`, `as_str_pair()`, and `category()`.
- `ImsCommandProcessor`: Executes operator commands:
  - `execute(&mut self, command: &str) -> Result<CommandResult, ImsError>`
- `MfsCompiler`: Compiles parsed MFS definitions into `FormatLibrary` containing DIF/DOF and MID/MOD blocks.
- `TransactionScheduler`: Manages message transaction queues and dispatches work to active `MppRegion` and `BmpRegion` instances.
- `ScratchPadArea`: Manages conversational transaction state between client interactions.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-encoding`: EBCDIC ↔ ASCII translation for 3270 MFS screen fields and data buffers.
- `thiserror`: Error derive macros.
- `serde` / `serde_json`: Persistence and configuration serialization.
- `postgres` (optional): Optional PostgreSQL persistence backend.

### Workspace Consumers

- `open-mainframe-assess`: Inspects COBOL source files for embedded DL/I and IMS migration dependencies.
- Root workspace member in `Cargo.toml`.

## Examples

### Executing Operator Commands

```rust
use open_mainframe_ims::operator::{ImsCommandProcessor, DisplayTarget};

fn main() {
    let mut processor = ImsCommandProcessor::new();
    let result = processor.execute("/DISPLAY TRANSACTION ALL").expect("command failed");
    println!("Operator response: {}", result.output);
    assert!(result.success);
}
```

### Checking DL/I Status Codes

```rust
use open_mainframe_ims::{StatusCode, StatusCategory};

fn main() {
    let status = StatusCode::from_chars('G', 'E');
    assert_eq!(status, StatusCode::GE);
    assert_eq!(status.category(), StatusCategory::NotFound);
    assert!(status.is_not_found());
    assert_eq!(status.description(), "Segment not found");
}
```

### Managing Conversational Scratch Pad Areas (SPA)

```rust
use open_mainframe_ims::tm::ScratchPadArea;

fn main() {
    let mut spa = ScratchPadArea::new("ORDTRANS", 100);
    spa.write(b"ORDER-ID-98765").expect("write failed");

    let data = spa.read();
    assert!(data.starts_with(b"ORDER-ID-98765"));
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-ims
```

The crate contains 372 unit tests verifying:
- `dli` & `runtime`: Hierarchical database traversal (`GU`, `GN`, `GNP`), segment insertion (`ISRT`), updates (`REPL`), deletions (`DLET`), GSAM sequential I/O, and status code setting.
- `status_codes`: Round-trip parsing and category validation for all 30+ DL/I status codes.
- `tm` & `regions`: Conversational SPA persistence, alternate PCB routing, MPP/BMP scheduling, and priority queue ordering.
- `mfs`: FMT/MSG macro parsing, compiler DIF/DOF generation, and runtime 3270 screen formatting.
- `operator`: `/DISPLAY`, `/START`, `/STOP`, and `/CHECKPOINT` command execution.
- `preprocess`: `EXEC DLI` scanning, tokenization, and CBLTDLI call generation.

## Limitations

- **In-Memory Segment Storage**: Hierarchical database records are held in an in-memory tree store (`HierarchicalStore`) rather than directly stored inside VSAM KSDS or ESDS dataset clusters on disk.
- **Simulated Networking**: `connect.rs` and `otma.rs` simulate message header framing (IRM/RSM) and connection pools in memory without binding TCP/IP network sockets.
- **Fast Path Disk Areas**: Fast Path DEDB support models buffer pools and EMH queues without direct disk area data set (ADS) multi-area block allocation.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-dataset](../open-mainframe-dataset/README.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
