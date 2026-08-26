# open-mainframe-mvs

The foundational **MVS (Multiple Virtual Storage)** System Services for the OpenMainframe project — providing core z/OS supervisor services (SVCs): dynamic allocation (SVC 99 / DYNALLOC), operator console services (WTO/WTOR/DOM), resource serialization (ENQ/DEQ/ECB), subpool storage management (GETMAIN/FREEMAIN), task management (TCB/ABEND), timer services, and recovery management (ESTAE/SDWA).

## Purpose

MVS System Services constitute the foundational kernel services of IBM z/OS, handling task dispatching, system recovery, dataset allocation, memory subpools, operator messaging, and resource serialization. `open-mainframe-mvs` models this core operating environment within OpenMainframe:
1. **Dynamic Allocation (SVC 99)**: Implements text-unit based dataset and DD allocation, deallocation, concatenation, and information retrieval.
2. **Operator Console (SVC 34/35)**: Manages WTO (Write to Operator) and WTOR (Write to Operator with Reply) message dispatching and DOM (Delete Operator Message) action clearing.
3. **Task & Process Management**: Manages Task Control Block (`Tcb`) hierarchy (mother, daughter, sister pointers), ATTACH subtask spawning, and ABEND condition percolation.
4. **Synchronization & Storage**: Provides ECB (Event Control Block) WAIT/POST event coordination, ENQ/DEQ major/minor resource locking, and subpool-isolated GETMAIN/FREEMAIN heap management.

## Capabilities

- **Dynamic Allocation (`DynallocEngine`, `DdTable`)**:
  - Processes SVC 99 verbs: `Allocate`, `Unallocate`, `Concatenate`, `Deconcatenate`, `InfoRetrieval`.
  - Full support for Text Unit keys: `DalDsnam`, `DalDdnam`, `DalStats` (`Shr`, `Old`, `New`, `Mod`), `DalNdisp`, `DalRtddn`, `DalRtdsn`.
  - Multi-dataset DD concatenation supporting ordered search paths.
- **Console Services (`Console`, `wto`, `wtor`, `dom`)**:
  - Asynchronous message distribution with `RoutingCode` (Master, Operator Info, etc.) and `DescriptorCode` (Immediate Action, Job Status, Informational).
  - Multi-line WTO messages and WTOR reply tokens synchronized with ECBs.
- **Synchronization & Serialization (`sync`)**:
  - **ECB (`Ecb`)**: Event completion signaling supporting single-event and multi-event WAIT/POST operations.
  - **ENQ/DEQ (`EnqManager`)**: Multi-task resource serialization by Major (QNAME) and Minor (RNAME) names with `STEP` and `SYSTEM` scopes, shared (SHR) or exclusive (EXCL) access.
- **Storage Subpool Management (`storage`)**:
  - Emulates MVS subpools (0–255) with ownership tracking and automatic subpool 0 reclamation on task termination.
- **Task Hierarchy & Recovery (`task`, `recovery`)**:
  - `Tcb` tree management supporting ATTACH subtask dispatching and priority scheduling.
  - `EstaeManager` recovery exit stack with SDWA (System Diagnostic Work Area) recording, retry routines, and ABEND percolation.
- **Timer Services (`timer`)**:
  - SVC 11 TIME formatting supporting `DEC`, `BIN`, `MIC`, and `STCK` timestamp structures.
- **Program Management (`program`)**:
  - Module search, loading, parameter passing, and stack frame lifecycle: `LINK`, `XCTL`, `LOAD`, `DELETE`, `ATTACH`, `DETACH`.

## Architecture

```text
    ┌─────────────────────────────────────────────────────────────┐
    │                    MVS System Services (SVCs)               │
    │                                                             │
    │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
    │  │   DYNALLOC   │  │   Task Mgmt   │  │   Storage Mgmt   │ │
    │  │   (SVC 99)   │  │   (TCB/ASID)  │  │   (Subpools)     │ │
    │  └──────┬───────┘  └───────┬───────┘  └────────┬─────────┘ │
    └─────────┼──────────────────┼───────────────────┼───────────┘
              │                  │                   │
    ┌─────────▼──────────────────▼───────────────────▼───────────┐
    │                  MVS Kernel Dispatcher                     │
    │        (SVC 1, 2, 4, 5, 6, 7, 11, 13, 34, 35, 42, 48, 56)  │
    └─────────┬──────────────────┬───────────────────┬───────────┘
              │                  │                   │
    ┌─────────▼──────┐  ┌────────▼───────┐  ┌────────▼─────────┐
    │ Program Manager│  │ Sync & Locking │  │ Recovery (ESTAE) │
    │ (LINK/LOAD)    │  │ (ENQ/DEQ/ECB)  │  │ (SDWA/ABEND)     │
    └────────────────┘  └────────────────┘  └──────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `dynalloc` | SVC 99 engine: `DynallocEngine`, `DdTable`, `DynallocRequest`, `DynallocResponse`, `TextUnit`. |
| `console` | Console I/O: `Console`, `ConsoleMessage`, `wto`, `wtor`, `dom`, `ReplyManager`. |
| `sync` | Synchronization: `Ecb`, `EnqManager`, `EnqResource`, `Scope`, `LockType`. |
| `storage` | Storage management: `SubpoolManager`, `GetmainMode`, `SubpoolEntry`. |
| `task` | Task management: `Tcb`, `ProcessManager`, `AbendCode`, ATTACH dispatcher. |
| `recovery` | Error recovery: `EstaeManager`, `Sdwa`, ESTAE exit registration, retry logic. |
| `timer` | Time services: `TimeService`, SVC 11 TIME formats (DEC, BIN, MIC, STCK). |
| `program` | Program control: `ProgramManager`, module resolution, LINK, XCTL, LOAD, DELETE. |
| `error` | Subsystem error types: `MvsError`, `Result`. |

## Public API

### Core Types and Services

```rust
use open_mainframe_mvs::{
    dynalloc::{DynallocEngine, DdTable, DynallocRequest, DynallocVerb, TextUnit, TextUnitKey, DatasetStatus, Disposition},
    console::{Console, ConsoleMessage, RoutingCode, DescriptorCode, wto, wtor, dom},
    sync::{Ecb, EnqManager, Scope},
    task::{Tcb, ProcessManager},
    recovery::{EstaeManager, Sdwa},
    storage::SubpoolManager,
    timer::TimeService,
    program::ProgramManager,
    MvsError, Result,
};
```

- `DynallocEngine`: Executes SVC 99 allocation and concatenation requests against an active `DdTable`.
- `Console`: Asynchronous operator message dispatcher for WTO and WTOR requests.
- `Ecb` / `EnqManager`: Primitives for task synchronization and named resource locking.
- `Tcb`: Represents an individual Task Control Block within the process hierarchy.

## Integration

### Workspace Dependencies

- [`open-mainframe-dataset`](../open-mainframe-dataset/README.md) — Dataset catalog and storage structures.
- [`open-mainframe-racf`](../open-mainframe-racf/README.md) — Security authorization checks during dataset allocation.

### Known Consumers

- Used as the foundational supervisor layer for batch jobs, TSO command processors, and CICS runtime storage across OpenMainframe.

## Examples

### Performing a Dynamic Allocation (SVC 99)

```rust
use open_mainframe_mvs::dynalloc::{
    DynallocEngine, DdTable, DynallocRequest, DynallocVerb,
    TextUnit, TextUnitKey, DatasetStatus,
};
use std::sync::Arc;
use tokio::sync::RwLock;

#[tokio::main]
async fn main() {
    let dd_table = Arc::new(RwLock::new(DdTable::new()));
    let engine = DynallocEngine::new(dd_table.clone());

    // Allocate DD SYSUT1 to dataset USER.PROD.DATA
    let request = DynallocRequest {
        verb: DynallocVerb::Allocate,
        flags: 0,
        text_units: vec![
            TextUnit::string(TextUnitKey::DalDsnam, "USER.PROD.DATA"),
            TextUnit::string(TextUnitKey::DalDdnam, "SYSUT1"),
            TextUnit::byte(TextUnitKey::DalStats, DatasetStatus::Shr.to_byte()),
        ],
    };

    let response = engine.execute(&request).await.unwrap();
    assert!(response.is_success());

    // Verify DD table mapping
    let table = dd_table.read().await;
    let entry = table.lookup("SYSUT1").unwrap();
    assert_eq!(entry.dsname, "USER.PROD.DATA");
}
```

### Issuing a WTO (Write to Operator)

```rust
use open_mainframe_mvs::console::{Console, wto, RoutingCode, DescriptorCode};

#[tokio::main]
async fn main() {
    let mut console = Console::new(16);
    let sender = console.sender();

    let msg_id = wto(
        &sender,
        "IEF142I JOB01 - STEP WAS EXECUTED - COND CODE 0000",
        RoutingCode::OPERATOR_INFO,
        DescriptorCode::JOB_STATUS,
    ).await;

    console.process_pending().await;
    assert_eq!(console.messages()[0].id, msg_id);
    assert_eq!(console.messages()[0].text, "IEF142I JOB01 - STEP WAS EXECUTED - COND CODE 0000");
}
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-mvs
```

The test suite covers:
- **`dynalloc::*`**: Text unit serialization, dynamic allocation, unallocation, DD concatenation, deconcatenation, and info retrieval.
- **`console::*`**: Single-line and multi-line WTO delivery, WTOR reply tokens, DOM action message clearing, and routing code masks.
- **`sync::*`**: ECB single/multi-wait posting, ENQ exclusive/shared lock contention, and STEP/SYSTEM scoping.
- **`task::*`**: TCB tree relationships (mother/daughter/sister), ATTACH subtask spawning, and ABEND propagation.
- **`recovery::*`**: ESTAE exit stack unwinding, SDWA error recording, and retry intercept.
- **`storage::*`**: Subpool GETMAIN/FREEMAIN isolation and termination cleanup.

## Limitations

- **Hardware Storage Keys**: Memory protection is enforced via Rust ownership and logical isolation rather than CPU hardware storage protection keys (0–15).
- **SVC Interrupt Mechanism**: Supervisor calls execute as asynchronous Rust functions rather than real hardware CPU SVC interrupt vectors.
- **Process Memory Space**: Emulated tasks execute within the host process memory space rather than hardware-isolated page tables.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [Dataset Subsystem (`open-mainframe-dataset`)](../open-mainframe-dataset/README.md)
- [RACF Security Subsystem (`open-mainframe-racf`)](../open-mainframe-racf/README.md)
- [UNIX System Services Subsystem (`open-mainframe-uss`)](../open-mainframe-uss/README.md)
