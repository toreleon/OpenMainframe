# open-mainframe-jes2

A high-fidelity Rust implementation of **JES2 (Job Entry Subsystem 2)** for the OpenMainframe project. This crate provides the complete mainframe batch job lifecycle: internal reader ingestion, priority job queues, initiator dispatching, spool management, JECL control statements, and operator command processing.

## Purpose

JES2 is the primary work manager on IBM z/OS, receiving jobs, scheduling them for execution based on class and priority, managing spool storage for inputs and SYSOUT outputs, and coordinating job execution across initiators. `open-mainframe-jes2` models this subsystem within OpenMainframe:
1. **Input & Ingestion**: The `InternalReader` receives JCL streams, parses job cards and JES2 Control Language (`/*`) statements, and writes input datasets to the spool.
2. **Job Scheduling & Execution**: The job queue organizes submitted work across priority tiers (0–15) and execution classes, where class-aware `Initiator` processes dispatch and track active jobs.
3. **Spool & SYSOUT Management**: Stores job logs, JCL listings, and output datasets on simulated spool volumes, enabling browsing and purge reclamation.
4. **Operator Control**: Implements standard JES2 `$` operator commands (`$D`, `$S`, `$P`, `$C`, `$A`, `$H`, `$T`) for managing jobs, queues, spool volumes, and initiators.

## Capabilities

- **Job Lifecycle State Machine**:
  - `Job` transitions through discrete states: `Input` -> `Conversion` -> `AwaitingExecution` -> `Executing` -> `AwaitingOutput` -> `Purged`.
  - Supports hold/release toggles on input jobs (`$H J` / `$A J`) and output datasets.
- **Internal Reader (`InternalReader`)**:
  - Ingests card-image streams, parses job names, classes, priority, and assigns unique `JobId` identifiers.
  - Recognizes JES2 Control Language (JECL) statements: `/*JOBPARM`, `/*OUTPUT`, `/*ROUTE`, `/*SETUP`, `/*MESSAGE`.
- **Initiator Scheduling (`InitiatorManager`)**:
  - Manages batch initiators configured with multi-class execution lists (e.g., `CLASS=ABC`).
  - Implements priority-weighted FIFO job selection within class hierarchies with anti-starvation priority aging.
- **Spool Storage (`SpoolManager`)**:
  - Multi-track buffer allocation for storing JCL streams and SYSOUT output datasets by DD name.
  - Random-access record retrieval by MTTR (Module-Track-Track-Record) addressing for fast spool browsing.
- **Operator Command Processing**:
  - `parse_command` and `execute_command` handle operator directives:
    - `$D J[OB]` / `$D A` / `$D I` / `$D S` / `$D Q` — Display jobs, active work, initiators, spool status, and queues.
    - `$S I` / `$P I` — Start and stop initiators.
    - `$C J` — Cancel running or queued jobs.
    - `$A J` / `$H J` — Release or hold jobs and output.
    - `$T I` / `$T J` — Modify initiator class lists or job priority/class.
- **Exit Framework**:
  - Pluggable exit points at critical lifecycle stages (input validation, JCL conversion, job initiation, output routing).
- **Checkpointing**:
  - State preservation and recovery supporting Warm and Cold start scenarios.

## Architecture

```text
    Job Submission                         JES2 Job Lifecycle
    ┌──────────────┐                      ┌────────────────────┐
    │  Internal    │    Input             │    Job Queue       │
    │  Reader      │ ──────────────────>  │    (Priority)      │
    └──────────────┘    InternalReader    │  Active, Output    │
           │                              └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Spooling          ┌────────────────────┐
    │  Spool       │ ──────────────────>  │   Initiator        │
    │  Volumes     │    SpoolManager      │   (Execution)      │
    └──────────────┘                      │ Class, Priority    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Output            ┌────────────────────┐
    │  SYSOUT      │ <──────────────────  │  Output Processor  │
    │  Datasets    │    OutputDescriptor  │  Print, Purge      │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `intrdr` | Internal Reader: Card-image ingestion, job identification, and submission pipeline. |
| `job` | Job model: `Job`, `JobId`, `JobStatus`, `JobType`, accounting fields, and lifecycle transitions. |
| `queue` | Job scheduling: Priority queues (0–15), class matching, and queue scanning. |
| `initiator`| Initiator manager: `Initiator`, `InitiatorManager`, class lists, and active job dispatch. |
| `spool` | Spool management: `SpoolManager`, volume allocation, track addressing, and dataset storage. |
| `output` | SYSOUT processing: `OutputGroup`, `OutputDescriptor`, class routing, and purge management. |
| `commands` | Operator command interface: `$D`, `$S`, `$P`, `$C`, `$A`, `$H`, `$T` parser and execution engine. |
| `jecl` | JES2 Control Language: Parser for `/*JOBPARM`, `/*OUTPUT`, `/*ROUTE`, `/*SETUP`, `/*MESSAGE`. |
| `exit` | Subsystem exit framework: Dynamic hooks across 20+ job processing milestones. |
| `config` | Subsystem configuration: `Jes2Config`, spool dataset paths, classes, and limits. |
| `checkpoint`| Persistence engine: Checkpoint record serialization for warm/cold start recovery. |

## Public API

### Core Types and Services

```rust
use open_mainframe_jes2::{
    Jes2, Job, JobId, JobStatus,
    intrdr::InternalReader,
    initiator::InitiatorManager,
    commands::{parse_command, execute_command, Jes2Command, CommandResponse},
    spool::SpoolManager,
};
```

- `Jes2`: Central subsystem controller maintaining the job registry, queue, spool, and configuration.
- `InternalReader`: Ingests JCL lines, extracts job metadata, and submits work into the JES2 queue.
- `InitiatorManager`: Manages concurrent initiators, selecting queued jobs based on class priority.
- `parse_command` / `execute_command`: Parses and executes JES2 `$` operator commands.

## Integration

### Workspace Dependencies

- None (core library using standard Rust libraries and workspace logging/error crates: `miette`, `thiserror`, `serde`, `tracing`).

### Known Consumers

- [`open-mainframe-tso`](../open-mainframe-tso/README.md) — Uses `InternalReader` and `Jes2` for the TSO `SUBMIT`, `STATUS`, and `CANCEL` commands.
- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Powers `/zosmf/restjobs/jobs` REST endpoints for job submission, status queries, and SYSOUT retrieval.

## Examples

### Submitting a Job via Internal Reader

```rust
use open_mainframe_jes2::Jes2;
use open_mainframe_jes2::intrdr::InternalReader;

let mut jes = Jes2::new();

let mut reader = InternalReader::new();
reader.write_lines(&[
    "//PAYROLL  JOB (ACCT123),'MONTHLY PAY',CLASS=A,MSGCLASS=X",
    "//STEP01   EXEC PGM=PAYCALC",
    "//SYSPRINT DD SYSOUT=*",
]);

// Submit with parent address space ID 1
let job_id = reader.submit(1, &mut jes).expect("Job submission failed");
assert_eq!(job_id.as_u32(), 1);

let job = jes.get_job(job_id).expect("Job not found");
assert_eq!(job.name, "PAYROLL");
assert_eq!(job.class, 'A');
```

### Executing Operator Commands

```rust
use open_mainframe_jes2::Jes2;
use open_mainframe_jes2::initiator::InitiatorManager;
use open_mainframe_jes2::commands::{parse_command, execute_command};

let mut jes = Jes2::new();
let mut initiators = InitiatorManager::new();

// Display all jobs in the queue
if let Some(cmd) = parse_command("$D J") {
    let response = execute_command(&mut jes, &mut initiators, &cmd);
    println!("JES2 Response: {}", response.message);
}

// Modify an initiator to handle classes A and B
if let Some(cmd) = parse_command("$T I1,C=AB") {
    let response = execute_command(&mut jes, &mut initiators, &cmd);
    assert!(response.success);
}
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-jes2
```

The test suite covers:
- **`intrdr::*`**: Card-image ingestion, job statement parsing, continuation handling, JECL parsing (`/*JOBPARM`), and invalid JCL error states.
- **`queue::*`**: Priority ordering (0–15), multi-class matching, FIFO queue preservation, and hold/release queue filtering.
- **`initiator::*`**: Multi-initiator workload selection, class-list matching, initiator draining (`$P I`), and execution state transitions.
- **`spool::*`**: Spool allocation, SYSOUT dataset creation, record appending, and MTTR addressing.
- **`commands::*`**: Command syntax parser for `$D`, `$S`, `$P`, `$C`, `$A`, `$H`, `$T`, wildcard job filters, and error formatting.
- **`checkpoint::*`**: Subsystem state serialization and warm start restoration.

## Limitations

- **Single-Node Execution**: Multi-Access Spool (MAS) clustering and Network Job Entry (NJE) cross-system node routing are not implemented.
- **Security Interception**: RACF `JESSPOOL` resource authorization hooks exist in the exit pipeline but default to permitted when RACF is unconfigured.
- **JCL Converter / Interpreter**: The internal reader extracts job headers and JECL; step-level JCL parsing (EXEC, DD) is delegated to downstream step processors.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [TSO/E Command Processor (`open-mainframe-tso`)](../open-mainframe-tso/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
