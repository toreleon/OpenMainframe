# open-mainframe-syscmd

z/OS System Commands & SDSF — command dispatcher and SDSF (System Display and Search Facility) monitoring engine for the OpenMainframe project.

## Purpose

Operators and automated operations on z/OS interact with the operating system and subsystems via operator commands (`DISPLAY`, `START`, `STOP`, `MODIFY`, `CANCEL`, `REPLY`) and browse active workloads and spool files via SDSF panels. This crate implements the MVS system command parser, routing dispatcher, and an in-memory SDSF query and line-command engine.

## Capabilities

- **Command Dispatcher** (`commands`): Parses and executes standard MVS console commands:
  - `DISPLAY` (`D`): Display active address spaces (`D A,L` with optional jobname filtering), job details (`D J,jobname`), system date/time and IPL volume (`D T`), and virtual storage areas (`D M`).
  - `START` (`S`): Start an address space with optional `PARM=` parameters.
  - `STOP` (`P`): Request graceful address space shutdown.
  - `MODIFY` (`F`): Send subsystem or application-specific modify command strings.
  - `CANCEL` (`C`): Cancel an address space with optional dump request (`DUMP`).
  - `FORCE`: Forcibly terminate a stuck address space.
  - `REPLY` (`R`): Deliver replies to outstanding Write-to-Operator with Reply (WTOR) messages.
  - Custom command registration via `CommandRegistry`.
- **JES2 Command Routing**: Identifies and parses `$DA`, `$DJ`, `$SA`, `$PA`, `$CA`, `$TA` commands.
- **SDSF Engine** (`sdsf`): Data model and panel renderer for monitoring batch jobs, started tasks (STC), and TSO users (TSU):
  - Panels: `DA` (Display Active), `ST` (Status), `O` (Output), `H` (Held Output), and `LOG` (System Log).
  - Prefix filtering (`set_prefix`) and multi-column sorting (`set_sort` by Name, Time, Priority, Status).
  - Line commands: `S` (Browse SYSOUT), `SJ` (View JCL), `SE` (View JES messages), `SP` (Purge), `SB` (Browse), and `?` (Job Details).
  - REXX ISFEXEC interface (`isfexec()`): Returns panel columns in a structured key-value map compatible with REXX stem variables.

## Architecture

```
                       Operator Console / API Call
                                   │
                                   ▼
    ┌─────────────────────────────────────────────────────────────┐
    │                      parse_command()                        │
    │  - Normalizes verb (D, S, P, F, C, FORCE, R, $...)          │
    │  - Produces structured SystemCommand enum                   │
    └──────────────────────────────┬──────────────────────────────┘
                                   │
                                   ▼
    ┌─────────────────────────────────────────────────────────────┐
    │                     CommandDispatcher                       │
    │  - Custom handler lookup via CommandRegistry                │
    │  - Built-in command processing against SystemState          │
    └──────────────────────────────┬──────────────────────────────┘
                                   │
                                   ▼
    ┌─────────────────────────────────────────────────────────────┐
    │                        SystemState                          │
    │  - address_spaces (ASID, status, step, program, CPU)        │
    │  - wtors (outstanding WTOR entries & replies)               │
    │  - memory (REAL, AUX, CSA, SQA storage areas)               │
    │  - jes2_jobs (in-memory job definitions)                    │
    └─────────────────────────────────────────────────────────────┘
                                   ▲
                                   │ (Job & Spool Data)
    ┌──────────────────────────────┴──────────────────────────────┐
    │                         SdsfEngine                          │
    │  - Manages SdsfJob models and SysoutDataset entries         │
    │  - Renders DA, ST, O, H, LOG panels (RenderedPanel)         │
    │  - Executes LineCommand (Select, Purge, Details, etc.)      │
    │  - Provides isfexec() tabular export for REXX               │
    └─────────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `commands` | Command parser (`parse_command`), `CommandDispatcher`, `CommandRegistry`, and `SystemState` model |
| `sdsf` | `SdsfEngine`, panel definitions (`PanelType`, `RenderedPanel`), line commands (`LineCommand`), and `isfexec` API |

## Public API

### Primary Types and Functions

- `CommandDispatcher`: Main entry point for executing console command strings (`new()`, `execute()`, `dispatch()`, `add_address_space()`, `add_wtor()`).
- `SystemCommand`: Parsed command representation (`DisplayActive`, `DisplayJob`, `DisplayTime`, `DisplayMemory`, `Start`, `Stop`, `Modify`, `Cancel`, `Force`, `Reply`, `Jes2`, `Unknown`).
- `CommandOutput`: Command execution result containing message lines (`Vec<String>`), success flag, and return code (`ok()`, `error()`).
- `SystemState`: State container holding active address spaces (`AddressSpace`, `AsidStatus`), outstanding WTORs (`WtorEntry`), memory statistics (`MemoryInfo`, `StorageArea`), and system date/time (`SystemTime`).
- `CommandRegistry`: Extensible registry for attaching custom closures to command verbs.
- `SdsfEngine`: SDSF query and line command manager (`new()`, `add_job()`, `add_log()`, `set_prefix()`, `set_sort()`, `render_panel()`, `execute_line_command()`, `isfexec()`, `cancel_job()`, `purge_job()`).
- `SdsfJob`: Job metadata model (`jobname`, `jobid`, `job_type`, `owner`, `status`, `step_name`, `program`, `cpu_time`, `return_code`, `sysout_datasets`, `jcl`).
- `JobType` (`Job`, `Stc`, `Tsu`) / `JobStatus` (`Active`, `Input`, `Output`, `Complete`, `Canceled`, `Abended`).
- `PanelType`: Supported SDSF panels (`Da`, `St`, `Output`, `Held`, `Log`).
- `LineCommand`: Line command actions (`Select`, `SelectJcl`, `SelectMessages`, `Purge`, `Browse`, `Details`).
- `LineCommandResult`: Output and status of a line command execution.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`).
- **Consumers**: Standalone system operations library; models console and SDSF behavior for operator tooling and simulation harnesses.

## Examples

### Executing Console Commands

```rust
use open_mainframe_syscmd::{
    AddressSpace, AsidStatus, CommandDispatcher,
};

let mut dispatcher = CommandDispatcher::new();

// Populate an active address space
dispatcher.add_address_space(AddressSpace {
    jobname: "PAYROLL".to_string(),
    asid: 0x002A,
    status: AsidStatus::In,
    step_name: "STEP01".to_string(),
    program: "PAYPROG".to_string(),
    cpu_time: 1.25,
    initiator: Some("INIT1".to_string()),
});

let output = dispatcher.execute("DISPLAY A,L");
assert!(output.success);
assert!(output.messages[0].starts_with("IEE114I"));
```

### Querying SDSF Panels and Executing Line Commands

```rust
use open_mainframe_syscmd::sdsf::{
    JobStatus, JobType, LineCommand, PanelType, SdsfEngine, SdsfJob,
};

let mut engine = SdsfEngine::new();
engine.add_job(SdsfJob {
    jobname: "JOB001".to_string(),
    jobid: "JOB00001".to_string(),
    job_type: JobType::Job,
    owner: "IBMUSER".to_string(),
    status: JobStatus::Output,
    step_name: "STEP1".to_string(),
    program: "IEBGENER".to_string(),
    cpu_time: 0.45,
    return_code: Some(0),
    priority: 9,
    sysout_datasets: Vec::new(),
    jcl: "//JOB001 JOB ...".to_string(),
});

// Render the Status panel
let panel = engine.render_panel(PanelType::St);
assert_eq!(panel.rows.len(), 1);

// Execute the View JCL line command
let result = engine.execute_line_command("JOB001", LineCommand::SelectJcl);
assert!(result.success);
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-syscmd
```

The test suite contains 52 unit tests covering:
- Parsing of all `DISPLAY`, `START`, `STOP`, `MODIFY`, `CANCEL`, `FORCE`, `REPLY`, and JES2 command variants.
- IBM `IEE` message prefix formatting and return codes.
- SDSF panel rendering (`DA`, `ST`, `O`, `H`, `LOG`) with prefix filtering and multi-attribute sorting.
- SDSF line commands (`S`, `SJ`, `SE`, `SP`, `?`) and spool content extraction.
- REXX `ISFEXEC` tabular data export.

## Limitations

- **In-Memory State**: `CommandDispatcher` and `SdsfEngine` operate on in-memory Rust structures (`SystemState`, `Vec<SdsfJob>`) rather than controlling live OS processes or kernel address spaces.
- **JES2 Cross-Crate Decoupling**: JES2 commands (`$D`, `$S`, `$P`, etc.) parse command strings and query/modify `SystemState.jes2_jobs` directly in-memory, without cross-crate RPC or dynamic IPC to a live JES2 daemon.
- **Spool Files**: SDSF SYSOUT datasets are stored in memory strings (`SysoutDataset.content`) rather than on physical DASD spool volumes.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-jes2](../open-mainframe-jes2/README.md)
- [open-mainframe-mvs](../open-mainframe-mvs/README.md)
