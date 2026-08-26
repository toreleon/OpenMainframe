# open-mainframe-utilities

Standard z/OS utility programs and service aids for the OpenMainframe project — providing core dataset manipulation (IEBCOPY, IEBGENER, IEBUPDTE, IEBCOMPR, IEBPTPCH), test data generation (IEBDG), system catalog and volume utilities (IEHPROGM, IEHLIST, IEHMOVE), patching facilities (AMASPZAP), and batch invocation interfaces (IKJEFT01, IRXJCL, BPXBATCH).

## Purpose

Mainframe batch workloads and systems operations rely heavily on standard IBM utility programs dispatched via `EXEC PGM=` in JCL. This crate implements the core z/OS utility suite within a unified `UtilityRegistry` framework. It provides standardized DD allocation mapping, condition code propagation (0, 4, 8, 12, 16), and IBM-format severity-coded message generation (`IEBnnnnS`).

## Capabilities

- **IEBCOPY** (`iebcopy`): Partitioned dataset (PDS) management supporting member copying, merging multiple input libraries, member selection (`SELECT MEMBER=`), exclusion (`EXCLUDE MEMBER=`), member renaming, replacement flags (`R`), and sequential unload/load format transport.
- **IEBGENER** (`iebgener`): Sequential dataset copy, member creation from sequential data, record filtering, and field-level reformatting via `GENERATE` and `RECORD FIELD=` control statements.
- **IEBUPDTE** (`iebupdte`): Source library maintenance supporting `./ ADD`, `./ REPL`, `./ CHANGE`, `./ NUMBER`, and `./ DELETE` control cards with sequence number matching.
- **IEBCOMPR** (`iebcompr`): Sequential and partitioned dataset comparison with record-by-record mismatch detection and reporting.
- **IEBPTPCH** (`iebptpch`): Dataset printing and punching with `RECORD FIELD=` extraction, hexadecimal conversions, and member filtering.
- **IEBDG** (`iebdg`): Test data generation supporting pattern specifications (`CREATE`, `REPEAT`, `SEQUENCE`, `PATTERN`, `DSD`, `FD`) including zoned decimal and alphanumeric sequences.
- **AMASPZAP** (`amaspzap`): Service aid ("Superzap") implementing `NAME`, `VER` (verify expected bytes), `REP` (replace bytes), `DUMP`, and `ABSDUMP` operations for inspect-and-patch workflows.
- **IEH Utilities** (`ieh`): System utilities implementing catalog management (`IEHPROGM` CATLG/UNCATLG/SCRATCH/RENAME), VTOC and catalog listing (`IEHLIST`), and dataset movement (`IEHMOVE`).
- **IEFBR14**: Standard "null" utility returning condition code 0, used for DD allocation side effects.
- **Batch Interfaces** (`batch`): Entry points for TSO command execution (`IKJEFT01`, `IKJEFT1A`, `IKJEFT1B`), REXX script execution (`IRXJCL`), and USS process dispatching (`BPXBATCH`).
- **Subsystem Wrappers** (`subsystems`): Registration stubs for subsystem utilities (`DFSRRC00`, `DFHCSDUP`, `SDSF`, `FTP`, `IGYCRCTL`, `IEWL`, `DFHECP1`, `DFHMAPS`, `DSNHPC`, `DSNTIAD`, `DSNTIAUL`, `DSNTEP4`, `DFSURGU0`).

## Architecture

```
                 JCL EXEC PGM= / Direct API Call
                               │
                               ▼
    ┌────────────────────────────────────────────────────────┐
    │                    UtilityRegistry                     │
    │  - Program name lookup (case-insensitive)              │
    │  - Built-in utilities and custom extensions            │
    └──────────────────────────┬─────────────────────────────┘
                               │
                               ▼
    ┌────────────────────────────────────────────────────────┐
    │                     UtilityContext                     │
    │  - DD allocations (SYSUT1, SYSUT2, SYSIN, SYSPRINT)   │
    │  - Sequential and in-memory PDS buffers (PdsData)      │
    │  - Condition code tracking & SYSPRINT message capture  │
    └──────────────────────────┬─────────────────────────────┘
                               │
                               ▼
    ┌────────────────────────────────────────────────────────┐
    │                 UtilityProgram Modules                 │
    │  - iebcopy, iebgener, iebupdte, iebcompr, iebptpch     │
    │  - iebdg, amaspzap, ieh (progm/list/move), batch       │
    └──────────────────────────┬─────────────────────────────┘
                               │
                               ▼
    ┌────────────────────────────────────────────────────────┐
    │                     UtilityResult                      │
    │  - Condition Code (0, 4, 8, 12, 16)                    │
    │  - UtilityMessage collection (e.g. IEB1013I)           │
    └──────────────────────────┬─────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `iebcopy` | PDS copying, merging, member selection, exclusion, renaming, and unload/load |
| `iebgener` | Sequential copying and `RECORD FIELD` reformatting |
| `iebupdte` | Source library maintenance with `./ ADD`, `./ REPL`, `./ CHANGE`, `./ DELETE` |
| `iebcompr` | Record-level comparison for sequential and partitioned datasets |
| `iebptpch` | Print and punch utility with field extraction and hex formatting |
| `iebdg` | Pattern-based test data generator (`CREATE`, `REPEAT`, `SEQUENCE`) |
| `amaspzap` | Inspect-and-patch service aid (`VER`, `REP`, `DUMP`, `ABSDUMP`) |
| `ieh` | System utilities: `IEHPROGM`, `IEHLIST`, and `IEHMOVE` |
| `batch` | Batch entry points: `IKJEFT01`, `IKJEFT1A`, `IKJEFT1B`, `IRXJCL`, `BPXBATCH` |
| `subsystems` | Subsystem utility wrappers (IMS, CICS, DB2, compilers, Linkage Editor) |
| `error` | `UtilityError` error definitions |

## Public API

### Framework Types and Traits

- `UtilityProgram`: The core trait implemented by all utility programs:
  ```rust
  pub trait UtilityProgram: Send + Sync {
      fn name(&self) -> &str;
      fn execute(&self, context: &mut UtilityContext) -> UtilityResult;
  }
  ```
- `UtilityRegistry`: Central dispatcher for executing programs by name (`with_builtins()`, `register()`, `dispatch()`, `is_registered()`, `list_programs()`).
- `UtilityContext`: Encapsulates the execution step, DD table, SYSIN input stream, and SYSPRINT output buffer (`new()`, `add_dd()`, `open_input()`, `read_sysin()`, `write_utility_message()`, `sysprint_output()`).
- `DdAllocation`: Represents a data definition allocation (`dataset()`, `inline()`, `output()`, `dummy()`, `pds()`, `sequential()`).
- `PdsData` / `PdsMemberData`: In-memory representation of Partitioned Data Sets and members for utility processing.
- `UnloadData`: In-memory sequential unload format buffer.
- `UtilityResult`: Execution outcome containing condition code and diagnostic messages (`success()`, `with_cc()`, `is_success()`, `is_warning()`, `is_error()`).
- `UtilityMessage` / `MessageSeverity`: IBM-formatted message container (`new()`, `info()`, `warning()`, `error()`, `severe()`) and severity levels (`I`, `W`, `E`, `S`).
- `Iefbr14`: Built-in null program returning condition code 0.
- `UtilityError`: Error type for program lookup failures and DD access issues.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`, `serde`, `serde_json`, `tracing`).
- **Consumers**:
  - `open-mainframe-jcl`: Re-exports `open_mainframe_utilities` and instantiates `UtilityRegistry::with_builtins()` in its JCL step executor (`open_mainframe_jcl::executor::JclExecutor`) to dispatch `EXEC PGM=` utility steps.

## Examples

### Dispatching IEBGENER via UtilityRegistry

```rust
use open_mainframe_utilities::{
    DdAllocation, UtilityContext, UtilityRegistry,
};

let registry = UtilityRegistry::with_builtins();
let mut ctx = UtilityContext::new("STEP01", "IEBGENER");

// Allocate input and output DDs
ctx.add_dd(DdAllocation::inline(
    "SYSUT1",
    vec!["RECORD 1 DATA".to_string(), "RECORD 2 DATA".to_string()],
));
ctx.add_dd(DdAllocation::output("SYSUT2"));
ctx.add_dd(DdAllocation::output("SYSPRINT"));

let result = registry.dispatch("IEBGENER", &mut ctx).unwrap();
assert_eq!(result.condition_code, 0);

let output = ctx.get_dd("SYSUT2").unwrap();
assert_eq!(output.output.len(), 2);
```

### Direct Program Invocation (IEFBR14)

```rust
use open_mainframe_utilities::{Iefbr14, UtilityContext, UtilityProgram};

let program = Iefbr14;
let mut ctx = UtilityContext::new("STEP00", "IEFBR14");
let result = program.execute(&mut ctx);

assert!(result.is_success());
assert_eq!(result.condition_code, 0);
```

## Testing

Run unit and documentation tests:

```bash
cargo test -p open-mainframe-utilities
```

The test suite contains 236 unit tests and 1 doc test across all utility modules, validating:
- Round-trip sequential and PDS copying, member selection, and rename operations in `iebcopy`.
- `RECORD FIELD=` reformatting, literal insertion, and error handling in `iebgener`.
- `./ ADD`, `./ REPL`, `./ CHANGE`, and sequence number updates in `iebupdte`.
- Pattern generation variants (fixed, sequence, random, wave) in `iebdg`.
- `VER` and `REP` patch success and rejection behavior in `amaspzap`.
- Batch entry point simulation and subsystem registration.

## Limitations

- **In-Memory DD Backing**: Data sets in `UtilityContext` are simulated via in-memory data structures (`PdsData`, `Vec<String>`). Physical catalog and disk I/O operations require integration with `open-mainframe-dataset`.
- **IEBCOPY COMPRESS**: Compression is modeled by removing marked-deleted members from in-memory PDS structures; physical disk track reclamation is not modeled.
- **IEHMOVE Volume Operations**: Full-volume movement and VTOC reorganization are simulated placeholders.
- **Subsystem Utilities**: Utilities such as `DSNTIAD` (DB2), `DFSRRC00` (IMS), `DFHCSDUP` (CICS), `IEWL` (Binder), and `IGYCRCTL` (COBOL compiler) are stubs that return successful completion and log messages without executing the full respective compiler or subsystem runtime within this crate.
- **SMF Recording**: Utilities do not directly generate or write SMF Type 14/15 dataset activity records.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-jcl](../open-mainframe-jcl/README.md)
- [open-mainframe-dataset](../open-mainframe-dataset/README.md)
