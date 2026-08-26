# open-mainframe-jcl

Job Control Language (JCL) parser, procedure expander, and batch execution engine for OpenMainframe.

## Purpose

`open-mainframe-jcl` models the z/OS Job Control Language interpreter and batch execution subsystem. It parses fixed-format JCL source cards, performs cataloged and in-stream procedure (PROC) expansion with parameter overrides, resolves generation data groups (GDGs) and passed datasets (`DISP=(PASS)`), evaluates condition codes and `IF/THEN/ELSE` control logic, dynamically allocates DD datasets, and dispatches batch programs and utilities.

## Capabilities

- **JCL Statement Parsing & AST**:
  - Full support for `//JOB`, `//EXEC` (PGM and PROC), `//DD`, `//PROC`, `//PEND`, `//SET`, `//IF/THEN/ELSE/ENDIF`, `//INCLUDE`, and `//OUTPUT` statements.
  - Column 72 continuation handling, in-stream datasets (`DD *`, `DD DATA` with custom delimiters), and concatenated DDs.
- **Two-Pass Procedure Expansion**:
  - In-stream and cataloged procedure resolution via `ProcedureLibrary` (`InMemoryProcLib`, `FilesystemProcLib`).
  - Symbolic parameter substitution (`&PARAM.`) with dot termination rules (`&DSN..DATA`).
  - Hierarchical step and DD override logic (`//STEP1.SYSUT1 DD ...`).
- **Batch Job Execution Engine (`JobExecutor`)**:
  - Step-by-step program and utility execution.
  - Condition code evaluation supporting `COND=(code,operator[,step])`, `COND=EVEN`, `COND=ONLY`, and complex Boolean `IF/THEN/ELSE` expressions.
  - Temporary dataset passing (`DISP=(NEW,PASS)`) with lifecycle management through final deletion or disposition.
  - Intra-job GDG consistency (e.g. `(+1)` refers to the same generation across subsequent steps in a single job).
- **Utility Integration**:
  - Re-exports and integrates with `open-mainframe-utilities` (`IEBGENER`, `IEBCOPY`, `IEBCOMPR`, `IEBDG`, `IEBEDIT`, `IEBUPDTE`, `IEFBR14`, `IDCAMS`, `SORT`, `IKJEFT01`).

## Architecture

```
    JCL Source Text                       Job Execution Engine
    ┌──────────────┐                      ┌────────────────────┐
    │ //JOB        │    Parsing           │    JobExecutor     │
    │ //STEP EXEC  │ ──────────────────>  │    (Lifecycle)     │
    │ //DD         │    JclParser         │  Steps, COND, CC   │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Proc Expansion    ┌────────────────────┐
    │  PROCLIB     │ ──────────────────>  │   Dataset Engine   │
    │  (Cataloged) │    ProcExpander      │   (Allocation)     │
    └──────────────┘                      │ SVC 99, DDnames    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Dispatch          ┌────────────────────┐
    │  Program     │ <──────────────────  │  Utility Registry  │
    │  Execution   │    JobExecutor       │  IEBCOPY, GENER    │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `parser/` | ~3 810 | Recursive descent parser for all z/OS JCL statements and operands |
| `executor/` | ~4 000 | Job state machine, step execution, condition codes, passed datasets |
| `procedure.rs` | ~1 330 | Multi-level PROC expansion, recursive symbolic parameter substitution |
| `ast/` | ~760 | AST definitions: Job, Step, DD, IF/THEN/ELSE, parameter types |
| `lexer/` | ~730 | Fixed-format scanner, column-based tokenization, column 72 continuation |
| `error.rs` | ~200 | `JclError` enum with source location tracking and miette diagnostics |

## Public API

### Primary Types and Functions

- `parse(source: &str) -> Result<Job, JclError>`: Primary parser entry point producing a `Job` AST.
- `Parser` / `Lexer` / `JclStatement`: Core lexing and AST parsing constructs.
- `JobExecutor`: State machine executing parsed jobs with `new()`, `with_config(config)`, and `execute(&mut self, job: &Job) -> Result<JobResult, JclError>`.
- `ExecutionConfig`: Configures `program_dir`, `dataset_dir`, `work_dir`, `sysout_dir`, and dataset path overrides.
- `JobResult` / `StepResult`: Execution outcomes, individual step return codes, stdout/stderr, and overall job status.
- `ProcedureExpander` / `ProcedureLibrary` / `InMemoryProcLib` / `FilesystemProcLib`: PROC management and expansion engine.
- `JclError` / `SourceLocation`: Diagnostic error structures.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-lang-core` — Shared `Span` and `AstNode` definitions.
- `open-mainframe-dataset` — Dataset allocation, catalog access, and record I/O.
- `open-mainframe-encoding` — EBCDIC encoding for SYSIN and in-stream datasets.
- `open-mainframe-sort` — DFSORT-compatible utility engine for SORT steps.
- `open-mainframe-utilities` — Standard IBM utility programs (`IEBGENER`, `IEBCOPY`, `IDCAMS`, etc.).

### Workspace Consumers

- `open-mainframe-zosmf` — Drives batch job submission (`/zosmf/restjobs/jobs`), status querying, and spool retrieval.
- `open-mainframe-wiki` — Automatic documentation and JCL syntax extraction.

## Examples

### Parsing and Executing a Batch Job

```rust
use open_mainframe_jcl::{parse, JobExecutor};

let jcl = r#"
//MYJOB    JOB (ACCT),'OPENMAINFRAME',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSUT1   DD *
HELLO MAINFRAME BATCH
/*
//SYSUT2   DD DSN=&&TEMP,DISP=(NEW,PASS),SPACE=(TRK,1)
//SYSPRINT DD SYSOUT=*
//STEP2    EXEC PGM=IEFBR14,COND=(0,NE,STEP1)
//SYSPRINT DD SYSOUT=*
//
"#;

let job = parse(jcl).expect("Failed to parse JCL");
assert_eq!(job.name, "MYJOB");
assert_eq!(job.steps.len(), 2);

let mut executor = JobExecutor::new();
let result = executor.execute(&job).expect("Execution failed");
assert!(result.success);
```

### Expanding a Cataloged Procedure

```rust
use open_mainframe_jcl::procedure::{InMemoryProcLib, ProcedureExpander};

let mut proclib = InMemoryProcLib::new();
proclib.add_proc("COMPILE", r#"
//COMPILE PROC DSN=DEFAULT.SRC
//COMP    EXEC PGM=COBOL
//SYSIN   DD DSN=&DSN,DISP=SHR
//SYSPRINT DD SYSOUT=*
//        PEND
"#);

let expander = ProcedureExpander::new(proclib);
```

## Testing

The crate includes 162 unit and integration tests verifying statement parsing, PROC parameter expansion, IF/THEN evaluation, and multi-step execution:

```bash
cargo test -p open-mainframe-jcl
```

Key test locations:
- `src/parser/` — All statement formats, complex DD parameters (DCB, DISP, SPACE, AMP, SUBSYS).
- `src/procedure.rs` — Multi-level nested PROCs, override precedence, and dot termination rules.
- `src/executor/` — Step execution order, COND evaluation logic, passed dataset lifecycles, and utility dispatch.
- `src/lexer/` — Column 72 continuations, in-stream comments, and card column boundaries.

## Limitations

- **Process Isolation**: Job steps execute as threads/subprocesses in the host process rather than isolated z/OS Address Spaces (ASIDs).
- **Restart / Checkpoints**: `RESTART=STEP` and checkpoint/restart facilities parse into the AST but execution currently begins at the first step.
- **Resource Enforcement**: Parameters like `REGION`, `TIME`, and `MEMLIMIT` are parsed into the AST but not strictly enforced by host OS limits.
- **Tape Simulation**: Tape volume allocation and mounting is simulated via sequential disk dataset files.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-utilities](../open-mainframe-utilities/README.md) — Standard batch utilities invoked by JCL.
- [open-mainframe-dataset](../open-mainframe-dataset/README.md) — Dataset allocation and DD I/O backend.
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md) — z/OSMF REST jobs API server.
