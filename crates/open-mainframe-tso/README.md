# open-mainframe-tso

A comprehensive Rust implementation of **TSO/E (Time Sharing Option/Extensions)** for the OpenMainframe project — providing interactive line-mode command processing, dataset allocation management, user session profile controls, parameter parsing services (IKJPARS), and bridges for REXX script execution and JES2 batch interaction.

## Purpose

TSO/E is the primary interactive command processor on IBM z/OS mainframes. `open-mainframe-tso` models this interactive terminal environment within OpenMainframe:
1. **Interactive Command Processing**: Dispatches and executes standard TSO commands (`ALLOCATE`, `FREE`, `LISTALC`, `LISTDS`, `PROFILE`, `SUBMIT`, `STATUS`, `CANCEL`, `DELETE`, `RENAME`, `CALL`, `EXEC`, `ALTLIB`).
2. **IKJPARS Parameter Parsing Service**: Implements the standard z/OS parameter parsing control entry architecture for validating positional parameters, keywords, and flags.
3. **Session & Profile Management**: Tracks user environment state including dataset prefixes (`USERID.DSN`), message identifiers (`MSGID`), and active DD allocations.
4. **Subsystem Bridges**: Integrates directly with `open-mainframe-jes2` for batch job submission and status monitoring, and `open-mainframe-rexx` for hosting `ADDRESS TSO` execution environments.

## Capabilities

- **TSO Command Dispatcher (`execute`, `execute_raw`)**:
  - Direct execution of dataset allocation commands (`ALLOCATE`, `FREE`, `LISTALC`, `LISTDS`).
  - File manipulation commands (`DELETE`, `RENAME`).
  - Program execution and invocation (`CALL`, `EXEC`).
  - Library concatenation management (`ALTLIB`).
- **IKJPARS Service (`ikjpars`, `ParseControlEntry`)**:
  - High-fidelity parameter parsing supporting keyword arguments (`DSN('MY.DATA')`), abbreviations, value validation lists, and boolean flags.
- **Session State (`TsoSession`, `TsoProfile`)**:
  - Manages active user ID, default dataset prefixing (automatically qualifying non-quoted dataset names), and current allocations.
- **JES2 Subsystem Integration**:
  - `SUBMIT` sends JCL files and dataset members directly to the `open-mainframe-jes2` internal reader.
  - `STATUS`, `CANCEL`, and `OUTPUT` monitor and manage active batch jobs.
- **REXX / CLIST Hosting**:
  - Hosts REXX scripts via `open-mainframe-rexx`, routing terminal `SAY`/`PULL` I/O to TSO streams.

## Architecture

```text
    User Terminal / REST API
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  TSO Command Dispatcher                │
    │  - Command line parsing (`parse_command`)               │
    │  - Alias resolution and execution (`execute`)          │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  TSO Service Routines                  │
    │  - IKJPARS (Parameter Parsing via ParseControlEntry)   │
    │  - Memory and Terminal I/O Streams                     │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Session & Profile                     │
    │  - TsoSession & TsoProfile (PREFIX, MSGID)             │
    │  - Active DD Table (Allocations)                       │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Subsystem Connectors                   │
    │  - JES2 Interface (SUBMIT, STATUS, CANCEL)             │
    │  - REXX Interpreter (ADDRESS TSO)                      │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `commands` | Core TSO command implementations: ALLOCATE, FREE, LISTDS, LISTALC, DELETE, RENAME, CALL, PROFILE. |
| `jobs` | JES2 interaction: SUBMIT, STATUS, CANCEL, and OUTPUT processing. |
| `session` | Session state: `TsoSession`, `TsoProfile`, active DD allocations (`AllocEntry`). |
| `services` | TSO service routines: `IKJPARS` parameter parser and `ParseControlEntry`. |
| `parser` | Command line tokenizer and keyword/positional extractor (`parse_command`, `ParsedCommand`). |
| `rexx_tso` | REXX host command environment routing `ADDRESS TSO` commands. |
| `exec` | Script invocation and execution runtime. |

## Public API

### Core Types and Services

```rust
use open_mainframe_tso::{
    TsoSession, TsoProfile, AllocEntry,
    parse_command, execute, execute_raw,
    ParsedCommand, CommandResult,
    services::{ikjpars, ParseControlEntry, ParsedParameters},
};
```

- `TsoSession`: Central interactive session structure holding user profile and allocation tables.
- `parse_command` / `execute`: Parse and execute TSO command lines.
- `ikjpars`: Validates parsed command tokens against a `ParseControlEntry` definition.

## Integration

### Workspace Dependencies

- [`open-mainframe-jes2`](../open-mainframe-jes2/README.md) — Used for submitting JCL and querying batch job statuses.
- [`open-mainframe-rexx`](../open-mainframe-rexx/README.md) — Powers REXX script execution under TSO.

### Known Consumers

- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Powers `/zosmf/tso` REST endpoints for interactive TSO address space sessions.

## Examples

### Executing a TSO Command Programmatically

```rust
use open_mainframe_tso::{TsoSession, parse_command, execute};
use std::path::PathBuf;

let mut session = TsoSession::new("IBMUSER", PathBuf::from("/tmp/datasets"));

// Allocate a dataset to DD SYSUT1
let cmd = parse_command("ALLOCATE FILE(SYSUT1) DA('SYS1.PARMLIB') SHR");
let result = execute(&mut session, &cmd);

assert!(result.success);
println!("TSO Output:\n{}", result.output);
```

### Validating Parameters with IKJPARS

```rust
use open_mainframe_tso::services::{ikjpars, ParseControlEntry};
use open_mainframe_tso::parse_command;

let pce = ParseControlEntry::new("MYCMD")
    .keyword("CLASS", &["A", "B", "C"])
    .flag("PURGE");

let cmd = parse_command("MYCMD CLASS(A) PURGE");
let parsed = ikjpars(&pce, &cmd).expect("IKJPARS validation failed");

assert_eq!(parsed.get_keyword("CLASS"), Some(&"A".to_string()));
assert!(parsed.has_flag("PURGE"));
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-tso
```

The test suite covers:
- **`parser::*`**: Command line tokenization, quoted string handling, keyword and positional argument extraction.
- **`commands::*`**: `ALLOCATE` (new, old, shr, mod), `FREE`, `LISTALC`, `LISTDS` attributes, `PROFILE` toggles (`PREFIX`, `MSGID`).
- **`services::*`**: `IKJPARS` keyword matching, flag extraction, positional ordering, and error diagnostics.
- **`jobs::*`**: `SUBMIT` card generation, job status polling, and cancellation handling.
- **`session::*`**: Dataset prefixing rules (qualifying unquoted dataset names with user prefix).

## Limitations

- **CLIST Language**: REXX is the primary scripting language; legacy CLIST parsing is partially implemented.
- **Interactive Prompts**: Prompts for missing required parameters return diagnostic messages rather than blocking on interactive stdin.
- **Terminal Control**: Line-mode TSO execution is fully supported; full-screen ISPF panel integration is handled separately by `open-mainframe-ispf`.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [JES2 Batch Subsystem (`open-mainframe-jes2`)](../open-mainframe-jes2/README.md)
- [REXX Interpreter (`open-mainframe-rexx`)](../open-mainframe-rexx/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
