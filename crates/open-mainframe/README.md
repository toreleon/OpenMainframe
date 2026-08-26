# open-mainframe

`open-mainframe` is the primary CLI binary and integration library (`open_mainframe_lib`) for the OpenMainframe z/OS emulation system. It connects the COBOL compiler, runtime interpreter, CICS transaction engine, and 3270 terminal UI into a unified executable and reusable runtime bridge.

## Purpose

The crate serves two core purposes:
1. **Command-Line Interface (`open-mainframe`)**: Provides direct user commands to launch interactive 3270 CICS terminal sessions (`cics`), execute automated headless CICS test workloads (`--headless`), and generate the comprehensive Mainframe Code Wiki (`wiki`).
2. **Runtime Integration Library (`open_mainframe_lib`)**: Bridges the tree-walking COBOL interpreter in [`open-mainframe-runtime`](../open-mainframe-runtime/README.md) with the transaction services in [`open-mainframe-cics`](../open-mainframe-cics/README.md) and the interactive terminal rendering in [`open-mainframe-tui`](../open-mainframe-tui/README.md). This library is also consumed by [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) to power RESTful CICS sessions.

## Capabilities

- **On-Demand COBOL Compilation**: Compiles COBOL source files and expands copybooks directly into executable `SimpleProgram` representations on the fly.
- **Interactive 3270 Terminal Session**: Hosts multi-screen CICS transactions inside an interactive terminal UI with customizable color themes (`classic`, `modern`, `mono`).
- **Headless JSON Testing Protocol**: Emits line-delimited JSON screen snapshots on `stdout` and accepts JSON input events (AID key and field modifications) on `stdin` for headless automation and CI validation.
- **Pseudo-Conversational Control Loop**: Handles CICS program transitions including `EXEC CICS XCTL`, `EXEC CICS RETURN TRANSID(name) COMMAREA(data)`, `EXEC CICS LINK`, and `EXEC CICS ABEND`.
- **BMS Screen Processing**: Renders BMS maps to 3270 terminal screens, decomposes modified input fields into COBOL variables upon user submission, and reconciles input/output map overlays.
- **File Control & Browsing**: Connects COBOL file I/O to VSAM datasets with automated EBCDIC/ASCII record detection, browse token tracking (`STARTBR`, `READNEXT`, `READPREV`, `ENDBR`, `RESETBR`), and group variable decomposition.
- **Static Documentation Generation**: Hosts the `open-mainframe wiki` command to scan mainframe repositories and generate Markdown/Mermaid documentation via [`open-mainframe-wiki`](../open-mainframe-wiki/README.md).

## Architecture

```text
                     CLI / REST API Entry
                               │
                               ▼
    COBOL Source ──► [compile_program] ──► [lower_program] ──► SimpleProgram
                                                                    │
    ┌───────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                        Interpreter Execution Loop                       │
│                                                                         │
│  open_mainframe_runtime::interpreter::execute(program, env)            │
│                              │                                          │
│                              ▼                                          │
│                    BridgeHandler (execute)                              │
│                              │                                          │
│                              ▼                                          │
│                         CicsBridge                                      │
│         ┌────────────────────┼────────────────────┐                     │
│         ▼                    ▼                    ▼                     │
│    Terminal I/O         File Control       Program Control              │
│    (SEND/RECEIVE MAP)   (READ/WRITE/BR)    (XCTL/RETURN/ABEND)          │
│         │                    │                    │                     │
│         ▼                    ▼                    ▼                     │
│    Session / TUI        CicsRuntime /       BridgeAction                │
│    (or Headless JSON)   FileManager         (deferred to loop)          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|---|---|
| `lib` | Library root exposing `compile_program`, `find_program_source`, `setup_env`, and `BridgeHandler`. |
| `bridge` | `CicsBridge` and `BridgeAction` managing CICS command execution, BMS mapping, VSAM data file routing, browse tokens, and EIB response code propagation. |
| `lower` | AST lowering pass translating `open_mainframe_cobol::ast::Program` into `SimpleProgram` for the interpreter. Handles group layout extraction, condition names, redefines aliases, and declarative sections. |
| `headless` | Line-oriented JSON protocol implementation (`ScreenOutput`, `FieldOutput`, `CursorPosition`, `HeadlessInput`, `EndOutput`) for automated testing without a TUI. |
| `main` | CLI application entry point providing argument parsing for `cics` and `wiki` subcommands. |

## Public API and Binaries

### Binary: `open-mainframe`

```text
open-mainframe [COMMAND]

Commands:
  cics  Run an interactive CICS terminal session (3270 TUI or headless JSON)
  wiki  Generate a Mainframe Code Wiki from source files
```

#### `cics` Subcommand Options

- `<file>`: Path to initial COBOL source file (`.cbl`).
- `-I, --include <dir>`: Additional copybook include directories (can be specified multiple times).
- `--data <DDNAME=path[:key_len[:rec_len]]>`: VSAM data files to mount into the session.
- `--bms-dir <dir>`: Directory containing BMS map definition sources.
- `--transid <TRANSID=PROGRAM>`: Mapping of transaction IDs to target program names.
- `--program-dir <dir>`: Directory to search for dynamic `XCTL` and `RETURN TRANSID` programs.
- `--theme <classic|modern|mono>`: Terminal color theme (default: `classic`).
- `--headless`: Run in headless JSON mode over `stdin`/`stdout`.
- `-v, --verbose`: Enable detailed logging to `open-mainframe.log`.

### Library: `open_mainframe_lib`

Key exported items from `open_mainframe_lib`:

```rust
pub struct BridgeHandler {
    pub inner: Rc<RefCell<CicsBridge>>,
}

pub struct CicsBridge { /* ... */ }

pub enum BridgeAction {
    Xctl { program: String, commarea: Option<Vec<u8>> },
    Return { transid: Option<String>, commarea: Option<Vec<u8>> },
    Abend { code: String },
}

pub fn compile_program(
    path: &Path,
    include_paths: &[PathBuf],
) -> Result<SimpleProgram, String>;

pub fn find_program_source(
    program_name: &str,
    search_dirs: &[PathBuf],
) -> Option<PathBuf>;

pub fn setup_env(
    bridge: &Rc<RefCell<CicsBridge>>,
    commarea: &Option<Vec<u8>>,
    pending_aid: &mut Option<u8>,
) -> Environment;
```

## Integration and Consumers

### Workspace Dependencies

- [`open-mainframe-cobol`](../open-mainframe-cobol/README.md) — Preprocessor, lexer, parser, and AST definitions.
- [`open-mainframe-runtime`](../open-mainframe-runtime/README.md) — Tree-walking interpreter, `SimpleProgram`, and runtime `CobolValue`.
- [`open-mainframe-cics`](../open-mainframe-cics/README.md) — CICS runtime environment, EIB, BMS parser/renderer, and command dispatcher.
- [`open-mainframe-tui`](../open-mainframe-tui/README.md) — 3270 terminal session, screen buffer, and keyboard event loop.
- [`open-mainframe-encoding`](../open-mainframe-encoding/README.md) — EBCDIC code page conversions.
- [`open-mainframe-wiki`](../open-mainframe-wiki/README.md) — Static documentation generator invoked by `open-mainframe wiki`.
- [`open-mainframe-precompilers`](../open-mainframe-precompilers/README.md) — Source transformations for EXEC statements.

### Known Consumers

- `open-mainframe` (`src/main.rs`) — The CLI application itself.
- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Uses `open_mainframe_lib` (`compile_program`, `setup_env`, `CicsBridge`, `headless`) in `src/cics_runner.rs` and `src/handlers/cics.rs` to run CICS sessions behind z/OSMF REST endpoints.

## Examples

### Running an Interactive CICS Application

```bash
cargo run --release -p open-mainframe -- cics app/cbl/COSGN00C.cbl \
    -I app/cpy \
    -I app/cpy-bms \
    --bms-dir app/bms \
    --program-dir app/cbl \
    --data "ACCTDAT=app/data/ASCII/acctdata.txt:11:300" \
    --transid "CC00=COSGN00C"
```

### Running in Headless Mode for Automated Verification

```bash
cargo run --release -p open-mainframe -- cics app/cbl/COSGN00C.cbl \
    -I app/cpy \
    --headless
```

### Compiling and Lowering Programmatically

```rust
use std::path::PathBuf;
use open_mainframe_lib::compile_program;

let source_path = PathBuf::from("app/cbl/MYPROG.cbl");
let include_paths = vec![PathBuf::from("app/cpy")];

let simple_program = compile_program(&source_path, &include_paths)
    .expect("Failed to compile COBOL source");

println!("Compiled program {} with {} statements",
    simple_program.name,
    simple_program.statements.len()
);
```

## Testing

Run unit tests within the crate:

```bash
cargo test -p open-mainframe
```

Test locations:
- `src/bridge.rs`: Unit tests for EBCDIC record inspection and conversion (`detects_and_decodes_ebcdic_fixed_records`, `leaves_ascii_fixed_records_unchanged`).
- Integration tests and headless end-to-end sessions are executed via repository test scripts such as `scripts/test-carddemo-full.sh`.

## Limitations

- **On-Demand Compilation Cost**: `compile_program()` reads and parses COBOL source directly from disk on every invocation without an in-memory compiler cache.
- **Flat Namespace Representation**: The runtime interpreter uses a single flat variable namespace; BMS input/output `REDEFINES` structures are resolved via naming conventions (`...I` and `...O` suffixes) rather than shared byte memory offsets.
- **Terminal Capabilities**: Interactive TUI mode requires a terminal emulator supporting standard ANSI escape codes and crossterm raw mode.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [CICS Subsystem (`open-mainframe-cics`)](../open-mainframe-cics/README.md)
- [Runtime Interpreter (`open-mainframe-runtime`)](../open-mainframe-runtime/README.md)
- [COBOL Compiler (`open-mainframe-cobol`)](../open-mainframe-cobol/README.md)
- [Terminal UI (`open-mainframe-tui`)](../open-mainframe-tui/README.md)
- [z/OSMF REST Server (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
- [Mainframe Wiki Generator (`open-mainframe-wiki`)](../open-mainframe-wiki/README.md)
