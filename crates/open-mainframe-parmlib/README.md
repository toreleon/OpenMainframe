# open-mainframe-parmlib

z/OS PARMLIB — system initialization configuration, system symbol substitution engine, subsystem configuration parsers, and initialization operator commands for the OpenMainframe project.

## Purpose

`SYS1.PARMLIB` is the central configuration repository on z/OS controlling system boot parameters, subsystem definitions, authorized libraries, and operator consoles. This crate implements the multi-dataset PARMLIB concatenation mechanism (`ParmlibConcat`), a robust `&symbol.` substitution engine (`SymbolEngine`) with static system symbols, parsers for core and subsystem configuration members, initialization sequence orchestration (`InitSequence`), and dynamic configuration operator commands (`SET`, `SETPROG`, `DISPLAY PARMLIB`).

## Capabilities

- **PARMLIB Concatenation & Parser Registry** (`members`):
  - Multi-directory dataset concatenation search with first-match directory precedence and member list deduplication.
  - Extensible `ParserRegistry` dispatching member parsing based on member prefix (e.g. `IEASYS`, `PROG`, `LNKLST`, `CONSOL`, `COMMND`, `IKJTSO`, `ALLOC`).
  - Core member parsers:
    - `IEASYSxx`: System parameter specifications (supports `KEY=VAL` and `KEY=(VAL)` syntax, comments, and continuation).
    - `PROGxx`: Authorized Program Facility (`APF ADD DSNAME(...) VOLUME(...)`) and Linklist (`LNKLST ADD NAME(...) DSNAME(...)`) statements.
    - `LNKLSTxx`: Legacy Linklist dataset concatenation list.
    - `CONSOLxx`: Master and secondary console definitions (`CONSOLE DEVNUM(...) NAME(...) AUTH(...)`).
    - `COMMNDxx`: System initialization commands (`COM='...'`).
- **System Symbol Engine** (`symbols`):
  - Resolves static system symbols (`&SYSNAME.`, `&SYSPLEX.`, `&SYSCLONE.`, `&SYSALPHADATE.`, etc.) via `StaticSymbols`.
  - Parses `IEASYMxx` symbol definition members (`SYMDEF(&NAME.='VALUE')`) up to the z/OS 200-symbol maximum limit.
  - Recursive symbol substitution with loop detection (depth limit 10), case-insensitivity, and escaped ampersands (`&&`).
- **Subsystem Configurations** (`subsystem`):
  - `IKJTSOxx`: TSO authorized commands (`AUTHCMD`), programs (`AUTHPGM`), and default logon parameters.
  - `ALLOCxx`: System allocation defaults and dataset integrity options.
  - `SubsystemDelegate`: Extensible delegation interface allowing external crates (JES2, RACF, USS, SMF) to register member parsers.
- **Operator Commands & Boot Sequence** (`operator`):
  - Boot orchestration state machine (`InitSequence`) with phase transitions: `Nip` (Nucleus Initialization) → `Subsystem` → `Master` → `Active`.
  - Operator commands: `SET` (parameter adjustments), `SETPROG` (dynamic APF/LNKLST updates), and `DISPLAY PARMLIB` (concatenation and suffix report).

## Architecture

```
    ┌─────────────────────────────────────────────────────────────┐
    │                      Public API (lib.rs)                    │
    ├──────────────────┬─────────────────────┬────────────────────┤
    │     members      │       symbols       │     operator       │
    ├──────────────────┼─────────────────────┼────────────────────┤
    │ ParmlibConcat    │ SymbolEngine        │ InitSequence       │
    │ ParserRegistry   │ StaticSymbols       │ SetCommand         │
    │ IeaSysConfig     │ IeaSymConfig        │ SetProgCommand     │
    │ ProgConfig       │                     │ DisplayParmlib     │
    │ LnkLstConfig     │                     │                    │
    │ ConsolConfig     │                     │                    │
    │ CommndConfig     │                     │                    │
    ├──────────────────┴─────────────────────┴────────────────────┤
    │ subsystem: AllocConfig, IkjTsoConfig, SubsystemDelegate     │
    └─────────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `members` | `ParmlibConcat` search order, `ParserRegistry`, and core member parsers (`IEASYSxx`, `PROGxx`, `LNKLSTxx`, `CONSOLxx`, `COMMNDxx`) |
| `symbols` | `SymbolEngine`, `StaticSymbols`, and `IEASYMxx` symbol table parser |
| `subsystem` | Subsystem configuration parsers (`IKJTSOxx`, `ALLOCxx`) and delegate parser registry |
| `operator` | Boot sequence manager (`InitSequence`) and operator commands (`SET`, `SETPROG`, `DISPLAY PARMLIB`) |

## Public API

### Primary Types and Functions

- `ParmlibConcat`: Multi-directory search order manager (`new()`, `find_member()`, `list_members()`).
- `ParserRegistry`: Central member parser dispatcher (`new()`, `with_defaults()`, `register()`, `parse()`).
- `IeaSysConfig` / `ProgConfig` / `LnkLstConfig` / `ConsolConfig` / `CommndConfig`: Parsed configuration models for core members.
- `SymbolEngine`: Symbol substitution engine (`new()`, `with_static()`, `load_config()`, `set_user_symbol()`, `substitute()`).
- `StaticSymbols`: Built-in system symbols (`default()`, `get()`, `as_map()`).
- `IeaSymConfig`: Parsed `IEASYMxx` symbol definition table (`parse()`).
- `AllocConfig` / `IkjTsoConfig` / `SubsystemDelegate`: Subsystem configuration models and registry.
- `InitSequence`: Boot phase state machine (`new()`, `advance()`, `current_phase()`, `is_active()`).
- `InitPhase`: Boot phases (`Nip`, `Subsystem`, `Master`, `Active`).
- `SetCommand` / `SetProgCommand` / `DisplayParmlibCommand`: Parsed operator configuration command structures.

## Integration

- **Internal workspace dependencies**: None (depends on workspace-configured `miette`, `thiserror`).
- **Consumers**:
  - `open-mainframe-zosmf`: Uses `ParmlibConcat`, `ParserRegistry`, `StaticSymbols`, and `SymbolEngine` in `crates/open-mainframe-zosmf/src/handlers/variables.rs` and `crates/open-mainframe-zosmf/src/state.rs` for z/OSMF system variables and parmlib endpoints.

## Examples

### Resolving System Symbols with SymbolEngine

```rust
use open_mainframe_parmlib::symbols::{StaticSymbols, SymbolEngine};

let statics = StaticSymbols::default();
let mut engine = SymbolEngine::with_static(&statics);
engine.set_user_symbol("APPNAME", "PAYROLL");

let raw_dsn = "DATA.&SYSNAME..&APPNAME..MASTER";
let resolved = engine.substitute(raw_dsn).unwrap();
assert!(resolved.contains(".PAYROLL.MASTER"));
```

### Parsing PARMLIB Members with ParmlibConcat

```rust
use open_mainframe_parmlib::members::{IeaSysConfig, ParmlibConcat};
use std::fs;
use tempfile::tempdir;

let dir = tempdir().unwrap();
let parmlib_path = dir.path().join("IEASYS00");
fs::write(&parmlib_path, "MAXUSER=500\nCMD=00\n").unwrap();

let concat = ParmlibConcat::new(&[dir.path().to_str().unwrap()]);
let content = concat.find_member("IEASYS00").unwrap().expect("Member found");

let config = IeaSysConfig::parse("IEASYS00", &content).unwrap();
assert_eq!(config.maxuser, Some(500));
```

## Testing

Run unit tests:

```bash
cargo test -p open-mainframe-parmlib
```

The test suite contains 52 unit tests covering:
- Multi-directory concatenation precedence and member deduplication.
- Core member parsing (`IEASYSxx`, `PROGxx`, `LNKLSTxx`, `CONSOLxx`, `COMMNDxx`).
- `IEASYMxx` parsing and `&symbol.` substitution (nested symbols, case-insensitivity, recursion limits).
- Subsystem configuration parsing (`IKJTSOxx`, `ALLOCxx`) and delegate registry error handling.
- `InitSequence` boot phase transitions and `SET`/`SETPROG`/`DISPLAY PARMLIB` command parsing.

## Limitations

- **Filesystem-Backed Concatenation**: Concatenation searches scan host filesystem directories rather than physical DASD PDS extents.
- **Subsystem Member Coverage**: Core and subsystem parsers cover standard members; unhandled member types require registration of custom `SubsystemDelegate` handlers.
- **Dynamic Clock Synchronization**: Built-in static and date/time symbols derive from local system time rather than Sysplex hardware timers (STP/ETR).

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md)
- [open-mainframe-mvs](../open-mainframe-mvs/README.md)
