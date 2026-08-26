# open-mainframe-ispf

A high-fidelity Rust implementation of **ISPF (Interactive System Productivity Facility)** for the OpenMainframe project — providing the complete mainframe development environment: panel-driven applications, dialog services, four-pool variable hierarchy, relational table services, file tailoring (skeletons), library management, and the full-featured ISPF line editor.

## Purpose

ISPF is the foundational full-screen application and development framework on IBM z/OS mainframes. `open-mainframe-ispf` models this environment within OpenMainframe:
1. **Panel Processing Engine**: Parses and renders ISPF panel definitions comprising `)ATTR`, `)BODY`, `)INIT`, `)REINIT`, `)PROC`, and `)MODEL` sections, evaluating field validation rules (`VER` statements) and dynamic formatting.
2. **Dialog Manager**: Dispatches core dialog services (`DISPLAY`, `SELECT`, `SETMSG`, `CONTROL`, `VGET`, `VPUT`) and routes user input events (`DisplayEvent`).
3. **Variable Hierarchy**: Manages the four ISPF variable pools (Function, Shared, Profile, System) with variable substitution (`&VAR`) across panels and commands.
4. **Data & Development Services**: Provides in-memory relational tables with cursor navigation, file tailoring of skeletons with control statements, library services (LM functions), and the interactive line editor.

## Capabilities

- **Panel Definition Parser (`parse_panel`, `Panel`)**:
  - Parses standard sections: `)ATTR` (custom attribute characters, highlighting, colors, input/output types), `)BODY` (screen layouts with variables and constants), `)INIT` (pre-display variable assignments), `)REINIT` (re-display logic on validation error), `)PROC` (post-display input validation and translation), and `)MODEL` (table display scrolling rows).
  - Validation rules: `VER` checks for `NONBLANK`, `ALPHA`, `NUM`, `DSNAME`, `PICT`, `LIST`, `RANGE`, `BIT`, `HEX`.
- **Dialog Manager (`DialogManager`)**:
  - Coordinates panel display cycles, executes `ISPEXEC` service calls, manages error messages (`SETMSG`), and tracks screen navigation.
- **Variable Pools (`VariablePoolManager`)**:
  - Four distinct pools:
    - **Function Pool**: Local to the executing dialog program.
    - **Shared Pool**: Shared across dialogs within the same application logical screen.
    - **Profile Pool**: User-specific variables persisted across sessions.
    - **System Pool**: Read-only variables maintained by ISPF (`ZCMD`, `ZUSER`, `ZDATE`, `ZTIME`, `ZAPPLID`).
- **Table Services (`TableManager`, `Table`)**:
  - Relational in-memory tables with named key and name fields.
  - Complete operations: `TBCREATE`, `TBOPEN`, `TBADD`, `TBPUT`, `TBMOD`, `TBDELETE`, `TBGET`, `TBSCAN`, `TBSORT`, `TBSARG`, `TBCLOSE`.
- **File Tailoring (`FileTailoringEngine`)**:
  - Skeleton processor substituting variables and interpreting control statements: `)SEL`/`)ENDSEL`, `)DOT`/`)ENDDOT`, `)IM`, `)SET`.
- **Line Editor & Macros (`IspfEditor`, `isredit`)**:
  - Command line and prefix area commands: `FIND`, `CHANGE`, `CAPS`, `HEX`, `RESET`, Insert (`I`), Delete (`D`), Copy (`C`), Move (`M`), Repeat (`R`), Text Split (`TS`), Text Flow (`TF`).
  - ISREDIT macro command interface for script-driven editing.
- **Library Management (`LibraryManager`)**:
  - Emulates `LMINIT`, `LMOPEN`, `LMGET`, `LMPUT`, `LMCLOSE`, and `LMFREE` services for dataset access.

## Architecture

```text
    User Terminal / TUI / REST
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Dialog Manager                        │
    │  - Service dispatcher (`DISPLAY`, `SELECT`, `SETMSG`)  │
    │  - Event loop (`DisplayEvent::Enter`, `End`, etc.)     │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Panel Engine                          │
    │  - Section parsing (`)ATTR`, `)BODY`, `)INIT`, `)PROC`) │
    │  - Field validation (`VER` checks)                     │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Variable Pools                        │
    │  - Function, Shared, Profile, and System pools         │
    │  - Substitution engine (`&VAR` expansion)              │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Data & Tailoring Services              │
    │  - ISPF Tables (`TableManager`: TBADD, TBSCAN, TBSORT) │
    │  - File Tailoring (`FileTailoringEngine`: FTINCL)      │
    │  - Line Editor (`IspfEditor` & ISREDIT macros)         │
    │  - Library Services (`LibraryManager`: LMINIT, LMOPEN) │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `dialog` | Dialog services: `DialogManager`, `DisplayEvent`, `ServiceCall`, `MessageDef`, variable pools. |
| `panel` | Panel definition engine: `parse_panel`, `Panel`, `PanelSection`, `FieldDef`, `AttrChar`, `VerCheck`. |
| `table` | Table services: `TableManager`, `Table`, `TableRow`, `SearchArg`, `SortDirection`. |
| `skeleton`| File tailoring: `FileTailoringEngine`, skeleton parsing, conditional substitution. |
| `editor` | Line editor: `IspfEditor`, line commands, primary commands, edit profiles, undo stack. |
| `isredit` | Editor macro interface: `IsreditMacroEngine`, macro commands, and buffer manipulation. |
| `library` | Library management: `LibraryManager`, dataset/member handles, data transfer services. |
| `utilities`| Utility applications: Member list display, dataset listing, search utilities. |

## Public API

### Core Types and Services

```rust
use open_mainframe_ispf::{
    dialog::{DialogManager, DisplayEvent, VariablePoolManager},
    panel::{parse_panel, Panel, PanelError},
    table::{TableManager, Table},
    skeleton::FileTailoringEngine,
    editor::IspfEditor,
    library::LibraryManager,
};
```

- `DialogManager`: Main controller for executing ISPF dialogs, setting variables, and presenting panels.
- `parse_panel`: Parses raw ISPF panel text source into executable `Panel` structures.
- `TableManager`: Central manager for creating, querying, and updating ISPF tables.
- `IspfEditor`: Text editing engine supporting mainframe line-mode and primary edit commands.

## Integration

### Workspace Dependencies

- None (pure Rust library using standard workspace error and serialization crates: `miette`, `thiserror`, `serde`, `serde_json`, `tracing`, `chrono`, `regex`).

### Known Consumers

- [`open-mainframe-tso`](../open-mainframe-tso/README.md) — Invokes ISPF dialog manager commands.
- Interactive terminal UI and web frontends providing mainframe developer experiences.

## Examples

### Parsing and Displaying an ISPF Panel

```rust
use open_mainframe_ispf::dialog::{DialogManager, DisplayEvent};
use open_mainframe_ispf::panel::parse_panel;

let panel_src = r#"
)ATTR
  % TYPE(TEXT) INTENS(HIGH)
  + TYPE(TEXT) INTENS(LOW)
  _ TYPE(INPUT) INTENS(HIGH)
)BODY
%---------------------- SAMPLE ISPF PANEL ----------------------
%COMMAND ===>_ZCMD
%
+User ID . . . : _ZUSER   +
)INIT
  &ZUSER = 'IBMUSER'
)PROC
  VER (&ZCMD, NONBLANK)
)END
"#;

let panel = parse_panel(panel_src).expect("Failed to parse panel");
let mut dm = DialogManager::new();
dm.vars.set("ZUSER", "IBMUSER".into());

// Render panel and handle return event
let event = dm.display_panel(&panel);
assert_eq!(event, DisplayEvent::Enter);
```

### Managing Relational ISPF Tables

```rust
use open_mainframe_ispf::table::TableManager;

let mut tm = TableManager::new();

// Create table with EMPID as key and NAME/DEPT as columns
tm.tbcreate("EMP_TABLE", &["EMPID"], &["NAME", "DEPT"], true).unwrap();

// Insert a row
tm.tbadd("EMP_TABLE", &[
    ("EMPID", "E001"),
    ("NAME", "ALICE"),
    ("DEPT", "DEV"),
]).unwrap();

// Retrieve row by key
let row = tm.tbget("EMP_TABLE", &[("EMPID", "E001")]).unwrap();
assert_eq!(row.get("NAME"), Some(&"ALICE".to_string()));
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-ispf
```

The test suite covers:
- **`panel::*`**: Attribute definitions, attribute byte mapping, body layout line alignment, section parsing (INIT, REINIT, PROC, MODEL), and `VER` validation rules.
- **`dialog::*`**: Variable pool scoping (Function, Shared, Profile, System), variable substitution (`&VAR`), and display event transitions.
- **`table::*`**: Table creation, row addition, key indexing, scan conditions (`TBSCAN`), and multi-column sorting (`TBSORT`).
- **`skeleton::*`**: File tailoring conditional logic (`)SEL`), table loops (`)DOT`), and file inclusions (`)IM`).
- **`editor::*`**: Line command execution (I, D, C, M, R), primary commands (`FIND`, `CHANGE`), hex mode display, and caps enforcement.

## Limitations

- **DBCS Panel Rendering**: Double-Byte Character Set (DBCS) characters are handled as plain UTF-8 rather than EBCDIC shift-out/shift-in pairs.
- **Graphical Displays**: GDDM (Graphical Data Display Manager) graphic primitive rendering is not supported.
- **Profile Persistence**: User profile pools serialize to in-memory/JSON storage rather than partitioned ISPF profile datasets.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [TSO/E Command Processor (`open-mainframe-tso`)](../open-mainframe-tso/README.md)
- [REXX Interpreter (`open-mainframe-rexx`)](../open-mainframe-rexx/README.md)
