# open-mainframe-tui

Interactive 3270 Terminal User Interface (TUI) — full-screen IBM 3270 terminal emulator built on `ratatui` and `crossterm` for the OpenMainframe project.

## Purpose

The `open-mainframe-tui` crate provides an authentic full-screen IBM 3270 display terminal environment for interactive mainframe workloads. It bridges native terminal emulators with the CICS transaction runtime and TSO sessions, handling 3270 buffer rendering, field attributes, cursor navigation, AID key dispatch, Modified Data Tag (MDT) tracking, Operator Information Area (OIA) status indicators, and DBCS character sets.

## Capabilities

- **IBM 3270 Terminal Models** (`terminal`):
  - Model 2 (24 rows × 80 cols) — standard 1,920-character display.
  - Model 3 (32 rows × 80 cols).
  - Model 4 (43 rows × 80 cols).
  - Model 5 (27 rows × 132 cols) — wide-format display.
- **Field Attributes & Buffer Management** (`fields`):
  - Unprotected (input) vs. Protected (read-only/labels), autoskip, hidden/non-display (password entry), intensified, and numeric-only fields.
  - Extended highlighting: Normal, Blink, Reverse Video, Underline.
  - Modified Data Tag (MDT): Automatically marked modified when user edits an input field; only modified fields are transmitted on Enter/PF keys.
- **Operator Information Area (OIA)** (`oia`):
  - Bottom status line rendering: System Lock (`X SYSTEM` / `X CLOCK`), Insert Mode (`^`), Input Inhibit, Communication Status (`4A`), and Cursor Position `(row, col)`.
- **Keyboard & AID Key Navigation** (`keymap`, `session`):
  - Field navigation: Tab (next unprotected field), BackTab (previous unprotected field), Home (first unprotected field), NewLine (next line's first field).
  - In-field editing: Character insert (with right-shift), delete (with left-shift), backspace, Erase EOF (clear from cursor to end of field).
  - Attention Identifier (AID) keys: Enter, PF1–PF24, PA1–PA3, Clear.
- **Color Themes** (`theme`):
  - `Green`: Classic green-phosphor monochrome monitor.
  - `Amber`: Amber-phosphor monochrome monitor.
  - `White`: White monochrome monitor.
  - `FullColor`: 3279 7-color / 16-color model (Neutral, Blue, Red, Pink, Green, Turquoise, Yellow, White).
- **Double-Byte Character Sets** (`dbcs`):
  - Shift-Out (`0x0E`) and Shift-In (`0x0F`) byte demarcation and wide-character rendering.
- **Headless & Snapshot Testing** (`events`, `snapshot`):
  - `MockTerminal` and `MockEventSource` for automated terminal scripting, keystroke replay, and visual golden-file snapshot verification.

## Architecture

```
     User Host Terminal (xterm, iTerm, Alacritty)
                          │
                          ▼
    ┌────────────────────────────────────────────────────────┐
    │                   open-mainframe-tui                   │
    │  - Crossterm Event Loop (key/mouse/resize)             │
    │  - FieldTable & Cursor state tracking (Tab, MDT)       │
    │  - OIA Status Line & Color Theme renderer              │
    │  - Ratatui Widget rendering engine                     │
    └─────────────────────┬──────────────────────────────────┘
                          │ (AID Key + Modified Field Values)
                          ▼
    ┌────────────────────────────────────────────────────────┐
    │                  open-mainframe-cics                   │
    │  - CICS TerminalManager & ScreenBuffer                 │
    │  - BMS Map execution and program dispatch              │
    └─────────────────────┬──────────────────────────────────┘
                          │
                          ▼
    ┌────────────────────────────────────────────────────────┐
    │                  open-mainframe-cobol                  │
    │  - COBOL Program execution (e.g. CardDemo)             │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `terminal` | `TerminalModel` geometry, screen buffer coordinates, and bounds checks |
| `fields` | `FieldTable`, `FieldAttribute`, extended colors, and MDT management |
| `oia` | Operator Information Area status line model and rendering |
| `theme` | `ColorTheme` palettes (Green, Amber, White, FullColor) |
| `keymap` | Crossterm `KeyEvent` mapping to 3270 AID keys and editing actions |
| `session` | Interactive `Session` controller, run loop, and `SessionConfig` |
| `events` | `EventSource` trait, `CrosstermEventSource`, and `MockEventSource` |
| `dbcs` | DBCS Shift-In/Shift-Out scanner and character width calculations |
| `render` | Ratatui rendering pipelines for 3270 character matrix and OIA |
| `snapshot` | Terminal screen buffer snapshot and ASCII art serialization |
| `error` | `TuiError` error definitions |

## Public API

### Primary Types and Functions

- `Session`: Main interactive session controller (`new()`, `run()`, `step()`, `stop()`).
- `SessionConfig`: Session configuration options (`model`, `theme`, `initial_transid`).
- `TerminalModel`: Display models (`Model2`, `Model3`, `Model4`, `Model5`).
- `FieldTable`: Manages list of 3270 screen fields, cursor position, and MDT states.
- `ColorTheme`: Color palettes (`Green`, `Amber`, `White`, `FullColor`).
- `Oia`: Operator Information Area state (`system_lock`, `insert_mode`, `cursor_pos`).
- `Aid`: Attention Identifier keys (`Enter`, `Pf(1..=24)`, `Pa(1..=3)`, `Clear`).
- `setup_terminal()` / `restore_terminal()`: Terminal raw mode initialization and cleanup.
- `MockTerminal` / `MockEventSource`: Scriptable mock terminal harness for integration testing.

## Integration

- **Internal workspace dependencies**:
  - `open-mainframe-cics`: CICS `TerminalManager` and `ScreenBuffer` interfaces.
  - `open-mainframe-cobol`: COBOL runtime data models.
  - `open-mainframe-runtime`: Language Environment error definitions.
- **Consumers**:
  - `open-mainframe-zosmf`: Uses `open-mainframe-tui` structures for CICS 3270 REST terminal session state in `crates/open-mainframe-zosmf/src/handlers/cics.rs`.
  - `open-mainframe`: CLI binary entry point for launching interactive full-screen CICS sessions (`open-mainframe cics`).

## Examples

### Running an Interactive Terminal Session

```rust,no_run
use open_mainframe_tui::session::{Session, SessionConfig};
use open_mainframe_tui::terminal::TerminalModel;
use open_mainframe_tui::theme::ColorTheme;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = SessionConfig {
        model: TerminalModel::Model2,
        theme: ColorTheme::Green,
        initial_transid: Some("CC00".to_string()),
        ..Default::default()
    };

    let mut session = Session::new(config)?;
    session.run()?;
    Ok(())
}
```

### Scripting Input with MockTerminal

```rust
use open_mainframe_tui::events::MockEventSource;
use open_mainframe_tui::terminal::TerminalModel;
use open_mainframe_tui::session::{Session, SessionConfig};

let mut mock_events = MockEventSource::new();
mock_events.queue_typing("USER01");
mock_events.queue_tab();
mock_events.queue_typing("PASSWORD");
mock_events.queue_enter();

// Session can process queued mock events deterministically in tests
```

## Testing

Run unit and integration tests:

```bash
cargo test -p open-mainframe-tui
```

The test suite contains 139 tests:
- 99 unit tests in `src/lib.rs` and submodules (field navigation, MDT setting, DBCS shifts, OIA formatting).
- 14 pipeline tests in `tests/callback_pipeline_tests.rs`.
- 6 mock terminal tests in `tests/mock_terminal_tests.rs`.
- 10 session interactive tests in `tests/session_interactive_tests.rs`.
- 10 screen snapshot verification tests in `tests/snapshot_tests.rs`.

## Limitations

- **Host Terminal Window Size**: If the host terminal window is smaller than the requested 3270 model geometry (e.g. 80×24 for Model 2), text will wrap or clip; automatic font scaling is constrained by the host terminal emulator.
- **Color Fidelity**: Extended 3279 colors map to ANSI 256-color or RGB escape sequences; appearance may vary across different host terminal terminal themes.
- **Light Pen & Magnetic Card Reader**: Physical 3270 peripheral hardware inputs are not simulated.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
- [open-mainframe-zosmf](../open-mainframe-zosmf/README.md)
