# open-mainframe-runtime

Language Environment (LE) runtime services and COBOL/PL/I execution support for OpenMainframe.

## Purpose

`open-mainframe-runtime` implements the z/OS Language Environment (LE) runtime model in Rust. It provides essential services for running compiled and interpreted mainframe applications, including decimal and binary arithmetic, string inspection and manipulation verbs, Lilian date/time calculations, LE callable services (CEExxx), condition/abend handling and formatted dumps, dynamic heap allocation, locale conventions, and inter-language communication (ILC).

## Capabilities

- **Language Environment (LE) Callable Services**:
  - Date & Time: `CEEDAYS` (date to Lilian), `CEEDATE` (Lilian to date string), `CEEDATM`, `CEEDYWK` (day of week), `CEEGMT`, `CEEGMTO`, `CEEISEC`, `CEELOCT`, `CEESECI`, `CEESECS`, `CEE3DLY` (delay).
  - Math Services: 30+ `CEESxxx` routines (`cees_sin`, `cees_cos`, `cees_tan`, `cees_log`, `cees_exp`, `cees_sqt`, `cees_mod`, `cees_xpx`, etc.) with single, double, and extended precision math.
  - Message & Condition: `CEENCOD`, `CEEDCOD`, `CEEMSG`, `CEEMOUT`, `CEEMGET` condition token and message catalog services.
  - Bit Operations: `ceesiclr`, `ceesiset`, `ceesishf`, `ceesitst`.
  - Locale: `CEE3LCT` and locale definition tables (`LocaleDefinition`, `LocaleManager`).
- **COBOL Verb Operations**:
  - String manipulation: `INSPECT TALLYING`, `INSPECT REPLACING`, `INSPECT CONVERTING`, `STRING`, `UNSTRING`.
  - Numeric display formatting: `format_numeric` with PIC edit masks (`Z`, `*`, `$`, `+`, `-`, `CR`, `DB`, commas, decimal points).
  - Terminal and console I/O: `display`, `display_to_writer`, `accept`, `accept_from_reader`.
  - COBOL `SORT` statement verb integration via `open-mainframe-sort`.
- **Value & Storage Abstractions**:
  - `CobolValue` and `NumericValue` variant types with arithmetic and comparison methods.
  - `StorageFormat` and `StoredNumeric` handlers for COMP-3 (packed), COMP (binary), and DISPLAY (zoned) buffers.
- **Error Recovery and Diagnostics**:
  - `ConditionToken` signaling and condition management.
  - Abend processing (`AbendCode`, `AbendInfo`, `ABEND_REGISTRY`).
  - Machine dumps (`MachineDump`), formatted dumps (`FormattedDump`), and snap dumps (`SnapDump`) capturing register states and call stacks.
- **Enclave and Process Lifecycles**:
  - Enclave lifecycle management and run-time options parser (`RuntimeOptions` for STACK, HEAP, TRAP, etc.).
- **Inter-Language Communication (ILC)**:
  - `IlcManager`, call stack frames (`IlcCallFrame`), and cross-language parameter descriptors (`ParameterDescriptor`, `PassingConvention`).

## Architecture

```
    Compiled / Interpreted Program        Language Environment (LE)
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Business Logic        │ ── Calls ──►    LE Math Services    │
    │  (COBOL / PL/I)        │            │    (CEESxxx)           │
    └────────────────────────┘            └────────────────────────┘
                                                       │
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Inter-Language        │ ── Logic ──►    ILC Manager         │
    │  Dispatch (ILC)        │            │    (COBOL <-> PL/I)    │
    └────────────────────────┘            └────────────────────────┘
                                                       │
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Error Handling        │ ── Signal ─►    Condition Manager   │
    │  (ON-Units, Handler)   │            │    (CEEDCOD, CEENCOD)  │
    └────────────────────────┘            └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────────┐            ┌────────────────────────┐
    │  Virtual Storage       │ <── SVC ───     Heap & Storage Mgr  │
    │  Decimal / Char Data   │            │    (GETMAIN/FREEMAIN)  │
    └────────────────────────┘            └────────────────────────┘
```

### Module Structure

| Module | Lines | Responsibility |
|---|---|---|
| `interpreter` | ~5 600 | Execution engine: AST interpretation of COBOL/PL/I logic |
| `file_io` | ~1 470 | Data adapter: Maps mainframe I/O to `open-mainframe-dataset` |
| `abend` | ~1 100 | System state capture, abend registry, and formatted dump generation |
| `date_time` | ~980 | Lilian date calculations and CEEDAYS/CEEDATE/CEELOCT services |
| `string` | ~660 | INSPECT, STRING, and UNSTRING verb implementations |
| `sort_verb` | ~630 | Runtime integration for COBOL SORT/MERGE statements |
| `numeric_editing` | ~600 | PIC edit mask formatting (Z, *, $, +, -, CR, DB) |
| `math` | ~600 | 30+ CEESxxx math routines and random number generation |
| `options` | ~560 | Runtime option configuration parsing (STACK, HEAP, TRAP) |
| `enclave` | ~535 | LE Enclave and Process lifecycle management |
| `condition` | ~525 | LE condition token creation, dispatch, and signaling |
| `message` | ~495 | LE message catalog and formatted message dispatch |
| `storage` | ~480 | COMP-3, COMP, and DISPLAY binary/decimal storage buffers |
| `ilc` | ~470 | Inter-language call frames and parameter translation |
| `heap` | ~440 | Dynamic heap memory manager (GETMAIN/FREEMAIN emulation) |
| `locale` | ~400 | Globalization, CEE3LCT, and currency/date formatting |
| `io` | ~300 | ACCEPT and DISPLAY console I/O |
| `value` | ~400 | Universal COBOL/PL/I runtime value representations |
| `bits` | ~150 | Bit manipulation services (CEESISxx) |
| `decimal` | ~400 | Decimal arithmetic primitives with rounding |
| `error` | ~100 | `RuntimeError` definitions with miette integration |

## Public API

### Core Functions & Types

- **Date & Time**: `ceedays`, `ceedate`, `ceedatm`, `ceedywk`, `ceeloct`, `date_to_lilian`, `lilian_to_date`, `is_leap_year`, `CenturyWindow`.
- **String Verbs**: `inspect_tallying`, `inspect_replacing`, `inspect_converting`, `string_concat`, `unstring`, `InspectMode`, `TallyingClause`, `ReplacingClause`.
- **Numeric Formatting**: `format_numeric(value, mask) -> Result<String, RuntimeError>`.
- **Math Services**: `call_le_math`, `cees_sin`, `cees_cos`, `cees_tan`, `cees_log`, `cees_sqt`, `cees_mod`, `Precision`.
- **Console I/O**: `display`, `display_to_writer`, `accept`, `accept_from_reader`, `DisplayOptions`, `DisplayTarget`.
- **Condition & Abend**: `ConditionToken`, `ceedcod`, `ceencod`, `ceemsg`, `AbendCode`, `AbendInfo`, `FormattedDump`, `MachineDump`, `RegisterSet`.
- **Storage & Values**: `CobolValue`, `NumericValue`, `StorageFormat`, `StoredNumeric`.
- **ILC**: `IlcManager`, `IlcCallFrame`, `ParameterDescriptor`, `PassingConvention`.
- **Options**: `RuntimeOptions`, `OptionValue`.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-encoding` — EBCDIC character translation, code page lookup, and decimal binary conversions.
- `open-mainframe-sort` — DFSORT-compatible sort/merge engine invoked by the COBOL `SORT` statement.

### Workspace Consumers

- `open-mainframe` — Top-level driver and CICS TUI runner.
- `open-mainframe-cobol` — Links against runtime services for intrinsic functions and code execution.
- `open-mainframe-tui` — Terminal display formatting and runtime session execution.
- `open-mainframe-zosmf` — REST server execution pipelines and abend dump capture.

## Examples

### COBOL-Style INSPECT TALLYING

```rust
use open_mainframe_runtime::string::inspect_tallying;

let source = "ABRACADABRA";
let count = inspect_tallying(source, "A", None).expect("Inspect failed");
assert_eq!(count, 5);
```

### Formatting Numeric Data with Edit Masks

```rust
use open_mainframe_runtime::numeric_editing::format_numeric;

let value = 1234.56;
let mask = "$Z,ZZ9.99";
let output = format_numeric(value, mask).unwrap();
assert_eq!(output, "$1,234.56");
```

### Date to Lilian Day Calculation

```rust
use open_mainframe_runtime::date_time::{date_to_lilian, lilian_to_date};

// October 15, 1582 is Lilian Day 1
let lilian = date_to_lilian(1582, 10, 15).unwrap();
assert_eq!(lilian, 1);

let (year, month, day) = lilian_to_date(lilian).unwrap();
assert_eq!((year, month, day), (1582, 10, 15));
```

### Console DISPLAY Output

```rust
use open_mainframe_runtime::{display_to_writer, CobolValue};

let mut output = Vec::new();
let values = vec![CobolValue::alphanumeric("HELLO FROM OPENMAINFRAME")];
display_to_writer(&values, &mut output, false).unwrap();
assert_eq!(String::from_utf8(output).unwrap(), "HELLO FROM OPENMAINFRAME\n");
```

## Testing

The runtime crate contains 439 unit and documentation tests verifying math precision, date algorithms, verb semantics, and dump generation:

```bash
cargo test -p open-mainframe-runtime
```

Key test locations:
- `src/date_time.rs` — Lilian day calculations, century windows, leap year transitions, and CEEDAYS/CEEDATE roundtrips.
- `src/string.rs` — INSPECT, STRING, and UNSTRING multi-delimiter parsing.
- `src/numeric_editing.rs` — Comprehensive PIC edit mask matrix ($ , . Z * + - CR DB).
- `src/math.rs` — Mathematical precision tests against standard mainframe outputs.
- `src/abend.rs` — Abend token lookup and formatted dump layout generation.
- `src/ilc.rs` — Multi-language frame dispatching.

## Limitations

- **Virtual Registers**: Hardware GPRs 0–15 and Access Registers (ARs) are emulated in virtual `Tcb` and `RegisterSet` structures.
- **Storage Keys**: Real z/Architecture page-level protection keys (0–15) are represented as logical ownership tags in memory buffers rather than MMU hardware keys.
- **SVC Dumps**: Abend dumps are generated as host files or written to logging streams rather than real MVS SYS1.DUMP datasets.
- **Debugger Invocation**: The `CEETEST` callable service is a stub and does not attach an interactive z/OS debugger.
- **Floating Point**: Internal math services use IEEE 754 64-bit floating point (`f64`) rather than hardware HFP emulation.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-encoding](../open-mainframe-encoding/README.md) — EBCDIC and decimal byte formats.
- [open-mainframe-cobol](../open-mainframe-cobol/README.md) — COBOL compiler.
- [open-mainframe-sort](../open-mainframe-sort/README.md) — Sorting engine for runtime SORT verbs.
