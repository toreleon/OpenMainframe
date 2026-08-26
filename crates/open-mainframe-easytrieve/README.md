# open-mainframe-easytrieve

Easytrieve Plus report-generation and data-extraction language parser, interpreter, report engine, and file I/O system for OpenMainframe.

## Purpose

`open-mainframe-easytrieve` models the CA Easytrieve Plus scripting and report generation environment on z/OS. It provides fixed-column source card parsing, declarative FILE and DEFINE layouts, JOB activity execution with arithmetic and nested control flow, multi-key record sorting and merge-join matching, automated control-break reporting with statistical summaries (SUM/COUNT/AVG/MIN/MAX), sequential and indexed file I/O with numeric edit masks, embedded SQL host variable extraction, and a macro/COPY expansion engine.

## Capabilities

- **Fixed-Column Parser & AST (`parser.rs`)**:
  - Handles Easytrieve card column boundaries: Columns 1–4 (line number), Columns 5–72 (statement text), Columns 73–80 (sequence number).
  - Recognizes 87 keywords and produces 46+ AST statement variants across `files`, `defines`, and `activities` sections.
  - Statements: `FILE`, `DEFINE`, `JOB`, `SORT`, `PUT`, `GET`, `PRINT`, `DISPLAY`, `HEADING`, `LINE`, `TITLE`, `IF`/`ELSE`/`ELSEIF`/`END-IF`, `DO`/`END-DO`, `GOTO`, `PERFORM`, `STOP`, `MACRO`/`END-MACRO`, `COPY`, `SQL`, `REPORT`, `SEQUENCE`, `CONTROL`, `SUM`, `CALL`, `CASE`/`WHEN`, `READ`, `WRITE`, `POINT`, `CLOSE`, `SEARCH`, `MASK`.
- **Interpreter Execution Engine (`interpreter.rs`)**:
  - Variable storage (`EzVariable`, `EzValue`), label indexing, and expression evaluation.
  - Control flow: IF/ELSE branching, DO loops, PERFORM subroutines, GOTO jumps, and runaway loop safety caps (100,000 steps).
- **Sequential & Keyed File I/O (`fileio.rs`)**:
  - `FileProcessor` for sequential GET/PUT record streaming.
  - `IndexedFileProcessor` for VSAM KSDS-style keyed READ, WRITE, and POINT navigation.
  - `EditMask` for numeric formatting (`9`, `Z`, `.`, `,`, `$`, `CR`, `-`).
  - `EzTable` in-memory lookup table with binary and sequential search.
- **Report Generation & Control Breaks (`report.rs`)**:
  - Formats multi-line headers, detail lines, page numbering, and title banners.
  - Control break detection (`ControlBreak`) with automatic level subtotals and final summaries (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`).
- **Sort & Merge-Join Matching (`sort.rs`)**:
  - `EzSort` multi-key record sorter with mixed ASCENDING/DESCENDING priorities.
  - `EzMatch` two-file merge-join supporting MATCHED, FILE1-ONLY, and FILE2-ONLY partitions.
- **Embedded SQL Bridge (`sql.rs`)**:
  - `SqlBridge` trait and `MockSqlBridge` backend.
  - Parses `:varname` host variables and compiles SQL statements into parameterized form (`?`).
- **Macro & Copybook Processor (`macros.rs`)**:
  - Macro definitions with `&PARAM` substitutions, recursion depth controls, and `COPY` member resolution.

## Architecture

```
┌───────────────────────────────────────────────────────────┐
│                    Public API (lib.rs)                    │
├──────────┬────────────┬────────────┬─────────────────────┤
│  Parser  │ Interpreter│   Report   │   File I/O          │
│          │            │            │                     │
│ parser   │ interpreter│ report     │ fileio              │
├──────────┴────────────┴────────────┼─────────────────────┤
│      Sort / Match / Control Break  │   SQL Bridge        │
│                                    │                     │
│ sort                               │ sql                 │
├────────────────────────────────────┴─────────────────────┤
│               Macros / COPY / External Calls             │
│ macros                                                   │
├──────────────────────────────────────────────────────────┤
│          miette (diagnostics)  │  thiserror (errors)     │
└────────────────────────────────┴─────────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `parser.rs` | ~1 325 | Fixed-column lexer, 87 keywords, 46+ statement AST types |
| `fileio.rs` | ~930 | Sequential/indexed file I/O, records, edit masks, in-memory tables |
| `interpreter.rs` | ~760 | Variable storage, arithmetic, control flow execution |
| `report.rs` | ~690 | Report formatting, page control, control breaks, summaries |
| `sort.rs` | ~610 | Multi-key SORT, sorted merge-join MATCH, control breaks |
| `macros.rs` | ~520 | MACRO/END-MACRO, COPY library, external CALL |
| `sql.rs` | ~390 | Embedded SQL blocks, host variables, SqlBridge trait |

## Public API

### Primary Types and Functions

- `EzParser`: `EzParser::tokenize(source)` and `EzParser::parse(source) -> Result<EzProgram, EzError>`.
- `EzInterpreter`: Executes AST programs with `new()`, `execute(&mut self, program: &EzProgram) -> Result<(), EzError>`, and inspectable `output: Vec<String>`.
- `FileProcessor` / `IndexedFileProcessor`: Record buffers, field extraction, and sequential/keyed file processing.
- `EditMask`: Formats numeric values with mask templates.
- `ReportDef` / `ReportFormatter`: Formatted report engine with `print_detail(&values)` and `print_summary()`.
- `EzSort` / `EzMatch`: Record sorting and merge-join engine.
- `SqlBridge` / `MockSqlBridge` / `EzSqlBlock`: Embedded SQL parsing and bridge interface.
- `EzMacro` / `MacroLibrary` / `CopyLibrary`: Macro expansion and COPY resolution.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-easytrieve` depends only on external crates (`miette`, `thiserror`, `serde`, `tracing`).

### Workspace Consumers

- Standalone workspace member providing report generation, data extraction, and batch migration capabilities.

## Examples

### Parsing and Interpreting an Easytrieve Program

```rust
use open_mainframe_easytrieve::{EzParser, EzInterpreter};

let source = r#"
    DEFINE WS-TOTAL W 5 N
    JOB INPUT NULL
    WS-TOTAL = 100 + 50
    DISPLAY 'TOTAL IS: ' WS-TOTAL
    STOP
"#;

let program = EzParser::parse(source).expect("Parse error");
let mut interp = EzInterpreter::new();
interp.execute(&program).expect("Runtime error");

assert_eq!(interp.output, vec!["TOTAL IS: 150"]);
```

### Formatting Numeric Values with Edit Masks

```rust
use open_mainframe_easytrieve::EditMask;

let mask = EditMask::new("SALARY-MASK", "ZZZ,ZZ9.99");
let formatted = mask.format(50000.0);
assert_eq!(formatted.trim(), "50,000.00");
```

### Generating a Summary Report

```rust
use open_mainframe_easytrieve::{ReportDef, ReportFormatter};
use std::collections::HashMap;

let mut report = ReportDef::new("SALESRPT");
report.add_heading(1, vec!["SALES SUMMARY".into()]);
report.add_line(1, vec!["REGION".into(), "AMOUNT".into()]);
report.set_sum_fields(vec!["AMOUNT".into()]);

let mut formatter = ReportFormatter::new(report);
let mut row = HashMap::new();
row.insert("REGION".into(), "EAST".into());
row.insert("AMOUNT".into(), "1500.00".into());

formatter.print_detail(&row);
formatter.print_summary();
```

## Testing

The crate includes 92 unit and integration tests across all compiler and runtime modules:

```bash
cargo test -p open-mainframe-easytrieve
```

Key test locations:
- `src/parser.rs` — Fixed-column card layout, tokenization, FILE/DEFINE/JOB sections, and statement AST generation.
- `src/interpreter.rs` — Arithmetic operations, variable evaluation, DISPLAY formatting, and STOP handling.
- `src/fileio.rs` — Sequential GET/PUT, indexed CRUD/POINT, record layout offsets, and edit mask formatting.
- `src/report.rs` — Page headers, control break detection, and statistical sum accumulations.
- `src/sort.rs` — Multi-key record sorting and two-file merge-join matching.
- `src/sql.rs` — Host variable extraction, parameterized SQL generation, and mock backend query execution.
- `src/macros.rs` — Parameter substitution and COPY member expansion.

## Limitations

- **CASE/WHEN Runtime Execution**: CASE/WHEN statements are parsed into the AST, but runtime execution routes through IF/ELSE evaluation in the interpreter.
- **Live Database Driver**: `SqlBridge` provides the abstraction and mock test double; connections to live relational database drivers (e.g. DB2/PostgreSQL) require host driver crates.
- **External CALL Simulation**: `EzExternalCall::simulate()` provides simulated returns; dynamic loading of external shared libraries is not yet enabled.
- **Multi-Level Report Subtotals**: Control break summaries calculate group and final statistics; multi-level nested subtotals per intermediate break field are in development.
- **Statement Line Continuations**: Source lines follow the standard fixed-format layout without multi-line continuation operators.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-sort](../open-mainframe-sort/README.md) — DFSORT batch sort engine.
- [open-mainframe-dataset](../open-mainframe-dataset/README.md) — Dataset record I/O backend.
