# open-mainframe-focus

Information Builders FOCUS — multi-dialect 4GL with TABLE/GRAPH/MODIFY engines, Dialogue Manager, data adapters, and report formatting for the OpenMainframe project.

## Purpose

Information Builders FOCUS is a Fourth-Generation Language (4GL) and database management system widely deployed on IBM mainframes for reporting, analytics, interactive data entry, and application scripting. The `open-mainframe-focus` crate provides a complete Rust implementation of FOCUS, including multi-dialect parsing, Master File Descriptor (MFD) metadata management, table report generation with cross-tabulation, ASCII graph rendering, data maintenance (MODIFY/MAINTAIN), Dialogue Manager execution with amper variables, data source adapters, and multi-file join processing.

## Capabilities

- **Multi-Dialect Parsing**: Recursive descent parser (`parser.rs` / FOC-100) handling distinct sub-languages: `TABLE FILE` reporting, `GRAPH FILE` visualization, `MODIFY FILE` and `MAINTAIN FILE` data maintenance, Dialogue Manager control statements (`-SET`, `-IF`, `-GOTO`, `-TYPE`, `-READ`, `-RUN`, `-INCLUDE`), and SQL passthrough queries.
- **Master File Descriptors (MFD)**: Metadata engine (`mfd.rs` / FOC-101: `MfdParser`, `MasterFileDescriptor`, `Segment`, `FieldDef`, `FocusDataType`, `AccessFile`) parsing hierarchical segments, field aliases, usage/actual formats (`A`, `I`, `F`, `D`, `P`, `YYMD`, smart dates), and access file physical mapping.
- **Table Reporting Engine**: Report execution engine (`table_engine.rs` / FOC-102: `TableEngine`, `TableRequest`, `ReportOutput`, `ReportRow`, `CellValue`) supporting `PRINT`, `SUM`, `COUNT`, multi-level sorting (`BY`), cross-tabulation pivoting (`ACROSS`), `WHERE` filter expressions, `COMPUTE` derived fields, grand totals, and custom headings/footings/subfoots.
- **Graph Visualization**: Text-mode charting engine (`graph_engine.rs` / FOC-103: `GraphEngine`, `GraphOutput`, `ChartType` [Bar, Line, Pie, Area], `ChartFormat`) generating ASCII visualizations with value scaling and legends.
- **Data Maintenance**: Transactional data management engine (`modify_engine.rs` / FOC-104: `ModifyEngine`, `ModifyRequest`, `MaintainRequest`, `FixformField`, `ValidationRule`, `MatchAction`, `TransactionLog`) for batch and interactive updates with field validation and rollback logging.
- **Dialogue Manager**: FOCEXEC procedure interpreter (`dialogue.rs` / FOC-105: `DialogueInterpreter`, `AmperVariable`) supporting local (`&VAR`), global (`&&VAR`), and system (`&DATE`, `&USER`) variable substitution, conditional branching, loops (`-REPEAT UNTIL`), input queuing (`-READ`), and nested procedure calls (`-RUN`, `-INCLUDE`).
- **Function Library**: Built-in function registry (`functions.rs` / FOC-106: `FunctionRegistry`) implementing 20+ standard character, numeric, date, and trigonometric functions (`SUBSTR`, `TRIM`, `UPCASE`, `POSIT`, `ROUND`, `SQRT`, `TODAY`, etc.).
- **Data Adapters**: Pluggable storage abstraction layer (`adapters.rs` / FOC-107: `AdapterRegistry`, `DataAdapter`, `FocusNativeAdapter`, `SequentialAdapter`, `VsamAdapter`, `Db2Adapter`, `ImsAdapter`).
- **Output Formatting**: Multi-target report output formatters (`output.rs` / FOC-108: `TextFormatter`, `HtmlFormatter`, `HoldFormatter`) with alignment, table styling, and delimited HOLD file generation.
- **Multi-Source Joins & Merging**: Cross-dataset join processor (`joins.rs` / FOC-109: `JoinEngine`, `JoinDefinition`, `JoinType` [Inner, LeftOuter, FullOuter], `MatchFileOp`, `CombineOp`).
- **Host Integration**: Mainframe file definition and environment services (`filedef.rs` / FOC-110: `FileDefRegistry`, `FileDefEntry`, `DynamAllocation`, `TsoInterface`, `CicsInterface`).

## Architecture

```
       FOCEXEC / Dialogue Source
                   │
                   ▼
       ┌───────────────────────┐
       │     FocusParser       │  (FOC-100: Multi-Dialect Parser)
       │  (TABLE / GRAPH / DM) │
       └───────────┬───────────┘
                   │
         ┌─────────┼─────────────────────┐
         │ (TABLE) │ (GRAPH)             │ (DM)
         ▼         ▼                     ▼
   ┌───────────┐ ┌───────────┐     ┌─────────────────────┐
   │TableEngine│ │GraphEngine│     │ DialogueInterpreter │
   │ (FOC-102) │ │ (FOC-103) │     │ (FOC-105: AmperVars)│
   └─────┬─────┘ └─────┬─────┘     └─────────────────────┘
         │             │
         └──────┬──────┘
                ▼
   ┌─────────────────────────┐     ┌─────────────────────┐
   │    Metadata & MFD       │ <── │   Data Adapters     │
   │(MasterFileDescriptor)   │     │(Native/VSAM/DB2/Seq)│
   │        FOC-101          │     │       FOC-107       │
   └────────────┬────────────┘     └─────────────────────┘
                │
                ▼
   ┌─────────────────────────┐
   │    Output Formatters    │  (FOC-108: Text / HTML / HOLD)
   │ (Text / Html / Hold)    │
   └─────────────────────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `parser` | FOC-100: Lexer and multi-dialect parser (`FocusLexer`, `FocusToken`, `TableRequest`, `GraphRequest`, `MaintainRequest`, `DialogueCmd`, `SqlPassthrough`) |
| `mfd` | FOC-101: Master File Descriptor parser (`MfdParser`), metadata structures (`MasterFileDescriptor`, `Segment`, `FieldDef`, `FocusDataType`, `AccessFile`) |
| `table_engine` | FOC-102: Table reporting engine (`TableEngine`, `ReportOutput`, `ReportRow`, `CellValue`, `RowType`, `TableVerb`) |
| `graph_engine` | FOC-103: Text-mode charting engine (`GraphEngine`, `GraphOutput`, `ChartType`, `ChartFormat`) |
| `modify_engine` | FOC-104: Data maintenance engine (`ModifyEngine`, `ModifyRequest`, `MaintainRequest`, `FixformField`, `ValidationRule`, `MatchAction`, `TransactionLog`) |
| `dialogue` | FOC-105: Dialogue Manager interpreter (`DialogueInterpreter`, `AmperVariable`, `DmValue`) |
| `functions` | FOC-106: Built-in function library registry (`FunctionRegistry`) |
| `adapters` | FOC-107: Data source adapters (`AdapterRegistry`, `DataAdapter`, `FocusNativeAdapter`, `SequentialAdapter`, `VsamAdapter`, `Db2Adapter`, `ImsAdapter`) |
| `output` | FOC-108: Report formatters (`TextFormatter`, `HtmlFormatter`, `HoldFormatter`, `OutputFormatter`) |
| `joins` | FOC-109: Multi-source join processing (`JoinEngine`, `JoinDefinition`, `JoinType`, `MatchFileOp`, `CombineOp`) |
| `filedef` | FOC-110: Mainframe environment integration (`FileDefRegistry`, `FileDefEntry`, `DynamAllocation`, `TsoInterface`, `CicsInterface`) |

## Public API

### Primary Types and Functions

- `TableEngine`: Executes `TableRequest` queries against in-memory records, performing sorting, grouping, aggregations (`PRINT`, `SUM`, `COUNT`), pivoting (`ACROSS`), and compute evaluation.
  - `execute(request: &TableRequest, data: &[HashMap<String, CellValue>]) -> Result<ReportOutput, TableError>`
- `MfdParser`: Parses MFD text definitions into structured `MasterFileDescriptor` representations.
  - `parse_mfd(source: &str) -> Result<MasterFileDescriptor, MfdError>`
- `DialogueInterpreter`: Manages execution of Dialogue Manager scripts, variable substitution, procedure registration, and control flow.
  - `execute(&mut self, source: &str) -> Result<DialogueResult, DialogueError>`
  - `set_local(&mut self, name: &str, value: DmValue)` / `set_global(&mut self, name: &str, value: DmValue)`
- `GraphEngine`: Renders ASCII charts from table data:
  - `render(request: &GraphRequest, data: &[HashMap<String, CellValue>]) -> Result<GraphOutput, GraphError>`
- `ModifyEngine`: Applies batch transaction streams with `MATCH`/`NOMATCH` actions and validation rules.
- `TextFormatter` / `HtmlFormatter` / `HoldFormatter`: Formats `ReportOutput` structures into plain text tables, HTML pages, or delimited data files.

## Integration

### Internal Workspace Dependencies

- `thiserror`: Error derive macros.
- `miette`: Diagnostic reporting.

### Workspace Consumers

- `open-mainframe-wiki`: Documented in the language catalog.
- Root workspace member in `Cargo.toml`.

## Examples

### Executing a TABLE Report Request

```rust
use std::collections::HashMap;
use open_mainframe_focus::parser::{TableRequest, TableVerb};
use open_mainframe_focus::table_engine::{CellValue, TableEngine};
use open_mainframe_focus::output::{OutputFormatter, TextFormatter};

fn main() {
    let request = TableRequest {
        file: "SALES".to_string(),
        verb: TableVerb::Sum,
        fields: vec!["SALES".to_string()],
        by_dims: vec!["DEPT".to_string()],
        across_dims: vec![],
        where_clauses: vec![],
        computes: vec![],
        heading: Some("DEPARTMENT SALES REPORT".to_string()),
        footing: None,
        subfoot: None,
    };

    let data = vec![
        HashMap::from([
            ("DEPT".to_string(), CellValue::Str("ENG".to_string())),
            ("SALES".to_string(), CellValue::Num(100_000.0)),
        ]),
        HashMap::from([
            ("DEPT".to_string(), CellValue::Str("SALES".to_string())),
            ("SALES".to_string(), CellValue::Num(150_000.0)),
        ]),
    ];

    let report = TableEngine::execute(&request, &data).expect("table execution failed");
    let formatter = TextFormatter::new();
    let text = formatter.format(&report);
    println!("{}", text);
}
```

### Running Dialogue Manager Scripts

```rust
use open_mainframe_focus::dialogue::{DialogueInterpreter, DmValue};

fn main() {
    let mut interpreter = DialogueInterpreter::new();
    interpreter.set_local("REGION", DmValue::Str("NORTH".to_string()));

    let script = r#"
-TYPE Starting report for region &REGION
-SET &COUNT = 10
-IF &COUNT GT 5 -GOTO OK
-TYPE Count too low
OK:
-TYPE Processing &COUNT items
"#;

    let result = interpreter.execute(script).expect("dialogue execution failed");
    assert_eq!(result.output_messages, vec![
        "Starting report for region NORTH",
        "Processing 10 items",
    ]);
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-focus
```

The crate contains 257 unit tests verifying:
- `parser`: Dialect switching, expression parsing, Dialogue Manager commands, `TABLE` across/compute/where syntax.
- `mfd`: Segment hierarchy resolution, field type parsing, alias lookups, and access file mapping.
- `table_engine`: `PRINT`/`SUM` aggregations, multi-level `BY` sorting, `ACROSS` pivoting, grand totals, and `COMPUTE` division-by-zero handling.
- `graph_engine`: Bar, Line, Pie, and Area chart generation and value aggregation.
- `modify_engine`: Batch `MATCH`/`NOMATCH` actions, field validation rules, and transaction commit/rollback.
- `dialogue`: Amper variable resolution, `-IF`/`-GOTO` branching, `-REPEAT UNTIL` loops, and `-RUN`/`-INCLUDE` procedure chaining.
- `functions`: Built-in function evaluation for string, date, and mathematical functions.
- `joins`: Inner, Left Outer, Full Outer joins, and `MATCH FILE` set comparisons.
- `output`: Text alignment, HTML table rendering, CSS classes, and HOLD file formatting.

## Limitations

- **Simulated Storage Adapters**: Data adapters (`VsamAdapter`, `Db2Adapter`, `ImsAdapter`) provide in-memory data access abstractions rather than direct socket or memory bridges to the respective mainframe subsystem engines.
- **ASCII-Only Visualization**: The `GraphEngine` generates text-mode ASCII charts and does not produce vector graphics or raster image formats.
- **PDF Generation**: Output formatters currently support Plain Text, HTML, and HOLD files; direct PDF report generation is not implemented.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-natural](../open-mainframe-natural/README.md)
