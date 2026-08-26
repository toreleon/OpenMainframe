# open-mainframe-natural

Software AG Natural 4GL — parser, interpreter, simulated ADABAS and SQL database access, reporting engine, interactive maps, work files, and security environment for the OpenMainframe project.

## Purpose

Natural is a Fourth-Generation Language (4GL) widely used on IBM mainframes for building business applications, traditionally paired with the ADABAS database or relational SQL databases. The `open-mainframe-natural` crate provides a complete Natural 4GL execution stack in Rust, modeling language parsing, runtime data structures, control flow, database interactions, formatted reporting, interactive map simulation, and administrative environment services.

## Capabilities

- **Source Parsing**: Lexer and recursive descent parser (`parse_natural`, `Lexer`, `Parser`) supporting Natural programs, subprograms (`CALLNAT`), subroutines (`PERFORM`), and copycode (`INCLUDE`).
- **Data Model**: Full type system supporting 11 Natural data types (`A` alphanumeric, `N` numeric, `P` packed decimal, `I` integer, `F` float, `B` binary, `D` date, `T` time, `L` logical, `C` control, `U` unicode), variable definitions via `DEFINE DATA` (Local, Global, Parameter), multi-dimensional arrays, and nested group structures.
- **Interpreter & Control Flow**: Stack-based execution engine (`NaturalInterpreter`) supporting conditional branches (`IF/ELSE`, `DECIDE ON`, `DECIDE FOR FIRST`), loops (`FOR`, `REPEAT UNTIL`), loop exits (`ESCAPE TOP`, `ESCAPE BOTTOM`), subprogram calls (`CALLNAT`), subroutines (`PERFORM`), error traps (`ON ERROR`), and transaction boundaries (`END TRANSACTION`, `BACKOUT TRANSACTION`).
- **Data Manipulation**: Built-in statement handlers for arithmetic and string processing (`COMPUTE`, `MOVE`, `COMPRESS`, `SEPARATE`, `EXAMINE`, `sort_records`, `move_edited`, `round_decimal`).
- **ADABAS Database Access**: Schema modeling and query evaluation via Data Definition Modules (`Ddm`, `AdabasFile`, `AdabasRecord`), supporting `FIND`, `READ`, `HISTOGRAM`, `STORE`, `UPDATE`, and `DELETE` operations with ISN (Internal Sequence Number) and descriptor matching.
- **SQL Access**: In-memory relational database emulation (`InMemorySql`, `SqlConnection`, `SqlCondition`) supporting `SELECT`, `INSERT`, `UPDATE`, and `DELETE` queries with cursor iteration and transaction commit/rollback.
- **Reporting & Output**: Columnar reporting engine (`ReportEngine`, `ColumnDef`, `ControlBreak`, `Alignment`) executing `DISPLAY`, `WRITE`, `PRINT`, page headers/footers, control breaks with automatic subtotaling/counting, and page ejects (`NEWPAGE`).
- **Interactive Maps**: Screen mapping framework (`MapDefinition`, `MapField`, `PfKey`, `TerminalSimulator`) supporting `INPUT USING MAP`, field protection, color attributes, and `REINPUT` validation flows.
- **Built-in Functions & System Variables**: Library of 25+ built-in functions (`eval_builtin`: string, mathematical, and date transformations) and 70+ Natural system variables (`*TIME`, `*USER`, `*PROGRAM`, `*DAT4E`, `*ISN`, `*COUNTER`, `*PAGE-NUMBER`, etc.).
- **Work Files & Environment**: Work file manager (`WorkFileManager`, `WorkFile`) supporting up to 32 sequential work files, and administrative environment services (`LibraryManager`, `NaturalLibrary`, `NaturalSecurity`, `SecurityProfile`, `EntireXBroker`).

## Architecture

```
     Natural Source Text
      (Programs / Subprograms / Maps)
                 │
                 ▼
       ┌───────────────────┐
       │   Parser/Lexer    │  (NAT-100)
       │  (parse_natural)  │
       └─────────┬─────────┘
                 │ Program AST
                 ▼
       ┌───────────────────┐        ┌───────────────────┐
       │NaturalInterpreter │ <────> │   VariablePool    │  (NAT-101: 11 Data Types)
       │(Call Stack, Flow) │        │ (Global/Local/Par)│
       └─────────┬─────────┘        └───────────────────┘
                 │
     ┌───────────┼───────────┬──────────────┬──────────────┐
     │           │           │              │              │
     ▼           ▼           ▼              ▼              ▼
┌─────────┐ ┌─────────┐ ┌─────────┐   ┌───────────┐  ┌───────────┐
│ADABAS   │ │SQL      │ │Reporting│   │Maps / I/O │  │Work Files │
│(Ddm,    │ │(In-Mem  │ │(Report  │   │(Terminal  │  │(WorkFile  │
│ Adabas) │ │ SqlConn)│ │ Engine) │   │ Simulator)│  │ Manager)  │
│NAT-104  │ │NAT-105  │ │NAT-106  │   │NAT-107    │  │NAT-109    │
└─────────┘ └─────────┘ └─────────┘   └───────────┘  └───────────┘
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `parser` | NAT-100: Lexer, recursive-descent parser, AST definitions (`Program`, `Statement`, `Expression`, `Token`) |
| `data_model` | NAT-101: 11 data types (`NaturalType`), runtime variant values (`NaturalValue`), type descriptors (`TypeSpec`), and variable scopes (`VariablePool`) |
| `interpreter` | NAT-102: Execution engine (`NaturalInterpreter`), subprogram registry (`NaturalObject`, `ObjectType`), and runtime state |
| `manipulation` | NAT-103: Data transformation verbs (`compress`, `examine`, `separate`, `sort_records`, `SortDirection`) |
| `adabas_access` | NAT-104: ADABAS access emulation (`AdabasFile`, `AdabasRecord`, `Ddm`, search criteria evaluation) |
| `sql_access` | NAT-105: SQL access emulation (`InMemorySql`, `SqlConnection`, `SqlCondition`, table CRUD) |
| `output` | NAT-106: Report formatting (`ReportEngine`, `ColumnDef`, `ControlBreak`, `Alignment`) |
| `maps` | NAT-107: Interactive 3270 map simulation (`MapDefinition`, `MapField`, `PfKey`, `TerminalSimulator`) |
| `functions` | NAT-108: Built-in function evaluator (`eval_builtin` for character, date, and math operations) |
| `sysvars` | NAT-108: Natural system variable access (`all_system_variables`, `get_system_variable`) |
| `workfiles` | NAT-109: Work file I/O (`WorkFile`, `WorkFileManager`) and error handler (`ErrorHandler`) |
| `environment` | NAT-110: Library management (`LibraryManager`), security profiles (`NaturalSecurity`), and RPC broker (`EntireXBroker`) |

## Public API

### Primary Types and Functions

- `parse_natural(src: &str, name: &str) -> Result<Program, String>`: Parse Natural source code into an AST `Program`.
- `NaturalInterpreter`: Execution engine managing global and local variable state, call stacks, registered subprograms, and standard output lines.
  - `execute(&mut self, program: &Program) -> Result<(), InterpreterError>`: Execute a parsed program.
  - `register_program(&mut self, obj: NaturalObject)`: Register a callable subprogram or subroutine.
  - `output_lines: Vec<String>`: Captured output lines from `DISPLAY`, `WRITE`, and `PRINT` statements.
- `NaturalValue`: Runtime value representation supporting numeric conversions (`to_f64`, `to_i64`), display formatting (`to_display_string`), and arithmetic.
- `Ddm`: Data Definition Module parser and schema representation for ADABAS files.
- `InMemorySql` / `SqlConnection`: In-memory relational database engine for SQL statement execution.
- `ReportEngine`: Formatted report builder supporting columns, alignments, and control breaks.
- `TerminalSimulator`: Map layout and field input simulator for interactive screens.
- `WorkFileManager`: Sequential work file manager handling up to 32 numbered work files.

## Integration

### Internal Workspace Dependencies

- `thiserror`: Error derive macros.
- `miette`: Diagnostic reporting.

### Workspace Consumers

- `open-mainframe-wiki`: Documented in the language catalog.
- Root workspace member in `Cargo.toml`.

## Examples

### Parsing and Executing a Natural Program

```rust
use open_mainframe_natural::{parse_natural, NaturalInterpreter};

fn main() {
    let source = r#"
        COMPUTE #X = 10
        COMPUTE #Y = 20
        COMPUTE #SUM = #X + #Y
        DISPLAY 'TOTAL:' #SUM
    "#;

    let program = parse_natural(source, "CALC").expect("failed to parse");
    let mut interpreter = NaturalInterpreter::new();
    interpreter.execute(&program).expect("execution error");

    assert_eq!(interpreter.output_lines, vec!["TOTAL: 30"]);
}
```

### Parsing a DDM and Simulating Data Access

```rust
use open_mainframe_natural::adabas_access::{AdabasFile, AdabasRecord, Ddm};
use open_mainframe_natural::data_model::NaturalValue;

fn main() {
    let ddm_text = r#"
        1 CUSTOMERS
          2 AA ID (A10)
          2 AB NAME (A30)
          2 AC BALANCE (N8.2)
    "#;

    let ddm = Ddm::parse(ddm_text).expect("invalid DDM");
    assert_eq!(ddm.file_name, "CUSTOMERS");
    assert_eq!(ddm.fields.len(), 3);

    let mut file = AdabasFile::new("CUSTOMERS");
    let mut record = AdabasRecord::new(1); // ISN 1
    record.fields.insert("NAME".to_string(), NaturalValue::Alpha("ACME CORP".to_string()));
    file.store(record);

    let found = file.read_by_isn(1).expect("record not found");
    assert_eq!(found.fields.get("NAME").unwrap().to_display_string(), "ACME CORP");
}
```

## Testing

Run the test suite for this crate:

```sh
cargo test -p open-mainframe-natural
```

The crate contains 326 unit tests verifying:
- `parser`: Lexing of keywords, decimal literals, system variables, strings, comments; statement parsing for all verbs and expressions.
- `data_model`: Type conversions, arithmetic precision, packed/numeric representations, and variable pools.
- `interpreter`: Control flow (`IF`, `FOR`, `REPEAT`, `DECIDE`), subprogram execution (`CALLNAT`), and error handling.
- `manipulation`: String formatting (`COMPRESS`, `SEPARATE`, `EXAMINE`), record sorting, and edited moves.
- `adabas_access` & `sql_access`: DDM parsing, ISN search, table CRUD operations, and transaction commit/rollback.
- `output` & `maps`: Column formatting, control break subtotals, map definitions, and PF key handling.
- `sysvars` & `workfiles`: System variable lookups, work file I/O operations, and error trapping.

## Limitations

- **Simulated ADABAS Layer**: The ADABAS integration in this crate is an in-memory emulation and does not directly invoke the `open-mainframe-adabas` nucleus ACB interface.
- **In-Memory SQL**: SQL statements are executed against an embedded in-memory table store rather than an external RDBMS or DRDA network protocol.
- **Terminal Rendering**: Screen interaction is modeled via `TerminalSimulator` rather than generating raw 3270 EBCDIC data streams.
- **EntireX RPC**: The RPC broker is an in-memory message dispatcher without TCP/IP network sockets.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-adabas](../open-mainframe-adabas/README.md)
