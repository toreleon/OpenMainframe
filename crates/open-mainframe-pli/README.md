# open-mainframe-pli

Enterprise PL/I (Programming Language One) lexer, context-sensitive parser, type system, interpreter, and built-in function library for OpenMainframe.

## Purpose

`open-mainframe-pli` models the IBM Enterprise PL/I for z/OS compiler front-end and runtime environment. It addresses PL/I's unique context-sensitive grammar (no reserved keywords), implements structured types (fixed/float decimal, binary, character, bit, picture, pointers, structures), handles hierarchical scopes (`PROCEDURE`, `BEGIN`), manages the ON-unit condition system, and executes PL/I programs via an AST interpreter.

## Capabilities

- **Lexer & Context-Sensitive Tokenizer (`lexer.rs`)**:
  - Handles string literals with doubled quotes (`'O''Connor'`), bit strings (`'1010'B`), hex strings (`'FF'X`), and nested C-style block comments (`/* ... */`).
  - Keyword classification helper (`is_keyword`) allowing keywords to serve freely as identifiers, labels, or procedure names without grammar conflicts.
- **Parser & AST (`parser.rs`)**:
  - Declarations: `DECLARE` / `DCL` with data attributes (`FIXED`, `FLOAT`, `DECIMAL`, `BINARY`, `CHARACTER`, `BIT`, `PICTURE`, `POINTER`, `OFFSET`, `AREA`, `FILE`, `ENTRY`), storage classes (`AUTOMATIC`, `STATIC`, `CONTROLLED`, `BASED`), initial values (`INITIAL`), dimensions, and level numbers for structures (levels 1, 2, 3...).
  - Control Flow: `IF/THEN/ELSE`, `DO` loops (iterative with `TO`/`BY`, `WHILE`, `UNTIL`, simple), `SELECT/WHEN/OTHERWISE/END`, `LEAVE`, `ITERATE`, `GOTO`.
  - Procedures & Blocks: `PROCEDURE` with `OPTIONS(MAIN)`, `RECURSIVE`, `RETURNS`, `BEGIN/END` blocks, `CALL`, `RETURN`.
  - Stream & Record I/O: `GET LIST`, `PUT LIST`, `GET EDIT`, `PUT EDIT`, `GET DATA`, `PUT DATA`, `OPEN`, `CLOSE`, `READ`, `WRITE`.
  - Exception Control: `ON <condition> [SNAP] <action>`, `SIGNAL <condition>`, `REVERT <condition>`.
  - Dynamic Memory: `ALLOCATE`, `FREE`.
- **Type System & Conversions (`types.rs`)**:
  - `PliType` and runtime `PliValue` supporting implicit type coercion between numeric formats (decimal ↔ binary, fixed ↔ float) and string/character buffers.
  - Structure layout tracking (`StructureMember`).
- **ON-Unit Condition Manager (`exceptions.rs`)**:
  - Standard condition hierarchy: `ERROR`, `FINISH`, `ZERODIVIDE`, `FIXEDOVERFLOW`, `OVERFLOW`, `UNDERFLOW`, `CONVERSION`, `ENDFILE`, `ENDPAGE`, `KEY`, `NAME`, `RECORD`, `TRANSMIT`, `UNDEFINEDFILE`, `STRINGRANGE`, `STRINGSIZE`, `SUBSCRIPTRANGE`, `AREA`, `ATTENTION`, `STORAGE`, `INVALIDOP`, and `UserDefined(String)`.
  - Condition inquiry functions: `ONCODE`, `ONLOC`, `ONFILE`, `ONKEY`, `ONCOUNT`.
- **Interpreter Execution Engine (`interpreter.rs`)**:
  - Block scoping with local variable frames (`Scope`).
  - Recursive procedure calls with argument passing and expression evaluation.
  - Console stream I/O buffer capture (`PUT LIST`).
- **50+ Built-in Functions (`builtins.rs`)**:
  - String: `SUBSTR`, `INDEX`, `LENGTH`, `VERIFY`, `TRANSLATE`, `TRIM`, `REPEAT`, `REVERSE`, `CENTER`, `LEFT`, `RIGHT`, `COLLATE`.
  - Math: `ABS`, `MOD`, `SIGN`, `SQRT`, `EXP`, `LOG`, `LOG10`, `LOG2`, `SIN`, `COS`, `TAN`, `ATAN`, `CEIL`, `FLOOR`, `ROUND`, `TRUNC`, `MAX`, `MIN`.
  - Date & Time: `DATE`, `TIME`, `DATETIME`, `DAYS`, `SECS`.
  - Conversion: `BINARY`, `DECIMAL`, `FIXED`, `FLOAT`, `CHARACTER`, `BIT`, `HEX`, `UNSPEC`.

## Architecture

```
    PL/I Source Text                      Execution Environment
    ┌──────────────┐                      ┌────────────────────┐
    │ DECLARE ...  │    Lexing/Parsing    │    Interpreter     │
    │ GET LIST...  │ ──────────────────>  │    (Recursive)     │
    │ ON ERROR...  │    Lexer / Parser    │  Context, Scopes   │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Type System       ┌────────────────────┐
    │  Data Types  │ ──────────────────>  │   Condition Mgr    │
    │  Dec, Char   │    PliType/PliValue  │   ON-units, SIGNAL │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Standard Lib      ┌────────────────────┐
    │  Built-ins   │ <──────────────────  │     I/O Buffer     │
    │  SUBSTR, ADDR│    call_builtin      │      PUT LIST      │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `parser.rs` | ~2 250 | Context-sensitive parser for all PL/I statements, declarations, and expressions |
| `interpreter.rs` | ~1 380 | AST interpreter, execution stack, procedure calling, scoping, PUT LIST |
| `builtins.rs` | ~940 | 50+ PL/I built-in functions (string, math, date, conversion) |
| `exceptions.rs` | ~740 | Exception handling: 25+ condition tokens, ON-unit scopes, SIGNAL/REVERT |
| `types.rs` | ~690 | PL/I data type system, `PliValue` variants, coercion and conversion rules |
| `lexer.rs` | ~580 | Tokenizer handling string/bit/hex literals, comments, and identifiers |

## Public API

### Primary Types and Functions

- `Lexer`: `Lexer::new(source).tokenize() -> Result<Vec<Token>, LexerError>`.
- `Parser`: `Parser::parse(tokens) -> Result<Program, ParseError>`.
- `Interpreter`: State machine executing programs with `new()`, `run(&mut self, program: &Program) -> Result<(), InterpreterError>`, `output(&self) -> &[String]`, and `get_var(&self, name) -> Option<&PliValue>`.
- `PliType` / `PliValue`: Runtime type representations (FixedDecimal, FloatDecimal, FixedBinary, FloatBinary, Character, Bit, Pointer).
- `ConditionManager`: Condition handling with `establish(&mut self, condition, action)` and `signal(&mut self, condition)`.
- `Condition`: Enumeration of 25+ standard PL/I runtime conditions.
- `call_builtin(name, args) -> BuiltinResult`: Direct invocation of PL/I built-ins.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-pli` depends only on external crates (`miette`, `thiserror`, `serde`, `tracing`).

### Workspace Consumers

- Direct workspace member providing PL/I parsing and execution facilities for multi-language applications and Language Environment integration.

## Examples

### Parsing and Executing a PL/I Procedure

```rust
use open_mainframe_pli::{Lexer, Parser, Interpreter};

let source = r#"
    SAMPLE: PROCEDURE OPTIONS(MAIN);
        DECLARE NAME CHARACTER(20);
        DECLARE AGE  FIXED DECIMAL(3);
        NAME = 'SMITH';
        AGE = 45;
        PUT LIST('NAME:', NAME, 'AGE:', AGE);
    END SAMPLE;
"#;

let tokens = Lexer::new(source).tokenize().expect("Lexer error");
let program = Parser::parse(tokens).expect("Parse error");

let mut interpreter = Interpreter::new();
interpreter.run(&program).expect("Runtime error");

assert_eq!(interpreter.output().len(), 1);
assert!(interpreter.output()[0].contains("SMITH"));
```

### Exception Signaling with `ConditionManager`

```rust
use open_mainframe_pli::exceptions::{Condition, ConditionManager, OnAction};

let mut cm = ConditionManager::new();
cm.establish(&Condition::Zerodivide, OnAction::SystemDefault);

let result = cm.signal(&Condition::Zerodivide).unwrap();
```

## Testing

The crate includes 184 unit and integration tests verifying context-sensitive grammar parsing, decimal type coercion, iterative DO loops, ON-unit handling, and all built-in functions:

```bash
cargo test -p open-mainframe-pli
```

Key test locations:
- `src/parser.rs` — No-reserved-word identifier disambiguation, complex DECLARE attribute strings, nested BEGIN/PROCEDURE blocks.
- `src/interpreter.rs` — Arithmetic type promotion, PUT LIST formatting, recursive procedure calls, and iterative loops.
- `src/exceptions.rs` — Scope-dependent ON-unit stacking, condition inquiry built-ins, and REVERT restoration.
- `src/builtins.rs` — Complete verification of string, math, date, and conversion functions.

## Limitations

- **Preprocessor**: The PL/I macro preprocessor (`%INCLUDE`, `%DECLARE`, `%IF`) is partially implemented; preprocessor statements are recognized by the parser but not evaluated dynamically before lexical analysis.
- **Record I/O Mode**: Stream I/O (`GET/PUT LIST`) is implemented; record I/O (`READ/WRITE/LOCATE`) operates on simulated memory buffers rather than VSAM/QSAM dataset connections.
- **Multitasking**: PL/I multitasking constructs (`EVENT`, `TASK`, `PRIORITY`, `WAIT`) are parsed as syntax but not dispatched onto OS threads.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-runtime](../open-mainframe-runtime/README.md) — Language Environment runtime and ILC calling conventions.
- [open-mainframe-cobol](../open-mainframe-cobol/README.md) — Companion mainframe compiled language.
