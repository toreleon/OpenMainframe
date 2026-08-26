# open-mainframe-clist

z/OS Command List (CLIST) scripting language parser, interpreter, and TSO/ISPF bridge for OpenMainframe.

## Purpose

`open-mainframe-clist` models the TSO/E CLIST language execution environment used to automate interactive sessions, dataset operations, and batch subcommands under IBM z/OS TSO. It provides source-line continuation processing (+/-), tokenization, AST generation for ~30 CLIST statements, dynamic variable pools with global/nested scoping, built-in function evaluation (`&EVAL`, `&SUBSTR`, `&SYSINDEX`), terminal and sequential dataset I/O, `ERROR`/`ATTN` handlers, and TSO/ISPF command dispatching.

## Capabilities

- **Source Line Processor & Continuation**:
  - Handles CLIST line continuation conventions: trailing `+` (concatenates with leading spaces stripped) and `-` (preserves leading spaces on continuation line).
  - Comment stripping (`/* ... */`).
- **Parser & AST (`parser.rs`)**:
  - Statements: `PROC`, `SET`, `IF/THEN/ELSE`, `DO` (simple, counted, `WHILE`, `UNTIL`), `SELECT/WHEN/OTHERWISE`, `GOTO`, `EXIT`, `RETURN`, `WRITE`, `WRITENR`, `READ`, `OPENFILE`, `GETFILE`, `PUTFILE`, `CLOSFILE`, `ERROR`, `ATTN`, `CONTROL`, `GLOBAL`, `NGLOBAL`, `SYSREF`, `SYSCALL`, `EXEC`, `LISTDSI`, `ISPEXEC`, `ISREDIT`, `DATA/ENDDATA`, `TERMIN`.
  - Expressions: Arithmetic (`+`, `-`, `*`, `/`, `//`), comparison (`EQ`, `NE`, `LT`, `GT`, `LE`, `GE`, `=`, `^=`, `<`, `>`, `<=`, `>=`), Boolean (`AND`, `OR`, `NOT`, `&&`, `|`, `^`), concatenation (`||`).
- **Interpreter Core (`interpreter.rs`)**:
  - Variable substitution: `&VAR`, double ampersand evaluation (`&&VAR`), positional and keyword parameters (`PROC 2 DSN VOLUME(SYSRES)`).
  - System Variables: `&SYSDATE`, `&SYSTIME`, `&SYSUSERID`, `&SYSUID`, `&SYSPROC`, `&SYSLINE`, `&SYSNEST`, `&LASTCC`, `&MAXCC`.
  - Scoping & Subprocedures: `SYSCALL` with `SYSREF` reference passing and shared `GLOBAL` pools.
- **Built-in Functions (`functions.rs`)**:
  - 11 built-in functions: `&EVAL`, `&SUBSTR`, `&LENGTH`, `&SYSINDEX`, `&SYSCAPS`, `&SYSLC`, `&DATATYPE`, `&STR`, `&NRSTR`, `&SYSDSN`, `&SYSNSUB`.
- **I/O & Control Management (`io.rs`)**:
  - Terminal I/O: `WRITE`, `WRITENR` (no carriage return), `READ` (into variables).
  - Dataset file I/O: `OPENFILE` (`INPUT`, `OUTPUT`, `UPDATE`), `GETFILE`, `PUTFILE`, `CLOSFILE`.
  - Execution Control (`CONTROL`): `LIST/NOLIST`, `CONLIST/NOCONLIST`, `SYMLIST/NOSYMLIST`, `MSG/NOMSG`, `PROMPT/NOPROMPT`, `ASIS/CAPS`, `MAIN`, `FLUSH/NOFLUSH`, `END/NOEND`.
  - Condition Handling: `ERROR` routines (with `&LASTCC` inspection) and `ATTN` interrupt traps.
- **TSO & ISPF Integration (`tso_bridge.rs`)**:
  - `TsoEnvironment` trait and `MockTsoEnvironment` for command dispatching, `ISPEXEC`/`ISREDIT` execution, and `LISTDSI` dataset attribute queries.

## Architecture

```
                  ┌────────────────────────┐
                  │     CLIST Source       │
                  └──────────┬─────────────┘
                             │
                 ┌───────────▼────────────┐
                 │  Source Line Processor │
                 │  (+/- continuations)   │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │      Tokenizer         │
                 │  (11 token kinds)      │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │    Statement Parser    │
                 │  (~30 statement types) │
                 └───────────┬────────────┘
                             │
                 ┌───────────▼────────────┐
                 │       ClistAst         │
                 └───────────┬────────────┘
                             │
          ┌──────────────────▼──────────────────┐
          │         ClistInterpreter            │
          │                                     │
          │  VariablePool ─── System Variables  │
          │  Label Map    ─── Control Options   │
          │  Error/Attn Handlers                │
          │                                     │
          ├────────┬──────────┬─────────────────┤
          │ IoMgr  │BuiltinFn │  TsoEnvironment │
          └────────┴──────────┴─────────────────┘
```


## Public API

### Core Types and Functions

- `parse_clist(source: &str) -> Result<ClistAst, ParseError>`: Parses raw CLIST source cards into an AST.
- `tokenize_clist(line: &str) -> Result<Vec<ClistToken>, ParseError>`: Direct tokenizer access.
- `ClistInterpreter`: Core execution engine with:
  - `new() -> Self`
  - `with_globals(globals: Arc<Mutex<HashMap<String, String>>>) -> Self`
  - `execute(&mut self, source: &str) -> Result<i32, InterpreterError>`
  - `execute_ast(&mut self, ast: &ClistAst) -> Result<i32, InterpreterError>`
  - `output(&self) -> &[String]`
  - `set_tso(&mut self, tso: Box<dyn TsoEnvironment>)`
- `evaluate_builtin(func: BuiltinFunction, args: &[String]) -> Result<String, FunctionError>`: Direct built-in invocation.
- `TsoEnvironment` (trait) / `DatasetAttributes` / `ListdsiResult`: TSO environment bridge.
- `ControlOptions` / `ClistFile` / `FileMode` / `IoManager`: Runtime control and I/O descriptors.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-clist` depends only on external crates (`miette`, `thiserror`).

### Workspace Consumers

- Direct workspace member providing scripting execution services for TSO command processing, batch JCL IKJEFT01 steps, and ISPF dialogs.

## Examples

### Basic Script Execution

```rust
use open_mainframe_clist::ClistInterpreter;

let mut interp = ClistInterpreter::new();
let source = r#"
PROC 0
CONTROL NOLIST NOMSG
SET &SUM = 0
SET &I = 1
LOOP: IF &I GT 10 THEN GOTO DONE
  SET &SUM = &SUM + &I
  SET &I = &I + 1
  GOTO LOOP
DONE: WRITE &SUM
EXIT 0
"#;
let rc = interp.execute(source).unwrap();
assert_eq!(rc, 0);
assert_eq!(interp.output(), &["55"]);
```

### With TSO Environment

```rust
use open_mainframe_clist::{ClistInterpreter, DatasetAttributes};
use open_mainframe_clist::tso_bridge::MockTsoEnvironment;

let mut interp = ClistInterpreter::new();
let mut mock = MockTsoEnvironment::new();
mock.add_dataset("SYS1.MACLIB", DatasetAttributes {
    dsorg: "PO".to_string(),
    recfm: "FB".to_string(),
    lrecl: 80,
    blksize: 27920,
    volume: "SYSRES".to_string(),
    ..Default::default()
});
interp.set_tso(Box::new(mock));

let rc = interp.execute(r#"
PROC 0
LISTDSI 'SYS1.MACLIB'
WRITE 'Dataset found'
EXIT 0
"#).unwrap();
assert_eq!(rc, 0);
```

### Shared Global Variables

```rust
use open_mainframe_clist::ClistInterpreter;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

let globals = Arc::new(Mutex::new(HashMap::new()));
let mut interp1 = ClistInterpreter::with_globals(globals.clone());
let mut interp2 = ClistInterpreter::with_globals(globals);

interp1.execute("GLOBAL &SHARED\nSET &SHARED = 'hello'").unwrap();
interp2.execute("GLOBAL &SHARED\nWRITE &SHARED").unwrap();
assert_eq!(interp2.output(), &["hello"]);
```

## Testing

The crate includes 93 unit and integration tests across all modules:

```bash
cargo test -p open-mainframe-clist
```

Key test locations:
- `src/parser.rs` — Source line processing (+/- continuation), tokenization, statement parsing, expression precedence, full CLIST parse, error/file handlers, DATA blocks.
- `src/interpreter.rs` — Variable pool, system variables, SET, IF, DO WHILE/UNTIL, GOTO, EXIT, GLOBAL, SELECT/WHEN, CONTROL, WRITE, arithmetic loop.
- `src/functions.rs` — `&EVAL` arithmetic precedence, `&SUBSTR`, `&LENGTH`, `&SYSINDEX`, `&SYSCAPS`, `&SYSLC`, `&DATATYPE`, `&STR`, `&SYSDSN`.
- `src/io.rs` — Terminal read/write, file open/close/get/put, file mode enforcement, error/attn routine defaults, CONTROL defaults.
- `src/tso_bridge.rs` — TSO command dispatch, ISPEXEC, ISREDIT, LISTDSI queries, mock environment, ISPF variables, dataset attributes.

## Limitations

- **&SYSDSN Validation**: `&SYSDSN` returns `"OK"` for non-empty names in standalone mode without a live catalog connection.
- **Substitution Suppression**: `&STR` and `&NRSTR` evaluation suppresses substitution during evaluation; multi-level nested ampersand suppression is approximate.
- **Arithmetic Precision**: Mathematical expressions use 64-bit signed integers (`i64`); decimal floating-point arithmetic is not supported in CLIST expressions.
- **LISTDSI Scope**: `LISTDSI` populates dataset-level attributes (`&SYSDSORG`, `&SYSRECFM`, `&SYSLRECL`, `&SYSBLKSIZE`); SMS storage classes and multi-volume lists are partially populated.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-rexx](../open-mainframe-rexx/README.md) — Companion REXX scripting interpreter.
- [open-mainframe-tso](../open-mainframe-tso/README.md) — TSO/E terminal environment.
