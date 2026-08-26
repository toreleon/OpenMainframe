# open-mainframe-rexx

REXX (Restructured Extended Executor) language lexer, parser, interpreter, and decimal runtime for OpenMainframe.

## Purpose

`open-mainframe-rexx` implements the TSO/E REXX scripting language for mainframe automation and batch scripting. It provides complete tokenization, predictive AST parsing, arbitrary-precision decimal arithmetic respecting `NUMERIC DIGITS`, variable pool scoping with compound stemmed arrays (`STEM.I.J`), the sophisticated `PARSE` template engine, and 40+ standard built-in functions.

## Capabilities

- **Lexical Analysis (`lexer`)**: Handles REXX tokenization, nested block comments (`/* ... /* ... */ ... */`), string literals (single/double quotes with doubling), hex/binary literals (`'FF'X`, `'1010'B`), and operator disambiguation.
- **Predictive Parser (`parser`)**: Builds a structured `Program` AST containing `Clause` nodes, handling expressions with full operator precedence, multi-clause lines, label markers, and `PARSE` template specifications.
- **Interpreter & Control Flow (`interpreter`)**:
  - Statements: `IF/THEN/ELSE`, `DO` loops (simple, counted, `WHILE`, `UNTIL`, `FOREVER`), `SELECT/WHEN/OTHERWISE`, `SIGNAL`, `CALL/RETURN`, `EXIT`, `SAY`, `PULL`, `ARG`.
  - Scoping: Routine calls with private or shared variable pools and selective exposure via `PROCEDURE EXPOSE`.
  - Host Command Dispatch: `ADDRESS` statement routing to host environments (e.g. `ADDRESS TSO`).
- **Compound Stemmed Variables**: Dynamic associative arrays with stem default initialization (e.g. `STEM. = "DEFAULT"`) and compound tails (`STEM.A.1`).
- **Arbitrary-Precision Decimal Arithmetic (`value`)**:
  - Formatted decimal operations (`rexx_add`, `rexx_sub`, `rexx_mul`, `rexx_div`, `rexx_idiv`, `rexx_rem`, `rexx_pow`, `rexx_compare`).
  - Configurable precision (`NUMERIC DIGITS`) and scientific or engineering exponential format (`NUMERIC FORM`).
- **PARSE Template Engine (`parse_template`)**: Positional numeric patterns (`1 5 10`), relative patterns (`+3`, `-2`), string literal patterns (`PARSE VAR S '(' VAR1 ')'`), variable patterns (`(MARKER)`), and word tokenization.
- **Built-in Functions Library (`builtins`)**: 40+ functions including `SUBSTR`, `LENGTH`, `POS`, `LASTPOS`, `COPIES`, `STRIP`, `WORD`, `WORDS`, `SUBWORD`, `DELWORD`, `SPACE`, `TRANSLATE`, `VERIFY`, `CENTER`, `LEFT`, `RIGHT`, `COMPARE`, `DATATYPE`, `TIME`, `DATE`, `RANDOM`, `D2X`, `X2D`, `C2X`, `X2C`, `B2X`, `X2B`, `MAX`, `MIN`, `ABS`, `SIGN`, `TRUNC`, `FORMAT`, `REVERSE`, `OVERLAY`, `INSERT`, `DELSTR`, `SOURCELINE`, `QUEUED`.

## Architecture

```
    REXX Source Text
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Lexer & Tokenizer                     │
    │  - Nested comment support (/* ... /* ... */ ... */)    │
    │  - Symbol and literal identification                   │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                  Parser Engine                         │
    │  - AST construction (Instructions & Expressions)       │
    │  - Template parsing for PARSE/ARG/PULL                 │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Interpreter & Runtime                  │
    │  - Variable Pools (Local & Global)                     │
    │  - Scoping (PROCEDURE EXPOSE)                          │
    │  - Call Stack & Subroutine dispatch                    │
    └──────┬─────────────────────────────────────────────────┘
           │
    ┌──────▼─────────────────────────────────────────────────┐
    │                 Support Subsystems                     │
    │  - High-Precision Decimal Arithmetic                   │
    │  - Built-in Function Registry                          │
    │  - Host Command Environment (ADDRESS)                  │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `interpreter.rs` | ~2 060 | Recursive execution engine, call stack, variable pools, signal handling |
| `parser.rs` | ~1 220 | Predictive AST parser building clauses and expressions from tokens |
| `builtins.rs` | ~1 150 | Library of 40+ standard REXX built-in functions |
| `value.rs` | ~920 | REXX string-based value system and arbitrary-precision decimal math |
| `lexer.rs` | ~750 | Tokenizer handling comments, literals, operators, and line continuation |
| `parse_template.rs` | ~450 | PARSE template tokenizer, pattern resolver, and string splitting logic |
| `ast.rs` | ~220 | AST nodes: `Program`, `Clause`, `ClauseBody`, `Expr`, `DoControl` |
| `token.rs` | ~140 | Token definitions: `Token`, `TokenKind`, `Span` |

## Public API

### Primary Types & Functions

- `parse(source: &str) -> Result<Program, ParseError>`: Parses source into a REXX AST.
- `interpret(program: &Program) -> Result<ExecResult, InterpError>`: Executes a parsed program.
- `interpret_with_args(program: &Program, args: &str) -> Result<ExecResult, InterpError>`: Executes a program with initial argument strings.
- `ExecResult`: Contains return code `rc: i32` and captured SAY output `output: Vec<String>`.
- `RexxValue`: String-based value representation.
- `NumericSettings` / `NumericForm`: Precision and formatting controls.
- `lex(source: &str) -> Result<Vec<Token>, LexError>`: Direct tokenizer access.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-rexx` depends only on external crates (`miette`, `thiserror`, `serde`, `tracing`).

### Workspace Consumers

- `open-mainframe-tso` — Executes REXX scripts from the TSO/E terminal command line and batch ISPF sessions.

## Examples

### Executing a REXX Script with Arguments

```rust
use open_mainframe_rexx::{interpret_with_args, parse};

let source = r#"
    PARSE ARG name
    SAY 'Hello, ' || name || '!'
    RETURN 0
"#;

let program = parse(source).expect("Parse error");
let result = interpret_with_args(&program, "WORLD").expect("Execution error");

assert_eq!(result.rc, 0);
assert_eq!(result.output, vec!["Hello, WORLD!"]);
```

### Arbitrary-Precision Decimal Arithmetic

```rust
use open_mainframe_rexx::value::{rexx_mul, NumericSettings};

let settings = NumericSettings {
    digits: 12,
    ..Default::default()
};

let product = rexx_mul("1.23456789", "2", &settings).unwrap();
assert_eq!(product, "2.46913578");
```

## Testing

The crate includes 169 unit tests verifying language syntax, arithmetic precision, PARSE templates, and built-in functions:

```bash
cargo test -p open-mainframe-rexx
```

Key test locations:
- `src/interpreter.rs` — Control flow (IF, DO WHILE/UNTIL, SELECT), PROCEDURE EXPOSE variable isolation, and subroutines.
- `src/value.rs` — Multi-precision addition, multiplication, division, truncation, and rounding.
- `src/parse_template.rs` — Absolute and relative positional splits, literal pattern matches, and variable pattern triggers.
- `src/builtins.rs` — All 40+ built-in functions with edge cases (empty strings, large indices, boundary conversions).

## Limitations

- **Dynamic `INTERPRET` Instruction**: Dynamic string-to-code execution (`INTERPRET "x = 5"`) is not currently implemented.
- **Stream I/O Functions**: The `STREAM`, `CHARIN`, and `LINEIN` functions are stubs and are being integrated with `open-mainframe-dataset`.
- **Host Environments**: External command execution is currently routed through `ADDRESS TSO` and local subprocess wrappers; full z/OS subsystem addressing (e.g. `ADDRESS DSNREXX`, `ADDRESS ISPEXEC`) depends on host subsystem bridges.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-tso](../open-mainframe-tso/README.md) — TSO command environment consumer.
- [open-mainframe-clist](../open-mainframe-clist/README.md) — Companion TSO scripting interpreter.
