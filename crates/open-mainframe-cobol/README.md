# open-mainframe-cobol

Multi-pass COBOL compiler front-end, semantic analyzer, and LLVM code generator for OpenMainframe.

## Purpose

`open-mainframe-cobol` provides a full-featured COBOL compilation pipeline supporting COBOL-85, COBOL-2002, and COBOL-2014 standards alongside IBM Enterprise COBOL extensions. It models the IBM Enterprise COBOL for z/OS compilation process, transforming fixed-format and free-format COBOL source text through 4 preprocessor passes, tokenization, recursive-descent parsing, three-phase semantic analysis, intrinsic function evaluation, XML/JSON parsing and generation, and LLVM IR generation.

## Capabilities

- **4-Pass Preprocessor Pipeline**:
  - Pass 0 (`CompilerOptions`): Extraction and parsing of `CBL` and `PROCESS` compiler option statements (ARITH, TRUNC, NUMPROC, NSYMBOL, INTDATE, CODEPAGE).
  - Pass 1 (`ConditionalProcessor`): Directive evaluation for `>>DEFINE`, `>>IF`/`>>ELSE`/`>>END-IF`, `>>EVALUATE`/`>>WHEN`/`>>END-EVALUATE`, and `>>SET SOURCEFORMAT`.
  - Pass 2 (`Preprocessor` / `CopybookResolver`): `COPY ... REPLACING` copybook expansion supporting full, leading, and trailing pseudo-text (`==...==`) substitutions, circular include prevention, and CICS `DFHRESP()` macro expansion.
  - Pass 3 (`ReplaceProcessor`): Global source substitution via `REPLACE ==from== BY ==to==` and `REPLACE OFF`.
- **Lexical Analysis (Scanner)**: Fixed-format card layout (cols 1–6 sequence, col 7 indicator, cols 8–72 code, cols 73–80 identification) and free-format parsing, string literals with doubled quotes, hex literals (`X"..."`), national literals (`N"..."`), and PICTURE string classification.
- **Recursive Descent Parser**: Complete AST generation across all four COBOL divisions:
  - `IDENTIFICATION DIVISION`: PROGRAM-ID (COMMON, INITIAL), AUTHOR, and informational paragraphs.
  - `ENVIRONMENT DIVISION`: CONFIGURATION (SOURCE-COMPUTER, OBJECT-COMPUTER, SPECIAL-NAMES, REPOSITORY), INPUT-OUTPUT (FILE-CONTROL, I-O-CONTROL).
  - `DATA DIVISION`: FILE SECTION (FD/SD), WORKING-STORAGE, LOCAL-STORAGE, LINKAGE SECTION, level numbers 01–49, 66 (RENAMES), 77 (independent items), 88 (condition names with THRU ranges), OCCURS (fixed and DEPENDING ON), and REDEFINES.
  - `PROCEDURE DIVISION`: Sections, paragraphs, USING/RETURNING parameters, DECLARATIVES, and 39 statement variants.
- **Three-Phase Semantic Analyzer**: Symbol table construction with qualified name resolution, type compatibility validation (MOVE, COMPUTE, PERFORM, GO TO), and nested program scoping.
- **77+ Intrinsic Functions**: Numeric (34), string (34+), datetime (24+), and general built-in functions.
- **COBOL-2014 XML/JSON**: `JSON GENERATE`/`JSON PARSE` and `XML GENERATE`/`XML PARSE` data transformations.
- **LLVM Code Generator** (optional `llvm` feature): LLVM IR module creation, target triples, and data item global allocation via Inkwell.

## Architecture

```
  COBOL Source
       │
  ┌────▼─────────────────────────────────────────────────┐
  │                  Preprocessor Pipeline                │
  │  Pass 0: CompilerOptions  (CBL/PROCESS)              │
  │  Pass 1: ConditionalProcessor (>>IF/>>EVALUATE)      │
  │  Pass 2: Preprocessor  (COPY/DFHRESP)                │
  │  Pass 3: ReplaceProcessor  (REPLACE ==...==)         │
  └────┬─────────────────────────────────────────────────┘
       │
  ┌────▼────────┐     ┌──────────────┐     ┌────────────────┐
  │   Scanner   │────▶│    Parser    │────▶│   Semantic     │
  │  (Lexer)    │     │  (AST gen)   │     │   Analyzer     │
  └─────────────┘     └──────────────┘     └───────┬────────┘
                                                   │
                                          ┌────────▼────────┐
                                          │  Code Generator  │
                                          │  (LLVM IR)       │
                                          └─────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `lexer/` | ~4 460 | 4-pass preprocessor, tokenizer, source management, compiler options |
| `ast/` | ~2 000 | AST node types: divisions, statements, data items, expressions |
| `parser/` | ~5 600 | Recursive descent parser for all COBOL divisions and clauses |
| `semantic/` | ~2 350 | Symbol table, type system, three-phase semantic analyzer |
| `codegen/` | ~1 115 | LLVM IR generation (behind `llvm` feature flag) |
| `intrinsics/` | ~2 870 | 77+ COBOL-2014 intrinsic functions |
| `xml_json/` | ~1 440 | JSON GENERATE/PARSE and XML GENERATE/PARSE |
| `macros.rs` | ~530 | Macro-driven keyword and statement dispatch tables |
| `error.rs` | ~110 | `CobolError` enum with miette diagnostics |

## Public API

### Core Compiler Types & Functions

- **Source & Lexer**: `SourceManager`, `SourceFile`, `SourceFormat`, `SourceLine`, `scan`, `Token`, `TokenKind`, `Keyword`, `CompilerOptions`, `parse_cbl_process`, `Preprocessor`, `CopybookConfig`, `CopybookResolver`, `ConditionalProcessor`, `ReplaceProcessor`.
- **AST Nodes**: `Program`, `IdentificationDivision`, `EnvironmentDivision`, `DataDivision`, `DataItem`, `PictureClause`, `ProcedureDivision`, `Statement`, `Expression`, `Condition`, `FileControlEntry`.
- **Parser**: `Parser::parse(tokens) -> (Option<Program>, Vec<CobolError>)`.
- **Semantic Analysis**: `analyze(program) -> SemanticResult`, `SemanticAnalyzer`, `SymbolTable`, `Symbol`, `SymbolKind`, `CobolType`, `TypeCategory`, `Diagnostic`, `Severity`.
- **XML / JSON**: `json_generate`, `json_parse`, `xml_generate`, `JsonGenerateOptions`, `XmlGenerateOptions`, `CobolField`.
- **Intrinsics**: Modules `numeric`, `string`, `datetime` in `open_mainframe_cobol::intrinsics`.
- **Codegen** (feature `llvm`): `CodeGenerator`, `CodegenOptions`, `DataLayout`, `LlvmType`.

## Integration

### Internal Workspace Dependencies

- `open-mainframe-lang-core` — Shared `Span`, `FileId`, `Location`, and compiler traits.
- `open-mainframe-encoding` — EBCDIC translation and numeric storage conversions.
- `open-mainframe-runtime` — Runtime support library and LE data formatting.

### Workspace Consumers

- `open-mainframe` — Mainframe CLI and CICS transaction runner.
- `open-mainframe-assess` — Code assessment, migration readiness metrics, and complexity analysis.
- `open-mainframe-symbolic` — Symbolic execution and formal verification of COBOL logic.
- `open-mainframe-tui` — Interactive CICS/COBOL debugging and execution.
- `open-mainframe-wiki` — Automatic documentation and syntax extraction.

## Examples

### Parsing and Analyzing a COBOL Program

```rust
use open_mainframe_cobol::{
    SourceManager, SourceFormat, scan, Parser, analyze,
};

let source_text = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG PIC X(12) VALUE 'HELLO WORLD!'.
       PROCEDURE DIVISION.
           DISPLAY WS-MSG.
           GOBACK.
"#;

let mut sm = SourceManager::new();
let id = sm.add_text(source_text.to_string(), SourceFormat::Fixed);
let source = sm.get(id).unwrap();

let (tokens, lex_errors) = scan(source);
assert!(lex_errors.is_empty());

let (program, parse_errors) = Parser::parse(&tokens);
assert!(parse_errors.is_empty());

if let Some(prog) = &program {
    let result = analyze(prog);
    assert!(!result.has_errors());
}
```

### Preprocessing with Copybooks

```rust
use open_mainframe_cobol::lexer::{CopybookConfig, Preprocessor, SourceFormat};

let mut config = CopybookConfig::new();
config.add_path("/path/to/copybooks");

let mut pp = Preprocessor::new(config, SourceFormat::Fixed);
let source = "       IDENTIFICATION DIVISION.\n       COPY CUSTREC REPLACING ==PREFIX== BY ==WS==.\n";
// Expands copybook and applies pseudo-text substitutions
let expanded = pp.preprocess(source);
```

### Intrinsic Function Evaluation

```rust
use open_mainframe_cobol::intrinsics::{numeric, datetime, string};

let fact = numeric::factorial(5); // 120.0
let trimmed = string::trim("  MAINFRAME  ", string::TrimDirection::Both); // "MAINFRAME"
let date_str = datetime::current_date();
```

## Testing

The crate includes 291 unit and integration tests verifying lexer tokens, preprocessor passes, parsing of all divisions, semantic type rules, intrinsic functions, and JSON/XML generators:

```bash
cargo test -p open-mainframe-cobol

# Testing with optional LLVM backend:
cargo test -p open-mainframe-cobol --features llvm
```

Key test locations:
- `src/lexer/` — Fixed/free formatting, CBL options, COPY replacement modes, and conditional directives.
- `src/parser/` — All division parses, nested programs, and 39 statement forms.
- `src/semantic/` — Symbol tables, qualified name lookups, and MOVE compatibility.
- `src/intrinsics/` — Mathematical accuracy, financial algorithms, date formats, and string manipulations.
- `src/xml_json/` — JSON/XML generation and roundtrip parsing.

## Limitations

- **LLVM Statement Codegen**: The LLVM backend builds module structures and data item memory layouts, but procedural statement translation to LLVM IR is in active development.
- **Embedded SQL / CICS**: `EXEC SQL` and `EXEC CICS` statement bodies are captured as raw text blocks in the AST; preprocessing and macro replacement is handled by `open-mainframe-precompilers`.
- **Report Writer**: `GENERATE`, `INITIATE`, and `TERMINATE` Report Writer statements are parsed but report generation runtime is not yet implemented.
- **Communication Section**: The legacy COBOL Communication Section is not supported.
- **Object-Oriented COBOL**: `INVOKE` statement is parsed, but full OOP constructs (`CLASS-ID`, `FACTORY`, `OBJECT`) are not implemented.
- **ARITH(EXTEND) 31-Digit Math**: 31-digit precision is declared and tracked in the type system, but intrinsic math functions use IEEE 754 `f64` for transcendental calculations.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural map.
- [open-mainframe-lang-core](../open-mainframe-lang-core/README.md) — Shared compiler spans, diagnostics, and traits.
- [open-mainframe-runtime](../open-mainframe-runtime/README.md) — Language Environment runtime execution library.
- [open-mainframe-encoding](../open-mainframe-encoding/README.md) — EBCDIC encoding and storage layout rules.
