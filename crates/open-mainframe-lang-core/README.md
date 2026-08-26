# open-mainframe-lang-core

Shared foundational types, traits, and preprocessing infrastructure for OpenMainframe language compilers and interpreters.

## Purpose

`open-mainframe-lang-core` provides zero-dependency, common compiler primitives across all mainframe language implementations in the OpenMainframe workspace (such as COBOL, JCL, and symbolic analyzers). It standardizes source tracking (`Span`, `FileId`, `Location`), line ending normalization (`PreprocessedSource`, `LineIndex`), unified diagnostic reporting (`Diagnostic`, `Severity`), and core compiler pipeline contracts (`AstNode`, `Lexer`, `Parse`).

## Capabilities

- **Source Location Tracking**: Byte-offset ranges (`Span`) referencing specific files via lightweight IDs (`FileId`), with support for span extension, length checks, and 1-based `(line, column)` coordinate conversion (`Location`).
- **Source Preprocessing and Indexing**: Universal line-ending normalization (`normalize_line_endings`) converting CRLF and bare CR to LF, paired with `LineIndex` and `PreprocessedSource` for binary-searched offset-to-coordinate queries (both 0-indexed and 1-indexed).
- **Diagnostics Reporting**: Structured compiler errors, warnings, and informational messages (`Diagnostic`) with error codes, human-readable explanations, source spans, and actionable fix suggestions (`suggestion`).
- **Compiler Stage Traits**: Shared, minimally prescriptive traits (`AstNode`, `Lexer`, `Parse`) for building modular, decoupled language parsers and AST consumers.

## Architecture

```
     Language Compilers (COBOL, JCL, etc.)
     ┌─────────────────────────────────────────────────────────┐
     │  Lexer / Tokenizer           Parser & AST               │
     │  (Implements Lexer)          (Implements Parse/AstNode) │
     └─────────────┬───────────────────────────┬───────────────┘
                   │                           │
                   ▼                           ▼
     ┌─────────────────────────────────────────────────────────┐
     │                open-mainframe-lang-core                 │
     │  ┌────────────────────────┐  ┌───────────────────────┐  │
     │  │ PreprocessedSource     │  │ Diagnostic / Severity │  │
     │  │ LineIndex / normalize  │  │ Span / FileId / Coord │  │
     │  └────────────────────────┘  └───────────────────────┘  │
     └─────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Responsibility |
|---|---|
| `span.rs` | Source byte tracking: `Span`, `FileId`, `Location`, and `offset_to_line_col` conversion |
| `diagnostic.rs` | Error and warning representations: `Diagnostic`, `Severity` |
| `preprocess.rs` | Universal line-ending normalization (`\r\n` / `\r` → `\n`), `LineIndex`, and `PreprocessedSource` |
| `traits.rs` | Stage contracts: `AstNode`, `Lexer`, and `Parse` traits |
| `lib.rs` | Crate root and top-level re-exports |

## Public API

### Core Types

- `Span`: Contiguous byte range (`file: FileId`, `start: u32`, `end: u32`) with constructors (`new`, `main`, `point`, `dummy`) and operations (`len`, `is_empty`, `extend`, `to_range`).
- `FileId`: Lightweight 32-bit source file identifier (`FileId(pub u32)`), with `FileId::MAIN` constant for the root source file.
- `Location`: Human-readable 1-indexed source position (`line: u32`, `column: u32`).
- `PreprocessedSource`: Container holding normalized source `text: String` and precomputed `line_index: LineIndex`. Provides `new`, `from_unix`, `offset_to_line_col_0`, `offset_to_line_col_1`, and line span helpers.
- `LineIndex`: Precomputed newline offsets enabling `O(log N)` binary-searched line and column resolution from raw byte offsets.
- `Diagnostic`: Structured compiler issue with `severity: Severity`, `code: String`, `message: String`, `span: Span`, and optional `suggestion: Option<String>`.
- `Severity`: 3-level severity enumeration: `Error`, `Warning`, `Info`.

### Shared Traits

- `AstNode`: Contract requiring `fn span(&self) -> Span`.
- `Lexer`: Interface defining `type Token`, `type Error`, and `fn tokenize(&mut self, source: &str) -> (Vec<Self::Token>, Vec<Self::Error>)`.
- `Parse`: Interface defining `type Ast: AstNode`, `type Error`, and `fn parse(&mut self) -> (Option<Self::Ast>, Vec<Self::Error>)`.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-lang-core` has zero external or workspace dependencies to keep the foundational dependency graph clean.

### Workspace Consumers

- `open-mainframe-cobol` — Uses `Span`, `FileId`, `Location`, and AST traits for COBOL lexical analysis, preprocessors, and parser AST nodes.
- `open-mainframe-jcl` — Uses `Span` and `AstNode` for JCL parsing and error attribution.
- `open-mainframe-symbolic` — Uses shared AST and span primitives for symbolic analysis.

## Examples

### Preprocessing and Coordinate Conversion

```rust
use open_mainframe_lang_core::{PreprocessedSource, Span};

let raw_source = "IDENTIFICATION DIVISION.\r\nPROGRAM-ID. HELLO.\r\n";
let preprocessed = PreprocessedSource::new(raw_source);

// Line endings normalized to Unix \n
assert_eq!(preprocessed.text, "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.\n");

// Binary-searched coordinate lookup (1-indexed: line 2, col 1)
let prog_id_offset = 25; // start of PROGRAM-ID
let (line, col) = preprocessed.line_index.offset_to_line_col_1(prog_id_offset);
assert_eq!(line, 2);
assert_eq!(col, 1);
```

### Emitting Diagnostics with Spans

```rust
use open_mainframe_lang_core::{Diagnostic, Severity, Span};

let span = Span::main(25, 35);
let diag = Diagnostic::error("COB001", "Missing period after PROGRAM-ID", span)
    .with_suggestion("Add '.' after program name");

assert_eq!(diag.severity, Severity::Error);
assert_eq!(diag.code, "COB001");
assert!(diag.is_error());
assert_eq!(
    format!("{}", diag),
    "error[COB001]: Missing period after PROGRAM-ID (Add '.' after program name)"
);
```

### Implementing `AstNode`

```rust
use open_mainframe_lang_core::{AstNode, Span};

struct ProgramHeader {
    name: String,
    span: Span,
}

impl AstNode for ProgramHeader {
    fn span(&self) -> Span {
        self.span
    }
}
```

## Testing

The crate includes 38 unit and documentation tests verifying mathematical correctness of span manipulation, line-ending normalization, coordinate conversion, and diagnostic formatting:

```bash
cargo test -p open-mainframe-lang-core
```

Key test locations:
- `src/span.rs` — Span merging, containment, empty spans, and 1-based coordinate conversions.
- `src/preprocess.rs` — CRLF, bare CR, mixed line endings, offset tracking roundtrips, and line indexing.
- `src/diagnostic.rs` — Diagnostic creation, severity formatting, and suggestion chaining.
- `src/traits.rs` — Trait compilation and mock implementations.

## Limitations

- **Byte Offset Basis**: All offsets in `Span` and `LineIndex` represent UTF-8 byte offsets. Languages with fixed card-column conventions (e.g. COBOL Area A/B, HLASM column rules) must layer column-aware slicing on top.
- **Synchronous Tokenization**: The `Lexer` and `Parse` traits are synchronous in-memory contracts and do not accommodate streaming or incremental parsing pipelines directly.
- **Single-File Diagnostics**: `Diagnostic` references a single primary `Span`. Multi-span diagnostics (such as related definitions or secondary labels) must be managed at higher layers or via downstream error reporters like `miette`.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Architectural overview of all workspace crates.
- [open-mainframe-cobol](../open-mainframe-cobol/README.md) — Primary language compiler consumer.
- [open-mainframe-jcl](../open-mainframe-jcl/README.md) — JCL interpreter consumer.
