# open-mainframe-hlasm

High Level Assembler (HLASM) lexer, parser, macro processor, and z/Architecture instruction encoding engine for OpenMainframe.

## Purpose

`open-mainframe-hlasm` implements the IBM High Level Assembler (HLASM) toolchain for mainframe assembly source code. It provides fixed-format source card parsing, operand tokenization, multi-pass symbol resolution with attribute references, macro expansion (`MACRO/MEND`, `&SYSNDX`, `&SYSLIST`, `MNOTE`, `MEXIT`, `COPY`), conditional assembly (`AIF`, `AGO`, `SETx`), binary machine instruction encoding for over 200 z/Architecture mnemonics, and object module generation (OBJ format with ESD, TXT, RLD, and END records).

## Capabilities

- **Fixed-Format Lexer & Card Slicing (`lexer.rs`)**:
  - Parses standard column boundaries: Column 1 (labels), Column 10 (opcodes), Column 16 (operands), Column 72 (continuation indicator).
  - Operand tokenization for registers (R0–R15, 0–15), symbols, literals (`=L'8'`, `=C'TEXT'`, `=F'100'`), self-defining terms, and base-displacement expressions (`D(X,B)` or `D(B)`).
- **z/Architecture Machine Instruction Encoding (`instruction.rs`)**:
  - Catalog of 200+ instructions covering formats: E, I, RR, RRE, RRF (a/b/c), RX, RXE, RXY, RS, RSY, RI, RIE, RIL, SI, SIY, SIL, S, SS (a/b), SSE, SSF, VRR, VRI, VRX, VRS, VRV, VSI.
  - Extended branch mnemonics: `B`, `BR`, `BE`, `BNE`, `BH`, `BL`, `BNH`, `BNL`, `BZ`, `BNZ`, `BO`, `BNO`, `BP`, `BM`, `NOP`, `NOPR`, `J`, `JE`, `JNE`, etc.
  - Decimal arithmetic instructions: `AP`, `SP`, `MP`, `DP`, `CP`, `ZAP`, `PACK`, `UNPK`, `SRP`, `MVO`.
- **Macro Language Engine (`macros.rs`)**:
  - Macro definitions (`MACRO/MEND`) with positional and keyword parameters (including default values).
  - System variable symbols: `&SYSNDX`, `&SYSDATE`, `&SYSTIME`, `&SYSECT`, `&SYSLIST`.
  - Nested and inner macro expansions with recursion limits.
  - `MNOTE` diagnostic messages and `MEXIT` early return.
  - `COPY` member inclusion via `CopyLibrary`.
- **Conditional Assembly (`conditional.rs`)**:
  - Statements: `LCLA`, `GBLA`, `LCLB`, `GBLB`, `LCLC`, `GBLC`, `SETA`, `SETB`, `SETC`, `AIF`, `AGO`, `ANOP`.
- **Symbol Table & Expressions (`symbol.rs`)**:
  - Two-pass symbol evaluation.
  - Attribute references: `L'` (length), `T'` (type), `S'` (scale), `I'` (integer), `D'` (defined).
- **Directives & Base-Displacement Resolution (`directives.rs`)**:
  - `USING` and `DROP` base register management (`UsingTable`).
  - `DC` (Define Constant) and `DS` (Define Storage) parsing across all standard data types (C, X, B, F, H, E, D, P, Z, A, Y, V, S, Q, J, R).
- **Object Module Generation (`object.rs`)**:
  - Standard 80-byte card deck OBJ format: External Symbol Dictionary (ESD), Text (TXT), Relocation Dictionary (RLD), and END records.
  - AMODE (24, 31, 64, ANY) and RMODE (24, 31, 64, ANY) tracking.

## Architecture

```
    HLASM Source                          Object Generation
    ┌──────────────┐                      ┌────────────────────┐
    │ Label Opcode │    Two-Pass          │   Object Engine    │
    │ Operands...  │ ──────────────────>  │    (ESD/TXT/RLD)   │
    └──────────────┘    MacroEngine       └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Instruction       ┌────────────────────┐
    │  Mnemonic    │ ──────────────────>  │   Binary Encoding  │
    │  Resolution  │    InsnCatalog       │   (2, 4, 6 bytes)  │
    └──────────────┘                      └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Symbol Table      ┌────────────────────┐
    │ Expressions  │ <──────────────────  │   Symbol Table     │
    │ L', T', S'   │    SymbolTable       │   Resolution       │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Lines | Description |
|---|---|---|
| `instruction.rs` | ~1 670 | z/Architecture instruction catalog (200+ mnemonics) and binary encoders |
| `macros.rs` | ~1 005 | Macro engine (`MACRO/MEND`), parameter substitution, `&SYSNDX`, `COPY` |
| `conditional.rs` | ~750 | Conditional assembly engine (`AIF`, `AGO`, `SETx` variables) |
| `object.rs` | ~740 | Object deck emission (ESD, TXT, RLD, END records) and AMODE/RMODE |
| `directives.rs` | ~640 | DC/DS constant parsing, USING/DROP table management |
| `lexer.rs` | ~540 | Fixed-format column parser and operand tokenizer |
| `symbol.rs` | ~440 | Symbol table, attribute lookup (`L'`, `T'`), expression evaluator |

## Public API

### Primary Types and Functions

- **Instruction Encoding**: `InsnCatalog`, `InsnDef`, `InsnFormat`, `InsnOperands`, `encode_instruction(def, ops) -> Result<Vec<u8>, EncodeError>`, `EncodeError`.
- **Lexer**: `parse_source(text) -> Vec<SourceLine>`, `parse_source_line(line) -> Result<InstructionLine, LexerError>`, `tokenize_operands(ops) -> Vec<Token>`, `InstructionLine`, `Token`.
- **Macros**: `MacroEngine`, `MacroDef`, `MacroParam`, `CopyLibrary`, `Mnote`, `SystemVars`, `MacroError`.
- **Object Module**: `ObjectModule`, `EsdItem`, `EsdType`, `TxtRecord`, `RldEntry`, `Amode`, `Rmode`.
- **Symbol & Directives**: `SymbolTable`, `Symbol`, `eval_expression`, `parse_dc_operand`, `UsingTable`, `Directive`.
- **Conditional Assembly**: `CondAsmEngine`, `CondAsmStmt`, `SetType`, `SetValue`.

## Integration

### Internal Workspace Dependencies

*None* — `open-mainframe-hlasm` depends only on external crates (`miette`, `thiserror`, `serde`, `tracing`).

### Workspace Consumers

- Direct workspace member providing assembly processing services for future tooling, program binder, and execution engines.

## Examples

### Encoding a Machine Instruction

```rust
use open_mainframe_hlasm::instruction::{encode_instruction, InsnCatalog, InsnOperands};

let catalog = InsnCatalog::new();

// Lookup 'LR' (Load Register: RR format, opcode 0x18)
let def = catalog.lookup("LR").expect("Instruction not found");
let ops = InsnOperands {
    r1: 3,
    r2: 5,
    ..Default::default()
};

let bytes = encode_instruction(def, &ops).unwrap();
assert_eq!(bytes, vec![0x18, 0x35]);
```

### Parsing HLASM Source Cards

```rust
use open_mainframe_hlasm::lexer::parse_source_line;

let card = "START1   LA    R1,DATA(R2)      Load address";
let insn = parse_source_line(card).expect("Parse error");

assert_eq!(insn.label.as_deref(), Some("START1"));
assert_eq!(insn.opcode, "LA");
assert_eq!(insn.operands, "R1,DATA(R2)");
```

### Defining and Expanding a Macro

```rust
use open_mainframe_hlasm::macros::MacroEngine;

let mut engine = MacroEngine::new();
let macro_lines = vec![
    "         GENREC &NAME".to_string(),
    "&NAME    DS    0CL80".to_string(),
    "         MEND".to_string(),
];

let def = engine.parse_macro(&macro_lines).unwrap();
engine.define_macro(def);

let expanded = engine.expand("MYREC", "GENREC", "CUST").unwrap();
assert_eq!(expanded, vec!["CUST     DS    0CL80".to_string()]);
```

## Testing

The crate contains 173 unit tests verifying instruction formats, operand tokenization, macro expansions, conditional branching, and OBJ card generation:

```bash
cargo test -p open-mainframe-hlasm
```

Key test locations:
- `src/instruction.rs` — Encoding roundtrips for RR, RX, RXY, RS, RSY, SI, SS, S, and vector instructions.
- `src/macros.rs` — Positional/keyword parameters, default values, `&SYSNDX` counter increments, and `&SYSLIST` indexing.
- `src/conditional.rs` — `SETA`/`SETB`/`SETC` evaluation and `AIF`/`AGO` branch execution.
- `src/object.rs` — Card column formatting for 80-column ESD, TXT, RLD, and END cards.
- `src/directives.rs` — DC type length calculations and USING displacement resolution.

## Limitations

- **Complete Assembly Pipeline**: The crate provides all individual components (lexer, parser, macro engine, symbol table, instruction encoder, object deck generator); full end-to-end multi-pass assembly linking into load modules is in progress.
- **GOFF Format**: Full Generalized Object File Format (GOFF) support is foundational; standard 80-column card deck OBJ is fully supported.
- **Floating Point Vector Constants**: Vector instruction encodings are complete, but vector constant directives (`DC VR...`) are partially supported.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md) — Workspace architectural overview.
- [open-mainframe-pgmmgmt](../open-mainframe-pgmmgmt/README.md) — Program management, binder, and loader services.
- [open-mainframe-encoding](../open-mainframe-encoding/README.md) — EBCDIC character translation for assembler strings.
