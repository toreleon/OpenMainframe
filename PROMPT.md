# zOS-clone Implementation Loop

## Project Context

You are implementing **zOS-clone**, an open-source mainframe compiler built in Rust that enables enterprises to execute existing mainframe source code (COBOL) on commodity Linux hardware.

## Planning Artifacts

All planning is complete. Reference these documents:

- **PRD**: `_bmad-output/planning-artifacts/prd.md` - 96 functional requirements, 27 NFRs
- **Architecture**: `_bmad-output/planning-artifacts/architecture.md` - Technical decisions, patterns, structure
- **Epics**: `_bmad-output/planning-artifacts/epics.md` - 14 epics, 84 stories with acceptance criteria

## Implementation Sequence

Follow this order (from Architecture):

```
1. Epic 1:  Project Foundation     → Cargo workspace, CI, tooling
2. Epic 2:  zos-encoding           → EBCDIC, packed decimal, zoned decimal
3. Epic 3:  zos-cobol lexer        → Tokenization, copybook resolution
4. Epic 4:  zos-cobol parser       → AST, all COBOL divisions
5. Epic 5:  zos-cobol semantic     → Symbol table, type checking
6. Epic 6:  zos-runtime            → Intrinsics, I/O, ABEND handling
7. Epic 7:  zos-cobol codegen      → LLVM IR generation
8. Epic 8:  zos-dataset            → File I/O, QSAM, catalog
9. Epic 9:  zos-jcl                → JCL interpreter, job execution
10. Epic 10: zos-clone CLI         → Commands (compile, run, check)
11. Epic 11: Configuration         → YAML config, env vars
12. Epic 12: Testing               → NIST compliance, benchmarks
13. Epic 13: Distribution          → Docker, releases
14. Epic 14: Documentation         → Guides, reference
```

## Your Task

1. **Check current state**: Run `cargo build --workspace` and `cargo test --workspace` to see what's implemented
2. **Identify next work**: Find the next incomplete epic/story based on the sequence above
3. **Implement**: Write the code following Architecture patterns exactly
4. **Test**: Ensure `cargo test`, `cargo clippy`, `cargo fmt --check` all pass
5. **Commit**: Create atomic commits with clear messages

## Architecture Constraints (MUST FOLLOW)

### Crate Structure
```
crates/
├── zos-clone/      # CLI binary (clap 4.x)
├── zos-cobol/      # Compiler (lexer, parser, ast, semantic, codegen)
├── zos-jcl/        # JCL interpreter
├── zos-runtime/    # Runtime library
├── zos-dataset/    # File I/O
└── zos-encoding/   # EBCDIC/decimal (leaf crate, no internal deps)
```

### Key Technical Decisions
- **Parser**: Hand-written recursive descent (NOT parser generators)
- **Code Generation**: LLVM via `inkwell` crate
- **CLI**: `clap` 4.x with derive macros
- **Errors**: `thiserror` + `miette` for diagnostics
- **Decimals**: `rust_decimal` + custom BCD serialization
- **Logging**: `tracing` crate

### Code Patterns
- All AST nodes MUST have `span: Span` field
- All errors MUST implement `miette::Diagnostic`
- No `.unwrap()` in library code (tests only)
- Use `Result<T, E>` for all fallible operations
- Run `cargo fmt` and `cargo clippy` before commits

### Naming Conventions
- Modules: `snake_case`
- Types/Structs: `PascalCase`
- Functions: `snake_case`
- Error types: `{Crate}{Category}Error`

## Progress Tracking

After each iteration, update this section with completed work:

### Completed
- Epic 1: Project Foundation (workspace, CI, tooling)
- Epic 2: zos-encoding (EBCDIC code pages, packed/zoned/binary decimal)
- Epic 3: COBOL Lexer (source, span, token, keywords, scanner, copybook)
- Story 4.1: AST Node Types defined (all divisions, statements, expressions)
- Story 4.2: IDENTIFICATION DIVISION Parser (recursive descent infrastructure)
- Story 4.3: DATA DIVISION Parser (file section, working storage, linkage, data items)
- Story 4.4: PROCEDURE DIVISION Parser (sections, paragraphs, basic statements)
- Story 4.5: Arithmetic & String Statement Parsing (ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE, STRING, UNSTRING)
- Story 4.6: CALL, GOTO, EXIT Statement Parsing

### In Progress
- Epic 4 completion: Parser refinements, error recovery

### Blocked
<!-- Any blockers discovered -->

## Completion Signal

When ALL of the following are true, output the completion promise:

1. All 6 crates compile with `cargo build --workspace`
2. `cargo test --workspace` passes
3. `cargo clippy --all-targets -- -D warnings` passes
4. `zos-clone compile examples/hello-world/HELLO.cbl` produces executable
5. `zos-clone run examples/hello-world/HELLO.jcl` executes successfully
6. NIST COBOL-85 test suite passes at 85%+ (MVP threshold)

When complete, output:
```
<promise>ZOS-CLONE MVP COMPLETE</promise>
```

## Current Iteration Instructions

1. Read this prompt and the planning artifacts
2. Check git log and current code state
3. Identify the highest-priority incomplete work
4. Implement ONE story or meaningful chunk
5. Ensure all checks pass
6. Commit your changes
7. Update the Progress Tracking section above

Start by running `cargo build --workspace` to assess current state.
