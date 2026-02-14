---
stepsCompleted: [1, 2, 3, 4, 5, 6, 7, 8]
inputDocuments: [prd.md, product-brief-OpenMainframe-2026-02-12.md]
workflowType: 'architecture'
project_name: 'OpenMainframe'
user_name: 'Tore'
date: '2026-02-12'
lastStep: 8
status: 'complete'
completedAt: '2026-02-12'
---

# Architecture Decision Document

_This document builds collaboratively through step-by-step discovery. Sections are appended as we work through each architectural decision together._

## Project Context Analysis

### Requirements Overview

**Functional Requirements:**
96 FRs across 12 capability areas representing a complete mainframe-compatible toolchain:

| Category | FR Count | Architectural Implication |
|----------|----------|---------------------------|
| COBOL Compilation | 15 | Core compiler with multi-dialect support |
| JCL Processing | 12 | Separate interpreter with job orchestration |
| File & Dataset Operations | 9 | File system abstraction layer |
| Data Conversion | 7 | Encoding/decoding subsystem |
| Runtime Execution | 10 | COBOL runtime library |
| Developer CLI | 9 | Command-line interface layer |
| Validation & Diagnostics | 8 | Error handling infrastructure |
| Configuration Management | 6 | Configuration subsystem |
| Distribution & Installation | 6 | Build and packaging |
| IDE & Tooling Integration | 4 | Language Server Protocol |
| Documentation & Help | 6 | Documentation system |
| Compatibility Validation | 4 | Testing infrastructure |

**Non-Functional Requirements:**
27 NFRs defining quality attributes that constrain architectural choices:

| Category | Key Constraints |
|----------|-----------------|
| Performance | <10s compile (10K lines), <500ms startup, 2x IBM execution |
| Security | Signed binaries, CVE auditing, sandboxed execution |
| Scalability | 10 concurrent compiles, 100 concurrent jobs, 100GB files |
| Integration | POSIX exit codes, JSON/SARIF output, LSP 3.17+ |
| Reliability | Deterministic output, 18-digit decimal precision, 95%+ NIST |
| Portability | x86_64 + ARM64, glibc 2.31+/musl, static linking |
| Usability | POSIX CLI conventions, <5min hello world, tab completion |
| Maintainability | cargo clippy clean, 80% coverage, SemVer |

**Scale & Complexity:**

- Primary domain: **Systems programming / Compiler toolchain**
- Complexity level: **HIGH** (novel compiler, legacy compatibility, clean-room)
- Estimated architectural components: **8-10 major subsystems**

### Technical Constraints & Dependencies

**Language & Runtime:**
- Implementation language: Rust (specified in product vision)
- Target runtime: Native Linux executables (no VM/interpreter)
- Minimum Rust version: 1.70+ (for build)

**Platform Constraints:**
- Primary: Linux x86_64, Linux ARM64
- Kernel: 4.19+ minimum (LTS baseline)
- libc: glibc 2.31+ or musl
- Static linking required for portable deployment

**Compatibility Constraints:**
- Must match IBM COBOL behavior exactly (bit-identical output)
- NIST COBOL-85 test suite: 95%+ pass rate required
- Decimal precision: 18 digits (IBM standard)
- EBCDIC code pages: CP037, CP1047, CP500

**Legal Constraints:**
- Clean-room implementation (no IBM source code)
- Permissive license (Apache 2.0 or MIT)
- Patent risk mitigation required

### Cross-Cutting Concerns Identified

1. **Error Handling & Diagnostics**
   - Spans: Compiler, JCL interpreter, runtime, CLI
   - Requires: Consistent error format, source locations, suggestions
   - Impact: Unified error type hierarchy, diagnostic infrastructure

2. **Configuration Management**
   - Spans: All components
   - Requires: Files (YAML/TOML), CLI flags, environment variables
   - Impact: Centralized configuration subsystem

3. **Data Encoding (EBCDIC/ASCII)**
   - Spans: Compiler (literals), runtime (I/O), file layer
   - Requires: Code page tables, conversion routines
   - Impact: Shared encoding library

4. **Logging & Observability**
   - Spans: All components
   - Requires: Structured logging, verbosity levels
   - Impact: Logging infrastructure with compile-time configurability

5. **Testing Infrastructure**
   - Spans: All components
   - Requires: NIST compliance, regression, fuzzing
   - Impact: Test harness architecture, output comparison framework

## Starter Template Evaluation

### Primary Technology Domain

**Systems Programming / Compiler Toolchain** - This is a Rust-based compiler project, not a typical web/mobile application. Standard "starter templates" don't apply; instead we evaluate compiler-specific tooling choices.

### Technical Foundation Decisions

#### Parsing Strategy: Hand-written Recursive Descent

**Decision:** Implement a hand-written lexer and recursive descent parser rather than using parser generators.

**Rationale:**
- COBOL's context-sensitive grammar (PICTURE clauses, area A/B, copybooks) requires maximum flexibility
- Multiple dialect support (cobol85, cobol2002, ibm) needs fine-grained control
- Production compilers (rustc, Go, clang) use this approach for complex languages
- Enables incremental parsing for future LSP integration
- Better error recovery and diagnostic generation

**Trade-offs:**
- More initial implementation effort vs. parser generators
- Requires careful grammar documentation
- Benefits: full control, better errors, easier maintenance

#### Code Generation: LLVM via inkwell

**Decision:** Use LLVM as the code generation backend, accessed through the `inkwell` Rust crate.

**Rationale:**
- Industry standard for production compilers (clang, rustc, Swift)
- Mature optimization passes critical for performance targets
- Native multi-target support (x86_64, ARM64)
- Deterministic output achievable
- `inkwell` provides safe Rust bindings to LLVM C API

**Dependencies:**
```toml
inkwell = { version = "0.4", features = ["llvm17-0"] }
```

**Trade-offs:**
- LLVM build dependency increases compile time
- Requires LLVM installation on build machine
- Benefits: world-class optimization, proven stability

#### CLI Framework: clap 4.x

**Decision:** Use `clap` with derive macros for command-line interface.

**Rationale:**
- De facto Rust standard, actively maintained
- Native subcommand support for `compile`, `run`, `check`, `convert`
- Built-in shell completion generation
- POSIX-compliant argument parsing

**Dependencies:**
```toml
clap = { version = "4", features = ["derive", "env"] }
clap_complete = "4"
```

#### Error Diagnostics: miette + codespan

**Decision:** Use `miette` for CLI error display with `codespan-reporting` compatible internals.

**Rationale:**
- Beautiful terminal output with source context
- LSP-compatible diagnostic format for future IDE integration
- Supports suggestions and related information
- Can integrate IBM-style error codes

**Dependencies:**
```toml
miette = { version = "7", features = ["fancy"] }
thiserror = "1"
```

### Project Initialization

**Workspace Structure:**
```
open-mainframe/
├── Cargo.toml              # Workspace root
├── crates/
│   ├── open-mainframe/          # Main CLI binary
│   ├── open-mainframe-cobol/          # COBOL compiler (lexer, parser, AST, codegen)
│   ├── open-mainframe-jcl/            # JCL interpreter
│   ├── open-mainframe-runtime/        # COBOL runtime library
│   ├── open-mainframe-dataset/        # File I/O, dataset handling
│   └── open-mainframe-encoding/       # EBCDIC/ASCII, code pages
├── tests/                  # Integration tests
│   ├── nist/               # NIST COBOL-85 test suite
│   └── regression/         # Output parity tests
└── examples/               # Example COBOL programs
```

**Cargo.toml (Workspace Root):**
```toml
[workspace]
resolver = "2"
members = [
    "crates/open-mainframe",
    "crates/open-mainframe-cobol",
    "crates/open-mainframe-jcl",
    "crates/open-mainframe-runtime",
    "crates/open-mainframe-dataset",
    "crates/open-mainframe-encoding",
]

[workspace.package]
version = "0.1.0"
edition = "2021"
rust-version = "1.75"
license = "Apache-2.0"
repository = "https://github.com/toreleon/OpenMainframe"

[workspace.dependencies]
# Shared dependencies with pinned versions
clap = { version = "4", features = ["derive", "env"] }
miette = { version = "7", features = ["fancy"] }
thiserror = "1"
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter"] }
serde = { version = "1", features = ["derive"] }
toml = "0.8"
```

**Crate Responsibilities:**

| Crate | Responsibility |
|-------|----------------|
| `open-mainframe` | CLI binary, orchestration |
| `open-mainframe-cobol` | Lexer, parser, AST, semantic analysis, LLVM codegen |
| `open-mainframe-jcl` | JCL parser, job orchestration, step execution |
| `open-mainframe-runtime` | COBOL runtime library, intrinsic functions |
| `open-mainframe-dataset` | File I/O, dataset abstraction, path mapping |
| `open-mainframe-encoding` | EBCDIC/ASCII conversion, code page tables |

### Development Tooling

| Tool | Purpose | Command |
|------|---------|---------|
| `cargo clippy` | Linting | `cargo clippy --all-targets` |
| `cargo fmt` | Formatting | `cargo fmt --all` |
| `cargo test` | Unit/integration tests | `cargo test --workspace` |
| `cargo fuzz` | Security fuzzing | `cargo +nightly fuzz run parser` |
| `cargo doc` | Documentation | `cargo doc --workspace --open` |

**Note:** Project initialization (`cargo new` + workspace setup) should be the first implementation task.

## Core Architectural Decisions

### Decision Priority Analysis

**Critical Decisions (Block Implementation):**
- AST design pattern: Hybrid typed + arena
- LLVM IR strategy: Direct generation from AST
- Decimal arithmetic: rust_decimal + custom BCD serialization
- EBCDIC handling: ASCII internal with conversion layer
- Runtime linkage: Static by default

**Important Decisions (Shape Architecture):**
- JCL process model: Single process sequential
- Dataset resolution: Config-based hybrid
- Testing harness: Rust-integrated with cargo test
- Distribution: Static binaries primary

**Deferred Decisions (Post-MVP):**
- Incremental compilation
- Parallel JCL step execution
- MIR optimization layer
- Dynamic runtime option

### Compiler Pipeline Decisions

#### AST Design: Hybrid Typed + Arena

**Decision:** Use strongly-typed AST nodes with arena allocation for memory efficiency.

**Implementation:**
```rust
// Typed AST nodes for safety and pattern matching
pub enum Statement {
    Move(MoveStatement),
    If(IfStatement),
    Perform(PerformStatement),
    // ...
}

// Arena allocation for performance
pub struct Ast<'arena> {
    arena: &'arena Bump,
    root: ProgramUnit<'arena>,
}
```

**Dependencies:**
```toml
bumpalo = "3"  # Arena allocator
```

**Rationale:**
- Strong typing catches AST manipulation errors at compile time
- Arena allocation reduces allocation overhead for large programs
- Pattern matching on enums is idiomatic Rust
- Similar to rustc's approach

#### LLVM IR Strategy: Direct Generation

**Decision:** Generate LLVM IR directly from AST without intermediate MIR.

**Rationale:**
- Simpler implementation for MVP
- COBOL's imperative structure maps well to LLVM
- Optimization handled by LLVM's mature passes
- Can add MIR layer post-MVP if needed

**Code Generation Pattern:**
```rust
impl<'ctx> CodeGen<'ctx> {
    fn compile_statement(&self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Move(m) => self.compile_move(m),
            Statement::If(i) => self.compile_if(i),
            // Direct translation to LLVM IR
        }
    }
}
```

### Data Representation Decisions

#### Decimal Arithmetic: rust_decimal + Custom BCD

**Decision:** Use `rust_decimal` for arithmetic operations with custom serialization for COMP-3 (packed decimal) storage format.

**Implementation:**
```rust
// Internal representation uses rust_decimal
use rust_decimal::Decimal;

// Custom serialization for COMP-3 format
pub fn to_packed_decimal(value: &Decimal, digits: usize) -> Vec<u8>;
pub fn from_packed_decimal(bytes: &[u8]) -> Decimal;
```

**Dependencies:**
```toml
rust_decimal = { version = "1", features = ["maths"] }
```

**Rationale:**
- `rust_decimal` provides 28-digit precision (exceeds IBM's 18)
- Proven, well-tested arithmetic implementation
- Custom BCD handles IBM-specific storage format
- Matches IBM behavior for all arithmetic operations

#### EBCDIC Handling: ASCII Internal

**Decision:** Convert EBCDIC to ASCII at I/O boundaries; process all text internally as UTF-8.

**Implementation:**
```rust
// open-mainframe-encoding crate
pub struct CodePage {
    ebcdic_to_ascii: [u8; 256],
    ascii_to_ebcdic: [u8; 256],
}

impl CodePage {
    pub const CP037: Self = /* US/Canada */;
    pub const CP1047: Self = /* Latin-1 Open Systems */;
    pub const CP500: Self = /* International */;
}
```

**Rationale:**
- Rust strings are UTF-8; internal ASCII simplifies processing
- Conversion at boundaries is predictable and testable
- Code page tables handle all IBM variations
- Clear separation of concerns in `open-mainframe-encoding` crate

### Runtime Architecture Decisions

#### Runtime Linkage: Static by Default

**Decision:** Statically link runtime library into executables by default; dynamic linking optional.

**Implementation:**
```rust
// In open-mainframe-runtime/Cargo.toml
[lib]
crate-type = ["staticlib", "cdylib", "rlib"]
```

**CLI Flag:**
```
open-mainframe compile --dynamic-runtime  # Use shared library
```

**Rationale:**
- Single-binary deployment critical for Docker/CI
- No runtime dependencies to manage
- Dynamic option available for production (smaller binaries)
- Follows Rust ecosystem conventions

#### Intrinsic Functions: Inline + Runtime Hybrid

**Decision:** Generate inline code for simple intrinsics; use runtime library for complex functions.

**Inline Generation (codegen):**
- LENGTH OF
- UPPER-CASE / LOWER-CASE
- Simple arithmetic (ADD, SUBTRACT)

**Runtime Library (open-mainframe-runtime):**
- Date/time functions (CURRENT-DATE, etc.)
- Complex string functions (INSPECT, STRING, UNSTRING)
- Mathematical functions (SIN, COS, SQRT)
- File I/O operations

**Rationale:**
- Inline generation for performance-critical simple operations
- Runtime library for complex logic requiring maintenance
- Maintains control over IBM-compatible behavior
- Clear boundary between generated and library code

### JCL Execution Decisions

#### Process Model: Single Process Sequential

**Decision:** Execute all JCL steps within a single process, sequentially.

**Implementation:**
```rust
pub struct JobRunner {
    job: Job,
    steps: Vec<Step>,
    condition_code: i32,
}

impl JobRunner {
    pub fn execute(&mut self) -> Result<i32> {
        for step in &self.steps {
            if self.should_execute(step) {
                self.condition_code = self.run_step(step)?;
            }
        }
        Ok(self.condition_code)
    }
}
```

**Rationale:**
- Simplest correct implementation for MVP
- Condition code propagation is straightforward
- Memory sharing between steps is implicit
- Can add parallel execution post-MVP

#### Dataset Resolution: Config-Based Hybrid

**Decision:** Use YAML configuration for DSN-to-path mapping with environment variable overrides.

**Configuration Format:**
```yaml
# open-mainframe.yaml
datasets:
  base_path: /data/datasets
  mappings:
    "PROD.CUSTOMER.FILE": "prod/customer.dat"
    "TEST.*.FILE": "test/{1}.dat"  # Wildcard support

environment:
  SYSOUT: /var/log/open-mainframe/sysout
  SYSIN: /dev/stdin
```

**Rationale:**
- Declarative configuration is maintainable
- Wildcard patterns reduce configuration burden
- Environment variables enable CI/CD flexibility
- Base path simplifies relative references

### Testing Infrastructure Decisions

#### NIST Test Harness: Rust-Integrated

**Decision:** Wrap NIST COBOL-85 tests in Rust test framework using data-driven testing.

**Implementation:**
```rust
// tests/nist/mod.rs
use datatest_stable::harness;

#[datatest_stable::files("tests/nist/cobol85/**/*.cbl")]
fn test_nist_program(path: &Path) {
    let result = compile_and_run(path);
    assert_output_matches(path, &result);
}
```

**Dependencies:**
```toml
[dev-dependencies]
datatest-stable = "0.2"
```

**Rationale:**
- Integration with `cargo test` and CI
- Parallel test execution for speed
- Individual test failure reporting
- Easy to add new test cases

#### Output Comparison: Configurable Precision

**Decision:** Byte-identical comparison by default with documented exceptions.

**Implementation:**
```rust
pub struct OutputComparison {
    mode: ComparisonMode,
    ignore_patterns: Vec<Regex>,  // Timestamps, paths
}

pub enum ComparisonMode {
    Exact,           // Byte-for-byte (default)
    Normalized,      // Ignore whitespace variations
    Semantic,        // Parse and compare structure
}
```

**Rationale:**
- Bit-identical is the primary goal
- Configurable exceptions for known differences (timestamps)
- Snapshot testing with `insta` for quick iteration
- Clear documentation of any deviations

### Distribution Decisions

#### Docker: Debian Slim Multi-Stage

**Decision:** Use multi-stage build with Debian slim as runtime base.

**Dockerfile Pattern:**
```dockerfile
# Build stage
FROM rust:1.75 as builder
RUN apt-get update && apt-get install -y llvm-17-dev
COPY . .
RUN cargo build --release

# Runtime stage
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libllvm17
COPY --from=builder /app/target/release/open-mainframe /usr/local/bin/
ENTRYPOINT ["open-mainframe"]
```

**Rationale:**
- glibc compatibility (LLVM requirement)
- Smaller than full Debian (~80MB vs ~300MB)
- Multi-stage keeps build dependencies out of runtime
- Can offer Alpine variant for musl environments

#### Binary Distribution: Static Primary

**Decision:** Distribute fully static binaries for GitHub releases; dynamic for package managers.

**Build Targets:**

| Target | Use Case | Size |
|--------|----------|------|
| `x86_64-unknown-linux-gnu` | Package managers | ~15MB |
| `x86_64-unknown-linux-musl` | GitHub release, Docker | ~20MB |
| `aarch64-unknown-linux-gnu` | ARM64 packages | ~14MB |
| `aarch64-unknown-linux-musl` | ARM64 portable | ~18MB |

**Rationale:**
- Static musl binaries work everywhere
- No runtime dependencies for portable use
- Dynamic builds available for size optimization
- Cross-compilation supported via `cross` tool

### Decision Impact Analysis

**Implementation Sequence:**
1. `open-mainframe-encoding` - Foundation for all data handling
2. `open-mainframe-cobol` lexer/parser - Core language processing
3. `open-mainframe-cobol` AST + semantic analysis - Program validation
4. `open-mainframe-runtime` - Runtime library functions
5. `open-mainframe-cobol` codegen - LLVM IR generation
6. `open-mainframe-dataset` - File I/O integration
7. `open-mainframe-jcl` - Job control interpreter
8. `open-mainframe` CLI - User interface

**Cross-Component Dependencies:**
- All crates depend on `open-mainframe-encoding` for data conversion
- `open-mainframe-cobol` codegen depends on `open-mainframe-runtime` for intrinsic signatures
- `open-mainframe-jcl` depends on `open-mainframe-dataset` for file operations
- `open-mainframe` CLI orchestrates all other crates

## Implementation Patterns & Consistency Rules

### Pattern Categories Defined

**Critical Conflict Points Identified:** 6 areas where developers/AI agents could make incompatible choices

| Category | Conflict Risk | Mitigation |
|----------|--------------|------------|
| Naming conventions | High | Strict naming rules per domain |
| Error handling | High | Crate-level error types with thiserror |
| AST node design | High | Consistent node structure with spans |
| Test organization | Medium | Clear location rules by test type |
| Logging/diagnostics | Medium | Structured logging with tracing |
| Public API surface | High | Visibility rules, #[non_exhaustive] |

### Naming Patterns

#### Rust Standard Conventions

All code follows standard Rust conventions enforced by `rustfmt` and `clippy`:

| Element | Convention | Example |
|---------|------------|---------|
| Modules | `snake_case` | `code_gen`, `semantic_analysis` |
| Types/Structs | `PascalCase` | `MoveStatement`, `SourceLocation` |
| Functions | `snake_case` | `parse_statement`, `emit_llvm_ir` |
| Constants | `SCREAMING_SNAKE_CASE` | `MAX_IDENTIFIER_LENGTH` |
| Crate names | `kebab-case` (Cargo.toml) | `open-mainframe-cobol`, `open-mainframe-runtime` |

#### Project-Specific Naming

**AST Nodes:**
```rust
// Pattern: {Verb}{Noun}Statement or {Noun}Expression
pub struct MoveStatement { ... }
pub struct PerformStatement { ... }
pub struct IfStatement { ... }
pub struct IdentifierExpression { ... }
pub struct LiteralExpression { ... }
```

**COBOL Keywords:** Use exact IBM keyword names in enum variants:
```rust
pub enum Verb {
    Move,
    Perform,
    Evaluate,
    GoTo,
    Display,
    Accept,
}
```

**Error Types:** Pattern `{Crate}{Category}Error`:
```rust
// open-mainframe-cobol
pub enum CobolParseError { ... }
pub enum CobolSemanticError { ... }
pub enum CobolCodegenError { ... }

// open-mainframe-jcl
pub enum JclParseError { ... }
pub enum JclExecutionError { ... }
```

**Visitor Traits:** Pattern `{Node}Visitor`:
```rust
pub trait StatementVisitor { ... }
pub trait ExpressionVisitor { ... }
pub trait DataItemVisitor { ... }
```

**Builders:** Pattern `{Type}Builder`:
```rust
pub struct CompileOptionsBuilder { ... }
pub struct JobBuilder { ... }
pub struct DatasetConfigBuilder { ... }
```

### Error Handling Patterns

#### Crate-Level Error Types

Each crate defines its own error enum using `thiserror` and `miette`:

```rust
// open-mainframe-cobol/src/error.rs
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum CobolError {
    #[error("Syntax error: {message}")]
    #[diagnostic(code(cobol::syntax), help("Check COBOL-85 reference"))]
    Syntax {
        message: String,
        #[label("error here")]
        span: SourceSpan,
    },

    #[error("Undefined identifier: {name}")]
    #[diagnostic(code(cobol::undefined))]
    UndefinedIdentifier {
        name: String,
        #[label("not defined")]
        span: SourceSpan,
        #[help]
        suggestion: Option<String>,
    },
}
```

#### Error Propagation Rules

| Rule | Description |
|------|-------------|
| Use `thiserror` | All error types derive `thiserror::Error` |
| Use `miette` | All user-facing errors implement `miette::Diagnostic` |
| Wrap with context | Crate errors wrap lower-level errors with `#[from]` |
| No `.unwrap()` | Never use `.unwrap()` in library code (tests only) |
| `anyhow` in CLI only | Use `anyhow` only in `open-mainframe` binary crate |
| `Result` everywhere | All fallible operations return `Result<T, E>` |

### AST Node Patterns

#### Node Structure Convention

Every AST node follows this structure:

```rust
/// A MOVE statement transfers data between identifiers.
#[derive(Debug, Clone, PartialEq)]
pub struct MoveStatement {
    /// Source location in the original COBOL file
    pub span: Span,
    /// The source value to move
    pub source: Expression,
    /// One or more destination identifiers
    pub destinations: Vec<Identifier>,
}
```

**Required Properties:**
| Property | Type | Purpose |
|----------|------|---------|
| `span` | `Span` | Source location for error reporting |
| Domain fields | Varies | The semantic content of the node |

**Required Derives:** `#[derive(Debug, Clone, PartialEq)]`

#### Visitor Pattern

All AST traversal uses the visitor pattern:

```rust
pub trait StatementVisitor {
    type Output;
    type Error;

    fn visit_move(&mut self, stmt: &MoveStatement) -> Result<Self::Output, Self::Error>;
    fn visit_if(&mut self, stmt: &IfStatement) -> Result<Self::Output, Self::Error>;
    fn visit_perform(&mut self, stmt: &PerformStatement) -> Result<Self::Output, Self::Error>;
}
```

#### Span and Source Location

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file_id: FileId,
}
```

### Test Organization Patterns

#### Test Location Rules

| Test Type | Location | Purpose |
|-----------|----------|---------|
| Unit tests | `#[cfg(test)] mod tests` in same file | Test individual functions/structs |
| Integration tests | `crates/{crate}/tests/*.rs` | Test crate public API |
| NIST compliance | `tests/nist/*.rs` | COBOL-85 compliance validation |
| Regression tests | `tests/regression/*.rs` | Prevent bug recurrence |
| Snapshot tests | `tests/{area}/snapshots/` | AST/output golden files |

#### Test Naming Convention

```rust
// Pattern: {action}_{subject}_{condition_or_variant}
#[test]
fn parse_move_statement_with_single_destination() { }

#[test]
fn parse_move_statement_fails_on_missing_to_keyword() { }
```

### Logging & Diagnostics Patterns

#### Structured Logging with tracing

```rust
use tracing::{debug, info, instrument};

#[instrument(skip(source), fields(file = %path.display()))]
pub fn compile_file(path: &Path, source: &str) -> Result<CompiledUnit> {
    info!("Starting compilation");
    // ...
}
```

#### Log Level Guidelines

| Level | Usage |
|-------|-------|
| `error!` | Unrecoverable, will abort |
| `warn!` | Recoverable issues |
| `info!` | High-level progress |
| `debug!` | Phase-level progress |
| `trace!` | Detailed internals |

### Public API Patterns

#### Visibility Rules

| Visibility | Usage |
|------------|-------|
| `pub` | Public API, documented, stable |
| `pub(crate)` | Internal to crate, may change |
| `pub(super)` | Internal to parent module |
| (private) | Internal to module |

#### Breaking Change Prevention

```rust
// Use #[non_exhaustive] for enums that may grow
#[non_exhaustive]
pub enum CobolDialect {
    Cobol85,
    Cobol2002,
    Ibm,
}

// Use builders for structs with many fields
impl CompileOptions {
    pub fn builder() -> CompileOptionsBuilder {
        CompileOptionsBuilder::default()
    }
}
```

### Enforcement Guidelines

#### All Developers/AI Agents MUST:

1. **Run `cargo fmt` before committing** - Formatting is non-negotiable
2. **Pass `cargo clippy` with no warnings** - Lint rules are enforced
3. **Include spans on all AST nodes** - Required for error reporting
4. **Use `Result` for fallible operations** - No panics in library code
5. **Document all public items** - `cargo doc` must succeed with no warnings
6. **Add tests for new functionality** - No untested code in PRs
7. **Follow naming conventions** - As documented above

#### CI Pipeline Checks

```yaml
- name: Format check
  run: cargo fmt --all -- --check

- name: Lint check
  run: cargo clippy --all-targets -- -D warnings

- name: Doc check
  run: cargo doc --no-deps
  env:
    RUSTDOCFLAGS: "-D warnings"

- name: Test
  run: cargo test --workspace
```

### Anti-Patterns (What to Avoid)

```rust
// ✗ Missing span on AST node
pub struct BadStatement {
    pub value: String,  // No span!
}

// ✗ Using unwrap in library code
pub fn bad_parse(source: &str) -> Ast {
    let tokens = tokenize(source).unwrap();  // NO!
}

// ✗ Inconsistent naming
pub struct move_stmt { ... }     // Should be MoveStatement
pub fn ParseStatement() { ... }  // Should be parse_statement
```

## Project Structure & Boundaries

### Complete Project Directory Structure

```
open-mainframe/
├── .github/
│   ├── workflows/
│   │   ├── ci.yml                    # Main CI pipeline
│   │   ├── release.yml               # Release automation
│   │   └── nist-compliance.yml       # Weekly NIST test run
│   ├── ISSUE_TEMPLATE/
│   │   ├── bug_report.md
│   │   ├── feature_request.md
│   │   └── compatibility_issue.md
│   └── PULL_REQUEST_TEMPLATE.md
│
├── crates/
│   ├── open-mainframe/                    # CLI binary crate
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── main.rs               # Entry point
│   │   │   ├── cli.rs                # Clap argument definitions
│   │   │   ├── commands/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── compile.rs        # open-mainframe compile
│   │   │   │   ├── run.rs            # open-mainframe run
│   │   │   │   ├── check.rs          # open-mainframe check
│   │   │   │   └── convert.rs        # open-mainframe convert
│   │   │   ├── config.rs             # Configuration loading
│   │   │   └── output.rs             # Output formatting
│   │   └── tests/
│   │       └── cli_integration.rs
│   │
│   ├── open-mainframe-cobol/                    # COBOL compiler crate
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs                # Public API
│   │   │   ├── lexer/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── token.rs          # Token types
│   │   │   │   ├── scanner.rs        # Lexical scanner
│   │   │   │   └── source.rs         # Source file handling
│   │   │   ├── parser/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── grammar.rs        # Recursive descent parser
│   │   │   │   ├── copybook.rs       # COPY statement handling
│   │   │   │   └── error_recovery.rs # Parse error recovery
│   │   │   ├── ast/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── nodes.rs          # AST node definitions
│   │   │   │   ├── visitor.rs        # Visitor traits
│   │   │   │   └── span.rs           # Source locations
│   │   │   ├── semantic/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── symbol_table.rs   # Symbol resolution
│   │   │   │   ├── type_check.rs     # Type checking
│   │   │   │   └── data_division.rs  # DATA DIVISION analysis
│   │   │   ├── codegen/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── context.rs        # LLVM context management
│   │   │   │   ├── module.rs         # Module code generation
│   │   │   │   ├── statements.rs     # Statement codegen
│   │   │   │   ├── expressions.rs    # Expression codegen
│   │   │   │   ├── intrinsics.rs     # Intrinsic function codegen
│   │   │   │   └── decimal.rs        # Decimal arithmetic codegen
│   │   │   ├── dialect/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── cobol85.rs        # COBOL-85 specifics
│   │   │   │   ├── cobol2002.rs      # COBOL-2002 extensions
│   │   │   │   └── ibm.rs            # IBM COBOL extensions
│   │   │   ├── error.rs              # Error types
│   │   │   └── compiler.rs           # High-level compiler API
│   │   └── tests/
│   │       ├── lexer_tests.rs
│   │       ├── parser_tests.rs
│   │       ├── semantic_tests.rs
│   │       └── codegen_tests.rs
│   │
│   ├── open-mainframe-jcl/                      # JCL interpreter crate
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── lexer.rs              # JCL tokenizer
│   │   │   ├── parser.rs             # JCL parser
│   │   │   ├── ast.rs                # JCL AST nodes
│   │   │   ├── executor/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── job.rs            # Job execution
│   │   │   │   ├── step.rs           # Step execution
│   │   │   │   ├── condition.rs      # Condition code handling
│   │   │   │   └── proc.rs           # Procedure expansion
│   │   │   ├── symbols.rs            # Symbolic parameter handling
│   │   │   └── error.rs
│   │   └── tests/
│   │       └── jcl_tests.rs
│   │
│   ├── open-mainframe-runtime/                  # COBOL runtime library
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── intrinsics/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── string.rs         # String functions
│   │   │   │   ├── numeric.rs        # Numeric functions
│   │   │   │   ├── datetime.rs       # Date/time functions
│   │   │   │   └── math.rs           # Mathematical functions
│   │   │   ├── io/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── display.rs        # DISPLAY verb
│   │   │   │   ├── accept.rs         # ACCEPT verb
│   │   │   │   └── file.rs           # File I/O runtime
│   │   │   ├── decimal/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── arithmetic.rs     # Decimal operations
│   │   │   │   └── conversion.rs     # Decimal conversions
│   │   │   ├── memory.rs             # WORKING-STORAGE management
│   │   │   ├── condition.rs          # Condition code management
│   │   │   └── abend.rs              # ABEND handling
│   │   └── tests/
│   │       └── runtime_tests.rs
│   │
│   ├── open-mainframe-dataset/                  # Dataset/file handling crate
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── catalog.rs            # Dataset catalog/resolution
│   │   │   ├── sequential.rs         # Sequential file (QSAM)
│   │   │   ├── record/
│   │   │   │   ├── mod.rs
│   │   │   │   ├── fixed.rs          # Fixed-length records
│   │   │   │   └── variable.rs       # Variable-length records
│   │   │   ├── allocation.rs         # Dataset allocation
│   │   │   ├── disposition.rs        # DISP handling
│   │   │   └── error.rs
│   │   └── tests/
│   │       └── dataset_tests.rs
│   │
│   └── open-mainframe-encoding/                 # Encoding/conversion crate
│       ├── Cargo.toml
│       ├── src/
│       │   ├── lib.rs
│       │   ├── ebcdic/
│       │   │   ├── mod.rs
│       │   │   ├── tables.rs         # Code page tables
│       │   │   ├── cp037.rs          # US/Canada
│       │   │   ├── cp1047.rs         # Latin-1 Open Systems
│       │   │   └── cp500.rs          # International
│       │   ├── decimal/
│       │   │   ├── mod.rs
│       │   │   ├── packed.rs         # COMP-3 packed decimal
│       │   │   ├── zoned.rs          # Zoned decimal
│       │   │   └── binary.rs         # COMP binary
│       │   └── conversion.rs         # High-level conversion API
│       └── tests/
│           └── encoding_tests.rs
│
├── tests/                            # Workspace-level integration tests
│   ├── nist/
│   │   ├── mod.rs                    # NIST test harness
│   │   └── cobol85/                  # NIST COBOL-85 test programs
│   ├── regression/
│   │   ├── mod.rs
│   │   └── issues/                   # Tests for specific issues
│   ├── integration/
│   │   ├── compile_and_run.rs
│   │   └── jcl_execution.rs
│   └── common/
│       └── mod.rs                    # Shared test utilities
│
├── examples/
│   ├── hello-world/
│   │   ├── HELLO.cbl
│   │   ├── HELLO.jcl
│   │   └── README.md
│   ├── file-processing/
│   │   ├── READFILE.cbl
│   │   ├── WRITFILE.cbl
│   │   └── FILEIO.jcl
│   ├── batch-job/
│   │   ├── STEP1.cbl
│   │   ├── STEP2.cbl
│   │   └── BATCH.jcl
│   └── copybook-demo/
│       ├── MAIN.cbl
│       ├── copybooks/
│       │   └── CUSTOMER.cpy
│       └── COPYDEMO.jcl
│
├── docs/
│   ├── architecture/
│   ├── user-guide/
│   └── contributing/
│
├── docker/
│   ├── Dockerfile
│   ├── Dockerfile.alpine
│   └── docker-compose.yml
│
├── Cargo.toml                        # Workspace manifest
├── Cargo.lock
├── rust-toolchain.toml
├── rustfmt.toml
├── clippy.toml
├── deny.toml
├── .gitignore
├── LICENSE
├── README.md
└── CHANGELOG.md
```

### Architectural Boundaries

#### Crate Dependency Graph

```
                    ┌─────────────┐
                    │  open-mainframe  │  (binary)
                    │    (CLI)    │
                    └──────┬──────┘
                           │
         ┌─────────────────┼─────────────────┐
         │                 │                 │
         ▼                 ▼                 ▼
   ┌───────────┐    ┌───────────┐    ┌───────────┐
   │ open-mainframe-cobol │    │  open-mainframe-jcl  │    │open-mainframe-dataset│
   │(compiler) │    │(interpret)│    │  (files)  │
   └─────┬─────┘    └─────┬─────┘    └─────┬─────┘
         │                │                 │
         │                └────────┬────────┘
         │                         │
         ▼                         ▼
   ┌───────────┐            ┌───────────┐
   │open-mainframe-runtime│            │open-mainframe-encoding│
   │ (library) │            │  (codec)  │
   └─────┬─────┘            └───────────┘
         │                         ▲
         └─────────────────────────┘
```

#### Crate Responsibilities

| Crate | Responsibility | Public API | Dependencies |
|-------|----------------|------------|--------------|
| `open-mainframe` | CLI binary, orchestration | `main()` only | All crates |
| `open-mainframe-cobol` | COBOL compilation | `Compiler`, `CompileOptions`, `CobolError` | `open-mainframe-runtime`, `open-mainframe-encoding` |
| `open-mainframe-jcl` | JCL interpretation | `Job`, `JobRunner`, `JclError` | `open-mainframe-dataset` |
| `open-mainframe-runtime` | COBOL runtime | Intrinsic functions, I/O ops | `open-mainframe-encoding` |
| `open-mainframe-dataset` | File operations | `Dataset`, `RecordFile` | `open-mainframe-encoding` |
| `open-mainframe-encoding` | Data conversion | `CodePage`, `PackedDecimal` | None (leaf crate) |

#### Communication Patterns

**Crate → Crate:** All communication via public Rust APIs (no IPC)

**Compiler → Runtime:** Compiler generates calls to runtime functions exposed via `#[no_mangle] pub extern "C"`

**JCL → Executables:** JCL executor spawns compiled COBOL executables; condition codes via exit codes

### Data Flow

```
User Input (COBOL, JCL, data files)
         │
         ▼
    open-mainframe CLI
         │
    ┌────┼────┐
    ▼    ▼    ▼
compile  run  check
    │    │    │
    ▼    ▼    ▼
open-mainframe-cobol → open-mainframe-jcl → open-mainframe-cobol
    │         │         │
    ▼         ▼         ▼
Executable  Jobs    Diagnostics
```

### File Organization Patterns

| File | Purpose | Format |
|------|---------|--------|
| `Cargo.toml` | Workspace manifest | TOML |
| `rust-toolchain.toml` | Rust version | TOML |
| `rustfmt.toml` | Formatting | TOML |
| `clippy.toml` | Linting | TOML |
| `.github/workflows/*.yml` | CI/CD | YAML |

### Test Organization

| Test Type | Location | Runner |
|-----------|----------|--------|
| Unit tests | `src/**/*.rs` | `cargo test` |
| Crate integration | `crates/*/tests/*.rs` | `cargo test -p crate` |
| Workspace integration | `tests/**/*.rs` | `cargo test --workspace` |
| NIST compliance | `tests/nist/**/*.rs` | `cargo test nist` |

### Development Commands

```bash
cargo fmt --all                    # Format code
cargo clippy --all-targets         # Lint check
cargo test --workspace             # Run all tests
cargo doc --workspace --open       # Build docs
cargo build --release              # Release build
```

## Architecture Validation Results

### Coherence Validation ✅

**Decision Compatibility:**
All technology choices are compatible and work together:
- Rust 1.75+ as implementation language
- LLVM 17 via inkwell for code generation
- clap 4.x for CLI with POSIX conventions
- thiserror + miette for error handling
- tracing for structured logging

No version conflicts or incompatibilities detected.

**Pattern Consistency:**
All implementation patterns align with technology choices:
- Rust naming conventions enforced via rustfmt/clippy
- Error handling patterns use standard Rust idioms
- AST patterns follow established compiler design practices
- Test patterns align with cargo test conventions

**Structure Alignment:**
Project structure supports all architectural decisions:
- 6-crate workspace enables separation of concerns
- Dependency graph is a strict DAG (no cycles)
- Test locations follow defined organization patterns
- CI/CD structure supports all enforcement checks

### Requirements Coverage Validation ✅

**Functional Requirements Coverage:**
All 96 functional requirements have architectural support:

| Category | FRs | Primary Crate |
|----------|-----|---------------|
| COBOL Compilation | FR1-15 | open-mainframe-cobol |
| JCL Processing | FR16-27 | open-mainframe-jcl |
| File Operations | FR28-36 | open-mainframe-dataset |
| Data Conversion | FR37-43 | open-mainframe-encoding |
| Runtime | FR44-53 | open-mainframe-runtime |
| CLI | FR54-62 | open-mainframe |
| Validation | FR63-70 | open-mainframe-cobol, open-mainframe-jcl |
| Configuration | FR71-76 | open-mainframe |
| Distribution | FR77-82 | Docker, Cargo |
| IDE Integration | FR83-86 | open-mainframe-cobol (post-MVP) |
| Documentation | FR87-92 | docs/ |
| Compatibility | FR93-96 | tests/nist/ |

**Non-Functional Requirements Coverage:**
All 27 NFRs have architectural support through technology choices, patterns, and structure decisions.

### Implementation Readiness Validation ✅

**Decision Completeness:**
- All critical decisions documented with specific versions
- Rationale provided for each major choice
- Trade-offs clearly articulated

**Structure Completeness:**
- Complete directory tree with all files specified
- Crate responsibilities clearly defined
- Integration points documented

**Pattern Completeness:**
- Naming conventions cover all code elements
- Error handling patterns are comprehensive
- Test organization rules are clear
- CI enforcement mechanisms defined

### Gap Analysis Results

**Critical Gaps:** None

**Important Gaps (Non-blocking, for future phases):**
- LSP implementation details (v1.1)
- VSAM architecture extension points (v1.1)
- Incremental compilation design (v1.2)

### Architecture Completeness Checklist

**✅ Requirements Analysis**
- [x] Project context thoroughly analyzed
- [x] Scale and complexity assessed (HIGH)
- [x] Technical constraints identified
- [x] Cross-cutting concerns mapped

**✅ Architectural Decisions**
- [x] Parsing: Hand-written recursive descent
- [x] Code generation: LLVM via inkwell
- [x] CLI: clap 4.x with derive
- [x] Error handling: thiserror + miette
- [x] Decimal arithmetic: rust_decimal + custom BCD
- [x] EBCDIC: ASCII internal with conversion layer
- [x] Runtime linkage: Static by default
- [x] Testing: Rust-integrated with cargo test

**✅ Implementation Patterns**
- [x] Naming conventions established
- [x] Error handling patterns defined
- [x] AST node patterns specified
- [x] Test organization rules documented
- [x] Logging patterns defined
- [x] Public API patterns established

**✅ Project Structure**
- [x] Complete directory structure defined
- [x] 6 crates with clear responsibilities
- [x] Dependency graph documented
- [x] CI/CD pipeline structure defined

### Architecture Readiness Assessment

**Overall Status:** ✅ READY FOR IMPLEMENTATION

**Confidence Level:** HIGH

**Key Strengths:**
1. Clean separation of concerns via crate boundaries
2. Industry-standard tooling (LLVM, clap, tracing)
3. Comprehensive patterns prevent implementation conflicts
4. Clear requirements-to-structure mapping
5. Enforceable consistency via CI checks

**Areas for Future Enhancement:**
1. LSP server architecture details (when implementing IDE integration)
2. VSAM/CICS architecture (when expanding beyond MVP)
3. Performance benchmarking infrastructure
4. Incremental compilation for faster development cycles

### Implementation Handoff

**AI Agent Guidelines:**
1. Follow all architectural decisions exactly as documented
2. Use implementation patterns consistently across all components
3. Respect crate boundaries and dependency graph
4. Run `cargo fmt` and `cargo clippy` before all commits
5. Refer to this document for all architectural questions

**First Implementation Priority:**
```bash
# 1. Initialize workspace
cargo new open-mainframe --name workspace-root
cd open-mainframe

# 2. Create crate structure
mkdir -p crates/{open-mainframe,open-mainframe-cobol,open-mainframe-jcl,open-mainframe-runtime,open-mainframe-dataset,open-mainframe-encoding}/src

# 3. Set up workspace Cargo.toml
# 4. Implement open-mainframe-encoding first (leaf crate, no dependencies)
# 5. Then open-mainframe-runtime, open-mainframe-dataset, open-mainframe-cobol, open-mainframe-jcl
# 6. Finally open-mainframe CLI
```

