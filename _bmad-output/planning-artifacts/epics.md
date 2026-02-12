---
stepsCompleted: [1, 2, 3]
inputDocuments: [prd.md, architecture.md]
workflowType: 'epics'
project_name: 'zOS-clone'
user_name: 'Tore'
date: '2026-02-12'
status: 'complete'
completedAt: '2026-02-12'
totalEpics: 14
totalStories: 84
frCoverage: '96/96'
nfrCoverage: '27/27'
---

# zOS-clone - Epic Breakdown

## Overview

This document provides the complete epic and story breakdown for zOS-clone, decomposing the requirements from the PRD, and Architecture requirements into implementable stories.

## Requirements Inventory

### Functional Requirements

**COBOL Compilation (FR1-FR15)**
- FR1: Developer can compile COBOL-85 source files to native Linux executables
- FR2: Developer can compile COBOL source with COBOL-2002 intrinsic functions
- FR3: Developer can specify copybook search paths for COPY statement resolution
- FR4: Developer can compile programs with nested COPY/COPYBOOK includes
- FR5: Developer can receive source-level error messages with line numbers and descriptions
- FR6: Developer can receive warning messages for non-critical issues without failing compilation
- FR7: Developer can specify the target COBOL dialect (cobol85, cobol2002, ibm)
- FR8: Developer can specify optimization level for compiled output
- FR9: Developer can include debug symbols in compiled executables
- FR10: Developer can compile programs containing all COBOL-85 divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- FR11: Developer can compile programs using WORKING-STORAGE, LOCAL-STORAGE, and LINKAGE sections
- FR12: Developer can compile programs with PERFORM statements (inline, out-of-line, VARYING, UNTIL)
- FR13: Developer can compile programs with table handling (OCCURS, SEARCH, INDEXED BY)
- FR14: Developer can compile programs with CALL statements (static and dynamic)
- FR15: Developer can compile programs with string handling (STRING, UNSTRING, INSPECT, REFERENCE MODIFICATION)

**JCL Processing (FR16-FR27)**
- FR16: Operations engineer can execute JCL job streams containing JOB, EXEC, and DD statements
- FR17: Operations engineer can execute multi-step jobs with condition code handling between steps
- FR18: Operations engineer can use cataloged procedures (PROC) in job streams
- FR19: Operations engineer can use in-stream procedures (PROC/PEND) in job streams
- FR20: Operations engineer can define symbolic parameters and substitution in JCL
- FR21: Operations engineer can specify SYSOUT destinations for job output
- FR22: Operations engineer can specify SYSIN for inline input data
- FR23: Operations engineer can reference sequential datasets in DD statements
- FR24: Operations engineer can use DISP parameter for dataset disposition handling
- FR25: Operations engineer can use COND parameter for conditional step execution
- FR26: Operations engineer can receive job completion status with condition codes
- FR27: Operations engineer can execute jobs in dry-run mode (parse and validate without execution)

**File & Dataset Operations (FR28-FR36)**
- FR28: Developer can read sequential files (QSAM) from COBOL programs
- FR29: Developer can write sequential files (QSAM) from COBOL programs
- FR30: Developer can process fixed-length record files
- FR31: Developer can process variable-length record files
- FR32: Developer can specify record format (RECFM) and logical record length (LRECL)
- FR33: Developer can map z/OS dataset names to Linux filesystem paths
- FR34: Developer can configure base paths for dataset resolution
- FR35: Operations engineer can allocate new datasets with SPACE and DCB parameters
- FR36: Developer can handle file status codes in COBOL programs

**Data Conversion (FR37-FR43)**
- FR37: Developer can process EBCDIC-encoded data files
- FR38: Developer can convert data between EBCDIC and ASCII encodings
- FR39: Developer can process packed decimal (COMP-3) data with correct arithmetic
- FR40: Developer can process zoned decimal data
- FR41: Developer can process binary (COMP) data with correct byte ordering
- FR42: Developer can convert datasets between formats using CLI utility
- FR43: Developer can specify source and target encodings for conversion

**Runtime Execution (FR44-FR53)**
- FR44: Developer can execute compiled COBOL programs on Linux
- FR45: Developer can pass parameters to COBOL programs at runtime
- FR46: Runtime can initialize WORKING-STORAGE according to COBOL specifications
- FR47: Runtime can handle ABEND conditions and generate diagnostic dumps
- FR48: Runtime can propagate condition codes between job steps
- FR49: Runtime can execute DISPLAY statements with correct output formatting
- FR50: Runtime can execute ACCEPT statements for input handling
- FR51: Runtime can perform decimal arithmetic matching IBM precision
- FR52: Runtime can handle all standard COBOL intrinsic functions
- FR53: Developer can set maximum condition code threshold for job execution

**Developer CLI (FR54-FR62)**
- FR54: Developer can invoke compilation via `zos-clone compile` command
- FR55: Developer can invoke JCL execution via `zos-clone run` command
- FR56: Developer can validate source without compilation via `zos-clone check` command
- FR57: Developer can convert datasets via `zos-clone convert` command
- FR58: Developer can view version information via `zos-clone version` command
- FR59: Developer can access help for any command via `zos-clone help` command
- FR60: Developer can specify output file name for compiled executables
- FR61: Developer can receive exit codes indicating success, warning, error, or fatal conditions
- FR62: Developer can receive output in multiple formats (text, JSON, SARIF) for tooling integration

**Validation & Diagnostics (FR63-FR70)**
- FR63: Developer can validate COBOL syntax without full compilation
- FR64: Developer can validate JCL syntax without execution
- FR65: Developer can receive clear error messages indicating problem location and cause
- FR66: Developer can receive suggestions for common errors
- FR67: Developer can configure warning levels (none, basic, strict)
- FR68: Developer can treat warnings as errors via strict mode
- FR69: Operations engineer can view execution logs with timestamps and step details
- FR70: Developer can reproduce ABEND conditions locally for debugging

**Configuration Management (FR71-FR76)**
- FR71: Developer can configure copybook search paths via configuration file
- FR72: Developer can configure dataset base paths via configuration file
- FR73: Developer can configure default COBOL dialect via configuration file
- FR74: Developer can override configuration options via command-line flags
- FR75: Developer can use environment variables for configuration
- FR76: Operations engineer can configure SYSOUT directory location

**Distribution & Installation (FR77-FR82)**
- FR77: Developer can install zOS-clone via Docker container
- FR78: Developer can install zOS-clone via pre-built binary releases
- FR79: Developer can install zOS-clone via Cargo (Rust package manager)
- FR80: Developer can build zOS-clone from source
- FR81: Developer can run zOS-clone on Linux x86_64 systems
- FR82: Developer can run zOS-clone on Linux ARM64 systems

**IDE & Tooling Integration (FR83-FR86)**
- FR83: Developer can use Language Server Protocol for IDE integration
- FR84: Developer can receive diagnostic messages via LSP publishDiagnostics
- FR85: Developer can navigate to copybook definitions via LSP go-to-definition
- FR86: Developer can view variable information via LSP hover

**Documentation & Help (FR87-FR92)**
- FR87: Developer can access quick start guide for initial setup
- FR88: Developer can access CLI reference documentation
- FR89: Developer can access migration guide for mainframe-to-zOS-clone transition
- FR90: Developer can access compatibility matrix showing supported features
- FR91: Contributor can access contributing guide for open source participation
- FR92: Developer can access API reference for programmatic usage

**Compatibility Validation (FR93-FR96)**
- FR93: Developer can run NIST COBOL-85 test suite against zOS-clone
- FR94: Developer can compare output against IBM compiler for parity validation
- FR95: Operations engineer can run parallel workloads on z/OS and zOS-clone for comparison
- FR96: Developer can report compatibility issues via GitHub issues

### NonFunctional Requirements

**Performance (NFR-P1 to NFR-P4)**
- NFR-P1: Compilation Speed - Programs <10K lines compile in <10s; <50K lines in <60s; cold startup <500ms
- NFR-P2: Execution Performance - Within 2x of IBM-compiled; numeric ops within 1.5x; file I/O at 80%+ native
- NFR-P3: JCL Processing - Parse/validate <1s for 500 statements; step transitions <100ms; symbolic resolution <50ms
- NFR-P4: Resource Efficiency - Compiler memory <2GB for 100K lines; predictable runtime allocation; <1% idle CPU

**Security (NFR-S1 to NFR-S3)**
- NFR-S1: Distribution Security - Signed binaries, SHA-256 checksums, vulnerability scanning, CVE auditing
- NFR-S2: Code Execution Safety - Isolated memory spaces, buffer overflow protection, restricted file access, no shell execution
- NFR-S3: Input Validation - Encoding validation, injection sanitization, configurable size limits (100MB source, 10GB data)

**Scalability (NFR-SC1 to NFR-SC3)**
- NFR-SC1: Workload Capacity - 10 concurrent compiles, 100 concurrent jobs, 100GB files, 10B records
- NFR-SC2: Container Scaling - Stateless containers, Kubernetes auto-scaling, <5s startup, configurable resource limits
- NFR-SC3: Enterprise Growth - Extensible for VSAM/CICS/DB2, plugin architecture, enterprise deployment support

**Integration (NFR-I1 to NFR-I4)**
- NFR-I1: CI/CD Integration - POSIX exit codes, JSON/SARIF output, GitHub Actions/GitLab/Jenkins examples
- NFR-I2: IDE Integration - LSP 3.17+ compliance, diagnostics with location, <200ms symbol navigation
- NFR-I3: Container Ecosystem - Docker Hub/GHCR, minimal base images, multi-arch (amd64/arm64), Helm charts
- NFR-I4: Data Format Compatibility - All IBM code pages (CP037/CP1047/CP500), configurable record formats

**Reliability (NFR-R1 to NFR-R4)**
- NFR-R1: Output Correctness - Deterministic compilation, 18-digit decimal precision, identical date/time ops, 95%+ NIST
- NFR-R2: Error Handling - All errors caught, no panics, no corruption, diagnostic dumps
- NFR-R3: Data Integrity - Atomic file ops, no data loss on shutdown, checkpoint support, documented recovery
- NFR-R4: Availability Targets - Immediate CLI availability, 99.9% batch runtime, <5min MTTR

**Portability (NFR-PO1 to NFR-PO3)**
- NFR-PO1: Platform Support - Linux x86_64/ARM64, kernel 4.19+, Ubuntu 20.04+/RHEL 8+/Debian 11+/Alpine 3.14+
- NFR-PO2: Dependency Management - glibc 2.31+ or musl only, static linking option, self-contained containers
- NFR-PO3: Configuration Portability - YAML/TOML formats, environment variable overrides, shareable configs

**Usability (NFR-U1 to NFR-U3)**
- NFR-U1: CLI Experience - POSIX conventions, specific error messages, tab completion (Bash/Zsh/Fish), progress indicators
- NFR-U2: Learning Curve - Hello World in <5min, copy-paste examples, doc-linked errors, migration guides
- NFR-U3: Diagnostic Quality - IBM-style error codes, source context, suggested fixes, verbose mode

**Maintainability (NFR-M1 to NFR-M3)**
- NFR-M1: Code Quality - cargo clippy clean, 80%+ coverage, documented public APIs, consistent formatting
- NFR-M2: Contribution Friendliness - Single-command build, <10min test suite, feature tests, architecture docs
- NFR-M3: Release Management - SemVer strict, breaking changes in major versions only, deprecation warnings, changelog

### Additional Requirements

**From Architecture - Starter Template:**
- 6-crate Cargo workspace structure exists (zos-clone, zos-cobol, zos-jcl, zos-runtime, zos-dataset, zos-encoding)
- Workspace Cargo.toml with shared dependencies defined
- Project initialization is the first implementation task

**From Architecture - Implementation Sequence:**
1. `zos-encoding` - Foundation for all data handling (leaf crate, no dependencies)
2. `zos-cobol` lexer/parser - Core language processing
3. `zos-cobol` AST + semantic analysis - Program validation
4. `zos-runtime` - Runtime library functions
5. `zos-cobol` codegen - LLVM IR generation
6. `zos-dataset` - File I/O integration
7. `zos-jcl` - Job control interpreter
8. `zos-clone` CLI - User interface

**From Architecture - Cross-Cutting Concerns:**
- Error Handling & Diagnostics: thiserror + miette across all crates
- Configuration Management: YAML/TOML + CLI flags + env vars
- Data Encoding (EBCDIC/ASCII): zos-encoding shared library
- Logging & Observability: tracing crate throughout
- Testing Infrastructure: NIST compliance, regression, fuzzing

**From Architecture - Technical Decisions:**
- Parser: Hand-written recursive descent (not parser generators)
- Code Generation: LLVM via inkwell crate
- CLI Framework: clap 4.x with derive macros
- Decimal Arithmetic: rust_decimal + custom BCD serialization
- Runtime Linkage: Static by default, dynamic optional

### FR Coverage Map

| Category | FRs | Primary Crate | Architecture Support |
|----------|-----|---------------|----------------------|
| COBOL Compilation | FR1-FR15 | zos-cobol | Lexer, parser, AST, codegen modules |
| JCL Processing | FR16-FR27 | zos-jcl | Executor module with job/step/proc handling |
| File & Dataset Operations | FR28-FR36 | zos-dataset | Sequential, record, catalog modules |
| Data Conversion | FR37-FR43 | zos-encoding | EBCDIC tables, decimal conversion |
| Runtime Execution | FR44-FR53 | zos-runtime | Intrinsics, I/O, decimal, memory modules |
| Developer CLI | FR54-FR62 | zos-clone | Commands module (compile, run, check, convert) |
| Validation & Diagnostics | FR63-FR70 | zos-cobol, zos-jcl | Error types with miette diagnostics |
| Configuration Management | FR71-FR76 | zos-clone | Config module with YAML/env support |
| Distribution & Installation | FR77-FR82 | Build infrastructure | Docker, Cargo, GitHub releases |
| IDE & Tooling Integration | FR83-FR86 | zos-cobol (post-MVP) | LSP server capability |
| Documentation & Help | FR87-FR92 | docs/ directory | User guide, contributing, architecture |
| Compatibility Validation | FR93-FR96 | tests/nist/ | NIST test harness integration |

## Epic List

| Epic | Title | Stories | Key FRs | Crate(s) | Dependencies |
|------|-------|---------|---------|----------|--------------|
| 1 | Project Foundation & Workspace Setup | 5 | FR79-FR80, NFR-M1/M2 | All | None |
| 2 | Data Encoding Layer | 6 | FR37-FR43, NFR-I4 | zos-encoding | Epic 1 |
| 3 | COBOL Lexer & Source Handling | 6 | FR3-FR7, FR10 | zos-cobol | Epic 1 |
| 4 | COBOL Parser & AST | 7 | FR10-FR15 | zos-cobol | Epic 3 |
| 5 | Semantic Analysis & Validation | 5 | FR5-FR6, FR63, FR65-FR68 | zos-cobol | Epic 4 |
| 6 | COBOL Runtime Library | 8 | FR44-FR52, NFR-R1 | zos-runtime | Epics 1, 2 |
| 7 | LLVM Code Generation | 6 | FR1-FR2, FR8-FR9, FR44 | zos-cobol | Epics 4, 5, 6 |
| 8 | Dataset & File Operations | 7 | FR28-FR36, NFR-R3 | zos-dataset | Epics 1, 2 |
| 9 | JCL Interpreter | 8 | FR16-FR27, FR64 | zos-jcl | Epic 8 |
| 10 | CLI & Developer Tools | 7 | FR54-FR62, FR67-FR70, NFR-U1 | zos-clone | Epics 7, 8, 9 |
| 11 | Configuration System | 4 | FR71-FR76, NFR-PO3 | zos-clone | Epic 10 |
| 12 | Testing & NIST Compliance | 5 | FR93-FR96, NFR-R1 | tests/ | Epics 7, 9 |
| 13 | Distribution & Packaging | 5 | FR77-FR82, NFR-S1, NFR-I3 | CI/CD | Epic 10 |
| 14 | Documentation | 5 | FR87-FR92, NFR-U2 | docs/ | Epic 10 |

**Total: 14 Epics, 84 Stories**

---

## Epic 1: Project Foundation & Workspace Setup

**Goal:** Establish the Cargo workspace structure, shared dependencies, CI/CD pipeline, and development tooling to enable parallel development across all crates.

**Architectural Basis:** Architecture Section "Project Initialization" - 6-crate workspace with shared dependencies.

### Story 1.1: Initialize Cargo Workspace

As a **developer**,
I want **the project initialized as a Cargo workspace with all six crates**,
So that **I can build and test all components with a single command**.

**Acceptance Criteria:**

**Given** a fresh clone of the repository
**When** I run `cargo build --workspace`
**Then** all six crates compile successfully with no errors
**And** the workspace structure matches the architecture specification

**Given** the workspace Cargo.toml
**When** I inspect the file
**Then** it contains workspace.dependencies for all shared crates (clap, miette, thiserror, tracing, serde, toml)
**And** resolver = "2" is specified

**Technical Notes:**
- Create: `Cargo.toml` (workspace root), `crates/{zos-clone,zos-cobol,zos-jcl,zos-runtime,zos-dataset,zos-encoding}/Cargo.toml`
- Each crate starts with minimal `lib.rs` or `main.rs`

---

### Story 1.2: Configure Rust Toolchain & Formatting

As a **contributor**,
I want **consistent Rust toolchain configuration and formatting rules**,
So that **all developers produce identically formatted code**.

**Acceptance Criteria:**

**Given** the repository contains `rust-toolchain.toml`
**When** I run `cargo fmt --all`
**Then** code formatting is applied consistently across all crates

**Given** the repository contains `rustfmt.toml`
**When** I check the configuration
**Then** it specifies edition = "2021" and max_width = 100

**Given** the repository contains `clippy.toml`
**When** I run `cargo clippy --all-targets`
**Then** no warnings are produced

**Supports:** NFR-M1 (Code Quality)

---

### Story 1.3: Establish CI Pipeline

As a **maintainer**,
I want **automated CI checks on every pull request**,
So that **code quality is enforced before merge**.

**Acceptance Criteria:**

**Given** a pull request is opened
**When** CI runs
**Then** the following checks execute: `cargo fmt --check`, `cargo clippy -- -D warnings`, `cargo test --workspace`, `cargo doc --no-deps`

**Given** any CI check fails
**When** the PR is reviewed
**Then** it cannot be merged until all checks pass

**Technical Notes:**
- Create `.github/workflows/ci.yml` per Architecture specification
- Target: Tests complete in <10 minutes (NFR-M2)

---

### Story 1.4: Create Crate Dependency Graph

As a **developer**,
I want **crate dependencies correctly configured**,
So that **the build order reflects the architecture's dependency graph**.

**Acceptance Criteria:**

**Given** the crate dependency configuration
**When** I examine Cargo.toml files
**Then** `zos-encoding` has no internal dependencies (leaf crate)
**And** `zos-runtime` depends on `zos-encoding`
**And** `zos-dataset` depends on `zos-encoding`
**And** `zos-cobol` depends on `zos-runtime` and `zos-encoding`
**And** `zos-jcl` depends on `zos-dataset`
**And** `zos-clone` depends on all other crates

---

### Story 1.5: Setup Error Handling Infrastructure

As a **developer**,
I want **a shared error handling pattern across all crates**,
So that **errors are consistent and user-friendly**.

**Acceptance Criteria:**

**Given** each crate defines error types
**When** I examine the error definitions
**Then** they use `thiserror::Error` derive macro
**And** user-facing errors implement `miette::Diagnostic`
**And** errors include source spans where applicable

**Given** an error occurs during compilation
**When** the error is displayed
**Then** it shows the source location, error code, and message

**Technical Notes:**
- Create `error.rs` module skeleton in each crate
- Pattern: `{Crate}{Category}Error` naming convention

---

## Epic 2: Data Encoding Layer

**Goal:** Implement EBCDIC/ASCII conversion and decimal data type handling as the foundation for all data processing.

**Crate:** `zos-encoding`
**FRs:** FR37-FR43
**NFRs:** NFR-I4, NFR-R1

### Story 2.1: Implement EBCDIC Code Page Tables

As a **developer**,
I want **EBCDIC-to-ASCII conversion tables for common IBM code pages**,
So that **I can process mainframe data files correctly**.

**Acceptance Criteria:**

**Given** code page CP037 (US/Canada)
**When** I convert EBCDIC bytes to ASCII
**Then** all 256 byte values map correctly per IBM specification

**Given** code page CP1047 (Latin-1 Open Systems)
**When** I convert EBCDIC bytes to ASCII
**Then** all 256 byte values map correctly per IBM specification

**Given** code page CP500 (International)
**When** I convert EBCDIC bytes to ASCII
**Then** all 256 byte values map correctly per IBM specification

**Supports:** FR37, FR38, FR43, NFR-I4

---

### Story 2.2: Implement Bidirectional EBCDIC/ASCII Conversion

As a **developer**,
I want **functions to convert between EBCDIC and ASCII**,
So that **I can read and write mainframe-compatible data**.

**Acceptance Criteria:**

**Given** an ASCII string "HELLO WORLD"
**When** I convert to EBCDIC using CP037
**Then** I get the correct EBCDIC byte sequence
**And** converting back produces the original ASCII string

**Given** a file containing EBCDIC data
**When** I call `CodePage::decode(&bytes)`
**Then** I receive a valid Rust String in UTF-8

**Given** an invalid byte sequence for a code page
**When** I attempt conversion
**Then** an appropriate error is returned (not a panic)

**Supports:** FR38, NFR-R2

---

### Story 2.3: Implement Packed Decimal (COMP-3) Support

As a **developer**,
I want **packed decimal encoding and decoding**,
So that **I can process COBOL COMP-3 data with correct precision**.

**Acceptance Criteria:**

**Given** a decimal value 12345.67 with PIC S9(5)V99 COMP-3
**When** I encode to packed decimal
**Then** the result is 4 bytes: `0x01 0x23 0x45 0x67 0x0C` (positive sign)

**Given** packed decimal bytes
**When** I decode to rust_decimal::Decimal
**Then** I get the exact decimal value with no precision loss

**Given** a 18-digit decimal (max IBM precision)
**When** I perform encoding/decoding roundtrip
**Then** the value is preserved exactly

**Supports:** FR39, NFR-R1 (18-digit precision)

---

### Story 2.4: Implement Zoned Decimal Support

As a **developer**,
I want **zoned decimal encoding and decoding**,
So that **I can process COBOL display numeric data**.

**Acceptance Criteria:**

**Given** a value 12345 with PIC S9(5) DISPLAY
**When** I encode to zoned decimal
**Then** each digit occupies one byte with zone nibble F (positive) or D (negative) on last byte

**Given** zoned decimal bytes
**When** I decode
**Then** I get the correct numeric value with sign

**Supports:** FR40

---

### Story 2.5: Implement Binary (COMP) Support

As a **developer**,
I want **binary integer encoding with correct byte ordering**,
So that **I can process COBOL COMP and COMP-4 data**.

**Acceptance Criteria:**

**Given** a COMP PIC S9(4) value (halfword)
**When** I encode
**Then** the result is 2 bytes in big-endian order (IBM convention)

**Given** a COMP PIC S9(9) value (fullword)
**When** I encode
**Then** the result is 4 bytes in big-endian order

**Given** a COMP PIC S9(18) value (doubleword)
**When** I encode
**Then** the result is 8 bytes in big-endian order

**Supports:** FR41

---

### Story 2.6: Create High-Level Conversion API

As a **developer**,
I want **a unified API for data type conversion**,
So that **other crates can easily convert data without knowing internal details**.

**Acceptance Criteria:**

**Given** a DataType enum specifying the COBOL picture clause
**When** I call `encode(value, data_type)` or `decode(bytes, data_type)`
**Then** the correct encoding/decoding strategy is applied

**Given** incompatible data for a data type
**When** conversion is attempted
**Then** a descriptive ConversionError is returned

**Technical Notes:**
- Public API: `CodePage`, `PackedDecimal`, `ZonedDecimal`, `BinaryInt`
- All types implement `encode()` and `decode()` methods

**Supports:** FR42, FR43

---

## Epic 3: COBOL Lexer & Source Handling

**Goal:** Implement the lexical analysis phase of the COBOL compiler, handling source format, tokenization, and copybook inclusion.

**Crate:** `zos-cobol` (lexer module)
**FRs:** FR3-FR7, FR10

### Story 3.1: Implement Source File Reader

As a **developer**,
I want **source files read with proper encoding detection**,
So that **both ASCII and EBCDIC source files can be compiled**.

**Acceptance Criteria:**

**Given** an ASCII-encoded COBOL source file
**When** the compiler reads it
**Then** the content is correctly interpreted

**Given** an EBCDIC-encoded COBOL source file with encoding hint
**When** the compiler reads it with `--encoding ebcdic`
**Then** the content is converted to internal UTF-8 representation

**Given** a source file with mixed line endings (CRLF, LF)
**When** the compiler reads it
**Then** line endings are normalized without affecting line numbers

**Supports:** NFR-S3 (Input Validation)

---

### Story 3.2: Implement COBOL Area Detection

As a **developer**,
I want **proper handling of COBOL source format areas**,
So that **column positions are correctly interpreted**.

**Acceptance Criteria:**

**Given** fixed-format COBOL source (columns 1-6: sequence, 7: indicator, 8-72: code)
**When** the lexer processes it
**Then** sequence numbers are ignored, indicator column is processed, code area is tokenized

**Given** a line with '*' in column 7
**When** the lexer processes it
**Then** the line is treated as a comment

**Given** a line with '-' in column 7
**When** the lexer processes it
**Then** the line is treated as a continuation

**Given** free-format source (dialect: cobol2002)
**When** the lexer processes it
**Then** column restrictions are not enforced

**Supports:** FR7, FR10

---

### Story 3.3: Implement Token Scanner

As a **developer**,
I want **COBOL source tokenized into meaningful tokens**,
So that **the parser can process structured input**.

**Acceptance Criteria:**

**Given** COBOL source text
**When** tokenized
**Then** tokens are produced for: keywords, identifiers, literals (numeric, string), operators, punctuation

**Given** a string literal with embedded quotes
**When** tokenized
**Then** the literal value correctly handles quote escaping

**Given** a numeric literal (123.45, -67, 1.2E5)
**When** tokenized
**Then** the correct numeric token type and value is produced

**Given** any token
**When** I inspect it
**Then** it includes a Span with start/end positions and file ID

**Supports:** FR5 (error locations)

---

### Story 3.4: Implement COBOL Keyword Recognition

As a **developer**,
I want **all COBOL-85 and COBOL-2002 keywords recognized**,
So that **the parser can distinguish keywords from identifiers**.

**Acceptance Criteria:**

**Given** a COBOL-85 reserved word (e.g., PERFORM, MOVE, IF)
**When** tokenized
**Then** it produces a keyword token, not an identifier

**Given** an identifier that matches a keyword in a different dialect
**When** tokenized with cobol85 dialect
**Then** dialect-appropriate keyword recognition is applied

**Given** the dialect is "ibm"
**When** IBM-specific keywords are encountered (e.g., EXEC SQL)
**Then** they are recognized as keywords

**Supports:** FR7

---

### Story 3.5: Implement Copybook Resolution

As a **developer**,
I want **COPY statements resolved and included**,
So that **programs using copybooks compile correctly**.

**Acceptance Criteria:**

**Given** a COPY statement `COPY CUSTFILE.`
**When** the lexer processes it
**Then** it searches configured copybook paths for CUSTFILE.cpy (or CUSTFILE.cbl)
**And** includes the content at that position

**Given** nested copybooks (copybook A includes copybook B)
**When** processing
**Then** both levels are resolved correctly
**And** circular inclusion is detected and reported as error

**Given** a copybook with REPLACING clause `COPY CUSTFILE REPLACING ==:PREFIX:== BY ==WS-==.`
**When** processed
**Then** text substitution is applied to the included content

**Given** a copybook cannot be found in any search path
**When** the compiler runs
**Then** an error is reported with the searched paths listed

**Supports:** FR3, FR4

---

### Story 3.6: Implement Lexer Error Handling

As a **developer**,
I want **lexer errors reported with source context**,
So that **I can fix syntax issues quickly**.

**Acceptance Criteria:**

**Given** an invalid character in source
**When** the lexer encounters it
**Then** an error is produced with: line number, column, the invalid character, source context

**Given** an unterminated string literal
**When** the lexer reaches end of line/file
**Then** an error is produced indicating the string start location

**Given** multiple lexer errors in one file
**When** compilation completes
**Then** all errors are reported (not just the first)

**Supports:** FR5, FR65

---

## Epic 4: COBOL Parser & AST

**Goal:** Implement the recursive descent parser that builds a complete Abstract Syntax Tree for COBOL programs.

**Crate:** `zos-cobol` (parser, ast modules)
**FRs:** FR10-FR15

### Story 4.1: Define AST Node Types

As a **developer**,
I want **strongly-typed AST nodes for all COBOL constructs**,
So that **semantic analysis and code generation can work with a well-defined structure**.

**Acceptance Criteria:**

**Given** the AST module
**When** I examine the types
**Then** there are node types for: ProgramUnit, Division, Section, Paragraph, Statement, Expression, DataItem

**Given** any AST node
**When** I inspect it
**Then** it has a `span: Span` field for source location

**Given** the Statement enum
**When** I examine variants
**Then** it includes: Move, If, Perform, Evaluate, Call, Display, Accept, Read, Write, Open, Close, GoTo, Stop, and others

**Technical Notes:**
- All nodes derive `Debug, Clone, PartialEq`
- Use `#[non_exhaustive]` on enums for forward compatibility

---

### Story 4.2: Implement IDENTIFICATION DIVISION Parser

As a **developer**,
I want **IDENTIFICATION DIVISION parsed correctly**,
So that **program metadata is captured**.

**Acceptance Criteria:**

**Given** source with `IDENTIFICATION DIVISION. PROGRAM-ID. MYPROG.`
**When** parsed
**Then** the AST contains program_id = "MYPROG"

**Given** optional paragraphs (AUTHOR, DATE-WRITTEN, etc.)
**When** present
**Then** they are captured in the AST

**Supports:** FR10

---

### Story 4.3: Implement DATA DIVISION Parser

As a **developer**,
I want **DATA DIVISION parsed with all sections and data items**,
So that **variable declarations are available for semantic analysis**.

**Acceptance Criteria:**

**Given** WORKING-STORAGE SECTION with data items
**When** parsed
**Then** each item is captured with: level number, name, picture clause, usage clause, value clause

**Given** nested group items (01 level containing 05 levels)
**When** parsed
**Then** the hierarchy is correctly represented in the AST

**Given** OCCURS clause with INDEXED BY
**When** parsed
**Then** table dimensions and index names are captured

**Given** REDEFINES clause
**When** parsed
**Then** the redefinition relationship is captured

**Supports:** FR11, FR13

---

### Story 4.4: Implement PROCEDURE DIVISION Parser

As a **developer**,
I want **PROCEDURE DIVISION parsed with all statements**,
So that **executable code is represented in the AST**.

**Acceptance Criteria:**

**Given** PROCEDURE DIVISION with sections and paragraphs
**When** parsed
**Then** the structure (sections containing paragraphs) is preserved

**Given** PERFORM statement (all variants)
**When** parsed
**Then** the AST captures: target paragraph/section, TIMES/UNTIL/VARYING clauses, inline vs out-of-line

**Given** IF/ELSE statement with nested conditions
**When** parsed
**Then** the condition tree and statement blocks are correctly structured

**Given** EVALUATE statement (COBOL's case/switch)
**When** parsed
**Then** subjects and WHEN clauses are captured

**Supports:** FR10, FR12

---

### Story 4.5: Implement Arithmetic & String Statement Parsing

As a **developer**,
I want **arithmetic and string manipulation statements parsed**,
So that **all data operations are representable**.

**Acceptance Criteria:**

**Given** MOVE statement with CORRESPONDING
**When** parsed
**Then** source, destinations, and CORRESPONDING flag are captured

**Given** COMPUTE statement with complex expression
**When** parsed
**Then** the expression tree is correctly structured with operator precedence

**Given** STRING and UNSTRING statements
**When** parsed
**Then** all clauses (DELIMITED BY, INTO, POINTER, etc.) are captured

**Given** INSPECT statement (all variants)
**When** parsed
**Then** TALLYING, REPLACING, and CONVERTING clauses are captured

**Supports:** FR15

---

### Story 4.6: Implement CALL Statement Parsing

As a **developer**,
I want **CALL statements parsed with all parameter modes**,
So that **inter-program communication is supported**.

**Acceptance Criteria:**

**Given** `CALL 'SUBPROG' USING BY REFERENCE WS-DATA`
**When** parsed
**Then** program name and parameters with passing mode are captured

**Given** `CALL WS-PROG-NAME` (dynamic call)
**When** parsed
**Then** the identifier reference is captured (not literal)

**Given** CALL with ON EXCEPTION/NOT ON EXCEPTION
**When** parsed
**Then** exception handlers are captured

**Supports:** FR14

---

### Story 4.7: Implement Parser Error Recovery

As a **developer**,
I want **the parser to recover from errors and continue**,
So that **multiple errors can be reported in one compilation**.

**Acceptance Criteria:**

**Given** a syntax error in one statement
**When** parsing continues
**Then** the parser synchronizes at the next statement boundary
**And** subsequent statements are parsed

**Given** multiple syntax errors in a file
**When** compilation completes
**Then** all errors are reported with locations

**Given** an error in DATA DIVISION
**When** PROCEDURE DIVISION parsing begins
**Then** parsing continues (though semantic errors may result from missing declarations)

**Supports:** FR5, FR65, NFR-R2

---

## Epic 5: Semantic Analysis & Validation

**Goal:** Implement symbol resolution, type checking, and diagnostic generation for parsed COBOL programs.

**Crate:** `zos-cobol` (semantic module)
**FRs:** FR5-FR6, FR63, FR65-FR68

### Story 5.1: Build Symbol Table

As a **developer**,
I want **all declared identifiers tracked in a symbol table**,
So that **references can be resolved and type-checked**.

**Acceptance Criteria:**

**Given** DATA DIVISION with variable declarations
**When** semantic analysis runs
**Then** each variable is entered in the symbol table with: name, level, type, size, offset

**Given** qualified names (e.g., `FIELD OF RECORD`)
**When** resolution is performed
**Then** the correct data item is identified

**Given** a duplicate identifier at the same level
**When** detected
**Then** an error is reported

---

### Story 5.2: Implement Type Checking

As a **developer**,
I want **type compatibility checked for all operations**,
So that **type mismatches are caught at compile time**.

**Acceptance Criteria:**

**Given** `MOVE ALPHA-FIELD TO NUMERIC-FIELD`
**When** type checked
**Then** a warning is issued for alphanumeric-to-numeric move

**Given** `COMPUTE ALPHA-FIELD = 5 + 3`
**When** type checked
**Then** an error is issued (COMPUTE requires numeric target)

**Given** valid implicit conversions (numeric display to numeric packed)
**When** type checked
**Then** no error is raised (conversion is valid in COBOL)

---

### Story 5.3: Validate PERFORM Targets

As a **developer**,
I want **PERFORM statement targets validated**,
So that **undefined paragraph/section references are caught**.

**Acceptance Criteria:**

**Given** `PERFORM NONEXISTENT-PARA`
**When** semantic analysis runs
**Then** an error is reported: "Undefined paragraph: NONEXISTENT-PARA"

**Given** `PERFORM PARA-A THRU PARA-B` where PARA-B is before PARA-A
**When** analyzed
**Then** a warning is issued about THRU range order

---

### Story 5.4: Generate Diagnostic Messages

As a **developer**,
I want **diagnostics generated with source context and suggestions**,
So that **errors are easy to understand and fix**.

**Acceptance Criteria:**

**Given** an undefined variable reference
**When** the error is displayed
**Then** it shows: error code, message, source line with caret pointing to the error, suggestion (did you mean X?)

**Given** a warning-level diagnostic
**When** displayed
**Then** it is visually distinct from errors (different color/prefix)

**Given** `--warn strict` mode
**When** warnings are generated
**Then** compilation fails (warnings treated as errors)

**Supports:** FR5, FR6, FR65, FR66, FR67, FR68

---

### Story 5.5: Implement Syntax-Only Validation Mode

As a **developer**,
I want **a mode that validates syntax without full semantic analysis**,
So that **quick feedback is available during editing**.

**Acceptance Criteria:**

**Given** `zos-clone check --syntax-only source.cbl`
**When** executed
**Then** only lexer and parser errors are reported (no semantic analysis)
**And** execution is faster than full compilation

**Given** a syntactically valid but semantically invalid program
**When** checked with `--syntax-only`
**Then** no errors are reported

**Supports:** FR63

---

## Epic 6: COBOL Runtime Library

**Goal:** Implement the runtime library that compiled COBOL programs link against for intrinsic functions, I/O, and system services.

**Crate:** `zos-runtime`
**FRs:** FR44-FR52
**NFRs:** NFR-R1, NFR-P2

### Story 6.1: Implement DISPLAY and ACCEPT Verbs

As a **developer**,
I want **DISPLAY and ACCEPT runtime functions**,
So that **compiled programs can perform console I/O**.

**Acceptance Criteria:**

**Given** `DISPLAY "Hello" WS-NAME`
**When** executed
**Then** the string "Hello" and value of WS-NAME are output to stdout

**Given** `DISPLAY ... UPON SYSOUT`
**When** executed
**Then** output goes to the configured SYSOUT destination

**Given** `ACCEPT WS-INPUT FROM CONSOLE`
**When** executed
**Then** input is read from stdin into WS-INPUT

**Supports:** FR49, FR50

---

### Story 6.2: Implement Decimal Arithmetic Functions

As a **developer**,
I want **decimal arithmetic with IBM-compatible precision**,
So that **financial calculations match mainframe results**.

**Acceptance Criteria:**

**Given** two 18-digit decimals
**When** added, subtracted, multiplied, or divided
**Then** the result matches IBM COBOL exactly (no floating point errors)

**Given** division resulting in non-terminating decimal
**When** truncation is applied
**Then** it follows COBOL ROUNDED/TRUNCATED rules

**Given** overflow during arithmetic
**When** detected
**Then** ON SIZE ERROR handler is triggered (if specified)

**Supports:** FR51, NFR-R1

---

### Story 6.3: Implement String Intrinsic Functions

As a **developer**,
I want **string manipulation runtime functions**,
So that **STRING, UNSTRING, INSPECT execute correctly**.

**Acceptance Criteria:**

**Given** `STRING A DELIMITED SIZE B DELIMITED "," INTO C`
**When** executed
**Then** A and B are concatenated into C with proper delimiter handling

**Given** `UNSTRING INPUT DELIMITED BY "," INTO F1 F2 F3`
**When** executed
**Then** INPUT is split at commas into the target fields

**Given** `INSPECT FIELD TALLYING CNT FOR ALL "A"`
**When** executed
**Then** CNT contains the count of "A" characters in FIELD

---

### Story 6.4: Implement Date/Time Functions

As a **developer**,
I want **date and time intrinsic functions**,
So that **CURRENT-DATE and date arithmetic work correctly**.

**Acceptance Criteria:**

**Given** `MOVE FUNCTION CURRENT-DATE TO WS-DATE`
**When** executed
**Then** WS-DATE contains: YYYYMMDDHHMMSSCC format

**Given** `COMPUTE DAYS = FUNCTION INTEGER-OF-DATE(WS-DATE)`
**When** executed
**Then** the result is the integer day number per IBM specification

**Given** `FUNCTION DATE-OF-INTEGER(DAYS)`
**When** executed
**Then** the result is the date in YYYYMMDD format

---

### Story 6.5: Implement Mathematical Functions

As a **developer**,
I want **mathematical intrinsic functions**,
So that **SQRT, SIN, COS, LOG, etc. work correctly**.

**Acceptance Criteria:**

**Given** `COMPUTE X = FUNCTION SQRT(25)`
**When** executed
**Then** X = 5

**Given** trigonometric functions (SIN, COS, TAN)
**When** called with radian values
**Then** results match IBM COBOL precision

**Given** `FUNCTION RANDOM` calls
**When** executed with same seed
**Then** deterministic sequence is produced

---

### Story 6.6: Implement WORKING-STORAGE Initialization

As a **developer**,
I want **WORKING-STORAGE initialized per COBOL rules**,
So that **programs start with correct initial values**.

**Acceptance Criteria:**

**Given** `01 WS-FIELD PIC X(10) VALUE "INITIAL".`
**When** program starts
**Then** WS-FIELD contains "INITIAL" padded to 10 characters

**Given** numeric field without VALUE clause
**When** program starts
**Then** field contains binary zeros (not spaces)

**Given** group item with VALUE clause
**When** program starts
**Then** subordinate items are initialized as a unit

**Supports:** FR46

---

### Story 6.7: Implement ABEND Handling

As a **developer**,
I want **ABEND conditions detected and reported**,
So that **runtime errors produce diagnostic information**.

**Acceptance Criteria:**

**Given** a data exception (S0C7 equivalent)
**When** detected
**Then** program terminates with diagnostic dump showing: abend code, failing statement, data values

**Given** a subscript out of range
**When** detected
**Then** program terminates with abend code indicating the error type

**Given** ABEND condition with dump
**When** generated
**Then** dump file contains: call stack, data division contents, register equivalents

**Supports:** FR47, FR70

---

### Story 6.8: Implement Condition Code Management

As a **developer**,
I want **condition codes set and propagated correctly**,
So that **JCL step coordination works properly**.

**Acceptance Criteria:**

**Given** `STOP RUN` with no return code
**When** executed
**Then** condition code 0 is returned

**Given** `MOVE 8 TO RETURN-CODE. STOP RUN.`
**When** executed
**Then** condition code 8 is returned to the caller

**Given** program called via CALL
**When** it returns
**Then** the calling program can check RETURN-CODE

**Supports:** FR48

---

## Epic 7: LLVM Code Generation

**Goal:** Implement the code generation phase that transforms the AST into LLVM IR and produces native executables.

**Crate:** `zos-cobol` (codegen module)
**FRs:** FR1-FR2, FR8-FR9, FR44

### Story 7.1: Setup LLVM Module Infrastructure

As a **developer**,
I want **LLVM context and module management**,
So that **code generation has a target to emit to**.

**Acceptance Criteria:**

**Given** compilation begins
**When** code generation initializes
**Then** an LLVM Context and Module are created

**Given** target triple (x86_64-unknown-linux-gnu)
**When** code generation configures
**Then** LLVM is configured for the correct target architecture

**Given** compilation completes
**When** module is finalized
**Then** LLVM verifier passes with no errors

**Technical Notes:**
- Use inkwell crate for safe LLVM bindings
- Configure for both x86_64 and ARM64 targets

---

### Story 7.2: Generate Data Division Layout

As a **developer**,
I want **WORKING-STORAGE represented in LLVM IR**,
So that **runtime can allocate and access variables**.

**Acceptance Criteria:**

**Given** a data item `01 WS-FIELD PIC X(10).`
**When** codegen runs
**Then** LLVM IR contains a global or stack allocation of 10 bytes

**Given** a group item with subordinate items
**When** codegen runs
**Then** a struct type is generated with correct field offsets

**Given** OCCURS clause `OCCURS 10 TIMES`
**When** codegen runs
**Then** an array type is generated with correct bounds

---

### Story 7.3: Generate Statement Code

As a **developer**,
I want **PROCEDURE DIVISION statements compiled to LLVM IR**,
So that **the program executes**.

**Acceptance Criteria:**

**Given** `MOVE A TO B`
**When** codegen runs
**Then** LLVM IR contains: load from A location, store to B location, with any necessary conversion

**Given** `IF condition ... ELSE ... END-IF`
**When** codegen runs
**Then** LLVM IR contains: conditional branch, then-block, else-block, merge block

**Given** `PERFORM PARA-A`
**When** codegen runs
**Then** LLVM IR contains: call to generated function for PARA-A

---

### Story 7.4: Generate CALL Linkage

As a **developer**,
I want **CALL statements compiled with correct ABI**,
So that **inter-program calls work correctly**.

**Acceptance Criteria:**

**Given** `CALL 'SUBPROG' USING BY REFERENCE WS-DATA`
**When** codegen runs
**Then** LLVM IR contains: function call to subprog with pointer to WS-DATA

**Given** static CALL (literal program name)
**When** linked
**Then** external symbol is resolved at link time

**Given** dynamic CALL (identifier program name)
**When** codegen runs
**Then** LLVM IR contains: dlopen/dlsym equivalent for runtime loading

**Supports:** FR14

---

### Story 7.5: Implement Optimization Levels

As a **developer**,
I want **configurable optimization levels**,
So that **I can trade compile time for execution speed**.

**Acceptance Criteria:**

**Given** `--optimize 0` (none)
**When** codegen runs
**Then** LLVM optimization passes are skipped for fast compilation

**Given** `--optimize 2` (standard)
**When** codegen runs
**Then** LLVM runs standard optimization passes

**Given** `--optimize 3` (aggressive)
**When** codegen runs
**Then** LLVM runs aggressive optimizations including inlining

**Supports:** FR8

---

### Story 7.6: Generate Debug Information

As a **developer**,
I want **debug symbols in compiled output**,
So that **I can use standard debuggers on compiled programs**.

**Acceptance Criteria:**

**Given** `--debug` flag
**When** codegen runs
**Then** DWARF debug information is emitted

**Given** a compiled program with debug info
**When** I run in gdb/lldb
**Then** I can set breakpoints by COBOL source line

**Given** debug mode
**When** inspecting variables in debugger
**Then** COBOL variable names are visible (not just memory addresses)

**Supports:** FR9

---

## Epic 8: Dataset & File Operations

**Goal:** Implement the file I/O layer that maps COBOL file operations to Linux filesystem operations.

**Crate:** `zos-dataset`
**FRs:** FR28-FR36
**NFRs:** NFR-R3

### Story 8.1: Implement Dataset Catalog

As a **developer**,
I want **z/OS dataset names resolved to Linux paths**,
So that **programs can access files using mainframe naming**.

**Acceptance Criteria:**

**Given** configuration `datasets.base_path: /data/datasets`
**When** program references `PROD.CUSTOMER.FILE`
**Then** it maps to `/data/datasets/PROD/CUSTOMER/FILE` (or configured mapping)

**Given** wildcard mapping `TEST.*.FILE -> test/{1}.dat`
**When** program references `TEST.APRIL.FILE`
**Then** it maps to `test/APRIL.dat`

**Given** dataset not found in catalog
**When** accessed
**Then** error indicates the dataset name and searched paths

**Supports:** FR33, FR34

---

### Story 8.2: Implement Sequential File Read (QSAM GET)

As a **developer**,
I want **COBOL READ statement for sequential files**,
So that **programs can read input data**.

**Acceptance Criteria:**

**Given** `OPEN INPUT file-name`
**When** executed
**Then** the file is opened for reading

**Given** `READ file-name INTO ws-record`
**When** executed
**Then** one record is read into ws-record

**Given** `READ file-name AT END ...`
**When** end of file reached
**Then** AT END imperative is executed

**Given** file status checking
**When** I/O operation completes
**Then** FILE STATUS variable is updated with correct 2-digit code

**Supports:** FR28, FR36

---

### Story 8.3: Implement Sequential File Write (QSAM PUT)

As a **developer**,
I want **COBOL WRITE statement for sequential files**,
So that **programs can produce output**.

**Acceptance Criteria:**

**Given** `OPEN OUTPUT file-name`
**When** executed
**Then** the file is created/truncated for writing

**Given** `WRITE output-record`
**When** executed
**Then** the record is written to the file

**Given** `OPEN EXTEND file-name`
**When** executed
**Then** the file is opened for append

**Supports:** FR29

---

### Story 8.4: Implement Fixed-Length Records

As a **developer**,
I want **fixed-length record format (RECFM=F/FB)**,
So that **programs can process fixed-format files**.

**Acceptance Criteria:**

**Given** LRECL=80, RECFM=F
**When** reading
**Then** exactly 80 bytes are read per record

**Given** LRECL=80, RECFM=FB, BLKSIZE=800
**When** reading
**Then** records are unblocked (10 records per physical block)

**Given** record shorter than LRECL
**When** writing
**Then** record is padded to LRECL

**Supports:** FR30, FR32

---

### Story 8.5: Implement Variable-Length Records

As a **developer**,
I want **variable-length record format (RECFM=V/VB)**,
So that **programs can process variable-format files**.

**Acceptance Criteria:**

**Given** RECFM=V
**When** reading
**Then** 4-byte RDW (Record Descriptor Word) is read first, then data

**Given** RECFM=VB (blocked)
**When** reading
**Then** BDW (Block Descriptor Word) and RDW are processed

**Given** record longer than LRECL
**When** writing
**Then** error is raised (record too long)

**Supports:** FR31, FR32

---

### Story 8.6: Implement Dataset Allocation

As a **developer**,
I want **dataset allocation with SPACE and DCB**,
So that **JCL DISP=NEW works correctly**.

**Acceptance Criteria:**

**Given** `DD DSN=NEW.FILE,DISP=(NEW,CATLG),SPACE=(TRK,10)`
**When** job runs
**Then** new file is created (space parameter logged but not enforced on Linux)

**Given** `DISP=(OLD,KEEP)`
**When** job runs
**Then** existing file is opened, preserved after job

**Given** `DISP=(NEW,DELETE)`
**When** job completes
**Then** file is deleted

**Supports:** FR24, FR35

---

### Story 8.7: Implement File I/O Error Handling

As a **developer**,
I want **file errors reported consistently**,
So that **programs can handle I/O failures**.

**Acceptance Criteria:**

**Given** file not found on OPEN
**When** executed
**Then** FILE STATUS = "35" (file not found)

**Given** permission denied
**When** OPEN executed
**Then** FILE STATUS = "37" (permission denied)

**Given** I/O error during READ/WRITE
**When** detected
**Then** FILE STATUS reflects the error type

**Supports:** FR36, NFR-R3

---

## Epic 9: JCL Interpreter

**Goal:** Implement the JCL parser and job executor that orchestrates batch job execution.

**Crate:** `zos-jcl`
**FRs:** FR16-FR27, FR64

### Story 9.1: Implement JCL Lexer & Parser

As an **operations engineer**,
I want **JCL source parsed correctly**,
So that **job streams execute as expected**.

**Acceptance Criteria:**

**Given** JCL with `//JOBNAME JOB (ACCT),'DESCRIPTION'`
**When** parsed
**Then** job name, accounting, and description are captured

**Given** `//STEP1 EXEC PGM=MYPROG`
**When** parsed
**Then** step name and program name are captured

**Given** `//DD1 DD DSN=MY.FILE,DISP=SHR`
**When** parsed
**Then** DD name, dataset, and disposition are captured

**Supports:** FR16, FR64

---

### Story 9.2: Implement EXEC Statement Processing

As an **operations engineer**,
I want **EXEC statements executed correctly**,
So that **programs run in the specified order**.

**Acceptance Criteria:**

**Given** `//STEP1 EXEC PGM=COBOLPGM`
**When** step runs
**Then** COBOLPGM executable is located and run

**Given** `//STEP1 EXEC PGM=COBOLPGM,PARM='PARAM1'`
**When** step runs
**Then** PARM value is passed to the program

**Given** `//STEP1 EXEC PROC=MYPROC`
**When** step runs
**Then** procedure MYPROC is expanded and executed

**Supports:** FR16

---

### Story 9.3: Implement DD Statement Processing

As an **operations engineer**,
I want **DD statements allocate datasets correctly**,
So that **file I/O in programs works**.

**Acceptance Criteria:**

**Given** `//INFILE DD DSN=INPUT.DATA,DISP=SHR`
**When** step runs
**Then** INFILE is associated with INPUT.DATA for the program

**Given** `//SYSOUT DD SYSOUT=*`
**When** step runs
**Then** SYSOUT output goes to job output

**Given** `//SYSIN DD *` with inline data
**When** step runs
**Then** inline data is available to SYSIN

**Supports:** FR21, FR22, FR23, FR24

---

### Story 9.4: Implement Condition Code Handling

As an **operations engineer**,
I want **condition codes control job flow**,
So that **jobs execute conditionally based on prior step results**.

**Acceptance Criteria:**

**Given** `//STEP2 EXEC PGM=PROG,COND=(4,LT)`
**When** prior step returned CC >= 4
**Then** STEP2 is skipped

**Given** `//STEP2 EXEC PGM=PROG,COND=(0,NE)`
**When** any prior step returned non-zero
**Then** STEP2 is skipped

**Given** IF/THEN/ELSE/ENDIF JCL constructs
**When** condition evaluates
**Then** appropriate branch is executed

**Supports:** FR17, FR25

---

### Story 9.5: Implement Symbolic Parameter Substitution

As an **operations engineer**,
I want **symbolic parameters resolved before execution**,
So that **jobs can be parameterized**.

**Acceptance Criteria:**

**Given** `SET DATE='20260212'` and `DSN=PROD.&DATE..FILE`
**When** parsed
**Then** DSN resolves to `PROD.20260212.FILE`

**Given** PROC with parameters `MYPROC PROC LIB=TEST`
**When** called with override `EXEC MYPROC,LIB=PROD`
**Then** LIB=PROD is used

**Given** undefined symbolic
**When** referenced
**Then** error is reported with symbolic name

**Supports:** FR20

---

### Story 9.6: Implement PROC Expansion

As an **operations engineer**,
I want **cataloged and in-stream procedures expanded**,
So that **reusable job patterns work**.

**Acceptance Criteria:**

**Given** cataloged PROC in configured library
**When** `EXEC PROC=MYPROC` executes
**Then** PROC content is expanded inline

**Given** in-stream PROC (between PROC and PEND)
**When** `EXEC PROC=INPROC` executes
**Then** in-stream content is expanded

**Given** PROC override `//STEP1.DD1 DD DSN=OVERRIDE.FILE`
**When** expanded
**Then** DD1 in STEP1 uses the override definition

**Supports:** FR18, FR19

---

### Story 9.7: Implement Job Completion Reporting

As an **operations engineer**,
I want **job status reported clearly**,
So that **I know if the job succeeded**.

**Acceptance Criteria:**

**Given** job completes normally
**When** I check output
**Then** I see: job name, each step name with condition code, overall max CC

**Given** job abends
**When** I check output
**Then** I see: abend code, failing step, diagnostic information

**Given** job output logging
**When** job runs
**Then** timestamps are recorded for each step

**Supports:** FR26, FR69

---

### Story 9.8: Implement Dry-Run Mode

As an **operations engineer**,
I want **JCL validated without execution**,
So that **I can check for errors before running**.

**Acceptance Criteria:**

**Given** `zos-clone run --dry-run job.jcl`
**When** executed
**Then** JCL is parsed and validated
**And** datasets are checked for existence
**And** no programs are executed

**Given** JCL with syntax error
**When** dry-run executes
**Then** error is reported with location

**Supports:** FR27, FR64

---

## Epic 10: CLI & Developer Tools

**Goal:** Implement the command-line interface that provides the primary user interaction with zOS-clone.

**Crate:** `zos-clone`
**FRs:** FR54-FR62, FR67-FR70
**NFRs:** NFR-U1

### Story 10.1: Implement CLI Framework

As a **developer**,
I want **a well-structured CLI with subcommands**,
So that **I can easily invoke different functions**.

**Acceptance Criteria:**

**Given** `zos-clone --help`
**When** executed
**Then** all subcommands are listed with descriptions

**Given** `zos-clone compile --help`
**When** executed
**Then** all compile options are listed with descriptions

**Given** invalid command
**When** executed
**Then** error message suggests valid commands

**Technical Notes:**
- Use clap 4.x with derive macros per Architecture

**Supports:** FR59, NFR-U1

---

### Story 10.2: Implement Compile Command

As a **developer**,
I want **`zos-clone compile` to build executables**,
So that **I can compile COBOL programs**.

**Acceptance Criteria:**

**Given** `zos-clone compile source.cbl`
**When** executed
**Then** native executable is produced (default: `source`)

**Given** `zos-clone compile source.cbl -o output`
**When** executed
**Then** executable is named `output`

**Given** `zos-clone compile -c /copybooks source.cbl`
**When** executed
**Then** copybooks are searched in `/copybooks`

**Given** compilation error
**When** command exits
**Then** exit code is 8 (error)

**Supports:** FR54, FR60, FR61

---

### Story 10.3: Implement Run Command

As a **developer**,
I want **`zos-clone run` to execute JCL**,
So that **I can run batch jobs**.

**Acceptance Criteria:**

**Given** `zos-clone run job.jcl`
**When** executed
**Then** JCL job runs and output is displayed

**Given** `zos-clone run job.jcl -D DATE=20260212`
**When** executed
**Then** symbolic DATE is set before execution

**Given** `zos-clone run job.jcl --maxcc 4`
**When** any step exceeds CC 4
**Then** job terminates and reports failure

**Supports:** FR55, FR53

---

### Story 10.4: Implement Check Command

As a **developer**,
I want **`zos-clone check` to validate source**,
So that **I can find errors without compiling**.

**Acceptance Criteria:**

**Given** `zos-clone check source.cbl`
**When** executed
**Then** syntax and semantic errors are reported

**Given** `zos-clone check job.jcl`
**When** executed
**Then** JCL syntax errors are reported

**Given** `zos-clone check source.cbl --format json`
**When** executed
**Then** errors are output as JSON array

**Supports:** FR56, FR62, FR63, FR64

---

### Story 10.5: Implement Convert Command

As a **developer**,
I want **`zos-clone convert` to transform data files**,
So that **I can prepare data for processing**.

**Acceptance Criteria:**

**Given** `zos-clone convert input.dat output.dat --from ebcdic --to ascii`
**When** executed
**Then** file is converted from EBCDIC to ASCII

**Given** `zos-clone convert input.dat output.dat --recfm FB --lrecl 80`
**When** executed
**Then** record structure is preserved during conversion

**Supports:** FR57, FR42

---

### Story 10.6: Implement Output Formatting

As a **developer**,
I want **output in multiple formats**,
So that **I can integrate with different tools**.

**Acceptance Criteria:**

**Given** `--format text` (default)
**When** errors occur
**Then** human-readable output with source context is displayed

**Given** `--format json`
**When** errors occur
**Then** JSON array of diagnostic objects is output

**Given** `--format sarif`
**When** errors occur
**Then** SARIF format (for GitHub code scanning) is output

**Supports:** FR62, NFR-I1

---

### Story 10.7: Implement Exit Codes

As a **developer**,
I want **standard exit codes from all commands**,
So that **CI/CD pipelines can detect success/failure**.

**Acceptance Criteria:**

**Given** successful operation
**When** command exits
**Then** exit code is 0

**Given** warnings but no errors
**When** command exits
**Then** exit code is 4

**Given** compilation/execution errors
**When** command exits
**Then** exit code is 8

**Given** internal error
**When** command exits
**Then** exit code is 12 or 16

**Supports:** FR61, NFR-I1

---

## Epic 11: Configuration System

**Goal:** Implement the configuration management system for customizing zOS-clone behavior.

**Crate:** `zos-clone` (config module)
**FRs:** FR71-FR76
**NFRs:** NFR-PO3

### Story 11.1: Implement Configuration File Loading

As a **developer**,
I want **configuration loaded from files**,
So that **settings persist across invocations**.

**Acceptance Criteria:**

**Given** `~/.zos-clone/config.yaml` exists
**When** any command runs
**Then** configuration is loaded from that file

**Given** `./zos-clone.yaml` in current directory
**When** any command runs
**Then** local config overrides global config

**Given** invalid YAML in config file
**When** loading
**Then** error is reported with file path and line number

**Supports:** FR71, FR72, FR73

---

### Story 11.2: Implement Environment Variable Configuration

As a **developer**,
I want **configuration via environment variables**,
So that **I can configure in CI/CD environments**.

**Acceptance Criteria:**

**Given** `ZOS_CLONE_COPYBOOK_PATH=/path/to/copybooks`
**When** compile runs
**Then** copybook path is used

**Given** `ZOS_CLONE_DIALECT=ibm`
**When** compile runs
**Then** IBM dialect is used by default

**Given** both env var and config file
**When** loading
**Then** env var takes precedence

**Supports:** FR75

---

### Story 11.3: Implement Command-Line Overrides

As a **developer**,
I want **CLI flags to override all configuration**,
So that **I can customize behavior per invocation**.

**Acceptance Criteria:**

**Given** config file sets dialect=cobol85
**When** `--dialect ibm` is specified on command line
**Then** IBM dialect is used for this invocation only

**Given** multiple copybook paths from config
**When** `-c /additional/path` is specified
**Then** additional path is prepended to search list

**Supports:** FR74

---

### Story 11.4: Implement Dataset Configuration

As an **operations engineer**,
I want **dataset paths and SYSOUT configured**,
So that **jobs find their data files**.

**Acceptance Criteria:**

**Given** config with `datasets.base_path: /data`
**When** job references DSN
**Then** path resolution uses /data as base

**Given** config with `sysout_path: /var/log/zos-clone`
**When** job produces SYSOUT
**Then** output files are written to that directory

**Given** `datasets.mappings` with explicit DSN-to-path rules
**When** DSN matches a rule
**Then** explicit mapping takes precedence over base_path

**Supports:** FR34, FR72, FR76

---

## Epic 12: Testing & NIST Compliance

**Goal:** Implement the testing infrastructure to validate COBOL compatibility and maintain quality.

**Location:** `tests/` directory
**FRs:** FR93-FR96
**NFRs:** NFR-R1, NFR-M2

### Story 12.1: Integrate NIST COBOL-85 Test Suite

As a **maintainer**,
I want **NIST test suite integrated with cargo test**,
So that **compliance is continuously validated**.

**Acceptance Criteria:**

**Given** NIST test programs in `tests/nist/cobol85/`
**When** `cargo test nist` runs
**Then** each test program is compiled and executed

**Given** test program with expected output file
**When** test runs
**Then** actual output is compared to expected output

**Given** test failure
**When** results are reported
**Then** the specific test name and difference are shown

**Supports:** FR93, NFR-R1

---

### Story 12.2: Implement Output Comparison Framework

As a **developer**,
I want **automated output comparison**,
So that **bit-identical output can be verified**.

**Acceptance Criteria:**

**Given** reference output from IBM compiler
**When** zOS-clone produces output
**Then** byte-by-byte comparison is performed

**Given** known acceptable differences (timestamps, paths)
**When** comparison runs
**Then** ignore patterns are applied

**Given** comparison failure
**When** reported
**Then** diff shows first divergence point

**Supports:** FR94

---

### Story 12.3: Create Regression Test Suite

As a **developer**,
I want **regression tests for bug fixes**,
So that **fixed bugs don't recur**.

**Acceptance Criteria:**

**Given** a bug fix
**When** PR is submitted
**Then** a regression test is required

**Given** `tests/regression/issues/` directory
**When** tests run
**Then** all issue reproduction tests pass

**Technical Notes:**
- Each regression test should reference the issue number

---

### Story 12.4: Implement Benchmark Suite

As a **maintainer**,
I want **performance benchmarks tracked over time**,
So that **performance regressions are detected**.

**Acceptance Criteria:**

**Given** benchmark programs in `tests/benchmarks/`
**When** benchmarks run
**Then** compilation time and execution time are measured

**Given** CI pipeline
**When** benchmarks complete
**Then** results are compared to baseline

**Technical Notes:**
- Use criterion or similar for statistically valid benchmarks

**Supports:** NFR-P1, NFR-P2

---

### Story 12.5: Implement Fuzzing Infrastructure

As a **maintainer**,
I want **fuzz testing for parser and codegen**,
So that **crashes and security issues are found**.

**Acceptance Criteria:**

**Given** fuzz targets in `fuzz/`
**When** `cargo +nightly fuzz run parser` runs
**Then** random input is generated and tested

**Given** crash found by fuzzer
**When** minimized
**Then** reproduction case is saved for regression testing

**Supports:** NFR-S2, NFR-M1

---

## Epic 13: Distribution & Packaging

**Goal:** Implement build, packaging, and release automation for multiple distribution channels.

**FRs:** FR77-FR82
**NFRs:** NFR-S1, NFR-I3

### Story 13.1: Create Docker Build

As a **developer**,
I want **Docker images available**,
So that **I can use zOS-clone without installing dependencies**.

**Acceptance Criteria:**

**Given** `docker pull zos-clone/zos-clone:latest`
**When** image is pulled
**Then** image contains working zOS-clone binary

**Given** `docker run -v $(pwd):/workspace zos-clone compile test.cbl`
**When** executed
**Then** compilation succeeds with output in current directory

**Given** multi-stage Dockerfile
**When** built
**Then** runtime image is <100MB (Debian slim base)

**Supports:** FR77, NFR-I3

---

### Story 13.2: Create Binary Releases

As a **developer**,
I want **pre-built binaries for download**,
So that **I can install without building from source**.

**Acceptance Criteria:**

**Given** GitHub release
**When** I download `zos-clone-linux-x64.tar.gz`
**Then** I get a working static binary

**Given** release artifacts
**When** I check signatures
**Then** GPG signature verifies against published key

**Given** release artifacts
**When** I check checksums
**Then** SHA-256 matches published value

**Supports:** FR78, FR81, NFR-S1

---

### Story 13.3: Configure Cargo Publishing

As a **developer**,
I want **zOS-clone installable via cargo**,
So that **Rust developers can install easily**.

**Acceptance Criteria:**

**Given** `cargo install zos-clone`
**When** executed
**Then** latest version is built and installed

**Given** crates.io listing
**When** viewed
**Then** documentation and metadata are complete

**Supports:** FR79

---

### Story 13.4: Create Multi-Architecture Builds

As a **developer**,
I want **ARM64 binaries available**,
So that **I can run on ARM-based servers (Graviton, M1)**.

**Acceptance Criteria:**

**Given** GitHub release
**When** I download `zos-clone-linux-arm64.tar.gz`
**Then** I get a working ARM64 binary

**Given** Docker image
**When** pulled on ARM64 host
**Then** native ARM64 image is used (multi-arch manifest)

**Supports:** FR82, NFR-I3

---

### Story 13.5: Implement Release Automation

As a **maintainer**,
I want **releases automated via CI/CD**,
So that **releases are consistent and reproducible**.

**Acceptance Criteria:**

**Given** tag `v1.0.0` pushed
**When** CI runs
**Then** binaries are built for all targets, signed, and uploaded to GitHub release

**Given** release workflow
**When** completed
**Then** Docker images are pushed to Docker Hub and GHCR

**Given** release
**When** published
**Then** CHANGELOG.md entry is included in release notes

**Supports:** NFR-M3

---

## Epic 14: Documentation

**Goal:** Create comprehensive documentation for users, operators, and contributors.

**Location:** `docs/` directory
**FRs:** FR87-FR92
**NFRs:** NFR-U2

### Story 14.1: Write Quick Start Guide

As a **developer**,
I want **a quick start guide**,
So that **I can compile my first program in under 5 minutes**.

**Acceptance Criteria:**

**Given** the quick start guide
**When** I follow it
**Then** I can: install zOS-clone, compile hello-world.cbl, run the result

**Given** example code in the guide
**When** copied
**Then** it works without modification

**Supports:** FR87, NFR-U2

---

### Story 14.2: Write CLI Reference

As a **developer**,
I want **complete CLI documentation**,
So that **I can find any option I need**.

**Acceptance Criteria:**

**Given** CLI reference
**When** I search for a command
**Then** I find: description, all options, examples

**Given** each command
**When** documented
**Then** exit codes and error conditions are listed

**Supports:** FR88

---

### Story 14.3: Write Migration Guide

As an **operations engineer**,
I want **a migration guide**,
So that **I can move workloads from mainframe to zOS-clone**.

**Acceptance Criteria:**

**Given** migration guide
**When** I follow it
**Then** I can: assess compatibility, migrate code, convert data, run parallel tests, cut over

**Given** common migration issues
**When** addressed
**Then** workarounds or solutions are provided

**Supports:** FR89

---

### Story 14.4: Create Compatibility Matrix

As an **evaluator**,
I want **a compatibility matrix**,
So that **I can assess zOS-clone against my requirements**.

**Acceptance Criteria:**

**Given** compatibility matrix
**When** I check COBOL features
**Then** I see: feature name, support status (full/partial/none), notes

**Given** compatibility matrix
**When** I check JCL features
**Then** I see: statement/parameter, support status, notes

**Supports:** FR90

---

### Story 14.5: Write Contributing Guide

As a **contributor**,
I want **a contributing guide**,
So that **I can submit quality contributions**.

**Acceptance Criteria:**

**Given** contributing guide
**When** I read it
**Then** I understand: code style, PR process, test requirements, review expectations

**Given** a new contributor
**When** they follow the guide
**Then** they can set up dev environment and run tests

**Supports:** FR91, NFR-M2

