# zOS-clone Bootstrap Prompt

## Objective

Initialize the Cargo workspace and project structure for zOS-clone. This is a ONE-TIME bootstrap - once complete, switch to `PROMPT.md` for iterative development.

## Task: Epic 1 - Project Foundation

Create the complete project structure per Architecture specification.

### Story 1.1: Initialize Cargo Workspace

Create workspace with 6 crates:

```
zos-clone/
├── Cargo.toml                    # Workspace root
├── crates/
│   ├── zos-clone/                # CLI binary
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   ├── zos-cobol/                # COBOL compiler
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   ├── zos-jcl/                  # JCL interpreter
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   ├── zos-runtime/              # Runtime library
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   ├── zos-dataset/              # File I/O
│   │   ├── Cargo.toml
│   │   └── src/lib.rs
│   └── zos-encoding/             # EBCDIC/decimal
│       ├── Cargo.toml
│       └── src/lib.rs
├── tests/                        # Integration tests
├── examples/
│   └── hello-world/
│       ├── HELLO.cbl
│       └── HELLO.jcl
├── rust-toolchain.toml
├── rustfmt.toml
├── clippy.toml
└── .github/workflows/ci.yml
```

### Workspace Cargo.toml

```toml
[workspace]
resolver = "2"
members = [
    "crates/zos-clone",
    "crates/zos-cobol",
    "crates/zos-jcl",
    "crates/zos-runtime",
    "crates/zos-dataset",
    "crates/zos-encoding",
]

[workspace.package]
version = "0.1.0"
edition = "2021"
rust-version = "1.75"
license = "Apache-2.0"
repository = "https://github.com/zos-clone/zos-clone"

[workspace.dependencies]
# CLI
clap = { version = "4", features = ["derive", "env"] }
clap_complete = "4"

# Error handling
miette = { version = "7", features = ["fancy"] }
thiserror = "1"

# Serialization
serde = { version = "1", features = ["derive"] }
toml = "0.8"

# Logging
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter"] }

# Decimal arithmetic
rust_decimal = { version = "1", features = ["maths"] }

# Arena allocation
bumpalo = "3"

# Internal crates
zos-encoding = { path = "crates/zos-encoding" }
zos-runtime = { path = "crates/zos-runtime" }
zos-dataset = { path = "crates/zos-dataset" }
zos-cobol = { path = "crates/zos-cobol" }
zos-jcl = { path = "crates/zos-jcl" }
```

### Crate Dependencies (per Architecture)

```
zos-encoding: (none - leaf crate)
zos-runtime:  zos-encoding
zos-dataset:  zos-encoding
zos-cobol:    zos-encoding, zos-runtime
zos-jcl:      zos-dataset
zos-clone:    all crates + clap + miette + tracing
```

### Story 1.2: Toolchain Configuration

**rust-toolchain.toml:**
```toml
[toolchain]
channel = "1.75"
components = ["rustfmt", "clippy"]
```

**rustfmt.toml:**
```toml
edition = "2021"
max_width = 100
```

**clippy.toml:**
```toml
msrv = "1.75"
```

### Story 1.3: CI Pipeline

**.github/workflows/ci.yml:**
```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  CARGO_TERM_COLOR: always
  RUSTFLAGS: -D warnings

jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: rustfmt, clippy

      - name: Format check
        run: cargo fmt --all -- --check

      - name: Clippy
        run: cargo clippy --all-targets -- -D warnings

      - name: Build
        run: cargo build --workspace

      - name: Test
        run: cargo test --workspace

      - name: Doc
        run: cargo doc --no-deps
        env:
          RUSTDOCFLAGS: "-D warnings"
```

### Story 1.5: Error Handling Infrastructure

Each crate's `lib.rs` should include error module skeleton:

```rust
// Example for zos-encoding/src/lib.rs
pub mod error;

// zos-encoding/src/error.rs
use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
pub enum EncodingError {
    #[error("Invalid code page: {0}")]
    #[diagnostic(code(encoding::invalid_code_page))]
    InvalidCodePage(String),

    #[error("Conversion failed: {message}")]
    #[diagnostic(code(encoding::conversion_failed))]
    ConversionFailed { message: String },
}
```

### Hello World Example

**examples/hello-world/HELLO.cbl:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO, WORLD!".
           STOP RUN.
```

**examples/hello-world/HELLO.jcl:**
```jcl
//HELLO    JOB (ACCT),'HELLO WORLD'
//STEP1    EXEC PGM=HELLO
//SYSOUT   DD SYSOUT=*
```

## Acceptance Criteria

When complete:
- [ ] `cargo build --workspace` succeeds
- [ ] `cargo test --workspace` succeeds
- [ ] `cargo fmt --all -- --check` passes
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] All 6 crates exist with proper Cargo.toml
- [ ] Dependency graph matches architecture
- [ ] CI workflow file exists
- [ ] Example files exist

## Completion Signal

When all criteria are met:
```
<promise>BOOTSTRAP COMPLETE</promise>
```

Then switch to `PROMPT.md` for continued development.
