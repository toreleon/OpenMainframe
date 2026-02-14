# OpenMainframe

An open-source mainframe COBOL compiler and JCL interpreter written in Rust.

## Features

- **COBOL Compiler**: Parse and compile COBOL programs
  - Lexer and parser for IBM-style COBOL
  - Semantic analysis with symbol table
  - Tree-walking interpreter for execution without LLVM
  - Optional LLVM backend for native code generation

- **JCL Interpreter**: Execute Job Control Language scripts
  - JCL parsing and job execution
  - DD statement handling for dataset allocation
  - EXEC statement for running programs

- **Dataset I/O**: QSAM (Queued Sequential Access Method) support
  - Fixed-length and variable-length records
  - Sequential read/write operations
  - Dataset catalog for DSN resolution

- **EBCDIC Support**: Full EBCDIC-ASCII translation
  - Character encoding conversion
  - Packed decimal (COMP-3) support

## Installation

### From Source

Requires Rust 1.82 or later.

```bash
git clone https://github.com/toreleon/OpenMainframe.git
cd open-mainframe
cargo build --release
```

The binary will be at `target/release/open-mainframe`.

### With LLVM Support

To enable native code generation, install LLVM 18 and build with:

```bash
cargo build --release --features llvm
```

## Usage

### Interpret a COBOL Program

```bash
open-mainframe interpret program.cbl
```

### Check COBOL Syntax

```bash
open-mainframe check program.cbl
```

### Compile a COBOL Program (requires LLVM feature)

```bash
open-mainframe compile program.cbl -o program
```

### Run a JCL Job

```bash
open-mainframe run job.jcl
```

### Parse JCL (show structure)

```bash
open-mainframe parse-jcl job.jcl
```

### Show Tokens (debugging)

```bash
open-mainframe lex program.cbl
```

### Configuration

```bash
# Show current configuration
open-mainframe config show

# Generate default config file
open-mainframe config init

# Show config paths and environment variables
open-mainframe config paths
```

### Shell Completions

```bash
# Bash
open-mainframe completions bash > ~/.local/share/bash-completion/completions/open-mainframe

# Zsh
open-mainframe completions zsh > ~/.zfunc/_open-mainframe

# Fish
open-mainframe completions fish > ~/.config/fish/completions/open-mainframe.fish
```

## Configuration

OpenMainframe supports configuration through:

1. **Project config**: `./open-mainframe.toml`
2. **User config**: `~/.config/open-mainframe/config.toml`
3. **Environment variables**: `OPEN_MAINFRAME_*`

Example configuration:

```toml
[compiler]
source_format = "fixed"  # fixed, free, or auto
copybook_paths = [".", "./copybooks", "./copy"]
optimization = 0
dialect = "ibm"

[runtime]
decimal_precision = 18
bounds_checking = true

[dataset]
base_dir = "./datasets"
default_recfm = "FB"
default_lrecl = 80

[jcl]
program_dir = "./bin"
work_dir = "./work"
```

## Example COBOL Program

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MESSAGE PIC X(20) VALUE "Hello, World!".
       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

Run with:
```bash
open-mainframe interpret hello.cbl
```

## Project Structure

```
crates/
  open-mainframe/     # CLI binary
  open-mainframe-cobol/     # COBOL lexer, parser, semantic analysis, codegen
  open-mainframe-jcl/       # JCL lexer, parser, executor
  open-mainframe-runtime/   # Runtime library and interpreter
  open-mainframe-dataset/   # Dataset I/O (QSAM, catalog)
  open-mainframe-encoding/  # EBCDIC encoding support
```

## Supported COBOL Features

### Data Types
- Alphanumeric (PIC X)
- Numeric (PIC 9)
- Packed decimal (COMP-3)
- Binary (COMP)

### Statements
- MOVE, COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE
- DISPLAY, ACCEPT
- IF/ELSE, EVALUATE
- PERFORM (inline, paragraph, TIMES, UNTIL)
- STRING, UNSTRING (limited)
- GO TO, STOP RUN

### Divisions
- IDENTIFICATION DIVISION
- ENVIRONMENT DIVISION
- DATA DIVISION (WORKING-STORAGE, FILE SECTION)
- PROCEDURE DIVISION

## License

Apache-2.0

## Contributing

Contributions welcome! Please ensure:
- Code passes `cargo fmt --check`
- Code passes `cargo clippy -- -D warnings`
- All tests pass with `cargo test`
