# OpenMainframe

An open-source z/OS-compatible mainframe environment written in Rust. Includes a COBOL compiler, JCL interpreter, z/OSMF-compatible REST API server, and implementations of core z/OS subsystems including JES2, RACF, TSO, ISPF, REXX, HLASM, MQ, and more.

## Features

- **z/OSMF REST API Server**: Drop-in replacement for IBM z/OSMF, compatible with Zowe CLI and Zowe Explorer
  - Dataset operations (create, list, read, write, delete, PDS members)
  - Job management (submit JCL, list, status, spool files, cancel, purge)
  - TSO command execution (stateful sessions and stateless v1 API)
  - MVS console commands
  - USS file operations (list, read, write, delete with Unix permissions)
  - JWT authentication with RACF user validation
  - TN3270E terminal server with 3270 screen emulation
  - Zowe API ML service registration (Eureka)

- **COBOL Compiler**: Parse and compile COBOL programs
  - Lexer and parser for IBM-style COBOL
  - Semantic analysis with symbol table
  - Tree-walking interpreter for execution without LLVM
  - Optional LLVM backend for native code generation

- **JCL Interpreter**: Execute Job Control Language scripts
  - JCL parsing and job execution
  - DD statement handling for dataset allocation
  - EXEC statement for running programs
  - Utility programs (IEBGENER, IEBCOPY, IDCAMS, DFSORT)

- **JES2 Job Entry Subsystem**: Full job lifecycle management
  - Job queue and scheduling engine
  - Spool management
  - Initiator management
  - JES2 operator commands ($D, $H, $A, $P, $C)

- **RACF Security**: Resource Access Control Facility
  - User/group management with RACF database
  - Dataset access control profiles and PERMIT
  - SAF router interface (RACROUTE)
  - Password and PassTicket authentication
  - RACDCERT certificate management

- **TSO/ISPF**: Interactive environment
  - TSO command processor (ALLOCATE, FREE, SUBMIT, STATUS, LISTDS)
  - REXX interpreter with full language support
  - ISPF panel services, table services, file tailoring, editor

- **Dataset I/O**: Comprehensive dataset support
  - VSAM (KSDS, ESDS, RRDS), QSAM/BSAM/BPAM
  - PDS/PDSE with member management
  - GDG (Generation Data Groups)
  - Dataset catalog for DSN resolution

- **EBCDIC Support**: Full EBCDIC-ASCII translation
  - 21 code pages
  - Packed/zoned decimal, HFP/IEEE floating point

- **Additional Subsystems**: HLASM assembler, IBM MQ, CICS, IMS/DB, DB2, WLM, SMF

## Installation

Requires Rust 1.82 or later.

```bash
git clone https://github.com/toreleon/OpenMainframe.git
cd OpenMainframe
cargo build --release
```

## z/OSMF Server (Zowe CLI Compatible)

The z/OSMF server provides a REST API that is wire-compatible with IBM z/OSMF, allowing standard Zowe CLI and Zowe Explorer to connect to it as if it were a real z/OS system.

### Quick Start

```bash
# Build and run the z/OSMF server
cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

The server starts on `http://127.0.0.1:10443` with a default user `IBMUSER` / `SYS1`.

Override the port with the `ZOSMF_PORT` environment variable:

```bash
ZOSMF_PORT=8443 cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

### Connecting Zowe CLI

```bash
# Install Zowe CLI (if not already installed)
npm install -g @zowe/cli

# Create a Zowe profile pointing to the OpenMainframe server
zowe profiles create zosmf openmf --host 127.0.0.1 --port 10443 --user IBMUSER --password SYS1 --reject-unauthorized false

# Or with Zowe v3 team config (zowe.config.json):
zowe config init
# Then edit the host/port/user/password to point to your server
```

### Verify Connection

```bash
# Check server status
zowe zosmf check status

# List datasets
zowe files list ds "IBMUSER.*"

# Create and write a dataset
zowe files create ds "IBMUSER.MY.DATA" --dsorg PS --recfm FB --lrecl 80
zowe files upload ftds "local-file.txt" "IBMUSER.MY.DATA"
zowe files view ds "IBMUSER.MY.DATA"

# Submit JCL and view output
zowe jobs submit lf my-job.jcl
zowe jobs list jobs
zowe jobs view sfbi JOB00001 1

# Issue TSO command
zowe tso issue command "TIME"

# Issue console command
zowe console issue command "D A,L"

# List USS files
zowe files list uss "/"
```

### REST API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/zosmf/info` | Server information |
| `POST` | `/zosmf/services/authenticate` | Authenticate (returns JWT) |
| `GET` | `/zosmf/restfiles/ds?dslevel=HLQ.*` | List datasets |
| `PUT` | `/zosmf/restfiles/ds/{dsname}` | Create dataset |
| `GET` | `/zosmf/restfiles/ds/{dsname}` | Read dataset content |
| `PUT` | `/zosmf/restfiles/ds/{dsname}` | Write dataset content |
| `DELETE` | `/zosmf/restfiles/ds/{dsname}` | Delete dataset |
| `GET` | `/zosmf/restfiles/ds/{dsname}/member` | List PDS members |
| `PUT` | `/zosmf/restjobs/jobs` | Submit JCL |
| `GET` | `/zosmf/restjobs/jobs?owner=*&prefix=*` | List jobs |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}` | Job status |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files` | List spool files |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files/{n}/records` | Read spool content |
| `PUT` | `/zosmf/restjobs/jobs/{name}/{id}` | Hold/release/cancel job |
| `DELETE` | `/zosmf/restjobs/jobs/{name}/{id}` | Purge job |
| `POST` | `/zosmf/tsoApp/tso` | Start TSO session |
| `PUT` | `/zosmf/tsoApp/tso/{key}` | Send TSO command |
| `PUT` | `/zosmf/tsoApp/v1/tso` | Stateless TSO command (v1 API) |
| `PUT` | `/zosmf/restconsoles/consoles/{name}` | Issue console command |
| `GET` | `/zosmf/restfiles/fs?path=/` | List USS directory |
| `GET` | `/zosmf/restfiles/fs/{path}` | Read USS file |
| `PUT` | `/zosmf/restfiles/fs/{path}` | Write USS file |
| `DELETE` | `/zosmf/restfiles/fs/{path}` | Delete USS file/directory |

All endpoints require HTTP Basic Auth (`Authorization: Basic ...`) or a JWT token (`Cookie: jwtToken=...`).

### Server Configuration

Create a `zosmf.toml` file to customize the server:

```toml
[server]
host = "0.0.0.0"
port = 10443

[auth]
token_ttl_seconds = 28800  # 8 hours
token_secret = "change-me-in-production"

[uss]
root_directory = "/opt/openmainframe/uss"

[cors]
allowed_origins = ["*"]

[zosmf_info]
hostname = "my-mainframe"
saf_realm = "SAFRealm"
```

### Adding Users

Users are managed through the RACF subsystem. The default server creates `IBMUSER` with password `SYS1`. Additional users can be added programmatically through the RACF API or by extending `src/main.rs`.

## COBOL Compiler Usage

```bash
# Interpret a COBOL program
open-mainframe interpret program.cbl

# Check COBOL syntax
open-mainframe check program.cbl

# Compile (requires LLVM feature)
cargo build --release --features llvm
open-mainframe compile program.cbl -o program

# Run a JCL job
open-mainframe run job.jcl

# Parse JCL (show structure)
open-mainframe parse-jcl job.jcl
```

### Configuration

OpenMainframe supports configuration through:

1. **Project config**: `./open-mainframe.toml`
2. **User config**: `~/.config/open-mainframe/config.toml`
3. **Environment variables**: `OPEN_MAINFRAME_*`

```bash
open-mainframe config show   # Show current configuration
open-mainframe config init   # Generate default config file
open-mainframe config paths  # Show config paths
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
  open-mainframe/             # CLI binary
  open-mainframe-zosmf/       # z/OSMF REST API server (Zowe-compatible)
  open-mainframe-cobol/       # COBOL lexer, parser, semantic analysis, codegen
  open-mainframe-jcl/         # JCL lexer, parser, executor, utilities
  open-mainframe-jes2/        # JES2 job entry subsystem
  open-mainframe-racf/        # RACF security (users, groups, profiles, SAF)
  open-mainframe-tso/         # TSO command processor
  open-mainframe-ispf/        # ISPF panels, tables, editor, file tailoring
  open-mainframe-rexx/        # REXX interpreter
  open-mainframe-hlasm/       # HLASM assembler
  open-mainframe-mq/          # IBM MQ queue manager
  open-mainframe-runtime/     # Language Environment runtime
  open-mainframe-dataset/     # Dataset I/O (VSAM, QSAM, PDS, GDG, catalog)
  open-mainframe-encoding/    # EBCDIC encoding (21 code pages)
  open-mainframe-sort/        # DFSORT (SORT/MERGE/COPY, ICETOOL)
  open-mainframe-cics/        # CICS transaction processor
  open-mainframe-ims/         # IMS/DB (DL/I)
  open-mainframe-db2/         # DB2 (EXEC SQL, PostgreSQL backend)
  open-mainframe-smf/         # SMF records
  open-mainframe-wlm/         # Workload Manager
  open-mainframe-pli/         # PL/I parser and interpreter
  open-mainframe-tui/         # TN3270E terminal server
  open-mainframe-deploy/      # Container deployment (K8s, Docker)
  open-mainframe-assess/      # Code complexity analysis
  open-mainframe-lang-core/   # Shared AST, spans, diagnostics
```

## License

Apache-2.0

## Contributing

Contributions welcome! Please ensure:
- Code passes `cargo fmt --check`
- Code passes `cargo clippy -- -D warnings`
- All tests pass with `cargo test`
