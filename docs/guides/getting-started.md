# Getting Started with OpenMainframe

This guide walks you through building, configuring, running, and verifying the
OpenMainframe z/OSMF server and CLI tools locally.

---

## Prerequisites

- **Rust Toolchain**: Rust `1.82` or later (enforced via `rust-toolchain.toml`).
- **Cargo**: Standard package manager included with Rust.
- **Optional Tools**:
  - `make` (for running Makefile convenience targets)
  - `curl` and `jq` (for API verification)
  - `zowe` (Zowe CLI)

## Building the Workspace

### 1. Build All Binaries and Crates

```bash
# Build the complete workspace, including the symbolic execution tool
cargo build --release --workspace
```

### 2. Build Specific Binaries

```bash
# Build the z/OSMF server binary
cargo build --release -p open-mainframe-zosmf --bin zosmf-server

# Build the OpenMainframe CLI binary
cargo build --release -p open-mainframe --bin open-mainframe

# Alternatively, use the Makefile target
make build-cli
```

---

## Running the Server and Tools

### 1. Start the z/OSMF REST API Server

```bash
# Start with built-in defaults (binds to 127.0.0.1:10443)
cargo run --release -p open-mainframe-zosmf --bin zosmf-server
```

#### Run with Custom Host, Port, and Config File

```bash
ZOSMF_HOST=0.0.0.0 ZOSMF_PORT=10443 cargo run --release -p open-mainframe-zosmf --bin zosmf-server -- --config zosmf.toml
```

#### Run with Explicit Host Filesystem Mounts

```bash
cargo run --release -p open-mainframe-zosmf --bin zosmf-server -- \
  --mount-dataset "/path/to/cbl:IBMUSER.SRC.COBOL:pds" \
  --mount-uss "/path/to/work:/u/ibmuser/work:uss"
```

The mount argument format is `HOST_PATH:VIRTUAL_PATH[:pds|seq|uss]`.

When the server starts successfully, it logs startup parameters:

```text
INFO open_mainframe_zosmf: z/OSMF server starting bind_addr=127.0.0.1:10443 sysplex=LOCAL systems=1 mounts=0
```

> [!NOTE]
> On startup, the server automatically spawns the DB2 DRDA wire protocol server
> on TCP port `50000` (configurable in `[db2_server]`) and provisions the default
> administrative credentials (`IBMUSER` / `SYS1`).

---

### 2. Run Interactive CICS Sessions (CLI)

The `open-mainframe` binary provides direct terminal emulation for COBOL and
CICS programs without going through the HTTP server.

```bash
# Interactive 3270 TUI session
cargo run --release -p open-mainframe --bin open-mainframe -- cics \
  path/to/PROGRAM.cbl \
  -I path/to/copybooks \
  --bms-dir path/to/bms \
  --data "ACCTFILE=path/to/data.txt:11:300" \
  --transid "CC00=PROGRAM"

# Headless mode (JSON protocol on stdin/stdout for automation)
cargo run --release -p open-mainframe --bin open-mainframe -- cics \
  path/to/PROGRAM.cbl \
  --headless
```

### 3. Run the Documentation Wiki Generator

```bash
cargo run --release -p open-mainframe --bin open-mainframe -- wiki \
  path/to/mainframe/source \
  --output path/to/wiki_output
```

---

## Health Check and Authentication

### 1. Health & Discovery (`GET /zosmf/info`)

The info endpoint is unauthenticated and returns server version, hostname, and
active plugin statuses:

```bash
curl -s http://127.0.0.1:10443/zosmf/info | jq .
```

Example response:

```json
{
  "api_version": "1",
  "zosmf_version": "27",
  "zosmf_full_version": "27.0",
  "zosmf_hostname": "openmainframe-host",
  "zosmf_port": "10443",
  "zos_version": "02.05.00",
  "zosmf_saf_realm": "SAFRealm",
  "plugins": [
    { "plugin_version": "1.0.0", "plugin_default_name": "z/OSMF Restfiles", "plugin_status": "ACTIVE" },
    { "plugin_version": "1.0.0", "plugin_default_name": "z/OSMF JES", "plugin_status": "ACTIVE" },
    { "plugin_version": "1.0.0", "plugin_default_name": "z/OSMF TSO", "plugin_status": "ACTIVE" },
    { "plugin_version": "1.0.0", "plugin_default_name": "z/OSMF Console", "plugin_status": "ACTIVE" },
    { "plugin_version": "1.0.0", "plugin_default_name": "z/OSMF WLM", "plugin_status": "ACTIVE" }
  ]
}
```

### 2. Authentication (`POST /zosmf/services/authenticate`)

Authenticate using default credentials (`IBMUSER` / `SYS1`) to receive a JWT
session cookie:

```bash
curl -s -i -X POST http://127.0.0.1:10443/zosmf/services/authenticate \
  -u IBMUSER:SYS1
```

The response body is `{}`. The signed JWT is returned only in the
`Set-Cookie: jwtToken=...` header. Protected endpoints also accept Basic
authentication directly, which the smoke tests below use for clarity.

---

## API Smoke Tests (Self-Contained)

All smoke tests below use standalone curl commands and built-in datasets
without requiring external sample applications.

The examples send the conventional `X-CSRF-ZOSMF-HEADER` on mutations for
z/OSMF client compatibility. The current router accepts that header but does
not install the repository's CSRF middleware, so it is not presently enforced.

### 1. Dataset Operations (`/zosmf/restfiles/ds/*`)

#### Create a Sequential Dataset

```bash
curl -s -X POST http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.SEQ \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: application/json" \
  -d '{"dsorg": "PS", "recfm": "FB", "lrecl": 80, "blksize": 800}'
```

#### Write Content to Dataset

```bash
curl -s -X PUT http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.SEQ \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: text/plain" \
  -d $'FIRST RECORD IN DATASET\nSECOND RECORD IN DATASET'
```

#### Read Dataset Content

```bash
curl -s http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.SEQ \
  -u IBMUSER:SYS1
```

#### Create a Partitioned Dataset (PDS) and Add a Member

```bash
# Create PDS
curl -s -X POST http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.PDS \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: application/json" \
  -d '{"dsorg": "PO", "recfm": "FB", "lrecl": 80}'

# Write member MEM1
curl -s -X PUT "http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.PDS(MEM1)" \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: text/plain" \
  -d $'IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.'

# List members
curl -s http://127.0.0.1:10443/zosmf/restfiles/ds/IBMUSER.SAMPLE.PDS/member \
  -u IBMUSER:SYS1 | jq .
```

#### List Datasets by Prefix

```bash
curl -s "http://127.0.0.1:10443/zosmf/restfiles/ds?dslevel=IBMUSER.*" \
  -u IBMUSER:SYS1 | jq .
```

---

### 2. Job Submission and Spool Inspection (`/zosmf/restjobs/jobs/*`)

#### Submit JCL via PUT

```bash
JOB_OUTPUT=$(curl -s -X PUT http://127.0.0.1:10443/zosmf/restjobs/jobs \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: text/plain" \
  -d $'//TESTJOB  JOB (ACCT),CLASS=A\n//STEP1    EXEC PGM=IEFBR14')

echo "$JOB_OUTPUT" | jq .
JOBID=$(echo "$JOB_OUTPUT" | jq -r .jobid)
JOBNAME=$(echo "$JOB_OUTPUT" | jq -r .jobname)
```

#### Check Job Status

```bash
curl -s "http://127.0.0.1:10443/zosmf/restjobs/jobs/$JOBNAME/$JOBID" \
  -u IBMUSER:SYS1 | jq .
```

#### List Spool Files and Retrieve Output

```bash
# List spool files
curl -s "http://127.0.0.1:10443/zosmf/restjobs/jobs/$JOBNAME/$JOBID/files" \
  -u IBMUSER:SYS1 | jq .

# Read JESMSGLG spool records (spool file index 0)
curl -s "http://127.0.0.1:10443/zosmf/restjobs/jobs/$JOBNAME/$JOBID/files/0/records" \
  -u IBMUSER:SYS1
```

---

### 3. TSO and Console Commands

#### Issue Stateless TSO Command

```bash
curl -s -X PUT http://127.0.0.1:10443/zosmf/tsoApp/v1/tso \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: application/json" \
  -d '{"command": "TIME"}' | jq .
```

#### Issue MVS Console Command

```bash
curl -s -X PUT http://127.0.0.1:10443/zosmf/restconsoles/consoles/CONSOLE1 \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: application/json" \
  -d '{"cmd": "D T"}' | jq .
```

---

### 4. DB2 SQL Execution (`/zosmf/db2/sql`)

```bash
curl -s -X POST http://127.0.0.1:10443/zosmf/db2/sql \
  -u IBMUSER:SYS1 \
  -H "X-CSRF-ZOSMF-HEADER: true" \
  -H "Content-Type: application/json" \
  -d '{"sql": "SELECT CURRENT TIMESTAMP FROM SYSIBM.SYSDUMMY1"}' | jq .
```

---

## Verifying with Zowe CLI

### 1. Create a z/OSMF Profile

```bash
zowe profiles create zosmf-profile openmf \
  --host 127.0.0.1 \
  --port 10443 \
  --user IBMUSER \
  --password SYS1 \
  --ru false
```

### 2. Verify Connection and Subsystems

```bash
# Check status
zowe zosmf check status --zosmf-p openmf

# List datasets
zowe files list ds "IBMUSER.*" --zosmf-p openmf

# Submit a local JCL file
zowe jobs submit local-file sample.jcl --zosmf-p openmf

# Issue console command
zowe console issue command "D A,L" --zosmf-p openmf
```

---

## Focused Test Commands

Run targeted test suites across individual subsystems:

```bash
# Run z/OSMF server unit and integration tests
cargo test -p open-mainframe-zosmf

# Run CICS transaction processor tests
cargo test -p open-mainframe-cics

# Run JCL interpreter and PROC expansion tests
cargo test -p open-mainframe-jcl

# Run dataset access and IDCAMS tests
cargo test -p open-mainframe-dataset

# Run RACF security and SAF router tests
cargo test -p open-mainframe-racf

# Run DB2 SQL engine tests
cargo test -p open-mainframe-db2

# Run full workspace test suite
cargo test --workspace

# Check lints and code style
cargo clippy --workspace -- -D warnings
cargo fmt --check
```

---

## Related Documentation

- [Architecture Overview](../architecture/overview.md) — System boundaries and end-to-end flows.
- [Workspace Crate Map](../architecture/crate-map.md) — Comprehensive guide to all 44 crates.
- [Configuration Reference](../reference/configuration.md) — TOML configuration settings and options.
- [z/OSMF API Reference](../reference/zosmf-api.md) — Full REST route and payload specification.
