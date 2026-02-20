# OpenMainframe

A z/OSMF-compatible REST API server written in Rust. Provides a drop-in replacement for IBM z/OSMF that works with standard Zowe CLI and Zowe Explorer tooling, backed by Rust implementations of core z/OS subsystems (JES2, RACF, TSO, ISPF, datasets, JCL, COBOL, REXX, and more).

## Quick Start

Requires Rust 1.82 or later.

```bash
git clone https://github.com/toreleon/OpenMainframe.git
cd OpenMainframe
cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

The server starts on `http://127.0.0.1:10443` with a default user `IBMUSER` / `SYS1`.

Override the port with the `ZOSMF_PORT` environment variable:

```bash
ZOSMF_PORT=8443 cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

## Features

- **Zowe CLI / Zowe Explorer compatible** — connect standard z/OS tooling to a local server
- **Dataset operations** — create, list, read, write, delete sequential datasets and PDS members
- **Job management** — submit JCL, list jobs, view status, read spool files, cancel, purge
- **TSO commands** — stateful sessions and stateless v1 API (Zowe CLI v3+)
- **MVS console commands** — issue operator commands with solicitation key support
- **USS file operations** — list, read, write, delete with Unix permissions and metadata
- **JWT authentication** — RACF user validation with HTTP Basic Auth and token cookies
- **TN3270E terminal server** — 3270 screen emulation
- **Zowe API ML registration** — Eureka service discovery

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

## Subsystem Libraries

The z/OSMF server is backed by standalone Rust implementations of z/OS subsystems, each available as a library crate:

| Crate | Subsystem |
|-------|-----------|
| `open-mainframe-zosmf` | z/OSMF REST API server (Zowe-compatible) |
| `open-mainframe-jes2` | JES2 job entry subsystem |
| `open-mainframe-racf` | RACF security (users, groups, profiles, SAF) |
| `open-mainframe-tso` | TSO command processor |
| `open-mainframe-ispf` | ISPF panels, tables, editor, file tailoring |
| `open-mainframe-rexx` | REXX interpreter |
| `open-mainframe-dataset` | Dataset I/O (VSAM, QSAM, PDS, GDG, catalog) |
| `open-mainframe-jcl` | JCL parser, executor, utilities |
| `open-mainframe-cobol` | COBOL lexer, parser, semantic analysis, codegen |
| `open-mainframe-hlasm` | HLASM assembler |
| `open-mainframe-mq` | IBM MQ queue manager |
| `open-mainframe-encoding` | EBCDIC encoding (21 code pages) |
| `open-mainframe-sort` | DFSORT (SORT/MERGE/COPY, ICETOOL) |
| `open-mainframe-runtime` | Language Environment runtime |
| `open-mainframe-cics` | CICS transaction processor |
| `open-mainframe-ims` | IMS/DB (DL/I) |
| `open-mainframe-db2` | DB2 (EXEC SQL, PostgreSQL backend) |
| `open-mainframe-smf` | SMF records |
| `open-mainframe-wlm` | Workload Manager |
| `open-mainframe-pli` | PL/I parser and interpreter |
| `open-mainframe-tui` | TN3270E terminal server |
| `open-mainframe-deploy` | Container deployment (K8s, Docker) |
| `open-mainframe-assess` | Code complexity analysis |
| `open-mainframe-lang-core` | Shared AST, spans, diagnostics |

## License

Apache-2.0

## Contributing

Contributions welcome! Please ensure:
- Code passes `cargo fmt --check`
- Code passes `cargo clippy -- -D warnings`
- All tests pass with `cargo test`
