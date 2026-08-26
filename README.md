# OpenMainframe

OpenMainframe is a Rust implementation of core z/OS subsystems with a z/OSMF-compatible REST server.

It is designed to let standard mainframe tooling (including Zowe CLI and compatible workflows) interact with an emulated environment for development, migration, and modernization scenarios.

## Documentation

The [documentation hub](docs/README.md) provides the system architecture, a
complete map of all workspace crates, task-oriented guides, and reference
material. Each crate also keeps implementation-level documentation in its local
README.

## What This Repository Provides

- z/OSMF-compatible REST API server (`open-mainframe-zosmf`)
- Emulated subsystems for JES2, RACF, TSO, ISPF, datasets, JCL, COBOL, REXX, CICS, and more
- Local-first architecture implemented in Rust workspace crates
- Container-friendly runtime for integration in larger environments (for example `OpenMainframeWorkspace`)

## Quick Start

### Prerequisites

- Rust `1.82` or later (see `rust-toolchain.toml`)
- Cargo

### Run z/OSMF Server Locally

```bash
cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

Default endpoint:
- `http://127.0.0.1:10443/zosmf/info`

Default credentials:
- User: `IBMUSER`
- Password: `SYS1`

Override host/port with environment variables:

```bash
ZOSMF_HOST=0.0.0.0 ZOSMF_PORT=10443 cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

## Workspace Integration (Recommended)

In `OpenMainframeWorkspace`, this repository is started by Docker Compose as service `zosmf` and configured using `docker/zosmf.toml` from the workspace root.

From workspace root:

```bash
make up
```

## Agent Gym Harness

`open-mainframe-gym` provides an in-process, task-based environment for
SWE-Gym/SWE-bench-style mainframe development agents. It wraps the same z/OSMF
router used by normal clients, provisions isolated USS and dataset storage, and
returns structured observations plus declarative task reports.

```rust
use open_mainframe_gym::{dataset_create_task, GymConfig, MainframeGymEnv};

# async fn example() -> Result<(), String> {
let env = MainframeGymEnv::new(GymConfig::isolated())?;
let report = env
    .run_task(dataset_create_task("IBMUSER.GYM.SEQ"))
    .await?;

assert!(report.passed());
# Ok(())
# }
```

## Compatibility Highlights

- z/OSMF-style endpoints for files, jobs, TSO, console, USS
- HTTP Basic authentication and JWT cookie support
- Dataset and member operations
- JCL submission and spool inspection
- CICS transaction execution through configured application mappings
- Optional DRDA server support in server configuration

## Verify with Zowe CLI

```bash
# Create profile
zowe profiles create zosmf openmf \
  --host 127.0.0.1 \
  --port 10443 \
  --user IBMUSER \
  --password SYS1 \
  --reject-unauthorized false

# Check status
zowe zosmf check status

# Dataset list
zowe files list ds "IBMUSER.*"

# Submit JCL
zowe jobs submit lf my-job.jcl
```

## Core API Surface

| Method | Endpoint | Purpose |
|---|---|---|
| `GET` | `/zosmf/info` | Server info and health |
| `POST` | `/zosmf/services/authenticate` | Authentication |
| `GET` | `/zosmf/restfiles/ds?dslevel=HLQ.*` | List datasets |
| `GET/PUT/DELETE` | `/zosmf/restfiles/ds/{dsname}` | Read/write/delete dataset |
| `GET` | `/zosmf/restfiles/ds/{dsname}/member` | List PDS members |
| `PUT` | `/zosmf/restjobs/jobs` | Submit JCL |
| `GET` | `/zosmf/restjobs/jobs?...` | List jobs |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}` | Job status |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files` | Spool files |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files/{n}/records` | Spool contents |
| `POST/PUT` | `/zosmf/tsoApp/tso` and `/zosmf/tsoApp/tso/{key}` | Stateful TSO |
| `PUT` | `/zosmf/tsoApp/v1/tso` | Stateless TSO |
| `PUT` | `/zosmf/restconsoles/consoles/{name}` | Console command |
| `GET/PUT/DELETE` | `/zosmf/restfiles/fs...` | USS operations |

## Server Configuration

The server accepts `--config <path>` and reads TOML settings.

Example:

```toml
[server]
host = "0.0.0.0"
port = 10443

[auth]
token_ttl_seconds = 28800
token_secret = "change-me"

[uss]
root_directory = "/opt/openmainframe/uss"

[cors]
allowed_origins = ["*"]

[zosmf_info]
hostname = "my-mainframe"
saf_realm = "SAFRealm"
```

## Development Workflow

### Build

```bash
cargo build
cargo build --release
```

### Test

```bash
cargo test
```

### Lint/Format

```bash
cargo fmt --check
cargo clippy -- -D warnings
```

## Subsystem Crates

The workspace currently includes these subsystem crates (plus shared/core crates):

| Crate | Subsystem |
|---|---|
| `open-mainframe-zosmf` | z/OSMF REST API server |
| `open-mainframe-jes2` | JES2 |
| `open-mainframe-racf` | RACF security |
| `open-mainframe-tso` | TSO |
| `open-mainframe-ispf` | ISPF |
| `open-mainframe-rexx` | REXX |
| `open-mainframe-dataset` | Dataset I/O |
| `open-mainframe-jcl` | JCL parser/executor |
| `open-mainframe-cobol` | COBOL front-end/codegen |
| `open-mainframe-hlasm` | HLASM |
| `open-mainframe-mq` | MQ |
| `open-mainframe-sort` | DFSORT-like processing |
| `open-mainframe-runtime` | Language Environment runtime |
| `open-mainframe-cics` | CICS |
| `open-mainframe-ims` | IMS |
| `open-mainframe-db2` | DB2 support |
| `open-mainframe-smf` | SMF |
| `open-mainframe-wlm` | WLM |
| `open-mainframe-pli` | PL/I |
| `open-mainframe-tui` | Terminal/TN3270E services |
| `open-mainframe-mvs` | MVS system services |
| `open-mainframe-uss` | UNIX system services |
| `open-mainframe-utilities` | Utility programs |
| `open-mainframe-deploy` | Deployment helpers |
| `open-mainframe-assess` | Assessment tooling |
| `open-mainframe-lang-core` | Shared AST/diagnostics |

See `Cargo.toml` workspace members for the full, current list.

## Troubleshooting

### Server starts but cannot reach endpoint

- Verify host/port:
  - `ZOSMF_HOST`
  - `ZOSMF_PORT`
- Check server logs for bind failures.

### Zowe authentication issues

- Confirm profile host/port/user/password.
- Use `--reject-unauthorized false` for local non-TLS/self-signed scenarios where needed.

### Config not applied

- Ensure server launched with `--config /path/to/zosmf.toml`.
- Validate TOML syntax and section names.

## Contributing

Before opening changes:

```bash
cargo fmt --check
cargo clippy -- -D warnings
cargo test
```

## License

Apache-2.0
