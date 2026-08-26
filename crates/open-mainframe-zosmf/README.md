# open-mainframe-zosmf

z/OSMF REST API Server — Axum-based z/OS Management Facility server providing standard REST endpoints for Zowe CLI, Zowe Explorer, and Zowe API Mediation Layer integration with OpenMainframe.

## Purpose

z/OSMF (z/OS Management Facility) is IBM's standard HTTP/JSON management interface for mainframe environments. This crate implements the z/OSMF REST API gateway over an Axum router, allowing external developer tooling and automation (such as Zowe) to manage datasets, submit JCL jobs, interact with CICS terminal sessions, run TSO commands, issue operator console commands, and query workload management (WLM) and topology without modifying client tools.

## Capabilities

- **REST API Endpoints** (16 handler modules in `handlers`):
  - **Authenticate** (`/zosmf/services/authenticate`): RACF-backed authentication producing HMAC-SHA256 signed JSON Web Tokens (JWT) and processing HTTP Basic Auth.
  - **Datasets** (`/zosmf/restfiles/ds/*`): List datasets and PDS members, read/write sequential datasets and PDS members, create datasets (PS, PDS, PDSE, VSAM), delete datasets, and execute IDCAMS / Access Method Services (AMS) statements.
  - **Jobs** (`/zosmf/restjobs/jobs/*`): Submit JCL jobs, query job execution status, list job spool output files, retrieve spool file content, and cancel/hold/release/purge jobs.
  - **TSO Commands & Servlets** (`/zosmf/tsoApp/tso*`): Stateless command execution and stateful interactive TSO servlet session lifecycle.
  - **Console Commands** (`/zosmf/restconsoles/consoles/*`): Issue MVS system operator commands and retrieve synchronous/asynchronous command responses.
  - **USS Files** (`/zosmf/restfiles/fs/*`): UNIX System Services file reading, writing, creation, and directory listing mapped to host paths.
  - **CICS Sessions** (`/zosmf/cicsApp/terminal/*`): Interactive CICS 3270 terminal session lifecycle over REST (start session, send AID key + field inputs, read screen buffer, terminate).
  - **Workload Management** (`/zosmf/zwlm/rest/1.0/*`): Query service policies and classes, install policies, and expose compatibility resource-pool operations.
  - **System Variables** (`/zosmf/variables/rest/1.0/systems/*`): Query system symbols for the local or a named sysplex member.
  - **Topology** (`/zosmf/resttopology/systems/*`): Query sysplex systems, CPU capacity, and active status.
  - **Workflows & Provisioning** (`/zosmf/workflow/rest/1.0/*`, `/zosmf/provisioning/rest/1.0/*`): In-memory workflow instances and cloud-provisioning compatibility records.
  - **Server Info** (`/zosmf/info`): Server version, hostname, SAF realm, and active plugin listings.
  - **DB2 & Logs** (`/zosmf/db2/*`, `/zosmf/logs`): DB2 compatibility responses, catalog/package listings, and operations-log queries.
  - **CMCI** (`/CICSSystemManagement/*`): CICS Management Client Interface compatibility routing.
- **Dedicated CICS Session Runner** (`cics_runner`): Bridges asynchronous Axum requests with synchronous CICS runtimes (`CicsBridge`) using dedicated OS worker threads and tokio channels (`SessionCommand`/`SessionResponse`).
- **Filesystem Mounts** (`mounts`): `MountTable` supporting CLI-configured host directory mappings (`--mount-dataset`, `--mount-uss`) to virtual PDS datasets and USS directory trees.
- **Zowe API Mediation Layer** (`eureka`): Client registration payload builder and periodic heartbeat generator for Eureka discovery services.
- **Server Binary** (`zosmf-server`): Standalone executable accepting `--config <path>`, `--mount-dataset <spec>`, and `--mount-uss <spec>` flags.

## Architecture

```
         Zowe CLI / Explorer / API ML / HTTP Client
                             │
                             ▼
    ┌────────────────────────────────────────────────────────┐
    │                      Axum Router                       │
    │  - HTTP tracing layer                                  │
    │  - AuthContext extractor on protected handlers        │
    └────────────────────────┬───────────────────────────────┘
                             │
                             ▼
    ┌────────────────────────────────────────────────────────┐
    │                    Handler Modules                     │
    │  - datasets, jobs, tso, console, files, cics, info...  │
    └────────────────────────┬───────────────────────────────┘
                             │
                             ▼
    ┌────────────────────────────────────────────────────────┐
    │                       AppState                         │
    │  - DashMap session stores (CICS, TSO, Console)         │
    │  - Subsystem handles: RACF, JES2, Catalog, WLM         │
    │  - MountTable (host filesystem -> PDS/USS paths)       │
    │  - Parmlib / SymbolEngine integration                  │
    └────────────────────────────────────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `handlers` | 16 REST endpoint modules: `authenticate`, `cics`, `cmci`, `console`, `datasets`, `db2`, `files`, `info`, `jobs`, `logs`, `provisioning`, `topology`, `tso`, `variables`, `wlm`, `workflow` |
| `state` | `AppState`: Central multi-threaded context shared by Axum handlers |
| `config` | `ZosmfConfig`: Configuration parser loading `zosmf.toml` |
| `cics_runner` | `CicsSessionRunner`: Dedicated OS thread runner for CICS sessions |
| `mounts` | `MountTable`: Virtual dataset and USS filesystem mapping |
| `jwt` | JSON Web Token encoding, decoding, and HMAC-SHA256 signature verification |
| `middleware` | `AuthContext` extractor plus an available CSRF middleware; the current router does not install the CSRF layer |
| `eureka` | Eureka service registration for Zowe API Mediation Layer |
| `types` | Request/response DTOs binary-compatible with IBM z/OSMF JSON specifications |
| `tn3270` | TN3270 session state and authentication helpers |
| `sysplex` | Sysplex system registration and topology modeling |

## Public API and Binaries

### Binaries

- `zosmf-server`: Standalone executable CLI entry point (`src/main.rs`).
  ```bash
  zosmf-server [--config <path>] [--mount-dataset <host:virtual[:type]>] [--mount-uss <host:virtual[:type]>]
  ```

### Primary Library Types and Functions

- `build_router(config: ZosmfConfig) -> axum::Router`: Assembles the full Axum router with all endpoint routes and state.
- `AppState`: Shared application state holding subsystem managers, catalog, mount table, and session caches (`new()`).
- `ZosmfConfig`: Server, auth, CICS, mounts, sysplex, and subsystem configuration (`from_file()`, `default()`).
- `cics_runner`: `CicsSessionRunner`, `SessionCommand`, `SessionResponse`.
- `mounts`: `MountTable`, `MountType`, `parse_mount_arg`.
- `types`: DTO modules for `auth`, `datasets`, `jobs`, `tso`, `console`, `info`, and `error`.
- `Result<T>`: Convenience alias returning `ZosmfErrorResponse`.

## Integration

- **Internal workspace dependencies**:
  - `open-mainframe-racf` (user authentication and access authorization)
  - `open-mainframe-dataset` (catalog and dataset record I/O)
  - `open-mainframe-jes2` (job entry subsystem and spool)
  - `open-mainframe-jcl` (JCL parsing and execution)
  - `open-mainframe-tso` (TSO command processor)
  - `open-mainframe-encoding` (EBCDIC/ASCII code page conversion)
  - `open-mainframe-wlm` (workload manager policy inspection)
  - `open-mainframe-parmlib` (system symbols and parmlib resolution)
  - `open-mainframe-cics` (CICS transaction processor)
  - `open-mainframe-lib` (COBOL compilation and runtime bridges)
  - `open-mainframe-runtime` (Language Environment runtime)
  - `open-mainframe-tui` (3270 terminal session modeling)
  - `open-mainframe-db2` (DB2 SQL preprocessor and runtime)
  - `open-mainframe-smf` (SMF recording)
  - `open-mainframe-drda` (DRDA server protocol integration)
- **Consumers**:
  - `open-mainframe-gym`: Consumes `ZosmfConfig`, `AppState`, and `open_mainframe_zosmf::handlers::build_router` for in-process agent benchmarking.

## Examples

### Building the Router Programmatically

```rust
use open_mainframe_zosmf::{build_router, config::ZosmfConfig};

#[tokio::main]
async fn main() {
    let config = ZosmfConfig::default();
    let router = build_router(config);

    // Router can now be served via axum or tested with tower::ServiceExt
    let listener = tokio::net::TcpListener::bind("127.0.0.1:10443").await.unwrap();
    axum::serve(listener, router).await.unwrap();
}
```

### Running the z/OSMF Server Binary

```bash
cargo run --release -p open-mainframe-zosmf --bin zosmf-server -- \
    --config zosmf.toml \
    --mount-dataset ./app/cbl:USER.COBOL.SRC \
    --mount-uss ./app/uss:/u/ibmuser
```

## Testing

Run unit, end-to-end, and benchmark tests:

```bash
cargo test -p open-mainframe-zosmf
```

The test suite contains 122 tests:
- 88 unit tests in `src/lib.rs` (DTO serialization, JWT validation, mount resolution, AID key parsing, Eureka payloads).
- 9 end-to-end dataset tests in `tests/e2e_datasets.rs` (CRUD, PDS members, JSON format compliance).
- 9 end-to-end job tests in `tests/e2e_jobs.rs` (JCL submission, spool reading, job purge).
- 11 end-to-end TSO and console tests in `tests/e2e_tso_console.rs` (login, stateless TSO, MVS commands).
- 4 end-to-end benchmark tests in `tests/e2e_benchmarks.rs` (endpoint latency, concurrent sessions).
- 1 documentation test.

## Limitations

- **CICS Session Thread Affinity**: Because the CICS execution bridge (`CicsBridge`) uses single-threaded cell references (`Rc<RefCell<>>`), CICS sessions are pinned to dedicated OS threads and managed via messaging channels rather than native async tasks.
- **JWT Token Revocation**: JWT authentication relies on cryptographic signature verification against `token_secret` with expiration; immediate token revocation lists are not persisted across server restarts.
- **In-Memory Spool Backing**: Spool files generated by jobs are retained in memory and temp files rather than multi-volume DASD checkpointed datasets.
- **Parsed but Inactive Settings**: TLS certificate and CORS origin settings deserialize into `ZosmfConfig`, but the current `zosmf-server` binary serves plain HTTP and the router installs no CORS layer.
- **CSRF Layer Not Registered**: `middleware::csrf` is implemented, but `handlers::build_router()` does not install it; mutating routes therefore do not currently enforce `X-CSRF-ZOSMF-HEADER`.
- **DB2 REST Compatibility Mode**: `POST /zosmf/db2/sql` currently returns a mock-mode protocol response. Stateful SQL execution is provided by the separate DRDA server path.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-gym](../open-mainframe-gym/README.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
- [open-mainframe-jcl](../open-mainframe-jcl/README.md)
