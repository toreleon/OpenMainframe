# Configuration Reference

This reference documents the configuration system for the OpenMainframe z/OSMF
server (`open-mainframe-zosmf`), CICS application runner, and subsystem services.

> This page describes the current implementation. The proposed refactor makes
> these and deployment-specific formats adapters to one versioned product
> schema; see [Workspace Convergence](../architecture/workspace-convergence.md#configuration).

---

## How the Server Binary Resolves Configuration

The `zosmf-server` binary does not apply one uniform precedence rule to every
field. Its startup sequence is:

1. Load the file named by `--config <path>`, or use `ZosmfConfig::default()`
   when the flag is absent. There is no automatic search for `zosmf.toml`.
2. Override `config.server.port` with `ZOSMF_PORT`, falling back to `10443`
   when the variable is absent or invalid.
3. Override `config.server.host` with `ZOSMF_HOST`, falling back to
   `127.0.0.1` when the variable is absent.
4. Override `config.uss.root_directory` with
   `<system-temp-dir>/openmainframe-uss`.
5. Build `AppState` from the resulting configuration, including TOML mounts,
   CICS profiles, sysplex settings, auth settings, and DRDA settings.
6. Append any repeatable `--mount-dataset` and `--mount-uss` entries to the
   mount table.

Consequently, `[server]` and `[uss]` values deserialize correctly but are
overwritten by the current binary. Library users that construct
`AppState::new(config)` directly retain the values they supply. A config-file
load error is logged and the binary continues with defaults.

---

## Environment Variables

| Variable | Target Section / Field | Description | Default |
|---|---|---|---|
| `ZOSMF_HOST` | `server.host` | Host interface / IP address for the server listener. | `127.0.0.1` (CLI binary fallback) / `0.0.0.0` (TOML default) |
| `ZOSMF_PORT` | `server.port` | Listen port for the z/OSMF HTTP server. | `10443` |
| `CARDDEMO_DIR` | Path expansion | Base directory substituted for `${CARDDEMO_DIR}` and `$CARDDEMO_DIR` tokens in paths. | Auto-detected from workspace candidates if unset |
| `RUST_LOG` | Tracing / Logging | Filter directives for `tracing-subscriber` (e.g., `info`, `debug`, `open_mainframe_zosmf=trace`). | `open_mainframe_zosmf=info,tower_http=debug,warn` |

---

## TOML Configuration Sections

Below is the complete reference of all supported TOML sections and fields in `ZosmfConfig`.

### `[server]` — HTTP Server Settings

Configures network binding for the REST API server.

```toml
[server]
host = "0.0.0.0"
port = 10443
```

| Field | Type | Default | Description |
|---|---|---|---|
| `host` | `String` | `"0.0.0.0"` | Bind IP address (`0.0.0.0` binds to all network interfaces). |
| `port` | `u16` | `10443` | TCP port for the HTTP listener. |

---

### `[tls]` — TLS / HTTPS Settings (Optional)

Describes certificate paths for embedding applications. The fields deserialize,
but the current `zosmf-server` binary does not read them and always serves plain
HTTP through `axum::serve`.

```toml
[tls]
cert_file = "/etc/ssl/certs/openmf.pem"
key_file = "/etc/ssl/private/openmf.key"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `cert_file` | `String` | *Required when section is present* | Path to the PEM-encoded X.509 certificate chain file. Currently not consumed by the binary. |
| `key_file` | `String` | *Required when section is present* | Path to the PEM-encoded private key file. Currently not consumed by the binary. |

> [!WARNING]
> **Security Notice**: Ensure `key_file` has restricted filesystem permissions (e.g., `chmod 600`) and is never committed to source control.

---

### `[auth]` — Authentication & Token Settings

Controls JWT token generation, lifetime, and signing.

```toml
[auth]
token_ttl_seconds = 28800
token_algorithm = "HS256"
token_secret = "openmainframe-default-secret-change-me"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `token_ttl_seconds` | `u64` | `28800` (8 hours) | Token expiration duration in seconds. |
| `token_algorithm` | `String` | `"HS256"` | Deserialized algorithm label. The current JWT implementation always signs and verifies with HS256; other values do not change runtime behavior. |
| `token_secret` | `String` | `"openmainframe-default-secret-change-me"` | Secret key used for signing and validating JWT tokens. |

> [!CAUTION]
> **Secret Key Warning**: The default `token_secret` value (`"openmainframe-default-secret-change-me"`) must be changed in non-development deployments to prevent token forgery.

---

### `[uss]` — UNIX System Services Root

Maps host storage to the simulated z/OS UNIX filesystem (`/u/`).

```toml
[uss]
root_directory = "/opt/openmainframe/uss"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `root_directory` | `String` | `"/opt/openmainframe/uss"` | Host directory mapping used by library-created state. The current server binary always replaces it with `<temp_dir>/openmainframe-uss`. |

---

### `[cors]` — Cross-Origin Resource Sharing

```toml
[cors]
allowed_origins = ["*"]
```

| Field | Type | Default | Description |
|---|---|---|---|
| `allowed_origins` | `Vec<String>` | `["*"]` | List of allowed browser origins for cross-origin API calls. |

This field is currently configuration-only: `handlers::build_router()` does
not install a Tower HTTP CORS layer.

---

### `[zosmf_info]` — System Information Endpoint Values

Configures values reported by `GET /zosmf/info`.

```toml
[zosmf_info]
hostname = "openmainframe-host"
saf_realm = "SAFRealm"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `hostname` | `String` | `"openmainframe-host"` | Hostname string reported in z/OSMF discovery responses. |
| `saf_realm` | `String` | `"SAFRealm"` | SAF security realm reported to clients. |

---

### `[sysplex]` — Multi-System Sysplex Settings (Optional)

Configures simulated multi-system sysplex environments and system targets.

```toml
[sysplex]
name = "LOCAL"

[[sysplex.systems]]
sysname = "SYS1"
sysclone = "S1"
dataset_dir = "/tmp/openmainframe-datasets-sys1"
uss_root = "/tmp/openmainframe-uss-sys1"
zos_vr = "V2R5"
jes_type = "JES2"

[[sysplex.systems]]
sysname = "SYS2"
sysclone = "S2"
dataset_dir = "/tmp/openmainframe-datasets-sys2"
zos_vr = "V2R5"
jes_type = "JES2"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `name` | `String` | `"LOCAL"` | Name of the Sysplex cluster. |
| `systems` | `Vec<SystemNodeConfig>` | Local system | List of system nodes participating in the sysplex. |
| `systems[].sysname` | `String` | *Required* | Target system identifier (e.g., used in `/*ROUTE XEQ <system>`). |
| `systems[].sysclone` | `Option<String>` | First two `sysname` characters | Two-character clone identifier. |
| `systems[].dataset_dir` | `Option<String>` | *None* | Root dataset directory for this sysplex member. |
| `systems[].uss_root` | `Option<String>` | *None* | USS root override for this member. |
| `systems[].zos_vr` | `String` | `"V2R5"` | z/OS version/release string. |
| `systems[].jes_type` | `String` | `"JES2"` | JES implementation label. |

---

### `[[mounts]]` — Filesystem Mount Table

Maps host directories and files to virtual dataset names or USS paths. Can be specified multiple times in TOML as an array of tables.

```toml
[[mounts]]
type = "dataset-pds"
host_path = "${CARDDEMO_DIR}/app/cbl"
virtual_path = "IBMUSER.CARDDEMO.COBOL"
file_filter = "*.cbl"
read_only = false

[[mounts]]
type = "dataset-seq"
host_path = "${CARDDEMO_DIR}/app/data/ASCII/acctdata.txt"
virtual_path = "IBMUSER.CARDDEMO.ACCTDAT"
read_only = true

[[mounts]]
type = "uss"
host_path = "${CARDDEMO_DIR}"
virtual_path = "/u/ibmuser/carddemo"
read_only = false
```

| Field | Type | Default | Description |
|---|---|---|---|
| `type` | `String` | *Required* | Mount type: `"dataset-pds"`, `"dataset-seq"`, or `"uss"`. |
| `host_path` | `String` | *Required* | Host filesystem directory or file path. |
| `virtual_path` | `String` | *Required* | Virtual DSN (e.g. `HLQ.SRC.COBOL`) or USS path (e.g. `/u/user/dir`). |
| `read_only` | `bool` | `false` | When `true`, write/delete requests to this mount are rejected with an error. |
| `file_filter` | `Option<String>` | *None* | Optional glob pattern (e.g. `"*.cbl"`) filtering which host files appear as PDS members. |

---

### `[cics]` — CICS Application Server Configuration

Configures CICS runtime defaults, timeouts, system copybooks, and application profiles.

```toml
[cics]
default_app = "CARDDEMO"
session_timeout_seconds = 1800
system_copybooks = ["crates/open-mainframe-cics/copybooks"]
```

| Field | Type | Default | Description |
|---|---|---|---|
| `default_app` | `Option<String>` | *None* | Default application profile loaded when `POST /zosmf/cicsApp/terminal` omits `appName`. |
| `session_timeout_seconds` | `u64` | `1800` (30 minutes) | Deserialized timeout value. No session-cleanup path currently reads it. |
| `system_copybooks` | `Vec<String>` | `[]` | System-level CICS copybook directories (containing `DFHAID`, `DFHBMSCA`, `DFHEIBLK`). Automatically appended to every application profile. |

---

### `[cics.apps.<NAME>]` — Named CICS Application Profile

Defines application paths, copybooks, BMS screens, datasets, and transaction ID mappings for a specific application.

```toml
[cics.apps.CARDDEMO]
program = "${CARDDEMO_DIR}/app/cbl/COSGN00C.cbl"
include_paths = [
    "${CARDDEMO_DIR}/app/cpy",
    "${CARDDEMO_DIR}/app/cpy-bms",
    "crates/open-mainframe-cics/copybooks",
]
bms_dir = "${CARDDEMO_DIR}/app/bms"
program_dir = "${CARDDEMO_DIR}/app/cbl"
data_files = [
    "ACCTDAT=${CARDDEMO_DIR}/app/data/ASCII/acctdata.txt:11:300",
    "USRSEC=${CARDDEMO_DIR}/app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS:8:80",
]

[cics.apps.CARDDEMO.transids]
CC00 = "COSGN00C"
CM00 = "COMEN01C"
CA00 = "COADM01C"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `program` | `String` | *Required* | Path to the initial COBOL entry program (`.cbl`). |
| `include_paths` | `Vec<String>` | `[]` | Directories searched during `COPY` statement expansion. |
| `bms_dir` | `Option<String>` | *None* | Directory containing `.bms` map definition files. |
| `program_dir` | `Option<String>` | *None* | Directory searched for dynamically invoked programs (`XCTL`, `LINK`). |
| `data_files` | `Vec<String>` | `[]` | VSAM data files formatted as `DDNAME=path[:key_len[:rec_len]]`. |
| `transids` | `HashMap<String, String>` | `{}` | Key-value mappings of 4-character CICS `TRANSID` to COBOL program name. |

---

### `[db2_server]` — DB2 DRDA Protocol Server

Configures the standalone DRDA wire protocol listener for DB2 clients.

```toml
[db2_server]
enabled = true
host = "0.0.0.0"
port = 50000
database = "DSN1"
location = "OPENMF"
```

| Field | Type | Default | Description |
|---|---|---|---|
| `enabled` | `bool` | `true` | Whether the DRDA TCP listener starts automatically with the server. |
| `host` | `String` | `"0.0.0.0"` | Bind IP address for the DRDA listener. |
| `port` | `u16` | `50000` | Standard DB2 DRDA port. |
| `database` | `String` | `"DSN1"` | Target relational database name accepted in `ACCRDB` negotiation. |
| `location` | `String` | `"OPENMF"` | Distributed Data Facility (DDF) location name returned to clients. |

---

## Variable Expansion in Paths

Configuration loading recognizes the `CARDDEMO_DIR` placeholder in either
braced or unbraced form:

- `${CARDDEMO_DIR}` / `$CARDDEMO_DIR`: Replaced by the value of the `CARDDEMO_DIR` environment variable.
- **Auto-Detection**: If `CARDDEMO_DIR` is unset in the environment, the server automatically checks candidate locations (`../carddemo`, `../OpenMainframeWorkspace/carddemo`, `../../carddemo`, etc.) containing `app/cbl` and `app/bms` directories.

---

## Filesystem and Security Implications

### 1. Storage Locations and Ephemeral Data
- **Native Datasets**: The current binary replaces the catalog with one rooted at `<temp_dir>/openmainframe-datasets`.
- **USS Filesystem**: The current binary always sets the root to `<temp_dir>/openmainframe-uss`.
- **Batch Spool & Temporary Work**: JCL batch jobs create temporary workspaces at `<temp_dir>/openmainframe-work-<jobid>` and `<temp_dir>/openmainframe-sysout-<jobid>`.
- **Operations Log**: Maintained in an in-memory ring buffer in `AppState`.

### 2. On-Demand COBOL Compilation
- `open_mainframe_lib::compile_program()` reads COBOL source files **fresh from disk** on every program invocation.
- There is **no compilation cache**. Edits to `.cbl` files in mounted or configured directories immediately affect subsequent CICS program loads.

### 3. Read-Only Mount Protection
- Setting `read_only = true` on `[[mounts]]` entries prevents accidental modification of source files, copybooks, or baseline test data via the REST API.

---

## Related Documentation

- [Architecture Overview](../architecture/overview.md) — System boundaries and subsystem architecture.
- [Workspace Crate Map](../architecture/crate-map.md) — 44-crate catalog and dependencies.
- [Getting Started](../guides/getting-started.md) — Build, run, and smoke test instructions.
- [z/OSMF API Reference](zosmf-api.md) — REST endpoint specifications.
