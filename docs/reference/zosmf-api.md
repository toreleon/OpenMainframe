# z/OSMF REST API Reference

This reference documents the complete REST API surface implemented by
`open-mainframe-zosmf` and the subsystem crates backing each route family.

---

## Authentication and Security Conventions

### Headers & Tokens
- **Bearer Token**: `Authorization: Bearer <jwtToken>`
- **Basic Authentication**: `Authorization: Basic <base64(user:password)>`
- **Cookie Authentication**: `Cookie: jwtToken=<jwtToken>`
- **CSRF Header**: `X-CSRF-ZOSMF-HEADER` is the conventional mutation header and a middleware implementation exists. The current `handlers::build_router()` does not install that middleware, so the registered routes do not presently enforce the header.
- **Unauthenticated Endpoints**: `GET /zosmf/info` and initial login `POST /zosmf/services/authenticate`.

### Standard Response Codes
- `200 OK`: Request succeeded, response body returned.
- `201 Created`: Resource successfully created (dataset, job, PDS member).
- `204 No Content`: Action succeeded, no response body (dataset delete, content write).
- `206 Partial Content`: Truncated list response when `X-IBM-Max-Items` limit is reached.
- `304 Not Modified`: Cached response valid via `If-None-Match` ETag.
- `400 Bad Request`: Invalid parameter, malformed JSON, or missing required body.
- `401 Unauthorized`: Missing or invalid credentials / expired JWT token.
- `403 Forbidden`: RACF authorization rejection (and the response used by the CSRF middleware if it is installed by an embedding application).
- `404 Not Found`: Dataset, member, job, session, or file does not exist.
- `412 Precondition Failed`: ETag mismatch during `If-Match` conditional writes.
- `500 Internal Server Error`: Subsystem error or lock poisoning.

---

## Route Families and Endpoints

### 1. System Information & Discovery

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `GET` | `/zosmf/info` | None | `handlers/info.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |

- **Semantics**: Returns z/OSMF version, OS version (`02.05.00`), hostname, SAF realm, and 9 active plugin descriptors (`z/OSMF Restfiles`, `z/OSMF JES`, `z/OSMF TSO`, `z/OSMF Console`, `z/OSMF WLM`, `z/OSMF Variables`, `z/OSMF Topology`, `z/OSMF Workflow`, `z/OSMF Provisioning`).

---

### 2. Authentication Service

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `POST` | `/zosmf/services/authenticate` | Basic / None | `handlers/authenticate.rs` (`open-mainframe-racf`) | **Fully Implemented** |
| `DELETE` | `/zosmf/services/authenticate` | Required | `handlers/authenticate.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |

- **POST Semantics**: Requires Basic authentication and validates credentials via SAF against the RACF user database. Returns the JSON body `{}` and sets `Set-Cookie: jwtToken=<token>; Path=/; HttpOnly; Secure; SameSite=Strict`; the token is not duplicated in the response body.
- **DELETE Semantics**: Invalidates the active token in the server token store.

---

### 3. Dataset REST Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `GET` | `/zosmf/restfiles/ds` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `GET` | `/zosmf/restfiles/ds/{dsn}` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `PUT` | `/zosmf/restfiles/ds/{dsn}` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `POST` | `/zosmf/restfiles/ds/{dsn}` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `DELETE` | `/zosmf/restfiles/ds/{dsn}` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `GET` | `/zosmf/restfiles/ds/{dsn}/member` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `GET` | `/zosmf/restfiles/ds/{dsn}/search` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |
| `PUT` | `/zosmf/restfiles/ams` | Required | `handlers/datasets.rs` (`open-mainframe-dataset`) | **Fully Implemented** |

- **GET `/zosmf/restfiles/ds`**: Query params `dslevel` (pattern, e.g. `IBMUSER.*`), `start` (pagination). Headers `X-IBM-Max-Items`, `X-IBM-Attributes` (`dsname`, `base`, `vol`). Response includes `X-IBM-Response-Rows`. Merges catalog datasets and active mount table entries.
- **GET `/zosmf/restfiles/ds/{dsn}`**: Reads sequential dataset or `DSN(MEMBER)`. Supports `X-IBM-Data-Type: text` or `binary`, `If-None-Match`, and returns `ETag`.
- **PUT `/zosmf/restfiles/ds/{dsn}`**: Writes dataset/member content or executes JSON action (`rename`, `copy`, `hmigrate`, `hrecall`). Supports `If-Match` ETag concurrency verification.
- **POST `/zosmf/restfiles/ds/{dsn}`**: Allocates dataset using `DatasetCreateParams` (`dsorg`, `recfm`, `lrecl`, `blksize`, `vol`).
- **GET `/zosmf/restfiles/ds/{dsn}/member`**: Lists PDS members with ISPF statistics (`vers`, `mod`, `c4date`, `m4date`, `cnorc`, `inorc`).
- **GET `/zosmf/restfiles/ds/{dsn}/search`**: Substring text search inside dataset records.
- **PUT `/zosmf/restfiles/ams`**: Executes IDCAMS (Access Method Services) control statements (`DEFINE CLUSTER`, `REPRO`, `DELETE`, `LISTCAT`).

---

### 4. Job REST Services (JES2)

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `GET` | `/zosmf/restjobs/jobs` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |
| `PUT` | `/zosmf/restjobs/jobs` | Required | `handlers/jobs.rs` (`open-mainframe-jcl`, `jes2`) | **Fully Implemented** |
| `GET` | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |
| `PUT` | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |
| `DELETE` | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |
| `GET` | `/zosmf/restjobs/jobs/{jobname}/{jobid}/files` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |
| `GET` | `/zosmf/restjobs/jobs/{jobname}/{jobid}/files/{id}/records` | Required | `handlers/jobs.rs` (`open-mainframe-jes2`) | **Fully Implemented** |

- **PUT `/zosmf/restjobs/jobs`**: Submits raw JCL text body or JSON dataset reference (`{"file": "//'IBMUSER.JCL(JOB1)'"}`). Parses JCL, expands PROCs, executes batch steps, writes execution results to JES2 spool datasets (`JESMSGLG`, `JESJCL`, `SYSPRINT`, `SYSOUT`), and returns full `JobResponse` JSON with HTTP `201 Created`.
- **GET `/zosmf/restjobs/jobs`**: Lists jobs filtered by `owner`, `prefix`, `status`, `jobid`, and `exec-member` (target system).
- **PUT `/zosmf/restjobs/jobs/{jobname}/{jobid}`**: Executes job action (`hold`, `release`, `cancel`).
- **DELETE `/zosmf/restjobs/jobs/{jobname}/{jobid}`**: Purges job entry and all associated spool datasets.
- **GET `.../files` and `.../files/{id}/records`**: Lists spool datasets and retrieves raw text records.

---

### 5. TSO REST Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `POST` | `/zosmf/tsoApp/tso` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |
| `PUT` | `/zosmf/tsoApp/tso/{servlet_key}` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |
| `GET` | `/zosmf/tsoApp/tso/{servlet_key}` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |
| `DELETE` | `/zosmf/tsoApp/tso/{servlet_key}` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |
| `PUT` | `/zosmf/tsoApp/tso/ping/{servlet_key}` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |
| `PUT` | `/zosmf/tsoApp/v1/tso` | Required | `handlers/tso.rs` (`open-mainframe-tso`) | **Fully Implemented** |

- **POST `/zosmf/tsoApp/tso`**: Launches a stateful TSO address space session or runs a single command. Returns `servletKey`.
- **PUT `/zosmf/tsoApp/v1/tso`**: Executes a stateless TSO command directly (e.g. `TIME`, `LISTCAT`, `ALLOCATE`) and returns immediate output lines.

---

### 6. MVS Console & Operations Log Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `PUT` | `/zosmf/restconsoles/consoles/{name}` | Required | `handlers/console.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `GET` | `/zosmf/restconsoles/consoles/{name}/solmsgs/{key}` | Required | `handlers/console.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `GET` | `/zosmf/restconsoles/consoles/{name}/detections/{key}` | Required | `handlers/console.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `GET` | `/zosmf/logs` | Required | `handlers/logs.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `GET` | `/zosmf/restconsoles/v1/log` | Required | `handlers/logs.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |

- **PUT `/zosmf/restconsoles/consoles/{name}`**: Executes MVS console operator commands (e.g. `D A,L`, `D T`, `D IPLINFO`, `VARY`, `START`, `STOP`).
- **GET `/zosmf/logs` / `/zosmf/restconsoles/v1/log`**: Queries recent system log records (WTO, WTOR, system messages) filtered by system and jobname.

---

### 7. UNIX System Services (USS) File REST Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `GET` | `/zosmf/restfiles/fs` & `/fs/{*path}` | Required | `handlers/files.rs` (`open-mainframe-zosmf`, host filesystem) | **Fully Implemented** |
| `PUT` | `/zosmf/restfiles/fs` & `/fs/{*path}` | Required | `handlers/files.rs` (`open-mainframe-zosmf`, host filesystem) | **Fully Implemented** |
| `POST` | `/zosmf/restfiles/fs` & `/fs/{*path}` | Required | `handlers/files.rs` (`open-mainframe-zosmf`, host filesystem) | **Fully Implemented** |
| `DELETE` | `/zosmf/restfiles/fs` & `/fs/{*path}` | Required | `handlers/files.rs` (`open-mainframe-zosmf`, host filesystem) | **Fully Implemented** |
| `GET` | `/zosmf/restfiles/mfs` | Required | `handlers/files.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `PUT` | `/zosmf/restfiles/mfs/{*fsname}` | Required | `handlers/files.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |

- **Semantics**: Full POSIX file hierarchy operations within the configured `[uss].root_directory` and external USS mounts. Supports directory listings, file read/write, directory creation, deletion, and zFS filesystem mount queries.

---

### 8. CICS Application Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `POST` | `/zosmf/cicsApp/terminal` | Required | `handlers/cics.rs` (`cics_runner.rs`, `open-mainframe-cics`) | **Fully Implemented** |
| `GET` | `/zosmf/cicsApp/terminal/{session_key}` | Required | `handlers/cics.rs` (`cics_runner.rs`) | **Fully Implemented** |
| `PUT` | `/zosmf/cicsApp/terminal/{session_key}` | Required | `handlers/cics.rs` (`cics_runner.rs`) | **Fully Implemented** |
| `DELETE` | `/zosmf/cicsApp/terminal/{session_key}` | Required | `handlers/cics.rs` (`cics_runner.rs`) | **Fully Implemented** |
| `GET` | `/zosmf/cicsApp/apps` | Required | `handlers/cics.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `POST` | `/zosmf/cicsApp/apps` | Required | `handlers/cics.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |
| `DELETE` | `/zosmf/cicsApp/apps/{app_name}` | Required | `handlers/cics.rs` (`open-mainframe-zosmf`) | **Fully Implemented** |

- **Session Lifecycle**: `POST /zosmf/cicsApp/terminal` spawns a dedicated OS thread running `CicsSessionRunner`. Returns initial 3270 screen buffer and session key.
- **Input Cycle**: `PUT /zosmf/cicsApp/terminal/{session_key}` sends AID key and input field values, executing COBOL programs on-demand from disk and returning the resulting screen buffer.
- **Dynamic Applications**: `POST /zosmf/cicsApp/apps` registers new CICS application profiles dynamically at runtime.

---

### 9. DB2 REST Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `POST` | `/zosmf/db2/sql` | Required | `handlers/db2.rs` (`open-mainframe-db2`) | **Fully Implemented** |
| `GET` | `/zosmf/db2/subsystems` | Required | `handlers/db2.rs` (`open-mainframe-db2`) | **Fully Implemented** |
| `GET` | `/zosmf/db2/tables` | Required | `handlers/db2.rs` (`open-mainframe-db2`) | **Fully Implemented** |
| `GET` | `/zosmf/db2/packages` | Required | `handlers/db2.rs` (`open-mainframe-db2`) | **Fully Implemented** |
| `POST` | `/zosmf/db2/call` | Required | `handlers/db2.rs` (`open-mainframe-db2`) | **Fully Implemented** |

- **Semantics**: `POST /sql` currently uses `SqlExecutor` in mock mode and returns a protocol-compatible success response with an empty result set. The listing endpoints expose the built-in subsystem, CICS-data-file-derived table names, and packages stored in `AppState`; procedure calls also return compatibility responses. The separate DRDA server owns the stateful wire-protocol SQL path.

---

### 10. Workload Manager (WLM) Services

| Method | Route | Auth | Responsible Module / Crate | Status |
|---|---|---|---|---|
| `GET` | `/zosmf/zwlm/rest/1.0/policy` | Required | `handlers/wlm.rs` (`open-mainframe-wlm`) | **Fully Implemented** |
| `GET` | `/zosmf/zwlm/rest/1.0/classes` | Required | `handlers/wlm.rs` (`open-mainframe-wlm`) | **Fully Implemented** |
| `PUT` | `/zosmf/zwlm/rest/1.0/policy/{name}` | Required | `handlers/wlm.rs` (`open-mainframe-wlm`) | **Fully Implemented** |
| `POST` | `/zosmf/zwlm/rest/1.0/wrps` | Required | `handlers/wlm.rs` (`open-mainframe-wlm`) | Compatibility Stub |
| `DELETE` | `/zosmf/zwlm/rest/1.0/wrps/{wrpid}` | Required | `handlers/wlm.rs` (`open-mainframe-wlm`) | Compatibility Stub |

---

### 11. System Variables, Topology, Workflows, & Provisioning (Compatibility Services)

| Family | Method | Route | Responsible Module | Status |
|---|---|---|---|---|
| **Variables** | `GET` | `/zosmf/variables/rest/1.0/systems/local` | `handlers/variables.rs` | **Fully Implemented** |
| **Variables** | `GET` | `/zosmf/variables/rest/1.0/systems/{system_ref}` | `handlers/variables.rs` | **Fully Implemented** |
| **Topology** | `GET` | `/zosmf/resttopology/systems` | `handlers/topology.rs` | Compatibility Model |
| **Topology** | `GET` | `/zosmf/resttopology/systems/{sysname}` | `handlers/topology.rs` | Compatibility Model |
| **Workflow** | `POST` | `/zosmf/workflow/rest/1.0/workflows` | `handlers/workflow.rs` | In-Memory Stub |
| **Workflow** | `GET` | `/zosmf/workflow/rest/1.0/workflows` | `handlers/workflow.rs` | In-Memory Stub |
| **Workflow** | `GET` | `/zosmf/workflow/rest/1.0/workflows/{key}` | `handlers/workflow.rs` | In-Memory Stub |
| **Workflow** | `DELETE` | `/zosmf/workflow/rest/1.0/workflows/{key}` | `handlers/workflow.rs` | In-Memory Stub |
| **Provisioning** | `GET` | `/zosmf/provisioning/rest/1.0/psc` | `handlers/provisioning.rs` | In-Memory Stub |
| **Provisioning** | `POST/GET` | `/zosmf/provisioning/rest/1.0/scr` | `handlers/provisioning.rs` | In-Memory Stub |
| **Provisioning** | `GET/DELETE` | `/zosmf/provisioning/rest/1.0/scr/{id}` | `handlers/provisioning.rs` | In-Memory Stub |
| **CMCI** | `GET/POST/PUT/DELETE` | `/CICSSystemManagement/{type}/{plex}/{region}` | `handlers/cmci.rs` | Compatibility Model |

- **Implementation Note**: Topology, Workflow, Cloud Provisioning, and CMCI expose compatibility-oriented representations backed by configuration and thread-safe in-memory collections. They do not invoke external topology, workflow, or provisioning engines.

---

## Related Documentation

- [Architecture Overview](../architecture/overview.md) — Dispatch and execution flows.
- [Workspace Crate Map](../architecture/crate-map.md) — Responsible crates and dependencies.
- [Getting Started](../guides/getting-started.md) — Testing commands and curl examples.
- [Configuration Reference](configuration.md) — Server settings and options.
