//! z/OSMF REST API documentation generator.

use std::fs;

use crate::{WikiConfig, WikiResult};

/// Generate all API reference pages.
pub fn generate_api_pages(config: &WikiConfig) -> WikiResult<()> {
    let out = config.output_dir.join("api");
    fs::write(out.join("index.md"), api_index())?;
    fs::write(out.join("auth.md"), auth_page())?;
    fs::write(out.join("datasets.md"), datasets_page())?;
    fs::write(out.join("jobs.md"), jobs_page())?;
    fs::write(out.join("tso.md"), tso_page())?;
    fs::write(out.join("console.md"), console_page())?;
    fs::write(out.join("files.md"), files_page())?;
    fs::write(out.join("cics.md"), cics_page())?;
    Ok(())
}

fn api_index() -> &'static str {
    r#"# z/OSMF REST API Reference

The z/OSMF REST API provides programmatic access to z/OS system services over HTTP.
This implementation exposes 56+ endpoints across 7 modules, providing dataset
management, job submission, TSO command execution, USS file operations, console
commands, and CICS terminal sessions.

## Base URL

```
http://host:port
```

Default: `http://localhost:10443`

## Authentication

All endpoints require authentication via one of:

1. **Basic Auth** -- `Authorization: Basic base64(user:password)`
2. **JWT Token** -- `Authorization: Bearer <token>` (obtained via `/zosmf/services/authenticate`)

Default credentials: `IBMUSER` / `SYS1`

## Common Headers

| Header | Required | Description |
|--------|----------|-------------|
| `Authorization` | Yes | Basic Auth or Bearer token |
| `X-CSRF-ZOSMF-HEADER` | Yes | CSRF protection (any non-empty value) |
| `Content-Type` | For PUT/POST | Usually `application/json` or `text/plain` |
| `Accept` | Optional | `application/json` (default) |

## API Modules

| Module | Base Path | Endpoints | Description |
|--------|-----------|-----------|-------------|
| **[Authentication](auth.md)** | `/zosmf/services/authenticate` | 1 | JWT token creation |
| **[Datasets](datasets.md)** | `/zosmf/restfiles/ds` | 10+ | Dataset CRUD, PDS members |
| **[Jobs](jobs.md)** | `/zosmf/restjobs/jobs` | 6 | JCL submission, status, spool |
| **[TSO](tso.md)** | `/zosmf/tsoApp/tso` | 4 | TSO address space management |
| **[Console](console.md)** | `/zosmf/restconsoles/consoles` | 2 | MVS console commands |
| **[USS Files](files.md)** | `/zosmf/restfiles/fs` | 5+ | UNIX file operations |
| **[CICS](cics.md)** | `/zosmf/cicsApp/terminal` | 4 | CICS session management |

## Error Responses

All error responses return JSON:

```json
{
  "rc": 8,
  "reason": 13,
  "message": "Dataset not found"
}
```

## Server Information

```
GET /zosmf/info
```

Returns server version, available services, and system details. Does not require
authentication.
"#
}

fn auth_page() -> &'static str {
    r#"# Authentication Reference

## Create JWT Token

```
POST /zosmf/services/authenticate
```

Authenticates the user and returns a JWT token for subsequent requests.

### Request Headers

| Header | Value | Required |
|--------|-------|----------|
| `Authorization` | `Basic base64(user:password)` | Yes |
| `X-CSRF-ZOSMF-HEADER` | Any non-empty value (e.g., `true`) | Yes |

### Example Request

```bash
curl -X POST https://host:port/zosmf/services/authenticate \
  -H "Authorization: Basic $(echo -n 'IBMUSER:SYS1' | base64)" \
  -H "X-CSRF-ZOSMF-HEADER: true"
```

### Response (200 OK)

```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

The token is also returned in the `Set-Cookie` header as `jwtToken`.

### Token TTL

Configured via `zosmf.toml`:
```toml
[auth]
token_ttl_seconds = 28800    # 8 hours default
token_secret = "change-me-in-production"
```

### Using the Token

Include the token in subsequent requests:

```bash
curl https://host:port/zosmf/restfiles/ds?dslevel=SYS1 \
  -H "Authorization: Bearer eyJhbG..." \
  -H "X-CSRF-ZOSMF-HEADER: true"
```

### Cookie-Based Authentication

The JWT token can also be sent via cookie:

```bash
curl https://host:port/zosmf/restfiles/ds?dslevel=SYS1 \
  -b "jwtToken=eyJhbG..."
```

### Error Responses

| Status | Condition |
|--------|-----------|
| 401 | Invalid credentials |
| 403 | Account locked or expired |
| 500 | Server error |
"#
}

fn datasets_page() -> &'static str {
    r#"# Dataset Endpoints

## List Datasets

```
GET /zosmf/restfiles/ds?dslevel={pattern}
```

List datasets matching a pattern. Supports wildcards (`*`, `**`).

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `dslevel` | string | Dataset name pattern (e.g., `SYS1.*`, `USER.**`) |

### Response (200 OK)

```json
{
  "returnedRows": 3,
  "items": [
    {
      "dsname": "SYS1.PARMLIB",
      "dsorg": "PO",
      "recfm": "FB",
      "lrecl": 80,
      "blksz": 27920,
      "vol": "RES001"
    },
    {
      "dsname": "SYS1.PROCLIB",
      "dsorg": "PO",
      "recfm": "FB",
      "lrecl": 80,
      "blksz": 27920,
      "vol": "RES001"
    }
  ]
}
```

---

## Get Dataset Attributes

```
GET /zosmf/restfiles/ds/{dsname}
```

Returns detailed attributes of a specific dataset.

### Response (200 OK)

```json
{
  "dsname": "USER.DATA.FILE",
  "dsorg": "PS",
  "recfm": "FB",
  "lrecl": 80,
  "blksz": 27920,
  "vol": "USR001",
  "cdate": "2024/01/15",
  "rdate": "2024/03/01",
  "spacu": "TRACKS",
  "sizex": 50,
  "usedx": 12
}
```

---

## List PDS Members

```
GET /zosmf/restfiles/ds/{dsname}/member
```

List members of a partitioned dataset.

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `pattern` | string | Member name pattern (optional) |

### Response (200 OK)

```json
{
  "returnedRows": 2,
  "items": [
    { "member": "IEASYS00" },
    { "member": "IEASYS01" }
  ]
}
```

---

## Read Dataset Content

```
GET /zosmf/restfiles/ds/-/{dsname}
```

Read the contents of a sequential dataset.

### Response (200 OK)

Returns the dataset content as plain text with `Content-Type: text/plain`.

---

## Write Dataset Content

```
PUT /zosmf/restfiles/ds/-/{dsname}
```

Write content to an existing sequential dataset.

### Request Headers

| Header | Value |
|--------|-------|
| `Content-Type` | `text/plain` |

### Request Body

Plain text content to write to the dataset.

### Response

| Status | Condition |
|--------|-----------|
| 204 | Success (no content) |
| 404 | Dataset not found |
| 500 | Write error |

---

## Create Dataset

```
POST /zosmf/restfiles/ds/{dsname}
```

Create a new dataset.

### Request Body (JSON)

```json
{
  "dsorg": "PS",
  "recfm": "FB",
  "lrecl": 80,
  "blksz": 27920,
  "primary": 50,
  "secondary": 10,
  "alcunit": "TRK",
  "vol": "USR001"
}
```

For a PDS:
```json
{
  "dsorg": "PO",
  "recfm": "FB",
  "lrecl": 80,
  "blksz": 27920,
  "primary": 50,
  "secondary": 10,
  "alcunit": "TRK",
  "dsntype": "PDS",
  "dirblk": 10
}
```

### Response

| Status | Condition |
|--------|-----------|
| 201 | Created |
| 409 | Already exists |
| 500 | Creation error |

---

## Delete Dataset

```
DELETE /zosmf/restfiles/ds/-/{dsname}
```

Delete a dataset.

### Response

| Status | Condition |
|--------|-----------|
| 204 | Deleted |
| 404 | Not found |
| 500 | Deletion error |

---

## Read PDS Member

```
GET /zosmf/restfiles/ds/-/{dsname}({member})
```

Read the contents of a PDS member.

### Response (200 OK)

Returns the member content as plain text.

---

## Write PDS Member

```
PUT /zosmf/restfiles/ds/-/{dsname}({member})
```

Write content to a PDS member. Creates the member if it does not exist.

### Request Headers

| Header | Value |
|--------|-------|
| `Content-Type` | `text/plain` |

### Request Body

Plain text content for the member.

### Response

| Status | Condition |
|--------|-----------|
| 204 | Success |
| 404 | PDS not found |
| 500 | Write error |
"#
}

fn jobs_page() -> &'static str {
    r#"# Job Endpoints

## List Jobs

```
GET /zosmf/restjobs/jobs
```

List jobs matching filter criteria.

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `owner` | string | Job owner (default: authenticated user) |
| `prefix` | string | Job name prefix (supports `*` wildcard) |
| `status` | string | Job status filter: `ACTIVE`, `OUTPUT`, `INPUT` |
| `jobid` | string | Specific job ID (e.g., `JOB00123`) |

### Response (200 OK)

```json
{
  "items": [
    {
      "jobname": "MYJOB",
      "jobid": "JOB00123",
      "owner": "IBMUSER",
      "status": "OUTPUT",
      "type": "JOB",
      "class": "A",
      "retcode": "CC 0000",
      "subsystem": "JES2"
    }
  ]
}
```

### Return Code Format
| Value | Meaning |
|-------|---------|
| `CC 0000` | Condition code 0 |
| `CC 0004` | Condition code 4 |
| `ABEND S0C7` | System abend |
| `ABEND U1234` | User abend |
| `JCL ERROR` | JCL error |
| `null` | Job still active |

---

## Get Job Status

```
GET /zosmf/restjobs/jobs/{jobname}/{jobid}
```

Get detailed status of a specific job.

### Response (200 OK)

```json
{
  "jobname": "MYJOB",
  "jobid": "JOB00123",
  "owner": "IBMUSER",
  "status": "OUTPUT",
  "type": "JOB",
  "class": "A",
  "retcode": "CC 0000",
  "phaseName": "Job is on the hard copy queue",
  "subsystem": "JES2"
}
```

---

## Submit JCL

```
PUT /zosmf/restjobs/jobs
```

Submit JCL for execution.

### Request Headers

| Header | Value |
|--------|-------|
| `Content-Type` | `text/plain` |
| `X-IBM-Intrdr-Class` | Input class (optional, default `A`) |
| `X-IBM-Intrdr-Recfm` | Record format (optional, default `F`) |
| `X-IBM-Intrdr-Lrecl` | Logical record length (optional, default `80`) |

### Request Body

JCL text:
```jcl
//MYJOB   JOB (ACCT),'MY JOB',CLASS=A,MSGCLASS=H
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
HELLO WORLD
/*
//SYSUT2   DD SYSOUT=*
//SYSIN    DD DUMMY
```

### Response (201 Created)

```json
{
  "jobname": "MYJOB",
  "jobid": "JOB00124",
  "owner": "IBMUSER",
  "status": "INPUT"
}
```

---

## Cancel / Purge Job

```
DELETE /zosmf/restjobs/jobs/{jobname}/{jobid}
```

Cancel an active job or purge a completed job's output.

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `modify` | string | `cancel` (cancel active) or `purge` (delete output) |

### Response

| Status | Condition |
|--------|-----------|
| 200 | Job cancelled/purged |
| 404 | Job not found |

---

## List Spool Files

```
GET /zosmf/restjobs/jobs/{jobname}/{jobid}/files
```

List spool (DD) files for a job.

### Response (200 OK)

```json
{
  "items": [
    {
      "id": 1,
      "ddname": "JESMSGLG",
      "stepname": "JES2",
      "procstep": "",
      "class": "H",
      "recfm": "UA",
      "lrecl": 133,
      "byteCount": 1024,
      "recordCount": 15
    },
    {
      "id": 2,
      "ddname": "JESJCL",
      "stepname": "JES2",
      "procstep": "",
      "class": "H",
      "recfm": "V",
      "lrecl": 136,
      "byteCount": 512,
      "recordCount": 8
    },
    {
      "id": 3,
      "ddname": "SYSPRINT",
      "stepname": "STEP1",
      "procstep": "",
      "class": "H",
      "recfm": "UA",
      "lrecl": 133,
      "byteCount": 256,
      "recordCount": 3
    }
  ]
}
```

---

## Read Spool Content

```
GET /zosmf/restjobs/jobs/{jobname}/{jobid}/files/{id}/records
```

Read the content of a specific spool file.

### Response (200 OK)

Returns spool content as plain text.
"#
}

fn tso_page() -> &'static str {
    r#"# TSO Endpoints

## Start TSO Address Space

```
POST /zosmf/tsoApp/tso
```

Start a new TSO address space for interactive command execution.

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `proc` | string | Logon procedure (default: `IKJACCNT`) |
| `acct` | string | Account number |
| `region` | string | Region size (e.g., `4096`) |
| `rows` | string | Terminal rows (default: `24`) |
| `cols` | string | Terminal columns (default: `80`) |

### Response (200 OK)

```json
{
  "servletKey": "IBMUSER-123-abc",
  "tsoData": [
    {
      "TSO MESSAGE": {
        "DATA": "READY"
      }
    }
  ]
}
```

The `servletKey` identifies this TSO session for subsequent requests.

---

## Receive Messages

```
GET /zosmf/tsoApp/tso/{servletKey}/receive
```

Receive pending messages and output from the TSO address space.

### Response (200 OK)

```json
{
  "servletKey": "IBMUSER-123-abc",
  "tsoData": [
    {
      "TSO MESSAGE": {
        "DATA": "LISTDS 'SYS1.PARMLIB'"
      }
    },
    {
      "TSO MESSAGE": {
        "DATA": "--VOLUMES-- RES001"
      }
    },
    {
      "TSO MESSAGE": {
        "DATA": "READY"
      }
    }
  ]
}
```

---

## Send Command

```
PUT /zosmf/tsoApp/tso/{servletKey}
```

Send a command to the active TSO address space.

### Request Body (JSON)

```json
{
  "TSO RESPONSE": {
    "DATA": "LISTDS 'SYS1.PARMLIB'"
  }
}
```

### Response (200 OK)

```json
{
  "servletKey": "IBMUSER-123-abc",
  "tsoData": [
    {
      "TSO MESSAGE": {
        "DATA": "SYS1.PARMLIB"
      }
    },
    {
      "TSO MESSAGE": {
        "DATA": "--RECFM-LRECL-BLKSIZE-DSORG"
      }
    },
    {
      "TSO MESSAGE": {
        "DATA": "  FB    80   27920   PO"
      }
    },
    {
      "TSO MESSAGE": {
        "DATA": "READY"
      }
    }
  ]
}
```

---

## Stop TSO Address Space

```
DELETE /zosmf/tsoApp/tso/{servletKey}
```

Terminate the TSO address space and release resources.

### Response

| Status | Condition |
|--------|-----------|
| 200 | Session terminated |
| 404 | Session not found |

### Response Body (200)

```json
{
  "servletKey": "IBMUSER-123-abc",
  "message": "TSO address space ended"
}
```
"#
}

fn console_page() -> &'static str {
    r#"# Console Endpoints

## Issue MVS Command

```
PUT /zosmf/restconsoles/consoles/{name}
```

Issue an MVS operator command and receive the response.

### Path Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | string | Console name (e.g., `defcn`) |

### Request Body (JSON)

```json
{
  "cmd": "D A,L",
  "sol-key": "optional-key"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `cmd` | string | MVS command to issue |
| `sol-key` | string | Key for retrieving solicited messages (optional) |

### Response (200 OK)

```json
{
  "sol-key": "C0000001",
  "cmd-response": " IEE114I 14.30.00 2024.060 ACTIVITY 801\n JOBS    M/S    TS USERS    SYSAS    INITS   ACTIVE/MAX\n 00005  00018  00003     00027  00005   00005/00020"
}
```

### Common Commands

| Command | Description |
|---------|-------------|
| `D A,L` | Display active address spaces |
| `D T` | Display date and time |
| `D M=STOR` | Display real storage |
| `D U,DASD` | Display DASD status |
| `D GRS,CONTENTION` | Display enqueue contention |
| `D SMF` | Display SMF status |
| `D PROD,STATE` | Display registered products |

---

## Get Solicited Messages

```
GET /zosmf/restconsoles/consoles/{name}/solmsgs
```

Retrieve solicited messages from a previous command.

### Path Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | string | Console name |

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `sol-key` | string | Solicited message key from previous command |

### Response (200 OK)

```json
{
  "sol-key": "C0000001",
  "cmd-response": "IEE114I ..."
}
```
"#
}

fn files_page() -> &'static str {
    r#"# USS File Endpoints

Endpoints for Unix System Services (USS) file and directory operations.

## List Directory / Read File

```
GET /zosmf/restfiles/fs/{path}
```

If `{path}` is a directory, returns a listing. If a file, returns the content.

### Response -- Directory Listing (200 OK)

```json
{
  "returnedRows": 3,
  "items": [
    {
      "name": "myfile.txt",
      "mode": "-rwxr-xr-x",
      "size": 1024,
      "uid": 0,
      "gid": 0,
      "mtime": "2024-01-15T10:30:00Z",
      "type": "file"
    },
    {
      "name": "subdir",
      "mode": "drwxr-xr-x",
      "size": 8192,
      "uid": 0,
      "gid": 0,
      "mtime": "2024-01-10T08:00:00Z",
      "type": "directory"
    }
  ]
}
```

### Response -- File Content (200 OK)

Returns file content as plain text with `Content-Type: text/plain`.

---

## Write File

```
PUT /zosmf/restfiles/fs/{path}
```

Write content to a USS file. Creates the file if it does not exist.

### Request Headers

| Header | Value |
|--------|-------|
| `Content-Type` | `text/plain` or `application/octet-stream` |

### Request Body

File content (text or binary).

### Response

| Status | Condition |
|--------|-----------|
| 204 | Written successfully |
| 404 | Parent directory not found |
| 500 | Write error |

---

## Create File or Directory

```
POST /zosmf/restfiles/fs/{path}
```

Create a new USS file or directory.

### Request Body (JSON)

```json
{
  "type": "file",
  "mode": "rwxr-xr-x"
}
```

For a directory:
```json
{
  "type": "directory",
  "mode": "rwxr-xr-x"
}
```

### Response

| Status | Condition |
|--------|-----------|
| 201 | Created |
| 409 | Already exists |
| 500 | Creation error |

---

## Delete File or Directory

```
DELETE /zosmf/restfiles/fs/{path}
```

Delete a USS file or directory.

### Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `recursive` | boolean | Delete directory recursively (default: `false`) |

### Response

| Status | Condition |
|--------|-----------|
| 204 | Deleted |
| 404 | Not found |
| 500 | Deletion error |

---

## Rename, Chmod, Chown

```
PUT /zosmf/restfiles/fs/{path}
```

Perform file operations using the `X-IBM-Option` header.

### Rename

```bash
curl -X PUT https://host:port/zosmf/restfiles/fs/u/ibmuser/old.txt \
  -H "X-IBM-Option: rename" \
  -H "Content-Type: application/json" \
  -d '{"newname": "/u/ibmuser/new.txt"}'
```

### Change Permissions (chmod)

```bash
curl -X PUT https://host:port/zosmf/restfiles/fs/u/ibmuser/myfile.txt \
  -H "X-IBM-Option: chmod" \
  -H "Content-Type: application/json" \
  -d '{"mode": "755"}'
```

### Change Ownership (chown)

```bash
curl -X PUT https://host:port/zosmf/restfiles/fs/u/ibmuser/myfile.txt \
  -H "X-IBM-Option: chown" \
  -H "Content-Type: application/json" \
  -d '{"owner": "IBMUSER", "group": "SYS1"}'
```

### Response

| Status | Condition |
|--------|-----------|
| 200 | Operation successful |
| 404 | File not found |
| 500 | Operation error |
"#
}

fn cics_page() -> &'static str {
    r#"# CICS Session Endpoints

The CICS REST API provides headless access to CICS terminal sessions, enabling
programmatic interaction with CICS transactions and BMS screens.

## Start New CICS Session

```
POST /zosmf/cicsApp/terminal
```

Creates a new CICS session and returns the initial screen (typically the logon screen).

### Request Body (JSON, optional)

```json
{
  "app": "CARDDEMO"
}
```

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `app` | string | Config default | CICS application name (from `zosmf.toml`) |

### Response (200 OK)

```json
{
  "sessionKey": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
  "screen": {
    "rows": 24,
    "cols": 80,
    "cursor": { "row": 10, "col": 25 },
    "fields": [
      {
        "row": 1,
        "col": 1,
        "text": "CardDemo Application",
        "attr": "protected"
      },
      {
        "row": 10,
        "col": 25,
        "text": "",
        "attr": "unprotected",
        "name": "USRIDINI"
      },
      {
        "row": 12,
        "col": 25,
        "text": "",
        "attr": "unprotected,dark",
        "name": "PASSWDI"
      }
    ]
  }
}
```

### Screen Field Attributes
| Attribute | Description |
|-----------|-------------|
| `protected` | Field cannot be modified by user |
| `unprotected` | Field accepts user input |
| `dark` | Non-display field (passwords) |
| `bright` | High-intensity display |
| `numeric` | Numeric-only input |

---

## Send Input

```
PUT /zosmf/cicsApp/terminal/{sessionKey}
```

Send an AID key press and field data to the CICS session. Returns the resulting screen.

### Path Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `sessionKey` | string | Session identifier from POST response |

### Request Body (JSON)

```json
{
  "aid": "Enter",
  "fields": {
    "USRIDINI": "IBMUSER",
    "PASSWDI": "SYS1"
  }
}
```

### AID Key Values
| Value | Key | Description |
|-------|-----|-------------|
| `Enter` | Enter | Submit current screen |
| `PF1` - `PF24` | PF keys | Program function keys |
| `PA1` - `PA3` | PA keys | Program attention keys |
| `Clear` | Clear | Clear screen |

### Response (200 OK)

Same format as the POST response -- returns the new screen state.

```json
{
  "sessionKey": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
  "screen": {
    "rows": 24,
    "cols": 80,
    "cursor": { "row": 5, "col": 1 },
    "fields": [
      {
        "row": 1,
        "col": 1,
        "text": "CardDemo Main Menu",
        "attr": "protected"
      }
    ]
  }
}
```

### Error Responses

| Status | Condition |
|--------|-----------|
| 404 | Session not found (expired or invalid key) |
| 500 | CICS execution error |

---

## Read Current Screen

```
GET /zosmf/cicsApp/terminal/{sessionKey}
```

Read the current screen state without sending any input. Useful for polling
screen updates or recovering the display after a network interruption.

### Path Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `sessionKey` | string | Session identifier |

### Response (200 OK)

Same screen format as POST and PUT responses.

### Error Responses

| Status | Condition |
|--------|-----------|
| 404 | Session not found |

---

## Terminate Session

```
DELETE /zosmf/cicsApp/terminal/{sessionKey}
```

End the CICS session and release all associated resources.

### Path Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `sessionKey` | string | Session identifier |

### Response

| Status | Condition |
|--------|-----------|
| 200 | Session terminated |
| 404 | Session not found |

### Response Body (200)

```json
{
  "message": "Session terminated"
}
```

## Session Timeout

Sessions automatically expire after the configured timeout period (default: 1800
seconds / 30 minutes). Expired sessions return 404 on subsequent requests.

Configure via `zosmf.toml`:
```toml
[cics]
session_timeout_seconds = 1800
```
"#
}
