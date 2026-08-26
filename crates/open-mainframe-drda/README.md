# open-mainframe-drda

`open-mainframe-drda` implements the Distributed Relational Database Architecture (DRDA) Application Server (AS) wire protocol over TCP. It allows DB2 client tools, ODBC/CLI drivers (such as `ibm_db`), and the Zowe CLI (`zowe db2 execute sql`) to connect directly to the OpenMainframe environment.

## Purpose

DRDA is IBM's standard protocol for relational database interoperability across z/OS, IBM i, and distributed platforms. This crate models the DRDA Application Server role by listening on a TCP socket (default port 50000), performing the DRDA connection handshake with optional Diffie-Hellman encrypted authentication, and routing SQL requests (`EXCSQLIMM`, `OPNQRY`, `PRPSQLSTT`, etc.) to the underlying database engine.

## Capabilities

- **DSS Frame Transport**: Parses and constructs 6-byte Data Stream Structure (DSS) headers, supporting Request (`0x01`), Reply (`0x02`), and Object (`0x03`) streams, segment chaining, continuation frames, and correlation ID tracking.
- **DDM Command & Object Parsing**: Decodes and serializes Distributed Data Management (DDM) objects and parameter lists.
- **4-Phase Connection Handshake**:
  1. `EXCSAT` / `EXCSATRD`: Server attribute exchange (product ID `SQL11050`, manager levels).
  2. `ACCSEC` / `ACCSECRD`: Security mechanism negotiation (`SECMEC_USRIDPWD` `0x0003` and `SECMEC_EUSRIDPWD` `0x0009`).
  3. `SECCHK` / `SECCHKRM`: User authentication via pluggable validator callback (`AuthFn`).
  4. `ACCRDB` / `ACCRDBRM`: Relational database access validation and session initialization.
- **Encrypted Authentication (SECMEC 0x0009)**: Implements Diffie-Hellman 256-bit key exchange (`EusridpwdState`) using standard DRDA prime and generator constants, deriving DES/CBC/PKCS5 keys and IVs to decrypt EBCDIC-encoded user credentials.
- **SQL Statement Execution**:
  - `EXCSQLIMM` (Execute Immediate) for DDL and DML (`INSERT`, `UPDATE`, `DELETE`) returning row-count `SQLCARD` responses.
  - `OPNQRY`, `CNTQRY`, and `CLSQRY` for cursor-based `SELECT` query execution and row pagination.
  - `PRPSQLSTT` (Prepare SQL Statement), `EXCSQLSTT` (Execute Prepared Statement), and `EXCSQLSET`.
  - `RDBCMM` (Commit) and `RDBRLLBCK` (Rollback) transaction demarcation.
- **FD:OCA Serialization**: Serializes `SQLDARD` column metadata and `QRYDSC`/`QRYDTA` binary row data across 10+ Formatted Data Object Content Architecture (FD:OCA) types: `Varchar`, `FixedChar`, `SmallInt`, `Integer`, `BigInt`, `Decimal`, `Float`, `Date`, `Time`, `Timestamp`, and their nullable counterparts.
- **EBCDIC / ASCII Handling**: Decodes incoming EBCDIC (cp037 / cp500) parameter bytes and encodes server response text.

## Architecture

```text
  Client (ODBC / ibm_db / Zowe CLI)
                 │
            TCP (port 50000)
                 ▼
        ┌──────────────────┐
        │   DrdaServer     │ (server.rs)
        │  (tokio listener)│
        └────────┬─────────┘
                 │ spawns per connection
                 ▼
        ┌──────────────────┐
        │  RequestHandler  │ (handler.rs)
        └────────┬─────────┘
                 │
      ┌──────────┴──────────────────────────┐
      ▼                                     ▼
┌─────────────────────────┐       ┌─────────────────────────┐
│   ConnectionHandler     │       │       QueryState        │
│   (connection.rs)       │       │    (sql_handler.rs)     │
│                         │       │                         │
│ • EXCSAT / ACCSEC       │       │ • EXCSQLIMM / OPNQRY    │
│ • SECCHK / ACCRDB       │       │ • PRPSQLSTT / EXCSQLSTT │
│ • secmec9.rs (DH + DES) │       │ • Cursor Management     │
└─────────────┬───────────┘       └────────────┬────────────┘
              │                                │
              └───────────────┬────────────────┘
                              ▼
                      ┌───────────────┐
                      │  response.rs  │ (SQLCARD / SQLDARD / QRYDTA)
                      │    dss.rs     │ (DSS segment builder)
                      └───────────────┘
```

### Module Structure

| Module | Description |
|---|---|
| `lib` | Crate root re-exporting `DrdaServerConfig`, `AuthFn`, `start_server`, `DrdaError`, and `DrdaResult`. |
| `server` | Asynchronous Tokio TCP server accepting incoming connections and managing connection worker tasks. |
| `handler` | `RequestHandler` dispatching incoming DSS payloads to handshake or query processors based on connection state. |
| `connection` | `ConnectionHandler` state machine (`Initial` → `Exchanged` → `SecurityNegotiated` → `Authenticated` → `Ready`). |
| `sql_handler` | `QueryState` and `QueryCursor` managing open result sets, prepared statements, and SQL statement dispatch. |
| `secmec9` | Diffie-Hellman key exchange and DES/CBC decryption routines for SECMEC 0x0009 (`EUSRIDPWD`). |
| `dss` | Data Stream Structure parser and serializer (`read_dss`, `write_dss`, `write_dss_chain`). |
| `ddm` | Distributed Data Management parameter parser and object builder (`DdmObject`, `DdmBuilder`). |
| `code_points` | Numeric constants for DRDA code points, DSS types, security mechanisms, and FD:OCA type identifiers. |
| `response` | DDM response builders for `SQLCARD`, `SQLDARD`, `QRYDSC`, `QRYDTA`, `ACCSECRD`, `SECCHKRM`, and `ACCRDBRM`. |
| `error` | `DrdaError` enumeration for I/O, framing, protocol, authentication, and database lookup failures. |

## Public API

### Configuration and Server Startup

```rust
use std::sync::Arc;
use open_mainframe_drda::{start_server, AuthFn, DrdaServerConfig};

#[derive(Debug, Clone)]
pub struct DrdaServerConfig {
    pub enabled: bool,
    pub host: String,
    pub port: u16,
    pub database: String,
    pub location: String,
}

pub type AuthFn = Arc<dyn Fn(&str, &str) -> bool + Send + Sync>;

pub async fn start_server(
    config: DrdaServerConfig,
    auth_fn: AuthFn,
) -> open_mainframe_drda::DrdaResult<()>;
```

### Protocol State Types

- `RequestHandler`: Per-connection request coordinator owning a `ConnectionHandler` and `QueryState`.
- `ConnectionState`: Connection lifecycle enum (`Initial`, `Exchanged`, `SecurityNegotiated`, `Authenticated`, `Ready`).
- `QueryState`: Cursor and prepared statement tracker.
- `DrdaError`: Strongly-typed error enum implementing `std::error::Error` and `thiserror`.

## Integration and Consumers

### Workspace Dependencies

- [`open-mainframe-db2`](../open-mainframe-db2/README.md) — DB2 SQL parsing, execution models, and data types.
- [`open-mainframe-racf`](../open-mainframe-racf/README.md) — User authentication and security validation.

### Known Consumers

- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — The `zosmf-server` binary starts `open_mainframe_drda::start_server` as a background Tokio task during startup when DRDA support is enabled.

## Examples

### Starting a DRDA Server Instance

```rust
use std::sync::Arc;
use open_mainframe_drda::{start_server, DrdaServerConfig};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = DrdaServerConfig {
        enabled: true,
        host: "0.0.0.0".to_string(),
        port: 50000,
        database: "DSN1".to_string(),
        location: "OPENMF".to_string(),
    };

    // Authenticate against RACF or custom user directory
    let auth_fn = Arc::new(|user: &str, pass: &str| {
        user.eq_ignore_ascii_case("IBMUSER") && pass == "SYS1"
    });

    start_server(config, auth_fn).await?;
    Ok(())
}
```

### Executing SQL via Zowe CLI

Once the server is running on port 50000:

```bash
zowe db2 execute sql "SELECT EMPNO, FIRSTNME, LASTNAME FROM DSN1.EMP" \
    --host 127.0.0.1 \
    --port 50000 \
    --database DSN1 \
    --user IBMUSER \
    --password SYS1
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-drda
```

The wire protocol handlers are tested against simulated DSS stream chains and validated via integration scripts with client drivers.

## Limitations

- **Standalone SQL Response Generation**: In the absence of an attached DB2 storage catalog or table schema, certain immediate queries rely on built-in mock responses.
- **Legacy Encryption Standard**: SECMEC 0x0009 implements 56-bit single-DES with 256-bit Diffie-Hellman key exchange as mandated by the original DRDA architecture specification, rather than modern TLS transport encryption.
- **Scrollable Cursors**: Query continuation (`CNTQRY`) implements forward-only cursor fetching.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [DB2 Precompiler and Engine (`open-mainframe-db2`)](../open-mainframe-db2/README.md)
- [RACF Security Subsystem (`open-mainframe-racf`)](../open-mainframe-racf/README.md)
- [z/OSMF Server (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
