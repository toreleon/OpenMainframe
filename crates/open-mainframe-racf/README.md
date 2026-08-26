# open-mainframe-racf

A high-performance Rust implementation of the **IBM RACF (Resource Access Control Facility)** security subsystem for the OpenMainframe project. This crate provides user and group management, multi-level resource authorization, digital certificate handling, and PassTicket generation, adhering to the z/OS SAF (System Authorization Facility) model.

## Purpose

RACF is the premier security server on IBM z/OS mainframes, providing centralized identification, authentication, and authorization services across datasets, subsystems, and general resources. `open-mainframe-racf` models this subsystem within OpenMainframe:
1. **SAF Router**: Implements the `RACROUTE` dispatcher interface (`AUTH`, `VERIFY`, `EXTRACT`, `LIST`), decoupling callers from internal security logic.
2. **Security Database**: Manages user profiles, group hierarchies, dataset profiles (discrete and generic), and general resource classes (`FACILITY`, `SURROGAT`, `PTKTDATA`, `DIGTCERT`, etc.).
3. **Authorization Engine**: Evaluates user attributes (`SPECIAL`, `OPERATIONS`), group connections, access control lists (ACLs), Universal Access (`UACC`), and generic pattern specificity.
4. **Authentication & PassTickets**: Verifies credentials and generates time-bounded, application-specific IBM DES PassTickets for secure single sign-on.

## Capabilities

- **System Authorization Facility (SAF) Router (`SafRouter`)**:
  - Centralized decision router for `AUTH` checks against dataset and general resource classes.
  - Returns standard SAF return codes and RACF reason codes (`Authorized`, `NotAuthorized`, `ResourceNotProtected`, `UserNotIdentified`).
- **Resource Authorization & Generic Matching**:
  - Full support for RACF generic pattern rules (`*` for single qualifier match, `**` for multi-qualifier spanning, `%` for single character).
  - Specificity scoring algorithm ensures the most specific matching profile takes precedence over generic definitions.
  - Hierarchical permission ladder: `None` < `Execute` < `Read` < `Update` < `Control` < `Alter`.
- **User and Group Management**:
  - `UserProfile` attributes: `SPECIAL`, `OPERATIONS`, `AUDITOR`, default group, password hashes, and revocation dates.
  - `GroupProfile` with superior group hierarchies and user connect attributes.
- **PassTicket Engine (`AuthService`)**:
  - IBM-compliant DES PassTicket generation and validation algorithms based on secret application keys and time-window evaluation (10-minute validity).
- **System Options (`Setropts`)**:
  - Class activation/deactivation, RACLIST in-memory profile caching, global generic profile options, and password syntax rules.
- **Multi-Level Security (MLS)**:
  - Security label (`SECLABEL`) verification and Bell-LaPadula dominance checking.
- **Digital Certificates & Keyrings**:
  - Certificate repository, keyring associations, and trust chain validation.
- **Unload Utilities**:
  - Implementation of `IRRDBU00` database unload formats for security auditing and reporting.

## Architecture

```text
    Application / Subsystem               RACF Security Subsystem
    ┌────────────────────┐                ┌────────────────────────┐
    │  Security Request  │    SAF Route   │    SAF Router          │
    │  (RACROUTE AUTH)   │ ─────────────> │    (SafRouter)         │
    └────────────────────┘    Interface   └────────────────────────┘
                                                       │
    ┌────────────────────┐                ┌────────────────────────┐
    │  Administrative    │    Management  │    Profile Matcher     │
    │  Commands          │ ─────────────> │    Discrete/Generic    │
    └────────────────────┘    Commands    │  Specificity Scoring   │
                                          └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Credential Store  │ <── Auth ───── │    Authentication Mgr  │
    │  Passwords, Keys   │                │    PassTickets (DES)   │
    └────────────────────┘                └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Database / JSON   │ <── Storage ── │    RacfDatabase        │
    │  IRRDBU00 Unload   │                │    Profiles, ACLs      │
    └────────────────────┘                └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `saf` | SAF Router: `SafRouter`, `RACROUTE` request dispatching, and return code generation. |
| `database` | Security database: `RacfDatabase`, profile repositories, CRUD operations, and JSON serialization. |
| `resource` | Access evaluation: ACL lookup, generic pattern matching, specificity scoring, and UACC checks. |
| `auth` | Authentication: `AuthService`, password verification, hashing, and PassTicket algorithms. |
| `profile` | Data models: `UserProfile`, `GroupProfile`, `DatasetProfile`, `GeneralProfile`, and ACL entries. |
| `types` | Core security types: `AccessLevel`, `SafResponse`, `AuthResult`, `UserAttributes`, `PassTicketKey`. |
| `setropts` | Global security settings: `Setropts`, active classes, RACLIST caching, and global audit flags. |
| `certificate`| PKI services: `CertificateManager`, digital certificates, keyrings, and certificate associations. |
| `seclabel` | Mandatory Access Control: Security labels, dominance checking, and MLS policies. |
| `dataset` | Dataset protection: High-Level Qualifier (HLQ) security and volume authorization. |
| `exits` | Security exits: Pre- and post-processing hooks for authentication and authorization events. |
| `utilities`| Subsystem tools: `IRRDBU00` database unload formatting and profile extraction. |

## Public API

### Core Types and Services

```rust
use open_mainframe_racf::{
    RacfDatabase, AccessLevel, SafResponse,
    saf::SafRouter,
    auth::AuthService,
    profile::{UserProfile, GroupProfile, DatasetProfile, GeneralProfile},
    types::{UserAttributes, PassTicketKey},
};
```

- `RacfDatabase`: Central security database containing all profiles, group trees, and access lists.
- `SafRouter`: The entry point for checking access permissions against datasets and general resource entities.
- `AuthService`: Manages user authentication, password hashing, and IBM PassTicket generation and verification.
- `AccessLevel`: Strict access hierarchy enum (`None`, `Execute`, `Read`, `Update`, `Control`, `Alter`).

## Integration

### Workspace Dependencies

- None (pure Rust library relying on standard crates: `miette`, `thiserror`, `serde`, `serde_json`, `tracing`, `chrono`, `des`, `sha2`, `rand`, `hex`).

### Known Consumers

- [`open-mainframe-drda`](../open-mainframe-drda/README.md) — Uses `AuthService` for DRDA client authentication and PassTicket verification.
- [`open-mainframe-mvs`](../open-mainframe-mvs/README.md) — Uses RACF database profiles to enforce dataset access during dynamic allocation.
- [`open-mainframe-zosmf`](../open-mainframe-zosmf/README.md) — Powers `/zosmf/restsecurity` endpoints for user authentication and authorization checks.

## Examples

### Defining Profiles and Performing Authorization Checks

```rust
use open_mainframe_racf::{RacfDatabase, AccessLevel};
use open_mainframe_racf::saf::SafRouter;

let mut db = RacfDatabase::new();

// Define user and default group
db.add_group("SYS1", "NONE", "System Group").unwrap();
db.add_user("IBMUSER", "SYS1", "System Administrator").unwrap();

// Protect datasets under SYS1.**
db.add_dataset("SYS1.**", "IBMUSER", AccessLevel::None).unwrap();

// Authorize user for READ access
let saf = SafRouter::new();
let result = saf.auth(&db, "DATASET", "SYS1.PARMLIB", "IBMUSER", AccessLevel::Read);
assert!(result.is_authorized());

// Check access for unpermitted user
db.add_user("ANON", "SYS1", "Anonymous User").unwrap();
let anon_result = saf.auth(&db, "DATASET", "SYS1.PARMLIB", "ANON", AccessLevel::Read);
assert!(!anon_result.is_authorized());
```

### Generating and Verifying IBM PassTickets

```rust
use open_mainframe_racf::auth::AuthService;

let mut auth = AuthService::new();

// Register application secret key in PTKTDATA class
let app_key = [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF];
auth.add_passticket_profile("CICSAPPL", app_key).unwrap();

// Generate PassTicket for user
let ptkt = auth.generate_passticket("IBMUSER", "CICSAPPL").unwrap();
assert_eq!(ptkt.len(), 8);

// Verify PassTicket (valid within 10-minute window)
let valid = auth.verify_passticket("IBMUSER", "CICSAPPL", &ptkt);
assert!(valid);
```

## Testing

Run tests for the crate:

```bash
cargo test -p open-mainframe-racf
```

The test suite covers:
- **`resource::*`**: Generic pattern matching (`*`, `**`, `%`), specificity scoring resolution, discrete vs generic override, and ACL evaluation.
- **`auth::*`**: Password hashing, verification, PassTicket generation against test vectors, replay rejection, and timestamp tolerance.
- **`saf::*`**: `RACROUTE` request mapping, return code formatting, and fallback handling when resources are unprotected.
- **`database::*`**: Profile CRUD operations, group hierarchy traversal, and JSON save/restore persistence.
- **`certificate::*`**: Keyring creation, digital certificate ingestion, and subject name matching.

## Limitations

- **Database Model**: Stores profiles in memory with JSON/binary file persistence rather than mainframe ICB/BAM block-structured VSAM files.
- **Hardware Cryptography**: Cryptographic operations (DES, SHA256) execute via software Rust crates rather than z/OS ICSF coprocessors.
- **Dynamic Exit DLLs**: Exit routines are compiled in-crate rather than dynamically loaded as LPA/LNKLST assembler modules.

## Related Documentation

- [Crate Map](../../docs/architecture/crate-map.md)
- [DRDA Protocol Subsystem (`open-mainframe-drda`)](../open-mainframe-drda/README.md)
- [MVS System Services (`open-mainframe-mvs`)](../open-mainframe-mvs/README.md)
- [z/OSMF REST Subsystem (`open-mainframe-zosmf`)](../open-mainframe-zosmf/README.md)
