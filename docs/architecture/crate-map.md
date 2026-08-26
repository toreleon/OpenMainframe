# Workspace Crate Map

This document catalogs all 44 crates in the OpenMainframe workspace. The crate
count and dependency relationships are derived directly from Cargo workspace
metadata (`cargo metadata --format-version 1 --no-deps`).

Every workspace member is listed exactly once, grouped by functional
responsibility, with links to its crate-level documentation, implemented role,
direct internal workspace dependencies, and primary workspace consumers.

This is a current-state Cargo map, not a retention decision. Declared dependency
does not prove source use, and a crate with no incoming workspace dependency may
still be a valid binary, plugin, or independent product. Target profiles,
mechanical edge verification, ownership, and retirement are governed by
[Workspace Convergence and Sustainable Architecture](workspace-convergence.md).

---

## Summary by Responsibility Group

| Responsibility Group | Crate Count | Focus Area |
|---|---|---|
| [Foundation & Encodings](#foundation--encodings) | 3 | Core AST, diagnostics, EBCDIC encoding, and Language Environment (LE) runtime |
| [Compilers, Interpreters, & Precompilers](#compilers-interpreters--precompilers) | 11 | COBOL, JCL, Assembler, PL/I, REXX, CLIST, 4GLs, precompilers, and formal verification |
| [Data, Storage, & Databases](#data-storage--databases) | 7 | Datasets (QSAM/VSAM/PDS), DFSORT, DB2 SQL, DRDA wire server, IMS, IDMS, and ADABAS |
| [Subsystems & System Services](#subsystems--system-services) | 16 | CICS, JES2, RACF, TSO, ISPF, MQ, MVS, WLM, SMF, USS, PARMLIB, utilities, and networking |
| [Operations, API, UI, & Evaluation](#operations-api-ui--evaluation) | 7 | z/OSMF REST server, 3270 TUI, CLI integration, deployment, gym evaluation, and wiki generator |
| **Total** | **44** | **Full z/OS emulation and tooling ecosystem** |

---

## Foundation & Encodings

| Crate | Implemented Role | Direct Internal Dependencies | Primary Consumers |
|---|---|---|---|
| [`open-mainframe-lang-core`](../../crates/open-mainframe-lang-core/README.md) | Shared AST nodes, source span management, miette diagnostic reporting, and compiler preprocessor traits. | None | `open-mainframe-cobol`, `open-mainframe-jcl`, `open-mainframe-symbolic` |
| [`open-mainframe-encoding`](../../crates/open-mainframe-encoding/README.md) | EBCDIC character conversion across 21 code pages, packed decimal (COMP-3), zoned decimal, and binary numeric encodings. | None | `open-mainframe`, `open-mainframe-cics`, `open-mainframe-cobol`, `open-mainframe-dataset`, `open-mainframe-ims`, `open-mainframe-jcl`, `open-mainframe-runtime`, `open-mainframe-sort`, `open-mainframe-wiki`, `open-mainframe-zosmf` |
| [`open-mainframe-runtime`](../../crates/open-mainframe-runtime/README.md) | Language Environment (LE) memory management, condition handling, COBOL variable values, and tree-walking interpreter execution loop. | `open-mainframe-encoding`, `open-mainframe-sort` | `open-mainframe`, `open-mainframe-cobol`, `open-mainframe-tui`, `open-mainframe-zosmf` |

---

## Compilers, Interpreters, & Precompilers

| Crate | Implemented Role | Direct Internal Dependencies | Primary Consumers |
|---|---|---|---|
| [`open-mainframe-cobol`](../../crates/open-mainframe-cobol/README.md) | Enterprise COBOL compiler front-end with an 8-stage pipeline (options, conditionals, copybook expansion, replacement, lexing, parsing, semantics, and optional LLVM code generation) and 77+ intrinsic functions. | `open-mainframe-encoding`, `open-mainframe-lang-core`, `open-mainframe-runtime` | `open-mainframe`, `open-mainframe-assess`, `open-mainframe-symbolic`, `open-mainframe-tui`, `open-mainframe-wiki` |
| [`open-mainframe-precompilers`](../../crates/open-mainframe-precompilers/README.md) | Source-to-source precompilers transforming embedded `EXEC SQL` (DB2) and `EXEC CICS` statements into standard COBOL CALL statements with host variable structures. | `open-mainframe-cics`, `open-mainframe-db2` | `open-mainframe` |
| [`open-mainframe-jcl`](../../crates/open-mainframe-jcl/README.md) | JCL parser, cataloged procedure (PROC) expander, symbol substitution engine, condition evaluation (`COND`/`IF`), and batch step execution runner. | `open-mainframe-dataset`, `open-mainframe-encoding`, `open-mainframe-lang-core`, `open-mainframe-sort`, `open-mainframe-utilities` | `open-mainframe-wiki`, `open-mainframe-zosmf` |
| [`open-mainframe-hlasm`](../../crates/open-mainframe-hlasm/README.md) | High Level Assembler lexer, parser, macro definition/expansion engine, conditional assembly, symbol resolution, and z/Architecture instruction encoding. | None | *Standalone / Tooling* |
| [`open-mainframe-pli`](../../crates/open-mainframe-pli/README.md) | Enterprise PL/I lexer, context-sensitive parser, preprocessor, attribute evaluation, built-in function library, and runtime interpreter. | None | *Standalone / Tooling* |
| [`open-mainframe-rexx`](../../crates/open-mainframe-rexx/README.md) | REXX language lexer, recursive descent parser, arbitrary precision decimal interpreter, compound variable arrays (`STEM.`), and built-in functions. | None | `open-mainframe-tso` |
| [`open-mainframe-clist`](../../crates/open-mainframe-clist/README.md) | CLIST procedural command interpreter supporting control flow, positional/keyword parameters, built-in functions, variable substitution, and statement execution. | None | *Standalone / Tooling* |
| [`open-mainframe-easytrieve`](../../crates/open-mainframe-easytrieve/README.md) | Easytrieve Plus compiler and execution engine: data definitions, report generation, match-file processing, and summary calculations. | None | *Standalone / Tooling* |
| [`open-mainframe-natural`](../../crates/open-mainframe-natural/README.md) | Software AG Natural 4GL parser and execution engine with database simulation (ADABAS/SQL), interactive maps, and reporting. | None | *Standalone / Tooling* |
| [`open-mainframe-focus`](../../crates/open-mainframe-focus/README.md) | Information Builders FOCUS 4GL reporting engine, TABLE/GRAPH request processor, Dialogue Manager, and data adapter interfaces. | None | *Standalone / Tooling* |
| [`open-mainframe-symbolic`](../../crates/open-mainframe-symbolic/README.md) | Symbolic execution and bounded verification engine for COBOL programs using a pure-Rust constraint solver for path exploration and model generation. | `open-mainframe-cobol`, `open-mainframe-lang-core` | *Standalone / Tooling* |

---

## Data, Storage, & Databases

| Crate | Implemented Role | Direct Internal Dependencies | Primary Consumers |
|---|---|---|---|
| [`open-mainframe-dataset`](../../crates/open-mainframe-dataset/README.md) | Comprehensive dataset management: ICF Catalog, sequential I/O (QSAM/BSAM), partitioned datasets (PDS/PDSE), VSAM (KSDS, ESDS, RRDS), and IDCAMS utility. | `open-mainframe-encoding` | `open-mainframe-cics`, `open-mainframe-gym`, `open-mainframe-jcl`, `open-mainframe-mvs`, `open-mainframe-wiki`, `open-mainframe-zosmf` |
| [`open-mainframe-sort`](../../crates/open-mainframe-sort/README.md) | DFSORT-compatible sort, merge, and copy engine with multi-key sorting, record selection (`INCLUDE`/`OMIT`), and field reformatting (`INREC`/`OUTREC`). | `open-mainframe-encoding` | `open-mainframe-jcl`, `open-mainframe-runtime` |
| [`open-mainframe-db2`](../../crates/open-mainframe-db2/README.md) | DB2 relational SQL database engine: SQL lexer/parser, catalog metadata storage, table storage, DDL/DML processing, and BIND package management. | None | `open-mainframe-drda`, `open-mainframe-precompilers`, `open-mainframe-zosmf` |
| [`open-mainframe-drda`](../../crates/open-mainframe-drda/README.md) | DRDA (Distributed Relational Database Architecture) wire protocol server over TCP (port 50000) for standard DB2 clients and ODBC drivers. | `open-mainframe-db2`, `open-mainframe-racf` | `open-mainframe-zosmf` |
| [`open-mainframe-ims`](../../crates/open-mainframe-ims/README.md) | IMS (Information Management System) hierarchical database engine supporting DL/I calls (GU, GN, GNP, ISRT, DLET, REPL) and transaction management. | `open-mainframe-encoding` | *Standalone / Tooling* |
| [`open-mainframe-idms`](../../crates/open-mainframe-idms/README.md) | CA IDMS CODASYL network database engine: schema/subschema definitions, DML navigational verbs (OBTAIN, FIND, STORE), and DMCL buffer management. | None | *Standalone / Tooling* |
| [`open-mainframe-adabas`](../../crates/open-mainframe-adabas/README.md) | Software AG ADABAS inverted-list database engine: FDT (Field Definition Table) management, search/read buffers, and ISN (Internal Sequence Number) indexing. | None | *Standalone / Tooling* |

---

## Subsystems & System Services

| Crate | Implemented Role | Direct Internal Dependencies | Primary Consumers |
|---|---|---|---|
| [`open-mainframe-cics`](../../crates/open-mainframe-cics/README.md) | CICS transaction processing engine: BMS map parsing and 3270 screen rendering, 30+ EXEC CICS runtime verbs, TS/TD queues, and file control. | `open-mainframe-dataset`, `open-mainframe-encoding` | `open-mainframe`, `open-mainframe-precompilers`, `open-mainframe-tui`, `open-mainframe-wiki`, `open-mainframe-zosmf` |
| [`open-mainframe-jes2`](../../crates/open-mainframe-jes2/README.md) | JES2 (Job Entry Subsystem 2) batch lifecycle: job submission, priority queues, state transitions, initiator tracking, and multi-stream spool dataset management. | None | `open-mainframe-tso`, `open-mainframe-zosmf` |
| [`open-mainframe-racf`](../../crates/open-mainframe-racf/README.md) | RACF security database, user/group management, password verification, SAF router integration, and multi-level resource access control checks. | None | `open-mainframe-drda`, `open-mainframe-mvs`, `open-mainframe-zosmf` |
| [`open-mainframe-tso`](../../crates/open-mainframe-tso/README.md) | TSO/E interactive command processor, line-mode session lifecycle, ALLOCATE/FREE dataset management, and CLIST/REXX command execution. | `open-mainframe-jes2`, `open-mainframe-rexx` | `open-mainframe-zosmf` |
| [`open-mainframe-ispf`](../../crates/open-mainframe-ispf/README.md) | ISPF dialog manager: panel parsing, variable pools, dynamic formatting, screen navigation, table services, and text editing operations. | None | *Standalone / Tooling* |
| [`open-mainframe-mq`](../../crates/open-mainframe-mq/README.md) | IBM MQ queue manager: Message Queue Interface (MQI) verbs (MQCONN, MQOPEN, MQPUT, MQGET, MQCLOSE), local/model queues, and MQSC command parsing. | None | *Standalone / Tooling* |
| [`open-mainframe-mvs`](../../crates/open-mainframe-mvs/README.md) | MVS supervisor services: dynamic allocation (SVC 99 / DYNALLOC), operator messages (WTO/WTOR), resource serialization (ENQ/DEQ), and recovery (ESTAE). | `open-mainframe-dataset`, `open-mainframe-racf` | *Standalone / Tooling* |
| [`open-mainframe-wlm`](../../crates/open-mainframe-wlm/README.md) | z/OS Workload Manager: goal-oriented resource policies, service class management, workload classification rules, and resource pool monitoring. | None | `open-mainframe-zosmf` |
| [`open-mainframe-smf`](../../crates/open-mainframe-smf/README.md) | System Management Facilities: binary SMF record generation (Type 30, Type 80, Type 110, Type 120), in-memory buffers, and auditing trails. | None | `open-mainframe-zosmf` |
| [`open-mainframe-uss`](../../crates/open-mainframe-uss/README.md) | z/OS UNIX System Services: POSIX-compliant hierarchical file system (zFS), directory permissions, process management, and shell execution environment. | None | *Standalone / Tooling* |
| [`open-mainframe-utilities`](../../crates/open-mainframe-utilities/README.md) | Standard z/OS batch utilities: dataset manipulation (IEBCOPY, IEBGENER, IEBUPDTE, IEBCOMPR, IEBPTPCH), test data generation (IEBDG), and system utilities (IEFBR14, IKJEFT01). | None | `open-mainframe-jcl` |
| [`open-mainframe-syscmd`](../../crates/open-mainframe-syscmd/README.md) | z/OS System Operator Commands and SDSF display engine: command dispatching (DISPLAY, START, STOP, VARY) and tabular monitoring panels. | None | *Standalone / Tooling* |
| [`open-mainframe-pgmmgmt`](../../crates/open-mainframe-pgmmgmt/README.md) | Program Management: Binder (linkage editor), Object Module (OBJ) and Load Module (LMOD) parser/formatter, and Program Manager loader. | None | *Standalone / Tooling* |
| [`open-mainframe-networking`](../../crates/open-mainframe-networking/README.md) | Mainframe networking: VTAM, SNA session management, TCP/IP socket abstractions, AT-TLS security, FTP server, and SSH/TN3270 emulation. | None | *Standalone / Tooling* |
| [`open-mainframe-crypto`](../../crates/open-mainframe-crypto/README.md) | ICSF cryptographic services: symmetric ciphers (AES, DES, 3DES), asymmetric algorithms (RSA, ECC), secure hashing (SHA-1, SHA-256), and key storage. | None | *Standalone / Tooling* |
| [`open-mainframe-parmlib`](../../crates/open-mainframe-parmlib/README.md) | System PARMLIB concatenation manager, static/dynamic system symbol substitution (`&SYSNAME`, `&SYSR1`), and initialization parameter parsing. | None | `open-mainframe-zosmf` |

---

## Operations, API, UI, & Evaluation

| Crate | Implemented Role | Direct Internal Dependencies | Primary Consumers |
|---|---|---|---|
| [`open-mainframe-zosmf`](../../crates/open-mainframe-zosmf/README.md) | Axum-based z/OS Management Facility (z/OSMF) REST server with registered route families for datasets, jobs, TSO, console, USS files, DB2, WLM, CICS, and compatibility services. | `open-mainframe`, `open-mainframe-cics`, `open-mainframe-dataset`, `open-mainframe-db2`, `open-mainframe-drda`, `open-mainframe-encoding`, `open-mainframe-jcl`, `open-mainframe-jes2`, `open-mainframe-parmlib`, `open-mainframe-racf`, `open-mainframe-runtime`, `open-mainframe-smf`, `open-mainframe-tso`, `open-mainframe-tui`, `open-mainframe-wlm` | `open-mainframe-gym` |
| [`open-mainframe`](../../crates/open-mainframe/README.md) | Workspace CLI binary (`open-mainframe`) and shared integration library (`open_mainframe_lib`) connecting the COBOL compiler, runtime, and CICS bridge. | `open-mainframe-cics`, `open-mainframe-cobol`, `open-mainframe-encoding`, `open-mainframe-precompilers`, `open-mainframe-runtime`, `open-mainframe-tui`, `open-mainframe-wiki` | `open-mainframe-zosmf` |
| [`open-mainframe-tui`](../../crates/open-mainframe-tui/README.md) | Full-screen IBM 3270 terminal user interface built on `ratatui` and `crossterm`, supporting Model 2-5 screen dimensions and AID key handling. | `open-mainframe-cics`, `open-mainframe-cobol`, `open-mainframe-runtime` | `open-mainframe`, `open-mainframe-zosmf` |
| [`open-mainframe-gym`](../../crates/open-mainframe-gym/README.md) | In-process evaluation harness for training and benchmarking agentic coding systems against z/OSMF interfaces and mainframe workloads. | `open-mainframe-dataset`, `open-mainframe-zosmf` | *Evaluation Harness* |
| [`open-mainframe-assess`](../../crates/open-mainframe-assess/README.md) | Mainframe code portfolio assessment engine: cyclomatic complexity, Halstead metrics, maintainability index, and migration readiness scoring. | `open-mainframe-cobol` | `open-mainframe-wiki` |
| [`open-mainframe-deploy`](../../crates/open-mainframe-deploy/README.md) | Deployment artifacts and orchestrator helpers for containerizing OpenMainframe workloads in Docker, Kubernetes, and Helm environments. | None | *Deployment Tooling* |
| [`open-mainframe-wiki`](../../crates/open-mainframe-wiki/README.md) | Documentation generator creating cross-linked Markdown wikis and Mermaid architecture diagrams from COBOL/JCL/CICS source repositories. | `open-mainframe-assess`, `open-mainframe-cics`, `open-mainframe-cobol`, `open-mainframe-dataset`, `open-mainframe-encoding`, `open-mainframe-jcl` | `open-mainframe` |

---

## Related Documentation

- [Architecture Overview](overview.md) — System boundaries, layers, and major runtime flows.
- [Getting Started](../guides/getting-started.md) — Build, run, and verify the local server.
- [Configuration Reference](../reference/configuration.md) — Server TOML settings and options.
- [z/OSMF API Reference](../reference/zosmf-api.md) — Complete REST endpoint mapping.
