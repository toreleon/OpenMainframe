# IMS Crate — Architecture Decisions

## AD-3.0-01: EXEC DLI Preprocessor Architecture

**Context:** IMS COBOL programs use either `EXEC DLI ... END-EXEC` blocks (translated by a preprocessor) or direct `CALL 'CBLTDLI'` statements. The current `preprocess/mod.rs` is an empty stub. The preprocessor must scan COBOL source, extract EXEC DLI blocks, and generate CBLTDLI CALL statements with proper parameter layout.

**Decision:** Implement the EXEC DLI preprocessor following the same architecture as the EXEC SQL (DB2) and EXEC CICS preprocessors already in the workspace. The preprocessor scans COBOL fixed-form source (columns 7-72), identifies `EXEC DLI` through `END-EXEC` blocks, classifies the DL/I function (GU, GN, ISRT, SCHD, CHKP, etc.), extracts SSA strings and host variable references, and replaces the block with `CALL 'CBLTDLI' USING function-code, pcb-mask, io-area, ssa1, ssa2, ...`. Also generate the DIB (DL/I Interface Block) copybook as the EXEC DLI equivalent of SQLCA.

**Consequences:**
- Consistent preprocessor architecture across CICS, DB2, and IMS crates
- EXEC DLI syntax is simpler than EXEC CICS (fewer command variations)
- Must handle SCHD (schedule PSB) which has no CBLTDLI equivalent in batch IMS
- DIB generation provides DIBSTAT for status checking
- Programs using CALL 'CBLTDLI' directly bypass the preprocessor entirely

## AD-3.0-02: System Service Calls via I/O PCB

**Context:** IMS system service calls (CHKP, ROLB, SYNC, XRST, ROLS, SETS, LOG, STAT) all require the I/O PCB, which is always the first PCB in the PSB. The current implementation has no I/O PCB concept — only DB and GSAM PCB types exist at runtime (Alt and Io are defined as enum variants but not used).

**Decision:** Implement the I/O PCB as a special `ProgramCommBlock` with `pcb_type: PcbType::Io`. When a PSB is scheduled, an I/O PCB is automatically created as PCB index 0 (matching IBM behavior). System service calls dispatch through the I/O PCB. CHKP commits all changes and records a checkpoint ID. ROLB undoes changes since last CHKP/SYNC. SYNC commits without a checkpoint ID. For the initial implementation, commit/rollback affect the in-memory store via a write-ahead log pattern (snapshot before changes, restore on ROLB).

**Consequences:**
- PCB index 0 is always the I/O PCB (existing tests use index 0 for DB PCB — need migration)
- CHKP/ROLB require tracking changes since last commit point
- In-memory rollback requires either copy-on-write snapshots or an undo log
- XRST (extended restart) requires persistent checkpoint data — defer to persistence epic

## AD-3.0-03: Database Persistence via PostgreSQL

**Context:** The crate already generates PostgreSQL schemas (closure table pattern) from DBD definitions. However, the runtime uses a purely in-memory `HierarchicalStore` with no persistence. Real IMS databases are disk-resident and survive across transactions and system restarts.

**Decision:** Create a `PersistentStore` trait that both `HierarchicalStore` (in-memory) and a new `PostgresStore` implement. The `ImsRuntime` is parameterized by store type. `PostgresStore` uses the generated schema to persist segments via `tokio-postgres`. The in-memory store remains the default for tests and mock mode. A `sync_to_postgres()` method on `HierarchicalStore` enables bulk export.

**Consequences:**
- Store trait abstracts the storage layer — runtime logic stays unchanged
- PostgreSQL persistence adds async I/O (tokio) dependency
- Requires connection management similar to DB2 crate's approach
- Record locking semantics change with database-backed storage
- Initial population from PostgreSQL to in-memory cache enables fast reads

## AD-3.0-04: GSAM as Sequential File Adapter

**Context:** GSAM (Generalized Sequential Access Method) databases provide sequential record I/O within IMS programs. They are used for batch input/output files. The `PcbType::Gsam` variant exists but has no I/O implementation.

**Decision:** Implement GSAM as a thin adapter over standard file I/O. A `GsamDatabase` struct wraps a `BufReader`/`BufWriter` for the backing file. GN reads the next record (fixed-length or variable-length). ISRT writes the next record. The GSAM PCB tracks the current record position (RSA — Record Search Argument). The file path is provided when the database is opened (analogous to DD card allocation in JCL).

**Consequences:**
- Simple implementation — GSAM is essentially sequential file I/O with IMS status code conventions
- Must handle end-of-file (GB status), I/O errors (AO status)
- Variable-length records need a length prefix convention
- Checkpoint/restart must save and restore file position (RSA)

## AD-3.0-05: Secondary Index as Inverted Index

**Context:** IMS secondary indexes provide alternate key access to segments. A secondary index is itself a database with index pointer segments. The XDFLD macro in the target DBD defines which field is indexed; the PROCSEQ option on the PCB selects the secondary index as the processing sequence.

**Decision:** Implement secondary indexes as an in-memory `HashMap<Vec<u8>, Vec<u64>>` (key → list of segment IDs) maintained alongside the primary `HierarchicalStore`. When PROCSEQ is specified on a PCB, GU/GN operations use the secondary index for retrieval instead of the primary hierarchy. Index maintenance (insert, delete, update) happens automatically when segments are modified.

**Consequences:**
- Index maintenance adds overhead to ISRT/REPL/DLET operations
- Multiple secondary indexes can exist for a database
- Must handle non-unique secondary indexes (multiple segments with same key)
- PROCSEQ changes the "logical" hierarchy as seen by the program
- Index-only retrieval is not supported (always fetches the full segment)

## AD-3.0-06: Boolean SSA Qualifications

**Context:** IBM IMS supports multiple qualification statements in a single SSA using `*AND` and `*OR` connectors. The current SSA parser only handles a single qualification per SSA.

**Decision:** Extend `SsaQualification` to support a list of qualification clauses connected by AND/OR operators. The parser recognizes `*AND` and `*OR` tokens within the parenthesized qualification. The `matches()` method evaluates the boolean expression. AND has higher precedence than OR (matching IBM behavior).

**Consequences:**
- SSA parsing becomes more complex but still manageable
- Evaluation short-circuits for performance
- Must handle mixed AND/OR with correct precedence
- The `to_string()` method must reconstruct the multi-qualification format
