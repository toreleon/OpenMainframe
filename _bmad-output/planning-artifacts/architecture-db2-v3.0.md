# DB2 Crate — Architecture Decisions

## AD-3.0-01: Dynamic SQL via Prepared Statement Pipeline

**Context:** DB2 dynamic SQL allows programs to construct and execute SQL at runtime using PREPARE, EXECUTE, and EXECUTE IMMEDIATE. The current preprocessor only handles static SQL (compile-time known statements). Dynamic SQL requires runtime statement preparation and parameter binding.

**Decision:** Extend the preprocessor to recognize PREPARE, EXECUTE, EXECUTE IMMEDIATE, and DESCRIBE as statement types. At runtime, PREPARE stores the SQL text in a named statement registry (`HashMap<String, PreparedDynamic>`). EXECUTE retrieves the prepared statement and executes it with optional USING parameters. EXECUTE IMMEDIATE combines prepare+execute for one-shot SQL. The runtime `SqlExecutor` gains a `prepared_dynamic` map alongside the existing cursor registry.

**Consequences:**
- Dynamic SQL statements are preprocessed differently — the SQL text is a host variable, not a literal
- Parameter markers (?) must be supported alongside named host variables (:VAR)
- DESCRIBE populates an SQLDA structure describing result columns
- The DBRM must record dynamic SQL entry points for BIND processing

## AD-3.0-02: WHENEVER as Preprocessor-Level Control Flow

**Context:** WHENEVER SQLERROR/SQLWARNING/NOT FOUND GO TO label inserts automatic error checking after every subsequent SQL statement in the compilation unit. This is a compile-time (preprocessor) feature, not a runtime feature — the precompiler inserts IF SQLCODE checks after each CALL.

**Decision:** Track WHENEVER state in the preprocessor as three slots (sqlerror, sqlwarning, not_found), each being either Continue or GoTo(label). After each EXEC SQL is replaced with a CALL, the preprocessor emits COBOL `IF SQLCODE < 0 GO TO label` (for SQLERROR), `IF SQLCODE = 100 GO TO label` (for NOT FOUND), and `IF SQLWARN0 = 'W' GO TO label` (for SQLWARNING) based on current WHENEVER state.

**Consequences:**
- WHENEVER is entirely a preprocessor feature — no runtime changes needed
- State must persist across EXEC SQL blocks within a compilation unit
- Each WHENEVER overrides the previous setting for that condition
- Generated IF statements use SQLCA fields, requiring SQLCA to be in scope

## AD-3.0-03: PostgreSQL Connectivity via tokio-postgres

**Context:** The runtime currently uses a mock execution mode. The `tokio-postgres` dependency is declared but not wired for actual database connectivity. The architecture calls for translating DB2 SQL to PostgreSQL.

**Decision:** Implement a `LiveExecutor` alongside the existing `MockExecutor`. The `LiveExecutor` uses `tokio-postgres` to connect to a PostgreSQL database, execute translated SQL, and map results back to COBOL host variables. Connection pooling uses `deadpool-postgres` or a simple connection cache. The executor mode (mock vs live) is selected at `Db2Runtime` construction time. Mock mode remains the default for tests.

**Consequences:**
- Requires async runtime (tokio) for database operations
- PostgreSQL error codes must be mapped back to DB2 SQLCODE/SQLSTATE values
- Connection lifecycle management (connect, disconnect, reconnect on error)
- Result set column types must be mapped from PostgreSQL types to DB2/COBOL types
- Integration tests require a running PostgreSQL instance (use testcontainers or skip in CI)

## AD-3.0-04: Indicator Variable Support

**Context:** DB2 uses indicator variables to handle NULL values. A host variable can be followed by an indicator variable (`:HOST-VAR :IND-VAR`). When fetching, the indicator is set to -1 for NULL, 0 for non-NULL, or a positive value for truncation. When inserting, setting the indicator to -1 sends NULL.

**Decision:** Extend host variable parsing to detect indicator variable pairs. The `HostVariable` struct gains an `Option<String>` for the indicator name. During FETCH result mapping, check for PostgreSQL NULL and set the indicator accordingly. During INSERT/UPDATE parameter binding, check the indicator value to determine whether to send NULL or the host variable value.

**Consequences:**
- Host variable extraction must handle the two-variable pattern `:VAR :IND`
- DBRM must record indicator variable associations
- Runtime parameter binding becomes conditional on indicator value
- DCLGEN should generate indicator variable declarations alongside host variables

## AD-3.0-05: Comprehensive SQLCODE Mapping Strategy

**Context:** DB2 defines hundreds of SQLCODEs (negative for errors, positive for warnings, 0 for success). The current implementation defines only ~11 codes. PostgreSQL uses a different error code system (5-character SQLSTATE strings). Bidirectional mapping is needed: DB2 SQLCODE → PostgreSQL execution → PostgreSQL error → DB2 SQLCODE.

**Decision:** Create a `SqlCodeRegistry` with two mapping tables: (1) DB2 SQLCODE ↔ SQLSTATE for the precompiler/SQLCA side, and (2) PostgreSQL SQLSTATE → DB2 SQLCODE for reverse mapping after execution. The registry is initialized with the ~100 most common SQLCODEs covering table/column not found, constraint violations, authorization, deadlock, timeout, data conversion, and capacity errors. Unmapped PostgreSQL errors map to SQLCODE -904 (resource unavailable) with the original SQLSTATE preserved in SQLCA.SQLSTATE.

**Consequences:**
- Error messages become more accurate and IBM-compatible
- WHENEVER SQLERROR/NOT FOUND behavior depends on correct SQLCODE values
- The mapping table can be extended incrementally as new codes are encountered
- Some DB2-specific SQLCODEs have no PostgreSQL equivalent and vice versa

## AD-3.0-06: Scrollable Cursor Implementation

**Context:** DB2 scrollable cursors allow bidirectional navigation through result sets. PostgreSQL supports DECLARE CURSOR WITH SCROLL natively, making translation straightforward.

**Decision:** Extend the `Cursor` struct with a `scrollable: bool` flag and a `sensitivity` enum (Insensitive, Sensitive, Asensitive). DECLARE CURSOR WITH SCROLL translates to PostgreSQL's WITH SCROLL. FETCH FIRST/LAST/PRIOR/ABSOLUTE/RELATIVE translate directly to PostgreSQL equivalents. The cursor must be declared within a transaction for scrollable operation (PostgreSQL requirement).

**Consequences:**
- Scrollable cursors require an active transaction (auto-commit must be disabled)
- Result set caching behavior differs between SENSITIVE and INSENSITIVE modes
- FETCH direction keywords must be added to the SQL translation layer
- Cursor state tracking becomes more complex (current position awareness)
