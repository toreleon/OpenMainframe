# DB2 SQL Preprocessing Crate — Product Requirements

## Overview

The `open-mainframe-db2` crate provides DB2 SQL preprocessing and runtime support for the OpenMainframe platform. It includes an EXEC SQL preprocessor that extracts embedded SQL from COBOL source, generates Database Request Modules (DBRMs), produces SQLCA copybooks, and provides a runtime layer that translates DB2 SQL to PostgreSQL for execution. It also includes BIND and DCLGEN utilities.

## Current State Assessment

- **Lines of code:** 5,461
- **Test count:** 105 (all passing)
- **Maturity:** Moderate
- **Files:** 16 Rust source files across 4 modules (preprocess, runtime, utilities, lib)

### What Works Well

**Preprocessing:**
- EXEC SQL block extraction from COBOL source (fixed-form column 7-72)
- SQL statement classification: SELECT, INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, CLOSE, FETCH, INCLUDE, COMMIT, ROLLBACK
- Host variable extraction (:VAR pattern recognition)
- DBRM generation with SQL statements, host variables, and consistency tokens (JSON format)
- SQLCA copybook generation (01 SQLCA with SQLCODE, SQLSTATE, SQLERRMC, SQLERRD, SQLWARN fields)
- COBOL CALL statement generation replacing EXEC SQL blocks
- Comment-line and string-literal handling during scanning

**Runtime:**
- Connection management with PostgreSQL connection string generation
- Environment variable configuration (DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD)
- Cursor operations: DECLARE, OPEN, FETCH (single row), CLOSE, WITH HOLD support
- SQL execution with parameter binding (:VAR → $1/$2/$3 positional parameters)
- Mock execution mode for testing without a database
- Transaction management: COMMIT, ROLLBACK, auto-commit mode
- SQL dialect translation: FETCH FIRST n ROWS → LIMIT, CONCAT() → ||, SUBSTR → SUBSTRING, VALUE → COALESCE, CURRENT DATE/TIME/TIMESTAMP
- DB2 type mapping: 18 types including INTEGER, DECIMAL, VARCHAR, DATE, TIME, TIMESTAMP, BLOB, CLOB
- COBOL PIC clause to DB2 type inference
- SQLCA runtime with SQLCODE/SQLSTATE setting and ~11 common SQLCODE definitions
- Result set to COBOL field mapping

**Utilities:**
- BIND: Reads DBRM, generates prepared statements, produces bind output
- DCLGEN: Generates COBOL copybooks from table column definitions, PIC clause mapping for all supported types

### What Does NOT Work

- No Dynamic SQL (PREPARE, EXECUTE, EXECUTE IMMEDIATE, DESCRIBE)
- No WHENEVER statement for declarative error handling
- No scrollable cursors (SCROLL, FETCH FIRST/LAST/PRIOR/ABSOLUTE/RELATIVE)
- No savepoints (SAVEPOINT, RELEASE SAVEPOINT, ROLLBACK TO SAVEPOINT)
- No DECLARE TABLE statement (used for compile-time type checking)
- No multi-row FETCH (FETCH ... FOR n ROWS)
- No INSERT from SELECT or INSERT with multiple rows
- No MERGE statement
- No CALL statement for stored procedures
- No actual PostgreSQL connectivity (mock mode only — tokio-postgres is a dependency but not wired)
- Incomplete SQLCODE/SQLSTATE mapping (~11 of hundreds of codes)
- No SQLWARNO through SQLWARN7 runtime behavior
- No DECFLOAT, ROWID, XML, BINARY, VARBINARY types
- No indicator variables for NULL handling
- No PL/I host language support (stub only)
- No EXPLAIN support
- Hardcoded timestamps in DBRM and BIND output
- No INCLUDE SQLDA
- No GET DIAGNOSTICS statement
- No SIGNAL/RESIGNAL statements
- No LABEL ON, COMMENT ON DDL
- No GRANT/REVOKE authorization
- No end-to-end integration tests (preprocessor → DBRM → BIND → execute)

## Functional Requirements

### FR-v3.0-300: Dynamic SQL Support
Implement PREPARE, EXECUTE, EXECUTE IMMEDIATE, and DESCRIBE statements for dynamic SQL. Support parameter markers (?) and USING DESCRIPTOR for dynamic parameter passing.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 Application Programming and SQL Guide, Chapter 26 — "Coding dynamic SQL in application programs." Dynamic SQL is essential for programs that construct SQL at runtime (e.g., query tools, ad-hoc reporting).

### FR-v3.0-301: WHENEVER Statement
Implement the WHENEVER statement for declarative error handling: WHENEVER SQLERROR, WHENEVER SQLWARNING, WHENEVER NOT FOUND with CONTINUE or GO TO actions.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 SQL Reference, WHENEVER statement. Nearly all production DB2 COBOL programs use WHENEVER for error handling flow control.

### FR-v3.0-302: PostgreSQL Runtime Connectivity
Wire the existing tokio-postgres dependency to actually connect to and execute SQL against a PostgreSQL database. Currently all execution is mock-only.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation
- **IBM Reference:** While IBM DB2 runs natively on z/OS, the project's architecture decision is to translate DB2 SQL to PostgreSQL. The runtime must actually execute translated SQL.

### FR-v3.0-303: Scrollable Cursors
Implement DECLARE CURSOR WITH SCROLL, and FETCH with FIRST, LAST, PRIOR, ABSOLUTE n, RELATIVE n positioning. Support SENSITIVE and INSENSITIVE scroll options.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 SQL Reference, DECLARE CURSOR with SCROLL. Used for bidirectional result set navigation in interactive applications.

### FR-v3.0-304: Savepoint Support
Implement SAVEPOINT, RELEASE SAVEPOINT, and ROLLBACK TO SAVEPOINT for partial transaction rollback.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 SQL Reference. Savepoints enable fine-grained transaction control without rolling back the entire unit of work.

### FR-v3.0-305: Comprehensive SQLCODE/SQLSTATE Mapping
Expand SQLCODE definitions from ~11 to cover all commonly encountered codes. Map DB2 SQLCODEs to PostgreSQL error codes bidirectionally.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** DB2 12 Messages and Codes Reference — hundreds of SQLCODEs. Critical for accurate error reporting and WHENEVER handling.

### FR-v3.0-306: Indicator Variables for NULL Handling
Implement indicator variable support (:HOST-VAR :INDICATOR-VAR) for detecting and setting NULL values in SQL operations.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 Application Programming Guide, Chapter 7 — "Using indicator variables." Standard mechanism for NULL handling in embedded SQL.

### FR-v3.0-307: Multi-Row FETCH
Implement FETCH ... FOR n ROWS INTO :host-variable-array for bulk data retrieval.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 SQL Reference. Multi-row FETCH significantly improves performance for batch processing.

### FR-v3.0-308: DECLARE TABLE Statement
Implement DECLARE TABLE for compile-time SQL validation and documentation of table structures.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DB2 12 SQL Reference. Used by the precompiler for type checking; commonly generated by DCLGEN alongside the host variable copybook.

### FR-v3.0-309: Additional SQL Statements
Implement MERGE, INSERT from SELECT, CALL (stored procedures), GET DIAGNOSTICS, and SIGNAL statements.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Modern DB2 applications use MERGE for upsert operations, CALL for stored procedure invocation, and GET DIAGNOSTICS for detailed error information.

### FR-v3.0-310: Extended Type Support
Add DECFLOAT (16 and 34 digit), ROWID, XML, BINARY, and VARBINARY types to the type system, SQL translation, and DCLGEN output.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** DB2 12 supports these types. DECFLOAT is increasingly used for financial calculations requiring exact decimal representation.

### FR-v3.0-311: End-to-End Integration Testing
Create integration tests that exercise the full pipeline: COBOL source → preprocess → DBRM → BIND → runtime execute → verify results.
- **Priority:** MAJOR
- **Gap Type:** Missing tests
- **IBM Reference:** The DB2 precompiler pipeline (DSNHPC → DBRM → BIND PLAN/PACKAGE → execute) is the standard DB2 application workflow. End-to-end testing validates the entire chain.

### FR-v3.0-312: SQL Dialect Translation Expansion
Expand DB2-to-PostgreSQL translation for: CURRENT SQLID, CURRENT SCHEMA, OPTIMIZE FOR n ROWS, WITH UR/CS/RS/RR isolation, FOR UPDATE OF, ORDER BY with column numbers, DISTINCT types, special registers.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** DB2 12 SQL Reference includes many syntax constructs not yet translated. Incomplete translation causes runtime errors.
