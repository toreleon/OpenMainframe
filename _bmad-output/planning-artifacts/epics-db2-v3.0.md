# DB2 Crate — Epics & Stories

## Epic 300: Dynamic SQL Support

**Goal:** Implement PREPARE, EXECUTE, EXECUTE IMMEDIATE, and DESCRIBE for runtime SQL construction.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-300

### Story 300.1: PREPARE and EXECUTE Statements

As a **DB2 developer**,
I want **PREPARE to compile a SQL string into a named statement and EXECUTE to run it**,
So that **programs can construct and execute SQL dynamically**.

**Acceptance Criteria:**

**Given** `EXEC SQL PREPARE STMT1 FROM :SQL-TEXT END-EXEC`
**When** SQL-TEXT contains 'SELECT * FROM EMP WHERE DEPT = ?'
**Then** STMT1 is prepared and available for EXECUTE

**Given** `EXEC SQL EXECUTE STMT1 USING :DEPT-NO END-EXEC`
**When** DEPT-NO = 'D01'
**Then** the query executes with DEPT = 'D01'

**Complexity:** L

### Story 300.2: EXECUTE IMMEDIATE

As a **DB2 developer**,
I want **EXECUTE IMMEDIATE for one-shot dynamic SQL**,
So that **simple dynamic statements don't require separate PREPARE/EXECUTE**.

**Acceptance Criteria:**

**Given** `EXEC SQL EXECUTE IMMEDIATE :DDL-TEXT END-EXEC`
**When** DDL-TEXT contains 'CREATE TABLE TEMP1 (COL1 INTEGER)'
**Then** the table is created and SQLCODE = 0

**Complexity:** M

### Story 300.3: Preprocessor Recognition of Dynamic SQL

As a **developer**,
I want **the preprocessor to recognize PREPARE, EXECUTE, EXECUTE IMMEDIATE, DESCRIBE**,
So that **dynamic SQL statements generate correct CALL replacements**.

**Acceptance Criteria:**

**Given** `EXEC SQL PREPARE S1 FROM :SQL-VAR END-EXEC`
**When** preprocessed
**Then** statement type is Prepare with statement name S1 and source variable SQL-VAR

**Complexity:** M

---

## Epic 301: WHENEVER Error Handling

**Goal:** Implement the WHENEVER statement for declarative error handling in the preprocessor.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-301

### Story 301.1: WHENEVER Preprocessor State Machine

As a **DB2 developer**,
I want **WHENEVER SQLERROR/SQLWARNING/NOT FOUND to insert automatic error checks**,
So that **every SQL statement is followed by appropriate error handling jumps**.

**Acceptance Criteria:**

**Given** `EXEC SQL WHENEVER SQLERROR GO TO ERR-PARA END-EXEC`
**When** a subsequent EXEC SQL SELECT is preprocessed
**Then** the generated code includes `IF SQLCODE < 0 GO TO ERR-PARA` after the CALL

**Given** `EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC`
**When** a subsequent EXEC SQL FETCH is preprocessed
**Then** no NOT FOUND check is generated after the CALL

**Complexity:** M

### Story 301.2: WHENEVER Scope and Override

As a **DB2 developer**,
I want **WHENEVER settings to persist until overridden by another WHENEVER**,
So that **error handling applies to all subsequent SQL statements in the compilation unit**.

**Acceptance Criteria:**

**Given** WHENEVER SQLERROR GO TO ERR1 at line 100
**When** WHENEVER SQLERROR GO TO ERR2 appears at line 200
**Then** SQL between lines 100-199 jumps to ERR1; SQL after line 200 jumps to ERR2

**Complexity:** S

---

## Epic 302: PostgreSQL Runtime Connectivity

**Goal:** Wire the tokio-postgres dependency for actual database execution.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-302

### Story 302.1: Live SQL Execution

As a **developer**,
I want **the runtime to execute translated SQL against a real PostgreSQL database**,
So that **DB2 COBOL programs can operate on actual data**.

**Acceptance Criteria:**

**Given** a PostgreSQL database running with connection config provided
**When** `EXEC SQL SELECT COUNT(*) INTO :WS-COUNT FROM EMPLOYEE END-EXEC` executes
**Then** WS-COUNT contains the actual row count from the EMPLOYEE table

**Given** the database is unreachable
**When** a SQL statement executes
**Then** SQLCODE = -904 (resource unavailable) is set in SQLCA

**Complexity:** L

### Story 302.2: Result Set to COBOL Variable Mapping

As a **developer**,
I want **PostgreSQL result rows mapped to COBOL host variables**,
So that **fetched data populates the correct COBOL fields with proper type conversion**.

**Acceptance Criteria:**

**Given** a FETCH that returns a DECIMAL(9,2) column
**When** the host variable is PIC S9(7)V99 COMP-3
**Then** the value is correctly converted and stored

**Given** a VARCHAR(100) column with 50 characters of data
**When** the host variable is PIC X(100)
**Then** the value is left-justified and padded with spaces

**Complexity:** L

### Story 302.3: PostgreSQL Error to DB2 SQLCODE Mapping

As a **developer**,
I want **PostgreSQL errors mapped back to DB2 SQLCODE values**,
So that **COBOL programs see familiar DB2 error codes**.

**Acceptance Criteria:**

**Given** PostgreSQL returns error 23505 (unique_violation)
**When** mapped to DB2
**Then** SQLCODE = -803 (duplicate key) and SQLSTATE = '23505'

**Given** PostgreSQL returns error 42P01 (undefined_table)
**When** mapped to DB2
**Then** SQLCODE = -204 (name not found) and SQLSTATE = '42704'

**Complexity:** M

---

## Epic 303: Scrollable Cursors

**Goal:** Implement DECLARE CURSOR WITH SCROLL and directional FETCH operations.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-303

### Story 303.1: Scrollable Cursor Declaration and FETCH Directions

As a **DB2 developer**,
I want **scrollable cursors with FETCH FIRST, LAST, PRIOR, ABSOLUTE, RELATIVE**,
So that **I can navigate result sets bidirectionally**.

**Acceptance Criteria:**

**Given** `EXEC SQL DECLARE C1 SCROLL CURSOR FOR SELECT * FROM EMP END-EXEC`
**When** FETCH FIRST, FETCH LAST, FETCH PRIOR are issued
**Then** the cursor positions correctly and returns the appropriate row

**Given** `EXEC SQL FETCH ABSOLUTE 5 C1 INTO :WS-REC END-EXEC`
**When** the result set has 10 rows
**Then** the 5th row is returned

**Complexity:** M

---

## Epic 304: Savepoint Support

**Goal:** Implement SAVEPOINT, RELEASE SAVEPOINT, and ROLLBACK TO SAVEPOINT.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-304

### Story 304.1: Savepoint Operations

As a **DB2 developer**,
I want **savepoints for partial transaction rollback**,
So that **I can undo part of a transaction without losing all work**.

**Acceptance Criteria:**

**Given** `EXEC SQL SAVEPOINT SP1 ON ROLLBACK RETAIN CURSORS END-EXEC`
**When** followed by INSERT and then ROLLBACK TO SAVEPOINT SP1
**Then** the INSERT is undone but prior work in the transaction is retained

**Given** `EXEC SQL RELEASE SAVEPOINT SP1 END-EXEC`
**When** executed
**Then** SP1 is no longer available for rollback

**Complexity:** M

### Story 304.2: Preprocessor Recognition of Savepoint Statements

As a **developer**,
I want **the preprocessor to recognize SAVEPOINT, RELEASE SAVEPOINT, ROLLBACK TO SAVEPOINT**,
So that **these statements generate correct runtime calls**.

**Acceptance Criteria:**

**Given** `EXEC SQL SAVEPOINT SP1 ON ROLLBACK RETAIN CURSORS END-EXEC`
**When** preprocessed
**Then** statement type is Savepoint with name SP1 and retain_cursors=true

**Complexity:** S

---

## Epic 305: Comprehensive SQLCODE/SQLSTATE Mapping

**Goal:** Expand the SQLCODE registry from ~11 to ~100 common codes with bidirectional PostgreSQL mapping.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-305

### Story 305.1: Extended SQLCODE Definitions

As a **developer**,
I want **comprehensive SQLCODE/SQLSTATE definitions covering common DB2 conditions**,
So that **error handling is accurate and IBM-compatible**.

**Acceptance Criteria:**

**Given** the SQLCODE registry
**When** queried for common error categories (constraint violations, authorization, data conversion, deadlock, capacity)
**Then** at least 80 SQLCODEs are defined with message text and SQLSTATE

**Complexity:** M

### Story 305.2: Bidirectional PostgreSQL Error Mapping

As a **developer**,
I want **PostgreSQL SQLSTATE values mapped to DB2 SQLCODEs and vice versa**,
So that **errors translate correctly across the DB2-to-PostgreSQL bridge**.

**Acceptance Criteria:**

**Given** a PostgreSQL SQLSTATE '23503' (foreign_key_violation)
**When** mapped
**Then** DB2 SQLCODE = -530 (referential integrity violation)

**Given** a DB2 SQLCODE -551 (authorization failure)
**When** executed against PostgreSQL
**Then** the corresponding PostgreSQL error is recognized and mapped back

**Complexity:** M

---

## Epic 306: Indicator Variables

**Goal:** Implement indicator variable support for NULL handling in embedded SQL.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-306

### Story 306.1: Indicator Variable Parsing and Runtime

As a **DB2 developer**,
I want **indicator variables to detect and set NULL values**,
So that **I can properly handle nullable columns in COBOL**.

**Acceptance Criteria:**

**Given** `EXEC SQL FETCH C1 INTO :WS-NAME :WS-NAME-IND END-EXEC`
**When** the NAME column is NULL
**Then** WS-NAME-IND = -1 and WS-NAME is unchanged

**Given** `EXEC SQL INSERT INTO EMP (NAME) VALUES (:WS-NAME :WS-NAME-IND) END-EXEC`
**When** WS-NAME-IND = -1
**Then** NULL is inserted for the NAME column

**Complexity:** M

---

## Epic 307: Multi-Row FETCH

**Goal:** Implement FETCH ... FOR n ROWS for bulk data retrieval.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-307

### Story 307.1: Multi-Row FETCH INTO Host Variable Arrays

As a **DB2 developer**,
I want **FETCH ... FOR n ROWS to retrieve multiple rows in one call**,
So that **batch processing is more efficient**.

**Acceptance Criteria:**

**Given** `EXEC SQL FETCH C1 FOR 10 ROWS INTO :WS-ARRAY END-EXEC`
**When** the cursor has 25 rows remaining
**Then** 10 rows are fetched, SQLERRD(3) = 10

**Given** the cursor has 3 rows remaining
**When** FETCH FOR 10 ROWS executes
**Then** 3 rows are fetched, SQLERRD(3) = 3, SQLCODE = 100

**Complexity:** L

---

## Epic 308: SQL Dialect Translation Expansion

**Goal:** Expand DB2-to-PostgreSQL SQL translation to cover additional syntax constructs.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-312

### Story 308.1: Isolation Level and Locking Translation

As a **developer**,
I want **WITH UR/CS/RS/RR and FOR UPDATE OF translated to PostgreSQL equivalents**,
So that **DB2 isolation and locking semantics are preserved**.

**Acceptance Criteria:**

**Given** `SELECT * FROM EMP WITH UR`
**When** translated
**Then** PostgreSQL receives `SELECT * FROM EMP` with transaction isolation set to READ UNCOMMITTED

**Given** `SELECT * FROM EMP FOR UPDATE OF SALARY`
**When** translated
**Then** PostgreSQL receives `SELECT * FROM EMP FOR UPDATE`

**Complexity:** M

### Story 308.2: Special Registers and Additional Functions

As a **developer**,
I want **DB2 special registers (CURRENT SQLID, CURRENT SCHEMA, CURRENT DEGREE) and functions translated**,
So that **DB2 SQL runs correctly on PostgreSQL**.

**Acceptance Criteria:**

**Given** `SET CURRENT SCHEMA = 'MYSCHEMA'`
**When** translated
**Then** PostgreSQL receives `SET search_path TO 'MYSCHEMA'`

**Given** `SELECT DIGITS(SALARY) FROM EMP`
**When** translated
**Then** PostgreSQL receives equivalent `SELECT CAST(SALARY AS TEXT) FROM EMP` (or equivalent)

**Complexity:** M

---

## Epic 309: End-to-End Integration Testing

**Goal:** Create integration tests covering the full DB2 precompiler pipeline.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-311

### Story 309.1: Preprocess → DBRM → BIND → Execute Pipeline Test

As a **developer**,
I want **end-to-end tests that exercise the entire DB2 pipeline**,
So that **the precompiler, DBRM, BIND, and runtime work together correctly**.

**Acceptance Criteria:**

**Given** a COBOL source file with EXEC SQL SELECT, INSERT, FETCH operations
**When** preprocessed, DBRM generated, BIND executed, and runtime invoked
**Then** all operations complete with correct SQLCODE values

**Given** a COBOL source with EXEC SQL errors (bad table name)
**When** executed end-to-end
**Then** appropriate SQLCODE is returned and SQLCA fields are populated

**Complexity:** L

### Story 309.2: DCLGEN Round-Trip Test

As a **developer**,
I want **DCLGEN output to be usable as input to the preprocessor**,
So that **generated copybooks work correctly in COBOL programs**.

**Acceptance Criteria:**

**Given** DCLGEN generates a copybook for table EMPLOYEE
**When** the copybook is included in a COBOL program with EXEC SQL INCLUDE
**Then** the host variables are correctly recognized by the preprocessor

**Complexity:** M

---

## Epic 310: Additional SQL Statement Support

**Goal:** Add MERGE, INSERT from SELECT, CALL, GET DIAGNOSTICS, DECLARE TABLE to the preprocessor and runtime.

**Crate:** `open-mainframe-db2`
**FRs:** FR-v3.0-308, FR-v3.0-309

### Story 310.1: DECLARE TABLE and INCLUDE SQLDA

As a **DB2 developer**,
I want **DECLARE TABLE recognized by the preprocessor**,
So that **DCLGEN-generated table declarations are properly handled**.

**Acceptance Criteria:**

**Given** `EXEC SQL DECLARE EMPLOYEE TABLE (EMPNO CHAR(6) NOT NULL, ...) END-EXEC`
**When** preprocessed
**Then** the declaration is recorded for compile-time validation (no runtime code generated)

**Complexity:** S

### Story 310.2: MERGE and INSERT from SELECT

As a **DB2 developer**,
I want **MERGE and INSERT ... SELECT supported**,
So that **modern DB2 SQL patterns work correctly**.

**Acceptance Criteria:**

**Given** `EXEC SQL MERGE INTO TGT USING SRC ON TGT.KEY = SRC.KEY WHEN MATCHED THEN UPDATE SET ... WHEN NOT MATCHED THEN INSERT ... END-EXEC`
**When** preprocessed and executed
**Then** the MERGE operates correctly (translated to PostgreSQL's INSERT ... ON CONFLICT)

**Complexity:** M

### Story 310.3: CALL for Stored Procedures

As a **DB2 developer**,
I want **CALL statement for invoking stored procedures**,
So that **programs can use server-side logic**.

**Acceptance Criteria:**

**Given** `EXEC SQL CALL MYPROC(:PARAM1, :PARAM2) END-EXEC`
**When** executed
**Then** the stored procedure is invoked with the provided parameters

**Complexity:** M
