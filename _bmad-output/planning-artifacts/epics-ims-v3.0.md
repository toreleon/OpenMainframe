# IMS Crate — Epics & Stories

## Epic 400: EXEC DLI Preprocessor

**Goal:** Implement the EXEC DLI preprocessor to translate embedded IMS commands in COBOL to CBLTDLI CALL statements.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-400

### Story 400.1: EXEC DLI Block Extraction

As a **developer**,
I want **the preprocessor to scan COBOL source and extract EXEC DLI blocks**,
So that **embedded IMS commands are identified for translation**.

**Acceptance Criteria:**

**Given** COBOL source containing `EXEC DLI GU USING PCB(1) SEGMENT(CUSTOMER) INTO(WS-CUST) WHERE(CUSTNO = :WS-KEY) END-EXEC`
**When** scanned by the preprocessor
**Then** the EXEC DLI block is extracted with function=GU, PCB=1, segment=CUSTOMER, into=WS-CUST, qualification CUSTNO=:WS-KEY

**Given** multiple EXEC DLI blocks in a single program
**When** scanned
**Then** all blocks are identified with correct line numbers

**Complexity:** L

### Story 400.2: CBLTDLI Call Generation

As a **developer**,
I want **EXEC DLI blocks replaced with COBOL CALL 'CBLTDLI' statements**,
So that **the translated program can execute DL/I calls at runtime**.

**Acceptance Criteria:**

**Given** `EXEC DLI GU USING PCB(1) SEGMENT(CUSTOMER) INTO(WS-CUST) WHERE(CUSTNO = :WS-KEY) END-EXEC`
**When** translated
**Then** the output is `CALL 'CBLTDLI' USING DLI-GU, PCB-001, WS-CUST, CUST-SSA-001` with SSA layout generated in WORKING-STORAGE

**Given** `EXEC DLI SCHD PSB(MYPSB) END-EXEC`
**When** translated
**Then** the output is `CALL 'CBLTDLI' USING DLI-SCHD, IO-PCB-MASK, PSB-NAME`

**Complexity:** L

### Story 400.3: DIB (DL/I Interface Block) Generation

As a **developer**,
I want **the preprocessor to generate a DIB copybook for EXEC DLI status checking**,
So that **programs can check DIBSTAT after each DL/I call**.

**Acceptance Criteria:**

**Given** a COBOL program using EXEC DLI
**When** preprocessed
**Then** a DIB copybook with DIBSTAT, DIBSEGM, DIBKFBL fields is generated

**Complexity:** S

---

## Epic 401: System Service Calls

**Goal:** Implement CHKP, ROLB, SYNC, and related system service calls for transaction integrity.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-401

### Story 401.1: CHKP (Checkpoint) Call

As an **IMS developer**,
I want **CHKP to commit all database changes and record a checkpoint**,
So that **data integrity is maintained and restart is possible**.

**Acceptance Criteria:**

**Given** a program has inserted segments and issues CHKP
**When** CHKP executes via the I/O PCB
**Then** all changes are committed, checkpoint ID is recorded, and status is blank (success)

**Given** the program abends after CHKP
**When** the program is restarted
**Then** all changes up to the last CHKP are preserved

**Complexity:** L

### Story 401.2: ROLB (Rollback) Call

As an **IMS developer**,
I want **ROLB to undo all changes since the last commit point**,
So that **I can back out a failed transaction**.

**Acceptance Criteria:**

**Given** a program inserts 3 segments, then issues ROLB
**When** ROLB executes
**Then** the 3 inserts are undone and the database is in pre-insert state

**Given** a program issues CHKP, then inserts 2 segments, then issues ROLB
**When** ROLB executes
**Then** only the 2 segments after CHKP are undone; earlier changes remain

**Complexity:** L

### Story 401.3: SYNC and Other Service Calls

As an **IMS developer**,
I want **SYNC, SETS, ROLS, LOG, and STAT system service calls**,
So that **programs have full transaction control and diagnostic capabilities**.

**Acceptance Criteria:**

**Given** `CALL 'CBLTDLI' USING DLI-SYNC, IO-PCB`
**When** executed
**Then** all changes since last commit point are committed (like CHKP without checkpoint ID)

**Given** `CALL 'CBLTDLI' USING DLI-LOG, IO-PCB, LOG-RECORD`
**When** executed
**Then** the log record is written to the IMS log

**Complexity:** M

---

## Epic 402: I/O PCB and Message Handling Foundation

**Goal:** Implement the I/O PCB as the foundation for system service calls and future IMS/TM support.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-403

### Story 402.1: I/O PCB Implementation

As a **developer**,
I want **the I/O PCB automatically created as PCB index 0 when a PSB is scheduled**,
So that **system service calls have the required PCB**.

**Acceptance Criteria:**

**Given** a PSB with 2 DB PCBs
**When** scheduled
**Then** the PSB has 3 PCBs: I/O PCB at index 0, DB PCBs at indexes 1 and 2

**Given** the I/O PCB
**When** inspected after a DL/I call
**Then** it contains status code, date, time, user ID, and terminal name fields

**Complexity:** M

### Story 402.2: Basic Message Queue Operations

As an **IMS developer**,
I want **GU to I/O PCB to read input messages and ISRT to I/O PCB to write output messages**,
So that **MPP-style message processing is supported**.

**Acceptance Criteria:**

**Given** a message queued for a transaction
**When** the program issues GU against the I/O PCB
**Then** the message is delivered to the I/O area

**Given** the program issues ISRT against the I/O PCB with response data
**When** executed
**Then** the response is queued for the originating terminal

**Complexity:** L

---

## Epic 403: Database Persistence

**Goal:** Enable IMS database data to persist across program invocations via PostgreSQL.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-402

### Story 403.1: PersistentStore Trait

As a **developer**,
I want **a store trait that abstracts in-memory vs PostgreSQL storage**,
So that **the runtime works with either storage backend**.

**Acceptance Criteria:**

**Given** the `PersistentStore` trait with methods for insert, get, find, delete, update
**When** implemented by `HierarchicalStore` (in-memory)
**Then** all existing tests pass unchanged

**Given** the `PersistentStore` trait
**When** implemented by `PostgresStore`
**Then** operations persist to the PostgreSQL database using the generated schema

**Complexity:** L

### Story 403.2: PostgreSQL Store Implementation

As a **developer**,
I want **a PostgresStore that uses the generated schema for persistent storage**,
So that **IMS data survives across program invocations and restarts**.

**Acceptance Criteria:**

**Given** a DBD with segments CUSTOMER → ORDER → ITEM
**When** the PostgresStore is initialized
**Then** the required tables (segments, hierarchy, per-segment typed tables) are created if not existing

**Given** a GU call with SSA for a specific customer
**When** executed against PostgresStore
**Then** the query uses the closure table for efficient hierarchical retrieval

**Complexity:** XL

---

## Epic 404: GSAM Database Support

**Goal:** Implement GSAM sequential file I/O within IMS programs.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-404

### Story 404.1: GSAM Read and Write Operations

As an **IMS developer**,
I want **GN and ISRT on GSAM PCBs to perform sequential file I/O**,
So that **batch programs can read and write sequential files using DL/I calls**.

**Acceptance Criteria:**

**Given** a GSAM database opened for input
**When** GN is issued
**Then** the next record is read from the sequential file into the I/O area

**Given** a GSAM database opened for output
**When** ISRT is issued
**Then** the record from the I/O area is written to the sequential file

**Given** all records have been read
**When** GN is issued
**Then** status code GB (end of database) is returned

**Complexity:** M

---

## Epic 405: Comprehensive Status Codes

**Goal:** Expand the status code set to cover all commonly encountered IMS DL/I codes.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-405

### Story 405.1: Extended Status Code Registry

As a **developer**,
I want **comprehensive DL/I status codes covering database, system service, and GSAM operations**,
So that **programs receive accurate status information**.

**Acceptance Criteria:**

**Given** the status code registry
**When** queried for common database call statuses
**Then** at least 50 codes are defined including: AA, AB, AC, AF, AH, AJ, AK, AL, AM, AO, BA, DA, DJ, DX, FD, FR, GA, GB, GD, GE, GK, GP, II, IX, NI, QC, QD, RX, TG, XD

**Given** a status code
**When** queried for its description and category
**Then** a human-readable description and category (informational, program error, I/O error) are returned

**Complexity:** M

---

## Epic 406: Secondary Index Support

**Goal:** Implement secondary indexes for alternate key access to segments.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-406

### Story 406.1: XDFLD in DBD and PROCSEQ in PCB

As an **IMS developer**,
I want **secondary indexes defined via XDFLD in DBD and selected via PROCSEQ in PCB**,
So that **I can access segments by non-sequence key fields**.

**Acceptance Criteria:**

**Given** DBD with `XDFLD NAME=CUSTNAME,SEGMENT=CUSTOMER,SRCH=CUSTNAME`
**When** a PCB specifies `PROCSEQ=CUSTNAME`
**Then** GU and GN calls navigate using the CUSTNAME index instead of the primary CUSTNO sequence

**Given** a new CUSTOMER segment is inserted
**When** it has a CUSTNAME field
**Then** the secondary index is automatically updated

**Complexity:** L

---

## Epic 407: Logical Relationships

**Goal:** Implement logical relationships between IMS databases.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-407

### Story 407.1: LCHILD Macro and Logical Navigation

As an **IMS developer**,
I want **LCHILD-defined logical relationships to enable cross-database navigation**,
So that **programs can traverse between related databases**.

**Acceptance Criteria:**

**Given** Database A with segment CUSTOMER and Database B with segment ORDER having LCHILD pointing to CUSTOMER
**When** navigating from ORDER to its logical parent CUSTOMER
**Then** the CUSTOMER segment from Database A is returned

**Given** LCHILD in DBD source
**When** parsed by the DBD parser
**Then** the logical child relationship is recorded in the DatabaseDefinition

**Complexity:** L

---

## Epic 408: Boolean SSA Qualifications

**Goal:** Implement AND/OR operators in SSA qualifications for compound search criteria.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-408

### Story 408.1: Multiple Qualification Statements

As an **IMS developer**,
I want **SSAs with AND/OR operators for compound qualifications**,
So that **I can search with multiple criteria in a single SSA**.

**Acceptance Criteria:**

**Given** SSA `CUSTOMER(CUSTNO >= 10000 *AND CUSTNO <= 20000)`
**When** evaluated
**Then** only customers with CUSTNO between 10000 and 20000 are matched

**Given** SSA `ORDER(STATUS = OPEN *OR STATUS = PENDING)`
**When** evaluated
**Then** orders with status OPEN or PENDING are matched

**Complexity:** M

---

## Epic 409: Segment Data Extraction

**Goal:** Implement structured access to segment data using field definitions.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-409

### Story 409.1: Field-Level Data Access

As an **IMS developer**,
I want **segment data parsed into typed fields using DBD field definitions**,
So that **I can work with structured data instead of raw bytes**.

**Acceptance Criteria:**

**Given** a CUSTOMER segment with raw bytes and fields CUSTNO(1,10,C), CUSTNAME(11,30,C), BALANCE(41,8,P)
**When** the segment data is extracted
**Then** CUSTNO returns a 10-byte character string, CUSTNAME returns 30 bytes, BALANCE returns a packed decimal value

**Given** typed field values
**When** packed back into a segment buffer
**Then** the resulting byte buffer matches the field layout defined in the DBD

**Complexity:** M

---

## Epic 410: DBD Parser Enhancements

**Goal:** Add DATASET, LCHILD, and variable-length segment support to the DBD parser.

**Crate:** `open-mainframe-ims`
**FRs:** FR-v3.0-410

### Story 410.1: DATASET and LCHILD Macros

As a **developer**,
I want **the DBD parser to handle DATASET and LCHILD macros**,
So that **physical storage and logical relationship definitions are captured**.

**Acceptance Criteria:**

**Given** `DATASET DD1=CUSTDD,DEVICE=3390,SIZE=(4096)`
**When** parsed
**Then** the database definition includes dataset DD name, device type, and block size

**Given** `LCHILD NAME=(ORDLINE,ORDERDB),POINTER=SNGL`
**When** parsed
**Then** the logical child relationship is recorded with target segment, target database, and pointer type

**Complexity:** M

### Story 410.2: Variable-Length Segment Support

As a **developer**,
I want **variable-length segments with COMPRTN and min/max bytes**,
So that **space-efficient segment storage is supported**.

**Acceptance Criteria:**

**Given** `SEGM NAME=REMARK,BYTES=(200,50),COMPRTN=CMPRTN01`
**When** parsed
**Then** the segment has max_bytes=200, min_bytes=50, variable_length=true, comprtn=CMPRTN01

**Complexity:** S
