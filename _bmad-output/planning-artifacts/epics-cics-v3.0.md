# CICS Crate — Epics & Stories

## Epic 200: Channel and Container Support

**Goal:** Implement CICS channels and containers as the modern data passing mechanism.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-200

### Story 200.1: Channel and Container Data Structures

As a **CICS developer**,
I want **Channel and Container types for structured data passing**,
So that **I can pass data > 32KB between programs**.

**Acceptance Criteria:**

**Given** a new Channel named "MY-CHANNEL"
**When** I PUT CONTAINER "DATA1" with 100KB of data
**Then** the container is stored and retrievable by name

**Given** a channel with multiple containers
**When** I DELETE CONTAINER "DATA1"
**Then** only DATA1 is removed; other containers remain

**Complexity:** M

### Story 200.2: Channel Passing on LINK/XCTL/RETURN

As a **CICS developer**,
I want **channels to flow through LINK, XCTL, and RETURN**,
So that **called programs receive the channel automatically**.

**Acceptance Criteria:**

**Given** LINK PROGRAM('SUB') CHANNEL('MY-CHANNEL')
**When** SUB executes
**Then** SUB can GET CONTAINER from MY-CHANNEL

**Given** SUB returns via RETURN
**When** the caller resumes
**Then** modified containers are visible to the caller

**Complexity:** L

### Story 200.3: Preprocessor Command Types for Containers

As a **CICS developer**,
I want **PUT CONTAINER, GET CONTAINER, MOVE CONTAINER, DELETE CONTAINER recognized by the preprocessor**,
So that **EXEC CICS container commands generate correct CALL statements**.

**Acceptance Criteria:**

**Given** `EXEC CICS PUT CONTAINER('DATA1') CHANNEL('CH') FROM(WS-DATA) END-EXEC`
**When** preprocessed
**Then** command type is PutContainer with options CONTAINER, CHANNEL, FROM

**Complexity:** S

---

## Epic 201: TD Queue Command Integration

**Goal:** Wire TD queue operations to the preprocessor and runtime dispatch.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-201

### Story 201.1: READQ TD and WRITEQ TD Command Types

As a **CICS developer**,
I want **READQ TD and WRITEQ TD recognized as command types**,
So that **EXEC CICS TD queue commands are preprocessed correctly**.

**Acceptance Criteria:**

**Given** `EXEC CICS WRITEQ TD QUEUE('CSSL') FROM(LOG-REC) END-EXEC`
**When** preprocessed
**Then** command type is WriteqTd with QUEUE and FROM options

**Given** `EXEC CICS READQ TD QUEUE('CSSL') INTO(LOG-REC) END-EXEC`
**When** preprocessed
**Then** command type is ReadqTd with QUEUE and INTO options

**Complexity:** S

### Story 201.2: DCT Configuration for TD Queues

As a **system administrator**,
I want **a Destination Control Table (DCT) to configure TD queue properties**,
So that **queue behavior matches production CICS configuration**.

**Acceptance Criteria:**

**Given** a DCT entry: QUEUE=CSSL, TYPE=INTRA, TRIGGER=100, TRANSID=CSSLRPT
**When** the 100th record is written to CSSL
**Then** transaction CSSLRPT is triggered

**Given** a DCT entry: QUEUE=EXTQ, TYPE=EXTRA, DSN=/data/output.dat
**When** WRITEQ TD writes to EXTQ
**Then** the record is appended to /data/output.dat

**Complexity:** M

---

## Epic 202: Persistent File Storage

**Goal:** Replace in-memory file storage with disk-backed VSAM persistence.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-203

### Story 202.1: PersistentFileManager with Dataset Integration

As a **CICS developer**,
I want **file operations to persist to disk via VSAM datasets**,
So that **data survives across transactions and program invocations**.

**Acceptance Criteria:**

**Given** a CICS file CUSTFILE backed by dataset MY.CUST.KSDS
**When** WRITE adds a record
**Then** the record is persisted to the KSDS dataset file on disk

**Given** a subsequent READ with the same key
**When** executed in a new transaction
**Then** the previously written record is returned

**Complexity:** L

### Story 202.2: File Control Table (FCT) Configuration

As a **system administrator**,
I want **a File Control Table to define CICS file properties and dataset mappings**,
So that **file definitions are externally configurable**.

**Acceptance Criteria:**

**Given** FCT entry: FILE=CUSTFILE, DSNAME=MY.CUST.KSDS, RLENGTH=200, KLENGTH=8, ACCESS=READ/UPDATE/ADD
**When** the CICS runtime initializes
**Then** CUSTFILE is available with the specified properties

**Complexity:** M

---

## Epic 203: CONVERSE Command and Terminal Improvements

**Goal:** Implement CONVERSE and fix terminal screen size handling.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-202, FR-v3.0-208

### Story 203.1: CONVERSE Command

As a **CICS developer**,
I want **CONVERSE to combine SEND and RECEIVE in one command**,
So that **terminal I/O is more efficient**.

**Acceptance Criteria:**

**Given** `EXEC CICS CONVERSE FROM(WS-OUT) INTO(WS-IN) MAXLENGTH(80) END-EXEC`
**When** executed
**Then** WS-OUT is sent to the terminal, then input is received into WS-IN

**Complexity:** M

### Story 203.2: Dynamic Terminal Screen Size

As a **CICS developer**,
I want **terminals to support configurable screen sizes (Models 2-5)**,
So that **programs can use larger screens when available**.

**Acceptance Criteria:**

**Given** a terminal configured as Model 4 (43x80)
**When** ASSIGN SCRNHT/SCRNWD is executed
**Then** SCRNHT=43 and SCRNWD=80 are returned

**Complexity:** S

---

## Epic 204: Complete EBCDIC Translation

**Goal:** Replace the 33-character EBCDIC stub with full code page support.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-204

### Story 204.1: Integrate open-mainframe-encoding for EBCDIC

As a **developer**,
I want **the BMS renderer to use the encoding crate for EBCDIC translation**,
So that **all characters are correctly encoded in 3270 data streams**.

**Acceptance Criteria:**

**Given** any printable ASCII character
**When** translated to EBCDIC for 3270 rendering
**Then** the correct Code Page 037 value is produced

**Given** the BMS renderer
**When** rendering a map with special characters (brackets, braces, pipe)
**Then** all characters render correctly (not as spaces or garbage)

**Complexity:** S

---

## Epic 205: ASSIGN Command Runtime

**Goal:** Implement the ASSIGN command to retrieve system and task information at runtime.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-209

### Story 205.1: ASSIGN System Values

As a **CICS developer**,
I want **ASSIGN to return system values like SYSID, APPLID, USERID**,
So that **programs can determine their execution environment**.

**Acceptance Criteria:**

**Given** `EXEC CICS ASSIGN SYSID(WS-SYS) USERID(WS-USER) APPLID(WS-APP) END-EXEC`
**When** executed
**Then** WS-SYS, WS-USER, WS-APP contain the current system/user/application IDs

**Given** ASSIGN with FACILITY, NETNAME, OPID, STARTCODE
**When** executed
**Then** the corresponding values are returned from the transaction context

**Complexity:** M

---

## Epic 206: Preprocessor-to-Runtime Bridge

**Goal:** Create the dispatch layer connecting preprocessed COBOL CALL statements to CICS runtime execution.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-213

### Story 206.1: Command Parameter Block Format

As a **developer**,
I want **a defined binary format for CICS command parameter blocks**,
So that **the COBOL runtime can pass command data to the CICS dispatcher**.

**Acceptance Criteria:**

**Given** a CALL "CICSREAD" USING CICS-CMD-001 DFHEIBLK
**When** the dispatcher receives the call
**Then** CICS-CMD-001 is deserialized into file name, key, INTO pointer, KEYLENGTH

**Complexity:** L

### Story 206.2: Dispatcher Integration with COBOL Runtime

As a **developer**,
I want **the COBOL runtime to route CALL "CICSxxxx" to the CICS dispatcher**,
So that **CICS COBOL programs execute end-to-end**.

**Acceptance Criteria:**

**Given** a COBOL program with EXEC CICS LINK PROGRAM('SUB')
**When** the preprocessed CALL "CICSLINK" executes
**Then** the CICS runtime's link() method is invoked with program='SUB'

**Given** the runtime sets EIBRESP=13 (NOTFND)
**When** the CALL returns
**Then** DFHEIBLK contains EIBRESP=13 accessible by the COBOL program

**Complexity:** XL

---

## Epic 207: Deadlock Detection and Lock Management

**Goal:** Add deadlock detection and timeout to ENQ/DEQ resource locking.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-211

### Story 207.1: Deadlock Detection

As a **CICS developer**,
I want **the ENQ manager to detect deadlocks between tasks**,
So that **one task is abended with AEYD instead of hanging forever**.

**Acceptance Criteria:**

**Given** Task A holds LOCK-1 and waits for LOCK-2
**When** Task B holds LOCK-2 and waits for LOCK-1
**Then** the deadlock is detected and one task receives ABEND AEYD

**Complexity:** L

### Story 207.2: NOSUSPEND and Timeout

As a **CICS developer**,
I want **NOSUSPEND option on ENQ to return immediately if lock unavailable**,
So that **programs can handle contention without blocking**.

**Acceptance Criteria:**

**Given** `EXEC CICS ENQ RESOURCE('MY-RES') NOSUSPEND END-EXEC`
**When** the resource is locked by another task
**Then** EIBRESP=ENQBUSY is set and control returns immediately

**Complexity:** M

---

## Epic 208: Auxiliary TS Storage and Queue Improvements

**Goal:** Implement auxiliary (disk-backed) TS queue storage.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-207

### Story 208.1: Auxiliary TS Queue Storage

As a **CICS developer**,
I want **TS queues with AUXILIARY option to persist to disk**,
So that **scratchpad data survives CICS restarts**.

**Acceptance Criteria:**

**Given** `EXEC CICS WRITEQ TS QUEUE('SCRTCH') FROM(WS-DATA) AUXILIARY END-EXEC`
**When** the item is written
**Then** it is stored on disk (not just in memory)

**Given** the CICS runtime restarts
**When** READQ TS QUEUE('SCRTCH') executes
**Then** the previously written data is available

**Complexity:** M

---

## Epic 209: BMS and Terminal Enhancements

**Goal:** Add BMS page building, ROUTE command, and extended attribute rendering.

**Crate:** `open-mainframe-cics`
**FRs:** FR-v3.0-205

### Story 209.1: BMS Extended Attribute Rendering

As a **CICS developer**,
I want **field colors and highlights rendered in 3270 data streams**,
So that **color terminals display fields correctly**.

**Acceptance Criteria:**

**Given** a BMS field with COLOR=RED and HILIGHT=REVERSE
**When** rendered to a 3270 data stream
**Then** SFE (Start Field Extended) orders include color and highlight attribute bytes

**Complexity:** M

### Story 209.2: BMS SEND PAGE and PURGE MESSAGE

As a **CICS developer**,
I want **BMS page building for multi-page output**,
So that **long reports can be sent to terminals with paging support**.

**Acceptance Criteria:**

**Given** multiple SEND MAP ACCUM calls
**When** SEND PAGE is issued
**Then** accumulated maps are delivered to the terminal as pages

**Complexity:** L
