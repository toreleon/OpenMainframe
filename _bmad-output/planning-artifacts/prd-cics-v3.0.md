# CICS Transaction Processing Gap Closure — Product Requirements

## Overview

The `open-mainframe-cics` crate provides CICS transaction processing support for the OpenMainframe platform. It includes an EXEC CICS preprocessor, a runtime library for command execution, BMS (Basic Mapping Support) for 3270 terminal I/O, file operations, queue services, time services, synchronization, and interval control.

## Current State Assessment

- **Lines of code:** 7,916
- **Test count:** 137 (all passing)
- **Maturity:** Moderate
- **Files:** 21 Rust source files across 8 modules (bms, interval, preprocess, queues, runtime, sync, terminal, time)

### What Works Well

**Preprocessing (32 command types recognized):**
- EXEC CICS block scanning and extraction from COBOL source
- Command type detection: LINK, XCTL, RETURN, READ, WRITE, REWRITE, DELETE, SEND, RECEIVE, SEND MAP, RECEIVE MAP, GETMAIN, FREEMAIN, HANDLE CONDITION/ABEND/AID, IGNORE CONDITION, START, RETRIEVE, CANCEL, DELAY, ASKTIME, FORMATTIME, ASSIGN, ADDRESS, READQ TS, WRITEQ TS, DELETEQ TS, ENQ, DEQ, SYNCPOINT, ABEND
- Option parsing with parenthesized values
- COBOL CALL statement generation for runtime dispatch

**Runtime:**
- Program control: LINK (with mock mode), XCTL, RETURN (with TRANSID/COMMAREA), ABEND
- Storage management: GETMAIN, FREEMAIN
- Condition handling: HANDLE CONDITION, IGNORE CONDITION, HANDLE ABEND
- EIB (Execute Interface Block): Full field set, time/date, AID keys, response codes
- COMMAREA management with byte-level operations
- Program registry with case-insensitive lookup

**BMS (Basic Mapping Support):**
- BMS macro parsing (DFHMSD, DFHMDI, DFHMDF)
- Field definitions with attributes (UNPROT, NUM, BRT, ASKIP, IC, FSET)
- Symbolic map generation (COBOL copybooks with I/O structures)
- 3270 data stream rendering (SBA, SF, SFE, WCC)
- Color and highlight support in definitions
- DFHBMSCA constant generation

**File Operations:**
- Full CRUD: READ, WRITE, REWRITE, DELETE
- READ for UPDATE with record locking
- Browse operations: STARTBR, READNEXT, READPREV, RESETBR, ENDBR
- UNLOCK for releasing locks without update
- File definition with read/write/browse/update/delete permissions

**Queue Services:**
- Temporary Storage (TS): WRITEQ, READQ, DELETEQ with item numbering and sequential access
- Transient Data (TD): FIFO queues with trigger level mechanism

**Other Services:**
- Time: ASKTIME (absolute time), FORMATTIME (multiple date/time formats)
- Sync: SYNCPOINT (commit/rollback), ENQ/DEQ with shared/exclusive locks
- Interval: START, CANCEL, RETRIEVE, DELAY
- Terminal: SEND MAP, RECEIVE MAP, SEND TEXT with screen buffer management

### What Does NOT Work

- No READQ TD / WRITEQ TD in preprocessor command types
- No CONVERSE command (combined send/receive)
- No INQUIRE/SET system programming commands
- No JOURNAL commands
- No Web Services / SOAP / REST commands
- No Document commands (DOCUMENT CREATE/INSERT/SET/RETRIEVE)
- No WEB SEND/RECEIVE/READ/WRITE commands
- No Channel/Container support (modern replacement for COMMAREA)
- No STARTBROWSE/GETNEXT/ENDBROWSE for system resources
- No BMS ROUTE command
- No BMS page building (SEND PAGE, PURGE MESSAGE)
- No persistent file storage (in-memory only)
- No actual 3270 data stream I/O (simulation only)
- Incomplete EBCDIC translation (33 chars vs full 256-char code page)
- No DCT (Destination Control Table) for TD queues
- No two-phase commit or distributed transactions

## Functional Requirements

### FR-v3.0-200: Channel and Container Support
Implement CICS channels and containers as the modern replacement for COMMAREA. Support PUT CONTAINER, GET CONTAINER, MOVE CONTAINER, DELETE CONTAINER, CHANNEL options on LINK/XCTL/RETURN.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** CICS TS 5.x+ channels/containers support up to 2GB data (vs COMMAREA's 32KB limit). Most new CICS applications use channels.

### FR-v3.0-201: TD Queue Commands (READQ TD / WRITEQ TD)
Add READQ TD and WRITEQ TD command types to the preprocessor and runtime. Support SYSID for remote queues.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature (TD queues exist but commands not wired to preprocessor)
- **IBM Reference:** TD queues are fundamental for audit trails, error logging, and asynchronous processing.

### FR-v3.0-202: CONVERSE Command
Implement CONVERSE as a combined SEND + RECEIVE operation for terminal I/O. Support FROM, INTO, MAXLENGTH, ERASE options.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** CONVERSE is used in many programs as a more efficient alternative to separate SEND/RECEIVE pairs.

### FR-v3.0-203: Persistent File Storage
Replace in-memory file storage with disk-backed VSAM-style persistence. Integrate with the `open-mainframe-dataset` crate for actual file I/O. Support KSDS, RRDS, and ESDS access methods.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation
- **IBM Reference:** CICS files map to VSAM datasets. File operations must persist across transactions.

### FR-v3.0-204: Complete EBCDIC Translation
Implement full 256-character EBCDIC code page translation (at minimum Code Page 037 — US/Canada). Currently only 33 characters are mapped.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** 3270 data streams use EBCDIC encoding. Incomplete translation corrupts field data.

### FR-v3.0-205: BMS Page Building
Implement BMS page building with SEND PAGE, PURGE MESSAGE, ROUTE commands. Support multi-page output with paging (CSPG transaction).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Page building is used for long reports sent to terminals. Less common than SEND MAP.

### FR-v3.0-206: Document Commands
Implement DOCUMENT CREATE, DOCUMENT INSERT, DOCUMENT SET, DOCUMENT RETRIEVE for building text documents.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Document commands are used for web-based CICS applications and text generation.

### FR-v3.0-207: Auxiliary TS Queue Storage
Implement auxiliary storage for TS queues (backed by disk). Currently only main storage (in-memory) is supported.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** Auxiliary TS storage survives CICS restarts and is used for data that must persist across transactions.

### FR-v3.0-208: Terminal Screen Size Flexibility
Fix the hardcoded Model 2 (24x80) screen size. Store actual screen size per terminal. Support Models 2-5 with dynamic sizing.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation (TODO in terminal.rs line 67)
- **IBM Reference:** CICS supports multiple 3270 terminal models with different screen sizes.

### FR-v3.0-209: ASSIGN Command Runtime
Implement the ASSIGN command at runtime to retrieve system values (SYSID, APPLID, USERID, OPID, FACILITY, NETNAME, etc.).
- **Priority:** MAJOR
- **Gap Type:** Missing feature (parsed but not executed)
- **IBM Reference:** ASSIGN is commonly used to get system context in application programs.

### FR-v3.0-210: INQUIRE/SET System Programming Commands
Implement basic INQUIRE and SET commands for runtime resource management (INQUIRE PROGRAM, INQUIRE FILE, INQUIRE TRANSACTION, SET FILE OPEN/CLOSED).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** System programming interface (SPI) commands are used by administrative transactions.

### FR-v3.0-211: Deadlock Detection and Lock Timeout
Add deadlock detection to the ENQ/DEQ mechanism. Support NOSUSPEND option. Add configurable lock timeout.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** Real CICS detects deadlocks and abends the appropriate task with AEYD.

### FR-v3.0-212: Two-Phase Commit
Implement two-phase commit protocol for distributed transactions spanning CICS, DB2, and IMS.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** Two-phase commit ensures atomicity across resource managers in production CICS.

### FR-v3.0-213: Runtime Command Dispatch Integration
Connect the preprocessor's COBOL CALL generation to actual runtime command dispatch. Currently the preprocessor generates CALL statements but there is no COBOL-callable runtime dispatcher.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** The preprocessor-to-runtime bridge is essential for running CICS COBOL programs end-to-end.
