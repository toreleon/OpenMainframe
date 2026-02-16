# IMS Hierarchical Database Crate — Product Requirements

## Overview

The `open-mainframe-ims` crate provides IMS (Information Management System) hierarchical database support for the OpenMainframe platform. It includes DBD (Database Definition) parsing, PSB (Program Specification Block) parsing, DL/I (Data Language/I) call processing, an in-memory hierarchical data store, PostgreSQL schema generation, and a runtime that ties these together.

## Current State Assessment

- **Lines of code:** ~3,200
- **Test count:** 62 (all passing)
- **Maturity:** Moderate (DBD/PSB/DL/I core), Prototype (preprocessor, persistence)
- **Files:** 9 Rust source files across 5 modules (dbd, dli, preprocess, psb, runtime, schema)

### What Works Well

**DBD Parsing:**
- Parses DBD, SEGM, FIELD macros with label handling
- Supports 9 access methods (HISAM, HIDAM, HDAM, HSAM, SHSAM, SHISAM, MSDB, DEDB, GSAM)
- 5 field types (Character, Packed, Zoned, Binary, Hex)
- Segment hierarchy tracking with parent-child relationships
- Sequence field designation and path computation
- DBDGEN/FINISH/END termination

**PSB Parsing:**
- Parses PSB, PCB, SENSEG macros
- 4 PCB types (Db, Gsam, Alt, Io)
- Processing options (PROCOPT) with G/I/R/D/A flags
- Sensitive segment definitions with per-segment PROCOPT override
- 4 language types (COBOL, PL/I, ASM, Pascal)
- Key feedback area length tracking

**DL/I Call Processing:**
- All 9 standard DL/I functions: GU, GN, GNP, GHU, GHN, GHNP, ISRT, DLET, REPL
- Segment Search Arguments (SSA) with qualified and unqualified forms
- 8 command codes (D, F, L, N, P, Q, U, V)
- 6 comparison operators (=, !=, >, >=, <, <=)
- SSA parsing from string representation
- DliCall builder pattern with SSA chaining

**In-Memory Hierarchical Store:**
- Full hierarchical segment storage with parent-child relationships
- Preorder traversal (GN navigation)
- GNP within-parent boundary navigation
- Qualified search on root and child segments
- Cascade delete of segment trees
- Duplicate key detection
- Key change validation for REPL

**Status Codes:**
- 13 status codes: OK, GE, GB, GA, GP, GK, II, DJ, RX, AP, AD, AI, AK
- Category helpers (is_ok, is_not_found, is_error)
- Display and from_chars conversions

**PostgreSQL Schema Generation:**
- Closure table pattern for hierarchical data
- Per-segment typed tables with field columns
- GIN index for JSONB data queries
- Recursive CTE view for hierarchical navigation
- Closure table maintenance trigger

**Runtime:**
- PSB scheduling and termination
- Multi-database support
- Database position tracking for GN/GNP
- Convenience methods (gu, gn, gnp, isrt)
- Full DL/I dispatch with SSA navigation

### What Does NOT Work

- EXEC DLI preprocessor is an empty stub (comment only, no code)
- No CBLTDLI call generation for COBOL programs
- No persistence — all data is in-memory only
- No system service calls (CHKP, ROLB, SYNC, XRST, ROLS, SETS, LOG, STAT)
- No GSAM database I/O (sequential file access)
- No I/O PCB for message handling or system service calls
- No IMS/TM transaction manager features (MPP, BMP, message queues)
- No secondary indexes
- No logical relationships (LCHILD/XDFLD)
- No DATASET macro processing in DBD parser
- No SENFLD (sensitive field) processing in PSB parser
- No variable-length segment support (COMPRTN, min/max bytes)
- Missing ~90+ status codes (only 13 of 100+ IBM status codes defined)
- No segment data extraction from raw byte buffers using field definitions
- No DIB (DL/I Interface Block) for EXEC DLI status checking
- No Fast Path (DEDB) runtime support despite access method definition
- No multiple qualification statements (boolean AND/OR in SSAs)
- No PROCSEQ (processing sequence) for secondary index navigation
- PostgreSQL schema generation has no actual database connectivity

## Functional Requirements

### FR-v3.0-400: EXEC DLI Preprocessor
Implement the EXEC DLI preprocessor that scans COBOL source for `EXEC DLI ... END-EXEC` blocks and generates CBLTDLI CALL statements. Support GU, GN, GNP, GHU, GHN, GHNP, ISRT, DLET, REPL, SCHD, TERM, CHKP commands. Generate proper SSA parameters.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature (empty stub file)
- **IBM Reference:** IMS Application Programming: EXEC DLI Commands for CICS and IMS. The preprocessor translates high-level EXEC DLI to CBLTDLI calls, similar to EXEC CICS and EXEC SQL preprocessors.

### FR-v3.0-401: System Service Calls
Implement CHKP (checkpoint), ROLB (rollback), SYNC (synchronization point), XRST (extended restart), ROLS (rollback to savepoint), SETS (set savepoint), LOG (write to log), and STAT (statistics) system service calls.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** IMS Application Programming Guide — System Service Calls. CHKP and ROLB are essential for transaction integrity. Every production IMS program uses CHKP.

### FR-v3.0-402: Database Persistence
Replace or augment the in-memory HierarchicalStore with disk-backed persistence. Enable data to survive across program invocations and support PostgreSQL as a backing store using the generated schema.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation
- **IBM Reference:** IMS databases are disk-resident (VSAM, OSAM). Data persists across transactions, program invocations, and IMS restarts.

### FR-v3.0-403: I/O PCB and Message Handling
Implement the I/O PCB for system service calls and basic message queue operations (GU to I/O PCB for input messages, ISRT to I/O PCB for output messages). This is the foundation for IMS/TM support.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** IMS TM Application Programming Guide. The I/O PCB is always the first PCB in the PSB. It is used for CHKP, ROLB, SYNC calls and for reading/writing messages in MPP regions.

### FR-v3.0-404: GSAM Database Support
Implement GSAM (Generalized Sequential Access Method) database operations. Support GN (read next record) and ISRT (write next record) for sequential file processing in batch programs.
- **Priority:** MAJOR
- **Gap Type:** Missing feature (PCB type exists but no I/O implementation)
- **IBM Reference:** IMS Application Programming Guide — GSAM Databases. GSAM provides sequential file access within IMS programs, commonly used for input/output files in batch processing.

### FR-v3.0-405: Comprehensive Status Codes
Expand the status code set from 13 to cover all commonly encountered IMS DL/I status codes (~50+ minimum). Include status codes for system service calls, GSAM, and error categories.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** IMS Messages and Codes Reference — over 100 two-character DL/I status codes covering database access, system service, GSAM, Fast Path, and I/O errors.

### FR-v3.0-406: Secondary Index Support
Implement secondary indexes for alternate key access. Support XDFLD (index field) in DBD, PROCSEQ option on PCB for secondary index processing, and index pointer segments.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** IMS Database Administration Guide — Secondary Indexes. Secondary indexes provide alternate access paths to segments, enabling retrieval by non-sequence fields.

### FR-v3.0-407: Logical Relationships
Implement logical relationships between databases (LCHILD macro in DBD). Support logical children, virtual logical children, and bidirectional physical-logical navigation.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** IMS Database Administration Guide — Logical Relationships. Logical relationships allow segments in one database to point to segments in another, enabling network-like structures within the hierarchical model.

### FR-v3.0-408: Multiple Qualification Statements (Boolean SSAs)
Implement AND/OR operators in SSA qualifications to support compound search criteria within a single SSA.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** IMS Application Programming Guide — Multiple Qualification Statements. IBM IMS supports `*AND` and `*OR` connectors between qualifications in a single SSA.

### FR-v3.0-409: Segment Data Extraction
Implement segment data extraction from raw byte buffers using field definitions (start position, length, type). Convert between raw segment bytes and structured field values.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** In real IMS, segments are fixed-layout byte buffers. Programs access fields by offset. The field definitions in the DBD define the layout.

### FR-v3.0-410: DBD Enhancements (DATASET, LCHILD, Variable Length)
Add DATASET macro processing, LCHILD (logical child) macro, variable-length segment support (COMPRTN), and segment concatenation.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation
- **IBM Reference:** IMS Database Administration Guide — DBD Generation. DATASET specifies physical storage characteristics; LCHILD defines logical relationships.

### FR-v3.0-411: SENFLD (Sensitive Field) Support
Implement SENFLD macro in PSB parser to restrict field-level access. Only specified fields should be visible to the program.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** IMS Application Programming Guide — Field Level Sensitivity. SENFLD restricts program access to specific fields within a segment.

### FR-v3.0-412: Fast Path DEDB Runtime
Implement runtime support for Data Entry Databases (DEDB), including direct dependent segments, sequential dependent segments, and subset pointer operations.
- **Priority:** MINOR
- **Gap Type:** Missing feature (access method defined but no runtime support)
- **IBM Reference:** IMS Fast Path Guide. DEDBs provide high-performance, high-availability access for frequently updated data.
