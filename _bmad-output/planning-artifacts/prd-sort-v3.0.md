# Sort Crate — Product Requirements

## Overview

The `open-mainframe-sort` crate provides a DFSORT-compatible sort utility for the OpenMainframe platform. It includes a control statement parser, sort engine with filtering and reformatting, and support for character, zoned decimal, packed decimal, binary, and fixed-point data types.

## Current State Assessment

- **Lines of code:** 1,918
- **Test count:** 33 (all passing)
- **Maturity:** Basic (core sort/filter/reformat pipeline works, many DFSORT features missing)
- **Files:** 7 Rust source files (lib, error, fields, filter, parser, reformat, engine)

### What Works Well

**Control Statement Parsing:**
- SORT FIELDS=(pos,len,type,order,...) with multiple keys
- INCLUDE COND=(pos,len,type,op,value) with character and hex literals
- OMIT COND=(pos,len,type,op,value)
- INREC FIELDS and OUTREC FIELDS for record reformatting
- SUM FIELDS=NONE (duplicate key removal)
- OPTION COPY mode
- Line continuation support

**Sort Engine:**
- In-memory sort with stable ordering
- Full processing pipeline: read → filter → INREC → sort → SUM → OUTREC → write
- Merge operation for pre-sorted files
- Copy mode (pass-through without sorting)
- Statistics collection (input/filtered/summed/output record counts)

**Data Types:**
- Character (CH) — byte-level comparison
- Zoned Decimal (ZD) — numeric comparison with sign nibble
- Packed Decimal (PD) — BCD numeric comparison
- Binary (BI) — big-endian comparison
- Fixed Point (FI) — signed binary comparison

**Filtering:**
- INCLUDE and OMIT with 6 comparison operators (EQ, NE, GT, GE, LT, LE)
- Multiple conditions with AND/OR logic
- Data-type-aware comparison

**Reformatting:**
- Field copy from input record by position/length
- Literal insertion
- Space and zero padding
- Field reordering

### What Does NOT Work

- No OUTFIL for multi-output routing
- No IFTHEN conditional reformatting
- No BUILD/OVERLAY/FINDREP reformatting syntax
- No numeric SUM field summation (only FIELDS=NONE for dedup)
- No external sort (all records in memory — MAX_MEMORY_RECORDS unused)
- No JOINKEYS for file joining/matching
- No ICETOOL utility operators (DISPLAY, OCCUR, STATS, UNIQUE, SELECT, etc.)
- No ALTSEQ (alternate collating sequence)
- No date/time editing functions (DT1-3, TM1-4, etc.)
- No numeric editing functions (EDIT, COUNT, SEQNUM, etc.)
- No RECORD statement for record type/length specification
- No variable-length record handling
- No VSAM input/output support
- No multi-condition INCLUDE/OMIT with compound expressions in parser
- No STOPAFT/SKIPREC for limiting processing
- No reporting features (HEADER/TRAILER records)
- Code duplication between fields.rs and filter.rs for numeric parsing

## Functional Requirements

### FR-v3.0-800: External Sort (Disk-Based Merge Sort)
Implement disk-based external sort for datasets larger than available memory. Divide input into sorted runs, write runs to temporary files, and merge them. Support configurable memory limits.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature (MAX_MEMORY_RECORDS constant exists but is unused)
- **IBM Reference:** DFSORT Application Programming Guide — DFSORT automatically uses disk-based merge sort when data exceeds available main storage. Production datasets can be millions or billions of records.

### FR-v3.0-801: OUTFIL Multi-Output Routing
Implement the OUTFIL statement for writing to multiple output files from a single sort pass. Support per-output INCLUDE/OMIT filtering, BUILD/OUTREC reformatting, and SPLIT/SPLITBY/SPLIT1R for load balancing.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — OUTFIL is one of DFSORT's most powerful features, enabling creation of different output views in a single pass.

### FR-v3.0-802: IFTHEN Conditional Reformatting
Implement IFTHEN clauses for conditional record reformatting based on record content. Support WHEN=(condition), WHEN=INIT, WHEN=NONE, WHEN=GROUP with BUILD and OVERLAY actions.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — IFTHEN allows different records to be reformatted differently based on criteria, essential for multi-format input files.

### FR-v3.0-803: Numeric SUM Field Summation
Implement actual numeric field summation for SUM FIELDS=(pos,len,type,...). Support character, zoned, packed, binary, and fixed-point accumulation.
- **Priority:** MAJOR
- **Gap Type:** Incomplete implementation (TODO in engine.rs line 233)
- **IBM Reference:** DFSORT Application Programming Guide — SUM sums numeric fields across records with identical sort keys.

### FR-v3.0-804: BUILD/OVERLAY/FINDREP Reformatting
Implement the full BUILD syntax for INREC/OUTREC/OUTFIL including OVERLAY (in-place modification) and FINDREP (find and replace). Support numeric editing functions (EDIT, COUNT, SEQNUM), date arithmetic (ADDDAYS, ADDMONS, ADDYEARS), and format conversion (TRAN, UFF, SFF).
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — BUILD provides comprehensive record reformatting with editing, conversion, and arithmetic capabilities.

### FR-v3.0-805: JOINKEYS File Matching
Implement JOINKEYS for matching, merging, or joining two input files based on key fields. Support JOIN types: inner, left outer, right outer, full outer, unpaired.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — JOINKEYS allows matching records from two files, similar to SQL JOIN operations.

### FR-v3.0-806: ICETOOL Utility
Implement the ICETOOL batch utility with operators: SORT, COPY, MERGE, DISPLAY (formatted reporting), OCCUR (frequency analysis), STATS (statistics), UNIQUE (distinct values), SELECT (conditional selection), COUNT, DATASORT, VERIFY, RANGE.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — ICETOOL provides a simplified batch interface for common data analysis tasks without requiring JCL-level DFSORT control statements.

### FR-v3.0-807: Variable-Length Record Support
Implement variable-length record (VB) input and output. Support FTOV (fixed to variable) and VTOF (variable to fixed) conversion via OUTFIL.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — DFSORT handles both fixed and variable-length records. VB records have a 4-byte RDW (Record Descriptor Word) prefix.

### FR-v3.0-808: ALTSEQ Alternate Collating Sequence
Implement the ALTSEQ statement for custom collating sequences. Support ALTSEQ CODE with hexadecimal substitution pairs and the AQ data type for fields using the alternate sequence.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — ALTSEQ allows customized sort ordering, commonly used for case-insensitive sorts or locale-specific ordering.

### FR-v3.0-809: STOPAFT/SKIPREC and Record Limits
Implement STOPAFT (stop after N records), SKIPREC (skip first N records), and OUTFIL STARTREC/ENDREC/ACCEPT/SAMPLE for controlling which records are processed.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — Record limiting controls are used for testing, sampling, and pagination.

### FR-v3.0-810: HEADER/TRAILER Report Records
Implement OUTFIL HEADER1/HEADER2 and TRAILER1/TRAILER2 for adding report headers and trailers to output files. Support page breaks and line counts.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — HEADER/TRAILER allow DFSORT to produce formatted reports with titles, page numbers, and summary lines.

### FR-v3.0-811: Date and Time Editing Functions
Implement DFSORT date/time formats (DT1-3, DC1-3, DE1-3 for dates; TM1-4, TC1-4, TE1-4 for times) and date arithmetic functions (ADDDAYS, ADDMONS, ADDYEARS, SUBDAYS, SUBMONS, SUBYEARS, DATEDIFF).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **IBM Reference:** DFSORT Application Programming Guide — Date/time formatting and arithmetic are commonly used in SMF record processing and report generation.
