# Sort Crate — Epics & Stories

## Epic 800: External Sort (Disk-Based Merge Sort)

**Goal:** Enable sorting of datasets larger than available memory via disk-based external merge sort.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-800

### Story 800.1: Sorted Run Generation

As a **data processor**,
I want **input records divided into sorted in-memory chunks written as temporary run files**,
So that **large datasets can be sorted without loading everything into memory**.

**Acceptance Criteria:**

**Given** a 10-million-record dataset and a 1-million-record memory limit
**When** the sort engine runs
**Then** 10 sorted run files are created, each containing 1 million sorted records

**Given** a dataset smaller than the memory limit
**When** the sort engine runs
**Then** in-memory sort is used (no temporary files created)

**Complexity:** L

### Story 800.2: K-Way Merge

As a **data processor**,
I want **all sorted runs merged into a single sorted output using k-way merge**,
So that **the final output is correctly sorted regardless of dataset size**.

**Acceptance Criteria:**

**Given** 10 sorted run files
**When** the k-way merge executes
**Then** a single sorted output file is produced with all records in correct order

**Given** merge completes or is interrupted
**When** the operation ends
**Then** all temporary run files are cleaned up automatically

**Complexity:** L

---

## Epic 801: OUTFIL Multi-Output Routing

**Goal:** Route sorted records to multiple output files with per-file filtering and reformatting in a single pass.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-801

### Story 801.1: OUTFIL Statement Parsing and Routing

As a **data processor**,
I want **OUTFIL statements that direct records to different output files based on conditions**,
So that **I can create multiple output views from a single sort operation**.

**Acceptance Criteria:**

**Given** two OUTFIL statements: FNAMES=OUT1,INCLUDE=(1,2,CH,EQ,C'NY') and FNAMES=OUT2,INCLUDE=(1,2,CH,EQ,C'CA')
**When** the sort processes a dataset with NY and CA records
**Then** NY records go to OUT1 and CA records go to OUT2

**Given** OUTFIL with SAVE option
**When** records don't match any OUTFIL filter
**Then** unmatched records are written to the SAVE output

**Complexity:** L

### Story 801.2: OUTFIL SPLIT Operations

As a **data processor**,
I want **SPLIT, SPLITBY, and SPLIT1R options for distributing records across output files**,
So that **I can load-balance output across multiple files**.

**Acceptance Criteria:**

**Given** OUTFIL with SPLIT and 3 output files
**When** 9 records are processed
**Then** records are distributed round-robin: 3 per file

**Given** OUTFIL with SPLITBY=100
**When** 250 records are processed
**Then** records 1-100 go to file 1, 101-200 to file 2, 201-250 to file 3

**Complexity:** M

---

## Epic 802: IFTHEN Conditional Reformatting

**Goal:** Implement conditional record reformatting based on record content.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-802

### Story 802.1: IFTHEN WHEN=(condition) with BUILD/OVERLAY

As a **data processor**,
I want **IFTHEN clauses that apply different reformatting based on record content**,
So that **I can handle multi-format input files in a single operation**.

**Acceptance Criteria:**

**Given** IFTHEN=(WHEN=(1,1,CH,EQ,C'H'),BUILD=(1,80,C' HEADER')) for header records
**When** a record starting with 'H' is processed
**Then** the BUILD action is applied to that record

**Given** IFTHEN=(WHEN=NONE,BUILD=(1,80)) as default
**When** a record matches no other WHEN condition
**Then** the WHEN=NONE action is applied

**Complexity:** L

### Story 802.2: IFTHEN WHEN=INIT and WHEN=GROUP

As a **data processor**,
I want **WHEN=INIT for universal pre-processing and WHEN=GROUP for group-based operations**,
So that **I can initialize fields and track record groups**.

**Acceptance Criteria:**

**Given** IFTHEN=(WHEN=INIT,OVERLAY=(81:SEQNUM,8,ZD))
**When** records are processed
**Then** every record gets a sequence number in columns 81-88 before other IFTHEN clauses

**Given** IFTHEN=(WHEN=GROUP,BEGIN=(1,3,CH,EQ,C'HDR'),PUSH=(81:ID,8))
**When** a group of records starting with HDR is processed
**Then** all records in the group receive the same group ID in columns 81-88

**Complexity:** XL

---

## Epic 803: Numeric SUM Field Summation

**Goal:** Implement actual numeric field accumulation for SUM FIELDS.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-803

### Story 803.1: Numeric Field Summation

As a **data processor**,
I want **SUM FIELDS=(pos,len,type,...) to sum numeric fields across duplicate key records**,
So that **I can aggregate totals in a single pass**.

**Acceptance Criteria:**

**Given** SUM FIELDS=(21,8,PD) and three records with the same sort key
**When** packed decimal values 100, 200, 300 are in positions 21-28
**Then** one output record is produced with the summed value 600 in packed decimal

**Given** SUM FIELDS with multiple fields
**When** duplicate keys are encountered
**Then** all specified numeric fields are summed independently

**Given** numeric overflow during summation
**When** the sum exceeds the field capacity
**Then** an error is reported (not silently truncated)

**Complexity:** M

---

## Epic 804: BUILD/OVERLAY/FINDREP Reformatting

**Goal:** Implement the full DFSORT reformatting syntax for INREC/OUTREC/OUTFIL.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-804

### Story 804.1: BUILD Syntax with Editing Functions

As a **data processor**,
I want **BUILD with EDIT, COUNT, SEQNUM, and TRAN functions**,
So that **I can format numeric fields, add sequence numbers, and translate characters in output records**.

**Acceptance Criteria:**

**Given** BUILD=(1,10,SEQNUM,8,ZD,21,8,PD,EDIT=(IIIIIIII.TT))
**When** records are reformatted
**Then** columns 1-10 from input, 8-digit ZD sequence number, and edited PD value with decimal point are produced

**Given** BUILD with COUNT function
**When** applied
**Then** the total record count is available as a field value

**Complexity:** L

### Story 804.2: OVERLAY and FINDREP

As a **data processor**,
I want **OVERLAY for in-place column modification and FINDREP for find/replace**,
So that **I can modify specific columns without rebuilding the entire record**.

**Acceptance Criteria:**

**Given** OVERLAY=(1:C'X',30:SEQNUM,5,ZD)
**When** applied to records
**Then** column 1 is set to 'X' and columns 30-34 get a sequence number; other columns unchanged

**Given** FINDREP=(IN=C'OLD',OUT=C'NEW')
**When** applied to records containing 'OLD'
**Then** all occurrences of 'OLD' are replaced with 'NEW'

**Complexity:** M

---

## Epic 805: JOINKEYS File Matching

**Goal:** Implement two-file matching/joining based on key fields.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-805

### Story 805.1: JOINKEYS with REFORMAT

As a **data processor**,
I want **JOINKEYS to match records from two files by key and REFORMAT to combine matched fields**,
So that **I can perform SQL-like JOIN operations on flat files**.

**Acceptance Criteria:**

**Given** F1 with JOINKEYS FIELDS=(1,8,A) and F2 with JOINKEYS FIELDS=(1,8,A)
**When** records are matched by the 8-byte key
**Then** matched pairs are combined per the REFORMAT specification

**Given** JOIN type INNER
**When** F1 has key 'A' but F2 does not
**Then** key 'A' is excluded from output

**Given** JOIN type FULL
**When** F1 has key 'A' but F2 does not
**Then** key 'A' appears in output with F2 fields filled with '?'

**Complexity:** XL

---

## Epic 806: ICETOOL Utility

**Goal:** Implement the ICETOOL batch utility for high-level data analysis.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-806

### Story 806.1: ICETOOL Core Operators (SORT, COPY, DISPLAY)

As a **data analyst**,
I want **ICETOOL SORT, COPY, and DISPLAY operators**,
So that **I can perform common data operations with simplified syntax**.

**Acceptance Criteria:**

**Given** ICETOOL with SORT FROM(IN) TO(OUT) USING(CTRL)
**When** executed
**Then** records from IN are sorted per CTRL and written to OUT

**Given** ICETOOL with DISPLAY FROM(IN) LIST(RPT) HEADER('Report') ON(1,10,CH) ON(21,8,PD,EDIT=(IIIIIIII.TT))
**When** executed
**Then** a formatted text report is produced with column headers and formatted values

**Complexity:** L

### Story 806.2: ICETOOL Analysis Operators (OCCUR, STATS, UNIQUE, SELECT)

As a **data analyst**,
I want **OCCUR, STATS, UNIQUE, and SELECT operators**,
So that **I can analyze data distribution, compute statistics, and filter based on occurrence counts**.

**Acceptance Criteria:**

**Given** ICETOOL with OCCUR FROM(IN) LIST(RPT) ON(1,2,CH)
**When** executed
**Then** a report showing each unique value in columns 1-2 and its frequency is produced

**Given** ICETOOL with STATS FROM(IN) ON(21,8,PD)
**When** executed
**Then** min, max, average, sum, and count for the PD field are reported

**Complexity:** L

---

## Epic 807: Variable-Length Record Support

**Goal:** Handle variable-length records with RDW (Record Descriptor Word) and format conversion.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-807

### Story 807.1: VB Record I/O

As a **data processor**,
I want **reading and writing variable-length records with 4-byte RDW prefix**,
So that **I can sort VB datasets from z/OS**.

**Acceptance Criteria:**

**Given** a VB dataset with records of varying lengths (50-200 bytes)
**When** the sort engine reads them
**Then** each record is correctly delimited by its RDW and sorted as a whole

**Given** VB sorted output
**When** written to disk
**Then** each record has its 4-byte RDW correctly set to the record length

**Complexity:** M

### Story 807.2: FTOV and VTOF Conversion

As a **data processor**,
I want **OUTFIL FTOV and VTOF to convert between fixed and variable-length formats**,
So that **I can transform record formats during sort processing**.

**Acceptance Criteria:**

**Given** OUTFIL with VTOF and 80-byte fixed output length
**When** VB records of varying lengths are processed
**Then** output records are padded or truncated to 80 bytes

**Given** OUTFIL with FTOV
**When** FB 80-byte records with trailing spaces are processed
**Then** output records have trailing spaces removed and RDW set to actual length

**Complexity:** M

---

## Epic 808: Record Limiting Controls

**Goal:** Implement STOPAFT, SKIPREC, and OUTFIL record selection controls.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-809

### Story 808.1: STOPAFT and SKIPREC

As a **data processor**,
I want **STOPAFT to limit input records and SKIPREC to skip initial records**,
So that **I can test with subsets or paginate processing**.

**Acceptance Criteria:**

**Given** OPTION STOPAFT=100
**When** a 1-million-record dataset is processed
**Then** only the first 100 records are read and sorted

**Given** OPTION SKIPREC=50,STOPAFT=100
**When** a dataset is processed
**Then** records 51-150 are read and sorted

**Complexity:** S

---

## Epic 809: HEADER/TRAILER Report Records

**Goal:** Add report header and trailer records to sort output.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-810

### Story 809.1: OUTFIL HEADER and TRAILER

As a **report generator**,
I want **HEADER1/HEADER2 for report titles and TRAILER1/TRAILER2 for summary lines**,
So that **DFSORT output can be used as formatted reports**.

**Acceptance Criteria:**

**Given** OUTFIL HEADER1=(C'Sales Report',50:DATE=(4MD/))
**When** the output file is written
**Then** the first line contains 'Sales Report' and the formatted date

**Given** OUTFIL TRAILER1=(C'Total Records: ',COUNT=(M11,LENGTH=8))
**When** the output file is written
**Then** the last line contains the total record count

**Complexity:** M

---

## Epic 810: Date/Time Editing Functions

**Goal:** Implement DFSORT date and time format codes and arithmetic functions.

**Crate:** `open-mainframe-sort`
**FRs:** FR-v3.0-811

### Story 810.1: Date/Time Format Codes

As a **data processor**,
I want **date format codes (DT1-3, DC1-3, DE1-3) and time format codes (TM1-4, TC1-4, TE1-4)**,
So that **I can format SMF timestamps and other mainframe date/time values in reports**.

**Acceptance Criteria:**

**Given** an SMF date in DT1 format (0cyydddF packed)
**When** formatted with EDIT function
**Then** the date is displayed as YYYY/DDD or converted to YYYY/MM/DD

**Given** a STCK (Store Clock) value in TOD format
**When** formatted with TC1 format
**Then** the time is displayed as HH:MM:SS

**Complexity:** M

### Story 810.2: Date Arithmetic Functions

As a **data processor**,
I want **ADDDAYS, ADDMONS, ADDYEARS, SUBDAYS, SUBMONS, SUBYEARS, and DATEDIFF functions**,
So that **I can perform date calculations in OUTREC/OUTFIL reformatting**.

**Acceptance Criteria:**

**Given** a date field and ADDDAYS=30
**When** applied
**Then** the output date is 30 days later (handling month/year boundaries)

**Given** two date fields and DATEDIFF
**When** applied
**Then** the number of days between the two dates is computed

**Complexity:** M
