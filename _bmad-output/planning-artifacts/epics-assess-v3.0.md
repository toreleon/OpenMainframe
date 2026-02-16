# Assess Crate — Epics & Stories

## Epic 1000: AST-Based Analysis

**Goal:** Replace text pattern matching with structured AST analysis using the COBOL parser crate.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1000

### Story 1000.1: COBOL Parser Integration

As a **migration analyst**,
I want **analysis based on the parsed COBOL AST rather than text pattern matching**,
So that **feature detection is accurate and free of false positives**.

**Acceptance Criteria:**

**Given** a COBOL source with `EXEC SQL` in a comment line
**When** analyzed with AST-based detection
**Then** no DB2 feature is detected (comment is not code)

**Given** a COBOL source with EXEC CICS LINK, READ, and SEND MAP commands
**When** analyzed
**Then** 3 distinct CICS feature occurrences are detected with correct line numbers

**Given** unparseable COBOL source (syntax errors)
**When** analyzed
**Then** a partial analysis is produced with a warning about parse errors

**Complexity:** L

### Story 1000.2: AST-Based Metrics

As a **migration analyst**,
I want **accurate metrics derived from AST nodes (statements, paragraphs, data items, sections)**,
So that **complexity scores reflect actual program structure**.

**Acceptance Criteria:**

**Given** a COBOL program with 50 executable statements
**When** metrics are calculated from the AST
**Then** the statement count is exactly 50 (not estimated from line patterns)

**Given** a program with nested IF/EVALUATE constructs
**When** cyclomatic complexity is calculated
**Then** the complexity correctly counts each decision point from the AST

**Complexity:** M

---

## Epic 1001: File and Directory Scanning

**Goal:** Enable batch assessment of entire codebases with file discovery and copybook resolution.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1001

### Story 1001.1: Directory Scanner

As a **migration analyst**,
I want **batch scanning of a directory tree for COBOL source files**,
So that **I can assess an entire codebase in one command**.

**Acceptance Criteria:**

**Given** a directory tree with 500 COBOL files (.cbl, .cob)
**When** the scanner runs
**Then** all 500 files are discovered and analyzed

**Given** a glob pattern `src/**/*.cbl`
**When** used to filter scanning
**Then** only matching files are analyzed

**Given** 500 files and 8 CPU cores
**When** parallel analysis runs
**Then** analysis completes faster than sequential (wall-clock time)

**Complexity:** M

### Story 1001.2: Copybook Resolution

As a **migration analyst**,
I want **COPY statements resolved to actual copybook files from configurable include paths**,
So that **data definitions in copybooks are included in the analysis**.

**Acceptance Criteria:**

**Given** COPY CUSTOMER-RECORD and include path `/copybooks`
**When** `/copybooks/CUSTOMER-RECORD.cpy` exists
**Then** the copybook content is included in the analysis

**Given** a missing copybook
**When** analysis runs
**Then** a warning is added to the report (analysis continues without the copybook)

**Complexity:** M

---

## Epic 1002: Call Graph Analysis

**Goal:** Build program-level call graphs for dependency mapping and migration ordering.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1002

### Story 1002.1: Call Graph Construction

As a **migration analyst**,
I want **a call graph showing which programs call which others**,
So that **I can understand program interdependencies**.

**Acceptance Criteria:**

**Given** Program A calls B and C; Program B calls D
**When** the call graph is built
**Then** edges A→B, A→C, B→D are present in the graph

**Given** CICS LINK PROGRAM('SUB') in a COBOL source
**When** the call graph is built
**Then** an edge of type "CICS LINK" connects the caller to SUB

**Given** CALL WS-PROG-NAME (dynamic call)
**When** the call graph is built
**Then** an uncertain edge is created with a note about dynamic dispatch

**Complexity:** L

### Story 1002.2: Migration Order and Dependency Analysis

As a **migration analyst**,
I want **topological sort for migration order and circular dependency detection**,
So that **I can plan migration phases (leaves first, callers later)**.

**Acceptance Criteria:**

**Given** a call graph with no cycles
**When** topological sort is computed
**Then** leaf programs appear first and entry points appear last

**Given** a call graph with A→B→C→A cycle
**When** circular dependency detection runs
**Then** the cycle [A, B, C] is reported

**Complexity:** M

---

## Epic 1003: CICS Command Inventory

**Goal:** Categorize and count CICS commands to map feature usage to implementation status.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1003

### Story 1003.1: CICS Command Classification

As a **migration analyst**,
I want **each EXEC CICS command classified by type (file, queue, terminal, program, etc.)**,
So that **I know exactly which CICS features are used and how often**.

**Acceptance Criteria:**

**Given** a program with EXEC CICS READ, WRITE, SEND MAP, LINK, WRITEQ TS
**When** the CICS inventory is produced
**Then** commands are classified: File(READ, WRITE), Terminal(SEND MAP), Program(LINK), Queue(WRITEQ TS)

**Given** the CICS inventory
**When** compared against open-mainframe-cics capabilities
**Then** each command shows whether it is supported, partially supported, or unsupported

**Complexity:** M

---

## Epic 1004: DB2 SQL Complexity Analysis

**Goal:** Analyze embedded SQL for complexity and PostgreSQL compatibility.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1004

### Story 1004.1: SQL Statement Classification

As a **migration analyst**,
I want **embedded SQL statements classified by complexity (simple SELECT, joins, subqueries, cursors, dynamic SQL)**,
So that **I can estimate SQL migration effort separately from COBOL effort**.

**Acceptance Criteria:**

**Given** a program with 5 simple SELECTs and 2 cursor declarations
**When** SQL complexity is analyzed
**Then** the report shows: 5 simple queries, 2 cursor operations

**Given** a program using WHENEVER SQLERROR and DECLARE CURSOR WITH HOLD
**When** SQL compatibility is checked
**Then** PostgreSQL compatibility notes are produced for each feature

**Complexity:** L

---

## Epic 1005: Dead Code Detection

**Goal:** Identify unreachable code to exclude from migration estimates.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1006

### Story 1005.1: Unreachable Paragraph Detection

As a **migration analyst**,
I want **paragraphs that are never performed or referenced detected as dead code**,
So that **migration effort estimates are not inflated by unused code**.

**Acceptance Criteria:**

**Given** paragraph OLD-ROUTINE that is never PERFORMed or GO TOed
**When** dead code detection runs
**Then** OLD-ROUTINE is flagged as potentially unreachable

**Given** 20% of paragraphs identified as dead code
**When** the report is generated
**Then** adjusted effort estimates exclude dead code volume

**Complexity:** M

---

## Epic 1006: Migration Progress Tracking

**Goal:** Compare assessment runs to track migration progress over time.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1007

### Story 1006.1: Assessment Snapshot and Comparison

As a **migration project manager**,
I want **saved assessment snapshots that can be compared across runs**,
So that **I can track migration progress and report status to stakeholders**.

**Acceptance Criteria:**

**Given** an assessment run saved as baseline.json
**When** a new assessment is compared against the baseline
**Then** a diff report shows: resolved issues, new issues, changed complexity scores, migrated programs

**Given** 100 programs in baseline and 30 migrated
**When** the progress report is generated
**Then** overall progress shows 30% complete with remaining effort estimate

**Complexity:** M

---

## Epic 1007: JCL Dependency Analysis

**Goal:** Analyze JCL jobs that invoke COBOL programs to understand batch scheduling dependencies.

**Crate:** `open-mainframe-assess`
**FRs:** FR-v3.0-1008

### Story 1007.1: JCL-to-Program Mapping

As a **migration analyst**,
I want **JCL jobs analyzed to identify which programs they execute and which datasets they reference**,
So that **batch job dependencies are included in migration planning**.

**Acceptance Criteria:**

**Given** a JCL job with `//STEP1 EXEC PGM=CUSTUPD`
**When** JCL dependency analysis runs
**Then** the job-to-program mapping STEP1→CUSTUPD is recorded

**Given** a JCL DD statement `//INFILE DD DSN=PROD.CUSTOMER.MASTER`
**When** analyzed
**Then** the dataset dependency CUSTUPD→PROD.CUSTOMER.MASTER is recorded

**Complexity:** M
