# Assess Crate — Product Requirements

## Overview

The `open-mainframe-assess` crate is a migration assessment tool for mainframe COBOL programs. It analyzes COBOL source code to produce migration reports including code complexity metrics, feature compatibility analysis, migration effort estimation, and modernization recommendations. Unlike other crates in the workspace, Assess is a tooling crate — it does not replicate an IBM z/OS subsystem but rather facilitates migration planning.

## Current State Assessment

- **Lines of code:** 1,807
- **Test count:** 25 (all passing)
- **Maturity:** Moderate (core assessment pipeline works, limited depth)
- **Files:** 5 Rust source files (lib, analyzer, compatibility, metrics, report)

### What Works Well

**Source Code Analysis:**
- PROGRAM-ID extraction from IDENTIFICATION DIVISION
- Metric calculation: total lines, code lines, comment lines, blank lines, executable statements, paragraphs, data items, cyclomatic complexity
- Feature detection via pattern matching: DB2 (EXEC SQL), CICS (EXEC CICS), IMS (EXEC DLI), VSAM, COPY, CALL, DISPLAY, ACCEPT, STRING/UNSTRING, INSPECT, COMPUTE

**Compatibility Checking:**
- 11 built-in compatibility rules with severity levels (Info, Warning, High, Critical)
- Custom rule registration
- Feature support database with percentage compatibility (Sequential Files 100%, VSAM KSDS 90%, DB2 SQL 85%, CICS 75%, IMS 0%)

**Migration Complexity Scoring:**
- 4-level complexity classification (Low, Medium, High, VeryHigh)
- Effort multiplier per level (1.0x to 8.0x)
- Effort estimation (base: 1hr per 100 LOC × multiplier)

**Metrics:**
- Cyclomatic complexity classification
- Comment ratio calculation
- Maintainability index (normalized 0-100)
- Technical debt estimation (hours)

**Report Generation:**
- 4 output formats: Text (ASCII tables), Markdown (with emoji severity), JSON (serialized), HTML (styled document)
- Executive summary, file details, issues list, recommendations, feature support table

### What Does NOT Work

- No file I/O — analyzer takes source string only, not file paths or directories
- No batch directory scanning for multi-program assessment
- Feature detection is simple substring matching — false positives possible
- No COBOL AST-based analysis (uses text pattern matching, not the COBOL parser crate)
- No copybook resolution (COPY statements detected but not expanded)
- No call graph analysis (CALL statements detected but relationships not mapped)
- No data flow analysis
- No dead code detection
- No JCL analysis integration
- No CICS command inventory (just detects presence)
- No DB2 SQL complexity analysis
- No free-form COBOL support
- No nested program detection
- Complexity scoring is heuristic (not calibrated against real migration projects)
- Feature support percentages are static (not updated as crates improve)
- No comparison/diff between assessments (before/after migration progress)
- No batch/interactive classification of programs

## Functional Requirements

### FR-v3.0-1000: AST-Based Analysis via COBOL Parser
Replace text pattern matching with AST-based analysis using the `open-mainframe-cobol` parser. Extract program structure, data definitions, procedure flow, and feature usage from the parsed AST for accurate analysis.
- **Priority:** CRITICAL
- **Gap Type:** Incomplete implementation (uses text search instead of structured parsing)
- **Reference:** The `open-mainframe-cobol` crate already provides a full COBOL parser. Using it eliminates false positives from comments, string literals, and partial matches.

### FR-v3.0-1001: File and Directory Scanning
Implement file I/O for single files, directories, and PDS-style member libraries. Support recursive scanning with glob patterns and copybook resolution.
- **Priority:** CRITICAL
- **Gap Type:** Missing feature
- **Reference:** Real migration assessments scan hundreds or thousands of programs in batch. Manual source string injection is impractical.

### FR-v3.0-1002: Call Graph Analysis
Build a program-level call graph from CALL statements, CICS LINK/XCTL, and copybook COPY dependencies. Identify entry points, leaf programs, call depth, and circular dependencies.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** Migration planning requires understanding program interdependencies. Call graphs identify the migration order (leaves first) and blast radius of changes.

### FR-v3.0-1003: CICS Command Inventory
Categorize and count CICS commands by type (file, queue, terminal, program control, etc.). Identify which CICS features are used and their compatibility with the open-mainframe-cics implementation.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** CICS migration requires knowing exactly which commands are used. Commands not yet implemented in open-mainframe-cics need gap identification.

### FR-v3.0-1004: DB2 SQL Complexity Analysis
Analyze embedded SQL statements for complexity: simple SELECT, joins, subqueries, cursors, dynamic SQL, stored procedures. Map SQL features to PostgreSQL compatibility.
- **Priority:** MAJOR
- **Gap Type:** Missing feature
- **Reference:** DB2 SQL migration to PostgreSQL is a major effort area. SQL complexity directly impacts migration difficulty.

### FR-v3.0-1005: Dynamic Feature Support Tracking
Replace static feature support percentages with dynamic values derived from actual crate capabilities. Query each crate's test coverage and implemented features to compute real compatibility scores.
- **Priority:** MINOR
- **Gap Type:** Incomplete implementation (percentages are hardcoded)
- **Reference:** As crates improve, the assessment should automatically reflect increased compatibility.

### FR-v3.0-1006: Dead Code Detection
Identify unreachable paragraphs, unused data items, and orphan programs (never called by any other program in the assessment scope).
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** Dead code inflates migration estimates. Excluding it from assessment reduces false complexity.

### FR-v3.0-1007: Migration Progress Tracking
Support comparison between assessment runs. Track which issues have been resolved, which programs have been migrated, and overall progress toward migration completion.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** Migration projects span multiple phases. Progress tracking helps project management and stakeholder reporting.

### FR-v3.0-1008: JCL Dependency Analysis
Analyze JCL jobs that execute COBOL programs. Map DD statements to datasets, identify batch job chains, and include JCL complexity in migration estimates.
- **Priority:** MINOR
- **Gap Type:** Missing feature
- **Reference:** COBOL programs don't run in isolation — they're invoked by JCL jobs. Understanding the JCL layer is essential for complete migration planning.
