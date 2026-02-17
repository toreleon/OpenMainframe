---
stepsCompleted: [1, 2, 3]
phase: 1-analysis
bmadWorkflow: Technical Research (TR)
agent: Mary (Analyst)
date: 2026-02-17
---

# OpenMainframe CLI → Agent Tool Mapping Research

**Date:** February 2026
**BMAD Phase:** 1-Analysis | **Step:** 3 of 11 | **Agent:** Mary 📊 Business Analyst

---

## 1. Current OpenMainframe CLI Commands

The OpenMainframe binary (`open-mainframe`) exposes these commands:

| Command | Crate | Description | Agent Tool Potential |
|---------|-------|-------------|---------------------|
| `compile <source>` | cobol | Compile COBOL to native executable | HIGH — compile & validate |
| `check <source>` | cobol | Syntax check without compilation | HIGH — quick validation |
| `interpret <source>` | runtime | Tree-walking COBOL execution | HIGH — debugging/testing |
| `run <jcl>` | jcl | Execute JCL job | HIGH — job execution |
| `parse-jcl <jcl>` | jcl | Show JCL AST structure | HIGH — JCL analysis |
| `lex <source>` | cobol | Show COBOL tokens | MEDIUM — low-level analysis |
| `cics` | cics/tui | Interactive CICS terminal (3270) | LOW — interactive, hard to automate |
| `idcams` | dataset | IDCAMS dataset management | HIGH — dataset operations |
| `gdg` | dataset | Generation Data Group management | MEDIUM — GDG operations |
| `bms` | cics | BMS map compilation | MEDIUM — screen analysis |
| `db2` | db2 | DB2 SQL preprocessing | HIGH — SQL analysis |
| `config init` | core | Generate configuration file | LOW — one-time setup |
| `completions` | core | Shell completions | NONE — dev tooling only |

**CRITICAL GAP:** No `assess` command exists in the CLI despite the assess crate being functional as a library.

---

## 2. Assess Crate Capabilities (Library Only)

### 2.1 What's Implemented

| Component | File | Status | Description |
|-----------|------|--------|-------------|
| Text Analyzer | `analyzer.rs` | DONE | Pattern-matching metrics: LOC, complexity, feature detection |
| AST Analyzer | `ast_analyzer.rs` | PARTIAL | COBOL parser integration with text fallback |
| Compatibility | `compatibility.rs` | DONE | 11 built-in rules, feature support percentages |
| Metrics | `metrics.rs` | DONE | Maintainability index, technical debt estimation |
| Report Generator | `report.rs` | DONE | Text, Markdown, JSON, HTML output formats |
| Scanner | `scanner.rs` | DONE | Directory walking, glob patterns, copybook resolution |

### 2.2 Analysis Capabilities

**Metrics Calculated:**
- Total lines, code lines, blank lines, comment lines
- Executable statement count, paragraph count, data item count
- Cyclomatic complexity (decision point counting)
- Maintainability Index (0-100 scale)
- Technical debt estimation (hours)

**Features Detected (11 patterns):**
- VSAM (INDEXED), Sequential Files
- DB2 (EXEC SQL), IMS (EXEC DLI), CICS (EXEC CICS)
- Subprogram CALL, COPY statements
- DISPLAY, ACCEPT, STRING/UNSTRING, INSPECT, COMPUTE

**Compatibility Rules (11):**
- Platform-specific: IMS/DL1 (CRITICAL), UPON CONSOLE, DATE/TIME accepts
- File handling: RELATIVE files (HIGH), EXTERNAL assignment (WARNING)
- Deprecated: ALTER (HIGH), GO TO DEPENDING (WARNING), ENTRY statements

**Feature Support Percentages (hardcoded):**

| Feature | Support Level |
|---------|--------------|
| Sequential Files | 100% |
| VSAM KSDS | 90% |
| VSAM ESDS | 90% |
| VSAM RRDS | 80% |
| DB2 SQL | 85% |
| CICS Commands | 75% |
| BMS Maps | 70% |
| IMS/DL1 | 0% |
| JCL | 80% |
| SORT Utility | 90% |

### 2.3 What's NOT Implemented

| Epic | Feature | Status | Agent Impact |
|------|---------|--------|-------------|
| 1002 | Call Graph Analysis | NOT STARTED | Cannot determine migration order |
| 1003 | CICS Command Inventory | NOT STARTED | Cannot quantify CICS migration effort |
| 1004 | DB2 SQL Complexity Analysis | NOT STARTED | Cannot assess SQL migration effort |
| 1005 | Dead Code Detection | NOT STARTED | Cannot exclude dead code from estimates |
| 1006 | Migration Progress Tracking | NOT STARTED | Cannot measure progress over time |
| 1007 | JCL Dependency Analysis | NOT STARTED | Cannot map job-to-program dependencies |

---

## 3. Agent Tool Mapping

### 3.1 Assessment Tools

| Agent Action | OpenMainframe Command | Implementation | Priority |
|-------------|----------------------|----------------|----------|
| Scan COBOL directory | `assess scan <dir>` | **NEEDS CLI** — library exists | P0 |
| Get assessment report | `assess report <dir> --format json` | **NEEDS CLI** — library exists | P0 |
| Check compatibility | `assess compat <file>` | **NEEDS CLI** — library exists | P0 |
| Get complexity metrics | `assess metrics <file>` | **NEEDS CLI** — library exists | P1 |
| Build call graph | `assess graph <dir>` | **NEEDS IMPL** — Epic 1002 | P1 |
| Inventory CICS commands | `assess cics <dir>` | **NEEDS IMPL** — Epic 1003 | P2 |
| Analyze DB2 SQL | `assess sql <dir>` | **NEEDS IMPL** — Epic 1004 | P2 |
| Detect dead code | `assess dead-code <dir>` | **NEEDS IMPL** — Epic 1005 | P2 |

**Alternative for P0:** The agent can use the assess crate directly as a Python extension (via PyO3/maturin) or by building a thin JSON CLI wrapper in Rust.

### 3.2 Compilation & Validation Tools

| Agent Action | OpenMainframe Command | Status | Notes |
|-------------|----------------------|--------|-------|
| Compile COBOL | `compile <source> -o <output>` | READY | Returns exit code + errors |
| Syntax check | `check <source>` | READY | Quick validation |
| Tokenize | `lex <source>` | READY | Low-level token analysis |

### 3.3 Execution Tools

| Agent Action | OpenMainframe Command | Status | Notes |
|-------------|----------------------|--------|-------|
| Run JCL job | `run <jcl>` | READY | Multi-step execution with return codes |
| Interpret COBOL | `interpret <source>` | READY | Tree-walking execution |
| Parse JCL | `parse-jcl <jcl>` | READY | Returns AST structure |

### 3.4 Dataset Tools

| Agent Action | OpenMainframe Command | Status | Notes |
|-------------|----------------------|--------|-------|
| Define VSAM cluster | `idcams DEFINE CLUSTER ...` | READY | Via IDCAMS sub-commands |
| List catalog | `idcams LISTCAT` | READY | Shows dataset inventory |
| Delete dataset | `idcams DELETE ...` | READY | With confirmation |
| Manage GDG | `gdg <subcommand>` | READY | Generation data groups |

### 3.5 Preprocessing Tools

| Agent Action | OpenMainframe Command | Status | Notes |
|-------------|----------------------|--------|-------|
| Preprocess DB2 SQL | `db2 preprocess <source>` | READY | Extract EXEC SQL blocks |
| Compile BMS maps | `bms <mapfile>` | READY | Screen definition analysis |

---

## 4. Gaps Between Current State and Agent Needs

### 4.1 Critical Gaps (Must Fix for MVP)

| Gap | Impact | Solution |
|-----|--------|----------|
| **No `assess` CLI command** | Cannot run assessment from agent tools | Add `assess` subcommand group to CLI |
| **No JSON output mode for most commands** | Agent cannot parse structured results | Add `--format json` flag to compile, check, run |
| **No call graph** | Cannot determine migration order or dependencies | Implement Epic 1002 or build in Python |
| **No streaming output** | Agent cannot show real-time progress | Add `--stream` flag for long-running operations |

### 4.2 Important Gaps (Needed for Full Agent)

| Gap | Impact | Solution |
|-----|--------|----------|
| No CICS command inventory | Cannot quantify CICS migration scope | Implement Epic 1003 |
| No SQL complexity analysis | Cannot assess DB2 migration effort | Implement Epic 1004 |
| No dead code detection | Overestimates migration effort | Implement Epic 1005 |
| No progress tracking | Cannot show migration progress | Implement Epic 1006 |
| No JCL dependency analysis | Cannot map batch job chains | Implement Epic 1007 |
| Hardcoded support percentages | Become stale as crates evolve | Dynamic capability query |
| No debugger API | Cannot support interactive debugging | Build debug protocol for interpret |

### 4.3 Nice-to-Have Gaps

| Gap | Impact | Solution |
|-----|--------|----------|
| No REST API server mode | Must use subprocess for every call | Add HTTP server mode |
| No language server protocol | Cannot integrate with VS Code | Build LSP server |
| No WASM compilation | Cannot run in browser | Investigate WASM target |

---

## 5. Recommended Agent Tool Architecture

### Option A: Subprocess Wrapping (MVP — Fastest)

```
LangGraph Agent (Python)
    └── subprocess.run("open-mainframe <cmd>")
        └── OpenMainframe CLI (Rust binary)
```

**Pros:** No Rust changes needed (except adding assess CLI), simple to implement
**Cons:** Subprocess overhead, text parsing fragility, no streaming

### Option B: JSON API Mode (Recommended — Phase 2)

```
LangGraph Agent (Python)
    └── HTTP requests to localhost:8080
        └── OpenMainframe HTTP Server (Rust, using open-mainframe-deploy)
```

**Pros:** Structured JSON responses, streaming support, connection pooling
**Cons:** Requires building HTTP API endpoints in Rust

### Option C: Python Extension via PyO3 (Future)

```
LangGraph Agent (Python)
    └── import open_mainframe (native Python extension)
        └── OpenMainframe Library (Rust via PyO3/maturin)
```

**Pros:** Zero overhead, full type safety, streaming
**Cons:** Significant build complexity, cross-platform challenges

### Recommendation

**Start with Option A** (subprocess wrapping) for MVP. The only blocker is adding the `assess` CLI command and `--format json` output to existing commands. Migrate to **Option B** as the agent matures.

---

## 6. OpenMainframe Crate Interaction Map for Agent

```
Agent Tools Layer (Python)
    │
    ├── assess_tool ──→ open-mainframe-assess
    │                       ├── uses cobol (parser/AST)
    │                       └── produces reports (text/json/md/html)
    │
    ├── compile_tool ──→ open-mainframe-cobol
    │                       ├── preprocessor (COPY, REPLACE)
    │                       ├── lexer → parser → semantic
    │                       └── codegen (LLVM) or interpreter
    │
    ├── run_tool ──→ open-mainframe-jcl
    │                   ├── JCL parser + executor
    │                   ├── step execution with COND
    │                   └── uses dataset (DD allocation)
    │
    ├── interpret_tool ──→ open-mainframe-runtime
    │                         ├── tree-walking interpreter
    │                         ├── uses encoding (EBCDIC, COMP-3)
    │                         └── uses dataset (file I/O)
    │
    ├── dataset_tool ──→ open-mainframe-dataset
    │                       ├── QSAM, VSAM, PDS operations
    │                       ├── catalog management
    │                       └── IDCAMS commands
    │
    └── explain_tool ──→ LLM + open-mainframe-cobol (AST)
                            ├── lex → token analysis
                            └── parse-jcl → JCL AST
```

---

## 7. Test Data Available

### CardDemo Application

The repository includes the AWS Mainframe Modernization CardDemo sample:

```
aws-mainframe-modernization-carddemo/
├── app/cbl/           # COBOL programs
├── app/cpy/           # Copybooks
├── app/jcl/           # JCL jobs
├── app/bms/           # BMS screen maps
└── app/data/          # Test datasets
```

This provides a realistic test case for the agent — a multi-program COBOL/CICS/VSAM application with JCL batch processing.

### Example Programs

```
examples/hello-world/
├── HELLO.cbl          # Simple DISPLAY program
└── HELLO.jcl          # JCL execution
```

---

## 8. Summary & Recommendations

1. **Add `assess` CLI command immediately** — This is the #1 blocker for the agent MVP
2. **Add `--format json` to all commands** — Structured output is essential for agent tools
3. **Start with subprocess wrapping** — Fastest path to working agent
4. **Use CardDemo as integration test** — Real-world COBOL/CICS/VSAM application
5. **Implement call graph (Epic 1002) early** — Critical for migration planning
6. **Build Python tool wrappers with proper error handling** — Timeouts, output truncation, structured errors
7. **Consider HTTP API mode for Phase 2** — Better streaming and structured responses
