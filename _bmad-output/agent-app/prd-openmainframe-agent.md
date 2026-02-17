---
version: 'v1.0'
date: '2026-02-17'
status: 'draft'
inputDocuments:
  - product-brief-openmainframe-agent.md
  - research-domain-modernization.md
  - research-technical-copilotkit-langgraph.md
  - research-technical-openmainframe-tools.md
---

# Product Requirements Document: OpenMainframe Agent v1.0

## 1. Overview

OpenMainframe Agent is an AI-powered copilot for mainframe modernization built on CopilotKit + LangGraph, wrapping the OpenMainframe Rust compiler/runtime as agent tools. It provides interactive, conversational guidance for codebase assessment, COBOL compilation, JCL execution, code explanation, and dataset management through a modern web interface.

### 1.1 Objective

Deliver the first open-source, self-hosted mainframe modernization agent that enables enterprises to assess, understand, compile, execute, and plan migration of their mainframe workloads — all through a conversational AI interface.

### 1.2 Scope

**In Scope (MVP v1.0):**
- Assessment agent (scan, metrics, compatibility reports)
- Compilation agent (compile, check, error guidance)
- Execution agent (JCL run, COBOL interpret)
- Code explanation agent (natural language COBOL/JCL explanation)
- Dataset management agent (IDCAMS, catalog browsing)
- CopilotKit chat UI with generative UI components
- Human-in-the-loop approval for destructive operations
- CardDemo end-to-end integration test

**Out of Scope (MVP):**
- Migration planning with wave generation (v1.1 — requires call graph)
- Code transformation COBOL→Java (v2.0)
- Multi-tenant deployment (v2.0)
- VS Code extension (v1.2)
- REST API for CI/CD (v1.2)

---

## 2. Functional Requirements

### 2.1 Project Infrastructure

#### FR-001: Project Scaffolding

The application shall consist of a Next.js frontend and a Python LangGraph agent backend.

**Acceptance Criteria:**
- Next.js 14+ App Router with CopilotKit provider wrapping the application
- Python FastAPI server hosting LangGraph agent via AG-UI protocol
- `npm run dev` starts both frontend and agent concurrently
- CopilotKit sidebar layout with chat on right, workspace on left
- Environment configuration via `.env` files for API keys and agent URL

#### FR-002: CopilotKit Runtime Configuration

The CopilotKit runtime shall connect to the LangGraph agent in agent-lock mode.

**Acceptance Criteria:**
- `ExperimentalEmptyAdapter` used (agent handles LLM interaction)
- `LangGraphHttpAgent` connects to Python FastAPI backend
- API route at `/api/copilotkit` handles all CopilotKit requests
- Agent name matches between runtime config and LangGraph agent registration

#### FR-003: LangGraph Agent Graph

The agent shall be implemented as a LangGraph StateGraph with a router node dispatching to capability-specific nodes.

**Acceptance Criteria:**
- `AgentState` extends `CopilotKitState` with custom fields
- Router node classifies user intent and dispatches to correct capability node
- Each capability node (assess, compile, execute, explain, dataset) is a separate function
- Tool node handles OpenMainframe CLI tool execution
- Graph compiled with checkpointer (MemorySaver for dev, PostgreSQL for prod)
- Frontend actions injected via `state["copilotkit"]["actions"]`

#### FR-004: OpenMainframe CLI Wrapper

The agent shall wrap OpenMainframe CLI commands as LangGraph tools using Python subprocess.

**Acceptance Criteria:**
- Each CLI command wrapped as a `@tool`-decorated function
- Configurable path to OpenMainframe binary via environment variable
- Timeout of 120 seconds for compilation, 300 seconds for JCL execution
- Output truncated to 20KB max to prevent context overflow
- Structured return values with `success`, `output`, `errors`, `return_code` fields
- FileNotFoundError handled gracefully if binary not found

---

### 2.2 Assessment Agent

#### FR-010: Directory Scanning

The agent shall scan directories of COBOL/JCL source files and produce an inventory.

**Acceptance Criteria:**
- User provides a directory path via chat
- Agent discovers `.cbl`, `.cob`, `.CBL`, `.COB` files recursively
- Returns file count, total LOC, and per-file summary
- Handles empty directories gracefully with informative message
- Results displayed in generative UI table component

#### FR-011: Code Metrics Analysis

The agent shall compute complexity metrics for individual COBOL programs.

**Acceptance Criteria:**
- Cyclomatic complexity score per program
- Lines of code breakdown (total, code, comment, blank)
- Executable statement count and paragraph count
- Data item count from data division
- Maintainability index (0-100 scale)
- Technical debt estimation in hours
- Results rendered as a metrics card in generative UI

#### FR-012: Compatibility Assessment

The agent shall check COBOL programs against OpenMainframe compatibility rules.

**Acceptance Criteria:**
- 11 built-in compatibility rules checked per program
- Issues classified by severity: Info, Warning, High, Critical
- Feature support percentage reported per technology (VSAM, CICS, DB2, etc.)
- Aggregate portfolio-level compatibility score
- Results rendered as a compatibility report with severity-colored indicators

#### FR-013: Assessment Report Generation

The agent shall produce formatted assessment reports.

**Acceptance Criteria:**
- JSON output for programmatic consumption
- Markdown output for human reading
- Executive summary with file count, total LOC, average complexity, issue count
- Per-file detail section with program ID, metrics, and issues
- Recommendations section with deduplicated actionable items
- Report downloadable from the UI

---

### 2.3 Compilation Agent

#### FR-020: COBOL Compilation

The agent shall compile COBOL source files and report results.

**Acceptance Criteria:**
- Wraps `open-mainframe compile <source>` command
- Reports success/failure with exit code
- Parses compiler error messages with file, line, and column information
- Provides context around error locations when possible
- Compilation output displayed in a dedicated panel in generative UI

#### FR-021: Syntax Checking

The agent shall perform quick syntax validation without full compilation.

**Acceptance Criteria:**
- Wraps `open-mainframe check <source>` command
- Returns valid/invalid status with diagnostic messages
- Faster than full compilation for quick feedback
- Suitable for "check as you type" workflows

#### FR-022: Error Explanation and Fix Suggestions

The agent shall explain compilation errors in natural language and suggest fixes.

**Acceptance Criteria:**
- LLM analyzes compiler error output alongside source code context
- Provides plain English explanation of each error
- Suggests specific code changes to fix common errors
- References relevant COBOL language rules when applicable
- Does NOT hallucinate fixes — uses "I'm not sure" when uncertain

---

### 2.4 Execution Agent

#### FR-030: JCL Job Execution

The agent shall execute JCL jobs and report step-by-step results.

**Acceptance Criteria:**
- Wraps `open-mainframe run <jcl>` command
- Reports per-step execution status (return code, SYSOUT)
- Captures full job output (stdout + stderr)
- Execution results displayed in a timeline/step-progress generative UI
- **Human-in-the-loop:** Requires user approval before execution (via LangGraph interrupt)

#### FR-031: COBOL Interpretation

The agent shall run COBOL programs through the tree-walking interpreter.

**Acceptance Criteria:**
- Wraps `open-mainframe interpret <source>` command
- Captures DISPLAY output and final return code
- Reports runtime errors with context
- Execution output shown in a terminal-style panel

#### FR-032: JCL Structure Analysis

The agent shall parse JCL files and explain their structure.

**Acceptance Criteria:**
- Wraps `open-mainframe parse-jcl <jcl>` command
- Returns structured AST representation
- LLM explains each JOB, EXEC, DD statement in natural language
- Identifies programs executed (EXEC PGM=), datasets referenced (DD DSN=)
- Visualizes job flow in generative UI

---

### 2.5 Code Explanation Agent

#### FR-040: COBOL Program Explanation

The agent shall explain COBOL programs in natural language.

**Acceptance Criteria:**
- Accepts a COBOL source file path or pasted code
- Produces section-by-section explanation:
  - Identification Division: program name, author, purpose
  - Environment Division: file assignments, special names
  - Data Division: key data structures, record layouts, copybooks
  - Procedure Division: paragraph-by-paragraph business logic
- Highlights business rules and calculations
- Identifies external dependencies (CALL, COPY, EXEC SQL, EXEC CICS)

#### FR-041: JCL Job Explanation

The agent shall explain JCL jobs in natural language.

**Acceptance Criteria:**
- Parses JCL using `open-mainframe parse-jcl`
- Explains each step: what program runs, what inputs/outputs, conditions
- Identifies dataset dependencies and data flow between steps
- Explains symbolic parameters and their effects
- Describes conditional execution logic (IF/THEN/ELSE, COND)

#### FR-042: Business Rule Extraction

The agent shall identify and extract business rules from COBOL code.

**Acceptance Criteria:**
- Identifies COMPUTE, IF/EVALUATE blocks that encode business logic
- Extracts rules in structured format: condition → action
- Labels rules with descriptive names based on surrounding context
- Handles PERFORM loops and nested conditions
- Outputs as a numbered list with source code references

---

### 2.6 Dataset Management Agent

#### FR-050: Dataset Catalog Browsing

The agent shall list and describe datasets in the catalog.

**Acceptance Criteria:**
- Wraps `open-mainframe idcams LISTCAT` commands
- Returns dataset names, types, record formats, record lengths
- Filters by pattern (e.g., "show all VSAM datasets")
- Results displayed in a table component

#### FR-051: Dataset Operations

The agent shall perform IDCAMS operations through conversational interface.

**Acceptance Criteria:**
- DEFINE CLUSTER for creating VSAM datasets
- DELETE for removing datasets
- REPRO for copying/backing up datasets
- **Human-in-the-loop:** Requires approval before DELETE operations
- Operation results reported with success/failure status

---

### 2.7 Chat UI and Generative UI

#### FR-060: CopilotKit Sidebar Layout

The application shall use CopilotKit sidebar layout.

**Acceptance Criteria:**
- Chat panel on the right side (collapsible)
- Main workspace on the left for code viewer, dashboards, reports
- Responsive design supporting 1280px+ viewports
- Dark mode support (developer preference)
- CopilotKit default styling with custom branding

#### FR-061: Code Viewer Component

The workspace shall include a code viewer with syntax highlighting.

**Acceptance Criteria:**
- COBOL syntax highlighting (keywords, divisions, sections, paragraphs)
- JCL syntax highlighting (JOB, EXEC, DD, comments)
- Line numbers
- Scroll to specific line (for error navigation)
- Read-only display (no editing in MVP)

#### FR-062: Assessment Dashboard Component

The workspace shall render assessment results as an interactive dashboard.

**Acceptance Criteria:**
- Summary card: total files, total LOC, average complexity, issue count
- Complexity distribution chart (Low/Moderate/High/VeryHigh counts)
- Feature support matrix (technology × support percentage)
- Issues list with severity filtering
- Rendered via `useCoAgentStateRender` or tool-based generative UI

#### FR-063: Job Execution Timeline

JCL execution shall be visualized as a step timeline.

**Acceptance Criteria:**
- Each JCL step shown as a card with name, program, status
- Color-coded status: green (RC=0), yellow (RC=4), red (RC>4)
- SYSOUT expandable per step
- Real-time updates during execution (via predictive state updates)

#### FR-064: File Tree Navigation

The workspace shall include a file tree for uploaded codebases.

**Acceptance Criteria:**
- Tree view of scanned directory structure
- Icons distinguishing COBOL, JCL, copybook, data files
- Click to view file in code viewer
- Click to send file path to chat for analysis

---

### 2.8 Human-in-the-Loop

#### FR-070: Execution Approval

JCL execution and COBOL interpretation shall require user approval.

**Acceptance Criteria:**
- LangGraph `interrupt()` pauses before execution
- Interrupt message includes: file to execute, expected action, estimated duration
- User sees approve/reject buttons in chat via generative UI
- Rejection returns to chat with explanation request
- Approval proceeds with execution

#### FR-071: Dataset Modification Approval

Destructive dataset operations shall require user confirmation.

**Acceptance Criteria:**
- DELETE operations show dataset name and size before confirmation
- DEFINE shows all parameters for review
- User can modify parameters before confirming
- Cancel returns to chat without action

---

### 2.9 Agent State Management

#### FR-080: Shared State Schema

The agent state shall be synchronized between frontend and backend.

**Acceptance Criteria:**
- State includes: `project_path`, `source_files`, `assessment_results`, `compilation_results`, `execution_results`, `current_operation`
- Frontend reads state via `useCoAgent` hook
- Frontend can set `project_path` via `setState`
- State changes trigger UI re-renders automatically
- Predictive state updates stream during long operations

#### FR-081: Conversation Persistence

Chat history shall persist across page reloads (in production).

**Acceptance Criteria:**
- LangGraph checkpointer saves conversation state
- PostgreSQL backend for production persistence
- Thread ID maintained per session
- Previous messages loaded on reconnection

---

## 3. Non-Functional Requirements

### 3.1 Performance

| Metric | Target |
|--------|--------|
| Chat response time | < 5 seconds for simple queries |
| Assessment scan speed | < 5 minutes for 500-file codebase |
| Compilation tool response | < 30 seconds per file |
| JCL execution | < 5 minutes for typical batch job |
| UI render time | < 1 second for state updates |
| Concurrent users (MVP) | 1 (single-user, single-tenant) |

### 3.2 Security

- No source code transmitted to external services (LLM calls send summaries, not full source)
- OpenMainframe binary executes locally only
- Environment variables for all secrets (API keys)
- Input sanitization on all file paths (prevent directory traversal)
- Subprocess commands use allowlist (only OpenMainframe CLI, no arbitrary shell execution)

### 3.3 Reliability

- Graceful handling of OpenMainframe CLI crashes (subprocess error handling)
- Agent state recovery via LangGraph checkpointing
- Frontend displays clear error messages for all failure modes
- No data loss on browser refresh (conversation persisted)

### 3.4 Deployment

- Docker Compose for single-command local deployment
- Requirements: Node.js 20+, Python 3.11+, Rust toolchain (for OpenMainframe binary)
- Pre-built OpenMainframe binary included or built during setup
- `.env.example` with all required configuration variables documented

---

## 4. Technical Constraints

### 4.1 OpenMainframe CLI Dependencies

| Constraint | Impact | Mitigation |
|-----------|--------|------------|
| No `assess` CLI command | Cannot run assessment from agent | Add assess subcommand to CLI (prerequisite) |
| No `--format json` output | Agent must parse text output | Add JSON output flag to key commands |
| No streaming output | Cannot show real-time progress | Use predictive state updates with polling |
| Subprocess overhead | ~100ms per tool invocation | Acceptable for MVP; HTTP API in v1.1 |

### 4.2 LLM Dependencies

| Constraint | Impact | Mitigation |
|-----------|--------|------------|
| LLM API key required | Cannot function without API access | Support multiple providers (OpenAI, Anthropic) |
| LLM latency | 2-5 second response times | Stream responses via AG-UI protocol |
| Token limits | Large COBOL programs may exceed context | Chunk large files, summarize sections |
| Hallucination risk | May suggest incorrect COBOL fixes | Ground all suggestions in actual compiler output |

### 4.3 Prerequisites (Must Complete Before Agent Development)

1. **Add `assess` CLI command** to OpenMainframe binary exposing the assess crate
2. **Add `--format json` flag** to `compile`, `check`, `run`, `interpret`, `parse-jcl` commands
3. **Build OpenMainframe release binary** for the target platform
4. **Verify CardDemo programs compile** successfully on current OpenMainframe version

---

## 5. User Interaction Flows

### 5.1 Assessment Flow

```
User: "Assess the COBOL programs in /path/to/source"
  → Router Node: classify intent → ASSESS
  → Assess Node: call assess_scan tool
    → subprocess: open-mainframe assess scan /path/to/source --format json
    → parse JSON results
  → Update state: assessment_results = {...}
  → Generate response with summary
  → Render AssessmentDashboard in generative UI
User: "Which programs are most complex?"
  → Assess Node: analyze state.assessment_results
  → Sort by cyclomatic complexity, return top 10
  → Render complexity ranking table
```

### 5.2 Compilation Flow

```
User: "Compile HELLO.cbl"
  → Router Node: classify intent → COMPILE
  → Compile Node: call compile_cobol tool
    → subprocess: open-mainframe compile HELLO.cbl --format json
  → If success:
    → "Compilation successful. No errors found."
  → If failure:
    → Parse errors, show in CompilerOutput generative UI
    → LLM explains each error and suggests fixes
User: "Fix the error on line 42"
  → Explain Node: analyze error context + source code
  → Suggest specific COBOL code change
```

### 5.3 Execution Flow (with Human-in-the-Loop)

```
User: "Run HELLO.jcl"
  → Router Node: classify intent → EXECUTE
  → Execute Node: prepare execution plan
  → interrupt("Approve execution of HELLO.jcl?")
    → Render approval card with job details
User: [clicks Approve]
  → Execute Node: call run_jcl tool
    → subprocess: open-mainframe run HELLO.jcl
  → Update state with step results
  → Render JobExecutionTimeline in generative UI
User: [clicks Reject]
  → "Execution cancelled. What would you like to do instead?"
```

### 5.4 Code Explanation Flow

```
User: "Explain CALC-PREMIUM in POLCY01.cbl"
  → Router Node: classify intent → EXPLAIN
  → Explain Node: read source file
    → Optional: call lex tool for token analysis
  → LLM analyzes CALC-PREMIUM paragraph with COBOL expertise
  → Response: structured explanation with business rules
  → Render code with annotations in CodeViewer
```

---

## 6. Data Model

### 6.1 Agent State Schema

```python
class AgentState(CopilotKitState):
    # Project context
    project_path: str | None              # Root directory of mainframe source
    source_files: list[SourceFile]         # Discovered source files

    # Assessment state
    assessment_results: AssessmentReport | None

    # Compilation state
    compilation_results: list[CompilationResult]

    # Execution state
    execution_results: list[ExecutionResult]

    # Current operation
    current_operation: str | None          # "assessing", "compiling", "executing", etc.
    operation_progress: float              # 0.0 - 1.0
```

### 6.2 Core Data Types

```python
class SourceFile(TypedDict):
    path: str
    type: str          # "cobol", "jcl", "copybook", "bms", "data"
    size_bytes: int
    line_count: int

class AssessmentReport(TypedDict):
    total_files: int
    total_loc: int
    average_complexity: float
    programs: list[ProgramMetrics]
    issues: list[CompatibilityIssue]
    recommendations: list[str]
    feature_support: dict[str, float]      # {"VSAM": 0.9, "CICS": 0.75, ...}

class ProgramMetrics(TypedDict):
    program_id: str
    file_path: str
    loc: int
    complexity: float
    maintainability: float
    technical_debt_hours: float
    features_used: list[str]

class CompatibilityIssue(TypedDict):
    file_path: str
    line: int | None
    severity: str      # "info", "warning", "high", "critical"
    rule: str
    message: str
    recommendation: str

class CompilationResult(TypedDict):
    file_path: str
    success: bool
    errors: list[CompilerError]
    warnings: list[CompilerError]
    timestamp: str

class CompilerError(TypedDict):
    line: int
    column: int
    message: str
    severity: str

class ExecutionResult(TypedDict):
    jcl_file: str
    steps: list[StepResult]
    max_return_code: int
    timestamp: str

class StepResult(TypedDict):
    step_name: str
    program: str
    return_code: int
    output: str
    duration_ms: int
```

---

## 7. Testing Strategy

### 7.1 Unit Tests

| Component | Test Coverage |
|-----------|--------------|
| OpenMainframe tool wrappers | Subprocess mocking, output parsing, error handling |
| Router node | Intent classification for all agent types |
| State management | State updates, serialization, predictive updates |
| Data type validation | All TypedDict schemas |

### 7.2 Integration Tests

| Test | Description |
|------|-------------|
| CardDemo Assessment | Full directory scan of CardDemo COBOL programs |
| CardDemo Compilation | Compile all CardDemo .cbl files |
| CardDemo Execution | Run CardDemo JCL jobs end-to-end |
| Code Explanation | Explain 5 CardDemo programs, validate accuracy |
| Chat Roundtrip | Full request → agent → tool → response → UI cycle |

### 7.3 End-to-End Tests

| Scenario | Steps |
|----------|-------|
| First-time user | Install → configure → assess → compile → explain → execute |
| Assessment workflow | Upload directory → scan → view dashboard → export report |
| Compilation workflow | Select file → compile → see error → get fix suggestion |
| Execution workflow | Select JCL → approve → execute → view results |

---

## 8. Success Metrics

### 8.1 MVP Launch Metrics

| Metric | Target | How to Measure |
|--------|--------|---------------|
| Setup to first assessment | < 30 minutes | Timed user testing |
| CardDemo full assessment | < 5 minutes | Automated benchmark |
| Compilation success rate | 90%+ of CardDemo programs | CI test suite |
| Explanation accuracy | 90%+ validated by COBOL expert | Manual review |
| Chat response time | < 5 seconds (p95) | Application monitoring |
| No critical bugs | 0 open P0 bugs at launch | Issue tracker |

### 8.2 Post-Launch Metrics

| Metric | Target (3 months) | Target (6 months) |
|--------|-------------------|-------------------|
| GitHub stars | 500+ | 2,000+ |
| Active users | 50+ | 200+ |
| Enterprise evaluations | 5+ | 15+ |
| Community PRs | 10+ | 50+ |
| Programs assessed | 10,000+ | 100,000+ |

---

## 9. Risks and Mitigations

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| OpenMainframe CLI missing assess command | HIGH | BLOCKER | Implement as prerequisite before agent dev |
| LLM hallucinations in code explanation | MEDIUM | HIGH | Ground all responses in actual tool output |
| Large codebases exceed LLM context | MEDIUM | MEDIUM | Chunk files, summarize, use RAG patterns |
| CopilotKit API changes | LOW | MEDIUM | Pin versions, abstract integration layer |
| OpenMainframe compilation failures | MEDIUM | MEDIUM | Graceful error handling, clear user messaging |
| Poor COBOL syntax highlighting | LOW | LOW | Use existing COBOL TextMate grammars |

---

## 10. Dependencies

### 10.1 External Dependencies

| Dependency | Version | Purpose |
|-----------|---------|---------|
| Next.js | 14+ | Frontend framework |
| @copilotkit/react-core | latest | Agent-UI integration |
| @copilotkit/react-ui | latest | Chat UI components |
| @copilotkit/runtime | latest | Node.js middleware |
| LangGraph (Python) | latest | Agent framework |
| copilotkit (Python) | latest | CopilotKitState, AG-UI |
| ag-ui-langgraph (Python) | latest | FastAPI endpoint |
| FastAPI | latest | Python web server |
| OpenAI or Anthropic API | - | LLM provider |

### 10.2 Internal Dependencies (OpenMainframe)

| Dependency | Status | Required Change |
|-----------|--------|----------------|
| `open-mainframe` binary | READY | Must be built for target platform |
| `assess` CLI command | **MISSING** | Must add subcommand exposing assess crate |
| `--format json` output | **MISSING** | Must add to compile, check, run, interpret |
| CardDemo test data | READY | Available in repo |

---

## 11. Release Plan

### v1.0 (MVP) — Target: 8-10 weeks

**Week 1-2:** Infrastructure
- Project scaffolding (Next.js + CopilotKit + LangGraph + FastAPI)
- OpenMainframe CLI prerequisites (assess command, JSON output)
- OpenMainframe tool wrappers (Python subprocess)

**Week 3-4:** Assessment Agent
- Directory scanning tool + assess tool integration
- Assessment dashboard generative UI
- Compatibility report generation

**Week 5-6:** Compilation + Execution Agents
- Compile/check tools with error parsing
- JCL execution with human-in-the-loop approval
- Compiler output and job timeline UI components

**Week 7-8:** Explanation Agent + Polish
- Code explanation with LLM + AST grounding
- Business rule extraction
- File tree navigation, code viewer

**Week 9-10:** Testing + Documentation
- CardDemo end-to-end integration tests
- User documentation and setup guide
- Bug fixes and performance optimization
- Launch preparation

### v1.1 — Target: +6 weeks after v1.0

- Migration planning agent (requires Epic 1002: call graph)
- CICS command inventory (Epic 1003)
- DB2 SQL analysis (Epic 1004)
- Progress tracking dashboard (Epic 1006)
- Interactive debugger for COBOL interpreter

### v1.2 — Target: +4 weeks after v1.1

- REST API for CI/CD integration
- Multi-project support
- Assessment history and comparison
- VS Code extension (basic)
