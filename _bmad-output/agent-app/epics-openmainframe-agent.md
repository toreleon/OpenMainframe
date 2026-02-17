---
version: 'v1.0'
date: '2026-02-17'
status: 'draft'
inputDocuments:
  - prd-openmainframe-agent.md
  - architecture-openmainframe-agent.md
  - ux-design-openmainframe-agent.md
---

# Epics & Stories: OpenMainframe Agent v1.0

**BMAD Phase:** 3-Solutioning | **Step:** 9 of 11 | **Agent:** John 📋 Product Manager

---

## Epic Overview

| Epic | Title | PRD Requirements | Stories | Effort |
|------|-------|-----------------|---------|--------|
| E-100 | Prerequisites: OpenMainframe CLI | FR-004 (partial) | 3 | M |
| E-200 | Project Infrastructure | FR-001, FR-002, FR-003 | 5 | L |
| E-300 | Tool Layer | FR-004 | 4 | M |
| E-400 | Assessment Agent | FR-010..013 | 5 | L |
| E-500 | Compilation Agent | FR-020..022 | 4 | M |
| E-600 | Execution Agent | FR-030..032 | 5 | L |
| E-700 | Code Explanation Agent | FR-040..042 | 4 | M |
| E-800 | Dataset Management Agent | FR-050..051 | 3 | S |
| E-900 | Chat UI & Generative UI | FR-060..064 | 6 | L |
| E-1000 | Human-in-the-Loop | FR-070..071 | 3 | M |
| E-1100 | State Management & Persistence | FR-080..081 | 3 | M |
| E-1200 | Integration Testing | Testing strategy | 4 | L |

**Total: 12 epics, 49 stories**

---

## Epic E-100: Prerequisites — OpenMainframe CLI Enhancements

**Goal:** Add the `assess` CLI command and JSON output mode to the OpenMainframe binary so the agent can consume structured output.

**Dependency:** Must be completed BEFORE agent development begins. This is a Rust development task on the existing OpenMainframe codebase.

### Story E-100.1: Add `assess scan` CLI Command

**As a** modernization agent, **I need** an `open-mainframe assess scan <directory>` command **so that** I can scan COBOL directories and get assessment reports.

**Acceptance Criteria:**
- [ ] `open-mainframe assess scan <dir>` scans for `.cbl/.cob` files recursively
- [ ] Returns JSON report with: file count, total LOC, per-file metrics, issues, recommendations
- [ ] `--format json|text|markdown` flag controls output format (default: text)
- [ ] `--include <glob>` flag filters files by pattern
- [ ] Uses existing `open-mainframe-assess` crate Scanner and Analyzer
- [ ] Exits with code 0 on success, 1 on error

**Size:** M

### Story E-100.2: Add `assess file` CLI Command

**As a** modernization agent, **I need** an `open-mainframe assess file <path>` command **so that** I can assess individual COBOL files.

**Acceptance Criteria:**
- [ ] `open-mainframe assess file <path>` analyzes a single COBOL source file
- [ ] Returns JSON with: metrics, compatibility issues, feature support
- [ ] Uses AST-based analyzer when possible, falls back to text analyzer
- [ ] `--format json|text|markdown` flag

**Size:** S

### Story E-100.3: Add `--format json` to Key Commands

**As a** modernization agent, **I need** structured JSON output from compile, check, and run commands **so that** I can parse results reliably.

**Acceptance Criteria:**
- [ ] `open-mainframe compile <file> --format json` returns `{success, errors, warnings, output_path}`
- [ ] `open-mainframe check <file> --format json` returns `{valid, diagnostics}`
- [ ] `open-mainframe run <jcl> --format json` returns `{steps: [{name, program, return_code, output}], max_rc}`
- [ ] Existing text output is the default (no breaking changes)
- [ ] JSON output goes to stdout; errors/diagnostics to stderr

**Size:** M

---

## Epic E-200: Project Infrastructure

**Goal:** Scaffold the Next.js frontend and Python LangGraph agent backend with CopilotKit integration.

### Story E-200.1: Next.js Frontend Scaffolding

**As a** developer, **I need** a Next.js 14+ App Router project with CopilotKit configured **so that** I have a working frontend shell.

**Acceptance Criteria:**
- [ ] Next.js 14+ App Router project created in `openmainframe-agent/` directory
- [ ] `@copilotkit/react-core`, `@copilotkit/react-ui`, `@copilotkit/runtime` installed
- [ ] Root layout wraps app with `<CopilotKit runtimeUrl="/api/copilotkit">`
- [ ] Global styles include CopilotKit CSS (`@copilotkit/react-ui/styles.css`)
- [ ] `npm run dev` starts on port 3000

**Size:** S

### Story E-200.2: CopilotKit Runtime API Route

**As a** frontend, **I need** a CopilotKit runtime endpoint at `/api/copilotkit` **so that** the chat UI can communicate with the agent.

**Acceptance Criteria:**
- [ ] `src/app/api/copilotkit/route.ts` created with `CopilotRuntime`
- [ ] Uses `ExperimentalEmptyAdapter` (agent-lock mode)
- [ ] `LangGraphHttpAgent` points to `AGENT_URL` environment variable
- [ ] Agent name is `"modernization_agent"`
- [ ] Returns 500 with helpful message if agent is unreachable

**Size:** S

### Story E-200.3: Python Agent Server Scaffolding

**As a** developer, **I need** a FastAPI server hosting the LangGraph agent **so that** the frontend can connect via AG-UI protocol.

**Acceptance Criteria:**
- [ ] `agent/` directory with `main.py`, `pyproject.toml`, `.env.example`
- [ ] FastAPI app with CORS middleware allowing localhost:3000
- [ ] `add_langgraph_fastapi_endpoint` registers the agent at `/`
- [ ] `uvicorn` starts on port 8123
- [ ] Health check endpoint at `/health`

**Size:** S

### Story E-200.4: LangGraph Graph Skeleton

**As a** developer, **I need** a minimal LangGraph StateGraph with router and chat nodes **so that** I can verify the full CopilotKit → Agent → Response pipeline works.

**Acceptance Criteria:**
- [ ] `AgentState` extends `CopilotKitState` with `project_path` field
- [ ] `router_node` classifies intent (initially just routes everything to `chat`)
- [ ] `chat_node` responds to user messages via LLM
- [ ] Graph compiled with `MemorySaver` checkpointer
- [ ] End-to-end test: type message in CopilotKit UI → get response from agent

**Size:** M

### Story E-200.5: Concurrent Dev Server

**As a** developer, **I need** `npm run dev` to start both frontend and agent **so that** development is a single-command experience.

**Acceptance Criteria:**
- [ ] `package.json` has `dev` script using `concurrently` or similar
- [ ] Frontend starts on :3000, agent on :8123
- [ ] Both servers support hot reload
- [ ] `.env.example` documents all required variables

**Size:** S

---

## Epic E-300: Tool Layer

**Goal:** Implement Python subprocess wrappers for all OpenMainframe CLI commands needed by the agent.

### Story E-300.1: Base Subprocess Wrapper

**As a** tool developer, **I need** a reusable `run_cli()` function **so that** all tool wrappers have consistent error handling, timeouts, and output truncation.

**Acceptance Criteria:**
- [ ] `agent/src/tools/base.py` with `run_cli(args, timeout, cwd)` function
- [ ] Configurable binary path via `OPEN_MAINFRAME_BIN` env var
- [ ] 20KB output truncation on stdout and stderr
- [ ] Handles `TimeoutExpired`, `FileNotFoundError`, generic `Exception`
- [ ] Returns structured dict: `{success, stdout, stderr, return_code}`
- [ ] Path sanitization: rejects paths outside `WORKSPACE_ROOT`

**Size:** S

### Story E-300.2: Assessment Tools

**As an** assessment agent node, **I need** `assess_scan` and `assess_file` tools **so that** I can analyze codebases.

**Acceptance Criteria:**
- [ ] `@tool assess_scan(directory)` wraps `open-mainframe assess scan <dir> --format json`
- [ ] `@tool assess_file(file_path)` wraps `open-mainframe assess file <path> --format json`
- [ ] Timeout: 300 seconds for scan, 120 seconds for file
- [ ] Returns parsed JSON when possible, raw output on parse failure

**Size:** S

### Story E-300.3: Compilation & Execution Tools

**As** compile and execute agent nodes, **I need** tools for compile, check, run, and interpret **so that** I can build and run mainframe programs.

**Acceptance Criteria:**
- [ ] `@tool compile_cobol(source_file)` wraps `open-mainframe compile`
- [ ] `@tool check_cobol(source_file)` wraps `open-mainframe check`
- [ ] `@tool run_jcl(jcl_file)` wraps `open-mainframe run` (timeout: 300s)
- [ ] `@tool interpret_cobol(source_file)` wraps `open-mainframe interpret`
- [ ] All return structured dicts

**Size:** S

### Story E-300.4: Parse & Dataset Tools

**As** explain and dataset agent nodes, **I need** tools for JCL parsing, tokenization, and IDCAMS **so that** I can analyze and manage datasets.

**Acceptance Criteria:**
- [ ] `@tool parse_jcl(jcl_file)` wraps `open-mainframe parse-jcl`
- [ ] `@tool lex_cobol(source_file)` wraps `open-mainframe lex`
- [ ] `@tool list_catalog(pattern)` wraps `open-mainframe idcams LISTCAT`
- [ ] `@tool idcams_command(command)` wraps IDCAMS with verb allowlist
- [ ] IDCAMS verb allowlist: DEFINE, DELETE, REPRO, LISTCAT, PRINT
- [ ] Shell metacharacter rejection in IDCAMS commands

**Size:** S

---

## Epic E-400: Assessment Agent

**Goal:** Implement the assessment capability — scan codebases, compute metrics, generate compatibility reports.

### Story E-400.1: Assess Node Implementation

**As a** user, **I need** the agent to scan my COBOL directory when I ask **so that** I get an assessment report.

**Acceptance Criteria:**
- [ ] `assess_node` in `agent/src/nodes/assess.py`
- [ ] System prompt specialized for assessment tasks
- [ ] Binds `assess_scan`, `assess_file` tools + frontend tools
- [ ] Updates `state.assessment_results` with parsed results
- [ ] Updates `state.current_operation = "assessing"` during scan
- [ ] Calls `show_assessment_dashboard` frontend action after completion

**Size:** M

### Story E-400.2: Router — Assess Intent Classification

**As a** user, **I need** the router to recognize assessment-related requests **so that** I'm directed to the right agent.

**Acceptance Criteria:**
- [ ] Router classifies these as ASSESS: "assess my project", "scan the COBOL directory", "how complex is this codebase", "show compatibility report", "what features are used"
- [ ] Does NOT classify as ASSESS: "compile this file", "run this JCL", "explain this code"

**Size:** S

### Story E-400.3: Assessment Report Formatting

**As a** user, **I need** the agent to present assessment results clearly **so that** I can understand my codebase's modernization readiness.

**Acceptance Criteria:**
- [ ] Agent summarizes: total files, total LOC, average complexity, issue count
- [ ] Lists top 5 most complex programs
- [ ] Lists all critical/high severity issues
- [ ] Reports feature support percentages
- [ ] Provides actionable recommendations

**Size:** S

### Story E-400.4: Directory Discovery

**As a** user, **I need** to set my project directory **so that** the agent knows where to find source files.

**Acceptance Criteria:**
- [ ] User can say "set project to /path/to/source" or use UI project selector
- [ ] Agent updates `state.project_path`
- [ ] Agent discovers and lists source files in `state.source_files`
- [ ] Supports `.cbl`, `.cob`, `.CBL`, `.COB`, `.jcl`, `.JCL`, `.cpy`, `.CPY`
- [ ] Reports count by type: "Found 42 COBOL, 15 JCL, 23 copybook files"

**Size:** M

### Story E-400.5: Assessment Export

**As a** user, **I need** to export the assessment report **so that** I can share it with stakeholders.

**Acceptance Criteria:**
- [ ] User can say "export the assessment" or click Export button
- [ ] Generates downloadable JSON file
- [ ] Generates downloadable Markdown report
- [ ] Report includes timestamp, project path, all metrics

**Size:** S

---

## Epic E-500: Compilation Agent

**Goal:** Implement COBOL compilation with error explanation and fix suggestions.

### Story E-500.1: Compile Node Implementation

**As a** user, **I need** to compile COBOL files through the agent **so that** I get compilation results with error guidance.

**Acceptance Criteria:**
- [ ] `compile_node` in `agent/src/nodes/compile.py`
- [ ] System prompt specialized for COBOL compilation and error analysis
- [ ] Binds `compile_cobol`, `check_cobol` tools + frontend tools
- [ ] Appends result to `state.compilation_results`
- [ ] Calls `open_file_in_viewer` frontend action to show file on error

**Size:** M

### Story E-500.2: Router — Compile Intent Classification

**Acceptance Criteria:**
- [ ] Router classifies these as COMPILE: "compile HELLO.cbl", "build this program", "check syntax of X.cbl", "fix the compilation error"
- [ ] Includes follow-up messages like "fix the error on line 42" when previous context was compilation

**Size:** S

### Story E-500.3: Error Explanation

**As a** user, **I need** the agent to explain compilation errors in plain English **so that** I can understand and fix them.

**Acceptance Criteria:**
- [ ] Agent parses compiler error output (line, column, message)
- [ ] Provides plain English explanation of each error
- [ ] References the source code around the error location
- [ ] Suggests specific fix when possible (e.g., "change PIC X to PIC 9")
- [ ] Uses "I'm not sure" when fix is unclear (no hallucination)

**Size:** M

### Story E-500.4: Batch Compilation

**As a** user, **I need** to compile multiple files at once **so that** I can validate my entire codebase.

**Acceptance Criteria:**
- [ ] User can say "compile all COBOL files in the project"
- [ ] Agent iterates through `state.source_files` filtering COBOL files
- [ ] Reports per-file success/failure
- [ ] Summarizes: "35/42 programs compiled successfully, 7 had errors"
- [ ] Updates `state.compilation_results` with all results

**Size:** M

---

## Epic E-600: Execution Agent

**Goal:** Execute JCL jobs and interpret COBOL programs with HITL approval.

### Story E-600.1: Execute Node with HITL

**As a** user, **I need** the agent to ask for my approval before running code **so that** I maintain control over execution.

**Acceptance Criteria:**
- [ ] `execute_node` in `agent/src/nodes/execute.py`
- [ ] `interrupt()` called before `run_jcl` or `interpret_cobol` tool invocation
- [ ] Interrupt payload includes: file name, action description, estimated scope
- [ ] Approved: proceeds to tool execution
- [ ] Rejected: agent acknowledges and returns to chat

**Size:** M

### Story E-600.2: Router — Execute Intent Classification

**Acceptance Criteria:**
- [ ] Router classifies these as EXECUTE: "run HELLO.jcl", "execute the batch job", "interpret this COBOL program", "test HELLO.cbl"

**Size:** S

### Story E-600.3: JCL Execution with Step Results

**As a** user, **I need** to see per-step execution results **so that** I can understand how my batch job performed.

**Acceptance Criteria:**
- [ ] After `run_jcl` tool returns, agent parses step results
- [ ] Reports per step: name, program, return code, key output lines
- [ ] Highlights steps with RC > 0
- [ ] Appends to `state.execution_results`
- [ ] Calls `show_execution_timeline` frontend action

**Size:** M

### Story E-600.4: JCL Structure Analysis

**As a** user, **I need** the agent to explain JCL job structure before running **so that** I understand what will happen.

**Acceptance Criteria:**
- [ ] Agent uses `parse_jcl` tool to get AST before execution
- [ ] Explains each EXEC step: what program, what inputs/outputs
- [ ] Lists datasets referenced (DD statements)
- [ ] Explains conditional logic (IF/COND)

**Size:** M

### Story E-600.5: COBOL Interpretation

**As a** user, **I need** to run COBOL programs through the interpreter **so that** I can test without full compilation.

**Acceptance Criteria:**
- [ ] `interpret_cobol` tool execution with HITL approval
- [ ] Captures DISPLAY output
- [ ] Reports final return code
- [ ] Shows runtime errors with context

**Size:** S

---

## Epic E-700: Code Explanation Agent

**Goal:** Explain COBOL and JCL code in natural language with business rule extraction.

### Story E-700.1: Explain Node Implementation

**As a** user, **I need** the agent to explain legacy code **so that** I can understand business logic without mainframe expertise.

**Acceptance Criteria:**
- [ ] `explain_node` in `agent/src/nodes/explain.py`
- [ ] System prompt with deep COBOL/JCL expertise
- [ ] Reads source file content (via tool or direct file read)
- [ ] Optionally uses `lex_cobol` or `parse_jcl` for structural analysis
- [ ] No tool calls for explanation itself — pure LLM analysis

**Size:** M

### Story E-700.2: Router — Explain Intent Classification

**Acceptance Criteria:**
- [ ] Router classifies: "explain POLCY01.cbl", "what does CALC-PREMIUM do", "what are the business rules", "how does this JCL work", "what programs are called"

**Size:** S

### Story E-700.3: COBOL Program Explanation

**As a** user, **I need** section-by-section explanations of COBOL programs **so that** new team members can understand the code.

**Acceptance Criteria:**
- [ ] Explains each division (ID, Environment, Data, Procedure)
- [ ] Identifies key data structures and their purposes
- [ ] Explains paragraph-level business logic
- [ ] Highlights external dependencies (CALL, COPY, EXEC SQL/CICS)
- [ ] Uses clear, non-technical language where possible

**Size:** M

### Story E-700.4: Business Rule Extraction

**As a** user, **I need** business rules extracted in a structured format **so that** I can document and validate them during migration.

**Acceptance Criteria:**
- [ ] Identifies COMPUTE, IF/EVALUATE blocks encoding business logic
- [ ] Formats as: "Rule N: When [condition], then [action]"
- [ ] Labels rules with descriptive names from context
- [ ] Includes source line references
- [ ] Handles nested conditions and PERFORM loops

**Size:** M

---

## Epic E-800: Dataset Management Agent

**Goal:** Browse catalogs and manage datasets through conversational interface.

### Story E-800.1: Dataset Node Implementation

**As a** user, **I need** to browse and manage datasets through chat **so that** I don't need to memorize IDCAMS commands.

**Acceptance Criteria:**
- [ ] `dataset_node` in `agent/src/nodes/dataset.py`
- [ ] System prompt with VSAM/QSAM/PDS expertise
- [ ] Binds `list_catalog`, `idcams_command` tools
- [ ] HITL interrupt before DELETE operations

**Size:** M

### Story E-800.2: Router — Dataset Intent Classification

**Acceptance Criteria:**
- [ ] Router classifies: "list datasets", "show the catalog", "create a VSAM cluster", "delete dataset X", "what datasets exist"

**Size:** S

### Story E-800.3: Catalog Browsing

**As a** user, **I need** to explore datasets with natural language **so that** I can understand my data landscape.

**Acceptance Criteria:**
- [ ] "Show all datasets" → runs LISTCAT ALL
- [ ] "Show VSAM datasets" → filters by type
- [ ] Presents results in a readable table format
- [ ] Shows: name, type (KSDS/ESDS/QSAM/PDS), record format, record length

**Size:** S

---

## Epic E-900: Chat UI & Generative UI

**Goal:** Build the frontend workspace with code viewer, assessment dashboard, job timeline, and file tree.

### Story E-900.1: Three-Column Layout Shell

**As a** user, **I need** a workspace layout with file tree, workspace, and chat **so that** I can navigate code while conversing with the agent.

**Acceptance Criteria:**
- [ ] Left panel: File Tree (240px, collapsible)
- [ ] Center panel: Workspace (flexible, min 500px)
- [ ] Right panel: CopilotSidebar (400px)
- [ ] Drag-to-resize borders between panels
- [ ] Dark theme matching UX design color scheme
- [ ] Responsive: collapses file tree at <1280px

**Size:** M

### Story E-900.2: File Tree Panel

**As a** user, **I need** to browse project files in a tree view **so that** I can select files for analysis.

**Acceptance Criteria:**
- [ ] Reads `state.source_files` to build tree structure
- [ ] Icons by file type: 📄 COBOL, 📋 JCL, 📎 Copybook, 📊 Data
- [ ] Click file → opens in Code Viewer tab
- [ ] Right-click context menu: Explain, Compile, Check Syntax, Assess
- [ ] Search/filter input at top
- [ ] Expandable/collapsible directories

**Size:** M

### Story E-900.3: Code Viewer Component

**As a** user, **I need** a code viewer with COBOL syntax highlighting **so that** I can read source code clearly.

**Acceptance Criteria:**
- [ ] COBOL syntax highlighting (divisions, verbs, data names, literals, comments)
- [ ] JCL syntax highlighting (JOB, EXEC, DD, comments)
- [ ] Line numbers in gutter
- [ ] Scroll-to-line capability (for error navigation)
- [ ] Read-only (no editing in MVP)
- [ ] Gutter annotations: 💡 explanation, ⚠ issue, 🔴 error

**Size:** L

### Story E-900.4: Assessment Dashboard Component

**As a** user, **I need** a visual assessment dashboard **so that** I can quickly understand my codebase's modernization readiness.

**Acceptance Criteria:**
- [ ] Summary metric cards: files, LOC, avg complexity, issues, compatibility %, tech debt
- [ ] Complexity distribution bar chart
- [ ] Feature support matrix (technology × percentage, color-coded)
- [ ] Issues table with severity filtering and sorting
- [ ] Programs table sortable by complexity, LOC, maintainability
- [ ] Clickable rows → open program in Code Viewer
- [ ] Export button (JSON, Markdown)

**Size:** L

### Story E-900.5: Job Execution Timeline Component

**As a** user, **I need** a visual timeline of JCL execution **so that** I can see step-by-step results.

**Acceptance Criteria:**
- [ ] Connected node visualization (step1 → step2 → step3)
- [ ] Color-coded by return code: green (0), yellow (4), red (>4)
- [ ] Click step → expand detail panel (program, RC, SYSOUT, DD statements)
- [ ] SYSOUT displayed in monospace terminal-style panel
- [ ] Summary: step count, max RC, total duration

**Size:** M

### Story E-900.6: Generative UI Cards for Chat

**As a** user, **I need** rich inline cards in the chat **so that** I can see tool results visually.

**Acceptance Criteria:**
- [ ] `AssessmentCard`: inline summary with key metrics
- [ ] `CompilerOutputCard`: errors with line references and fix suggestions, [Go↗] button
- [ ] `ApprovalCard`: HITL approve/reject with job details
- [ ] `ProgressCard`: operation name + progress bar
- [ ] `ExplanationCard`: code snippet with inline annotations
- [ ] All cards follow dark theme design
- [ ] Cards are registered via `useCopilotAction` with `available: "disabled"` (render-only)

**Size:** L

---

## Epic E-1000: Human-in-the-Loop

**Goal:** Implement approval workflows for execution and destructive operations.

### Story E-1000.1: Execution Approval Flow

**As a** user, **I need** to approve JCL/COBOL execution before it runs **so that** I maintain control.

**Acceptance Criteria:**
- [ ] `interrupt()` in execute_node before `run_jcl` and `interpret_cobol`
- [ ] Interrupt payload: `{action, file, description}`
- [ ] Frontend renders ApprovalCard with Approve/Cancel buttons
- [ ] Approve → tool executes, results shown
- [ ] Cancel → agent says "Execution cancelled" and waits

**Size:** M

### Story E-1000.2: Dataset Modification Approval

**As a** user, **I need** to confirm before dataset deletion **so that** I don't accidentally lose data.

**Acceptance Criteria:**
- [ ] `interrupt()` in dataset_node before DELETE IDCAMS commands
- [ ] Shows dataset name in approval card
- [ ] Approve → DELETE executes
- [ ] Cancel → agent acknowledges

**Size:** S

### Story E-1000.3: Frontend Interrupt Rendering

**As a** user, **I need** approval cards rendered properly in chat **so that** I can make informed decisions.

**Acceptance Criteria:**
- [ ] `useLangGraphInterrupt` hook registered in main page
- [ ] Renders ApprovalCard component with event data
- [ ] `resolve()` called with `{approved: true/false, reason?}`
- [ ] Card grays out after decision
- [ ] Cannot re-click after decision

**Size:** M

---

## Epic E-1100: State Management & Persistence

**Goal:** Implement bi-directional state sync and conversation persistence.

### Story E-1100.1: Agent State Sync

**As a** frontend, **I need** reactive state from the agent **so that** the UI updates in real-time.

**Acceptance Criteria:**
- [ ] `useAgentState` hook wraps `useCoAgent<AgentState>`
- [ ] State changes in agent → UI components re-render
- [ ] `setState({project_path: "..."})` from frontend → agent receives update
- [ ] All state fields defined in matching TypeScript and Python types

**Size:** M

### Story E-1100.2: Predictive State Updates

**As a** user, **I need** to see progress during long operations **so that** I know the agent is working.

**Acceptance Criteria:**
- [ ] Agent updates `current_operation` and `operation_progress` mid-node
- [ ] Frontend shows ProgressCard or StatusBar during operations
- [ ] Progress resets when operation completes

**Size:** S

### Story E-1100.3: PostgreSQL Persistence (Production)

**As an** operator, **I need** conversation history persisted **so that** users can resume sessions.

**Acceptance Criteria:**
- [ ] `DATABASE_URL` env var enables PostgreSQL checkpointer
- [ ] `AsyncPostgresSaver` replaces `MemorySaver` when configured
- [ ] Thread ID maintained per session
- [ ] Previous messages loaded on reconnection
- [ ] Schema migration runs on first start

**Size:** M

---

## Epic E-1200: Integration Testing

**Goal:** Validate the complete system against the CardDemo application.

### Story E-1200.1: CardDemo Assessment Test

**As a** QA engineer, **I need** the agent to successfully assess all CardDemo COBOL programs **so that** I know assessment works on real code.

**Acceptance Criteria:**
- [ ] Agent scans `aws-mainframe-modernization-carddemo/app/cbl/`
- [ ] Reports correct file count
- [ ] Produces valid metrics for each program
- [ ] Generates compatibility report without errors
- [ ] Completes within 5 minutes

**Size:** M

### Story E-1200.2: CardDemo Compilation Test

**As a** QA engineer, **I need** the agent to compile CardDemo programs **so that** I know compilation works.

**Acceptance Criteria:**
- [ ] Agent compiles each CardDemo .cbl file
- [ ] Reports success/failure accurately
- [ ] Compiler errors are parseable and explainable
- [ ] 90%+ compilation success rate

**Size:** M

### Story E-1200.3: CardDemo Execution Test

**As a** QA engineer, **I need** the agent to run CardDemo JCL jobs **so that** I know execution works end-to-end.

**Acceptance Criteria:**
- [ ] Agent executes CardDemo JCL with HITL approval
- [ ] Step results match expected return codes
- [ ] SYSOUT captured and displayed
- [ ] Timeline component renders correctly

**Size:** M

### Story E-1200.4: Code Explanation Accuracy Test

**As a** QA engineer, **I need** the agent's code explanations validated **so that** I can trust them.

**Acceptance Criteria:**
- [ ] Agent explains 5 different CardDemo programs
- [ ] Each explanation reviewed by COBOL-knowledgeable person
- [ ] 90%+ accuracy on business rule identification
- [ ] No factually incorrect statements about program behavior

**Size:** M

---

## Dependency Graph

```
E-100 (Prerequisites)
  │
  ├──→ E-300 (Tool Layer)
  │       │
  │       ├──→ E-400 (Assessment Agent)
  │       ├──→ E-500 (Compilation Agent)
  │       ├──→ E-600 (Execution Agent)
  │       ├──→ E-700 (Explanation Agent)
  │       └──→ E-800 (Dataset Agent)
  │
  └──→ E-200 (Infrastructure)
          │
          ├──→ E-900 (UI Components)
          ├──→ E-1000 (HITL)
          └──→ E-1100 (State/Persistence)

All ──→ E-1200 (Integration Testing)
```

---

## Implementation Batch Order

| Batch | Epic(s) | Stories | Description |
|-------|---------|---------|-------------|
| 1 | E-100 | 100.1, 100.2, 100.3 | OpenMainframe CLI prerequisites |
| 2 | E-200 | 200.1, 200.2, 200.3, 200.4, 200.5 | Project scaffolding |
| 3 | E-300 | 300.1, 300.2, 300.3, 300.4 | Tool layer |
| 4 | E-400 | 400.1, 400.2, 400.3, 400.4, 400.5 | Assessment agent |
| 5 | E-500 | 500.1, 500.2, 500.3, 500.4 | Compilation agent |
| 6 | E-600 | 600.1, 600.2, 600.3, 600.4, 600.5 | Execution agent |
| 7 | E-700 | 700.1, 700.2, 700.3, 700.4 | Explanation agent |
| 8 | E-800 | 800.1, 800.2, 800.3 | Dataset agent |
| 9 | E-900 | 900.1, 900.2, 900.3, 900.4, 900.5, 900.6 | UI components |
| 10 | E-1000 | 1000.1, 1000.2, 1000.3 | Human-in-the-loop |
| 11 | E-1100 | 1100.1, 1100.2, 1100.3 | State & persistence |
| 12 | E-1200 | 1200.1, 1200.2, 1200.3, 1200.4 | Integration testing |
