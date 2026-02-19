# Ralph Loop Prompt: Agent-Native UX Research for OpenMainframe

## Mission

Research and design the optimal UX for OpenMainframe as an **agent-native application**. This means understanding TWO domains deeply and fusing them:

1. **OpenMainframe itself** — What does the CLI do? What are the real user workflows? What output does each command produce? Where are the pain points?
2. **Agent-native UX** — How should an AI agent present these capabilities? What interaction models exist? What works, what doesn't?

The output is a complete UX specification that replaces the existing `_bmad-output/agent-app/ux-design-openmainframe-agent.md` (old-school 3-panel IDE layout).

---

## Context: OpenMainframe CLI — The Tool We're Wrapping

The `open-mainframe` CLI is a Rust binary with 14 commands. The agent wraps these commands via bash and presents results to the user. **Every UX decision must be grounded in what these commands actually do and output.**

### Command Inventory

| Command | Purpose | Output (JSON) | Typical Use |
|---------|---------|---------------|-------------|
| `assess scan <dir>` | Scan codebase for modernization readiness | Complexity scores, feature matrix, compatibility issues per file | First thing a migration architect does |
| `assess file <file>` | Deep-assess a single COBOL program | Detailed metrics, LOC, complexity, features used, issues | Drill into a specific program |
| `compile <file>` | Compile COBOL to object code via LLVM | status, program_id, diagnostics (errors/warnings with lines), symbol count | Developer iteration loop |
| `check <file>` | Syntax + semantic check (no codegen) | Same as compile but faster (no LLVM) | Quick validation during editing |
| `run <jcl>` | Execute a JCL job | job_name, return_code, steps[] with per-step RC/stdout/stderr | Batch job testing |
| `interpret <file>` | Tree-walk execute COBOL (no compile) | output lines, return_code, diagnostics | Quick run without build step |
| `parse-jcl <file>` | Parse JCL structure | job_name, class, steps[] with exec/parm/dd_statements | Understand job before running |
| `lex <file>` | Tokenize COBOL source | Token list with types | Debug lexer, understand source structure |
| `bms <file>` | Compile BMS maps to COBOL copybooks | Mapset info, generated copybook path | CICS screen development |
| `cics <file>` | Interactive 3270 terminal session | (Interactive TUI, not JSON) | Test CICS transactions live |
| `idcams <cmd>` | Dataset management (DEFINE, DELETE, REPRO, etc.) | Command result, dataset info | Data setup and management |
| `gdg create/list/delete/newgen` | Generation Data Group management | GDG base info, generation list | Versioned dataset management |
| `db2 preprocess <file>` | Extract EXEC SQL from COBOL | SQL statements, host variables, DBRM | Pre-process before DB2 execution |
| `config show/init/paths` | Show/manage configuration | Config values, paths | Setup and troubleshooting |

**Global flags:** `--format json` (machine-readable), `-v` (verbose logging)

### Key Architectural Facts

- **Source code stays local.** The agent server talks to a WebSocket bridge on the user's machine. Commands execute locally. Source never uploads.
- **Agent has one tool: bash.** The agent calls `open-mainframe <command> --format json` via bash and parses the JSON output.
- **AG-UI protocol streams events.** The frontend receives SSE events: TEXT_MESSAGE, TOOL_CALL, TOOL_CALL_RESULT, CUSTOM (thinking), RUN_FINISHED/ERROR.

### Real User Workflows (What People Actually Do)

**Workflow 1: Migration Assessment (Priya, Migration Architect)**
```
Intent: "How ready is this codebase for migration?"
Steps the agent takes:
  1. assess scan ./app/cbl → inventory of 42 files, complexity scores
  2. For flagged files: assess file POLCY01.cbl → detailed issues
  3. Synthesize: "78% compatible, 12 issues, 340 hrs estimated tech debt"
Output artifacts: Assessment report, program-by-program table, issue list
```

**Workflow 2: Compile-Debug Loop (Marcus, Developer)**
```
Intent: "Compile this program" or "Fix the errors"
Steps the agent takes:
  1. compile HELLO.cbl → diagnostics (2 errors, 1 warning)
  2. Explain errors with fix suggestions
  3. User fixes → compile again → success
Output artifacts: Compiler diagnostics, fix suggestions
```

**Workflow 3: JCL Job Execution (Marcus, Developer)**
```
Intent: "Run this job"
Steps the agent takes:
  1. parse-jcl RUNCARD.jcl → show job structure (3 steps, DD statements)
  2. HITL approval: "This will execute 3 steps. Approve?"
  3. run RUNCARD.jcl → per-step results with return codes
Output artifacts: Job timeline with step-by-step results, SYSOUT
```

**Workflow 4: Code Understanding (New Developer)**
```
Intent: "What does CALC-PREMIUM do?"
Steps the agent takes:
  1. Read the file (cat or bash)
  2. LLM analyzes → structured explanation with business rules
Output artifacts: Explanation with paragraph-level annotations
```

**Workflow 5: CICS Transaction Testing**
```
Intent: "Test the CARDDEMO transaction"
Steps the agent takes:
  1. bms COSGN00.bms → compile BMS maps
  2. cics COSGN00.cbl --transid CARD=COSGN00 → interactive 3270 session
  3. User interacts with TUI screens
Output artifacts: Screen captures, transaction flow
```

**Workflow 6: Dataset Setup**
```
Intent: "Set up the VSAM files for testing"
Steps the agent takes:
  1. idcams exec "DEFINE CLUSTER ..."
  2. gdg create MY.GDG.BASE --limit 10
  3. idcams exec "REPRO INFILE(INPUT) OUTFILE(VSAM)"
Output artifacts: Dataset catalog, operation results
```

### Current CLI Pain Points (What We Need to Fix Through UX)

1. **Sequential, manual orchestration** — User must know which commands to run in which order
2. **Raw JSON/text output** — No visualization, no summarization
3. **No workflow context** — Each command is stateless; no memory of what was assessed/compiled before
4. **No progress feedback** — Large codebase scans appear to hang
5. **No approval gates** — `run` executes immediately, no review of what will happen
6. **Error messages are technical** — Compiler diagnostics are COBOL jargon, not explained
7. **No artifact management** — Assessment reports are ephemeral stdout, not saved/versioned
8. **CICS is isolated** — TUI mode can't integrate with the chat experience

---

## What "Agent-Native" Means (Guiding Principles)

1. **Outcome-driven, not task-driven** — User says "Modernize this codebase", agent runs assess → plan → compile → test autonomously.
2. **The agent drives, the human steers** — Agent proposes plans and executes. Human approves, redirects, or provides context.
3. **Artifacts, not messages** — Primary output is assessment reports, compiled binaries, migration plans. Conversation is the mechanism, not the product.
4. **Progressive abstraction** — Show high-level progress by default. Drill into tool calls, stdout, thinking only when needed.
5. **Trust through transparency** — Show what the agent is doing without requiring the user to read every line.
6. **Multiplexed attention** — Agent may assess + compile + explain in parallel. UI supports this.
7. **Memory and continuity** — Agent remembers previous assessments, compilation results, project state across sessions.

---

## BMAD Phase 1: Research (Iterations 1-5)

Produce research documents in `_bmad-output/agent-app/research/`.

### Research Track 1: OpenMainframe CLI Deep Dive

**Output file:** `_bmad-output/agent-app/research/openmainframe-cli-ux-analysis.md`

Read the actual CLI source code and existing research docs to deeply understand:

1. **Each command's output structure** — Read `crates/open-mainframe/src/commands/*.rs` and `crates/open-mainframe/src/output.rs`. Document the exact JSON schema each command produces.
2. **User workflow mapping** — For each of the 6 workflows above, trace the exact sequence of commands, their inputs, outputs, and what artifacts the user cares about.
3. **Output visualization opportunities** — For each command output, ask: "How should this look in a UI?" Assessment results → what kind of visualization? Compiler errors → what kind of presentation? Job results → what kind of timeline?
4. **Pain points per command** — What's hard about using each command today? What information is missing? What's overwhelming?
5. **Cross-command relationships** — How do commands relate? (assess identifies problems → compile validates → run tests → explain helps understand). Map the dependency graph.
6. **CICS challenge** — The CICS command runs an interactive TUI. How does this translate to an agent-native web UI? Research how other tools handle terminal-in-browser (xterm.js, ttyd, etc.).

Also read these existing research docs for context:
- `_bmad-output/agent-app/research-technical-openmainframe-tools.md`
- `_bmad-output/agent-app/research-domain-modernization.md`
- `_bmad-output/agent-app/product-brief-openmainframe-agent.md`

### Research Track 2: Agent-Native UX Patterns

**Output file:** `_bmad-output/agent-app/research/agent-native-ux-patterns.md`

Research UX patterns from leading agent-native applications. For each, document how it works AND how OpenMainframe could adopt or reject the pattern:

| Application | Why Study It | OpenMainframe Relevance |
|-------------|-------------|------------------------|
| **Claude Code** | Activity log, thinking disclosure, tool calls | Closest to current TerminalChat.tsx implementation |
| **Cursor** | IDE + agent mode, inline diffs, plan-then-execute | Code editing + compilation workflow |
| **Devin** | Full autonomous agent, workspace, timeline | Long-running assessment + migration tasks |
| **GitHub Copilot Workspace** | Issue-to-PR, plan review, iterative refinement | Assessment-to-migration pipeline |
| **Bolt.new / v0.dev** | Generative UI, artifact preview | Assessment dashboards, report generation |
| **AWS Transform** | Multi-agent modernization, assessment pipeline | Direct competitor — study what they got right/wrong |

For each, answer:
1. How does the user express intent?
2. How does the agent show plan/thinking?
3. How does the agent show progress on multi-step work?
4. How does the user intervene/redirect?
5. What are the primary artifacts vs. conversation?
6. **Would this pattern work for a mainframe engineer who's never used an AI agent?**
7. **Would this pattern work for each of the 6 OpenMainframe workflows?**

### Research Track 3: Modernization-Specific UX

**Output file:** `_bmad-output/agent-app/research/modernization-ux-landscape.md`

Research how existing mainframe modernization tools present their UX:

- **AWS Mainframe Modernization (Transform)** — Multi-agent pipeline, assessment dashboard
- **Google Cloud Dual Run** — Side-by-side execution, parity validation
- **Micro Focus / OpenText Enterprise Analyzer** — IDE-centric, code analysis
- **IBM Watsonx Code Assistant for Z** — AI-assisted refactoring in editor
- **Raincode** — .NET conversion, Visual Studio integration
- **TSRI** — Automated conversion with quality dashboard

For each, specifically study:
1. How do they present **assessment results**? (This is OpenMainframe's `assess` output)
2. How do they present **compilation feedback**? (This is OpenMainframe's `compile`/`check` output)
3. How do they present **job execution results**? (This is OpenMainframe's `run` output)
4. What visualization works for **complexity scores**, **feature support matrices**, **dependency graphs**?
5. What feels outdated or agent-unfriendly about their approach?

### Research Track 4: Agent UX Design Principles

**Output file:** `_bmad-output/agent-app/research/agent-ux-design-principles.md`

Research emerging thinking on agent UX design:

- Search for: "agent native UX design", "agentic UI patterns", "AI agent interface design 2025 2026"
- Look at: Lenny's Newsletter, a16z blog, Intercom on AI, Jakob Nielsen on AI UX
- Research: CopilotKit's generative UI patterns, AG-UI protocol capabilities
- Study: How autonomous systems (self-driving cars, automated trading) show status and allow intervention
- Explore: The "outcome-driven interface" concept

Synthesize into design principles **specific to OpenMainframe**. Not generic "agent UX" — but "here's how an agent that runs COBOL compilers and JCL jobs should present itself."

---

## BMAD Phase 2: UX Design (Iterations 6-12)

Use research findings to produce a new UX specification.

### Step 1: Define Interaction Model

Choose the primary interaction model. Consider how each handles the 6 OpenMainframe workflows:

| Model | Assessment Workflow | Compile Loop | JCL Execution | Code Explanation |
|-------|-------------------|--------------|----------------|------------------|
| Activity log (Claude Code) | ? | ? | ? | ? |
| Plan-execute (Copilot Workspace) | ? | ? | ? | ? |
| Workspace (Devin) | ? | ? | ? | ? |
| Pipeline (CI/CD) | ? | ? | ? | ? |
| Hybrid | ? | ? | ? | ? |

Fill in this table. Choose one. Explain why.

### Step 2: Design Per-Workflow UX

For each of the 6 workflows, design the specific user experience:

**Assessment Workflow UX:**
- What does the user see when they say "Assess this codebase"?
- How does progress appear as 42 files are scanned?
- How are results presented? (Summary → drill-down → individual file)
- What's the assessment report artifact look like?
- How does the user act on findings? ("Fix the 3 critical issues", "Explain POLCY01")

**Compile-Debug Loop UX:**
- What does the user see when they say "Compile HELLO.cbl"?
- How are compiler errors presented? (severity, line reference, explanation, fix suggestion)
- How does the fix-recompile iteration feel?
- How does the user go from error → seeing the code → understanding the fix?

**JCL Execution UX:**
- How does parse-jcl output become a visual job preview?
- How does the HITL approval gate look and feel?
- How does the job timeline render during and after execution?
- How are per-step return codes, SYSOUT, DD statements presented?

**Code Explanation UX:**
- How does the explanation appear? (inline annotations? structured sections? conversational?)
- How does the user navigate from explanation → source code → back?
- How are business rules extracted and presented separately from code structure?

**CICS Transaction UX:**
- Can a 3270 TUI be embedded in the web UI? (Research xterm.js, ttyd)
- Or should the agent describe screens in text and the user interacts via chat?
- How does BMS map compilation feed into the transaction test?

**Dataset Management UX:**
- How do IDCAMS/GDG operations present results?
- Is a dataset catalog browser useful, or should the agent handle it?
- How does VSAM file setup integrate with the JCL execution workflow?

### Step 3: Design Core Screens

ASCII wireframes for each screen state:

1. **First visit / no project connected** — Onboarding, bridge connection
2. **Intent entry** — User declares what they want to accomplish
3. **Agent working (assessment)** — Scanning 42 files with progress
4. **Assessment results** — The artifact view for assessment data
5. **Agent working (compile)** — Compilation in progress
6. **Compiler results** — Errors with explanations and fix suggestions
7. **JCL preview + approval** — Job structure review before execution
8. **Job execution results** — Step timeline with return codes
9. **Code explanation** — Annotated source with business rules
10. **Steering/redirection** — User changes direction mid-task

### Step 4: Map CLI Output to UI Components

For each CLI command's JSON output, specify the exact UI component:

| CLI Command | JSON Field | UI Component | Visual Treatment |
|-------------|-----------|--------------|------------------|
| `assess scan` | complexity_scores | ? | ? |
| `assess scan` | feature_matrix | ? | ? |
| `assess scan` | compatibility_issues | ? | ? |
| `compile` | diagnostics[].severity | ? | ? |
| `compile` | diagnostics[].message | ? | ? |
| `compile` | summary.errors | ? | ? |
| `run` | steps[].return_code | ? | ? |
| `run` | steps[].stdout | ? | ? |
| `parse-jcl` | steps[].dd_statements | ? | ? |
| `db2 preprocess` | sql_statements | ? | ? |
| ... | ... | ... | ... |

### Step 5: Map AG-UI Events to UI Behaviors

| AG-UI Event | UI Behavior | Example |
|-------------|-------------|---------|
| TEXT_MESSAGE_START/CONTENT/END | ? | Agent explains assessment results |
| TOOL_CALL_START (bash) | ? | Agent runs `open-mainframe compile` |
| TOOL_CALL_ARGS (streaming) | ? | Command being typed |
| TOOL_CALL_END | ? | Command ready, executing |
| TOOL_CALL_RESULT | ? | Compile output received |
| CUSTOM (thinking_start/content/end) | ? | Agent reasoning about errors |
| CUSTOM (on_interrupt) | ? | JCL execution approval |
| RUN_FINISHED | ? | Agent done with turn |
| RUN_ERROR | ? | LLM or network error |

### Step 6: Component Architecture

React component tree that maps to the wireframes:

```
App
├── [layout components]
├── [interaction surface]
│   ├── [per-workflow components]
│   └── [shared components]
├── [artifact display]
│   ├── [assessment artifacts]
│   ├── [compilation artifacts]
│   ├── [execution artifacts]
│   └── [explanation artifacts]
└── [agent status]
```

### Step 7: Write the UX Specification

**Output file:** `_bmad-output/agent-app/ux-design-agent-native.md`

Complete spec with:
1. Design principles (from research, specific to OpenMainframe)
2. Interaction model (chosen, with rationale)
3. Per-workflow UX design (assessment, compile, execute, explain, CICS, dataset)
4. Layout architecture (ASCII wireframes for all screen states)
5. CLI output → UI component mapping
6. AG-UI event → UI behavior mapping
7. Component hierarchy
8. Interaction patterns (intent entry, steering, approval)
9. Color scheme and theming
10. Responsive behavior
11. Accessibility
12. Comparison to old UX spec (what changed and why)

---

## Iteration Guide

Each Ralph Loop iteration should:

1. **Read** current state of research/design files in `_bmad-output/agent-app/research/`
2. **Read** CLI source code in `crates/open-mainframe/src/` for command details
3. **Read** existing frontend in `openmainframe-agent/src/` for current implementation
4. **Search** the web for agent UX patterns and modernization UX (use WebSearch)
5. **Write** or refine one research document or one section of the UX spec
6. **Commit** progress with a descriptive message

### Iteration Targets

| Iteration | Focus | Output |
|-----------|-------|--------|
| 1 | OpenMainframe CLI deep dive — read source, map commands/outputs/workflows | `research/openmainframe-cli-ux-analysis.md` |
| 2 | Agent-native UX patterns — Claude Code, Cursor, Devin, etc. | `research/agent-native-ux-patterns.md` |
| 3 | Modernization UX landscape — AWS Transform, IBM, competitors | `research/modernization-ux-landscape.md` |
| 4 | Agent UX design principles — theory + OpenMainframe-specific synthesis | `research/agent-ux-design-principles.md` |
| 5 | Cross-reference all research, fill gaps, resolve contradictions | All research docs refined |
| 6 | Choose interaction model, design assessment workflow UX | UX spec: sections 1-3 (assessment) |
| 7 | Design compile, execute, explain workflow UX | UX spec: section 3 (remaining workflows) |
| 8 | Core screen wireframes | UX spec: section 4 |
| 9 | CLI→UI mapping + AG-UI event mapping + component architecture | UX spec: sections 5-7 |
| 10 | Interaction patterns + theming + accessibility | UX spec: sections 8-11 |
| 11 | Comparison to old spec + final review | UX spec: section 12, polish |
| 12 | Quality check — verify all CLI commands have UI mapping, all workflows have wireframes | Final commit |

---

## File Structure

```
_bmad-output/agent-app/
├── research/
│   ├── openmainframe-cli-ux-analysis.md   # Track 1: CLI commands, outputs, workflow mapping
│   ├── agent-native-ux-patterns.md        # Track 2: Agent app UX patterns
│   ├── modernization-ux-landscape.md      # Track 3: Competitor modernization UX
│   └── agent-ux-design-principles.md      # Track 4: Agent UX theory + synthesis
├── ux-design-agent-native.md              # NEW: Agent-native UX specification
└── ux-design-openmainframe-agent.md       # OLD: Keep for comparison
```

---

## Rules

1. **Understand the CLI before designing the UX.** Read `crates/open-mainframe/src/commands/*.rs` and `output.rs`. Know what JSON each command produces.
2. **Every CLI command must have a UX mapping.** Don't leave any command without a defined UI treatment.
3. **Every workflow must have a wireframe.** Assessment, compile, execute, explain, CICS, dataset — all six.
4. **Research first, design second.** Do not start Phase 2 until all 4 research documents exist.
5. **Web search is mandatory.** Every research iteration must include web searches for current data.
6. **Cite sources.** Every claim in research documents must link to a source.
7. **Be opinionated.** Make clear choices, not option lists.
8. **Challenge convention.** If every modernization tool uses dashboards, ask whether that's right for agent-native.
9. **Think from the user's perspective.** A mainframe engineer (50s, 20 years COBOL experience, never used an AI agent) opens this app. What happens?
10. **Keep it buildable.** Next.js + React + Tailwind + AG-UI event stream. No fantasy tech.

---

## Completion Promise

Output `<promise>UX RESEARCH COMPLETE</promise>` when ALL of the following are true:

1. All 4 research documents exist with substantive, cited content
2. `openmainframe-cli-ux-analysis.md` covers all 14 CLI commands with output schemas and UX opportunities
3. The UX spec (`ux-design-agent-native.md`) is complete with all 12 sections
4. The UX spec makes a clear choice of interaction model with filled comparison table
5. All 6 workflows have dedicated UX designs with wireframes
6. All 10 core screen wireframes exist
7. CLI output → UI component mapping is complete for every command
8. AG-UI event mapping is complete for every event type
9. Component hierarchy maps to the wireframes
10. All files are committed to git
