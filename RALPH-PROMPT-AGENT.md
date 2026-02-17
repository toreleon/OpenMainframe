# OpenMainframe Modernization Agent — BMAD Method Ralph Loop Plan

You are executing a BMAD Method-driven planning and implementation loop for building an AI Agent application that enables mainframe modernization and migration through interactive interaction with the OpenMainframe platform.

## Project Vision

Build **OpenMainframe Agent** — an AI-powered copilot application that helps enterprises modernize and migrate their mainframe workloads by interacting with the OpenMainframe compiler/runtime platform. The agent understands COBOL, JCL, CICS, DB2, IMS, VSAM, and guides users through assessment, migration planning, code transformation, and validation — all powered by OpenMainframe's Rust-based execution engine.

## Technology Stack

- **Frontend:** Next.js + CopilotKit (React-based agent-native UI)
- **Agent Framework:** LangGraph (Python) with CopilotKit CoAgents
- **Backend Engine:** OpenMainframe CLI (Rust — compile, interpret, assess, run)
- **Protocol:** AG-UI (Agent-UI protocol for streaming state/tool calls)
- **State Management:** CopilotKit Shared State (bi-directional agent ↔ UI)
- **Persistence:** PostgreSQL (conversation history, migration projects)

---

## Self-Orientation (Do This First Every Iteration)

1. Run `git log --oneline -10` to see what has already been done
2. Check which **BMAD Phase** and **Step** below is the next uncompleted one
3. A step is complete when its output artifact exists in `_bmad-output/agent-app/`
4. If ALL phases are done, output the completion promise

---

## BMAD Phase Order

### Phase 1 — ANALYSIS (Steps 1–4)

| Step | BMAD Workflow | Agent | Description | Output |
|------|--------------|-------|-------------|--------|
| 1 | Domain Research (DR) | Mary 📊 Analyst | Research mainframe modernization landscape: existing tools (Micro Focus, LzLabs, AWS Blu Age, Google Dual Run), pain points, enterprise workflows, common migration patterns (rehost, replatform, refactor, replace). Document what enterprises actually need from a modernization agent. | `_bmad-output/agent-app/research-domain-modernization.md` |
| 2 | Technical Research (TR) | Mary 📊 Analyst | Research the CopilotKit + LangGraph architecture for building CoAgents: shared state, human-in-the-loop, generative UI, frontend actions, tool calling. Evaluate how OpenMainframe CLI tools map to agent capabilities. Document technical feasibility. | `_bmad-output/agent-app/research-technical-copilotkit-langgraph.md` |
| 3 | Technical Research (TR) | Mary 📊 Analyst | Research OpenMainframe's current capabilities as agent tools: `open-mainframe compile`, `interpret`, `run`, `check`, `assess` commands. Map each to agent actions. Identify gaps between what the assess crate provides and what a modernization agent needs. | `_bmad-output/agent-app/research-technical-openmainframe-tools.md` |
| 4 | Create Brief (CB) | Mary 📊 Analyst | Create a Product Brief for the OpenMainframe Agent application synthesizing all research. Define the core vision, target users (mainframe developers, migration architects, operations teams), key differentiators, and MVP scope. | `_bmad-output/agent-app/product-brief-openmainframe-agent.md` |

### Phase 2 — PLANNING (Steps 5–7)

| Step | BMAD Workflow | Agent | Description | Output |
|------|--------------|-------|-------------|--------|
| 5 | Create PRD (CP) | John 📋 PM | Create a comprehensive PRD for the agent app. Define functional requirements for each agent capability: code assessment, migration planning, code transformation, test generation, interactive debugging, COBOL explanation, JCL conversion. Include success metrics and acceptance criteria. | `_bmad-output/agent-app/prd-openmainframe-agent.md` |
| 6 | Create UX (CU) | Sally 🎨 UX Designer | Design the UX for the agent application: chat interface with CopilotKit, generative UI components for code diffs, migration dashboards, assessment reports, COBOL syntax highlighting, file tree navigation, terminal output rendering. | `_bmad-output/agent-app/ux-design-openmainframe-agent.md` |
| 7 | Validate PRD (VP) | John 📋 PM | Validate the PRD is comprehensive, lean, well-organized, and cohesive. Ensure all agent capabilities map to concrete OpenMainframe CLI commands. Verify acceptance criteria are testable. | `_bmad-output/agent-app/prd-validation-report.md` |

### Phase 3 — SOLUTIONING (Steps 8–11)

| Step | BMAD Workflow | Agent | Description | Output |
|------|--------------|-------|-------------|--------|
| 8 | Create Architecture (CA) | Winston 🏗️ Architect | Design the system architecture. Key decisions: LangGraph graph structure (nodes for each agent capability), CopilotKit integration pattern, OpenMainframe CLI wrapper (Python subprocess vs REST API), state schema, tool definitions, MCP server for OpenMainframe. | `_bmad-output/agent-app/architecture-openmainframe-agent.md` |
| 9 | Create Epics & Stories (CE) | John 📋 PM | Break the PRD into epics and stories. Each epic should represent a major agent capability with concrete acceptance criteria. Stories should be implementable in 1-2 iterations. | `_bmad-output/agent-app/epics-openmainframe-agent.md` |
| 10 | Check Implementation Readiness (IR) | Winston 🏗️ Architect | Verify PRD, UX, Architecture, and Epics are aligned and ready for implementation. Identify any gaps, contradictions, or missing technical details. Ensure the LangGraph graph design covers all agent workflows. | `_bmad-output/agent-app/readiness-report.md` |
| 11 | Sprint Planning (SP) | Bob 🏃 Scrum Master | Generate the sprint plan ordering all stories for implementation. Group by epic, respect dependencies (infrastructure first, then capabilities). Define the Ralph Loop batch order for implementation. | `_bmad-output/agent-app/sprint-plan.md` |

### Phase 4 — IMPLEMENTATION (Steps 12+)

Implementation batches will be defined by the Sprint Plan output from Step 11. The general structure follows:

| Batch | Epic | Description |
|-------|------|-------------|
| 1 | Infrastructure | Project scaffolding: Next.js + CopilotKit frontend, LangGraph Python agent, OpenMainframe CLI wrapper |
| 2 | Assessment Agent | Code analysis tool: scan COBOL/JCL files, run `open-mainframe assess`, return structured reports |
| 3 | Compilation Agent | Compile/check tool: run `open-mainframe compile` and `check`, parse errors, suggest fixes |
| 4 | Execution Agent | Run tool: execute JCL jobs via `open-mainframe run`, stream output, capture return codes |
| 5 | Migration Planner | Multi-step migration workflow: assess → plan → transform → validate pipeline |
| 6 | Code Explainer | COBOL/JCL/CICS code explanation with LLM-powered analysis + OpenMainframe AST parsing |
| 7 | Interactive Debugger | Step-through COBOL interpretation with `open-mainframe interpret`, breakpoints, variable inspection |
| 8 | Generative UI | CopilotKit generative UI components: code diff viewer, migration dashboard, assessment report cards |
| 9 | Human-in-the-Loop | Approval checkpoints for destructive operations, migration plan review, code transformation confirmation |
| 10 | End-to-End Testing | CardDemo integration test: full migration workflow from assessment to execution |

---

## Agent Capabilities (Feature Map)

The OpenMainframe Agent provides these core capabilities mapped to OpenMainframe CLI tools:

### 1. Code Assessment & Analysis
- **Tool:** `open-mainframe assess <path>` (uses `open-mainframe-assess` crate)
- **Agent Action:** Scan COBOL/JCL/CICS source directories, produce compatibility reports
- **Output:** Complexity metrics, call graphs, CICS/DB2/IMS feature inventory, migration risk score
- **UI Component:** Assessment dashboard with pie charts, feature matrix, risk heatmap

### 2. COBOL Compilation & Validation
- **Tool:** `open-mainframe compile <source>` and `open-mainframe check <source>`
- **Agent Action:** Compile COBOL programs, parse errors, suggest fixes
- **Output:** Compilation status, error locations with context, fix suggestions
- **UI Component:** Inline error annotations, compiler output panel

### 3. JCL Job Execution
- **Tool:** `open-mainframe run <jcl>`
- **Agent Action:** Execute JCL jobs, monitor step execution, capture output
- **Output:** Step return codes, SYSOUT content, dataset changes
- **UI Component:** Job execution timeline, step-by-step progress, output viewer

### 4. COBOL Interpretation & Debugging
- **Tool:** `open-mainframe interpret <source>`
- **Agent Action:** Run COBOL programs interactively, inspect variables, trace execution
- **Output:** Variable state, execution trace, DISPLAY output
- **UI Component:** Debugger panel with variable watch, call stack, step controls

### 5. Code Explanation
- **Tool:** LLM analysis + OpenMainframe AST parsing (`open-mainframe lex`, `parse-jcl`)
- **Agent Action:** Explain COBOL/JCL/CICS code in natural language, identify business logic
- **Output:** Annotated code with explanations, business rule extraction
- **UI Component:** Side-by-side code + explanation view

### 6. Migration Planning
- **Tool:** Composite workflow using assess + compile + run
- **Agent Action:** Multi-step migration wizard: inventory → assess → plan → transform → validate
- **Output:** Migration roadmap, risk assessment, effort estimation, step-by-step plan
- **UI Component:** Migration wizard with progress tracking, Gantt-style timeline

### 7. Dataset Management
- **Tool:** OpenMainframe dataset/catalog operations
- **Agent Action:** Manage VSAM, QSAM, PDS datasets; IDCAMS operations
- **Output:** Dataset listings, record counts, structure information
- **UI Component:** Dataset explorer with record viewer

---

## LangGraph Agent Architecture

```
                    ┌─────────────────────────┐
                    │   CopilotKit Frontend    │
                    │   (Next.js + React)      │
                    │                          │
                    │  ┌───────────────────┐   │
                    │  │  Chat Interface   │   │
                    │  │  Generative UI    │   │
                    │  │  Shared State     │   │
                    │  └───────────────────┘   │
                    └─────────┬───────────────┘
                              │ AG-UI Protocol
                              ▼
                    ┌─────────────────────────┐
                    │   CopilotKit Runtime    │
                    │   (Node.js middleware)   │
                    └─────────┬───────────────┘
                              │
                              ▼
                    ┌─────────────────────────┐
                    │   LangGraph Agent       │
                    │   (Python/FastAPI)       │
                    │                          │
                    │  ┌───────────────────┐   │
                    │  │  Router Node      │   │
                    │  │  (intent classify)│   │
                    │  └───┬───┬───┬───┬───┘   │
                    │      │   │   │   │       │
                    │      ▼   ▼   ▼   ▼       │
                    │  ┌─────────────────────┐ │
                    │  │ Agent Nodes:        │ │
                    │  │ - assess_node       │ │
                    │  │ - compile_node      │ │
                    │  │ - execute_node      │ │
                    │  │ - explain_node      │ │
                    │  │ - migrate_node      │ │
                    │  │ - debug_node        │ │
                    │  │ - dataset_node      │ │
                    │  └─────────┬───────────┘ │
                    │            │              │
                    │            ▼              │
                    │  ┌─────────────────────┐ │
                    │  │ OpenMainframe       │ │
                    │  │ Tool Layer          │ │
                    │  │ (subprocess wrapper)│ │
                    │  └─────────┬───────────┘ │
                    └────────────┼──────────────┘
                                 │
                                 ▼
                    ┌─────────────────────────┐
                    │  OpenMainframe CLI      │
                    │  (Rust binary)          │
                    │                          │
                    │  compile | interpret     │
                    │  run | check | assess    │
                    │  lex | parse-jcl         │
                    └─────────────────────────┘
```

### LangGraph State Schema

```python
class AgentState(TypedDict):
    messages: Annotated[list, add_messages]
    # Current project context
    project_path: str
    source_files: list[dict]  # {path, type, size, last_modified}
    # Assessment results
    assessment: dict | None  # complexity, features, risks
    # Migration state
    migration_plan: dict | None
    migration_progress: dict | None
    # Compilation results
    compilation_results: list[dict]
    # Execution results
    execution_results: list[dict]
    # Active operation
    current_operation: str | None
    operation_progress: float  # 0.0 - 1.0
```

---

## Implementation Protocol (Each Iteration)

### Step 1: Identify Current Phase & Step

Read this document and check which step is next based on existing outputs:
```bash
ls _bmad-output/agent-app/ 2>/dev/null || echo "No outputs yet"
```

### Step 2: Execute the BMAD Workflow

For the current step:
1. Load the corresponding BMAD agent persona and workflow
2. Read all relevant input documents (previous phase outputs, OpenMainframe codebase)
3. Execute the workflow producing the required output artifact
4. Write output to the specified location

### Step 3: Validate Output

- Ensure the artifact follows BMAD standards
- Check consistency with previous phase outputs
- Verify all sections are complete and actionable

### Step 4: Commit

```bash
git add _bmad-output/agent-app/
git commit -m "bmad(agent-app): Phase N Step M — <description>

Output: <artifact-name>
BMAD Workflow: <workflow-name>
Agent: <agent-name>"
```

### Step 5: Exit

After committing, stop. The loop will restart and pick up the next step.

---

## Important Rules

1. **ONE STEP PER ITERATION** — Complete one BMAD workflow step, produce the artifact, commit, then exit.
2. **FOLLOW BMAD PHASES IN ORDER** — Analysis → Planning → Solutioning → Implementation. Do not skip ahead.
3. **REQUIRED WORKFLOWS ARE GATES** — Steps marked as required in BMAD must be completed before progressing to the next phase.
4. **REFERENCE OPENMAINFRAME** — Every agent capability must map to a concrete OpenMainframe CLI command or crate. Do not design features the platform cannot support.
5. **STAY GROUNDED IN COPILOTKIT** — Architecture must use CopilotKit + LangGraph patterns: shared state, human-in-the-loop, generative UI, tool calling.
6. **READ BEFORE WRITE** — Always read existing artifacts and OpenMainframe source code before producing new artifacts.
7. **COMMIT MESSAGE FORMAT** — Always start with `bmad(agent-app): Phase N Step M — <title>`.

---

## Key Reference Files

### OpenMainframe Codebase
- `Cargo.toml` — Workspace with 15 crates
- `crates/open-mainframe/src/main.rs` — CLI entry point
- `crates/open-mainframe-assess/src/` — Assessment/analysis crate
- `crates/open-mainframe-cobol/src/` — COBOL compiler
- `crates/open-mainframe-jcl/src/` — JCL interpreter
- `crates/open-mainframe-runtime/src/` — Runtime engine
- `crates/open-mainframe-dataset/src/` — Dataset I/O

### Existing BMAD Artifacts
- `_bmad-output/planning-artifacts/product-brief-zOS-clone-2026-02-12.md` — OpenMainframe product brief
- `_bmad-output/planning-artifacts/prd-v2.0.md` — Current PRD (compiler-focused)
- `_bmad-output/planning-artifacts/architecture-v2.0.md` — Current architecture
- `_bmad-output/planning-artifacts/epics-assess-v3.0.md` — Assessment crate epics

### CopilotKit Documentation
- CopilotKit + LangGraph quickstart: `npx copilotkit@latest create -f langgraph-py`
- Key features: Shared State, Human-in-the-Loop, Generative UI, Frontend Actions
- CoAgents pattern: bi-directional state, multi-actor, streaming tool calls

### BMAD Workflows
- `_bmad/_config/bmad-help.csv` — Full workflow catalog
- `_bmad/bmm/workflows/` — All workflow definitions
- `_bmad/bmm/agents/` — Agent persona files

---

## Completion

When ALL steps through Phase 4 implementation batches have been completed (all artifacts exist in `_bmad-output/agent-app/` and implementation code is committed), output the completion promise tag with text OPENMAINFRAME AGENT COMPLETE.
