# OpenMainframe Agent — Implementation Ralph Loop

You are implementing the **OpenMainframe Agent** — an AI-powered mainframe modernization copilot built with CopilotKit + LangGraph, wrapping the OpenMainframe Rust compiler/runtime CLI as agent tools.

All planning is DONE. All BMAD artifacts are in `_bmad-output/agent-app/`. Your ONLY job is to write working code, batch by batch, following the sprint plan exactly.

---

## Self-Orientation (EVERY Iteration)

```bash
# 1. Check what batches are done
ls openmainframe-agent/ 2>/dev/null && cat openmainframe-agent/BATCH_LOG.md 2>/dev/null || echo "No implementation yet"

# 2. Check git history
git log --oneline -15

# 3. Determine the next uncompleted batch from the list below
```

A batch is COMPLETE when:
- All files for that batch exist and are syntactically valid
- `BATCH_LOG.md` records the batch as done
- Changes are committed

---

## Implementation Batches (Execute In Order)

### BATCH 1: Project Scaffolding (E-200)

**Goal:** Working Next.js + CopilotKit frontend + Python LangGraph agent with end-to-end chat.

**Create these files:**

#### Frontend (`openmainframe-agent/`)

```
openmainframe-agent/
├── package.json               # Next.js 15 + CopilotKit deps + concurrently
├── tsconfig.json
├── next.config.ts
├── tailwind.config.ts
├── postcss.config.mjs
├── .env.example               # AGENT_URL, LANGSMITH_API_KEY
├── .env.local                 # gitignored
├── src/
│   ├── app/
│   │   ├── layout.tsx         # CopilotKit provider wrapping app
│   │   ├── page.tsx           # Main page with CopilotSidebar
│   │   ├── globals.css        # Tailwind + CopilotKit styles
│   │   └── api/
│   │       └── copilotkit/
│   │           └── route.ts   # CopilotRuntime with LangGraphAgent
│   └── lib/
│       └── types.ts           # AgentState TypeScript types
└── agent/
    ├── pyproject.toml         # Python deps (langgraph, copilotkit, fastapi, etc.)
    ├── .env.example           # OPENAI_API_KEY or ANTHROPIC_API_KEY, OPEN_MAINFRAME_BIN
    ├── main.py                # FastAPI + LangGraph agent registration
    └── src/
        ├── __init__.py
        ├── agent.py           # Graph: router → chat (skeleton)
        └── state.py           # AgentState(CopilotKitState)
```

**Key patterns from latest CopilotKit (IMPORTANT — use these exact APIs):**

Frontend route.ts:
```typescript
import { CopilotRuntime, ExperimentalEmptyAdapter, copilotRuntimeNextJSAppRouterEndpoint } from "@copilotkit/runtime";
import { LangGraphAgent } from "@copilotkit/runtime/langgraph";
import { NextRequest } from "next/server";

const agent = new LangGraphAgent({
  deploymentUrl: process.env.AGENT_URL || "http://localhost:8123",
  graphId: "modernization_agent",
  langsmithApiKey: process.env.LANGSMITH_API_KEY || "",
});

export const POST = async (req: NextRequest) => {
  const { handleRequest } = copilotRuntimeNextJSAppRouterEndpoint({
    endpoint: "/api/copilotkit",
    serviceAdapter: new ExperimentalEmptyAdapter(),
    runtime: new CopilotRuntime({ agents: { default: agent } }),
  });
  return handleRequest(req);
};
```

Python agent main.py — use `create_agent` from `langchain.agents` with `CopilotKitMiddleware`:
```python
from langchain.agents import create_agent
from copilotkit import CopilotKitMiddleware
from langchain_openai import ChatOpenAI

agent = create_agent(
    model=ChatOpenAI(model="gpt-4.1-mini"),
    tools=[...],
    middleware=[CopilotKitMiddleware()],
    state_schema=AgentState,
    system_prompt="..."
)
graph = agent
```

Python agent state.py:
```python
from typing import TypedDict, Optional

class AgentState(TypedDict):
    project_path: Optional[str]
    source_files: list[dict]
    assessment_results: Optional[dict]
    compilation_results: list[dict]
    execution_results: list[dict]
    current_operation: Optional[str]
    operation_progress: float
```

Layout.tsx:
```typescript
import { CopilotKit } from "@copilotkit/react-core";
import "@copilotkit/react-ui/styles.css";

export default function RootLayout({ children }) {
  return (
    <html><body>
      <CopilotKit runtimeUrl="/api/copilotkit">
        {children}
      </CopilotKit>
    </body></html>
  );
}
```

Page.tsx:
```typescript
"use client";
import { CopilotSidebar } from "@copilotkit/react-ui";

export default function Home() {
  return (
    <CopilotSidebar
      defaultOpen={true}
      labels={{ title: "OpenMainframe Agent", initial: "Hello! I'm your mainframe modernization assistant." }}
    >
      <main className="flex h-screen">
        <div className="flex-1 p-8">
          <h1>OpenMainframe Agent</h1>
          <p>Set your project directory to get started.</p>
        </div>
      </main>
    </CopilotSidebar>
  );
}
```

**package.json scripts:**
```json
{
  "scripts": {
    "dev": "concurrently \"npm run dev:frontend\" \"npm run dev:agent\"",
    "dev:frontend": "next dev --port 3000",
    "dev:agent": "cd agent && uvicorn main:app --host 0.0.0.0 --port 8123 --reload"
  }
}
```

**Exit criteria:** All files created, syntactically valid. Do NOT run `npm install` or `pip install` — just create the files.

---

### BATCH 2: Tool Layer (E-300)

**Goal:** All 10 OpenMainframe CLI tools as Python @tool functions.

**Create these files in `openmainframe-agent/agent/src/tools/`:**

```
tools/
├── __init__.py        # Exports ALL_TOOLS list
├── base.py            # run_cli(), sanitize_path(), sanitize_idcams()
├── assess_tools.py    # assess_scan, assess_file
├── compile_tools.py   # compile_cobol, check_cobol
├── execute_tools.py   # run_jcl, interpret_cobol
├── parse_tools.py     # parse_jcl, lex_cobol
└── dataset_tools.py   # list_catalog, idcams_command
```

**Key code — base.py:**
```python
import subprocess, os, re
from typing import Optional

OPEN_MAINFRAME_BIN = os.getenv("OPEN_MAINFRAME_BIN", "./target/release/open-mainframe")
MAX_OUTPUT_BYTES = 20_000
WORKSPACE_ROOT = os.getenv("WORKSPACE_ROOT", os.getcwd())

def sanitize_path(path: str) -> str:
    path = path.replace('\x00', '')
    resolved = os.path.realpath(path)
    if not resolved.startswith(os.path.realpath(WORKSPACE_ROOT)):
        raise ValueError(f"Path {path} is outside allowed workspace")
    return resolved

def run_cli(args: list[str], timeout: int = 120, cwd: Optional[str] = None) -> dict:
    try:
        result = subprocess.run(
            [OPEN_MAINFRAME_BIN, *args],
            capture_output=True, text=True, timeout=timeout, cwd=cwd,
            env={**os.environ, "LANG": "C.UTF-8"},
        )
        return {"success": result.returncode == 0, "stdout": result.stdout[:MAX_OUTPUT_BYTES], "stderr": result.stderr[:MAX_OUTPUT_BYTES], "return_code": result.returncode}
    except subprocess.TimeoutExpired:
        return {"success": False, "stdout": "", "stderr": f"Command timed out after {timeout}s", "return_code": -1}
    except FileNotFoundError:
        return {"success": False, "stdout": "", "stderr": f"Binary not found at {OPEN_MAINFRAME_BIN}", "return_code": -1}
```

**Tool pattern — each @tool function:**
```python
from langchain.tools import tool
from .base import run_cli, sanitize_path

@tool
def compile_cobol(source_file: str) -> dict:
    """Compile a COBOL source file to a native executable."""
    path = sanitize_path(source_file)
    return run_cli(["compile", path])
```

**IDCAMS security — verb allowlist + shell metacharacter rejection:**
```python
ALLOWED_VERBS = {"DEFINE", "DELETE", "REPRO", "LISTCAT", "PRINT"}

@tool
def idcams_command(command: str) -> dict:
    """Execute an IDCAMS command. Supports: DEFINE, DELETE, REPRO, LISTCAT, PRINT."""
    verb = command.strip().split()[0].upper()
    if verb not in ALLOWED_VERBS:
        return {"success": False, "stderr": f"Verb '{verb}' not allowed"}
    if re.search(r'[;&|`$]', command):
        return {"success": False, "stderr": "Shell metacharacters not allowed"}
    return run_cli(["idcams", command])
```

---

### BATCH 3: Compilation Agent (E-500)

**Goal:** Compile COBOL through chat with error explanation.

**Create/modify:**
- `agent/src/nodes/__init__.py`
- `agent/src/nodes/router.py` — Router with COMPILE classification
- `agent/src/nodes/chat.py` — General conversation fallback
- `agent/src/nodes/compile.py` — Compile node with tools
- Update `agent/src/agent.py` — Add compile + chat + router nodes to graph

**Router system prompt:**
```
Classify the user's intent into exactly one category:
- COMPILE: compiling COBOL files, syntax checking, fixing errors, building programs
- EXECUTE: running JCL jobs, interpreting COBOL programs
- EXPLAIN: explaining code, extracting business rules
- ASSESS: scanning codebases, metrics, compatibility reports
- DATASET: managing datasets, browsing catalogs, IDCAMS
- CHAT: general conversation, greetings, questions about the tool
Respond with ONLY the category name.
```

**Compile node system prompt:**
```
You are a COBOL compilation expert. Help users compile COBOL programs using OpenMainframe.
When compilation fails:
1. Parse the error output to identify line numbers and error types
2. Explain each error in plain English
3. Suggest specific code fixes when possible
4. Say "I'm not sure about the fix" when uncertain — never hallucinate
When compilation succeeds, congratulate and mention the output binary location.
```

---

### BATCH 4: Execution Agent (E-600)

**Goal:** Run JCL and interpret COBOL with HITL approval.

**Create/modify:**
- `agent/src/nodes/execute.py` — Execute node with `interrupt()` before run_jcl/interpret
- Update router with EXECUTE classification
- Update `agent/src/agent.py` graph

**HITL pattern using LangGraph interrupt:**
```python
from langgraph.types import interrupt, Command

async def execute_node(state, config):
    # ... LLM decides to call run_jcl ...
    for tc in tool_calls:
        if tc.name in ("run_jcl", "interpret_cobol"):
            approval = interrupt({
                "action": f"Execute {tc.name}",
                "file": tc.args.get("jcl_file") or tc.args.get("source_file"),
                "description": f"Run {tc.name}",
            })
            if not approval.get("approved", False):
                return Command(goto="__end__", update={"messages": [AIMessage(content="Execution cancelled.")]})
    # ... proceed with tool execution ...
```

---

### BATCH 5: Explanation Agent (E-700)

**Goal:** Explain COBOL/JCL code with business rule extraction.

**Create/modify:**
- `agent/src/nodes/explain.py` — Explain node (LLM-only, reads files, uses lex/parse optionally)
- Update router with EXPLAIN classification

**Explain system prompt:**
```
You are a COBOL and JCL code explanation expert. When asked to explain code:
1. Provide section-by-section explanations for each COBOL division
2. Identify key data structures and their purposes
3. Extract business rules in "When [condition], then [action]" format
4. Highlight external dependencies (CALL, COPY, EXEC SQL/CICS)
5. Use clear, non-technical language where possible
```

---

### BATCH 6: Dataset Agent (E-800)

**Goal:** Browse catalogs and manage datasets through chat.

**Create/modify:**
- `agent/src/nodes/dataset.py` — Dataset node with HITL for DELETE
- Update router with DATASET classification

---

### BATCH 7: Assess CLI Prerequisites (E-100) — RUST

**Goal:** Add `assess scan` and `assess file` commands to OpenMainframe CLI binary.

**Modify files in the EXISTING Rust codebase (NOT in openmainframe-agent/):**
- `crates/open-mainframe/src/main.rs` — Add `assess` subcommand
- `crates/open-mainframe/src/assess.rs` (new) — CLI handler wiring to assess crate

**Pattern:**
```rust
// In main.rs — add to clap subcommands:
#[derive(clap::Subcommand)]
enum Commands {
    // ... existing commands ...
    /// Assess COBOL source files for modernization readiness
    Assess {
        #[command(subcommand)]
        command: AssessCommand,
    },
}

#[derive(clap::Subcommand)]
enum AssessCommand {
    /// Scan a directory for COBOL files and produce assessment report
    Scan {
        directory: PathBuf,
        #[arg(long, default_value = "text")]
        format: String,
    },
    /// Assess a single COBOL file
    File {
        path: PathBuf,
        #[arg(long, default_value = "text")]
        format: String,
    },
}
```

Wire to existing `open-mainframe-assess` crate's `Scanner` and `Analyzer`. Output JSON when `--format json` is passed.

---

### BATCH 8: Assessment Agent (E-400)

**Goal:** Scan codebases through chat, show metrics and compatibility reports.

**Create/modify:**
- `agent/src/nodes/assess.py` — Assess node with assess_scan/assess_file tools
- Update router with ASSESS classification
- Directory discovery logic (set project_path, populate source_files)

---

### BATCH 9a: UI Layout + Viewers (E-900 part 1)

**Goal:** Three-column layout, file tree, code viewer.

**Create/modify in `openmainframe-agent/src/`:**
- `components/layout/Header.tsx`
- `components/layout/FileTreePanel.tsx`
- `components/layout/WorkspacePanel.tsx`
- `components/layout/StatusBar.tsx`
- `components/workspace/CodeViewer.tsx` — COBOL/JCL syntax highlighting
- `components/workspace/WelcomeScreen.tsx`
- `components/workspace/TabManager.tsx`
- `hooks/useAgentState.ts` — Typed wrapper around useCoAgent
- `hooks/useWorkspaceTabs.ts`
- Update `page.tsx` — 3-column layout

**Dark theme colors (UX design spec):**
```css
--bg: #1e1e2e;
--surface: #282a36;
--border: #44475a;
--text: #f8f8f2;
--text-muted: #6272a4;
--accent: #8be9fd;
--success: #50fa7b;
--warning: #f1fa8c;
--error: #ff5555;
--info: #bd93f9;
```

---

### BATCH 9b: UI Data Components (E-900 part 2)

**Goal:** Assessment dashboard, job timeline, generative UI cards.

**Create/modify:**
- `components/workspace/AssessmentDashboard.tsx`
- `components/workspace/JobTimeline.tsx`
- `components/chat/AssessmentCard.tsx`
- `components/chat/CompilerOutputCard.tsx`
- `components/chat/ApprovalCard.tsx`
- `components/chat/ProgressCard.tsx`
- `components/chat/ExplanationCard.tsx`

**Register generative UI in page.tsx:**
```typescript
useRenderToolCall({
  name: "assess_scan",
  render: ({ status, args }) => {
    if (status === "inProgress") return <ProgressCard label="Scanning..." />;
    return <AssessmentCard />;
  },
});

useRenderToolCall({
  name: "compile_cobol",
  render: ({ status, args }) => {
    if (status === "inProgress") return <ProgressCard label={`Compiling ${args?.source_file}`} />;
    return <CompilerOutputCard />;
  },
});
```

---

### BATCH 10: Human-in-the-Loop Polish (E-1000)

**Goal:** Approval cards rendering in CopilotKit chat.

**Create/modify:**
- Wire `useHumanInTheLoop` or interrupt handling in page.tsx
- Ensure ApprovalCard renders properly with approve/reject
- Card grays out after decision

---

### BATCH 11: State Management (E-1100)

**Goal:** Bi-directional state sync, progress indicators, PostgreSQL persistence.

**Create/modify:**
- `hooks/useAgentState.ts` — Full typed state sync
- Progress indicators in StatusBar and chat
- PostgreSQL checkpointer option in agent config

---

### BATCH 12: Integration Testing (E-1200)

**Goal:** Validate against CardDemo.

**Create:**
- `openmainframe-agent/tests/integration/test_assessment.py`
- `openmainframe-agent/tests/integration/test_compilation.py`
- `openmainframe-agent/tests/integration/test_execution.py`
- `openmainframe-agent/tests/integration/test_explanation.py`

Test against: `aws-mainframe-modernization-carddemo/app/cbl/`

---

## Important Rules

1. **ONE BATCH PER ITERATION** — Implement one batch, commit, then exit. The loop restarts for the next batch.
2. **WRITE REAL CODE** — Not pseudocode, not architecture docs. Write complete, syntactically valid source files.
3. **FOLLOW THE ARCHITECTURE** — Read `_bmad-output/agent-app/architecture-openmainframe-agent.md` for exact code patterns. Use the code snippets from that doc as the ground truth.
4. **USE LATEST COPILOTKIT APIS** — Use `LangGraphAgent` (not `LangGraphHttpAgent`), `create_agent` from `langchain.agents`, `CopilotKitMiddleware`, `useRenderToolCall`, `useHumanInTheLoop`.
5. **COMMIT FORMAT:** `feat(agent): Batch N — <description>` with a summary of files created.
6. **BATCH_LOG.md** — After each batch, append to `openmainframe-agent/BATCH_LOG.md`:
   ```
   ## Batch N: <Title>
   - Status: COMPLETE
   - Date: YYYY-MM-DD
   - Files: <list>
   - Notes: <any issues>
   ```
7. **READ BEFORE WRITE** — Always read existing files before modifying them. Don't overwrite work from previous batches.
8. **DON'T INSTALL** — Just create files. Don't run npm install, pip install, cargo build, or any package manager commands.
9. **SECURITY** — Path sanitization on all file paths. Verb allowlist on IDCAMS. No shell metacharacters. No command injection.
10. **PIN VERSIONS** — Use specific version numbers in package.json and pyproject.toml.

---

## Reference Documents (READ THESE)

| Document | Path | Purpose |
|----------|------|---------|
| Architecture | `_bmad-output/agent-app/architecture-openmainframe-agent.md` | **PRIMARY REFERENCE** — exact code patterns, file structure, graph definition |
| Epics & Stories | `_bmad-output/agent-app/epics-openmainframe-agent.md` | Acceptance criteria for every story |
| Sprint Plan | `_bmad-output/agent-app/sprint-plan.md` | Batch order, dependencies, exit criteria |
| PRD | `_bmad-output/agent-app/prd-openmainframe-agent.md` | Functional requirements and data model |
| UX Design | `_bmad-output/agent-app/ux-design-openmainframe-agent.md` | UI wireframes, color scheme, component hierarchy |
| CopilotKit Research | `_bmad-output/agent-app/research-technical-copilotkit-langgraph.md` | CopilotKit integration patterns |
| OpenMainframe Tools | `_bmad-output/agent-app/research-technical-openmainframe-tools.md` | CLI command mapping, assess crate audit |

---

## OpenMainframe CLI Commands (what the tools wrap)

```
open-mainframe compile <file>       # Compile COBOL to native binary
open-mainframe check <file>         # Syntax check COBOL without compilation
open-mainframe run <jcl>            # Execute a JCL job
open-mainframe interpret <file>     # Tree-walking interpreter for COBOL
open-mainframe lex <file>           # Tokenize COBOL source
open-mainframe parse-jcl <file>     # Parse JCL to AST
open-mainframe assess scan <dir>    # [BATCH 7] Scan directory for assessment
open-mainframe assess file <path>   # [BATCH 7] Assess single file
open-mainframe idcams <command>     # IDCAMS dataset management
```

---

## Completion

When ALL 12 batches are implemented (BATCH_LOG.md shows all 12 as COMPLETE), output the completion promise:

**OPENMAINFRAME AGENT COMPLETE**
