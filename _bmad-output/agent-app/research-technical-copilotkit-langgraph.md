---
stepsCompleted: [1, 2]
phase: 1-analysis
bmadWorkflow: Technical Research (TR)
agent: Mary (Analyst)
date: 2026-02-17
---

# CopilotKit + LangGraph Technical Feasibility Research

**Date:** February 2026
**BMAD Phase:** 1-Analysis | **Step:** 2 of 11 | **Agent:** Mary 📊 Business Analyst

---

## Table of Contents

1. [CopilotKit Architecture](#1-copilotkit-architecture)
2. [LangGraph CoAgents Integration](#2-langgraph-coagents-integration)
3. [LangGraph Agent Design](#3-langgraph-agent-design)
4. [Project Structure](#4-project-structure)
5. [Wrapping CLI Tools as LangGraph Tools](#5-wrapping-cli-tools-as-langgraph-tools)
6. [Key Architectural Decisions](#6-key-architectural-decisions)

---

## 1. CopilotKit Architecture

### 1.1 CopilotKit Runtime (Node.js Middleware)

The Copilot Runtime is the back-end component of CopilotKit. It acts as a middleware layer that handles communication with LLMs, manages message history, state synchronization, and agent coordination.

**Core Responsibilities:**
- Proxying requests between the React frontend and LLM providers (or LangGraph agents)
- Managing message history and conversational state
- Routing tool calls to backend actions or forwarding frontend actions
- Supporting multiple LLM adapters (OpenAI, Anthropic, Google, Groq, LangChain)
- Connecting to remote agent endpoints (LangGraph Platform, self-hosted FastAPI)

**Runtime Endpoint Setup (Next.js App Router):**

```typescript
// app/api/copilotkit/route.ts
import {
  CopilotRuntime,
  ExperimentalEmptyAdapter,
  copilotRuntimeNextJSAppRouterEndpoint,
} from "@copilotkit/runtime";
import { LangGraphHttpAgent } from "@copilotkit/runtime/langgraph";
import { NextRequest } from "next/server";

const serviceAdapter = new ExperimentalEmptyAdapter();

const runtime = new CopilotRuntime({
  agents: {
    sample_agent: new LangGraphHttpAgent({
      url: process.env.AGENT_URL || "http://localhost:8123",
    }),
  },
});

export const POST = async (req: NextRequest) => {
  const { handleRequest } = copilotRuntimeNextJSAppRouterEndpoint({
    runtime,
    serviceAdapter,
    endpoint: "/api/copilotkit",
  });
  return handleRequest(req);
};
```

**Key insight:** When using a single LangGraph agent in "agent-lock mode," the LLM adapter is only used for peripherals like chat suggestions. The primary LLM interaction happens inside the LangGraph agent itself. For pure agent-lock mode, `ExperimentalEmptyAdapter` is used.

**Agent Connection Modes:**
- `LangGraphHttpAgent` — connects to a self-hosted FastAPI endpoint serving a LangGraph agent via the AG-UI protocol
- `LangGraphAgent` — connects to LangGraph Platform (cloud or self-hosted) using a deployment URL and graph ID
- Backend actions can be defined directly on the `CopilotRuntime` via an `actions` generator function

**Supported Runtime Environments:**
- Next.js App Router / Pages Router
- Node.js Express / HTTP
- NestJS

### 1.2 CopilotKit React Components

**`<CopilotKit>` Provider** — The root context provider:
```tsx
import { CopilotKit } from "@copilotkit/react-core";

export default function RootLayout({ children }) {
  return (
    <CopilotKit runtimeUrl="/api/copilotkit">
      {children}
    </CopilotKit>
  );
}
```

**UI Components:**
- `<CopilotChat>` — Flexible chat interface, freely positionable
- `<CopilotPopup>` — Floating popup chat window
- `<CopilotSidebar>` — Collapsible sidebar wrapping main content
- Headless UI via `useCopilotChat` hook for fully custom interfaces

### 1.3 Frontend Actions (`useCopilotAction`)

```tsx
useCopilotAction({
  name: "highlightCode",
  description: "Highlight a specific code section in the editor",
  available: "remote",  // only available to agent, not direct LLM
  parameters: [
    { name: "filePath", type: "string", required: true },
    { name: "lineStart", type: "number", required: true },
    { name: "lineEnd", type: "number", required: true },
  ],
  handler: async ({ filePath, lineStart, lineEnd }) => {
    // Execute in the browser — update UI state
    setHighlightedRange({ filePath, lineStart, lineEnd });
  },
});
```

**How frontend actions reach the agent:** CopilotKit injects frontend actions into `state["copilotkit"]["actions"]`. The agent binds them alongside backend tools:
```python
fe_tools = state.get("copilotkit", {}).get("actions", [])
model_with_tools = model.bind_tools([*fe_tools, *tools])
```

### 1.4 Frontend Readable State (`useCopilotReadable`)

```tsx
useCopilotReadable({
  description: "Currently selected COBOL source file",
  value: JSON.stringify({ path: selectedFile, content: fileContent }),
});
```

Reaches agent via `state["copilotkit"]["context"]` as `{ description, value }` objects.

---

## 2. LangGraph CoAgents Integration

### 2.1 CoAgent Pattern

CopilotKit's "CoAgent" pattern extends LangGraph with:
- **Bi-directional state sharing** between agent and UI
- **Message management** where CopilotKit acts as ground-truth
- **Human-in-the-loop** via interrupt nodes
- **Generative UI** rendering tool calls as React components
- **Frontend tool invocation** from within the agent

```tsx
import { useCoAgent } from "@copilotkit/react-core";

type AgentState = {
  assessmentResults: AssessmentReport | null;
  migrationPlan: MigrationPlan | null;
};

const { state, setState } = useCoAgent<AgentState>({
  name: "modernization_agent",
  initialState: { assessmentResults: null, migrationPlan: null },
});
```

### 2.2 Shared State Mechanism

**Agent → UI:** `useCoAgent` returns reactive `state` that auto-updates when agent state changes.

**UI → Agent:** `setState()` pushes state to the agent immediately.

**Agent State (Python):**
```python
from copilotkit import CopilotKitState

class AgentState(CopilotKitState):
    """Inherits: messages (list), copilotkit (dict)"""
    assessment_results: dict | None
    migration_plan: dict | None
    compilation_output: str | None
```

**Predictive State Updates:** CopilotKit streams state mid-node execution. If state is not persisted at node boundary, streamed updates disappear. Finalized state at node boundaries is the source of truth.

### 2.3 Human-in-the-Loop

**Interrupt-based (Recommended):**

```python
from langgraph.types import interrupt

def migration_approval_node(state):
    plan = state["migration_plan"]
    result = interrupt({
        "question": f"Approve migration of {plan['program_count']} programs?",
        "plan_summary": plan["summary"],
        "estimated_effort": plan["effort_hours"]
    })
    if result.get("approved"):
        return {"migration_status": "approved"}
    return {"migration_status": "rejected", "feedback": result.get("reason")}
```

**Frontend handling:**
```tsx
useLangGraphInterrupt({
  render: ({ event, resolve }) => (
    <MigrationApprovalCard
      plan={event.value.plan_summary}
      effort={event.value.estimated_effort}
      onApprove={() => resolve({ approved: true })}
      onReject={(reason) => resolve({ approved: false, reason })}
    />
  ),
});
```

### 2.4 Generative UI

Three approaches:

**1. Tool-based rendering (backend tools rendered in frontend):**
```tsx
useCopilotAction({
  name: "run_assessment",
  available: "disabled",  // render-only
  render: ({ status, args, result }) => {
    if (status === "executing") return <AssessmentProgress />;
    if (status === "complete") return <AssessmentReport data={result} />;
    return null;
  },
});
```

**2. Agent state rendering:**
```tsx
useCoAgentStateRender<AgentState>({
  name: "modernization_agent",
  nodeName: "assess_node",
  render: ({ status, state }) => (
    <AssessmentDashboard
      results={state.assessment_results}
      isRunning={status === "inProgress"}
    />
  ),
});
```

### 2.5 AG-UI Protocol

AG-UI (Agent-User Interaction Protocol) is the open event-based streaming standard:
- Server-Sent Events for real-time communication
- Framework-agnostic (LangGraph, ADK, AG2, etc.)
- Event types: `onRunStartedEvent`, `onStateChanged`, `onMessagesChanged`, `onCustomEvent`, `onRunFinalized`

**Python integration:**
```python
from copilotkit import LangGraphAGUIAgent
from ag_ui_langgraph import add_langgraph_fastapi_endpoint

add_langgraph_fastapi_endpoint(
    app=app,
    agent=LangGraphAGUIAgent(
        name="modernization_agent",
        description="Mainframe modernization assistant",
        graph=graph,
    ),
    path="/",
)
```

---

## 3. LangGraph Agent Design

### 3.1 Graph Structure

```python
from langgraph.graph import StateGraph

workflow = StateGraph(AgentState)
workflow.add_node("router", router_node)
workflow.add_node("assess", assess_node)
workflow.add_node("compile", compile_node)
workflow.add_node("execute", execute_node)
workflow.add_node("explain", explain_node)
workflow.add_node("tools", ToolNode(tools=tools))

workflow.set_entry_point("router")
workflow.add_conditional_edges("router", route_intent, {
    "assess": "assess",
    "compile": "compile",
    "execute": "execute",
    "explain": "explain",
})
workflow.add_edge("tools", "router")

graph = workflow.compile(checkpointer=MemorySaver())
```

### 3.2 Tool Definitions

```python
from langchain.tools import tool

@tool
def compile_cobol(source_file: str) -> dict:
    """Compile a COBOL source file using OpenMainframe."""
    result = subprocess.run(
        ["./target/release/open-mainframe", "compile", source_file],
        capture_output=True, text=True, timeout=60
    )
    return {
        "success": result.returncode == 0,
        "stdout": result.stdout[:10000],
        "stderr": result.stderr[:5000],
        "returncode": result.returncode,
    }
```

### 3.3 Multi-Agent Patterns

CopilotKit supports LangGraph subgraphs:
```tsx
const { state, nodeName } = useCoAgent<AgentState>({
  name: "modernization_agent",
  config: { streamSubgraphs: true },
});
// nodeName reveals which sub-agent is active
```

### 3.4 Checkpointing

```python
# Development:
from langgraph.checkpoint.memory import MemorySaver
graph = workflow.compile(checkpointer=MemorySaver())

# Production:
from langgraph.checkpoint.postgres.aio import AsyncPostgresSaver
async with AsyncPostgresSaver.from_conn_string(DB_URL) as checkpointer:
    await checkpointer.setup()
    graph = workflow.compile(checkpointer=checkpointer)
```

---

## 4. Project Structure

### 4.1 Recommended Layout for OpenMainframe Agent

```
openmainframe-agent/
├── agent/                          # Python LangGraph agent
│   ├── main.py                     # FastAPI entry point
│   ├── pyproject.toml              # Python dependencies
│   ├── .env                        # API keys
│   └── src/
│       ├── agent.py                # Graph definition, state, nodes
│       ├── tools/
│       │   ├── assess.py           # open-mainframe assess wrapper
│       │   ├── compile.py          # open-mainframe compile wrapper
│       │   ├── execute.py          # open-mainframe run wrapper
│       │   ├── interpret.py        # open-mainframe interpret wrapper
│       │   └── dataset.py          # open-mainframe dataset tools
│       ├── nodes/
│       │   ├── router.py           # Intent classification
│       │   ├── assess_node.py      # Assessment workflow
│       │   ├── compile_node.py     # Compilation workflow
│       │   ├── execute_node.py     # JCL execution workflow
│       │   ├── explain_node.py     # Code explanation
│       │   └── migrate_node.py     # Migration planning
│       └── util.py                 # Shared utilities
│
├── src/                            # Next.js frontend
│   ├── app/
│   │   ├── layout.tsx              # CopilotKit provider
│   │   ├── page.tsx                # Main workspace
│   │   └── api/copilotkit/route.ts # CopilotRuntime endpoint
│   ├── components/
│   │   ├── AssessmentDashboard.tsx  # Assessment results UI
│   │   ├── CodeViewer.tsx          # COBOL syntax highlighting
│   │   ├── CompilerOutput.tsx      # Compilation results
│   │   ├── MigrationWizard.tsx     # Migration planning UI
│   │   └── DatasetExplorer.tsx     # Dataset browser
│   └── lib/
│       └── types.ts                # Shared types
│
├── package.json
├── next.config.js
└── .env
```

### 4.2 Key NPM Dependencies

```json
{
  "@copilotkit/react-core": "latest",
  "@copilotkit/react-ui": "latest",
  "@copilotkit/runtime": "latest"
}
```

### 4.3 Key Python Dependencies

```
copilotkit
ag-ui-langgraph
langgraph
langchain
langchain-openai
langchain-anthropic
fastapi
uvicorn
python-dotenv
```

### 4.4 Development Workflow

```bash
npm run dev  # Starts Next.js + Python agent concurrently
```

---

## 5. Wrapping CLI Tools as LangGraph Tools

### 5.1 OpenMainframe CLI as Agent Tools

```python
import subprocess
import json
from langchain.tools import tool

OPEN_MAINFRAME_BIN = "./target/release/open-mainframe"

@tool
def assess_cobol(directory: str) -> dict:
    """Scan a directory of COBOL source files and return assessment metrics."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "assess", "scan", directory, "--format", "json"],
        capture_output=True, text=True, timeout=120
    )
    if result.returncode == 0:
        return json.loads(result.stdout)
    return {"error": result.stderr, "returncode": result.returncode}

@tool
def compile_cobol(source_file: str) -> dict:
    """Compile a COBOL source file and return compilation results."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "compile", source_file],
        capture_output=True, text=True, timeout=60
    )
    return {
        "success": result.returncode == 0,
        "output": result.stdout[:10000],
        "errors": result.stderr[:5000],
    }

@tool
def check_cobol(source_file: str) -> dict:
    """Syntax check a COBOL source file without compiling."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "check", source_file],
        capture_output=True, text=True, timeout=30
    )
    return {
        "valid": result.returncode == 0,
        "diagnostics": result.stderr if result.returncode != 0 else "No issues found",
    }

@tool
def run_jcl(jcl_file: str) -> dict:
    """Execute a JCL job and return step results."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "run", jcl_file],
        capture_output=True, text=True, timeout=300
    )
    return {
        "success": result.returncode == 0,
        "output": result.stdout[:20000],
        "errors": result.stderr[:5000],
        "return_code": result.returncode,
    }

@tool
def interpret_cobol(source_file: str) -> dict:
    """Interpret a COBOL program (tree-walking execution)."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "interpret", source_file],
        capture_output=True, text=True, timeout=120
    )
    return {
        "output": result.stdout[:20000],
        "errors": result.stderr[:5000],
        "return_code": result.returncode,
    }

@tool
def parse_jcl(jcl_file: str) -> dict:
    """Parse a JCL file and return the AST structure."""
    result = subprocess.run(
        [OPEN_MAINFRAME_BIN, "parse-jcl", jcl_file],
        capture_output=True, text=True, timeout=30
    )
    return {
        "ast": result.stdout[:20000],
        "errors": result.stderr[:5000] if result.returncode != 0 else None,
    }
```

### 5.2 Error Handling Best Practices

```python
import asyncio

@tool
async def async_compile(source_file: str) -> dict:
    """Compile COBOL asynchronously with timeout."""
    try:
        proc = await asyncio.create_subprocess_exec(
            OPEN_MAINFRAME_BIN, "compile", source_file,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout, stderr = await asyncio.wait_for(proc.communicate(), timeout=120.0)
        return {
            "success": proc.returncode == 0,
            "output": stdout.decode()[:10000],
            "errors": stderr.decode()[:5000],
        }
    except asyncio.TimeoutError:
        proc.kill()
        return {"error": "Compilation timed out after 120 seconds."}
```

---

## 6. Key Architectural Decisions

### 6.1 Recommended Stack for OpenMainframe Agent

| Layer | Technology | Rationale |
|-------|-----------|-----------|
| Frontend | Next.js 14+ (App Router) | Native CopilotKit support |
| UI Layout | `CopilotSidebar` | Workspace-style: code left, chat right |
| Runtime | Self-hosted CopilotRuntime | Full control, agent-lock mode |
| Agent | LangGraph (Python) + FastAPI | Rich Python ecosystem for subprocess/CLI wrapping |
| Protocol | AG-UI via `ag_ui_langgraph` | Standard streaming protocol |
| LLM | Claude (Anthropic) or GPT-4o | Strong tool-calling + code understanding |
| State Sync | `useCoAgent` + `CopilotKitState` | Bi-directional state |
| Persistence | MemorySaver (dev) / PostgreSQL (prod) | LangGraph checkpointing |
| CLI Backend | `subprocess.run` wrapping OpenMainframe binary | Direct CLI tool invocation |

### 6.2 Technical Feasibility Assessment

| Capability | Feasibility | Notes |
|-----------|------------|-------|
| Code Assessment via Agent | **HIGH** | Wrap `open-mainframe assess` as LangGraph tool |
| COBOL Compilation | **HIGH** | Wrap `open-mainframe compile/check` |
| JCL Execution | **HIGH** | Wrap `open-mainframe run` |
| Code Explanation | **HIGH** | LLM + AST from `open-mainframe lex` |
| Migration Planning | **MEDIUM** | Requires assess + dependency graph (partially implemented) |
| Interactive Debugging | **MEDIUM** | `open-mainframe interpret` exists but lacks debugger API |
| Generative UI Dashboards | **HIGH** | CopilotKit generative UI pattern is well-suited |
| Human-in-the-Loop Approval | **HIGH** | LangGraph interrupt pattern is mature |
| Dataset Management | **HIGH** | `open-mainframe idcams` and dataset tools exist |

### 6.3 Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| CLI tool timeouts for large codebases | Medium | Async subprocess + configurable timeouts |
| Assess crate not exposed via CLI | High | Must add `assess` CLI command first |
| No call graph analysis yet | Medium | Implement Epic 1002 or build Python-side analysis |
| LLM hallucination in code explanation | Medium | Ground explanations in AST data |
| State size limits for large assessments | Low | Summarize results, store full data in files |

---

## Sources

- CopilotKit documentation: https://docs.copilotkit.ai
- CopilotKit `with-langgraph-fastapi` repo
- CopilotKit `with-langgraph-python` repo
- AG-UI protocol: https://docs.ag-ui.com
- LangGraph documentation: https://docs.langchain.com/oss/python/langgraph/overview
