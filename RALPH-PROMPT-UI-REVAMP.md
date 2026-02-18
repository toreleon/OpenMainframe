# Ralph Loop Prompt: BMAD UI/UX Revamp — Chat-First + Local Environment Agent

## Mission

Revamp the OpenMainframe Agent app from an IDE-style 3-panel layout to a **chat-first conversational UI** where the AI agent connects to and operates on the **customer's local environment** — their source code never leaves their machine. Think Google Colab's "Connect to local runtime" model applied to mainframe modernization.

---

## Core Design Principles

1. **Chat is the primary interface** — All interaction flows through the conversation. No file tree panel, no workspace tabs as primary navigation. Rich inline cards render results directly in the chat stream.
2. **Code stays local** — The customer's COBOL/JCL source code never uploads to our platform. A lightweight local bridge (WebSocket relay) runs on their machine next to the OpenMainframe CLI binary.
3. **Agent reaches into the customer's environment** — The Python agent sends commands through the local bridge. The bridge executes the CLI and streams results back. The customer sees everything in the chat.
4. **Progressive disclosure** — Simple chat by default. Expandable panels for deep inspection (assessment reports, code viewers, job timelines) appear as collapsible sections within the chat or as slide-over drawers, not permanent panels.

---

## Architecture Overview

```
┌──────────────────────────────────────────────────────────┐
│  Customer's Browser (Next.js Frontend)                   │
│  ┌────────────────────────────────────────────────────┐  │
│  │  CopilotKit Chat UI (full-screen, chat-first)      │  │
│  │  - Inline rich cards (assessment, compile, etc.)    │  │
│  │  - Connection status indicator                      │  │
│  │  - Expandable detail drawers                        │  │
│  └────────────────────────────────────────────────────┘  │
│              │ POST /api/copilotkit                       │
│              ▼                                            │
│  ┌────────────────────────────────────────────────────┐  │
│  │  Next.js API Route (CopilotRuntime)                │  │
│  │  LangGraphHttpAgent → Python Agent                 │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
               │ AG-UI Protocol
               ▼
┌──────────────────────────────────────────────────────────┐
│  Python Agent (FastAPI + LangGraph) — Cloud/Server       │
│  - Router → Capability Nodes → Tool Calls                │
│  - Tools NO LONGER call CLI directly                     │
│  - Tools send commands via WebSocket to Local Bridge     │
│  - Agent orchestrates, bridge executes                   │
└──────────────────────────────────────────────────────────┘
               │ WebSocket (wss://)
               ▼
┌──────────────────────────────────────────────────────────┐
│  Customer's Local Machine                                │
│  ┌────────────────────────────────────────────────────┐  │
│  │  Local Bridge (lightweight daemon)                  │  │
│  │  - WebSocket client connects to agent server        │  │
│  │  - Receives CLI commands, executes open-mainframe   │  │
│  │  - Streams stdout/stderr back                       │  │
│  │  - File listing, file read (on-demand, not bulk)    │  │
│  │  - Sandboxed: only operates within project dir      │  │
│  └────────────────────────────────────────────────────┘  │
│  ┌────────────────────────────────────────────────────┐  │
│  │  open-mainframe CLI binary                          │  │
│  │  Customer's COBOL/JCL source code                   │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
```

---

## BMAD Phase 1: Business Analysis & Requirements

### Problem Statement

The current OpenMainframe Agent UI is an IDE-style 3-panel layout (file tree + workspace + chat sidebar). This design:
- Requires users to upload/expose their mainframe source code to the platform server
- Overwhelms users with IDE complexity when they just want to talk to an AI about their code
- Doesn't scale to enterprise customers with strict data residency requirements
- Competes with actual IDEs (VS Code, Eclipse) where mainframe devs already work

### Target Users

1. **Mainframe application developers** — Want to assess and modernize COBOL/JCL but can't send code offsite
2. **Enterprise architects** — Need assessment reports and recommendations via conversation
3. **DevOps teams** — Want to run compile/execute cycles through natural language

### Key User Stories

- US-1: As a developer, I open the app and see a chat interface. I type "Connect to my project at ~/carddemo" and the agent connects to my local machine.
- US-2: As a developer, I say "Assess my COBOL programs" and see assessment results appear as rich cards inline in the chat, with expandable details.
- US-3: As a developer, I say "Compile CBACT01C.cbl" and see a compile result card with success/failure, errors, and warnings — all executed on my local machine.
- US-4: As a developer, I can click "View full report" on an assessment card and a slide-over drawer opens with the full AssessmentDashboard, without leaving the chat.
- US-5: As an enterprise architect, I'm assured my source code never leaves my network — only CLI command results (JSON metadata) flow through the agent.
- US-6: As a developer, I see a persistent connection status indicator showing whether my local bridge is connected.
- US-7: As a developer, I can run "Execute RUNCARD.jcl" and the agent asks for approval (HITL) inline in the chat before executing on my machine.

---

## BMAD Phase 2: Technical Specification

### Batch 1: Local Bridge Daemon (Python)

**Goal:** Create a lightweight WebSocket client that runs on the customer's machine, receives commands from the agent server, executes the OpenMainframe CLI, and streams results back.

**Files to create:**
- `openmainframe-agent/bridge/bridge.py` — Main daemon: WebSocket client, command executor, file operations
- `openmainframe-agent/bridge/protocol.py` — Message protocol definitions (JSON schema for commands/responses)
- `openmainframe-agent/bridge/security.py` — Path sandboxing, command allowlist validation
- `openmainframe-agent/bridge/requirements.txt` — Minimal deps: `websockets`, `click`

**Protocol messages:**
```
→ Server-to-Bridge: { "type": "exec", "id": "uuid", "command": "assess scan /path --format json" }
→ Server-to-Bridge: { "type": "list_files", "id": "uuid", "directory": "/path", "pattern": "*.cbl" }
→ Server-to-Bridge: { "type": "read_file", "id": "uuid", "path": "/path/file.cbl", "lines": [1, 50] }
→ Server-to-Bridge: { "type": "ping" }

← Bridge-to-Server: { "type": "result", "id": "uuid", "status": "ok", "data": {...} }
← Bridge-to-Server: { "type": "result", "id": "uuid", "status": "error", "error": "..." }
← Bridge-to-Server: { "type": "stream", "id": "uuid", "chunk": "partial output..." }
← Bridge-to-Server: { "type": "pong", "project_path": "/path", "cli_version": "1.0" }
```

**Security:**
- Bridge only accepts connections authenticated with a one-time token displayed at startup
- All file paths sandboxed to the project directory
- Only allowlisted CLI subcommands: assess, compile, check, run, interpret, parse-jcl, lex, idcams
- Command output truncated at 50KB

### Batch 2: Agent-Side Bridge Integration

**Goal:** Replace the current `run_cli()` subprocess calls with WebSocket commands to the local bridge.

**Files to modify:**
- `openmainframe-agent/agent/src/tools/base.py` — Replace `subprocess.run` with `await bridge_client.execute(command)`
- `openmainframe-agent/agent/src/tools/__init__.py` — Tools now async-first, awaiting bridge responses

**Files to create:**
- `openmainframe-agent/agent/src/bridge_client.py` — WebSocket server that accepts bridge connections, routes commands, tracks sessions
- `openmainframe-agent/agent/src/session.py` — Session manager: maps user sessions to bridge connections

**Key design:**
- Agent server runs a WebSocket server endpoint at `/ws/bridge`
- Bridge daemon connects with a session token
- When LangGraph tools need to execute CLI, they call `bridge_client.execute(session_id, command)`
- If no bridge is connected for the session, the tool returns a helpful error: "No local environment connected. Start the bridge with: `openmainframe bridge connect`"

### Batch 3: Chat-First Frontend Layout

**Goal:** Replace the 3-panel IDE layout with a full-screen chat-first interface.

**Files to modify:**
- `openmainframe-agent/src/components/HomeContent.tsx` — Complete rewrite: full-screen CopilotChat layout
- `openmainframe-agent/src/app/layout.tsx` — Simplify: dark theme, no sidebar wrapping
- `openmainframe-agent/src/app/globals.css` — Update om-* tokens for chat-first aesthetic

**Files to remove/deprecate:**
- `openmainframe-agent/src/components/layout/FileTreePanel.tsx` — No longer needed (no file tree)
- `openmainframe-agent/src/components/layout/WorkspacePanel.tsx` — Replaced by inline chat cards + drawers
- `openmainframe-agent/src/components/workspace/TabManager.tsx` — No tabs
- `openmainframe-agent/src/components/layout/StatusBar.tsx` — Replaced by connection indicator in header

**New layout:**
```
┌─────────────────────────────────────────────┐
│  Header: Logo + Connection Status + Settings│
├─────────────────────────────────────────────┤
│                                             │
│           CopilotKit Chat Area              │
│           (full width, centered,            │
│            max-width ~800px)                │
│                                             │
│  ┌─────────────────────────────────────┐    │
│  │  [Assessment Card - inline]          │    │
│  │  ┌─ Expandable detail ────────────┐ │    │
│  │  │  Full assessment dashboard     │ │    │
│  │  └────────────────────────────────┘ │    │
│  └─────────────────────────────────────┘    │
│                                             │
│  ┌─────────────────────────────────────┐    │
│  │  [Approval Card - HITL inline]      │    │
│  └─────────────────────────────────────┘    │
│                                             │
│  ┌──────────────────────────────────────┐   │
│  │  Chat input                          │   │
│  └──────────────────────────────────────┘   │
└─────────────────────────────────────────────┘
```

### Batch 4: Rich Inline Chat Cards (Revamped)

**Goal:** Upgrade all chat cards to be self-contained, expandable, and visually polished. Each card shows a summary by default with a "View Details" toggle for full content.

**Files to modify/rewrite:**
- `openmainframe-agent/src/components/chat/AssessmentCard.tsx` — Summary card with expandable full AssessmentDashboard inline
- `openmainframe-agent/src/components/chat/CompilerOutputCard.tsx` — Success/failure with expandable error list
- `openmainframe-agent/src/components/chat/ProgressCard.tsx` — Animated progress with step indicators
- `openmainframe-agent/src/components/chat/ExplanationCard.tsx` — Code explanation with syntax-highlighted snippets
- `openmainframe-agent/src/components/chat/ApprovalCard.tsx` — HITL card, already good

**Files to create:**
- `openmainframe-agent/src/components/chat/ConnectionCard.tsx` — Shows bridge connection status, project path, CLI version
- `openmainframe-agent/src/components/chat/FileListCard.tsx` — Renders discovered source files as a compact tree
- `openmainframe-agent/src/components/chat/CodeSnippetCard.tsx` — Syntax-highlighted COBOL/JCL code viewer (inline, not a panel)

### Batch 5: Connection Status & Session Management

**Goal:** Frontend knows whether a local bridge is connected and shows status.

**Files to create:**
- `openmainframe-agent/src/hooks/useConnectionStatus.ts` — Polls agent `/health` endpoint for bridge status, exposes `{ connected, projectPath, cliVersion }`
- `openmainframe-agent/src/components/chat/ConnectionStatusBadge.tsx` — Green dot/red dot + "Connected to ~/carddemo" or "No local environment"

**Files to modify:**
- `openmainframe-agent/agent/main.py` — Extend `/health` endpoint to include bridge connection status
- `openmainframe-agent/src/components/layout/Header.tsx` — Add ConnectionStatusBadge

### Batch 6: Onboarding Flow

**Goal:** When no bridge is connected, the chat shows a friendly onboarding flow.

**Behavior:**
1. User opens the app → sees welcome message in chat
2. Chat message includes: "To get started, install the local bridge on your machine:"
3. Shows copy-pasteable commands:
   ```
   pip install openmainframe-bridge
   openmainframe bridge connect --project ~/your-cobol-project
   ```
4. ConnectionStatusBadge updates to green when bridge connects
5. Agent automatically greets: "Connected to ~/carddemo (47 COBOL files, 12 JCL files). How can I help?"

**Files to modify:**
- `openmainframe-agent/src/components/HomeContent.tsx` — Conditional initial message based on connection status
- `openmainframe-agent/src/components/chat/ConnectionCard.tsx` — Onboarding instructions card

### Batch 7: Detail Drawers (Progressive Disclosure)

**Goal:** For complex results (full assessment report, job timeline, code viewer), render a slide-over drawer that opens from the right edge without replacing the chat.

**Files to create:**
- `openmainframe-agent/src/components/drawers/Drawer.tsx` — Reusable slide-over panel (width: 60%, animation, close button)
- `openmainframe-agent/src/components/drawers/AssessmentDrawer.tsx` — Full AssessmentDashboard in a drawer
- `openmainframe-agent/src/components/drawers/CodeViewerDrawer.tsx` — Full CodeViewer in a drawer
- `openmainframe-agent/src/components/drawers/JobTimelineDrawer.tsx` — Full JobTimeline in a drawer

**Interaction:** Click "View Full Report" on an AssessmentCard → drawer slides in from right → chat is still visible underneath (dimmed).

### Batch 8: State & Hook Cleanup

**Goal:** Simplify frontend state now that we don't have workspace tabs or file tree panels.

**Files to modify:**
- `openmainframe-agent/src/hooks/useAgentState.ts` — Keep as-is (still need shared state)
- `openmainframe-agent/src/hooks/useWorkspaceTabs.ts` — Delete (no longer needed)
- `openmainframe-agent/src/hooks/useProgressSync.ts` — Delete (no auto-opening tabs)
- `openmainframe-agent/src/hooks/useInterruptHandler.tsx` — Keep as-is

**Files to delete:**
- `openmainframe-agent/src/components/workspace/WelcomeScreen.tsx`
- `openmainframe-agent/src/components/workspace/TabManager.tsx`

### Batch 9: Updated Integration Tests

**Goal:** Update integration tests for the new bridge-based tool execution.

**Files to modify:**
- `openmainframe-agent/tests/integration/conftest.py` — Mock bridge client instead of subprocess
- `openmainframe-agent/tests/integration/test_assessment.py` — Test via bridge mock
- `openmainframe-agent/tests/integration/test_compilation.py` — Test via bridge mock
- `openmainframe-agent/tests/integration/test_execution.py` — Test HITL + bridge mock
- `openmainframe-agent/tests/integration/test_explanation.py` — Test via bridge mock

**Files to create:**
- `openmainframe-agent/tests/integration/test_bridge.py` — Bridge protocol tests: connect, exec, stream, disconnect
- `openmainframe-agent/bridge/tests/test_bridge_daemon.py` — Daemon-side tests: command execution, sandboxing

---

## BMAD Phase 3: Implementation Rules

### Execution Order

Implement batches sequentially: 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9

### Tech Stack

- **Frontend:** Next.js 15 + React 19 + Tailwind CSS + CopilotKit
- **Agent:** Python 3.12 + FastAPI + LangGraph + CopilotKit Python SDK
- **Bridge:** Python 3.9+ (minimal deps) + websockets
- **Protocol:** JSON over WebSocket
- **Styling:** Dark theme with `om-*` color tokens (existing)

### CopilotKit Patterns (MUST follow)

- Use `CopilotChat` (not `CopilotSidebar`) for the main chat — full-page chat, not a sidebar
- Use `useCoAgent` for shared state between frontend and agent
- Use `useRenderToolCall` for generative UI (inline chat cards)
- Use `useLangGraphInterrupt` for HITL approval flows
- Agent name in `useCoAgent` must match the key in `CopilotRuntime.agents` (currently `"default"`)
- Frontend route.ts uses `LangGraphHttpAgent` (NOT `LangGraphAgent` — that's for LangGraph Platform)
- Python agent uses `add_langgraph_fastapi_endpoint` + `LangGraphAGUIAgent` (AG-UI protocol)

### Code Quality Rules

- All new `.tsx` files must have `"use client"` directive if they use hooks
- No JSX in `.ts` files — only in `.tsx`
- All Python async functions must use `async def` + `await`
- Bridge daemon must be self-contained with minimal dependencies
- Security: all paths sandboxed, all commands allowlisted, output size limited

### What NOT to Change

- The LangGraph graph structure (router → capability nodes → tools) stays the same
- The Python agent state schema (`AgentState`) stays the same
- The tool definitions stay the same (just their execution mechanism changes)
- The `open-mainframe` Rust CLI binary is unchanged
- The `agent/.env` configuration approach stays the same

---

## Completion Promise

This Ralph Loop is complete when:
1. The app renders a full-screen chat-first UI (no 3-panel IDE layout)
2. A local bridge daemon exists and can connect to the agent server
3. Tools execute via the bridge (not local subprocess)
4. Rich inline cards render in the chat for all tool results
5. HITL approval works inline in the chat
6. Connection status is visible in the UI
7. Detail drawers work for assessment reports, code viewing, and job timelines
8. `npx tsc --noEmit` passes with zero errors
9. `npm run build` succeeds
10. All batches are committed to git
