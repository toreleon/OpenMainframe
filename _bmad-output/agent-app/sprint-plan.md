---
version: 'v1.0'
date: '2026-02-17'
status: 'approved'
inputDocuments:
  - epics-openmainframe-agent.md
  - architecture-openmainframe-agent.md
  - readiness-report.md
---

# Sprint Plan: OpenMainframe Agent v1.0

**BMAD Phase:** 3-Solutioning | **Step:** 11 of 11 | **Agent:** Bob 🏃 Scrum Master

---

## 1. Planning Approach

### 1.1 Strategy

Based on the Readiness Report's recommendation, we use a **two-track parallel strategy**:

- **Track A (Agent App):** E-200 → E-300 → E-500 → E-600 → E-700 → E-800 → E-900 → E-1000 → E-1100 → E-1200
- **Track B (CLI Prerequisites):** E-100 → feeds into E-400 (Assessment Agent)

This maximizes throughput by not blocking on the assess CLI prerequisite.

### 1.2 Implementation Batches for Ralph Loop

Each batch is designed to be implementable in a single Ralph Loop iteration. Stories within a batch are ordered to minimize context switching.

---

## 2. Implementation Batches

### Batch 1: Project Scaffolding (E-200)

**Goal:** Working Next.js + CopilotKit frontend and Python LangGraph agent with end-to-end chat.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-200.1 | Next.js frontend scaffolding with CopilotKit | S |
| 2 | E-200.2 | CopilotKit runtime API route | S |
| 3 | E-200.3 | Python agent server scaffolding (FastAPI) | S |
| 4 | E-200.4 | LangGraph graph skeleton (router + chat) | M |
| 5 | E-200.5 | Concurrent dev server (npm run dev) | S |

**Entry Criteria:** None (first batch)
**Exit Criteria:** `npm run dev` starts both servers; typing in CopilotKit chat gets a response from the LangGraph agent.
**Estimated Implementation:** 1 iteration

---

### Batch 2: Tool Layer (E-300)

**Goal:** All OpenMainframe CLI tools wrapped as Python @tool functions with base error handling.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-300.1 | Base subprocess wrapper (run_cli) | S |
| 2 | E-300.2 | Assessment tools (assess_scan, assess_file) | S |
| 3 | E-300.3 | Compilation & execution tools | S |
| 4 | E-300.4 | Parse & dataset tools | S |

**Entry Criteria:** Batch 1 complete (agent server running)
**Exit Criteria:** All 10 tools importable and callable; unit tests pass with mocked subprocess.
**Estimated Implementation:** 1 iteration

---

### Batch 3: Compilation Agent (E-500)

**Goal:** Compile COBOL files through chat with error explanation.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-500.1 | Compile node implementation | M |
| 2 | E-500.2 | Router — compile intent classification | S |
| 3 | E-500.3 | Error explanation (LLM-powered) | M |
| 4 | E-500.4 | Batch compilation | M |

**Entry Criteria:** Batch 2 complete (tools available)
**Exit Criteria:** User says "compile HELLO.cbl" → agent compiles → returns results with error explanations.
**Estimated Implementation:** 1 iteration

---

### Batch 4: Execution Agent (E-600)

**Goal:** Run JCL jobs and interpret COBOL with HITL approval.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-600.1 | Execute node with HITL interrupt | M |
| 2 | E-600.2 | Router — execute intent classification | S |
| 3 | E-600.3 | JCL execution with step results | M |
| 4 | E-600.4 | JCL structure analysis (parse before run) | M |
| 5 | E-600.5 | COBOL interpretation | S |

**Entry Criteria:** Batch 2 complete (tools available)
**Exit Criteria:** User says "run HELLO.jcl" → approval card appears → user approves → job executes → step results shown.
**Estimated Implementation:** 1 iteration
**Note:** Batch 3 and Batch 4 can be developed in the same iteration if scope allows, since they are independent.

---

### Batch 5: Explanation Agent (E-700)

**Goal:** Explain COBOL/JCL code in natural language with business rule extraction.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-700.1 | Explain node implementation | M |
| 2 | E-700.2 | Router — explain intent classification | S |
| 3 | E-700.3 | COBOL program explanation (section-by-section) | M |
| 4 | E-700.4 | Business rule extraction | M |

**Entry Criteria:** Batch 2 complete (tools available for optional lex/parse)
**Exit Criteria:** User says "explain POLCY01.cbl" → agent provides section-by-section explanation with business rules.
**Estimated Implementation:** 1 iteration

---

### Batch 6: Dataset Agent (E-800)

**Goal:** Browse catalogs and manage datasets through chat.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-800.1 | Dataset node implementation | M |
| 2 | E-800.2 | Router — dataset intent classification | S |
| 3 | E-800.3 | Catalog browsing | S |

**Entry Criteria:** Batch 2 complete (tools available)
**Exit Criteria:** User says "list datasets" → catalog displayed; "delete X" → HITL approval → deletion.
**Estimated Implementation:** 1 iteration (small batch — can combine with Batch 5)

---

### Batch 7: Prerequisites — Assess CLI (E-100)

**Goal:** Add `assess scan`, `assess file`, and `--format json` to OpenMainframe CLI.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-100.1 | Add `assess scan` CLI command (Rust) | M |
| 2 | E-100.2 | Add `assess file` CLI command (Rust) | S |
| 3 | E-100.3 | Add `--format json` to key commands (Rust) | M |

**Entry Criteria:** None (independent track)
**Exit Criteria:** `open-mainframe assess scan <dir> --format json` returns valid JSON; `cargo test` passes.
**Estimated Implementation:** 1 iteration
**Note:** This batch is on the **Rust development track** and can run in parallel with Batches 1-6. It should be scheduled as early as possible to unblock Batch 8.

---

### Batch 8: Assessment Agent (E-400)

**Goal:** Scan codebases, compute metrics, generate compatibility reports.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-400.1 | Assess node implementation | M |
| 2 | E-400.2 | Router — assess intent classification | S |
| 3 | E-400.3 | Assessment report formatting | S |
| 4 | E-400.4 | Directory discovery (project_path + source_files) | M |
| 5 | E-400.5 | Assessment export (JSON/Markdown) | S |

**Entry Criteria:** Batch 7 complete (assess CLI available) + Batch 2 complete (tool layer)
**Exit Criteria:** User says "assess my project" → agent scans → metrics displayed → exportable report.
**Estimated Implementation:** 1 iteration

---

### Batch 9: Chat UI & Generative UI (E-900)

**Goal:** Build the complete frontend workspace with all visual components.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-900.1 | Three-column layout shell + dark theme | M |
| 2 | E-900.2 | File tree panel | M |
| 3 | E-900.3 | Code viewer with COBOL/JCL syntax highlighting | L |
| 4 | E-900.4 | Assessment dashboard component | L |
| 5 | E-900.5 | Job execution timeline component | M |
| 6 | E-900.6 | Generative UI cards (Assessment, Compiler, Approval, Progress, Explanation) | L |

**Entry Criteria:** Batch 1 complete (frontend running). For full integration: Batches 3-8 complete (agent nodes providing data).
**Exit Criteria:** All workspace components render with agent data; generative UI cards appear in chat; file tree navigation works.
**Estimated Implementation:** 2 iterations (largest batch)
**Strategy:** Split into 9a (E-900.1, E-900.2, E-900.3 — layout + viewers) and 9b (E-900.4, E-900.5, E-900.6 — data-driven components).

---

### Batch 10: Human-in-the-Loop (E-1000)

**Goal:** Approval workflows rendered in CopilotKit chat.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-1000.1 | Execution approval flow (interrupt + resolve) | M |
| 2 | E-1000.2 | Dataset modification approval | S |
| 3 | E-1000.3 | Frontend interrupt rendering (useLangGraphInterrupt) | M |

**Entry Criteria:** Batch 4 complete (execute node with interrupt) + Batch 9 partial (generative UI cards).
**Exit Criteria:** Approval cards render in chat; approve/reject flows work end-to-end; cards gray out after decision.
**Estimated Implementation:** 1 iteration

---

### Batch 11: State Management & Persistence (E-1100)

**Goal:** Bi-directional state sync and production persistence.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-1100.1 | Agent state sync (useCoAgent ↔ AgentState) | M |
| 2 | E-1100.2 | Predictive state updates (progress indicators) | S |
| 3 | E-1100.3 | PostgreSQL persistence (production checkpointer) | M |

**Entry Criteria:** Batch 1 complete (agent + frontend running).
**Exit Criteria:** State changes propagate bidirectionally; progress bars update during operations; conversations persist across page reloads (with PostgreSQL).
**Estimated Implementation:** 1 iteration

---

### Batch 12: Integration Testing (E-1200)

**Goal:** Validate complete system against CardDemo application.

| Order | Story | Description | Size |
|-------|-------|-------------|------|
| 1 | E-1200.1 | CardDemo assessment test | M |
| 2 | E-1200.2 | CardDemo compilation test | M |
| 3 | E-1200.3 | CardDemo execution test | M |
| 4 | E-1200.4 | Code explanation accuracy test | M |

**Entry Criteria:** ALL previous batches complete.
**Exit Criteria:** All CardDemo integration tests pass; 90%+ compilation success; explanation accuracy validated.
**Estimated Implementation:** 1 iteration

---

## 3. Batch Dependency Graph

```
Track B (Rust CLI):
  Batch 7 (E-100 Prerequisites)
       │
       └──→ Batch 8 (E-400 Assessment Agent)
                │
                └──→ Batch 12 (E-1200 Integration)

Track A (Agent App):
  Batch 1 (E-200 Infrastructure)
       │
       ├──→ Batch 2 (E-300 Tool Layer)
       │       │
       │       ├──→ Batch 3 (E-500 Compile)     ──┐
       │       ├──→ Batch 4 (E-600 Execute)      ──┤
       │       ├──→ Batch 5 (E-700 Explain)      ──┤
       │       └──→ Batch 6 (E-800 Dataset)      ──┤
       │                                           │
       ├──→ Batch 9 (E-900 UI) ←─────────────────┤
       │       │                                   │
       ├──→ Batch 10 (E-1000 HITL)  ←────────────┤
       │                                           │
       └──→ Batch 11 (E-1100 State) ──────────────┤
                                                   │
                                    Batch 12 ←─────┘
```

**Critical Path:** Batch 1 → Batch 2 → Batch 3/4/5/6 → Batch 9 → Batch 12

**Parallel Opportunities:**
- Batches 3, 4, 5, 6 can all run in parallel (independent agent capabilities)
- Batch 7 can run in parallel with Batches 1-6 (different codebase — Rust vs Python/TS)
- Batch 11 can start after Batch 1 (only needs infrastructure)
- Batch 9a (layout) can start after Batch 1; Batch 9b needs Batches 3-8 for data

---

## 4. Ralph Loop Implementation Order

For the Ralph Loop, batches must be serialized. The optimal order minimizes wait time:

| Iteration | Batch | Stories | Running Total |
|-----------|-------|---------|---------------|
| 1 | Batch 1: Infrastructure | 5 | 5/49 |
| 2 | Batch 2: Tool Layer | 4 | 9/49 |
| 3 | Batch 7: CLI Prerequisites (Rust) | 3 | 12/49 |
| 4 | Batch 3: Compilation Agent | 4 | 16/49 |
| 5 | Batch 4: Execution Agent | 5 | 21/49 |
| 6 | Batch 5: Explanation Agent | 4 | 25/49 |
| 7 | Batch 6: Dataset Agent | 3 | 28/49 |
| 8 | Batch 8: Assessment Agent | 5 | 33/49 |
| 9 | Batch 9a: UI Layout + Viewers | 3 | 36/49 |
| 10 | Batch 9b: UI Data Components | 3 | 39/49 |
| 11 | Batch 10: HITL | 3 | 42/49 |
| 12 | Batch 11: State & Persistence | 3 | 45/49 |
| 13 | Batch 12: Integration Testing | 4 | 49/49 |

**Total: 13 implementation iterations, 49 stories**

### Why This Order?

1. **Infrastructure first** (Batch 1) — everything depends on this
2. **Tool layer second** (Batch 2) — all agents need tools
3. **CLI prerequisites third** (Batch 7) — unblocks assessment agent; slot it here while agent patterns are fresh from Batch 2
4. **Capability agents** (Batches 3-7) — build each agent node incrementally, adding router classification as we go
5. **Assessment agent** (Batch 8) — now unblocked by Batch 7 (CLI prerequisites)
6. **UI components** (Batch 9a/9b) — build visuals after all data sources exist
7. **HITL polish** (Batch 10) — refine approval flows with real UI cards
8. **State management** (Batch 11) — optimize sync and add persistence
9. **Integration testing** (Batch 12) — validate everything against CardDemo

---

## 5. Story-Level Implementation Guide

### 5.1 Per-Story Implementation Notes

#### Batch 1 Key Decisions

- **E-200.1:** Use `npx create-next-app` with App Router. Add CopilotKit deps manually rather than using CopilotKit CLI template (more control).
- **E-200.3:** Use `uv` for Python package management. Define all deps in `pyproject.toml` with `[project.optional-dependencies]` for dev tools.
- **E-200.4:** Start with only `router` → `chat` edge. Other nodes added in subsequent batches.

#### Batch 2 Key Decisions

- **E-300.1:** `WORKSPACE_ROOT` should default to the repo root. `sanitize_path` must be called in every tool before subprocess invocation.
- **E-300.2:** If OpenMainframe binary doesn't have `assess` command yet, return a mock JSON response so downstream development isn't blocked.

#### Batch 7 Key Decisions (Rust)

- **E-100.1:** Add `assess` subcommand to `crates/open-mainframe/src/main.rs` using `clap`. Wire to `open-mainframe-assess` crate's `Scanner` and `Analyzer`.
- **E-100.3:** For JSON output, implement a `--format` flag on `compile`, `check`, `run` commands. Use `serde_json` to serialize results. This is a cross-cutting change across multiple crates.

#### Batch 9 Key Decisions

- **E-900.3:** Use `react-syntax-highlighter` with a custom COBOL grammar or `prismjs` with COBOL TextMate scope. If no COBOL grammar exists, define one based on UX §6.2 token table.
- **E-900.4:** Use `recharts` or `visx` for the complexity distribution bar chart. Keep dependencies minimal.

---

## 6. Definition of Done

A batch is **DONE** when:

1. All stories in the batch have their acceptance criteria met
2. Code compiles/runs without errors
3. Basic manual testing confirms the happy path works
4. Code is committed with batch-level commit message
5. No regressions in previously completed batches

---

## 7. Risk Mitigations per Batch

| Batch | Primary Risk | Mitigation |
|-------|-------------|------------|
| 1 | CopilotKit version incompatibility | Pin exact versions; test scaffold before adding features |
| 2 | OpenMainframe binary not built | Provide mock responses; detect binary absence at startup |
| 3 | Compiler output format varies | Create test fixtures from real compiler output |
| 4 | HITL interrupt doesn't render | Test interrupt() flow in isolation before integrating UI |
| 5 | LLM hallucination in explanations | Ground in AST/lex output; add "confidence" disclaimer |
| 7 | Assess crate API changes | Pin to current crate version; abstract crate API |
| 9 | COBOL syntax highlighting gaps | Start with basic keyword matching; iterate |
| 12 | CardDemo compilation failures | Document known failures; set realistic pass rate |

---

## 8. Summary

| Metric | Value |
|--------|-------|
| Total Epics | 12 |
| Total Stories | 49 |
| Implementation Batches | 13 (12 original + split Batch 9) |
| Estimated Ralph Loop Iterations | 13 |
| Critical Path Length | 7 batches (1→2→7→8→9a→9b→12) |
| Maximum Parallelism | 4 batches concurrent (3,4,5,6) |

Phase 3 (Solutioning) is **COMPLETE**. Ready for Phase 4 (Implementation).
