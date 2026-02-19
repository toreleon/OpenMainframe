---
track: 1
status: draft
date: 2026-02-19
iteration: 1
---

# Research Track 1: OpenMainframe CLI — UX Analysis

**Purpose:** Deep analysis of every CLI command's output structure, user workflows, cross-command relationships, and UX opportunities. This document is the foundation for all UX design decisions — every UI component must trace back to a specific CLI output field.

---

## 1. Command Output Schema Reference

### 1.1 Commands with Structured JSON Output

These commands support `--format json` and produce machine-parseable output the agent can consume:

#### `assess scan <directory>`

**Purpose:** Scan a directory of COBOL files and produce a modernization readiness report.

```json
{
  "title": "string",
  "results": [AnalysisResult],
  "overall_complexity": "Low" | "Medium" | "High" | "VeryHigh",
  "total_lines": 28450,
  "total_issues": 12,
  "critical_issues": 2,
  "estimated_effort_hours": 340.0
}
```

**UX-critical fields:**
- `results[]` — Array of per-file analysis. The main drilldown axis.
- `overall_complexity` — Single badge for executive summary.
- `total_issues` / `critical_issues` — Primary concern indicators.
- `estimated_effort_hours` — Effort estimation for planning.

**Visualization opportunity:** This is the richest output in the entire CLI. Supports:
- Summary cards (total files, LOC, complexity, issues, effort)
- Complexity distribution chart (count per Low/Med/High/VeryHigh)
- Feature support matrix (bar chart per feature category)
- Issue list sorted by severity
- Program-by-program table with sortable columns

---

#### `assess file <file>`

**Purpose:** Deep assessment of a single COBOL program.

```json
{
  "file_name": "POLCY01.cbl",
  "program_id": "POLCY01",
  "metrics": {
    "total_lines": 2450,
    "code_lines": 2100,
    "blank_lines": 200,
    "comment_lines": 150,
    "executable_statements": 890,
    "cyclomatic_complexity": 12,
    "paragraph_count": 24,
    "data_items": 67,
    "has_identification": true,
    "has_environment": true,
    "has_data": true,
    "has_procedure": true
  },
  "features": [
    {
      "name": "VSAM",
      "category": "FileHandling",
      "count": 3,
      "lines": [45, 67, 89]
    }
  ],
  "issues": [
    {
      "code": "IMS-001",
      "description": "IMS/DL1 call detected — not supported",
      "severity": "Critical",
      "category": "Database",
      "line": 245,
      "recommendation": "Refactor to use DB2 or VSAM equivalent"
    }
  ],
  "complexity": "High",
  "recommendations": ["string"]
}
```

**UX-critical fields:**
- `metrics` — Rich set of code metrics. Good for metric cards or radar chart.
- `features[]` — What mainframe features this program uses. Good for tag list or feature matrix.
- `features[].lines` — Enables "click to jump to line" in code viewer.
- `issues[]` — Actionable findings. Good for issue cards with severity badges.
- `issues[].line` — Enables inline code annotation.
- `issues[].recommendation` — Agent can present these as action items.
- `complexity` — Single badge.

**Visualization opportunity:**
- Metrics dashboard for single file (LOC breakdown pie, complexity gauge)
- Feature tag cloud with click-to-see-lines
- Issue cards with severity coloring and "Go to line" links
- Code viewer with inline issue annotations at specific lines
- Recommendations as checklist/action items

---

#### `compile <file>` / `check <file>`

**Purpose:** Compile or syntax-check a COBOL program. Same output structure.

```json
{
  "status": "success" | "error",
  "program_id": "HELLO",
  "diagnostics": [
    {
      "severity": "error" | "warning" | "info",
      "message": "Expected numeric operand, found alphanumeric field WS-NAME",
      "file": "/path/to/HELLO.cbl",
      "line": 52,
      "col": 10
    }
  ],
  "summary": {
    "errors": 2,
    "warnings": 1,
    "infos": 0
  },
  "symbols": 45
}
```

**UX-critical fields:**
- `status` — Binary success/failure. Determines card color (green/red).
- `diagnostics[]` — The main content. Each has severity + message + location.
- `diagnostics[].line` / `col` — Enables "Go to line" in code viewer.
- `summary` — Quick count for header badge.

**Visualization opportunity:**
- Success: Green card with program ID and symbol count
- Failure: Red card with error count, expandable diagnostic list
- Each diagnostic: severity icon + message + file:line link
- Agent adds: LLM-generated explanation + fix suggestion per diagnostic
- Code viewer integration: highlight error lines, show inline annotations

---

#### `run <jcl-file>`

**Purpose:** Execute a JCL job. Returns per-step results.

```json
{
  "status": "success" | "error",
  "job_name": "CARDDEMO",
  "return_code": 4,
  "steps": [
    {
      "name": "STEP010",
      "return_code": 0,
      "success": true,
      "stdout": ["Processing batch file...", "Records: 1000"],
      "stderr": []
    },
    {
      "name": "STEP020",
      "return_code": 4,
      "success": true,
      "stdout": ["WARNING: No data for region NW"],
      "stderr": []
    }
  ]
}
```

**UX-critical fields:**
- `job_name` — Title for the result view.
- `return_code` — Maximum RC. Color: 0=green, 4=yellow, 8+=red.
- `steps[]` — Each step is a node in a timeline.
- `steps[].return_code` — Per-node status coloring.
- `steps[].stdout` — Expandable per-step output.
- `steps[].stderr` — Error details per step.

**Visualization opportunity:**
- **Job timeline**: Horizontal or vertical flow of step nodes, each color-coded by RC
- Click step → expand SYSOUT/SYSERR panel below
- Summary bar: "3 steps | Max RC=4 | Total: 7.6s"
- Re-run button (triggers HITL approval)
- Mainframe engineers are very familiar with step/RC presentation — this should feel like SDSF

---

#### `parse-jcl <file>`

**Purpose:** Parse JCL structure without executing. Used for preview before `run`.

```json
{
  "status": "success",
  "job_name": "CARDDEMO",
  "accounting": "ACCT123",
  "programmer": "DEVELOPER",
  "class": "A",
  "msgclass": "H",
  "steps": [
    {
      "name": "STEP010",
      "exec": "PGM=COBOL001",
      "parm": "PARM1,PARM2",
      "dd_statements": [
        { "name": "SYSIN", "definition": "DSN=MY.INPUT DISP=SHR" },
        { "name": "SYSOUT", "definition": "SYSOUT=*" }
      ]
    }
  ]
}
```

**UX-critical fields:**
- `steps[]` — Job structure preview. Each step shows program + DD list.
- `steps[].exec` — Which program runs at each step.
- `steps[].dd_statements` — Data definitions — critical for understanding I/O.

**Visualization opportunity:**
- **Job preview card**: Used in HITL approval flow
- Step list with program name and DD summary
- DD statements as a sub-table per step
- This becomes the "what you're about to run" view before approval
- Combined with LLM explanation: "This job compiles COBOL001, then sorts output, then generates a report"

---

#### `interpret <file>`

**Purpose:** Run a COBOL program directly via tree-walking interpreter (no compile step).

```json
{
  "status": "success" | "error",
  "output": ["Hello, World!", "Processing complete"],
  "return_code": 0,
  "diagnostics": []
}
```

**UX-critical fields:**
- `output[]` — Program output lines (DISPLAY statements).
- `return_code` — Exit status.
- `diagnostics[]` — Runtime errors.

**Visualization opportunity:**
- Terminal-style output view (monospace, dark background)
- Success/failure badge
- If errors: diagnostic cards with line references
- Simpler than `run` — single program, no step timeline

---

### 1.2 Commands with Text-Only Output

These commands do NOT support `--format json`. The agent must parse text or the output is informational.

#### `lex <file>`

**Output:** Token list with types (text format).
```
Source format: Fixed
Tokens (512 total):
   1: IDENTIFICATION
   2: DIVISION
   3: PERIOD
   ...
Total: 512 tokens
```

**UX opportunity:** Low priority for visualization. Debugging tool. Could show as collapsible raw text. Potentially useful for syntax highlighting validation.

---

#### `bms <file>`

**Output:** Generated copybook summary (text format).
```
Parsed mapset: MYMAPSET
  Maps: 2
  - MAP1 (8 fields, 24x80)
  - MAP2 (12 fields, 24x80)
Generated copybook: /path/to/MYMAPSET.cpy
```

**UX opportunity:** Medium. BMS compilation is a prerequisite for CICS testing. Show as a build step card with map summary. Could visualize the 24x80 screen layout.

---

#### `cics <file>`

**Output:** Interactive 3270 TUI (not parseable).

**UX challenge:** This is the hardest command to integrate into an agent-native UI. The CICS command launches a full-screen terminal UI (ratatui-based) with:
- 24x80 character screen
- Color attributes, field positioning
- Keyboard interaction (PF keys, TAB, ENTER)
- Multiple screens (pseudo-conversation flow)

**Options for agent-native UX:**
1. **Embed xterm.js** — Run CICS in a terminal emulator widget in the browser. Requires a PTY bridge.
2. **Screenshot capture** — Agent runs CICS, captures screen state, describes it in text.
3. **Screen-by-screen narration** — Agent drives CICS, shows each screen as a rendered 24x80 grid image or ASCII art.
4. **Skip for MVP** — Focus on compile/run/assess. CICS interactive testing is a v2 feature.

**Recommendation for research:** Investigate xterm.js + WebSocket PTY as a potential path. This would let the user see the 3270 screen in the browser while the agent can also observe and interact with it.

---

#### `idcams <command>`

**Output:** Command result text.
```
DEFINE CLUSTER ... - COMPLETE
DELETE MY.FILE - DELETED
LISTCAT MY.* -
  MY.VSAM.FILE   TYPE=KSDS   RECORDS=1000
  MY.SEQ.FILE    TYPE=PS     RECORDS=500
```

**UX opportunity:** Medium. Dataset operations are setup work. Show as operation result cards. LISTCAT output could render as a dataset catalog table.

---

#### `gdg <subcommand>`

**Output:** GDG management text.
```
GDG: MY.GDG.BASE
  Limit: 10
  Generations: 3
  (-2)  MY.GDG.BASE.G0002V00   2026-02-10
  (-1)  MY.GDG.BASE.G0001V00   2026-02-11
  (0)   MY.GDG.BASE.G0000V00   2026-02-19
```

**UX opportunity:** Low-medium. GDG is a specialized concept. Show as a version list (like git log). Each generation is a row with date and relative generation number.

---

#### `db2 preprocess <file>`

**Output:** SQL listing text.
```
DB2 SQL Preprocessor Listing
Program: MYPROGRAM
Found 2 SQL statement(s):

Statement 001 (lines 42-48) - SELECT
  SELECT * FROM EMPLOYEE WHERE ID = ?

Statement 002 (lines 50-52) - INSERT
  INSERT INTO AUDIT (ID, ACTION) VALUES (?, ?)

Host Variables:
  EMPLOYEE_ID (NUMERIC) in statement 1
```

**UX opportunity:** Medium-high. SQL statements are very readable and could be syntax-highlighted. Host variable mapping is valuable for understanding data flow. Could show as a SQL card list with line references back to COBOL source.

---

#### `config show/init/paths`

**Output:** Configuration text.

**UX opportunity:** None for the agent UX. Configuration is system-level, not user-facing in the agent.

---

## 2. User Workflow Mapping

### Workflow 1: Migration Assessment

```
User intent: "How ready is my codebase for migration?"
│
├─ Agent step 1: assess scan ./app/cbl --format json
│  Output: Report { results[], overall_complexity, total_issues, estimated_effort_hours }
│  UI: Assessment summary → progress bar during scan → summary cards
│
├─ Agent step 2: (LLM synthesizes)
│  Output: Natural language summary with key findings
│  UI: Text message with inline stats
│
├─ Agent step 3 (optional): assess file POLCY01.cbl --format json
│  Output: AnalysisResult { metrics, features, issues, complexity }
│  UI: Drill-down view for specific program
│
└─ Artifacts produced:
   - Assessment report (exportable)
   - Program-by-program table (sortable/filterable)
   - Issue list by severity
   - Feature support matrix
   - Effort estimation summary
```

**Cross-command dependency:** `assess scan` → `assess file` (for drill-down)

**Key UX question:** How does the user go from summary → specific program → back to summary?

---

### Workflow 2: Compile-Debug Loop

```
User intent: "Compile HELLO.cbl" or "Fix the errors"
│
├─ Agent step 1: compile HELLO.cbl --format json (or check for quick validation)
│  Output: CompileOutput { status, diagnostics[], summary }
│  UI: Success/failure card with diagnostic list
│
├─ Agent step 2: (LLM explains errors)
│  Output: Per-diagnostic explanation + fix suggestion
│  UI: Expandable cards per error with explanation
│
├─ Agent step 3: (User fixes code, or agent suggests fix)
│  Then: goto step 1 (recompile)
│
└─ Artifacts produced:
   - Compiler diagnostics with explanations
   - Fix suggestions
   - Success confirmation with binary location
```

**Cross-command dependency:** `check` (fast) → `compile` (full) → `interpret` (test)

**Key UX question:** How does the fix-recompile loop feel? Tight iteration = good.

---

### Workflow 3: JCL Job Execution

```
User intent: "Run CARDDEMO.jcl"
│
├─ Agent step 1: parse-jcl CARDDEMO.jcl --format json
│  Output: ParseJclOutput { job_name, steps[], dd_statements }
│  UI: Job preview card with step list
│
├─ Agent step 2: HITL approval
│  Output: Interrupt event with job details
│  UI: Approval card — "Run this job? 3 steps, these programs, these datasets"
│
├─ Agent step 3: run CARDDEMO.jcl --format json
│  Output: RunOutput { job_name, return_code, steps[] }
│  UI: Job timeline with per-step results
│
└─ Artifacts produced:
   - Job timeline (step-by-step with RCs)
   - Per-step SYSOUT (expandable)
   - Overall job summary
```

**Cross-command dependency:** `parse-jcl` → HITL approval → `run`

**Key UX question:** The approval gate is the critical trust-building moment. It must show enough context (what programs run, what datasets are touched) for informed consent.

---

### Workflow 4: Code Understanding

```
User intent: "What does CALC-PREMIUM do?"
│
├─ Agent step 1: bash: cat ./app/cbl/POLCY01.cbl
│  Output: Source code text
│  UI: (Not shown directly — agent reads internally)
│
├─ Agent step 2: (LLM analyzes)
│  Output: Structured explanation
│  UI: Explanation with sections: Purpose, Inputs/Outputs, Business Rules, Data Flow
│
├─ Optional: assess file POLCY01.cbl → add metrics context
│  Optional: lex POLCY01.cbl → token-level analysis
│
└─ Artifacts produced:
   - Code explanation document
   - Business rules extraction
   - Annotated source (if displayed alongside)
```

**Cross-command dependency:** `cat` (read) → LLM analysis → optionally `assess file` for metrics

**Key UX question:** Should the explanation be alongside the code or a standalone artifact?

---

### Workflow 5: CICS Transaction Testing

```
User intent: "Test the CARDDEMO transaction"
│
├─ Agent step 1: bms COSGN00.bms → compile screen maps
│  Output: Copybook generated
│  UI: Build step card
│
├─ Agent step 2: cics COSGN00.cbl --transid CARD=COSGN00
│  Output: Interactive TUI session (NOT JSON)
│  UI: ??? (This is the CICS challenge)
│
└─ Artifacts produced:
   - BMS compilation results
   - Transaction test results (screen flow)
```

**Key UX challenge:** The CICS TUI cannot be directly embedded in a chat-based UI without significant engineering (xterm.js + PTY bridge). For MVP, the agent could:
- Run CICS non-interactively if possible
- Capture and describe screen state
- Defer interactive CICS to v2

---

### Workflow 6: Dataset Setup

```
User intent: "Set up VSAM files for testing"
│
├─ Agent step 1: idcams exec "DEFINE CLUSTER (NAME(MY.VSAM) ...)"
│  Output: Operation result text
│  UI: Operation card (success/failure)
│
├─ Agent step 2: gdg create MY.GDG.BASE --limit 10
│  Output: GDG creation result
│  UI: Operation card
│
├─ Agent step 3: idcams exec "REPRO INFILE(INPUT) OUTFILE(VSAM)"
│  Output: Data load result
│  UI: Operation card
│
└─ Artifacts produced:
   - Dataset catalog (what was created)
   - Operation log
```

**Key UX question:** Should there be a "dataset browser" view, or is the agent sufficient for managing datasets conversationally?

---

## 3. Cross-Command Relationship Map

```
                    ┌──────────────┐
                    │  assess scan │ ─── Identifies codebase overview
                    └──────┬───────┘
                           │ drills into
                    ┌──────▼───────┐
                    │ assess file  │ ─── Detailed per-program analysis
                    └──────┬───────┘
                           │ identifies issues → leads to
              ┌────────────┼────────────┐
              │            │            │
       ┌──────▼───┐  ┌────▼─────┐  ┌──▼──────────┐
       │ compile  │  │ explain  │  │ plan migrate │
       │ / check  │  │ (LLM)   │  │ (future)     │
       └──────┬───┘  └──────────┘  └──────────────┘
              │ success → test
       ┌──────▼───────┐
       │  interpret   │ ─── Quick single-program test
       └──────────────┘

              or

       ┌──────────────┐
       │  parse-jcl   │ ─── Preview job structure
       └──────┬───────┘
              │ approve → execute
       ┌──────▼───────┐
       │     run      │ ─── Full JCL job execution
       └──────────────┘

   Supporting commands:
   ┌──────────┐  ┌──────┐  ┌──────┐
   │  idcams  │  │ gdg  │  │ db2  │ ─── Data infrastructure
   └──────────┘  └──────┘  └──────┘

   ┌──────────┐  ┌──────┐
   │   bms    │  │ cics │ ─── CICS transaction development
   └──────────┘  └──────┘
```

**Workflow chains:**
1. **Assessment chain:** `assess scan` → `assess file` → `explain` / `compile`
2. **Development chain:** `check` → `compile` → `interpret` / `run`
3. **Execution chain:** `parse-jcl` → (approve) → `run`
4. **CICS chain:** `bms` → `cics`
5. **Data chain:** `idcams` / `gdg` → `run` (job uses the datasets)

---

## 4. Pain Points Per Command

| Command | Current Pain Point | UX Opportunity |
|---------|-------------------|----------------|
| `assess scan` | No progress during large scans | Progress bar: "Scanning 18/42 files..." |
| `assess scan` | Raw JSON report, no visualization | Rich dashboard: cards, charts, tables |
| `assess file` | Issues listed without context | Inline code annotations at issue lines |
| `compile` | Diagnostics are COBOL jargon | LLM-generated plain-English explanations per error |
| `compile` | No "fix and retry" loop | Agent suggests fix → user approves → auto-recompile |
| `check` | Same as compile | Same opportunities |
| `run` | Executes immediately, no preview | `parse-jcl` first → approval gate → then `run` |
| `run` | Step results are flat text | Visual timeline with color-coded RC badges |
| `run` | SYSOUT hard to read | Monospace output viewer with search/filter |
| `interpret` | No variable inspection | Future: debug mode with variable watch |
| `parse-jcl` | Useful but disconnected from `run` | Integrated preview→approve→execute flow |
| `lex` | Raw token dump | Low priority; debugging only |
| `bms` | Just generates files | Show as build step; preview screen layout |
| `cics` | Interactive TUI, can't integrate | Major challenge — needs xterm.js or alternative approach |
| `idcams` | Text output, no catalog view | Dataset catalog table/list |
| `gdg` | Specialized, text output | Version list view |
| `db2` | SQL listing, no analysis | SQL card list with syntax highlighting |
| `config` | System-level | Not user-facing in agent UX |

---

## 5. Output Visualization Matrix

For each CLI output type, what UI component best presents it:

| Output Data | Best Visualization | Why |
|-------------|-------------------|-----|
| Assessment summary (total files, LOC, complexity, issues) | **Stat cards row** (4-6 cards) | Quick scan, executive-friendly |
| Complexity distribution (Low/Med/High/VeryHigh counts) | **Horizontal bar chart** or **donut chart** | Proportional breakdown at a glance |
| Feature support matrix (Sequential 100%, VSAM 90%, etc.) | **Progress bars with percentages** | Intuitive "how ready" indicator |
| Issue list by severity | **Sortable table** with severity badges | Actionable, filterable |
| Per-program metrics | **Data table** with sparkline columns | Comparison across programs |
| Compiler diagnostics | **Expandable card list** with severity icons | Each error is its own actionable item |
| Diagnostic with line number | **Code annotation** (inline marker + tooltip) | Direct connection to source |
| Job step results | **Vertical timeline** with colored nodes | Familiar pattern for sequential steps |
| Step SYSOUT | **Monospace text panel** (collapsible) | Terminal-like output reading |
| Return code | **Badge** (green/yellow/red) | Instant status recognition |
| JCL job structure | **Step cards** with DD sub-lists | Preview before execution |
| DD statements | **Key-value list** per step | Data definitions |
| Program output (DISPLAY) | **Terminal-style text block** | COBOL output is line-oriented |
| SQL statements | **Syntax-highlighted code blocks** | SQL is readable when formatted |
| GDG generations | **Version list** (like git log) | Chronological data versions |
| Dataset catalog | **File-browser-style table** | List of datasets with type/size |

---

## 6. The CICS Challenge

### Problem

The `cics` command launches a full-screen 3270 TUI using `ratatui`. This is a terminal-based UI that:
- Occupies the full terminal (24 rows x 80 columns)
- Uses ANSI colors and cursor positioning
- Requires keyboard interaction (PF keys, TAB, ENTER, cursor keys)
- Supports multiple screens (pseudo-conversation navigation)
- Has themes: classic (green-on-black), modern, mono

This fundamentally conflicts with a web-based agent UI.

### Options

| Approach | Effort | Quality | Agent Integration |
|----------|--------|---------|-------------------|
| **xterm.js + PTY bridge** | High | High | Agent can observe terminal state |
| **Screen capture + describe** | Medium | Medium | Agent runs, captures, narrates |
| **3270 web renderer** | High | High | Custom widget renders field layout |
| **Defer to v2** | None | N/A | Not available in MVP |

### Recommendation

For MVP: **Defer interactive CICS to v2.** The agent can still:
- Compile BMS maps (`bms` command)
- Describe what a CICS program does (code explanation)
- Show the BMS field layout as a static preview

For v2: Investigate **xterm.js + WebSocket PTY relay** where:
1. Bridge daemon runs CICS in a PTY
2. Terminal output streams to frontend via WebSocket
3. Frontend renders via xterm.js widget
4. Agent can observe screen state for narration

---

## 7. Key UX Insights for Design Phase

### Insight 1: Assessment is the Killer Feature
The `assess scan` command produces the richest output and the most impactful user moment. A migration architect goes from "I have 400 COBOL programs and no idea where to start" to "I know my complexity distribution, compatibility score, and estimated effort." The assessment UX must be exceptional.

### Insight 2: Compiler Diagnostics Need LLM Enhancement
Raw COBOL compiler errors are jargon-heavy. The agent's value is explaining each error in plain English and suggesting a fix. The UI must pair each diagnostic with its LLM explanation seamlessly.

### Insight 3: The JCL Approval Flow Is the Trust-Builder
`parse-jcl` → approve → `run` is where the user decides to trust the agent with execution. The preview must show enough detail (programs, datasets, expected behavior) for informed consent.

### Insight 4: The Compile Loop Needs Tight Iteration
`check` → `compile` → fix → recompile is a developer's core loop. The faster this feels, the more developers will adopt the tool. Sub-30-second response time is the target.

### Insight 5: Most Commands Are Text-Only
Only 7 of 14 commands have JSON output. For the text-only commands (`lex`, `bms`, `cics`, `idcams`, `gdg`, `db2`, `config`), the agent must parse text or present it as-is. This is a CLI improvement opportunity (add `--format json` to more commands).

### Insight 6: Cross-Command Workflows Are Where Agent Value Lives
Individual commands are useful. But the agent's real value is orchestrating them: assess → identify issues → compile flagged files → explain errors → suggest fixes. The UI should make these workflow chains feel natural and progressive, not like disconnected command-response pairs.

### Insight 7: CICS Is a v2 Problem
Interactive 3270 in a web UI is a significant engineering challenge. The MVP should focus on assessment, compilation, execution, and explanation — which cover the primary user stories.
