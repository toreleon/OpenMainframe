---
track: 3
status: draft
date: 2026-02-19
iteration: 3
---

# Research Track 3: Modernization UX Landscape

**Purpose:** Analyze how existing mainframe modernization tools present their UX — specifically how they handle assessment results, compilation feedback, job execution results, and complexity visualization. Identify what works, what doesn't, and what OpenMainframe should adopt or reject.

---

## 1. Competitor Analysis

### 1.1 AWS Transform for Mainframe

**Source:** [AWS Transform](https://aws.amazon.com/transform/mainframe/), [AWS blog — Reimagine capabilities](https://aws.amazon.com/blogs/aws/aws-transform-for-mainframe-introduces-reimagine-capabilities-and-automated-testing-functionality/), [re:Invent 2025 guide](https://aws.amazon.com/blogs/migration-and-modernization/aws-for-mainframe-modernization-reinvent-2025-guide/)

**Launched:** May 2025 (GA). First agentic AI service for mainframe modernization.

**Interaction model:** Multi-agent pipeline with unified web console + AI chat.

**Assessment presentation:**
- Automated codebase analysis categorizing: JCL, BMS, COBOL programs, copybooks
- AI-driven dependency detection with **visual dependency graphs**
- Key metrics: LOC, component classification, missing artifact detection
- Component inventory organized by type and relationship

**Compilation/transformation feedback:**
- Not focused on compile feedback (this is a transformation tool, not a compiler)
- Code refactoring with side-by-side comparison views
- AI-generated code explanations alongside transformation suggestions

**Job execution results:**
- Automated testing functionality (added December 2025)
- Test validation reports comparing original vs. transformed behavior

**Complexity visualization:**
- Dependency mapping: visual graphs showing program-to-program and program-to-data relationships
- Component classification tables with categorization
- Job plan visualization: customizable workflow pipelines

**What works well:**
- **Customizable job plans**: Users select predefined workflows (full modernization, analysis focus, business logic focus) or create custom combinations. This is excellent for migration architects with different needs.
- **Multi-agent architecture**: Specialized agents for different tasks (assessment, analysis, refactoring, testing). Each agent has clear scope.
- **Visual dependency mapping**: Graphs showing relationships between programs, copybooks, JCL, datasets.
- **Unified web experience**: Cross-team collaboration in one interface.

**What feels outdated or agent-unfriendly:**
- **Traditional web console**: Despite AI agents, the interface is a dashboard with AI features bolted on, not an agent-native experience where the AI drives.
- **Workflow selection is manual**: User picks a predefined workflow; agent doesn't propose one based on the codebase.
- **AWS lock-in**: All data and processing in AWS cloud.

**OpenMainframe lesson:** Adopt dependency visualization and customizable workflow concepts. Reject the traditional dashboard approach — let the agent drive workflow selection.

---

### 1.2 Google Cloud Dual Run + MAT

**Source:** [Google Cloud mainframe modernization](https://cloud.google.com/solutions/mainframe-modernization), [Google MAT docs](https://cloud.google.com/mainframe-assessment-tool/docs/overview), [Google Cloud AI blog](https://cloud.google.com/blog/products/infrastructure-modernization/accelerate-mainframe-modernization-with-google-cloud-ai)

**Interaction model:** Three-product suite: Assessment (MAT) → Transformation → Validation (Dual Run).

**Assessment presentation (MAT — Mainframe Assessment Tool):**
- Scans mainframe artifacts and generates assessment reports
- Produces inventory of programs, copybooks, JCL, screens
- Classifies components by type and complexity
- Generates migration roadmap recommendations

**Compilation/transformation feedback:**
- mLogica LIBER*M handles automated code conversion
- Side-by-side comparison of original COBOL and generated target code
- Transformation quality metrics

**Job execution results (Dual Run):**
- **Parallel execution comparison**: Runs same workload on mainframe AND cloud simultaneously
- **Output diff visualization**: Highlights differences between mainframe and cloud output
- **Transaction-level comparison**: Each transaction shows pass/fail status
- **Confidence score**: Percentage of transactions matching exactly

**What works well:**
- **Dual Run's output comparison** is a unique and powerful UX pattern. Showing "mainframe produced X, cloud produced X, match: 100%" builds trust in the transformation.
- **Assessment generates actionable roadmap**, not just metrics.
- **Gemini AI integration** for code explanation and test generation.

**What feels outdated or agent-unfriendly:**
- **Three separate products**: User navigates between MAT, transformation tools, and Dual Run. No unified agent experience.
- **GCP lock-in**: All processing on Google Cloud.
- **No conversational interface**: Traditional web console with reports.

**OpenMainframe lesson:** The **output comparison** pattern (original vs. modernized) is powerful and should be considered for validation workflows. Assessment roadmap generation is table stakes.

---

### 1.3 IBM Watsonx Code Assistant for Z

**Source:** [IBM WCA for Z](https://www.ibm.com/products/watsonx-code-assistant-z), [CROZ real-world review](https://croz.net/honest-take-on-watsonx-code-assistant-for-z/), [IBM announcement v2.6](https://www.ibm.com/new/announcements/ibm-watsonx-code-assistant-for-z-adds-ai-code-generation-and-assembler-support)

**Interaction model:** IDE-centric (Eclipse-based) with AI assistant panel. New web-based chat interface in preview (v2.6+).

**Assessment presentation:**
- **Application Discovery and Delivery Intelligence (ADDI)**: Graph-based program analysis
- **Graph view**: Shows execution flow of business logic with dynamic analysis from Test Accelerator recordings
- Call graph visualization for program dependencies
- Business rule extraction from procedure division

**Compilation/transformation feedback:**
- COBOL code generation: AI generates new COBOL or updates existing code
- Refactoring Assistant with enriched graph view showing execution flow
- Side-by-side: original COBOL → suggested refactored COBOL
- Supports: COBOL, JCL, PL/I, REXX, and now Assembler explanation

**Code explanation:**
- Natural language explanation of COBOL, JCL, PL/I, REXX
- Multi-language support (English, Japanese, Portuguese, German, French, Spanish)
- Business rule discovery through AI analysis
- **Chat experience** (new in v2.6): Web-based chat for analyzing programs, discovering business rules, exploring code structure

**What works well:**
- **Graph view for program analysis**: Showing execution flow with dynamic test recordings is powerful for understanding complex programs.
- **Code explanation quality**: Multi-language support addresses global mainframe teams.
- **Chat for code exploration** (v2.6): Moving toward conversational analysis is the right direction.
- **Deep IBM Z integration**: Leverages ADDI, Test Accelerator, z/OS Connect for comprehensive analysis.

**What feels outdated or agent-unfriendly:**
- **Eclipse-based IDE**: Most modern developers have moved to VS Code or web-based editors.
- **IBM ecosystem lock-in**: Requires ADDI, Test Accelerator, z/OS — expensive stack.
- **AI is assistant, not agent**: User drives; AI suggests. Not autonomous.
- **Web chat is "preview"**: Not the primary interaction mode.

**OpenMainframe lesson:** The **graph view for execution flow** is excellent for code understanding. Business rule extraction is a high-value feature. Moving toward chat-based code exploration (v2.6) validates the conversational approach. But the IDE-centric model is a trap — don't build another IDE.

---

### 1.4 Micro Focus / OpenText Enterprise Analyzer

**Source:** [OpenText Enterprise Analyzer](https://10252761.microfocus.com/products/enterprise-suite/enterprise-analyzer/), [Enterprise Suite 7.0](https://community.opentext.com/cobol/b/am-blog/posts/enterprise-suite-7-0-now-available)

**Interaction model:** Desktop application with repository-based analysis. CIO dashboards + developer-level granular tools.

**Assessment presentation:**
- **CIO dashboards**: Portfolio-level metrics, complexity distribution, risk heat maps
- **Application inventory**: Programs, copybooks, JCL, screens organized by system
- **Advanced reports**: Custom report generation for scope definition and risk reduction
- **Visual representations of application relationships**: Dependency graphs, call trees

**Compilation/transformation feedback:**
- Not a compiler — focuses on analysis and documentation
- Code quality metrics per program
- Business rule mining from procedure division

**Complexity visualization:**
- Repository-based analysis at scale (multi-million LOC portfolios)
- Impact analysis: "If I change this copybook, what programs are affected?"
- Dead code identification
- Code duplication detection

**What works well:**
- **CIO dashboards**: Executive-friendly portfolio overview is essential for getting migration funding.
- **Scale**: Handles multi-million LOC portfolios.
- **Impact analysis**: Understanding change ripple effects.
- **Business Rule Manager**: Separate tool for extracting and managing business rules.

**What feels outdated or agent-unfriendly:**
- **Desktop application**: Not web-based, not agent-compatible.
- **Repository-based**: Requires importing all code into a proprietary repository.
- **No AI assistance**: Pure static analysis, no LLM-powered explanation.
- **Expensive and proprietary**: Part of the OpenText enterprise suite.
- **Report-centric**: Produces reports, not interactive experiences.

**OpenMainframe lesson:** CIO dashboards and portfolio-level metrics are important for the "Priya" persona (migration architect). Impact analysis is a high-value feature for v2. But the desktop/repository model is completely wrong for an agent-native application.

---

### 1.5 Raincode

**Source:** [Raincode](https://www.raincode.com/), [Azure Architecture Center — Raincode](https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/app-modernization/raincode-reference-architecture), [Raincode COBOL](https://www.raincode.com/cobol/)

**Interaction model:** Compiler + Visual Studio plugin. Standard IDE development experience.

**Assessment presentation:**
- No dedicated assessment tool. Focus is on compilation/rehosting.

**Compilation/transformation feedback:**
- COBOL compiled to .NET/JVM
- Visual Studio plugin with: debugger, syntax highlighting, unit testing, project management
- Side-by-side: COBOL source → .NET execution
- Thread-safe managed code output

**What works well:**
- **Visual Studio integration**: Leverages an existing, powerful IDE.
- **Zero rewrite migration**: COBOL runs as-is on .NET — reduces transformation risk.
- **Debugging**: Full Visual Studio debugger for COBOL programs.

**What feels outdated or agent-unfriendly:**
- **IDE-only**: No web interface, no conversational AI.
- **No assessment capability**: Cannot evaluate codebase readiness.
- **Microsoft lock-in**: .NET/Azure ecosystem.
- **No AI features**: Pure compilation, no intelligent guidance.

**OpenMainframe lesson:** The debugging experience (Visual Studio debugger for COBOL) sets a high bar. OpenMainframe should eventually match this for the `interpret` command. But the approach (compile to .NET, run in VS) is fundamentally different from OpenMainframe's agent-native vision.

---

### 1.6 TSRI (JANUS Studio)

**Source:** [TSRI](https://tsri.com/), [TSRI solution page](https://tsri.com/solution), [COBOL modernization](https://tsri.com/languages/code-modernization-focus-on-cobol)

**Interaction model:** JANUS Studio AI Platform. Automated assessment → transformation → validation pipeline.

**Assessment presentation:**
- **Transformation Blueprint**: Visual review of source → transformed code, side-by-side
- Structure Charts: Program structure visualization
- Control Flow Graphs: Execution path visualization
- Data Flow Diagrams: Data movement through programs
- State Machine Models: State transition visualization
- **McCabe's Complexity Index**: Per-program complexity scoring
- Similar Code detection: Code duplication analysis

**Compilation/transformation feedback:**
- Automated conversion: COBOL → Java, C#, Python
- Side-by-side comparison of original and generated code
- Quality metrics for generated code

**What works well:**
- **Visualization suite**: Structure charts, control flow graphs, data flow diagrams — comprehensive set of visual analysis tools.
- **McCabe's Complexity**: Standard industry metric, directly comparable across codebases.
- **Similar Code detection**: Reduces transformation scope by identifying duplicate logic.

**What feels outdated or agent-unfriendly:**
- **Desktop tool**: JANUS Studio is a desktop application.
- **Batch processing**: Run analysis, wait, get report. No interactive exploration.
- **No AI explanation**: Pure static analysis and automated conversion.
- **Expert tool**: Requires modernization expertise to interpret results.

**OpenMainframe lesson:** The visualization suite (structure charts, control flow, data flow) is aspirational for OpenMainframe v2+. McCabe's complexity is already implemented in the assess crate. The batch analysis → report model is what we're trying to replace with agent-native interaction.

---

## 2. How Competitors Present Each Output Type

### 2.1 Assessment Results

| Vendor | Presentation | Agent-Friendliness |
|--------|-------------|-------------------|
| AWS Transform | Visual dependency graphs + component classification tables + metrics dashboard | Medium — good visualization, but traditional dashboard |
| Google MAT | Assessment reports with inventory + roadmap recommendations | Low — static reports |
| IBM WCA4Z | Graph view with execution flow + ADDI deep analysis | Medium — graph view is excellent, but requires desktop tool |
| Micro Focus EA | CIO dashboards + advanced reports + relationship visualization | Low — desktop reports |
| Raincode | N/A (no assessment) | N/A |
| TSRI | Structure charts + control flow + data flow + complexity | Low — desktop batch analysis |

**OpenMainframe should:** Present assessment as an **interactive, agent-generated artifact** — not a static report. Summary cards → drill-down to program details → drill further to specific issues. The agent narrates findings while the artifact panel shows the visual dashboard.

### 2.2 Compilation Feedback

| Vendor | Presentation | Agent-Friendliness |
|--------|-------------|-------------------|
| AWS Transform | N/A (transformation, not compilation) | N/A |
| Google | mLogica side-by-side comparison | Low |
| IBM WCA4Z | IDE diagnostic list + AI code generation + refactoring suggestions | Medium — good IDE integration |
| Micro Focus | N/A (analysis, not compilation) | N/A |
| Raincode | VS plugin with debugger + syntax highlighting + error list | Medium — good IDE experience |
| TSRI | N/A (automated conversion) | N/A |

**OpenMainframe should:** Present compiler diagnostics as **expandable cards with LLM-generated explanations** — severity icon, COBOL error message, plain-English explanation, fix suggestion, "Go to line" link. This is better than any competitor because they don't combine compilation with AI explanation.

### 2.3 Job Execution Results

| Vendor | Presentation | Agent-Friendliness |
|--------|-------------|-------------------|
| AWS Transform | Test validation reports | Low — batch reports |
| Google Dual Run | Parallel execution comparison with output diff | Medium — unique pattern |
| IBM WCA4Z | N/A (analysis/refactoring tool) | N/A |
| Micro Focus | N/A | N/A |
| Raincode | Visual Studio debugger for .NET-compiled COBOL | Medium |
| TSRI | N/A | N/A |

**OpenMainframe should:** Present JCL execution as a **visual step timeline** — horizontal flow of step nodes, color-coded by return code, expandable SYSOUT per step. This is a gap in the market — no competitor does this well in an agent-native context.

### 2.4 Complexity Visualization

| Vendor | Technique | Quality |
|--------|-----------|---------|
| AWS Transform | Component classification + dependency graphs | Good |
| Google MAT | Inventory + roadmap metrics | Basic |
| IBM WCA4Z | ADDI graph view + execution flow | Excellent (but desktop) |
| Micro Focus EA | CIO dashboards + risk heat maps + complexity distribution | Excellent (but desktop) |
| TSRI | McCabe's + structure charts + control flow + data flow | Excellent (but desktop) |

**OpenMainframe should:** Use McCabe's cyclomatic complexity (already in the assess crate) with:
- **Complexity distribution chart**: Bar/donut showing Low/Med/High/VeryHigh counts
- **Feature support matrix**: Progress bars per feature (Sequential 100%, VSAM 90%, etc.)
- **Issue severity table**: Sortable by severity, filterable by category
- Future: dependency graphs and call flow visualization (requires Epic 1002)

---

## 3. Key Takeaways for OpenMainframe

### 3.1 What Every Competitor Gets Right
1. **Assessment is the entry point** — Every tool starts with scanning and analyzing the codebase
2. **Dependency visualization matters** — Graphs showing relationships between programs are essential
3. **Executive dashboards** — CIO/architect personas need high-level metrics at a glance
4. **Side-by-side comparisons** — Original vs. transformed/modernized code

### 3.2 What Every Competitor Gets Wrong (for Agent-Native)
1. **User drives, tool assists** — All competitors are tool-centric, not agent-centric. The user picks the workflow; the tool executes. OpenMainframe should invert this.
2. **Desktop applications** — Most tools are desktop (Micro Focus EA, TSRI JANUS, Eclipse for IBM). Web-native is the future.
3. **No conversational exploration** — Only IBM (v2.6 preview) has a chat interface for code exploration. Everyone else relies on clicking through dashboards.
4. **Batch analysis → static reports** — Analyze everything, produce a report, done. No progressive, iterative exploration guided by an agent.
5. **No LLM-powered explanation** — Only IBM and AWS use AI for explanation. The rest rely on static analysis metrics.

### 3.3 OpenMainframe's Differentiation Opportunity

| Area | Competitors | OpenMainframe (Agent-Native) |
|------|------------|------------------------------|
| Assessment | Dashboard with static reports | Agent scans, narrates findings, generates interactive artifact |
| Compilation | IDE error list | Agent explains errors, suggests fixes, offers auto-recompile |
| Execution | Batch reports or dual-run comparison | Agent previews job, asks approval, shows live timeline |
| Explanation | Multi-language text explanation (IBM only) | Agent narrates while highlighting code, extracts business rules conversationally |
| Workflow | User selects from menu | Agent proposes workflow based on codebase analysis |
| Interaction | Click through dashboards/reports | Conversational: "What are the riskiest programs?" "Show me the DB2 dependencies" |
| Deployment | Desktop application or cloud console | Web-native, self-hosted, local bridge for source privacy |

### 3.4 Missing from All Competitors

These features don't exist in any competitor and represent blue-ocean opportunities:

1. **Conversational assessment exploration** — "Which programs use IMS?" "What's the most complex batch job?" "Show me programs that share copybook CUSTCPY"
2. **Agent-driven compile-fix loop** — Agent compiles, explains errors, suggests fixes, offers to fix and recompile in one turn
3. **JCL execution preview with approval** — No tool shows a visual job preview before execution with HITL approval
4. **Source-local with agent-remote** — Code stays on user's machine; agent runs in cloud. No competitor offers this.
5. **Open-source assessment + AI** — Assessment metrics with LLM-powered explanation is unique

---

## 4. Sources

- [AWS Transform for Mainframe](https://aws.amazon.com/transform/mainframe/)
- [AWS Transform — Reimagine capabilities](https://aws.amazon.com/blogs/aws/aws-transform-for-mainframe-introduces-reimagine-capabilities-and-automated-testing-functionality/)
- [AWS re:Invent 2025 Mainframe Guide](https://aws.amazon.com/blogs/migration-and-modernization/aws-for-mainframe-modernization-reinvent-2025-guide/)
- [Google Cloud Mainframe Modernization](https://cloud.google.com/solutions/mainframe-modernization)
- [Google MAT Overview](https://cloud.google.com/mainframe-assessment-tool/docs/overview)
- [IBM Watsonx Code Assistant for Z](https://www.ibm.com/products/watsonx-code-assistant-z)
- [IBM WCA4Z v2.6 Announcement](https://www.ibm.com/new/announcements/ibm-watsonx-code-assistant-for-z-adds-ai-code-generation-and-assembler-support)
- [CROZ — WCA4Z Real-World Review](https://croz.net/honest-take-on-watsonx-code-assistant-for-z/)
- [OpenText Enterprise Analyzer](https://10252761.microfocus.com/products/enterprise-suite/enterprise-analyzer/)
- [Enterprise Suite 7.0](https://community.opentext.com/cobol/b/am-blog/posts/enterprise-suite-7-0-now-available)
- [Raincode COBOL](https://www.raincode.com/cobol/)
- [Azure Architecture — Raincode](https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/app-modernization/raincode-reference-architecture)
- [TSRI Solution](https://tsri.com/solution)
- [TSRI — COBOL Modernization](https://tsri.com/languages/code-modernization-focus-on-cobol)
- [Code District — Best COBOL Modernization Platforms 2025](https://codedistrict.com/blog/cobol-modernization-platforms)
