# open-mainframe-assess

Codebase assessment and migration readiness analysis engine for legacy mainframe applications in the OpenMainframe project.

## Purpose

Migrating legacy mainframe applications to modern environments requires in-depth static analysis of COBOL source code, JCL job streams, database interactions, and transaction processing patterns. The `open-mainframe-assess` crate automates migration assessment by performing dual-path analysis (fast text scanning and deep AST syntax tree inspection via `open-mainframe-cobol`), calculating industry-standard code metrics, checking dialect compatibility rules, building program call graphs, identifying dead code, inventorying CICS/DB2/IMS statements, mapping JCL dataset dependencies, generating multi-format migration reports, and diffing snapshot progression.

## Capabilities

- **Dual-Path Source Analysis**:
  - **Text-Based Analyzer (`Analyzer`)**: Fast, error-tolerant regex/keyword scanning for mainframe features across any source file.
  - **AST-Based Analyzer (`AstAnalyzer`)**: Deep semantic analysis powered by `open-mainframe-cobol` parser, extracting accurate AST constructs, nested control flows, cyclomatic decision counts, and data definitions with automatic fallback to text analysis.
- **Code Quality & Complexity Metrics (`metrics`)**:
  - Computes Lines of Code (total, SLOC, blank, comment lines, comment ratio).
  - Calculates McCabe Cyclomatic Complexity (`IF`, `EVALUATE WHEN`, `PERFORM UNTIL`, `ON SIZE ERROR`, `AT END`).
  - Halstead software metrics (vocabulary, length, volume, difficulty, effort).
  - Computes Maintainability Index (MI: normalized 0–100 scale) and technical debt estimates.
- **Dialect Compatibility Checking (`compatibility`)**:
  - Rule-based evaluation against IBM Enterprise COBOL, Micro Focus, GnuCOBOL, COBOL-85, and OpenMainframe dialects.
  - Reports severity classifications (`Info`, `Warning`, `High`, `Critical`) with targeted remediation recommendations.
- **Subsystem & Middleware Inventory**:
  - **CICS Inventory (`cics_inventory`)**: Scans for `EXEC CICS` commands, categorizing 12 functional areas and mapping support status against `open-mainframe-cics`.
  - **DB2 SQL Analysis (`sql_analysis`)**: Extracts `EXEC SQL` blocks, classifies query complexity (`Simple`, `Join`, `Subquery`, `Cursor`, `Dynamic`, `Utility`), computes effort scores, and provides PostgreSQL compatibility guidance.
  - **IMS / DL/I Detection**: Identifies `EXEC DLI` statements and hierarchical database dependencies.
- **Structural Codebase Analysis**:
  - **Call Graph Generator (`callgraph`)**: Builds directed program call graphs from static `CALL`, dynamic calls, and `EXEC CICS LINK/XCTL`, with DFS cycle detection and Kahn's topological sorting for leaf-first migration ordering.
  - **Dead Code Detection (`dead_code`)**: Identifies unreachable sections, unreferenced paragraphs, and unused variables.
  - **JCL Job Dependency Mapping (`jcl_deps`)**: Analyzes JCL job streams (`analyze_jcl`), constructing step-to-program maps, dataset I/O dependency graphs, and shared dataset conflict reports.
- **Batch Scanning & Reporting (`scanner`, `report`, `snapshot`)**:
  - Directory scanner (`Scanner`) with glob filtering, include path traversal, and copybook resolution (`COPY`).
  - Report generators producing Text, Markdown, JSON, and HTML executive summaries.
  - Assessment snapshot diffing (`compare_snapshots`, `SnapshotDiff`) tracking migration velocity and complexity reduction over time.

## Architecture

```
                       +----------------+
                       |    Scanner     |
                       | (batch scan)   |
                       +-------+--------+
                               |
               +---------------+---------------+
               |                               |
       +-------+-------+              +--------+-------+
       |  AstAnalyzer  |              | CopybookResolver|
       | (AST-first)   |              |  (COPY inline)  |
       +-------+-------+              +----------------+
               |
     +---------+---------+
     |                   |
+----+----+       +------+------+
| Analyzer|       | open-mainframe-cobol |
|(text)   |       |   (parser)  |
+---------+       +-------------+
     |
     +-------+--------+--------+---------+
     |       |        |        |         |
+----+-+ +---+---+ +--+---+ +-+------+ +-+--------+
|Compat| |Metrics| |Report| |Features| |Complexity|
|Checker| |       | |      | |detect  | |scoring   |
+------+ +-------+ +------+ +--------+ +----------+

Additional stand-alone analyzers:
+-------------+ +-----------+ +---------+ +----------+ +---------+
|CicsInventory| |SqlAnalysis| |DeadCode | |CallGraph | |JclDeps  |
+-------------+ +-----------+ +---------+ +----------+ +---------+
```

### Module Structure

| Module | Description |
|--------|-------------|
| `analyzer` | Text-based COBOL source analyzer: `Analyzer`, `AnalysisResult`, `Feature`, feature pattern matching, metrics calculation, complexity scoring, recommendation generation |
| `ast_analyzer` | AST-based analyzer: `AstAnalyzer` parses via `open-mainframe-cobol`, walks the AST for accurate feature/metric extraction, falls back to text-based analysis on parse failure |
| `callgraph` | Call graph builder: `CallGraph`, `CallEdge`, `CallType` (static/dynamic/CICS LINK/XCTL), cycle detection via DFS, topological sort via Kahn's algorithm, source text extraction |
| `cics_inventory` | CICS command inventory: `CicsInventory`, `CicsCommand`, `CicsCategory` (12 categories), `SupportStatus` (Supported/Partial/Unsupported), source scanning and classification |
| `compatibility` | Compatibility checker: `CompatibilityChecker` with 10 default rules, `CompatibilityRule`, `CompatibilityIssue`, `Severity` (Info/Warning/High/Critical), `FeatureSupport` catalog |
| `dead_code` | Dead code detection: `detect_dead_code`, `DeadCodeReport`, `DeadCodeItem`, paragraph/section identification, PERFORM/GO TO/THRU reference tracking |
| `jcl_deps` | JCL dependency analysis: `analyze_jcl`, `JclDependencyMap`, `JclJob`, `JclStep`, `DdStatement`, program-to-dataset and dataset-to-program cross-reference maps |
| `metrics` | Code metrics: `CodeMetrics`, `ComplexityLevel`, `calculate_maintainability_index`, `estimate_technical_debt`, line counts, cyclomatic complexity, comment ratio, statements per paragraph |
| `report` | Report generation: `Report`, `ReportFormat` (Text/Markdown/JSON/HTML), executive summary, per-file details, issue listing, recommendations, feature support table |
| `scanner` | Batch scanner: `Scanner`, `ScanConfig`, `ScanEntry`, `ScanResult`, recursive directory walking, glob pattern filtering, copybook resolution from configurable include paths |
| `snapshot` | Snapshot comparison: `AssessmentSnapshot`, `ProgramSnapshot`, `SnapshotDiff`, `ProgressSummary`, `ComplexityChange`, JSON serialization round-trip, migration progress tracking |
| `sql_analysis` | DB2 SQL analysis: `SqlAnalysis`, `SqlStatement`, `SqlComplexity` (Simple/Join/Subquery/Cursor/Dynamic/Utility), PostgreSQL compatibility notes, effort scoring |

## Key Types and Traits

### Core Analysis
- `Analyzer` — Text-based COBOL source analyzer with feature pattern matching and compatibility checking
- `AstAnalyzer` — AST-based analyzer using the `open-mainframe-cobol` parser, with automatic fallback
- `AnalysisResult` — Per-file result: program ID, metrics, features, issues, complexity rating, recommendations
- `Feature` — Detected feature with name, category, occurrence count, and line numbers
- `FeatureCategory` — `CoreLanguage`, `FileHandling`, `Database`, `Transaction`, `PlatformSpecific`, `Interoperability`, `Batch`
- `MigrationComplexity` — `Low`, `Medium`, `High`, `VeryHigh` with effort multipliers (1.0x / 2.0x / 3.5x / 5.0x)

### Metrics
- `CodeMetrics` — Total/code/blank/comment lines, executable statements, cyclomatic complexity, paragraph count, data items, division presence flags
- `ComplexityLevel` — `Low` (<= 10), `Moderate` (11-20), `High` (21-50), `VeryHigh` (50+)
- `calculate_maintainability_index` — Simplified MI formula: 171 - 5.2*ln(HV) - 0.23*CC - 16.2*ln(LOC), clamped to 0-100
- `estimate_technical_debt` — Base hours (1hr per 100 LOC) + complexity penalty + documentation penalty

### Compatibility
- `CompatibilityChecker` — Rule-based checker with 10 default rules (IMS/DL1, console display, date/time, relative files, external assignments, ALTER, GO TO DEPENDING, ENTRY, BY CONTENT LENGTH, SEARCH ALL)
- `CompatibilityIssue` — Issue with code, description, severity, category, optional line number, recommendation
- `Severity` — Ordered: `Info` < `Warning` < `High` < `Critical`
- `FeatureSupport` — Catalog of 10 feature areas with support boolean, level (0-100), and notes

### CICS Inventory
- `CicsInventory` — Scans COBOL source for EXEC CICS commands, classifies by category and support status
- `CicsCommand` — Verb, category, count, line numbers, support status
- `CicsCategory` — 12 categories: FileControl, TerminalControl, ProgramControl, QueueControl, IntervalControl, TaskControl, StorageControl, BmsControl, JournalControl, SyncpointControl, ExceptionHandling, Other
- `SupportStatus` — `Supported`, `Partial`, `Unsupported` — maps each CICS verb to its support level in `open-mainframe-cics`

### SQL Analysis
- `SqlAnalysis` — Extracts EXEC SQL blocks, classifies complexity, computes effort scores
- `SqlStatement` — Verb, complexity level, line number, PostgreSQL compatibility notes
- `SqlComplexity` — 6 levels with effort weights: Simple (1.0), Join (1.5), Subquery (2.0), Cursor (2.5), Dynamic (3.0), Utility (1.0)

### Call Graph
- `CallGraph` — Directed graph with adjacency list, supports adding programs/edges, cycle detection (DFS), topological sort (Kahn's)
- `CallEdge` — Caller, callee, call type, uncertain flag
- `CallType` — `StaticCall`, `DynamicCall`, `CicsLink`, `CicsXctl`

### Dead Code
- `detect_dead_code` — Scans procedure division for paragraph/section definitions and references (PERFORM, GO TO, THRU/THROUGH, INPUT/OUTPUT PROCEDURE), flags unreachable code
- `DeadCodeReport` — Total paragraphs, dead items, dead lines, dead percentage, adjusted line count
- `DeadCodeItem` — Name, line, line count, kind (Paragraph/Section)

### JCL Dependencies
- `analyze_jcl` — Parses JCL job streams into `JclDependencyMap`
- `JclDependencyMap` — Jobs, program-to-job mapping, program-to-dataset mapping, dataset-to-program mapping, shared dataset detection
- `JclJob` / `JclStep` / `DdStatement` — Job/step/DD hierarchy with PGM, PROC, DSN, DISP, I/O type classification

### Scanning & Reporting
- `Scanner` — Batch scanner: discovers COBOL files, resolves copybooks, analyzes each, produces a `Report`
- `ScanConfig` — Root directory, include patterns (glob), source extensions, copybook paths, recursive flag
- `Report` — Aggregated results with Text/Markdown/JSON/HTML generation
- `AssessmentSnapshot` — Serializable assessment run for progress tracking
- `SnapshotDiff` / `ProgressSummary` — Comparison between snapshots: resolved programs, new programs, complexity changes, migration progress percentage, remaining effort

## Implementation Details

### Text-Based vs AST-Based Analysis
The `Analyzer` (text-based) performs line-by-line keyword matching against configurable `FeaturePattern` lists. It's fast and never fails, but can produce false positives (e.g., EXEC SQL in a comment). The `AstAnalyzer` parses source via `open-mainframe-cobol` (`scan` + `Parser::parse_program`), then walks the AST using `collect_all_statements` which recursively descends through `ProcedureBody::Sections/Paragraphs/Statements` and nested `If`/`Evaluate`/`Perform`/`Compute`/`Read` blocks. When parsing fails, `AstAnalyzer` falls back to `Analyzer` with a warning recommendation.

### Complexity Scoring
Both analyzers use a point-based scoring system:
- Code size: >500 lines (+1), >2000 (+2), >5000 (+3)
- Cyclomatic complexity: >10 (+1), >20 (+2), >50 (+3)
- Feature categories: Database (+2), Transaction (+2), PlatformSpecific (+3), FileHandling (+1)
- Issue severity: Critical (+3 each), High (+2 each)
- Total maps to: 0-3 = Low, 4-7 = Medium, 8-12 = High, 13+ = VeryHigh

### Search Buffer / CICS Classification
`CicsInventory::from_source` does a two-pass approach: first extracts the verb (handling two-word verbs like "SEND MAP", "WRITEQ TS", etc.), then classifies via `classify_verb` into 12 categories and `support_status` into 3 support levels, all based on match tables.

### SQL Complexity Detection
`SqlAnalysis::from_source` collects multi-line EXEC SQL...END-EXEC blocks, then `classify_sql` determines the verb and base complexity, then upgrades Simple to Join (if contains " JOIN ") or Subquery (if contains "(SELECT" or "EXISTS("). DB2-specific constructs (WITH UR/CS/RS/RR, OPTIMIZE FOR, FOR UPDATE OF) generate PostgreSQL compatibility notes.

### Call Graph Algorithms
- **Cycle detection**: DFS with on-stack tracking; back edges indicate cycles, which are extracted from the stack
- **Topological sort**: Kahn's algorithm (BFS from zero-in-degree nodes), then reversed to produce leaf-first order for migration planning
- **Source extraction**: Text-based pattern matching for `CALL 'literal'`, `CALL variable`, `EXEC CICS LINK PROGRAM('name')`, `EXEC CICS XCTL PROGRAM('name')`

### Copybook Resolution
`Scanner::resolve_copybooks` scans for `COPY <name>.` lines, searches configured include paths with copybook extensions (.cpy, .CPY, .copy, .COPY), source extensions (.cbl, .cob), and bare names, then inlines the first match. Already-resolved names are tracked via `HashSet` to prevent infinite recursion.

## Syntax / Feature Coverage

### Detected COBOL Features

| Feature | Category | Detection |
|---------|----------|-----------|
| VSAM (ORGANIZATION IS INDEXED) | FileHandling | Implemented (text + AST) |
| Sequential Files | FileHandling | Implemented (text + AST) |
| Relative Files | FileHandling | Implemented (AST only) |
| DB2 (EXEC SQL) | Database | Implemented (text + AST) |
| IMS (EXEC DLI) | Database | Implemented (text only) |
| CICS (EXEC CICS) | Transaction | Implemented (text + AST) |
| Subprogram Calls (CALL) | Interoperability | Implemented (text + AST) |
| COPY Statements | Interoperability | Implemented (text only) |
| DISPLAY | CoreLanguage | Implemented (text + AST) |
| ACCEPT | CoreLanguage | Implemented (text + AST) |
| STRING/UNSTRING | CoreLanguage | Implemented (text + AST) |
| INSPECT | CoreLanguage | Implemented (text + AST) |
| COMPUTE | CoreLanguage | Implemented (text + AST) |
| SORT/MERGE | Batch | Implemented (AST only) |

### Compatibility Rules (Default)

| Code | Pattern | Severity | Category |
|------|---------|----------|----------|
| PLAT001 | EXEC DLI | Critical | Database |
| PLAT002 | UPON CONSOLE | Warning | PlatformSpecific |
| PLAT003 | ACCEPT FROM DATE | Info | PlatformSpecific |
| PLAT004 | ACCEPT FROM TIME | Info | PlatformSpecific |
| FILE001 | ORGANIZATION IS RELATIVE | High | FileHandling |
| FILE002 | ASSIGN TO EXTERNAL | Warning | FileHandling |
| DEPR001 | ALTER | High | CoreLanguage |
| DEPR002 | GO TO DEPENDING | Warning | CoreLanguage |
| DEPR003 | ENTRY | Warning | Interoperability |
| CALL001 | CALL USING BY CONTENT LENGTH | Warning | Interoperability |
| PERF001 | SEARCH ALL | Info | CoreLanguage |

### Report Formats

| Format | Status |
|--------|--------|
| Plain Text | Implemented |
| Markdown | Implemented |
| JSON | Implemented |
| HTML | Implemented |

## Usage Examples

```rust
use open_mainframe_assess::analyzer::Analyzer;
use open_mainframe_assess::ast_analyzer::AstAnalyzer;
use open_mainframe_assess::report::{Report, ReportFormat};
use open_mainframe_assess::scanner::{ScanConfig, Scanner};
use open_mainframe_assess::cics_inventory::CicsInventory;
use open_mainframe_assess::sql_analysis::SqlAnalysis;
use open_mainframe_assess::dead_code::detect_dead_code;
use open_mainframe_assess::callgraph::{CallGraph, CallType};
use open_mainframe_assess::jcl_deps::analyze_jcl;
use open_mainframe_assess::snapshot::{AssessmentSnapshot, compare_snapshots};

// --- Text-based analysis ---
let analyzer = Analyzer::new();
let result = analyzer.analyze(cobol_source, "CUSTINQ.cbl").unwrap();
println!("{}", result.summary());

// --- AST-based analysis (preferred) ---
let ast_analyzer = AstAnalyzer::new();
let result = ast_analyzer.analyze(cobol_source, "CUSTINQ.cbl").unwrap();

// --- Batch directory scan ---
let config = ScanConfig::new("/path/to/cobol/src")
    .with_copybook_path("/path/to/copybooks")
    .with_pattern("src/**/*.cbl");
let scanner = Scanner::new(config);
let scan_result = scanner.scan().unwrap();
let report = scan_result.report;
println!("{}", report.generate(ReportFormat::Markdown));

// --- CICS command inventory ---
let inventory = CicsInventory::from_source(cobol_source);
println!("Total CICS commands: {}", inventory.total_count);
println!("Supported: {}", inventory.supported_count());

// --- DB2 SQL analysis ---
let sql = SqlAnalysis::from_source(cobol_source);
println!("SQL statements: {}, effort score: {:.1}", sql.total_count, sql.effort_score);

// --- Dead code detection ---
let dead = detect_dead_code("CUSTINQ", cobol_source);
println!("Dead paragraphs: {} ({:.1}%)", dead.dead_items.len(), dead.dead_percentage());

// --- Call graph ---
let mut graph = CallGraph::new();
let edges = CallGraph::extract_from_source("MAINPROG", cobol_source);
for edge in edges {
    graph.add_edge(&edge.caller, &edge.callee, edge.call_type);
}
if let Some(order) = graph.topological_sort() {
    println!("Migration order: {:?}", order);
}

// --- JCL dependency analysis ---
let dep_map = analyze_jcl(jcl_source);
println!("Programs: {:?}", dep_map.all_programs());
println!("Shared datasets: {:?}", dep_map.shared_dataset_programs());

// --- Snapshot comparison ---
let baseline = AssessmentSnapshot::from_json(&baseline_json).unwrap();
let current = AssessmentSnapshot::from_json(&current_json).unwrap();
let diff = compare_snapshots(&baseline, &current);
println!("Progress: {:.1}%", diff.progress.progress_percent);
```

## Integration

### Internal Workspace Dependencies

- `open-mainframe-cobol`: COBOL parser, lexer, and AST types used for deep AST syntax walking in `AstAnalyzer`.
- `miette`: Diagnostic error reporting.
- `thiserror`: Error type derive macros.
- `serde` / `serde_json`: Serialization for reports, snapshots, and assessment results.

### Workspace Consumers

- `open-mainframe-wiki`: Documented in the migration and developer tooling catalog.
- Root workspace member in `Cargo.toml`.

## Testing

Run the full test suite:

```sh
cargo test -p open-mainframe-assess
```

The crate contains 99 unit tests organized by module:

- **analyzer**: Basic analysis, metrics calculation, DB2/CICS feature detection, complexity rating, recommendation generation
- **ast_analyzer**: Basic AST parsing, comment false-positive avoidance, DISPLAY detection, cyclomatic complexity with IF statements, paragraph counting, graceful fallback on parse failure
- **callgraph**: Graph edges, topological sort, cycle detection, static/dynamic call extraction, CICS LINK/XCTL extraction, single-node graph
- **cics_inventory**: Command classification, support status tracking, category grouping, comment skipping, multiple occurrences, category/status display names
- **compatibility**: Rule matching, severity ordering, IMS detection, custom rule addition, feature support catalog, issue builder pattern
- **dead_code**: Dead paragraph detection, no false positives on all-live code, PERFORM THRU range marking, GO TO references, section dead code, percentage calculation, entry point exemption
- **jcl_deps**: Simple job parsing, step-to-program mapping, dataset dependencies, multi-step jobs, DD I/O classification, comment skipping, shared datasets, multiple jobs, job class extraction
- **metrics**: Complexity level thresholds, code metrics calculation, maintainability index range, technical debt comparison, summary generation
- **report**: Report creation, Text/Markdown/JSON/HTML generation, empty report handling
- **scanner**: File discovery, recursive/non-recursive scanning, glob filtering, full scan end-to-end, non-existent directory error, copybook resolution, missing copybook tolerance, analysis result verification (uses temp directories)
- **snapshot**: JSON round-trip serialization, migrated count, resolved programs, complexity changes, newly migrated detection, progress summary, new program detection, remaining effort calculation, empty snapshots
- **sql_analysis**: Simple queries, cursor operations, JOIN detection, subquery detection, dynamic SQL, PostgreSQL compatibility notes (WITH HOLD, WHENEVER, isolation clauses), effort scoring

Scanner tests create and clean up temporary directories; all other tests use inline source strings.

## Limitations

- **IMS/DL1 Detection**: Only text-based pattern matching is performed for `EXEC DLI`; AST-level analysis is not available for IMS macros without precompiler expansion.
- **Dead Code Analysis Scope**: Tracks explicit control flow references (`PERFORM`, `GO TO`, `THRU`/`THROUGH`, `INPUT`/`OUTPUT PROCEDURE`). Fall-through execution across consecutive unreferenced paragraphs is not modeled.
- **Dynamic Call Target Resolution**: Dynamic subprogram calls (`CALL identifier`) are flagged as uncertain because the runtime target cannot be deterministically resolved at compile time.
- **Copybook Resolution Depth**: Single-level copybook resolution is performed; nested `COPY` statements inside included copybooks are not recursively expanded.
- **Text-Mode Complexity Approximation**: Cyclomatic complexity calculated via text scanning estimates decision points by keyword occurrences; AST mode provides exact decision point counts.

## Related Documentation

- [OpenMainframe Crate Map](../../docs/architecture/crate-map.md)
- [open-mainframe-cobol](../open-mainframe-cobol/README.md)
- [open-mainframe-dataset](../open-mainframe-dataset/README.md)
- [open-mainframe-db2](../open-mainframe-db2/README.md)
- [open-mainframe-cics](../open-mainframe-cics/README.md)
- [open-mainframe-jcl](../open-mainframe-jcl/README.md)
