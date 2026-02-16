# Assess Crate — Architecture Decisions

## AD-3.0-01: AST Integration via open-mainframe-cobol Dependency

**Context:** The current analyzer uses text pattern matching (case-insensitive substring search) to detect COBOL features. This produces false positives from comments, string literals, and partially matching identifiers. The `open-mainframe-cobol` crate provides a full COBOL parser that produces an AST.

**Decision:** Add `open-mainframe-cobol` as a dependency. The `Analyzer::analyze()` method first parses the source using the COBOL parser, then walks the AST to extract features, metrics, and compatibility issues. Text-based analysis remains as a fallback for unparseable code (with a warning in the report). Feature detection becomes AST-node-based: EXEC SQL blocks, EXEC CICS blocks, CALL statements, file SELECT clauses, etc.

**Consequences:**
- Eliminates false positives from pattern matching
- Provides accurate statement and paragraph counts
- Enables data flow and control flow analysis
- Parser errors in source code are handled gracefully (partial analysis)
- Adds a compile-time dependency on the COBOL crate

## AD-3.0-02: Batch Scanner with Parallel Analysis

**Context:** Real migration assessments scan hundreds or thousands of COBOL programs. The current API accepts a single source string. Directory scanning, file discovery, and parallel analysis are needed for practical use.

**Decision:** Add a `Scanner` struct that takes a root directory path and optional glob patterns. The scanner discovers COBOL source files (`.cbl`, `.cob`, `.cpy`), resolves copybook paths, and runs `Analyzer::analyze()` on each file. Analysis is parallelized using `rayon` for multi-core utilization. Results are aggregated into a single `Report`. Copybook paths are resolved relative to configurable include directories.

**Consequences:**
- Single command to assess an entire codebase
- Parallel analysis utilizes all available CPU cores
- Copybook resolution prevents duplicate analysis
- Include path configuration matches COBOL compiler conventions
- File I/O errors on individual files don't abort the entire scan

## AD-3.0-03: Call Graph as Directed Graph

**Context:** Understanding program interdependencies is critical for migration planning. CALL statements, CICS LINK/XCTL, and COPY dependencies form a graph structure.

**Decision:** Build a directed graph (adjacency list) where nodes are programs and edges are call relationships (CALL, LINK, XCTL, COPY). Each edge is typed (static call, dynamic call, CICS link, copybook include). The graph supports queries: entry points (no incoming edges), leaf programs (no outgoing edges), call depth, strongly connected components (circular dependencies), and topological sort (migration order).

**Consequences:**
- Migration order is computed automatically (topological sort)
- Circular dependencies are identified and flagged
- Blast radius of changes is quantifiable
- Dynamic calls (CALL variable) create uncertain edges (marked as such)
- Graph can be exported as DOT format for visualization

## AD-3.0-04: Feature Support as Live Query

**Context:** Feature support percentages are currently hardcoded (e.g., VSAM KSDS 90%, CICS 75%). These become stale as crates are improved.

**Decision:** Replace static percentages with a `FeatureSupportQuery` trait that each crate can implement. The assess crate queries each crate's capabilities at compile time (via feature detection) or runtime (via a capabilities manifest). A fallback to static percentages is maintained for crates that don't implement the trait. The manifest approach uses a JSON file listing implemented features per crate.

**Consequences:**
- Support percentages stay accurate as crates improve
- No need to manually update assess crate when other crates add features
- Trait-based approach requires crate cooperation
- Manifest-based approach is simpler but requires separate maintenance
- Initial implementation uses manifest (simpler); trait can be added later
