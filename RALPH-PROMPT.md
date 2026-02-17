# OpenMainframe v3.0 Implementation — Epic Execution Agent

You are an Implementation Agent executing the v3.0 epics across all OpenMainframe crates. Each iteration you implement ONE batch of related epics (1–3 epics from the same crate), write tests, ensure all tests pass, and commit.

## Self-Orientation (Do This First Every Iteration)

1. Run `git log --oneline -20` to see what has already been implemented
2. Check the **Batch Order** below and find the next batch that does NOT have a matching commit
3. A batch is complete when its commit message starts with `feat(<crate>): Epic NNN`
4. If ALL batches are done, output the completion promise

## Batch Order

Batches are ordered by priority (CRITICAL → MAJOR → MINOR) with dependency-aware sequencing. Each batch lists the crate, epic numbers, and a short description.

### Phase 1 — CRITICAL Gaps (Batches 1–21)

| Batch | Crate | Epics | Description |
|-------|-------|-------|-------------|
| 1 | encoding | 700 | Extended EBCDIC code page library (CP1140–1149, national pages, registry) |
| 2 | runtime | 500 | External CALL resolution and dynamic dispatch |
| 3 | runtime | 501 | True GO TO and paragraph flow control |
| 4 | runtime | 502 | Complete intrinsic function library |
| 5 | jcl | 100, 101 | Symbolic parameters & in-stream procedures (PROC/PEND) |
| 6 | jcl | 102 | Cataloged procedure support (JCLLIB, nesting) |
| 7 | jcl | 103 | IF/THEN/ELSE/ENDIF conditional processing |
| 8 | dataset | 600 | VSAM alternate index (AIX) support |
| 9 | dataset | 601 | PDS/PDSE member management |
| 10 | cics | 200 | Channel and container support |
| 11 | cics | 201, 206 | TD queue commands + preprocessor-to-runtime bridge |
| 12 | cics | 202 | Persistent file storage via dataset integration |
| 13 | db2 | 300 | Dynamic SQL (PREPARE/EXECUTE/EXECUTE IMMEDIATE) |
| 14 | db2 | 301, 302 | WHENEVER error handling + PostgreSQL runtime connectivity |
| 15 | ims | 400 | EXEC DLI preprocessor |
| 16 | ims | 401 | System service calls (CHKP, SYNC, ROLB, LOG, STAT) |
| 17 | ims | 402, 403 | I/O PCB message handling + database persistence |
| 18 | sort | 800 | External sort (disk-based k-way merge) |
| 19 | sort | 801, 802 | OUTFIL multi-output routing + IFTHEN conditional reformatting |
| 20 | assess | 1000, 1001 | AST-based analysis + file/directory scanning |
| 21 | deploy | 1100, 1101 | HTTP server for metrics/health + runtime integration hooks |

### Phase 2 — MAJOR Gaps (Batches 22–48)

| Batch | Crate | Epics | Description |
|-------|-------|-------|-------------|
| 22 | encoding | 701, 702 | COMP-5 native binary + IBM HFP floating point (COMP-1/COMP-2) |
| 23 | runtime | 503, 504 | Numeric editing/display formats + binary storage formats |
| 24 | runtime | 505 | File I/O with status codes |
| 25 | runtime | 506 | SORT verb runtime integration |
| 26 | runtime | 507 | PERFORM VARYING + 88-level conditions |
| 27 | jcl | 104, 105 | Extended DD parameters + JOB/EXEC parameters |
| 28 | jcl | 106, 107 | INCLUDE/DD concatenation + GDG/OUTPUT statement |
| 29 | dataset | 602, 603 | VSAM LDS + concurrent access/record locking |
| 30 | dataset | 604, 605 | VSAM free space management + spanned records |
| 31 | cics | 203, 204 | CONVERSE command + complete EBCDIC translation |
| 32 | cics | 205, 207 | ASSIGN runtime + deadlock detection/lock management |
| 33 | cics | 208 | Auxiliary TS queue storage |
| 34 | db2 | 303, 304 | Scrollable cursors + savepoint support |
| 35 | db2 | 305, 306 | Comprehensive SQLCODE/SQLSTATE mapping + indicator variables |
| 36 | db2 | 307, 308 | Multi-row FETCH + SQL dialect translation expansion |
| 37 | db2 | 309 | End-to-end integration testing |
| 38 | ims | 404, 405 | GSAM database support + comprehensive status codes |
| 39 | ims | 406, 407 | Secondary index support + logical relationships |
| 40 | ims | 408, 409 | Boolean SSA qualifications + segment data extraction |
| 41 | sort | 803, 804 | Numeric SUM summation + BUILD/OVERLAY/FINDREP |
| 42 | sort | 805, 806 | JOINKEYS file matching + ICETOOL utility |
| 43 | sort | 807 | Variable-length record support (VB, FTOV, VTOF) |
| 44 | tui | 900, 901 | Extended screen sizes (Models 2–5) + PA key support |
| 45 | tui | 902 | Cursor positioning via SEND MAP CURSOR option |
| 46 | assess | 1002, 1003 | Call graph analysis + CICS command inventory |
| 47 | assess | 1004 | DB2 SQL complexity analysis |
| 48 | deploy | 1102, 1103, 1104 | Distributed tracing + K8s manifests + secrets management |

### Phase 3 — MINOR Gaps (Batches 49–61)

| Batch | Crate | Epics | Description |
|-------|-------|-------|-------------|
| 49 | encoding | 703, 707 | IEEE binary floating point + EBCDIC collation order |
| 50 | encoding | 704, 705, 706 | DBCS mixed encoding + NATIONAL (UTF-16) + field conversion pipeline |
| 51 | runtime | 508 | Date/time accuracy + LE callable services |
| 52 | jcl | 108, 109 | Utility program registry + error reporting/test coverage |
| 53 | dataset | 606, 607, 608 | REPRO filtering + catalog persistence + BSAM/BPAM |
| 54 | cics | 209 | BMS extended attributes + page building |
| 55 | db2 | 310 | Additional SQL statements (GRANT, REVOKE, LABEL, COMMENT) |
| 56 | ims | 410 | DBD parser enhancements |
| 57 | sort | 808, 809, 810 | Record limits + HEADER/TRAILER + date/time editing |
| 58 | tui | 903, 906 | Field validation attributes + OIA |
| 59 | tui | 904, 905, 907 | TN3270 protocol + structured fields + DBCS rendering |
| 60 | assess | 1005, 1006, 1007 | Dead code detection + progress tracking + JCL dependency analysis |
| 61 | deploy | 1105, 1106, 1107 | Batch job metrics + dashboards/alerts + container image |

## Implementation Protocol (Each Iteration)

### Step 1: Read Planning Artifacts

Read all three planning documents for the current crate:
```
_bmad-output/planning-artifacts/epics-<crate>-v3.0.md
_bmad-output/planning-artifacts/architecture-<crate>-v3.0.md
_bmad-output/planning-artifacts/prd-<crate>-v3.0.md
```
Focus on the specific epic(s) for this batch. Understand:
- Acceptance criteria for each story
- Architecture decisions that apply
- Complexity estimates (S/M/L/XL)

### Step 2: Read Existing Code

Read all relevant source files in the target crate:
```bash
find crates/<crate-name>/src -name "*.rs" -type f
```
Understand the existing architecture before making changes.

### Step 3: Implement

For each story in the batch:
1. Implement the feature following the architecture decisions
2. Write unit tests that verify the acceptance criteria
3. Keep changes focused — don't refactor unrelated code
4. Follow existing code patterns and conventions

**Implementation rules:**
- Add new modules/files when needed, but prefer extending existing files
- Use `pub` visibility for types and functions that other crates may need
- Add `#[cfg(test)]` test modules in the same file as the implementation
- Follow Rust naming conventions (snake_case functions, CamelCase types)
- Add doc comments (`///`) on all public types and functions
- Do NOT add unnecessary dependencies — reuse what's in the workspace

### Step 4: Test

Run the full test suite for the crate:
```bash
cargo test -p <package-name> 2>&1
```
**ALL tests must pass** — both new and existing. If an existing test breaks, fix the regression before committing.

Also run clippy:
```bash
cargo clippy -p <package-name> 2>&1
```
Fix any warnings.

### Step 5: Commit

```bash
git add crates/<crate-name>/
git commit -m "feat(<crate>): Epic NNN — <epic title>

Implemented:
- Story NNN.1: <description>
- Story NNN.2: <description>
...

Tests: N new tests added, M total passing"
```

If the batch has multiple epics, list all:
```
feat(<crate>): Epic NNN & NNN — <combined description>
```

### Step 6: Exit

After committing, stop. The loop will restart and you'll pick up the next batch.

## Important Rules

1. **ONE BATCH PER ITERATION** — Implement the epics listed in one batch, commit, then exit.
2. **ACCEPTANCE CRITERIA ARE REQUIREMENTS** — Every acceptance criterion in the epic must be satisfied and tested.
3. **ALL TESTS MUST PASS** — Run `cargo test -p <package>` and fix any failures. Never commit with failing tests.
4. **NO REGRESSIONS** — Existing tests must continue to pass. If your changes break something, fix it.
5. **FOLLOW ARCHITECTURE DECISIONS** — The `architecture-<crate>-v3.0.md` documents describe HOW to implement. Follow them.
6. **COMMIT MESSAGE FORMAT** — Always start with `feat(<crate>): Epic NNN — <title>`.
7. **DEPENDENCY AWARENESS** — If an epic depends on another crate's types, import them. If the dependency doesn't exist in Cargo.toml, add it.
8. **TEST THOROUGHLY** — Each acceptance criterion should have at least one test. Complex features need edge case tests.
9. **READ BEFORE WRITE** — Always read the existing code before modifying it. Understand patterns before changing them.

## Quality Checklist (Verify Before Commit)

For each batch, confirm:
- [ ] Read the epic(s) acceptance criteria
- [ ] Read the architecture decisions
- [ ] Read existing source files in the crate
- [ ] Implemented all stories in the batch
- [ ] Each acceptance criterion has a corresponding test
- [ ] `cargo test -p <package>` passes (all tests)
- [ ] `cargo clippy -p <package>` has no warnings
- [ ] Commit message follows format
- [ ] No unrelated changes included

## Completion

When ALL 61 batches have been implemented and committed (visible in `git log`), output the completion promise tag with text V3 IMPLEMENTATION COMPLETE.
