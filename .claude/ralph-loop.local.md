---
active: true
iteration: 2
max_iterations: 30
completion_promise: "VERSION PLANNING COMPLETE"
started_at: "2026-02-12T16:41:58Z"
---

# zOS-clone Version Planning Loop

## Objective

Use BMAD workflows to plan post-MVP versions of zOS-clone. Create complete planning artifacts (PRD updates, Architecture extensions, Epics) for each version in the roadmap.

## Context

MVP (v1.0) planning is complete:
- `_bmad-output/planning-artifacts/prd.md` - MVP requirements
- `_bmad-output/planning-artifacts/architecture.md` - MVP architecture
- `_bmad-output/planning-artifacts/epics.md` - MVP epics (14 epics, 84 stories)

## Version Roadmap (from PRD)

### v1.1 - Batch Workload Ready
- VSAM file support (KSDS, ESDS, RRDS)
- SORT utility compatibility (DFSORT syntax)
- GDG support (Generation Data Groups)
- IDCAMS utility
- apt/yum package distribution

### v1.2 - Enterprise Features
- DB2 SQL support (PostgreSQL backend)
- Basic CICS command-level support (EXEC CICS)
- PL/I compiler (initial release)
- Migration assessment tooling
- VS Code extension / LSP

### v1.3 - Production Ready
- Full CICS transaction support
- IMS/DB connectivity
- COBOL-2014 features
- Commercial support offering
- Cloud marketplace listings

## Your Task

For EACH version (v1.1, v1.2, v1.3), create planning artifacts:

### Step 1: Version PRD

Create `_bmad-output/planning-artifacts/prd-v{VERSION}.md`:

1. **Read existing PRD** to understand format and baseline
2. **Define version scope** from roadmap above
3. **Write functional requirements** (FR-v{VERSION}-001, etc.)
4. **Write non-functional requirements** specific to version
5. **Define success criteria** and exit criteria
6. **Identify dependencies** on prior versions

Template structure:
```markdown
---
version: 'v1.1'
baseVersion: 'v1.0'
date: 'YYYY-MM-DD'
status: 'draft'
---

# PRD - zOS-clone v1.1: Batch Workload Ready

## Executive Summary
## Version Goals
## Dependencies on v1.0
## Functional Requirements
## Non-Functional Requirements
## Success Criteria
## Risk Assessment
```

### Step 2: Architecture Extension

Create `_bmad-output/planning-artifacts/architecture-v{VERSION}.md`:

1. **Read existing architecture** to understand patterns
2. **Identify new components** needed for version features
3. **Document architectural decisions** specific to version
4. **Define module structure** extensions
5. **Update dependency graph** if needed
6. **Specify integration points** with existing code

Key areas per version:

**v1.1 Architecture Focus:**
- VSAM file format implementation (B+ tree for KSDS)
- SORT utility as separate binary or integrated
- GDG catalog extension to zos-dataset
- IDCAMS command parser

**v1.2 Architecture Focus:**
- SQL preprocessor in zos-cobol
- PostgreSQL adapter in zos-dataset
- CICS transaction monitor architecture
- PL/I parser/codegen in new crate or zos-cobol extension
- LSP server architecture

**v1.3 Architecture Focus:**
- Full CICS (regions, transactions, BMS)
- IMS database adapter
- COBOL-2014 language extensions
- Commercial support infrastructure

### Step 3: Version Epics

Create `_bmad-output/planning-artifacts/epics-v{VERSION}.md`:

1. **Read existing epics** for format reference
2. **Create epics** for each major feature area
3. **Write stories** with acceptance criteria (Given/When/Then)
4. **Map to requirements** (FR traceability)
5. **Estimate relative complexity** (S/M/L/XL)
6. **Identify dependencies** between stories

## Output Structure

```
_bmad-output/planning-artifacts/
├── prd.md                      # v1.0 MVP (existing)
├── architecture.md             # v1.0 MVP (existing)
├── epics.md                    # v1.0 MVP (existing)
├── prd-v1.1.md                 # NEW
├── architecture-v1.1.md        # NEW
├── epics-v1.1.md               # NEW
├── prd-v1.2.md                 # NEW
├── architecture-v1.2.md        # NEW
├── epics-v1.2.md               # NEW
├── prd-v1.3.md                 # NEW
├── architecture-v1.3.md        # NEW
└── epics-v1.3.md               # NEW
```

## Quality Criteria

Each version's artifacts must:

- [ ] PRD has clear scope boundaries (what's in vs out)
- [ ] PRD requirements are testable and specific
- [ ] Architecture extends existing patterns (no conflicts)
- [ ] Architecture decisions have rationale
- [ ] Epics cover all PRD requirements
- [ ] Stories have Given/When/Then acceptance criteria
- [ ] Dependencies on prior versions are explicit
- [ ] No orphan requirements (all FRs mapped to epics)

## Iteration Instructions

1. **Check current state**: List files in `_bmad-output/planning-artifacts/`
2. **Identify next version** needing planning (v1.1 → v1.2 → v1.3)
3. **Create PRD first** for that version
4. **Create Architecture** extending MVP architecture
5. **Create Epics** with full story breakdown
6. **Validate coverage**: All requirements have stories
7. **Move to next version** or signal completion

## Progress Tracking

### Completed
<!-- Update as you complete each artifact -->

### In Progress
<!-- Current version being planned -->

### Versions Status
- [ ] v1.1 PRD
- [ ] v1.1 Architecture
- [ ] v1.1 Epics
- [ ] v1.2 PRD
- [ ] v1.2 Architecture
- [ ] v1.2 Epics
- [ ] v1.3 PRD
- [ ] v1.3 Architecture
- [ ] v1.3 Epics

## Completion Signal

When ALL version planning is complete (v1.1, v1.2, v1.3 all have PRD + Architecture + Epics):

```
<promise>VERSION PLANNING COMPLETE</promise>
```

## Notes

- Reference existing artifacts frequently for consistency
- Each version builds on prior versions
- Keep architecture decisions consistent with MVP patterns
- Use same epic/story format as MVP epics.md
- Estimated scope: v1.1 (~40 stories), v1.2 (~50 stories), v1.3 (~40 stories)
