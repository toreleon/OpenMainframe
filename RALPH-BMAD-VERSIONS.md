# Ralph Loop: BMAD Version Planning Automation

## Objective

Create versioned planning artifacts for zOS-clone using BMAD methodology:
- **v1.1** - Batch Workload Ready (MVP)
- **v1.2** - Enterprise Features (VSAM, CICS foundations)
- **v1.3** - Production Ready (Full enterprise deployment)

Each version requires: PRD → Architecture → Epics (in sequence)

## State Tracking

Check `_bmad-output/planning-artifacts/` for completed artifacts:
- `v1.1-prd.md` - v1.1 PRD complete
- `v1.1-architecture.md` - v1.1 Architecture complete
- `v1.1-epics.md` - v1.1 Epics complete
- `v1.2-prd.md` - v1.2 PRD complete
- `v1.2-architecture.md` - v1.2 Architecture complete
- `v1.2-epics.md` - v1.2 Epics complete
- `v1.3-prd.md` - v1.3 PRD complete
- `v1.3-architecture.md` - v1.3 Architecture complete
- `v1.3-epics.md` - v1.3 Epics complete

## Version Definitions

### v1.1 - Batch Workload Ready
**Theme:** Developer-first batch processing foundation
**Scope:**
- COBOL-85 core compilation to native Linux executables
- JCL interpreter for batch job execution
- Sequential file I/O (QSAM)
- EBCDIC/ASCII conversion
- CLI tooling for compile/run/check
- Basic error diagnostics with source locations
**Key Differentiator:** Prove compilation approach works with real batch workloads

### v1.2 - Enterprise Features
**Theme:** Data access and transaction foundations
**Scope:**
- VSAM file support (KSDS, ESDS, RRDS)
- CICS foundation (transaction manager, basic BMS screens)
- DB2 SQL preprocessing (embedded SQL)
- Enhanced JCL (SORT, MERGE, utilities)
- Configuration management system
- IDE integration (LSP server)
**Key Differentiator:** Enable existing enterprise data access patterns

### v1.3 - Production Ready
**Theme:** Enterprise deployment and operations
**Scope:**
- High availability and clustering
- Production monitoring and observability
- Security hardening (authentication, audit logs)
- Performance optimization and caching
- Migration tooling and compatibility validation
- Commercial support infrastructure
**Key Differentiator:** Ready for production mainframe workload migration

## Execution Instructions

### Step 1: Determine Current State

1. List files in `_bmad-output/planning-artifacts/`
2. Check which versioned artifacts exist
3. Determine next artifact to create based on sequence:
   - v1.1-prd.md → v1.1-architecture.md → v1.1-epics.md
   - v1.2-prd.md → v1.2-architecture.md → v1.2-epics.md
   - v1.3-prd.md → v1.3-architecture.md → v1.3-epics.md

### Step 2: Create Next Artifact

Based on current state, execute the appropriate BMAD workflow:

**For PRD creation:**
Execute: `/bmad-bmm-create-prd`
- When asked about existing documents, reference the base `prd.md` as foundation
- Focus scope on the specific version's theme and features from definitions above
- When prompted for output name, specify `v{X.Y}-prd.md`

**For Architecture creation:**
Execute: `/bmad-bmm-create-architecture`
- When asked for input documents, specify the corresponding version's PRD (`v{X.Y}-prd.md`)
- Reference base `architecture.md` for established patterns
- Focus architectural decisions on version-specific scope
- When prompted for output name, specify `v{X.Y}-architecture.md`

**For Epics creation:**
Execute: `/bmad-bmm-create-epics-and-stories`
- When asked for input documents, specify corresponding version's PRD and Architecture
- Break down version-specific features into implementable stories
- When prompted for output name, specify `v{X.Y}-epics.md`

### Step 3: Validate and Continue

After completing an artifact:
1. Verify the output file exists in `_bmad-output/planning-artifacts/`
2. If more artifacts remain, continue to next
3. If all 9 artifacts complete, output completion promise

## BMAD Workflow Interaction

When running BMAD workflows interactively:
- Respond thoughtfully to each step based on version scope
- Use version definitions above for scoping decisions
- Reference base artifacts (prd.md, architecture.md, epics.md) for consistency
- Maintain continuity between versions (v1.2 builds on v1.1, etc.)

## Completion Criteria

All 9 versioned artifacts exist:
- `v1.1-prd.md`, `v1.1-architecture.md`, `v1.1-epics.md`
- `v1.2-prd.md`, `v1.2-architecture.md`, `v1.2-epics.md`
- `v1.3-prd.md`, `v1.3-architecture.md`, `v1.3-epics.md`

When complete, output:
```
<promise>BMAD VERSION PLANNING COMPLETE</promise>
```

## Notes

- Each BMAD workflow is interactive and requires step-by-step responses
- The workflows have menus - select appropriate options based on version scope
- Maintain consistency with the base artifacts' decisions
- Each version should be a logical progression from the previous
