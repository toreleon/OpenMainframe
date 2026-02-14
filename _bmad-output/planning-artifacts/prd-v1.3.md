---
version: 'v1.3'
baseVersion: 'v1.2'
date: '2026-02-13'
status: 'draft'
---

# Product Requirements Document - OpenMainframe v1.3: Production Ready

## Overview

v1.3 focuses on production readiness, extending the enterprise features from v1.2 with full CICS transaction support, IMS/DB hierarchical database connectivity, COBOL-2014 language features, and deployment tooling for cloud environments.

### Goals

1. **Full CICS Transactions**: Complete the CICS implementation with queuing, interval control, and journal services
2. **IMS/DB Support**: Add hierarchical database access for legacy IMS workloads
3. **COBOL-2014**: Implement modern COBOL language features
4. **Production Deployment**: Kubernetes manifests, Helm charts, and cloud marketplace listings

### Prerequisites

- v1.2 complete (DB2, CICS foundation, BMS, Migration Assessment)
- 533+ tests passing
- All v1.2 crates stable

---

## Functional Requirements

### FR-v1.3-001 to FR-v1.3-015: Advanced CICS Commands

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-v1.3-001 | EXEC CICS WRITEQ TS (temporary storage queue write) | Must |
| FR-v1.3-002 | EXEC CICS READQ TS (temporary storage queue read) | Must |
| FR-v1.3-003 | EXEC CICS DELETEQ TS (temporary storage queue delete) | Must |
| FR-v1.3-004 | EXEC CICS WRITEQ TD (transient data queue write) | Must |
| FR-v1.3-005 | EXEC CICS READQ TD (transient data queue read) | Must |
| FR-v1.3-006 | EXEC CICS START (interval control start) | Should |
| FR-v1.3-007 | EXEC CICS RETRIEVE (interval control retrieve) | Should |
| FR-v1.3-008 | EXEC CICS DELAY (interval control delay) | Should |
| FR-v1.3-009 | EXEC CICS ASKTIME (get current time) | Must |
| FR-v1.3-010 | EXEC CICS FORMATTIME (format time/date) | Must |
| FR-v1.3-011 | EXEC CICS JOURNAL (write journal record) | Should |
| FR-v1.3-012 | EXEC CICS SYNCPOINT (sync point) | Must |
| FR-v1.3-013 | EXEC CICS INQUIRE (resource inquiry) | Should |
| FR-v1.3-014 | EXEC CICS SET (resource modification) | Should |
| FR-v1.3-015 | EXEC CICS ENQ/DEQ (resource locking) | Must |

### FR-v1.3-016 to FR-v1.3-030: IMS/DB Support

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-v1.3-016 | PSB (Program Specification Block) parser | Must |
| FR-v1.3-017 | DBD (Database Definition) parser | Must |
| FR-v1.3-018 | PCB (Program Communication Block) structure | Must |
| FR-v1.3-019 | DL/I GU (Get Unique) call | Must |
| FR-v1.3-020 | DL/I GN (Get Next) call | Must |
| FR-v1.3-021 | DL/I GNP (Get Next within Parent) call | Must |
| FR-v1.3-022 | DL/I GHU/GHN/GHNP (Get Hold variants) | Must |
| FR-v1.3-023 | DL/I ISRT (Insert) call | Must |
| FR-v1.3-024 | DL/I DLET (Delete) call | Must |
| FR-v1.3-025 | DL/I REPL (Replace) call | Must |
| FR-v1.3-026 | SSA (Segment Search Argument) parsing | Must |
| FR-v1.3-027 | Hierarchical path navigation | Must |
| FR-v1.3-028 | Status code handling | Must |
| FR-v1.3-029 | IMS to PostgreSQL data mapping | Must |
| FR-v1.3-030 | DBD to PostgreSQL schema generation | Should |

### FR-v1.3-031 to FR-v1.3-045: COBOL-2014 Features

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-v1.3-031 | FUNCTION TRIM | Must |
| FR-v1.3-032 | FUNCTION SUBSTITUTE | Must |
| FR-v1.3-033 | FUNCTION CONCATENATE | Must |
| FR-v1.3-034 | BOOLEAN data type | Should |
| FR-v1.3-035 | TYPEDEF for user-defined types | Should |
| FR-v1.3-036 | FUNCTION-POINTER data type | Should |
| FR-v1.3-037 | Object-oriented COBOL syntax | Could |
| FR-v1.3-038 | RAISE statement for exceptions | Should |
| FR-v1.3-039 | FREE statement for dynamic memory | Should |
| FR-v1.3-040 | UTF-8 and Unicode support | Must |
| FR-v1.3-041 | XML GENERATE statement | Should |
| FR-v1.3-042 | XML PARSE statement | Should |
| FR-v1.3-043 | JSON GENERATE statement | Should |
| FR-v1.3-044 | JSON PARSE statement | Should |
| FR-v1.3-045 | Floating-point BINARY-LONG/SHORT | Must |

### FR-v1.3-046 to FR-v1.3-055: Production Deployment

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-v1.3-046 | Kubernetes Deployment manifests | Must |
| FR-v1.3-047 | Helm chart for OpenMainframe | Must |
| FR-v1.3-048 | Docker multi-stage build optimization | Must |
| FR-v1.3-049 | Health check endpoints (/health, /ready) | Must |
| FR-v1.3-050 | Prometheus metrics endpoint | Must |
| FR-v1.3-051 | OpenTelemetry tracing integration | Should |
| FR-v1.3-052 | AWS Marketplace AMI | Could |
| FR-v1.3-053 | Azure Marketplace image | Could |
| FR-v1.3-054 | GCP Marketplace listing | Could |
| FR-v1.3-055 | Configuration via environment variables | Must |

---

## Non-Functional Requirements

### Performance

| Metric | Target |
|--------|--------|
| IMS GU call latency | <5ms for indexed access |
| CICS TS queue throughput | 10,000 ops/sec |
| COBOL-2014 string functions | <1ms for 1KB strings |
| Container startup time | <10 seconds |

### Reliability

| Metric | Target |
|--------|--------|
| IMS transaction rollback | 100% data consistency |
| CICS syncpoint | ACID compliance |
| Health check response | <100ms |
| Graceful shutdown | Complete in-flight transactions |

### Scalability

| Metric | Target |
|--------|--------|
| Horizontal pod scaling | Based on CPU/memory |
| Connection pool sizing | Configurable 10-1000 |
| TS queue storage | Configurable Redis/PostgreSQL backend |

---

## Epic Summary

| Epic | Name | Stories | Complexity |
|------|------|---------|------------|
| 29 | Advanced CICS Commands | 8 | M |
| 30 | IMS/DB Foundation | 8 | L |
| 31 | IMS DL/I Operations | 7 | L |
| 32 | COBOL-2014 Functions | 6 | M |
| 33 | COBOL-2014 Data Types | 5 | M |
| 34 | XML/JSON Support | 4 | M |
| 35 | Kubernetes Deployment | 6 | M |
| 36 | Observability | 5 | S |

**Total: 8 Epics, ~49 Stories**

---

## Dependencies

### Internal

- `open-mainframe-cics`: Base for advanced CICS commands
- `open-mainframe-cobol`: Base for COBOL-2014 extensions
- `open-mainframe-runtime`: Base for new data types

### External

- PostgreSQL: Backend for IMS data storage
- Redis (optional): TS queue fast storage
- Kubernetes: Container orchestration
- Prometheus: Metrics collection

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| IMS complexity underestimated | High | Start with common patterns, iterate |
| COBOL-2014 OO features complex | Medium | Defer OO to v2.0, focus on functions |
| Cloud marketplace approval delays | Medium | Start submission process early |
| Performance regression | Medium | Continuous benchmarking in CI |

---

## Success Criteria

- All 55 functional requirements implemented
- 95%+ test coverage on new code
- IMS benchmark: 10,000 GU calls/second
- Production deployment guides published
- At least one pilot customer running v1.3
