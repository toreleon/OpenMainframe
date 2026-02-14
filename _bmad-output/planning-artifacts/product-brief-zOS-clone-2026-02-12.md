---
stepsCompleted: [1, 2, 3, 4, 5]
inputDocuments: []
date: 2026-02-12
author: Tore
---

# Product Brief: OpenMainframe

## Executive Summary

OpenMainframe is an open-source mainframe compiler built in Rust that enables enterprises to execute their existing mainframe source code on commodity Linux hardware. By providing a faster, cheaper alternative to IBM mainframe licensing, OpenMainframe breaks vendor lock-in and democratizes access to mainframe computing. The project combines a novel compiler architecture with Rust's performance characteristics to deliver full z/OS language and feature compatibility without the prohibitive costs of traditional mainframe infrastructure.

---

## Core Vision

### Problem Statement

Enterprises running IBM mainframes face crippling licensing costs, hardware expenses, and vendor lock-in. Despite decades of investment in mainframe applications, organizations have limited options: continue paying IBM's premium pricing or undertake expensive, risky migration projects that often require rewriting mission-critical code.

### Problem Impact

- **Financial burden:** IBM mainframe licensing consumes significant IT budgets
- **Talent scarcity:** Mainframe skills are increasingly rare and expensive
- **Innovation paralysis:** Legacy lock-in prevents modernization efforts
- **Accessibility barriers:** Developers cannot easily learn or test mainframe code without expensive environments

### Why Existing Solutions Fall Short

Current alternatives like Micro Focus, LzLabs, and cloud provider migration tools offer partial solutions but come with their own licensing costs, limited compatibility, or require significant code changes. None have delivered a truly open, cost-effective, high-performance path to mainframe freedom.

### Proposed Solution

OpenMainframe is a Rust-based compiler system that executes mainframe source code natively on Linux. It targets full compatibility with IBM-supported languages and features including COBOL, PL/I, Assembler, JCL, CICS, IMS, DB2, VSAM, and RACF. The system allows enterprises to lift their existing mainframe workloads onto commodity hardware with minimal or no code changes.

### Key Differentiators

- **Open source:** Community-driven development with commercial support options
- **Rust performance:** High-level language delivering near-native execution speed
- **Novel compiler architecture:** Purpose-built for mainframe compatibility and performance
- **Cost reduction:** Eliminates IBM licensing fees entirely
- **Linux deployment:** Runs on commodity hardware, cloud VMs, or containers
- **No rewrite required:** Execute existing mainframe code as-is

---

## Target Users

### Primary Users

#### 1. Mainframe Developer - "Marcus Chen"

**Context & Background:**
Marcus is a senior COBOL developer with 15 years of mainframe experience at a regional insurance company. He maintains critical policy administration and claims processing applications - millions of lines of COBOL, some dating back to the 1990s. His team of 6 developers supports 40+ batch jobs and a CICS-based online system.

**Current Pain Points:**
- Waits hours for compile/test cycles on shared mainframe LPAR
- Limited access to test environments due to MIPS cost constraints
- Cannot run code locally - must submit JCL and wait
- Modern IDE support is clunky; stuck with ISPF or expensive proprietary tools
- Younger developers avoid mainframe roles, leaving him overworked
- Feels isolated from modern development practices (CI/CD, Git workflows)

**Goals & Motivations:**
- Faster feedback loops when developing and debugging
- Ability to run and test code on his laptop or in cloud CI pipelines
- Attract new talent by modernizing the development experience
- Preserve his expertise while embracing modern tooling

**Success Looks Like:**
"I can clone our COBOL codebase, compile it locally in seconds, run unit tests, and push to a CI pipeline - just like my colleagues working on the Java microservices."

---

#### 2. Mainframe Operations Engineer - "Diana Okonkwo"

**Context & Background:**
Diana leads a 4-person operations team at a logistics company running z/OS workloads. She manages batch scheduling (CA-7/TWS), monitors system health, handles incident response, and coordinates with capacity planning. Her systems process 2 million transactions daily.

**Current Pain Points:**
- Constant pressure to reduce mainframe costs while maintaining SLAs
- Hardware refresh cycles are expensive and disruptive
- Monitoring and alerting tools don't integrate well with modern observability stacks
- Scaling is expensive - every MIPS increase hits the budget
- Disaster recovery requires duplicate expensive hardware
- On-call incidents often require IBM support with slow response times

**Goals & Motivations:**
- Maintain five-nines reliability without the five-figure monthly bills
- Integrate mainframe workloads into existing Linux/Kubernetes monitoring
- Elastic scaling during peak periods (month-end, holiday season)
- Simpler DR with cloud-based failover options

**Success Looks Like:**
"Our batch window runs on a Kubernetes cluster. I monitor it in Grafana alongside everything else. When we need more capacity, I spin up more pods instead of calling IBM."

---

### Secondary Users

#### IT Leadership / Decision Makers

While not daily users, CIOs and IT Directors drive adoption decisions. They care about:
- TCO reduction and ROI projections
- Risk mitigation during migration
- Compliance and audit continuity
- Vendor independence and long-term sustainability

#### Integration Developers

Modern application developers who need to call mainframe services or consume mainframe data. They benefit from:
- Easier local development environments
- Standard APIs instead of proprietary protocols
- Faster iteration when building integrations

---

### User Journey

#### Mainframe Developer Journey

1. **Discovery:** Hears about OpenMainframe at a SHARE conference or through mainframe community forums
2. **Evaluation:** Downloads open-source version, attempts to compile a sample COBOL program locally
3. **Proof of Concept:** Works with ops team to run a non-critical batch job on OpenMainframe in a test environment
4. **Adoption:** Integrates OpenMainframe into CI/CD pipeline for automated testing
5. **Aha Moment:** First time running full test suite locally in minutes instead of hours on shared LPAR
6. **Long-term:** Development workflow modernized; new developers onboard faster

#### Operations Team Journey

1. **Discovery:** IT leadership asks team to evaluate mainframe cost reduction options
2. **Evaluation:** Runs parallel workloads on OpenMainframe vs z/OS, comparing output and performance
3. **Proof of Concept:** Migrates low-risk batch workloads to Linux environment running OpenMainframe
4. **Adoption:** Gradually shifts production workloads, maintaining rollback capability
5. **Aha Moment:** First month-end close runs successfully on commodity hardware at 20% of previous cost
6. **Long-term:** Mainframe workloads fully integrated into modern infrastructure and observability stack

---

## Success Metrics

### User Success Metrics

#### Mainframe Developer Success
| Metric | Target | Measurement |
|--------|--------|-------------|
| Local compile time | <30 seconds for typical COBOL program | Benchmark against IBM compile times |
| Test cycle reduction | 80% faster than mainframe LPAR | Time from code change to test result |
| CI/CD integration | Developers running tests in pipelines | GitHub Actions / Jenkins adoption |
| Onboarding time | New developer productive in <1 week | Time to first successful compile |

**Key Behavior Indicators:**
- Developers choose OpenMainframe for daily development over mainframe access
- Test suites running in CI pipelines instead of batch submission
- Community contributions (bug reports, PRs, documentation)

#### Operations Team Success
| Metric | Target | Measurement |
|--------|--------|-------------|
| Cost reduction | 60-80% vs IBM licensing | Monthly infrastructure cost comparison |
| Workload compatibility | 100% output parity with z/OS | Automated regression testing |
| Migration time | Non-critical batch in <30 days | Time from evaluation to production |
| Uptime | 99.9% availability | Standard monitoring/alerting |

**Key Behavior Indicators:**
- Ops teams recommend OpenMainframe for new workload deployments
- Production workloads running without rollback to mainframe
- Integration with existing observability stack (Prometheus, Grafana)

---

### Business Objectives

#### Phase 1: Community & Credibility (Months 1-12)
- **Open source adoption:** 5,000+ GitHub stars, 500+ forks
- **Community engagement:** 50+ external contributors, active Discord/Slack
- **Technical validation:** 3+ public case studies from pilot users
- **Conference presence:** Talks at SHARE, Open Mainframe Project, KubeCon

#### Phase 2: Enterprise Traction (Months 12-24)
- **Pilot deployments:** 10+ enterprises running non-production workloads
- **Production migrations:** 3+ enterprises with production workloads on OpenMainframe
- **Commercial revenue:** First paying support/consulting customers
- **Partnership:** Relationship with cloud providers (AWS, Azure, GCP)

#### Phase 3: Market Establishment (Months 24-36)
- **Market position:** Recognized alternative to Micro Focus/LzLabs
- **Revenue growth:** Sustainable commercial business supporting development
- **Feature parity:** 80%+ coverage of common z/OS workload patterns
- **Enterprise trust:** Fortune 500 production deployments

---

### Key Performance Indicators

#### Technical KPIs
| KPI | Target | Frequency |
|-----|--------|-----------|
| Language coverage | COBOL 85/2002, PL/I, basic Assembler | Quarterly |
| JCL compatibility | 90%+ common JCL statements | Monthly |
| VSAM support | Full KSDS/ESDS/RRDS operations | Quarterly |
| Performance ratio | Within 20% of native z/OS for batch | Per release |
| Test suite pass rate | 99%+ on compatibility tests | Per commit |

#### Adoption KPIs
| KPI | Target (Year 1) | Measurement |
|-----|-----------------|-------------|
| GitHub stars | 5,000 | Monthly tracking |
| Docker pulls | 50,000 | Monthly tracking |
| Active contributors | 50+ | Quarterly |
| Enterprise evaluations | 25 | Sales pipeline |
| Production deployments | 5 | Customer success |

#### Business KPIs
| KPI | Target (Year 1) | Target (Year 2) |
|-----|-----------------|-----------------|
| Support contracts | 3 | 15 |
| Consulting engagements | 5 | 20 |
| Annual recurring revenue | $250K | $1M |
| Customer retention | 90%+ | 95%+ |

---

### Success Validation Framework

**Leading Indicators (Early Signals):**
- GitHub engagement (stars, issues, discussions)
- Conference talk acceptances
- Inbound enterprise inquiries
- Community Slack/Discord activity

**Lagging Indicators (Confirmed Success):**
- Production workloads migrated
- Revenue recognized
- Public case studies published
- Competitive wins against incumbents

**North Star Metric:**
> **Mainframe MIPS displaced** - Total estimated MIPS of workloads running on OpenMainframe instead of IBM hardware. This single metric captures user adoption, technical success, and business value creation.

---

## MVP Scope

### Core Features

#### MVP Phase: Developer-First Batch Processing

The MVP focuses on enabling developers to compile and run COBOL batch jobs locally, providing immediate value while laying the foundation for full workload migration.

**1. COBOL Compiler**
- COBOL-85 standard compliance (most common in enterprise codebases)
- COBOL-2002 intrinsic functions support
- Compile to native Linux executables via Rust backend
- Source-level error messages with line numbers
- COPY/COPYBOOK resolution from standard paths

**2. JCL Interpreter**
- Core JCL statements: JOB, EXEC, DD, PROC, INCLUDE
- Step execution with condition code handling
- DD statement types: SYSOUT, SYSIN, sequential datasets
- Symbolic parameter substitution
- Basic cataloged procedure support

**3. File System Layer**
- Sequential file I/O (QSAM - PUT/GET)
- Fixed-length and variable-length record support
- EBCDIC/ASCII conversion handling
- Linux filesystem mapping for datasets
- Basic dataset allocation (DISP, SPACE, DCB)

**4. Runtime Environment**
- COBOL runtime library (DISPLAY, ACCEPT, MOVE, etc.)
- Standard intrinsic functions
- WORKING-STORAGE initialization
- Condition code propagation between steps
- ABEND handling with dump generation

**5. Developer Tooling**
- CLI: `open-mainframe compile <source>` - compile COBOL to executable
- CLI: `open-mainframe run <jcl>` - execute JCL job
- CLI: `open-mainframe check <source>` - syntax validation
- Docker image for consistent environment
- VS Code extension for syntax highlighting (community contribution)

---

### Out of Scope for MVP

**Deferred to Post-MVP Releases:**

| Feature | Rationale | Target Release |
|---------|-----------|----------------|
| VSAM (KSDS/ESDS/RRDS) | Complex indexed file handling | v1.1 |
| CICS transaction support | Requires transaction monitor | v1.2 |
| DB2 / SQL support | Database layer complexity | v1.2 |
| PL/I compiler | Second language priority | v1.3 |
| Assembler support | Lowest usage, highest complexity | v2.0 |
| IMS/DB and IMS/TM | Specialized subsystem | v2.0 |
| RACF security emulation | Enterprise security layer | v2.0 |
| JES2/JES3 spool management | Full job entry subsystem | v2.0+ |
| GDG (Generation Data Groups) | Dataset versioning | v1.1 |
| SORT utility (DFSORT/SYNCSORT) | Common but separate tool | v1.1 |

**Explicitly Not Planned:**
- z/OS system services emulation (not needed for application code)
- Hardware emulation (we compile to native, not emulate)
- TSO/ISPF interface (developers use modern tools)
- SNA/VTAM networking (legacy protocol, use modern APIs)

---

### MVP Success Criteria

**Technical Validation:**
| Criterion | Target | Measurement |
|-----------|--------|-------------|
| COBOL-85 compliance | 95%+ of language features | NIST COBOL-85 test suite |
| Compile success rate | 90%+ on sample codebases | Open-source COBOL repos |
| Output parity | Bit-identical output vs IBM | Regression test suite |
| Performance | Within 2x of native z/OS | Benchmark suite |

**User Validation:**
| Criterion | Target | Measurement |
|-----------|--------|-------------|
| Developer adoption | 100+ active users | GitHub activity, Discord |
| Successful compiles | 1,000+ programs compiled | Telemetry (opt-in) |
| Community feedback | Net positive sentiment | GitHub issues, surveys |
| External contributions | 10+ PRs merged | GitHub metrics |

**MVP Exit Criteria (Ready for v1.1):**
- [ ] NIST COBOL-85 test suite passing at 95%+
- [ ] 3+ external users running real (non-toy) COBOL programs
- [ ] Documentation complete for all MVP features
- [ ] CI/CD pipeline with automated compatibility testing
- [ ] No critical bugs open for 2+ weeks

---

### Future Vision

#### Near-Term Roadmap (v1.x)

**v1.1 - Batch Workload Ready**
- VSAM file support (KSDS, ESDS, RRDS)
- SORT utility compatibility (DFSORT syntax)
- GDG support for dataset versioning
- Improved JCL: IDCAMS utility, more DD options
- Performance optimizations

**v1.2 - Enterprise Features**
- DB2 SQL preprocessing and PostgreSQL backend
- Basic CICS command-level support (EXEC CICS)
- PL/I compiler (initial release)
- Enhanced error diagnostics
- Migration assessment tooling

**v1.3 - Production Ready**
- Full CICS transaction support
- IMS/DB connectivity
- COBOL-2014 features
- Enterprise support offering
- Cloud marketplace listings (AWS, Azure, GCP)

#### Long-Term Vision (v2.0+)

**Platform Expansion:**
- Kubernetes-native deployment with auto-scaling
- Serverless execution model for batch jobs
- Multi-tenant SaaS offering for smaller organizations

**Ecosystem Development:**
- Migration assessment and code analysis tools
- Automated COBOL modernization suggestions
- Integration with mainframe DevOps tools (Endevor, ChangeMan)
- IDE plugins (IntelliJ, Eclipse, VS Code)

**Advanced Capabilities:**
- Assembler support for performance-critical code
- Full IMS/TM transaction support
- RACF-compatible security model
- Real-time replication from z/OS for gradual migration

**Market Expansion:**
- Managed service offering
- Partnership with system integrators
- Training and certification program
- Enterprise support tiers (24/7, dedicated engineers)

**Ultimate Goal:**
> A complete, open-source z/OS-compatible runtime that allows any enterprise to migrate their mainframe workloads to commodity infrastructure with zero code changes, achieving 80%+ cost reduction while improving developer experience and operational agility.
