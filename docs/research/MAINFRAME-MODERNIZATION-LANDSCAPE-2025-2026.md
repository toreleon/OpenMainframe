# Mainframe Modernization & Migration Landscape: Comprehensive Domain Research

**Date:** February 2026
**Scope:** Enterprise modernization tools, migration patterns, pain points, enterprise needs, and industry trends

---

## Table of Contents

1. [Market Overview](#1-market-overview)
2. [Existing Modernization Tools & Competitors](#2-existing-modernization-tools--competitors)
3. [Common Migration Patterns](#3-common-migration-patterns)
4. [Enterprise Pain Points](#4-enterprise-pain-points)
5. [What Enterprises Need from a Modernization Agent](#5-what-enterprises-need-from-a-modernization-agent)
6. [Industry Trends](#6-industry-trends)
7. [Competitive Gap Analysis](#7-competitive-gap-analysis)
8. [Sources](#8-sources)

---

## 1. Market Overview

### Market Size & Growth

| Metric | Value |
|--------|-------|
| 2025 Market Size | USD 8.2-8.4 billion |
| 2026 Estimated | USD 9.0 billion |
| 2030 Projected | USD 13.3 billion (CAGR 9.7%) |
| 2034 Projected | USD 18.4 billion (CAGR 9.5%) |
| 2035 Projected | USD 25.9 billion (CAGR 12.7%) |

### Key Statistics

- **71% of Fortune 500 companies** rely on mainframes as their systems of record.
- Mainframes support roughly **USD 13 trillion of revenue** each year.
- **96% of companies** are moving at least some workloads (average 36%) to the cloud (Kyndryl 2024 Survey).
- Large enterprises account for **75% of mainframe modernization spending**.
- North America dominates the market with **31.9% revenue share** in 2025.
- By end of 2025, an estimated **25% of experienced COBOL/z/OS workforce retired**.

---

## 2. Existing Modernization Tools & Competitors

### 2.1 Micro Focus / OpenText / Rocket Software

**Current Status:** In November 2023, OpenText (which acquired Micro Focus in 2022) divested its Application Modernization and Connectivity (AMC) division to **Rocket Software** for USD 2.275 billion. The deal closed May 1, 2024. This AMC division includes the original Micro Focus COBOL business.

**Products (Now Under Rocket Software):**

| Product | Description |
|---------|-------------|
| **Enterprise Server** | Runs COBOL/PL/I applications on distributed or cloud servers without recompilation |
| **Enterprise Developer** | Development environment for COBOL with VS Code support (v7.0) |
| **Enterprise Analyzer** | Code analysis and understanding tool for legacy applications |
| **Enterprise Test Server** | Testing environment for mainframe application workloads |
| **Enterprise Server for .NET** | Runs COBOL applications in .NET environments |

**Approach:** Lift-and-shift COBOL workloads to distributed platforms without rewriting code. Best for organizations wanting to preserve existing COBOL investments while moving off mainframe hardware.

**Strengths:**
- Mature, battle-tested products with decades of enterprise deployment
- Broad language support (COBOL, PL/I, JCL, CICS, IMS)
- Large installed base (12,500+ companies post-acquisition)
- VS Code integration in Enterprise Developer 7.0

**Weaknesses:**
- Ownership transitions (Micro Focus -> OpenText -> Rocket Software) create uncertainty
- Primarily a rehost approach; does not modernize the code itself
- Still runs COBOL, perpetuating skills dependency

---

### 2.2 LzLabs Software Defined Mainframe (SDM)

**Overview:** LzLabs SDM allows mainframe workloads to run on x86 hardware or in the cloud (primarily Azure) without recompilation or data reformatting.

**Key Capabilities:**
- **No Recompilation Required:** Binary-level compatibility; applications migrate without source code changes
- **Incremental Modernization:** Once on SDM, individual programs can be selectively modernized
- **Cloud Deployment:** Available on Microsoft Azure Marketplace
- **Cost Savings:** Customers report up to 70% reduction in OpEx through license elimination

**Notable Customers:** Swisscom, a premium car manufacturer (Azure), North American telecom companies, global automotive manufacturers.

**Strengths:**
- Zero-change migration eliminates transformation risk
- Incremental approach lets enterprises modernize at their own pace
- Strong partnership with Microsoft/Azure

**Weaknesses:**
- Still runs legacy code (just on different hardware)
- Dependency on LzLabs proprietary runtime
- Does not address skills gap (COBOL code remains)
- Limited to specific mainframe workload types

---

### 2.3 AWS Mainframe Modernization

**Products & Services:**

#### AWS Blu Age (Acquired 2021)
- French software company specializing in model-driven code transformation
- Automated COBOL/PL/I to Java transformation
- Generates cloud-native, Java-based microservices
- Uses newer web frameworks and cloud DevOps best practices

#### AWS Transform for Mainframe (GA May 2025)
**The first agentic AI service for modernizing mainframe workloads at scale.**

| Capability | Description |
|------------|-------------|
| **Assessment** | AI agents reduce assessment from months to days |
| **Code Analysis** | Automated dependency mapping and business rule extraction |
| **Refactoring** | COBOL/JCL/DB2 to Java/Postgres transformation |
| **Testing** | Automated test plan generation, data collection, test case automation |
| **Documentation** | Technical documentation auto-generation |

**Performance Metrics:**
- 1.5 million lines of code migrated per month
- 30% lower costs vs. manual approaches
- 50% reduction in technical debt
- 1,009,000 hours of manual effort saved (equivalent to 483 developer-years)

**Note:** AWS Mainframe Modernization Service (Managed Runtime Environment) is no longer open to new customers as of November 7, 2025. AWS is directing customers toward AWS Transform.

**Strengths:**
- Most advanced AI/agentic approach in the market
- End-to-end coverage (assessment through deployment)
- Backed by 19 years of AWS migration experience
- Strong ecosystem of partners (Astadia, Infosys, etc.)

**Weaknesses:**
- AWS lock-in
- Relatively new (GA May 2025); limited long-term track record
- Complex enterprise workloads may still need significant manual intervention
- Managed Runtime being deprecated signals strategy shifts

---

### 2.4 Google Cloud Mainframe Modernization

**Products & Services:**

#### Dual Run
- **Parallel processing:** Run mainframe and cloud applications simultaneously
- **Real-time comparison:** Capture and replay live production events on modernized cloud application
- **Continuous validation:** Functional, performance, and regression testing under real-world conditions
- **Regulatory compliance:** Demonstrates compliance for banking, healthcare, retail, public sector

#### Mainframe Assessment Tool (MAT)
- Assesses and reverse-engineers mainframe applications and data
- Generates comprehensive knowledge base with code explanations, dependency insights
- Automated documentation and test case generation
- Powered by Gemini AI models

#### Mainframe Rewrite
- AI-powered code transformation using Gemini models

**Key Partnerships:**
- **mLogica:** Joint mainframe modernization offering
- **Kyndryl:** Expanded partnership leveraging generative AI for mainframe modernization
- **EPAM:** GenAI-powered acceleration

**Strengths:**
- Dual Run is unique in the market for risk mitigation
- Strong AI capabilities (Gemini integration)
- Excellent for highly regulated industries
- Comprehensive assessment tooling

**Weaknesses:**
- Smaller mainframe modernization ecosystem compared to AWS
- Dual Run requires maintaining both environments during transition (cost)
- Less mature transformation tooling compared to AWS Transform

---

### 2.5 Azure Mainframe Migration Services

**Key Partners & Solutions:**

#### Avanade (Accenture + Microsoft Joint Venture)
- ISG-recognized Leader in Mainframe Modernization
- **Automated Migration Technology (AMT):** Track record of 29,000+ MIPS migrated to Azure
- Combined Accenture/Avanade/Microsoft approach for holistic modernization
- AI-powered modernization capabilities

#### Raincode
- COBOL compiler that modernizes mainframe applications to Azure-based stack
- **No code changes required** -- compiles mainframe COBOL to .NET
- **IMS to Azure:** New offering for IBM IMS/DB and IMS/TM workload migration
- Available in Azure Marketplace

#### Microsoft's AI Agents for COBOL Migration
- Research initiative using Semantic Kernel for AI-powered COBOL to Java Quarkus transformation
- Open-source sample code published on GitHub (Azure-Samples/Legacy-Modernization-Agents)
- GitHub Copilot integration for COBOL understanding and migration

#### LzLabs SDM on Azure
- Available in Azure Marketplace
- Binary-level mainframe rehosting on Azure infrastructure

**Strengths:**
- Broadest partner ecosystem for mainframe modernization
- Multiple approaches (rehost via LzLabs/TmaxSoft, replatform via Raincode, refactor via AI agents)
- Strong enterprise relationships through Avanade/Accenture
- GitHub Copilot integration for developer experience

**Weaknesses:**
- Fragmented approach (many partners, no single cohesive service like AWS Transform)
- AI agents still in research/preview stage
- Requires stitching together multiple vendor solutions

---

### 2.6 Specialized Vendors

#### Astadia
- **Focus:** End-to-end mainframe modernization including replatforming and DevOps integration
- **Approach:** Minimizes migration effort while preserving application functionality
- **Partnerships:** Works with AWS Blu Age for cloud migration
- **Recognition:** Named Leader in 2025 ISG Provider Lens Mainframe report
- Active blog and thought leadership positioning 2025 as "the year to modernize"

#### Modern Systems (Now part of Rocket Software)
- Legacy application analysis and transformation
- Automated COBOL-to-Java conversion
- Part of Rocket Software's broader modernization portfolio post-acquisition

#### TSRI (Transformation Software Research Inc.)
- **Product:** Automated COBOL-to-Java transformation platform
- **Approach:** Preserves business logic while modernizing codebases
- **Speed:** Significantly faster than manual rewriting
- **Recognition:** Named Leader in 2025 ISG Provider Lens Mainframe report

#### Heirloom Computing
- **Focus:** Cloud-native COBOL modernization
- **Approach:** COBOL-to-Java refactoring with AI-driven automation
- **Integration:** DevOps pipelines, automated testing, cloud deployment
- **Recognition:** Named Leader in 2025 ISG Provider Lens Mainframe report; selected by enterprises for high-accuracy, minimal-disruption migration

#### CloudFrame
- **Products:**
  - **CodeNavigator:** Automated COBOL to Java (SpringBoot/SpringBatch) transformation
  - **Renovate:** Automated refactoring and Java code optimization
  - **Atlas:** AI-powered knowledge graph for understanding legacy codebases (business purpose, structure, dependencies)
- **Guarantee:** 100% functional equivalence and numeric precision
- **Approach:** Modular modernization (Retain, Refactor, Reimagine)
- Available in Azure Marketplace

#### Mechanical Orchard
- **Founded:** November 2022 by Rob Mee (founder of Pivotal Labs; former CEO of Pivotal Software, acquired by VMware for USD 2.7B)
- **Funding:** Over USD 84 million (investors: GV, Emergence Capital)
- **Product:** **Imogen** -- end-to-end mainframe modernization platform (launched April 2025)
- **Approach:** Rewrites mainframe applications by focusing on system behavior via data flows rather than code translation
- **Partnerships:** Thoughtworks (first partner); available in AWS Marketplace (December 2025)
- **Differentiator:** Behavior-driven rewrite rather than line-by-line code translation

#### TmaxSoft OpenFrame
- **Product:** Mainframe rehosting solution for Linux, Unix, Docker, cloud platforms
- **Approach:** Lift-and-shift with no code changes required
- **Language Support:** COBOL, Assembler, PL/I, Easytrieve, JCL
- **Cloud Support:** AWS, Azure, Google Cloud
- **Cost Savings:** 50-75% TCO reduction claimed
- Replaces legacy CICS/IMS/JES mainframe engines

#### OpenLegacy
- **Focus:** API enablement for mainframe applications
- Wraps legacy systems into modern API-accessible services
- Supports web, mobile, and cloud application integration
- Does not transform the underlying code

---

### 2.7 Open-Source Alternatives

#### GnuCOBOL
- Free, open-source COBOL compiler
- Translates COBOL to C, then compiles to native executables
- Supports Linux, BSD, Windows, macOS
- Widely used in education and some commercial environments
- **Limitation:** Compiler only; no modernization tooling, runtime emulation, or migration automation

#### SuperBOL Studio
- Modern COBOL development environment for VS Code
- Leading French contributor to GnuCOBOL
- Provides LSP-based editing, navigation, and analysis
- **Limitation:** Development tooling only; no transformation capability

#### OpenCobolIDE
- Lightweight, cross-platform COBOL IDE
- Built on GnuCOBOL compiler
- Designed for simplicity
- **Limitation:** Minimal features compared to enterprise tools

#### Zowe (Open Mainframe Project)
- **First open-source project based on z/OS**
- Components: CLI, Web UI, VS Code Extension (Zowe Explorer), Client SDKs
- Enables modern developer interaction with z/OS mainframes
- LTS V3 released for mission-critical applications
- Supported commercially by Rocket Software and Broadcom
- **Limitation:** Works with existing mainframes; not a migration tool

#### COBOL Check (Open Mainframe Project)
- Open-source unit testing framework for COBOL
- Enables test-driven development for COBOL programs
- Tests individual paragraphs/sections of COBOL programs
- **Limitation:** Testing only; no transformation or migration

#### Open Mainframe Project (Linux Foundation)
- Umbrella organization hosting Zowe, COBOL Check, and other projects
- 10+ years of development
- Launched Mainframe Connect platform in 2025
- Research initiative: "Automating Mainframe Modernization Using AI Agents to Reduce Costs"

---

## 3. Common Migration Patterns

### 3.1 Rehost (Lift-and-Shift)

| Attribute | Detail |
|-----------|--------|
| **Description** | Move mainframe applications as-is to cloud-based runtime that mimics mainframe environment |
| **Code Changes** | None or minimal |
| **Timeline** | 6-12 months typically |
| **Risk Level** | Low |
| **Cost** | Low upfront; ongoing runtime costs |
| **Key Vendors** | TmaxSoft OpenFrame, LzLabs SDM, Micro Focus/Rocket Enterprise Server |

**Pros:**
- Fastest path off mainframe hardware
- Eliminates IBM hardware/licensing costs immediately
- Preserves all existing business logic exactly
- Minimal testing required (same code running)

**Cons:**
- Technical debt remains (COBOL code unchanged)
- Skills gap not addressed
- Dependency on rehosting vendor runtime
- Limited cloud-native benefits (no microservices, no auto-scaling)
- Often described as "moving the problem, not solving it"

---

### 3.2 Replatform (Move to Cloud-Compatible Runtime)

| Attribute | Detail |
|-----------|--------|
| **Description** | Replace mainframe OS/middleware with modern infrastructure while preserving most application logic |
| **Code Changes** | Moderate (configuration, middleware adaptation) |
| **Timeline** | 6-18 months |
| **Risk Level** | Medium-Low |
| **Key Vendors** | Raincode (.NET), AWS Managed Runtime, various |

**Pros:**
- Moves to standard platforms (Linux, .NET, containers)
- Better integration with modern DevOps/CI-CD
- Reduced vendor lock-in compared to rehost
- Applications can gradually leverage cloud services

**Cons:**
- Core code still in COBOL (compiled to different target)
- Still requires COBOL skills for maintenance
- Middleware mapping may be imperfect
- Performance characteristics differ from mainframe

---

### 3.3 Refactor (Convert to Modern Languages)

| Attribute | Detail |
|-----------|--------|
| **Description** | Transform COBOL/PL/I code to modern languages (Java, C#, Python) while preserving business logic |
| **Code Changes** | Complete language transformation |
| **Timeline** | 12-24+ months |
| **Risk Level** | High |
| **Cost** | High upfront; lowest long-term TCO |
| **Key Vendors** | AWS Transform/Blu Age, CloudFrame, TSRI, Heirloom Computing |

**Pros:**
- Eliminates legacy language dependency
- Addresses skills gap (modern developers can maintain)
- Enables true cloud-native architecture (microservices, APIs)
- Best long-term ROI according to most analysts
- Unlocks innovation velocity

**Cons:**
- Highest risk of business logic loss during transformation
- Extensive testing/validation required
- Auto-generated Java can be "COBOL in Java syntax" without careful refactoring
- Longest timeline
- Most expensive upfront investment

---

### 3.4 Replace (Rewrite with COTS/SaaS)

| Attribute | Detail |
|-----------|--------|
| **Description** | Replace legacy applications with commercial off-the-shelf software or SaaS solutions |
| **Code Changes** | Complete replacement |
| **Timeline** | 24-48 months |
| **Risk Level** | Very High |
| **Key Vendors** | SAP, Salesforce, ServiceNow, industry-specific SaaS |

**Pros:**
- Modern, supported, continuously updated software
- Best for commodity functions (HR, finance, CRM)
- Vendor manages updates, security, compliance

**Cons:**
- Custom business logic may not map to COTS features
- Data migration is complex and risky
- Vendor lock-in to SaaS provider
- Loss of competitive differentiation from custom logic
- Extremely long timelines for large-scale replacement
- Highest total cost for complex systems

---

### 3.5 Encapsulate (Wrap with APIs)

| Attribute | Detail |
|-----------|--------|
| **Description** | Keep mainframe applications running but expose functionality through modern APIs |
| **Code Changes** | None to core application; API layer added |
| **Timeline** | 3-6 months per service |
| **Risk Level** | Low |
| **Key Vendors** | OpenLegacy, Software AG, IBM z/OS Connect |

**Pros:**
- Fastest time-to-value for integration needs
- No risk to existing business logic
- Enables modern front-ends and mobile apps
- Can be a stepping stone to full modernization

**Cons:**
- Mainframe costs continue (hardware, licensing, staff)
- Adds latency through API layer
- Mainframe becomes a bottleneck for innovation
- Does not solve skills gap or cost pressure
- Technical debt continues to accumulate

---

### Migration Pattern Selection Framework

```
                    +-----------+
                    |  Assess   |
                    |  Current  |
                    |  State    |
                    +-----+-----+
                          |
            +-------------+-------------+
            |             |             |
      Low Complexity  Med Complexity  High Complexity
      Low Risk        Med Risk        High Business Value
            |             |             |
      +-----------+ +-----------+ +-----------+
      | Rehost or | | Replatform| | Refactor  |
      |Encapsulate| | or Refactor| | or Replace|
      +-----------+ +-----------+ +-----------+
```

**Most enterprises use a hybrid approach**, applying different patterns to different application portfolios based on:
- Business criticality
- Code complexity
- Regulatory requirements
- Available timeline
- Budget constraints
- Strategic importance

---

## 4. Enterprise Pain Points

### 4.1 Mainframe Costs

#### IBM Licensing Model
- IBM uses **Million Service Units (MSUs)** as the licensing metric (1 MSU ~ 8.5 MIPS)
- **Monthly License Charge (MLC):** Recurring fee based on peak MSU consumption
- A mainframe with 11,000+ MIPS costs approximately **USD 1,000-2,000 per MIPS annually**
- **Cost escalation trap:** As digital transaction volumes grow (AI processing, real-time analytics, edge computing), MIPS/MSU consumption increases, causing licensing costs to scale linearly with success -- effectively **punishing growth**
- Upgrading to newer z15/z16 hardware can increase software bills even with unchanged workload, as newer machines are assigned higher MSU-per-core values
- Third-party ISV software (BMC, Broadcom/CA, Compuware) adds additional MIPS-based licensing costs on top of IBM charges

#### Total Cost of Ownership
- Hardware lease/purchase: USD 500K-10M+
- IBM z/OS and middleware licensing: USD 1-5M+ annually
- Third-party software licensing: USD 500K-5M+ annually
- Facilities (power, cooling, floor space): USD 200K-1M annually
- Specialized staff: USD 150K-300K per mainframe engineer
- **Total: USD 5-20M+ annually for mid-to-large enterprises**

---

### 4.2 Skills Shortage

- **25% of experienced COBOL/z/OS workforce retired** by end of 2025
- Replacing them requires hiring at a **30-40% salary premium** vs. cloud-native engineers
- **39% of organizations** report skills-gap pressure, especially in application development and cross-platform architecture roles
- Average age of mainframe professionals: **55+ years**
- Universities have largely stopped teaching COBOL/mainframe courses
- Knowledge transfer is poor: decades of tribal knowledge not documented
- **Critical risk:** Single points of failure when one or two engineers understand an entire application portfolio

---

### 4.3 Integration Challenges

- Legacy protocols (SNA/LU 6.2, 3270 terminal emulation, EBCDIC encoding) incompatible with modern APIs
- Batch-oriented processing (JCL/JES) does not align with real-time, event-driven cloud architectures
- Data formats (VSAM, IMS/DB, EBCDIC packed decimal, COMP-3 fields) require complex conversion
- Mainframe security models (RACF, ACF2, Top Secret) do not map cleanly to cloud IAM
- Transaction monitors (CICS, IMS/TM) have no direct cloud equivalent
- **Middleware cost:** Legacy systems that cannot communicate natively with hybrid cloud require expensive middleware and wrappers, adding latency and cost

---

### 4.4 Testing & Validation During Migration

- Testing typically consumes **over 50% of project duration** in mainframe modernization
- Business logic embedded in COBOL spans decades; no comprehensive test suites exist for most applications
- **Functional equivalence** is extremely difficult to verify for:
  - Packed decimal arithmetic (COMP-3 precision)
  - Date/time handling (2-digit years, Julian dates)
  - EBCDIC sort orders vs. ASCII/UTF-8
  - File status codes and error handling paths
  - CICS conversation state management
- Batch job chains (JCL) with complex dependencies are hard to replicate
- Production data cannot easily be used for testing (PII/compliance)
- Lack of automated testing frameworks for legacy COBOL code
- Regression testing requires comparison of outputs across millions of records

---

### 4.5 Risk Management

- **Business continuity risk:** Mainframes run mission-critical workloads (banking transactions, insurance claims, government services)
- **Regulatory risk:** Financial services, healthcare, government have strict requirements for data handling and system availability
- **"Big bang" migration failures** are well-documented and feared (e.g., TSB Bank migration disaster in 2018)
- **Vendor risk:** Dependency on transformation vendors who may pivot, be acquired, or go bankrupt
- **Data integrity risk:** EBCDIC-to-ASCII conversion errors can corrupt financial data
- **Timeline risk:** Modernization projects routinely overrun by 2-3x original estimates
- **Cultural risk:** Mainframe teams may resist change; organizational politics between legacy and modern teams

---

## 5. What Enterprises Need from a Modernization Agent

### 5.1 Code Inventory & Assessment

**Requirements:**
- Automated scanning of all mainframe artifacts (COBOL programs, copybooks, JCL, CICS maps, DB2 schemas, VSAM definitions)
- Language version detection (COBOL-68, COBOL-74, COBOL-85, COBOL-2002, Enterprise COBOL)
- Dead code identification (unreachable paragraphs, unused copybooks, obsolete programs)
- Code quality metrics (lines of code, comment density, code duplication)
- Technology stack inventory (which middleware, databases, transaction monitors are used)

**What Exists Today:**
- AWS Transform assessment agents (months reduced to days)
- Google Cloud MAT (Mainframe Assessment Tool)
- CloudFrame Atlas (AI-powered knowledge graph)
- Micro Focus/Rocket Enterprise Analyzer

---

### 5.2 Dependency Mapping

**Requirements:**
- Program-to-program call graphs (static and dynamic calls)
- Program-to-data relationships (which programs read/write which files and databases)
- Copybook usage tracking (shared data structures across programs)
- JCL job flow analysis (PROC dependencies, dataset creation/consumption chains)
- CICS transaction routing maps
- Cross-system integration points (MQ, FTP, web services)
- Missing component detection (referenced but not found artifacts)

**Why It Matters:**
Modernizing a component without knowing its dependencies can break integrated systems, cause data inconsistencies, and require expensive rework. Dependency mapping is the foundation of safe migration planning.

---

### 5.3 Complexity Scoring

**Requirements:**
- Per-program complexity score (recommended: 0-10 scale)
- Metrics should include:
  - Cyclomatic complexity
  - Lines of code
  - Number of PERFORM branches
  - Database interactions (SQL complexity, VSAM operations)
  - External system integrations
  - Use of non-standard extensions (IBM-specific, vendor-specific)
  - Dynamic CALL usage (runtime-resolved program names)
  - REDEFINES/RENAMES complexity
  - Nested copybook depth
- Aggregate portfolio-level complexity scores
- Complexity-to-migration-strategy mapping (e.g., score > 7 = manual review required)

---

### 5.4 Migration Roadmap Generation

**Requirements:**
- Automated grouping of related components into "migration waves"
- Dependency-aware sequencing (migrate dependencies before dependents)
- Effort estimation per wave (LOC, complexity, testing requirements)
- Risk scoring per wave
- Timeline generation with milestones
- Resource requirement projections (skills, tools, environments)
- Parallel vs. sequential migration path recommendations
- Rollback strategy for each wave

---

### 5.5 Automated Code Transformation

**Requirements:**
- COBOL to Java/C#/Python transformation with:
  - Business logic preservation
  - Packed decimal arithmetic equivalence (COMP-3 fidelity)
  - EBCDIC/ASCII encoding handling
  - Copybook to class/struct mapping
  - CICS command translation to modern equivalents
  - JCL to workflow/orchestration transformation
  - DB2 SQL preservation or PostgreSQL/modern DB adaptation
  - VSAM to modern storage mapping (relational DB, key-value stores)
- **Generated code quality:** Must be maintainable, idiomatic, not "COBOL in Java"
- Incremental transformation support (transform one module at a time)
- Transformation audit trail (mapping from original to generated code)

---

### 5.6 Regression Testing Validation

**Requirements:**
- Automated test case generation from existing code
- Test data synthesis (production-representative but PII-free)
- Output comparison framework (byte-level comparison of batch outputs)
- Performance benchmarking (migrated code vs. original)
- Edge case detection and testing
- CICS screen flow testing automation
- Batch job chain end-to-end validation
- Continuous validation during incremental migration

**Industry Reference:** Google Cloud Dual Run sets the gold standard for validation -- running both environments in parallel with real production traffic and comparing outputs.

---

### 5.7 Interactive Guidance & Explanation

**Requirements:**
- Natural language explanation of legacy code ("What does this COBOL paragraph do?")
- Migration decision support ("Should I rehost or refactor this module?")
- Impact analysis ("What happens if I change this copybook?")
- Progress dashboards and reporting
- Stakeholder communication templates
- Risk alerts and recommendations
- Integration with existing project management tools
- Chat/conversational interface for non-technical stakeholders

---

## 6. Industry Trends

### 6.1 AI-Assisted Code Migration

**Current State (2025-2026):**

The single biggest shift in mainframe modernization is the move from rule-based, template-driven transformation to **AI-powered, agentic transformation**.

| Vendor | AI Approach | Status |
|--------|------------|--------|
| **AWS Transform** | Multi-agent architecture with specialized AI agents for assessment, analysis, refactoring, testing | GA May 2025 |
| **Google Cloud** | Gemini AI for assessment, code explanation, test generation | Production |
| **Microsoft/Azure** | Semantic Kernel agents for COBOL-to-Java; GitHub Copilot for COBOL | Research/Preview |
| **IBM** | Watsonx Code Assistant for Z | Production |
| **CloudFrame** | Atlas AI knowledge graph for codebase understanding | Production |

**Key Capabilities:**
- Analyze millions of lines of COBOL/PL/I in minutes
- Detect interdependencies across applications automatically
- Translate legacy components into Java, C#, or Python
- Extract and document business rules from undocumented code
- Generate test cases and validation plans

**Impact:**
- Organizations report **50%+ reduction** in modernization timelines using AI-assisted tools
- AWS claims **1.5 million LOC per month** migration velocity with agentic AI
- Testing, traditionally 50%+ of project time, is being automated with AI-generated test plans

---

### 6.2 Agent-Based Development Tools

**The Agentic AI Paradigm:**

2025 marked the emergence of **autonomous AI agents** for mainframe modernization, moving beyond "copilot" (human-in-the-loop) to "agent" (AI-autonomous-with-human-oversight).

**AWS Transform's Agent Architecture:**
- **Assessment Agent:** Analyzes codebase, generates inventory, maps dependencies
- **Analysis Agent:** Extracts business rules, creates documentation
- **Refactoring Agent:** Transforms code with state machines and state transition graphs
- **Testing Agent:** Generates test plans, data collection scripts, test automation
- **Planning Agent:** Creates migration waves and roadmaps

**Microsoft's Legacy Modernization Agents:**
- Open-source sample using Semantic Kernel
- Agents for analysis, conversion, and dependency mapping
- COBOL to Java Quarkus transformation

**Open Mainframe Project Research:**
- "Automating Mainframe Modernization Using AI Agents to Reduce Costs" -- mentorship research initiative exploring agent-based approaches

**Mechanical Orchard's Imogen:**
- Data-flow-driven rewriting (behavior-based, not code-translation-based)
- End-to-end platform with AI-assisted analysis and transformation

---

### 6.3 Copilot-Style Interfaces for Legacy Code

**GitHub Copilot:**
- Microsoft began COBOL migration experiments with GPT-4 in late 2024
- GitHub Copilot now supports COBOL comprehension and migration suggestions
- Autonomous agents can analyze codebase, detect breaking changes, suggest safe migration paths, and apply fixes
- Modernizing legacy apps can now "take days" according to Microsoft

**IBM Watsonx Code Assistant for Z:**
- Understands COBOL at enterprise scale
- Provides code explanations, transformations, and modernization suggestions
- Integrated with IBM's z/OS development environment

**Conversational Interfaces:**
- Trend toward natural language interaction with legacy codebases
- "Ask questions about your COBOL code in English"
- Bridges the knowledge gap between mainframe experts and modern developers

**Developer Experience Expectations:**
- Modern developers expect VS Code / IDE-native experiences
- Chat-based interfaces (similar to ChatGPT/Claude) for code exploration
- Inline suggestions and automated transformations
- Real-time collaboration features

---

### 6.4 Emerging Patterns

1. **Hybrid modernization:** Enterprises apply different strategies to different application portfolios rather than one-size-fits-all
2. **Incremental migration:** "Strangler fig" pattern -- gradually replacing mainframe components while both systems coexist
3. **Data-first modernization:** Migrating data layer first (VSAM/DB2 to cloud databases) before touching application code
4. **API-first approach:** Encapsulate first, then modernize -- gets immediate integration value
5. **Platform engineering:** Building internal developer platforms that abstract away migration complexity
6. **FinOps for mainframe:** Applying cloud cost management principles to mainframe licensing optimization

---

## 7. Competitive Gap Analysis

### What No One Does Well Yet

| Gap | Description | Opportunity |
|-----|-------------|-------------|
| **Open-Source End-to-End** | No open-source tool provides assessment + transformation + testing | Major opportunity for an OSS modernization platform |
| **True Code Quality** | AI-generated Java is often "COBOL in Java" -- not idiomatic | Need for quality-focused transformation that produces maintainable code |
| **JCL Modernization** | Most tools focus on COBOL; JCL/batch orchestration gets less attention | Full JCL-to-modern-workflow transformation is underserved |
| **VSAM Fidelity** | VSAM's unique characteristics (KSDS, ESDS, RRDS, spanned records, free space) are poorly mapped to modern storage | Need for semantically accurate VSAM emulation/migration |
| **Interactive Agent** | Most tools are batch-oriented; lack conversational, iterative guidance | Opportunity for copilot/agent that guides engineers through migration interactively |
| **Self-Hosted/On-Premise** | Cloud vendor tools require cloud deployment; some enterprises need on-prem first | Self-hosted modernization agent for air-gapped or regulated environments |
| **Cost Transparency** | Migration cost estimation is opaque; vendors profit from uncertainty | Open, transparent effort/cost modeling |
| **Vendor Neutrality** | Every cloud vendor pushes toward their own cloud | Vendor-neutral modernization that lets enterprises choose their target platform |
| **Small-to-Medium Enterprise** | Tools are priced for Fortune 500; SMEs with smaller mainframes are underserved | Affordable, self-service modernization tooling |
| **COBOL Dialect Coverage** | Many tools handle standard COBOL well but struggle with IBM extensions, CICS embedded SQL, obscure dialects | Comprehensive dialect support is a differentiator |

### Positioning Opportunity for OpenMainframe

Given the research above, an open-source mainframe modernization platform has significant opportunity if it provides:

1. **Accurate parsing and analysis** of COBOL, JCL, copybooks, and VSAM definitions
2. **Dependency mapping** with visualization
3. **Complexity scoring** with migration strategy recommendations
4. **Incremental, safe transformation** with comprehensive testing
5. **Interactive, agent-style guidance** rather than batch processing
6. **Vendor neutrality** -- not locked to any cloud provider
7. **Transparency** -- open algorithms, open cost models, open transformation rules
8. **Community-driven** -- benefit from collective enterprise knowledge

---

## 8. Sources

### Market Research & Analysis
- [Top Mainframe Modernization Firms 2025-2026 (IN-COM)](https://www.in-com.com/blog/top-mainframe-modernization-firms-in-2025-2026-the-leading-vendors-powering-digital-transformation/)
- [Mainframe Modernization Market (MarketsandMarkets)](https://www.marketsandmarkets.com/Market-Reports/mainframe-modernization-market-52477.html)
- [Mainframe Modernization Market Scope 2026 (InsightAce)](https://www.insightaceanalytic.com/report/mainframe-modernization-market/3306)
- [Mainframe Market Size 2026-2031 (Mordor Intelligence)](https://www.mordorintelligence.com/industry-reports/mainframe-market)
- [Best Mainframe Modernization Companies 2026 (Swimm)](https://swimm.io/learn/mainframe-modernization/best-mainframe-modernization-companies-top-8-players-in-2025)
- [10 Mainframe Modernization Tools 2025 (Swimm)](https://swimm.io/learn/application-modernization/10-mainframe-modernization-tools-to-know-in-2025)
- [Best Mainframe Modernization Solutions 2026 (DataStealth)](https://datastealth.io/blogs/best-mainframe-upgrade-and-modernization-solutions-for-2026)

### Cloud Vendors
- [AWS Mainframe Modernization Service](https://aws.amazon.com/mainframe-modernization/)
- [AWS Transform for Mainframe](https://aws.amazon.com/transform/mainframe/)
- [AWS Transform GA Announcement (InfoQ)](https://www.infoq.com/news/2025/05/aws-transform-ai-legacy-migrate/)
- [AWS re:Invent 2025 Mainframe Refresher](https://aws.amazon.com/blogs/migration-and-modernization/aws-for-mainframe-modernization-reinvent-2025-refresher/)
- [Blu Age - AWS Mainframe Migration](https://www.bluage.com/)
- [Google Cloud Mainframe Modernization](https://cloud.google.com/solutions/mainframe-modernization)
- [Google Cloud Dual Run](https://cloud.google.com/blog/products/infrastructure-modernization/dual-run-by-google-cloud-helps-mitigate-mainframe-migration-risks)
- [Google Cloud MAT Overview](https://cloud.google.com/mainframe-assessment-tool/docs/overview)
- [Azure Mainframe Migration with Raincode (Microsoft Learn)](https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/app-modernization/raincode-reference-architecture)
- [Avanade Mainframe Modernization](https://www.avanade.com/en/services/microsoft-tech/cloud-and-applications/mainframe-to-azure)

### Specialized Vendors
- [LzLabs Software Defined Mainframe](https://www.lzlabs.com/)
- [LzLabs SDM on Azure (Microsoft Learn)](https://learn.microsoft.com/en-us/azure/architecture/example-scenario/mainframe/lzlabs-software-defined-mainframe-in-azure)
- [TmaxSoft OpenFrame](https://www.tmaxsoft.com/products/openframe/)
- [CloudFrame Modernization Platform](https://cloudframe.com/)
- [Heirloom Computing (Code District)](https://codedistrict.com/blog/mainframe-modernization-tools)
- [TSRI Modernization Platform (Code District)](https://codedistrict.com/blog/mainframe-modernization-tools)
- [Astadia Mainframe Modernization](https://www.astadia.com/solutions/mainframe-modernization)
- [OpenLegacy API Enablement](https://swimm.io/learn/application-modernization/10-mainframe-modernization-tools-to-know-in-2025)
- [Mechanical Orchard Imogen Launch](https://www.thoughtworks.com/about-us/news/2025/mechanical-orchard-launch-imogen-1st-partner-thoughtworks)
- [Mechanical Orchard on AWS Marketplace](https://www.mechanical-orchard.com/insights/mechanical-orchards-mainframe-modernization-platform-now-available-in-aws-marketplace)
- [Rocket Software acquires OpenText AMC](https://www.rocketsoftware.com/en-us/news/rocket-software-closes-2275b-acquisition-opentexts-application-modernization-and-connectivity)

### AI & Agents
- [Microsoft AI Agents for COBOL Migration (Azure DevBlog)](https://devblogs.microsoft.com/all-things-azure/how-we-use-ai-agents-for-cobol-migration-and-mainframe-modernization/)
- [Azure Legacy Modernization Agents (GitHub)](https://github.com/Azure-Samples/Legacy-Modernization-Agents)
- [GitHub Copilot App Modernization (Microsoft Learn)](https://learn.microsoft.com/en-us/dotnet/core/porting/github-copilot-app-modernization/overview)
- [AWS Transform Agentic AI (AWS Blog)](https://aws.amazon.com/blogs/migration-and-modernization/accelerate-your-mainframe-modernization-journey-using-ai-agents-with-aws-transform/)
- [Using AI to Modernize Mainframes (CIO)](https://www.cio.com/article/4085184/using-ai-to-modernize-mainframes-turning-legacy-tech-into-a-strategic-advantage.html)

### Pain Points & Costs
- [Mainframe Modernization ROI Guide (EPAM)](https://www.epam.com/insights/blogs/mainframe-modernization-roi-a-cost-focused-guide-for-businesses)
- [Mainframe Cost Optimization MIPS Strategies (Royal Cyber)](https://www.royalcyber.com/blogs/mainframe/mainframe-cost-optimization-mips-reduction/)
- [IBM Mainframe Licensing Guide](https://ibmlicensingexperts.com/ibm-mainframe-licensing-z-systems-models-costs-and-negotiation-tips/)
- [IBM Z Software Pricing (IBM)](https://www.ibm.com/products/z/pricing)
- [Mainframe Workforce Signals 2026 (Planet Mainframe)](https://planetmainframe.com/2026/01/mainframe-workforce-signals-are-shifting/)
- [Sustaining and Modernizing Mainframe Systems (Pillsbury Law)](https://www.pillsburylaw.com/en/news-and-insights/modernizing-mainframes.html)

### Migration Patterns
- [AWS Migration Strategies (Prescriptive Guidance)](https://docs.aws.amazon.com/prescriptive-guidance/latest/large-migration-guide/migration-strategies.html)
- [5 R's of Mainframe Migration (Software Mining)](https://softwaremining.com/papers/comparison-of-COBOL-modernization-strategies.jsp)
- [Mainframe Migration Cheat Sheet (Kyndryl)](https://www.kyndryl.com/us/en/perspectives/articles/2024/11/mainframe-migration-tips)
- [Mainframe to Cloud Migration Guide (Superblocks)](https://www.superblocks.com/blog/mainframe-cloud-migration)

### Open Source
- [Zowe - Open Mainframe Project](https://openmainframeproject.org/projects/zowe/)
- [SuperBOL Studio](https://superbol.eu/en/)
- [COBOL Check (Open Mainframe Project)](https://www.living-mainframe.de/2025/03/25/modernizing-cobol-testing-with-cobol-check/)
- [Open Mainframe Project AI Agents Research](https://openmainframeproject.org/blog/mentorship-series-automating-mainframe-modernization-using-ai-agents-to-reduce-costs-by-a-vijay-aditya/)

### Testing
- [AWS Transform Automated Testing (AWS Blog)](https://aws.amazon.com/blogs/aws/aws-transform-for-mainframe-introduces-reimagine-capabilities-and-automated-testing-functionality/)
- [testRigor Mainframe Testing](https://testrigor.com/mainframe-testing/)
- [Mainframe Testing Tools Overview](https://testautomationtools.dev/mainframe-testing-tools/)
