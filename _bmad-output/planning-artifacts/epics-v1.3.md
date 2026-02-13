---
version: 'v1.3'
baseVersion: 'v1.2'
date: '2026-02-13'
status: 'draft'
---

# Epics - zOS-clone v1.3: Production Ready

## Epic Overview

| Epic | Name | Stories | Complexity | Phase |
|------|------|---------|------------|-------|
| 29 | Advanced CICS Commands | 8 | M | CICS Complete |
| 30 | IMS/DB Foundation | 8 | L | IMS Core |
| 31 | IMS DL/I Operations | 7 | L | IMS Complete |
| 32 | COBOL-2014 Functions | 6 | M | Language |
| 33 | COBOL-2014 Data Types | 5 | M | Language |
| 34 | XML/JSON Support | 4 | M | Language |
| 35 | Kubernetes Deployment | 6 | M | Deploy |
| 36 | Observability | 5 | S | Deploy |

**Total: 8 Epics, ~49 Stories**

---

## Epic 29: Advanced CICS Commands

**Goal:** Complete CICS implementation with queues, interval control, and synchronization.

**Crate:** `zos-cics`
**FRs:** FR-v1.3-001 to FR-v1.3-015

### Story 29.1: Temporary Storage Queue Write

As a **CICS developer**,
I want **EXEC CICS WRITEQ TS**,
So that **I can store data in temporary storage queues**.

**Acceptance Criteria:**

**Given** WRITEQ TS QUEUE('QNAME') FROM(DATA) LENGTH(LEN)
**When** executed
**Then** item added to queue, NUMITEMS updated

**Given** ITEM option specified
**When** WRITEQ TS with ITEM(n) REWRITE
**Then** existing item n is replaced

**Complexity:** M
**Supports:** FR-v1.3-001

---

### Story 29.2: Temporary Storage Queue Read

As a **CICS developer**,
I want **EXEC CICS READQ TS**,
So that **I can retrieve data from temporary storage queues**.

**Acceptance Criteria:**

**Given** READQ TS QUEUE('QNAME') INTO(DATA) ITEM(n)
**When** executed
**Then** item n copied to DATA

**Given** NEXT option
**When** READQ TS NEXT
**Then** next sequential item returned

**Given** queue not found
**When** READQ TS executed
**Then** QIDERR condition raised

**Complexity:** M
**Supports:** FR-v1.3-002

---

### Story 29.3: Temporary Storage Queue Delete

As a **CICS developer**,
I want **EXEC CICS DELETEQ TS**,
So that **I can remove temporary storage queues**.

**Acceptance Criteria:**

**Given** DELETEQ TS QUEUE('QNAME')
**When** executed
**Then** entire queue deleted

**Complexity:** S
**Supports:** FR-v1.3-003

---

### Story 29.4: Transient Data Queue Operations

As a **CICS developer**,
I want **EXEC CICS WRITEQ TD and READQ TD**,
So that **I can use transient data queues for asynchronous processing**.

**Acceptance Criteria:**

**Given** WRITEQ TD QUEUE('TDQNAME') FROM(DATA)
**When** executed
**Then** record written to TD queue

**Given** READQ TD QUEUE('TDQNAME') INTO(DATA)
**When** executed
**Then** next record read from queue

**Complexity:** M
**Supports:** FR-v1.3-004, FR-v1.3-005

---

### Story 29.5: Interval Control START

As a **CICS developer**,
I want **EXEC CICS START**,
So that **I can schedule transactions for later execution**.

**Acceptance Criteria:**

**Given** START TRANSID('TRAN') INTERVAL(001500)
**When** executed
**Then** TRAN scheduled to run in 15 minutes

**Given** START with FROM(DATA)
**When** executed
**Then** data available via RETRIEVE in started transaction

**Complexity:** M
**Supports:** FR-v1.3-006, FR-v1.3-007

---

### Story 29.6: Time Services

As a **CICS developer**,
I want **EXEC CICS ASKTIME and FORMATTIME**,
So that **I can work with date/time values**.

**Acceptance Criteria:**

**Given** ASKTIME ABSTIME(WS-TIME)
**When** executed
**Then** absolute time stored in WS-TIME

**Given** FORMATTIME ABSTIME(WS-TIME) DDMMYYYY(WS-DATE)
**When** executed
**Then** formatted date in WS-DATE

**Complexity:** M
**Supports:** FR-v1.3-009, FR-v1.3-010

---

### Story 29.7: Synchronization Commands

As a **CICS developer**,
I want **EXEC CICS SYNCPOINT and ENQ/DEQ**,
So that **I can control transaction boundaries and resource locking**.

**Acceptance Criteria:**

**Given** SYNCPOINT
**When** executed
**Then** all updates committed

**Given** SYNCPOINT ROLLBACK
**When** executed
**Then** all updates since last syncpoint rolled back

**Given** ENQ RESOURCE('RESNAME')
**When** executed
**Then** resource locked for this task

**Given** DEQ RESOURCE('RESNAME')
**When** executed
**Then** resource lock released

**Complexity:** M
**Supports:** FR-v1.3-012, FR-v1.3-015

---

### Story 29.8: Resource Inquiry

As a **CICS developer**,
I want **EXEC CICS INQUIRE and SET**,
So that **I can query and modify CICS resources**.

**Acceptance Criteria:**

**Given** INQUIRE PROGRAM('PGMNAME') STATUS(WS-STATUS)
**When** executed
**Then** program status returned

**Given** SET PROGRAM('PGMNAME') NEWCOPY
**When** executed
**Then** program refreshed from load library

**Complexity:** M
**Supports:** FR-v1.3-013, FR-v1.3-014

---

## Epic 30: IMS/DB Foundation

**Goal:** Parse IMS definitions and establish runtime infrastructure.

**Crate:** `zos-ims`
**FRs:** FR-v1.3-016 to FR-v1.3-019

### Story 30.1: DBD Parser

As a **IMS developer**,
I want **DBD (Database Definition) parsed**,
So that **database structure is understood**.

**Acceptance Criteria:**

**Given** DBD macro source
**When** parsed
**Then** database name, segments, fields extracted

**Given** SEGM statement with PARENT
**When** parsed
**Then** hierarchical relationships established

**Complexity:** L
**Supports:** FR-v1.3-017

---

### Story 30.2: Segment Definition

As a **IMS runtime**,
I want **segment definitions stored**,
So that **DL/I calls can navigate the hierarchy**.

**Acceptance Criteria:**

**Given** SEGM NAME=CUSTOMER,PARENT=0
**When** loaded
**Then** root segment defined

**Given** FIELD NAME=CUSTNO,START=1,BYTES=10,TYPE=C
**When** loaded
**Then** field accessible for SSA qualification

**Complexity:** M
**Supports:** FR-v1.3-017

---

### Story 30.3: PSB Parser

As a **IMS developer**,
I want **PSB (Program Specification Block) parsed**,
So that **program's database access is defined**.

**Acceptance Criteria:**

**Given** PSB macro source
**When** parsed
**Then** PCBs and sensitivity extracted

**Given** SENSEG statement
**When** parsed
**Then** accessible segments identified

**Complexity:** M
**Supports:** FR-v1.3-016

---

### Story 30.4: PCB Structure

As a **IMS runtime**,
I want **PCB (Program Communication Block) structures**,
So that **program state is maintained**.

**Acceptance Criteria:**

**Given** PCB defined in PSB
**When** program starts
**Then** PCB initialized with status and position

**Given** DL/I call completed
**When** PCB examined
**Then** status code reflects result

**Complexity:** M
**Supports:** FR-v1.3-018

---

### Story 30.5: PostgreSQL Schema Generation

As a **DBA**,
I want **DBD converted to PostgreSQL schema**,
So that **IMS data can be stored relationally**.

**Acceptance Criteria:**

**Given** DBD with hierarchical segments
**When** schema generated
**Then** tables created with parent references

**Given** segment fields
**When** schema generated
**Then** JSONB column stores segment data

**Complexity:** M
**Supports:** FR-v1.3-030

---

### Story 30.6: SSA Parser

As a **IMS runtime**,
I want **SSA (Segment Search Arguments) parsed**,
So that **DL/I calls can qualify segments**.

**Acceptance Criteria:**

**Given** unqualified SSA: CUSTOMER
**When** parsed
**Then** segment name identified

**Given** qualified SSA: CUSTOMER(CUSTNO =12345)
**When** parsed
**Then** field, operator, value extracted

**Given** command codes: CUSTOMER*D
**When** parsed
**Then** D (path call) code recognized

**Complexity:** L
**Supports:** FR-v1.3-026

---

### Story 30.7: EXEC DLI Scanner

As a **COBOL preprocessor**,
I want **EXEC DLI blocks identified**,
So that **they can be transformed to runtime calls**.

**Acceptance Criteria:**

**Given** EXEC DLI GU ... END-EXEC
**When** scanned
**Then** DLI call extracted with SSAs

**Given** multiple SSAs
**When** scanned
**Then** all SSAs captured in order

**Complexity:** M
**Supports:** FR-v1.3-019

---

### Story 30.8: IMS Runtime Connection

As a **IMS runtime**,
I want **PostgreSQL connection managed**,
So that **DL/I calls can access data**.

**Acceptance Criteria:**

**Given** IMS program starts
**When** first DL/I call
**Then** connection established from pool

**Given** PSB schedule requested
**When** SCHD call made
**Then** PCBs initialized, database ready

**Complexity:** M
**Supports:** FR-v1.3-029

---

## Epic 31: IMS DL/I Operations

**Goal:** Implement core DL/I database calls.

**Crate:** `zos-ims/dli`
**FRs:** FR-v1.3-019 to FR-v1.3-028

### Story 31.1: GU (Get Unique) Call

As a **IMS developer**,
I want **DL/I GU call**,
So that **I can retrieve a specific segment**.

**Acceptance Criteria:**

**Given** GU with qualified SSA
**When** executed
**Then** matching segment returned

**Given** no match found
**When** GU executed
**Then** status code GE (segment not found)

**Complexity:** M
**Supports:** FR-v1.3-019

---

### Story 31.2: GN (Get Next) Call

As a **IMS developer**,
I want **DL/I GN call**,
So that **I can sequentially retrieve segments**.

**Acceptance Criteria:**

**Given** GN with SSA
**When** executed
**Then** next segment in hierarchy returned

**Given** end of database
**When** GN executed
**Then** status code GB (end of database)

**Complexity:** M
**Supports:** FR-v1.3-020

---

### Story 31.3: GNP (Get Next within Parent) Call

As a **IMS developer**,
I want **DL/I GNP call**,
So that **I can retrieve children of current parent**.

**Acceptance Criteria:**

**Given** GNP after establishing parentage
**When** executed
**Then** next child segment returned

**Given** no more children
**When** GNP executed
**Then** status code GE

**Complexity:** M
**Supports:** FR-v1.3-021

---

### Story 31.4: Get Hold Variants

As a **IMS developer**,
I want **DL/I GHU/GHN/GHNP calls**,
So that **I can retrieve segments for update**.

**Acceptance Criteria:**

**Given** GHU (Get Hold Unique)
**When** executed
**Then** segment retrieved and locked for update

**Given** successful Get Hold
**When** followed by REPL or DLET
**Then** update/delete applies to held segment

**Complexity:** M
**Supports:** FR-v1.3-022

---

### Story 31.5: ISRT (Insert) Call

As a **IMS developer**,
I want **DL/I ISRT call**,
So that **I can add new segments**.

**Acceptance Criteria:**

**Given** ISRT with parent SSA and new segment SSA
**When** executed
**Then** segment added under parent

**Given** duplicate key
**When** ISRT executed
**Then** status code II (duplicate)

**Complexity:** M
**Supports:** FR-v1.3-023

---

### Story 31.6: DLET (Delete) Call

As a **IMS developer**,
I want **DL/I DLET call**,
So that **I can remove segments**.

**Acceptance Criteria:**

**Given** DLET after Get Hold
**When** executed
**Then** segment and dependents deleted

**Given** DLET without prior Get Hold
**When** executed
**Then** status code DJ (no prior get hold)

**Complexity:** M
**Supports:** FR-v1.3-024

---

### Story 31.7: REPL (Replace) Call

As a **IMS developer**,
I want **DL/I REPL call**,
So that **I can update segment data**.

**Acceptance Criteria:**

**Given** REPL after Get Hold
**When** executed
**Then** segment data replaced

**Given** key field changed
**When** REPL executed
**Then** status code RX (key change not allowed)

**Complexity:** M
**Supports:** FR-v1.3-025

---

## Epic 32: COBOL-2014 Functions

**Goal:** Implement modern COBOL intrinsic functions.

**Crate:** `zos-cobol`
**FRs:** FR-v1.3-031 to FR-v1.3-033, FR-v1.3-040

### Story 32.1: FUNCTION TRIM

As a **COBOL developer**,
I want **FUNCTION TRIM**,
So that **I can remove leading/trailing spaces**.

**Acceptance Criteria:**

**Given** FUNCTION TRIM(WS-STRING)
**When** evaluated
**Then** both leading and trailing spaces removed

**Given** FUNCTION TRIM(WS-STRING LEADING)
**When** evaluated
**Then** only leading spaces removed

**Given** FUNCTION TRIM(WS-STRING TRAILING)
**When** evaluated
**Then** only trailing spaces removed

**Complexity:** S
**Supports:** FR-v1.3-031

---

### Story 32.2: FUNCTION SUBSTITUTE

As a **COBOL developer**,
I want **FUNCTION SUBSTITUTE**,
So that **I can replace substrings**.

**Acceptance Criteria:**

**Given** FUNCTION SUBSTITUTE(STR "OLD" "NEW")
**When** evaluated
**Then** all occurrences of OLD replaced with NEW

**Given** multiple replacement pairs
**When** SUBSTITUTE(STR "A" "B" "C" "D")
**Then** both replacements applied

**Complexity:** M
**Supports:** FR-v1.3-032

---

### Story 32.3: FUNCTION CONCATENATE

As a **COBOL developer**,
I want **FUNCTION CONCATENATE**,
So that **I can join multiple strings**.

**Acceptance Criteria:**

**Given** FUNCTION CONCATENATE(STR1 STR2 STR3)
**When** evaluated
**Then** strings joined without trimming

**Given** FUNCTION CONCATENATE with TRIM
**When** CONCATENATE(TRIM(STR1) TRIM(STR2))
**Then** trimmed strings joined

**Complexity:** S
**Supports:** FR-v1.3-033

---

### Story 32.4: UTF-8 Support

As a **COBOL developer**,
I want **UTF-8 string handling**,
So that **I can process international text**.

**Acceptance Criteria:**

**Given** PIC N(100) USAGE NATIONAL
**When** compiled
**Then** UTF-16 storage allocated

**Given** FUNCTION DISPLAY-OF(NATIONAL-STR)
**When** evaluated
**Then** converted to UTF-8

**Complexity:** M
**Supports:** FR-v1.3-040

---

### Story 32.5: Extended Numeric Functions

As a **COBOL developer**,
I want **additional numeric functions**,
So that **I can perform calculations**.

**Acceptance Criteria:**

**Given** FUNCTION E
**When** evaluated
**Then** returns 2.71828...

**Given** FUNCTION EXP(X)
**When** evaluated
**Then** returns e^X

**Given** FUNCTION LOG10(X)
**When** evaluated
**Then** returns base-10 logarithm

**Complexity:** M
**Supports:** FR-v1.3-045

---

### Story 32.6: Floating-Point Types

As a **COBOL developer**,
I want **BINARY-LONG and BINARY-SHORT types**,
So that **I can use efficient floating-point**.

**Acceptance Criteria:**

**Given** 01 WS-FLOAT USAGE FLOAT-SHORT
**When** compiled
**Then** 32-bit IEEE 754 storage

**Given** 01 WS-DOUBLE USAGE FLOAT-LONG
**When** compiled
**Then** 64-bit IEEE 754 storage

**Complexity:** M
**Supports:** FR-v1.3-045

---

## Epic 33: COBOL-2014 Data Types

**Goal:** Implement modern COBOL data types.

**Crate:** `zos-cobol`
**FRs:** FR-v1.3-034 to FR-v1.3-039

### Story 33.1: BOOLEAN Type

As a **COBOL developer**,
I want **BOOLEAN data type**,
So that **I can use true/false values**.

**Acceptance Criteria:**

**Given** 01 WS-FLAG PIC 1 TYPE BOOLEAN
**When** compiled
**Then** single-bit storage allocated

**Given** SET WS-FLAG TO B"1"
**When** executed
**Then** flag set to true

**Given** IF WS-FLAG
**When** evaluated
**Then** true branch taken if B"1"

**Complexity:** M
**Supports:** FR-v1.3-034

---

### Story 33.2: TYPEDEF Support

As a **COBOL developer**,
I want **TYPEDEF for custom types**,
So that **I can create reusable type definitions**.

**Acceptance Criteria:**

**Given** TYPEDEF CUSTOMER-ID AS PIC X(10)
**When** defined
**Then** type available for use

**Given** 01 WS-CUST TYPE CUSTOMER-ID
**When** compiled
**Then** PIC X(10) applied

**Complexity:** M
**Supports:** FR-v1.3-035

---

### Story 33.3: FUNCTION-POINTER Type

As a **COBOL developer**,
I want **FUNCTION-POINTER type**,
So that **I can pass procedures as parameters**.

**Acceptance Criteria:**

**Given** 01 WS-PROC-PTR USAGE FUNCTION-POINTER
**When** compiled
**Then** pointer storage allocated

**Given** SET WS-PROC-PTR TO ENTRY "SUBPROG"
**When** executed
**Then** pointer set to entry point

**Complexity:** M
**Supports:** FR-v1.3-036

---

### Story 33.4: RAISE Statement

As a **COBOL developer**,
I want **RAISE statement**,
So that **I can throw exceptions**.

**Acceptance Criteria:**

**Given** RAISE EXCEPTION EC-SIZE
**When** executed
**Then** size error condition raised

**Given** DECLARATIVES with USE AFTER EXCEPTION
**When** exception raised
**Then** handler executed

**Complexity:** M
**Supports:** FR-v1.3-038

---

### Story 33.5: FREE Statement

As a **COBOL developer**,
I want **FREE statement**,
So that **I can release dynamic memory**.

**Acceptance Criteria:**

**Given** FREE ADDRESS OF WS-PTR
**When** executed
**Then** memory deallocated

**Given** FREE of unallocated pointer
**When** executed
**Then** EC-STORAGE-NOT-ALLOC raised

**Complexity:** S
**Supports:** FR-v1.3-039

---

## Epic 34: XML/JSON Support

**Goal:** Implement XML and JSON processing statements.

**Crate:** `zos-cobol`
**FRs:** FR-v1.3-041 to FR-v1.3-044

### Story 34.1: XML GENERATE

As a **COBOL developer**,
I want **XML GENERATE statement**,
So that **I can create XML from COBOL data**.

**Acceptance Criteria:**

**Given** XML GENERATE WS-XML FROM WS-DATA
**When** executed
**Then** XML document created from structure

**Given** WITH XML-DECLARATION
**When** specified
**Then** <?xml version="1.0"?> prepended

**Complexity:** M
**Supports:** FR-v1.3-041

---

### Story 34.2: XML PARSE

As a **COBOL developer**,
I want **XML PARSE statement**,
So that **I can process XML documents**.

**Acceptance Criteria:**

**Given** XML PARSE WS-XML PROCESSING PROCEDURE HANDLE-XML
**When** executed
**Then** parser calls procedure for each event

**Given** START-OF-ELEMENT event
**When** in procedure
**Then** XML-EVENT and XML-TEXT available

**Complexity:** L
**Supports:** FR-v1.3-042

---

### Story 34.3: JSON GENERATE

As a **COBOL developer**,
I want **JSON GENERATE statement**,
So that **I can create JSON from COBOL data**.

**Acceptance Criteria:**

**Given** JSON GENERATE WS-JSON FROM WS-DATA
**When** executed
**Then** JSON document created from structure

**Given** NAME OF field IS 'jsonName'
**When** generated
**Then** custom JSON property name used

**Complexity:** M
**Supports:** FR-v1.3-043

---

### Story 34.4: JSON PARSE

As a **COBOL developer**,
I want **JSON PARSE statement**,
So that **I can process JSON documents**.

**Acceptance Criteria:**

**Given** JSON PARSE WS-JSON INTO WS-DATA
**When** executed
**Then** JSON values mapped to COBOL fields

**Given** WITH DETAIL
**When** specified
**Then** JSON-STATUS set on errors

**Complexity:** M
**Supports:** FR-v1.3-044

---

## Epic 35: Kubernetes Deployment

**Goal:** Production-ready Kubernetes deployment.

**Crate:** `zos-deploy`
**FRs:** FR-v1.3-046 to FR-v1.3-049, FR-v1.3-055

### Story 35.1: Kubernetes Manifests

As a **DevOps engineer**,
I want **Kubernetes deployment manifests**,
So that **I can deploy zOS-clone to clusters**.

**Acceptance Criteria:**

**Given** kubectl apply -f deployment.yaml
**When** executed
**Then** pods created with correct resources

**Given** resource limits specified
**When** pods running
**Then** CPU/memory enforced

**Complexity:** M
**Supports:** FR-v1.3-046

---

### Story 35.2: Helm Chart

As a **DevOps engineer**,
I want **Helm chart for zOS-clone**,
So that **I can customize deployments**.

**Acceptance Criteria:**

**Given** helm install zos-clone ./helm/zos-clone
**When** executed
**Then** all resources created

**Given** values.yaml overrides
**When** helm install --set replicas=5
**Then** 5 replicas created

**Complexity:** M
**Supports:** FR-v1.3-047

---

### Story 35.3: Docker Optimization

As a **DevOps engineer**,
I want **optimized Docker images**,
So that **deployment is fast and secure**.

**Acceptance Criteria:**

**Given** multi-stage Dockerfile
**When** built
**Then** final image <100MB

**Given** non-root user
**When** container runs
**Then** process runs as unprivileged user

**Complexity:** M
**Supports:** FR-v1.3-048

---

### Story 35.4: Health Checks

As a **Kubernetes**,
I want **health check endpoints**,
So that **I can manage pod lifecycle**.

**Acceptance Criteria:**

**Given** GET /health
**When** pod healthy
**Then** 200 OK returned

**Given** GET /ready
**When** database connected
**Then** 200 OK returned

**Given** database unavailable
**When** GET /ready
**Then** 503 Service Unavailable

**Complexity:** S
**Supports:** FR-v1.3-049

---

### Story 35.5: Environment Configuration

As a **DevOps engineer**,
I want **environment variable configuration**,
So that **I can configure without rebuilding**.

**Acceptance Criteria:**

**Given** ZOS_DB_URL environment variable
**When** application starts
**Then** uses specified database

**Given** ZOS_LOG_LEVEL=debug
**When** application runs
**Then** debug logging enabled

**Complexity:** S
**Supports:** FR-v1.3-055

---

### Story 35.6: ConfigMap and Secrets

As a **DevOps engineer**,
I want **Kubernetes ConfigMaps and Secrets**,
So that **sensitive data is managed properly**.

**Acceptance Criteria:**

**Given** ConfigMap with application config
**When** mounted
**Then** config file available to application

**Given** Secret with database password
**When** mounted as environment
**Then** password available securely

**Complexity:** S
**Supports:** FR-v1.3-055

---

## Epic 36: Observability

**Goal:** Production monitoring and tracing.

**Crate:** `zos-deploy`
**FRs:** FR-v1.3-050, FR-v1.3-051

### Story 36.1: Prometheus Metrics

As a **SRE**,
I want **Prometheus metrics exposed**,
So that **I can monitor system health**.

**Acceptance Criteria:**

**Given** GET /metrics
**When** scraped
**Then** Prometheus format returned

**Given** COBOL program executed
**When** metrics examined
**Then** execution count and duration recorded

**Complexity:** M
**Supports:** FR-v1.3-050

---

### Story 36.2: Custom Metrics

As a **SRE**,
I want **application-specific metrics**,
So that **I can monitor business operations**.

**Acceptance Criteria:**

**Given** CICS transaction completed
**When** metrics examined
**Then** cics_transactions_total incremented

**Given** IMS DL/I call made
**When** metrics examined
**Then** ims_dli_calls_total incremented

**Complexity:** M
**Supports:** FR-v1.3-050

---

### Story 36.3: OpenTelemetry Tracing

As a **SRE**,
I want **distributed tracing**,
So that **I can trace requests across services**.

**Acceptance Criteria:**

**Given** OTEL_EXPORTER_OTLP_ENDPOINT configured
**When** request processed
**Then** traces exported to collector

**Given** database query executed
**When** trace examined
**Then** DB span with query visible

**Complexity:** M
**Supports:** FR-v1.3-051

---

### Story 36.4: Structured Logging

As a **SRE**,
I want **JSON-formatted logs**,
So that **logs can be aggregated and searched**.

**Acceptance Criteria:**

**Given** ZOS_LOG_FORMAT=json
**When** log emitted
**Then** JSON structure with timestamp, level, message

**Given** trace context active
**When** log emitted
**Then** trace_id and span_id included

**Complexity:** S
**Supports:** FR-v1.3-051

---

### Story 36.5: Grafana Dashboards

As a **SRE**,
I want **pre-built Grafana dashboards**,
So that **I can visualize metrics immediately**.

**Acceptance Criteria:**

**Given** dashboard JSON imported
**When** viewed in Grafana
**Then** COBOL, CICS, IMS, DB panels visible

**Given** alert rules included
**When** thresholds exceeded
**Then** alerts triggered

**Complexity:** M
**Supports:** FR-v1.3-050, FR-v1.3-051

---

## Requirements Traceability

### FR to Story Mapping

| Requirement | Story | Status |
|-------------|-------|--------|
| FR-v1.3-001 | 29.1 | Planned |
| FR-v1.3-002 | 29.2 | Planned |
| FR-v1.3-003 | 29.3 | Planned |
| FR-v1.3-004 | 29.4 | Planned |
| FR-v1.3-005 | 29.4 | Planned |
| FR-v1.3-006 | 29.5 | Planned |
| FR-v1.3-007 | 29.5 | Planned |
| FR-v1.3-008 | 29.5 | Planned |
| FR-v1.3-009 | 29.6 | Planned |
| FR-v1.3-010 | 29.6 | Planned |
| FR-v1.3-011 | 29.7 | Planned |
| FR-v1.3-012 | 29.7 | Planned |
| FR-v1.3-013 | 29.8 | Planned |
| FR-v1.3-014 | 29.8 | Planned |
| FR-v1.3-015 | 29.7 | Planned |
| FR-v1.3-016 | 30.3 | Planned |
| FR-v1.3-017 | 30.1, 30.2 | Planned |
| FR-v1.3-018 | 30.4 | Planned |
| FR-v1.3-019 | 30.7, 31.1 | Planned |
| FR-v1.3-020 | 31.2 | Planned |
| FR-v1.3-021 | 31.3 | Planned |
| FR-v1.3-022 | 31.4 | Planned |
| FR-v1.3-023 | 31.5 | Planned |
| FR-v1.3-024 | 31.6 | Planned |
| FR-v1.3-025 | 31.7 | Planned |
| FR-v1.3-026 | 30.6 | Planned |
| FR-v1.3-027 | 30.6 | Planned |
| FR-v1.3-028 | 30.4 | Planned |
| FR-v1.3-029 | 30.8 | Planned |
| FR-v1.3-030 | 30.5 | Planned |
| FR-v1.3-031 | 32.1 | Planned |
| FR-v1.3-032 | 32.2 | Planned |
| FR-v1.3-033 | 32.3 | Planned |
| FR-v1.3-034 | 33.1 | Planned |
| FR-v1.3-035 | 33.2 | Planned |
| FR-v1.3-036 | 33.3 | Planned |
| FR-v1.3-037 | - | Deferred to v2.0 |
| FR-v1.3-038 | 33.4 | Planned |
| FR-v1.3-039 | 33.5 | Planned |
| FR-v1.3-040 | 32.4 | Planned |
| FR-v1.3-041 | 34.1 | Planned |
| FR-v1.3-042 | 34.2 | Planned |
| FR-v1.3-043 | 34.3 | Planned |
| FR-v1.3-044 | 34.4 | Planned |
| FR-v1.3-045 | 32.5, 32.6 | Planned |
| FR-v1.3-046 | 35.1 | Planned |
| FR-v1.3-047 | 35.2 | Planned |
| FR-v1.3-048 | 35.3 | Planned |
| FR-v1.3-049 | 35.4 | Planned |
| FR-v1.3-050 | 36.1, 36.2 | Planned |
| FR-v1.3-051 | 36.3, 36.4 | Planned |
| FR-v1.3-052 | - | Deferred |
| FR-v1.3-053 | - | Deferred |
| FR-v1.3-054 | - | Deferred |
| FR-v1.3-055 | 35.5, 35.6 | Planned |
