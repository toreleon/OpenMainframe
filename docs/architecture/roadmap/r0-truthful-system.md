# R0 Contract — Truthful System

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **0–6 weeks**
Authority change: **None**
Phase plan: [R0 Phase Plan](phases/r0-phase-plan.md)

## Contract Outcome

R0 makes the current OpenMainframe behavior safe to change. At completion, the
selected COBOL, CICS, JCL, symbolic, and z/OSMF paths have deterministic
compatibility fixtures, explicit semantic coverage, and accepted performance
and resource baselines. Unsupported executable semantics in scope cannot
disappear silently. The core and optional support profiles also have an accepted
component disposition, so later cleanup does not silently change product scope.

R0 is a characterization and safety wave. It does not introduce a new execution
authority, IR authority, scheduler, durable store, or plugin loading mechanism.

## Dependencies

- Current workspace source and test suites.
- Existing CardDemo, JCL, z/OSMF, Zowe, and symbolic test scripts/fixtures.
- Current behavior is treated as evidence, not automatically as the desired
  compatibility contract. Incorrect or ambiguous behavior is classified.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r0--truthful-system)
- [Execution Backend current-state findings](../execution-backend.md#current-state-findings)
- [IR current-state findings](../plugin-ir-architecture.md#current-state-findings)
- [Workspace convergence baseline](../workspace-convergence.md#point-in-time-audit-baseline)

## Scope

### In scope

- COBOL statement support inventory across lexer/parser, semantic analysis,
  concrete lowering/execution, symbolic execution, and LLVM.
- Selected CardDemo online transaction flows.
- Selected JCL batch, utility, DD, condition, and spool flows.
- Decimal, PIC, EBCDIC, REDEFINES, OCCURS, group layout, and call parameter
  behavior.
- CICS terminal, file, condition, LINK, XCTL, RETURN, and ABEND behavior used by
  selected fixtures.
- z/OSMF/Zowe compatibility smoke paths used as release indicators.
- Thread, task, memory, queue, throughput, latency, cancellation, malformed
  input, and overload baselines.
- Minimal execution, artifact, outcome, diagnostic, plugin, IR, legality, and
  phase contract decisions needed by R1/R2.
- Public selectors, configuration defaults, dependency closure, state ownership,
  and compatibility evidence for DRDA, TUI/session, assessment, wiki tooling,
  and the Gym harness.
- All-crate portfolio/profile/ownership classification and point-in-time
  dependency, authority, mock/stub, public-surface, and test-maturity baselines.

### Out of scope

- Replacing the current COBOL interpreter or `SimpleProgram`.
- Introducing a production execution coordinator.
- Removing the dedicated CICS session thread.
- Completing LLVM procedure generation.
- Distributed queues, stores, or workers.
- Public third-party plugin interfaces.
- Deleting a component that still owns protocol-neutral runtime state, public
  behavior, or the only accepted compatibility oracle.

## Entry Contract

R0 may enter **In Progress** when:

- The workspace revision and required toolchain are recorded.
- Focused test commands for COBOL, CICS, JCL, symbolic, and z/OSMF can be run or
  their known blockers are documented.
- Representative CardDemo source, copybooks, BMS maps, and data fixtures are
  available.
- Owners are assigned for compatibility, quality, and architecture decisions.
- No concurrent migration is allowed to move authority in the selected paths
  without being included in the baseline.

## Mandatory Deliverables

### D0.1 Semantic coverage inventory

Produce a machine-readable matrix with at least:

```text
language
construct or operation
parse support
semantic support
concrete support
symbolic support
native support
unsupported behavior
source evidence
test evidence
owner
```

Every COBOL statement variant and every CICS operation used by the selected
CardDemo flow is classified as `implemented`, `partial`, `unsupported`,
`intentionally ignored`, or `unknown`.

### D0.2 Explicit unsupported diagnostics

- Replace selected production `_ => None`, silent omission, or unsupported
  `Nop` behavior with structured diagnostics.
- Scanner errors and semantic errors become executable-compilation gates for the
  selected paths.
- Analyze-mode recovery remains possible only when it is labeled incomplete.
- Diagnostic codes are stable, source located, and phase identified.

### D0.3 Compatibility fixture set

Create or formalize golden fixtures for:

- CardDemo terminal screens and AID/input cycles.
- EIBRESP/EIBRESP2 and CICS condition behavior.
- File reads/writes/browse and record decomposition.
- CALL parameter modes, contained programs, LINK/XCTL/RETURN, and ABEND.
- JCL step order, condition codes, DD bindings, utilities, and spool output.
- Decimal rounding/overflow, PIC editing, encoding/collation, REDEFINES, OCCURS,
  and group layouts.
- Symbolic branch/path results including bounded and unsupported outcomes.
- z/OSMF/Zowe response compatibility for selected endpoints.

Fixture normalization must not remove observable values needed to detect
regressions.

### D0.4 Performance and resource baseline

Record for representative workloads:

- p50/p95/p99 latency and throughput;
- compile and transaction elapsed time;
- CPU utilization and blocking time;
- resident/high-water memory;
- thread/task counts for active and idle sessions;
- queue depth and rejection behavior;
- output and artifact sizes;
- cancellation and shutdown behavior;
- behavior under at least 2x the current accepted load, even if the result is a
  documented failure.

### D0.5 Contract decision pack

Approve or explicitly defer:

- execution and run-unit identity;
- invocation context and principal propagation;
- explicit outcome and failure categories;
- source and executable artifact identity;
- plugin/capability descriptor minimum fields;
- diagnostic and phase envelope minimum fields;
- IR operation identity, types, effects, legality, and provenance;
- compatibility ownership and deprecation authority.

### D0.6 Support Profile and Component Decision Pack

For DRDA, TUI/session, assessment, wiki tooling, and Gym:

- classify the component as core, compatibility adapter, tooling, test
  infrastructure, or legacy implementation;
- name accepted deployment profiles and public selectors;
- record owner, fixtures, dependency closure, state authority, and configuration
  defaults;
- approve retain, split, replace, deprecate, or remove disposition;
- define replacement boundary, compatibility window, and measurable removal or
  long-term retention gate.

The initial target is DRDA as an explicit default-off optional adapter,
protocol-neutral session state extracted from the TUI frontend in R3, standalone
assessment replaced through R2 analysis parity, wiki tooling removed from the
runtime closure, and Gym retained as deterministic test infrastructure.

### D0.7 Workspace Fitness Baseline

For every workspace crate and public selector:

- record owner, entry point/consumer, product profile, support state, public
  surface, dependency closure, state/protocol/configuration authority, test
  maturity, and target boundary;
- inventory mock, stub, unconditional-success, duplicated-authority, and
  migration-adapter paths;
- distinguish an independently packaged leaf/plugin candidate from genuinely
  unused code instead of inferring removal from incoming dependency count;
- publish initial core-server, compatibility, language-pack, subsystem-pack,
  analysis/test, and operations profile manifests; and
- assign every suspected unused or reverse-layer dependency edge to R1
  verification with an owner and evidence method.

## Invariants

- R0 does not move production authority.
- Characterization tests distinguish current behavior from desired behavior.
- A newly discovered semantic gap is recorded; it is not normalized away to
  make the baseline green.
- Tests do not depend on uncontrolled clock, randomness, or environment without
  recording those dependencies.
- Every baseline result is tied to source revision and configuration.
- No new unbounded channel, thread, queue, or fixture output is introduced.
- No component is called unsupported merely because it is excluded from the
  core profile; optional-profile and deprecation status remain explicit.

## Exit Criteria

All criteria are mandatory:

- 100% of COBOL statement variants are present in the semantic coverage matrix.
- 100% of CICS operations exercised by selected CardDemo fixtures are
  classified.
- Selected executable paths contain zero known silent unsupported-operation
  omissions.
- Scanner/semantic error gate tests demonstrate that invalid selected programs
  cannot publish an executable result.
- Golden fixtures pass repeatedly in the agreed environment or have approved,
  deterministic expected failures.
- Performance/resource baseline reports are reproducible and stored as CI or
  release artifacts.
- Minimal R1 execution contracts and R2 IR contracts are accepted or have named
  blocking decisions.
- Every public behavior fixture and contract has an owner.
- D0.6 assigns every named component a profile, owner, dependency/state boundary,
  and approved transition gate.
- D0.7 classifies every workspace crate and public selector and leaves no
  unowned mock/stub, dependency, authority, or retirement finding.

## Required Evidence

- Semantic coverage matrix and its generation/check procedure.
- Fixture inventory and test reports.
- Before/after examples for explicit unsupported diagnostics.
- Baseline performance/resource report with environment description.
- Contract/ADR decision links.
- Known-gap register with severity, owner, selector, and target wave.
- Support-profile/component matrix and current dependency-closure report.
- All-crate portfolio, authority, mock/stub, test-maturity, and profile manifests.
- Confirmation that no production authority changed.

## Stop-the-Line Conditions

R0 enters remediation when:

- Selected tests cannot distinguish success from silently omitted behavior.
- The baseline depends on unavailable or unversioned fixtures.
- A concurrent change moves execution or semantic authority without updated
  characterization.
- Performance results cannot be tied to a reproducible configuration.
- A security/authorization regression is discovered in a selected path.

## Rollout and Rollback

R0 changes are primarily diagnostics, tests, and documentation. Diagnostic
enforcement may be enabled first in CI/shadow mode, then in selected executable
paths. Rollback may relax enforcement only through an explicit selector with a
known-gap entry; it may not restore silent success as an undocumented default.

## Handoff to R1 and R2

R1 receives:

- accepted execution/artifact/outcome contracts;
- workload and overload baselines;
- compatibility fixtures for adapter parity;
- registry/capability inventory.

R2 receives:

- accepted IR/diagnostic/legality minimum contracts;
- COBOL and CICS semantic coverage matrices;
- vertical-slice fixture candidates;
- known semantic gaps that must not be hidden by lowering.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D0.1–D0.7 delivered.
- [ ] All exit criteria passed.
- [ ] Stop-the-line findings resolved or scope removed explicitly.
- [ ] Evidence pack reviewed independently.
- [ ] R1 and R2 owners accepted the handoff.
- [ ] Contract status changed to **Completed**.
