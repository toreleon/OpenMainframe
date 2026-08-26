# R1 Contract — One Execution Spine

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **6–16 weeks**
Authority change: **Representative selectors only, through reversible adapters**
Phase plan: [R1 Phase Plan](phases/r1-phase-plan.md)

## Contract Outcome

R1 establishes one versioned, bounded execution boundary for representative
request, utility, JCL, and program workloads. Existing engines remain behind
adapters, but resolution, admission, identity, limits, cancellation, events,
artifacts, execution, and outcomes become consistent and observable.

R1 proves the execution seam. It does not require a new common IR, removal of
the current CICS session thread, or durable distributed infrastructure.
It also establishes package boundaries so standalone tooling and optional
protocol/frontends do not remain accidental dependencies of the core server.

## Dependencies

- Completed [R0 Truthful System](r0-truthful-system.md) evidence pack.
- Accepted execution, artifact, outcome, diagnostic, and plugin descriptor
  contracts.
- Compatibility fixtures for every selector migrated in R1.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r1--one-execution-spine)
- [Scalable Execution Backend](../execution-backend.md)
- [IR common phase contract](../plugin-ir-architecture.md#common-compiler-phase-contract)
- [Workspace dependency and packaging rules](../workspace-convergence.md#dependency-and-packaging-rules)

## Scope

### In scope

- Dependency-light execution API.
- Immutable plugin/capability registry snapshots.
- Single-node coordinator, admission, bounded worker lanes, cancellation,
  deadlines, events, and in-memory stores.
- Adapters for `UtilityProgram`, one stateless z/OSMF service, current COBOL
  `SimpleProgram`, and selected JCL/program paths.
- `ProgramResolver`, `SourceArtifact`, `ExecutableArtifact`, `ArtifactStore`,
  compiler capability, executor capability, and `ProgramService`.
- Explicit invocation outcomes for completion, condition/failure, suspension,
  transfer, ABEND, and cancellation at the contract level.
- Source/dependency identity and content-addressed artifact metadata.
- Core-server, compatibility-adapter, CLI/tool, and test-support dependency
  tiers, with explicit feature/profile selection.

### Out of scope

- Replacing `SimpleProgram` or language ASTs.
- Full CICS run-unit/session migration.
- Removing every private registry.
- Durable queues, shared state stores, or multiple active worker nodes.
- Public Wasm/process plugin protocols.
- Native compilation as the default executor.
- Removing protocol-neutral session state with the TUI frontend or deleting an
  accepted compatibility adapter before its deprecation gate.

## Entry Contract

R1 may enter **In Progress** when:

- R0 exit evidence is accepted.
- Representative migration selectors are named and bounded.
- Legacy behavior for those selectors has compatibility fixtures.
- Execution API and artifact contracts have architecture owners.
- Rollback can route each selector to its current implementation.
- Initial queue, worker, output, and deadline limits are configured.

## Mandatory Deliverables

### D1.1 Execution API

Define versioned types for:

- execution, run-unit, parent, principal, plugin, and generation identity;
- invocation operation and typed/identified payload;
- deadlines, cancellation, limits, service class, trace, and idempotency;
- completion, suspension, transfer, ABEND, condition, cancellation, and failure;
- structured events and bounded output;
- plugin descriptors, capabilities, instance scope, and concurrency model.

The API crate must not depend on COBOL, CICS, JCL, z/OSMF, or a mutable
subsystem implementation.

### D1.2 Registry and lifecycle snapshot

- Register built-in capability descriptors.
- Reject duplicate exclusive capabilities and incompatible versions.
- Publish immutable ready snapshots atomically.
- Track plugin generation, readiness, health, and draining state even if full
  generation rollout arrives later.
- Produce capability introspection from registry data.

### D1.3 Bounded single-node coordinator

- Admission checks identity, authorization hooks, quotas, deadlines, and
  configured limits.
- Separate request, batch, interactive, blocking/legacy, and event-hook lanes.
- Every lane has bounded queue and concurrency.
- Cancellation and deadlines propagate to adapters.
- Panics/failures are contained at the invocation/run-unit boundary where
  possible.
- Events account for accepted, queued, started, suspended, completed, failed,
  cancelled, rejected, and retried states.

### D1.4 Program and artifact service

Implement the canonical path:

```text
ProgramSelector
    -> ProgramResolver
    -> SourceArtifact
    -> compiler capability
    -> ExecutableArtifact
    -> executor capability
    -> InvocationOutcome
```

Artifact identity includes source bytes, dependency hashes, canonical options,
compiler generation, contract version, and target. Source is resolved freshly
on every program load; unchanged hashes may reuse immutable artifacts.

### D1.5 Migration adapters

Provide tested adapters for at least:

- one `UtilityProgram` selector;
- one stateless z/OSMF operation;
- one JCL program/utility path;
- one COBOL batch or direct-execution path using current `SimpleProgram`.

Adapters preserve current output, condition codes, side effects, and public
entry points. Every adapter has a named removal or long-term ownership decision.

### D1.6 Observability and operations

Expose:

- execution/run-unit/plugin/generation IDs;
- queue delay, execution time, suspension time, and total elapsed time;
- queue depth, active count, rejection, timeout, cancellation, and failure;
- output bytes and configured resource limits;
- program/source/artifact/executor selection;
- parent/child invocation relationships.

### D1.7 Package and Optional-Adapter Boundaries

- Separate the reusable integration/runtime library from CLI-only subcommands
  and standalone portfolio tooling.
- Keep `open-mainframe-wiki` and the legacy assessment implementation out of the
  core z/OSMF/server build closure; package any retained tool independently.
- Make DRDA an explicit optional profile dependency. Record the mixed TUI crate
  as a time-bounded exception: its frontend profile is optional, but its neutral
  session dependency remains until the R3 extraction gate.
- Keep protocol-neutral terminal/session state available to headless and server
  paths until R3 extracts its final boundary.
- Make Gym depend on public router/execution test seams and deterministic fixture
  contracts, not CLI-only or portfolio-tooling dependencies.
- Add dependency fitness checks for all declared tiers.

### D1.8 Workspace Dependency Convergence

- Mechanically verify every suspicious internal dependency edge before removal.
- Remove verified unused, reverse-layer, and prohibited core-to-tool/test/UI/
  adapter implementation edges.
- Build accepted profile feature matrices and verify that excluded components
  are absent from dependency and package closures.
- Record every temporary edge with owner, reason, affected profiles, removal
  condition, and expiry gate.
- Decide whether each leaf crate is an independent entry point, plugin/provider,
  test/tool component, merge candidate, or retirement candidate; lack of an
  internal consumer alone is not a removal decision.

## Invariants

- Registry snapshots are immutable during an invocation.
- Artifact bytes are immutable after hashing.
- Existing public entry points remain adapters until separately deprecated.
- A queue or worker lane cannot grow without a configured bound.
- Normal program control is represented as an outcome, not an infrastructure
  crash.
- Source edits and dependency edits are visible on the next resolution.
- Compiler/executor selection is deterministic for selector, options, target,
  policy, and registry snapshot.
- A coordinator failure cannot be reported as successful program completion.
- Enabling or disabling an optional adapter cannot alter unrelated core-server
  semantics or pull standalone tooling into the runtime closure; the documented
  mixed TUI exception may expose no new frontend authority.

## Exit Criteria

- Representative REST, utility, JCL, and COBOL selectors execute through the
  coordinator with fixture parity.
- All coordinator queues and worker lanes have capacity and overload tests.
- Under 2x offered overload, excess work is bounded, queued, or rejected by
  policy; memory does not grow without bound.
- Cancellation/deadline tests release queue permits, worker permits, plugin
  instances, and bounded output resources.
- Editing a selected source or copybook changes the next resolved artifact key.
- Unchanged content may reuse an artifact without stale source behavior.
- Program/artifact/executor selection appears in events and diagnostics.
- Rollback routes every migrated selector to the legacy path without data
  repair.
- No compatibility regression is accepted for migrated selectors.
- Core-server dependency checks exclude wiki/legacy-assessment tooling, optional
  adapters are visible in profile metadata, the mixed TUI exception points to
  R3, and Gym remains usable without CLI-only dependencies.
- Accepted profiles contain zero verified-unused or prohibited dependency edges;
  all remaining exceptions are time-bounded and visible in the portfolio manifest.

## Required Evidence

- Execution API and dependency-direction review.
- Registry validation and snapshot tests.
- Adapter conformance reports against R0 fixtures.
- Overload, cancellation, deadline, and panic/failure tests.
- Source-edit and dependency-edit artifact tests.
- Event/metric examples for successful and failed invocations.
- Selector rollout and rollback exercise.
- Adapter inventory with owners and removal criteria.
- Package/profile dependency graph and feature-matrix build evidence.
- Verified-edge report, leaf-component decisions, and dependency-exception ledger.

## Stop-the-Line Conditions

R1 enters remediation when:

- A migrated selector bypasses admission, identity, limits, or events.
- An introduced channel, task set, output buffer, or registry can grow
  unbounded.
- Source edits can resolve a stale executable artifact.
- Cancellation leaks capacity or mutable state.
- Adapter behavior differs from fixtures without an approved compatibility
  decision.
- Registry selection depends on nondeterministic registration order.

## Rollout Contract

1. Register adapters without routing production selectors.
2. Run shadow invocation where side effects can be safely suppressed or mocked.
3. Canary one bounded selector or principal cohort.
4. Compare outputs, effects, latency, and events.
5. Increase coverage only while error and resource budgets hold.
6. Retain legacy routing for at least one release after authority moves.

## Rollback Contract

- Flip the selector to the legacy adapter/generation.
- Stop admitting new work to the new generation.
- Drain or cancel work according to outcome semantics.
- Preserve event/artifact evidence for diagnosis.
- Do not delete immutable artifacts as part of authority rollback.
- R1 does not introduce durable state requiring reverse migration.

## Handoff

R2 receives:

- stable compiler/executor/artifact boundaries;
- an IR executable media-type slot and executor selection mechanism;
- compatibility fixtures for the COBOL/CICS vertical slice;
- registry, diagnostics, events, and artifact identity.

R3 receives:

- bounded worker-lane and cancellation primitives;
- host-service invocation context and principal identity;
- workload/resource baselines through the new coordinator.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D1.1–D1.8 delivered.
- [ ] All exit criteria passed.
- [ ] Overload and rollback exercised.
- [ ] Compatibility, quality, operations, and security owners approved evidence.
- [ ] R2/R3 owners accepted the handoff.
- [ ] Contract status changed to **Completed**.
