# R3 Contract — Strong Single Node

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **6–11 months**
Authority change: **Interactive/session and selected host-service paths**
Phase plan: [R3 Phase Plan](phases/r3-phase-plan.md)

## Contract Outcome

R3 makes a single OpenMainframe node predictably scalable and operable. Selected
programs and sessions use typed, capability-limited host services; interactive
wait does not consume a dedicated OS thread; workload classes are isolated by
bounded worker lanes; caches and optimizations preserve fresh-source behavior;
and overload results in controlled queueing or rejection rather than resource
growth.

R3 is the mandatory scale-up gate before distribution work becomes production
authority.

R3 is also the boundary-cleanup gate for interactive execution: terminal and
session semantics become runtime services, while Ratatui/Crossterm rendering
and any wire-protocol frontend remain optional adapters.

## Dependencies

- Completed [R1 One Execution Spine](r1-one-execution-spine.md) bounded kernel,
  identity, limits, and cancellation.
- Completed/promoted [R2 One Semantic Spine](r2-one-semantic-spine.md) selectors
  or an accepted typed adapter boundary for the selected interactive paths.
- R0 performance/resource baselines and R1 coordinator metrics.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r3--strong-single-node)
- [Execution worker lanes and host services](../execution-backend.md#scheduling-and-backpressure)
- [IR runtime-provider contract](../plugin-ir-architecture.md#runtime-provider-plugin-contract)
- [Workspace authority convergence](../workspace-convergence.md#authority-convergence)

## Scope

### In scope

- Typed dataset, terminal, spool, security, database, queue, clock/random,
  telemetry, state, and program service facades needed by selected paths.
- Principal, capability, effect, condition, transaction, idempotency, and audit
  enforcement at service boundaries.
- Explicit CICS program-control and suspension outcomes.
- Run-unit frames and session actors.
- Extraction of protocol-neutral terminal, screen, field, AID, and session state
  from the current TUI frontend boundary.
- Bounded interactive worker lanes and removal of idle dedicated session
  threads for promoted selectors.
- Profiling, cache policy, pooling, allocation/cloning reduction, service
  concurrency limits, circuit breaking, and backpressure.
- Long-run, overload, cancellation, failure-isolation, and leak tests.
- Native acceleration only for measured and differentially verified subsets.

### Out of scope

- Cross-node session mobility.
- Multiple active worker nodes sharing execution state.
- Durable distributed leases and queues.
- Public third-party plugin ABI.
- Replacing every subsystem implementation or local state structure.
- Requiring the optional Ratatui/Crossterm frontend to remain a supported product
  after the neutral boundary and compatibility selectors are promoted.

## Entry Contract

R3 may enter **In Progress** when:

- R1 queues, lanes, limits, cancellation, and events pass their exit gate.
- Selected interactive selectors have compatibility fixtures and reversible
  routing.
- Service interfaces needed by selected paths have owners and permission models.
- R0/R1 provide representative mixed-load and session baselines.
- Session/run-unit state is classified at least enough to separate CPU work,
  suspended state, terminal I/O, and subsystem handles.
- Native/performance proposals include a baseline and hypothesis.

## Mandatory Deliverables

### D3.1 Host-service facade set

For services used by promoted selectors, define cloneable, scoped interfaces
that enforce:

- principal and authorization;
- granted capability/version;
- execution/run-unit/session identity;
- deadlines, cancellation, limits, and idempotency;
- tracing, metrics, audit, and SMF where applicable;
- typed conditions/status separate from infrastructure failure;
- transaction ownership and concurrency.

Plugins do not receive broad `AppState` or raw mutable subsystem locks.

### D3.2 Interactive outcome model

Implement explicit contracts and tests for:

- CALL and return frames;
- CICS LINK, XCTL, RETURN, and ABEND;
- terminal input suspension/resume;
- timer or provider suspension used by selected fixtures;
- cancellation while running and suspended;
- condition handler propagation and EIB response mapping.

### D3.3 Session actor and worker model

- Store suspended session/run-unit data outside the CPU worker stack.
- Admit active execution to a bounded interactive lane.
- Terminal wait owns no CPU worker and no dedicated idle OS thread.
- Session actors serialize only the state requiring ordering; unrelated sessions
  execute concurrently.
- Legacy `Rc<RefCell<_>>` state is removed from promoted server paths or confined
  behind a bounded affine adapter with explicit reduced capacity.
- Protocol-neutral terminal/session state is owned by a runtime-facing crate or
  service contract. Headless and z/OSMF paths do not depend on Ratatui,
  Crossterm, renderer, or keyboard-input implementations.
- The optional TUI frontend consumes the same terminal/session contract and may
  be retained or deprecated independently.

### D3.4 Vertical performance program

- Profile representative request, batch, interactive, and compiler workloads.
- Partition blocking/legacy work from latency-sensitive work.
- Add bounded content-addressed source/preprocessed/IR/executable caches with
  eviction and observability.
- Pool only resources with proven reset/isolation semantics.
- Reduce avoidable cloning, string dispatch, and repeated conversion on hot
  typed paths.
- Configure provider concurrency and connection/transaction limits.
- Add native/JIT execution only when a measured CPU-bound subset and
  differential suite justify it.

### D3.5 Operational hardening

- Structured queue, worker, session, host-call, artifact, and condition metrics.
- Readiness based on required registry/provider health.
- Long-duration mixed-load and idle-session tests.
- Leak detection for tasks, threads, permits, sessions, artifacts, and handles.
- Chaos/failure injection for provider error, panic, timeout, cancellation, and
  slow host service.
- Capacity and overload runbook.

### D3.6 Host, Protocol, and Configuration Convergence Catalog

- Publish the supported-profile provider catalog and prevent plugins/handlers
  from receiving broad `AppState` or unrelated subsystem locks.
- Establish one neutral TN3270/TN3270E protocol-state authority; networking,
  z/OSMF, and TUI use listener/translation/presentation adapters.
- Make retained DRDA a typed adapter to DB2 and security services or keep it
  unsupported pending retirement; no production selector may use mock success.
- Establish one versioned product configuration schema with TOML, YAML,
  environment, and deployment adapters and explicit provider-private namespaces.
- Classify private registries as provider internals or migrate discovery,
  identity, lifecycle, and health to the logical capability registry.

## Invariants

- Every workload lane, session mailbox, cache, output, and provider pool is
  bounded.
- An idle session consumes bounded state but no dedicated CPU worker/thread.
- Blocking work cannot execute on a latency-sensitive lane without policy.
- Cache keys include all source/dependency/plugin/target inputs needed for
  correctness.
- Pool reuse resets principal, execution, state, and output completely.
- Host services re-check sensitive authorization at the service boundary.
- A provider failure cannot corrupt unrelated run units.
- Optimization cannot bypass differential/compatibility gates.

## Exit Criteria

- Promoted idle CICS/TSO sessions require zero dedicated OS threads per session.
- Under 2x offered overload, memory remains within the configured bound and
  excess work is rejected/queued by policy.
- Request, batch, interactive, and blocking lanes demonstrate isolation under
  the accepted mixed-load scenario.
- Blocking load does not exceed the accepted interactive p95 latency isolation
  budget.
- Supported concurrent idle sessions increase by at least 2x without
  proportional thread growth.
- Cancellation/timeout/provider-failure tests release all permits, handles, and
  scoped state.
- Selected host-service paths enforce principal, permissions, conditions,
  transaction, and audit contracts.
- Source/copybook edits invalidate or bypass stale cache entries immediately.
- Cache hits materially reduce repeated compile latency in the accepted
  workload.
- Rollback restores legacy interactive routing without persistent data repair.
- Core headless/server profiles build and pass terminal/session fixtures without
  the Ratatui/Crossterm frontend; any retained TUI profile builds separately.
- Each accepted single-node profile has one protocol, configuration, and
  capability-resolution authority per selector, with no reachable mock success.

## Required Evidence

- Host-service API/security review and contract tests.
- Session/run-unit state model and actor sequence tests.
- Terminal/session boundary dependency graph and core-versus-TUI feature-matrix
  tests.
- Protocol/configuration/registry authority map and adapter conformance evidence.
- Thread/task/session measurements before and after.
- Mixed-load, 2x overload, long-run, leak, cancellation, and failure reports.
- Profiles and benchmark evidence for every accepted optimization.
- Cache correctness, eviction, bound, and source-edit tests.
- Native/interpreter differential reports if native work is promoted.
- Capacity runbook and rollback exercise.

## Stop-the-Line Conditions

R3 enters remediation when:

- A promoted plugin receives broad mutable global state instead of granted
  services.
- Idle session count still causes proportional thread growth.
- Any introduced mailbox/cache/pool can grow without policy bounds.
- Pool reuse leaks principal or mutable execution state.
- Overload causes uncontrolled memory/task growth or process instability.
- An optimization changes compatibility behavior without approval.
- Cache reuse can miss a source/dependency change.

## Rollout Contract

1. Introduce host-service adapters behind current implementations.
2. Route one stateless and one interactive selector.
3. Run session actors in shadow/affine-compatible mode where possible.
4. Canary bounded session cohorts.
5. Increase concurrency only after mixed-load and leak budgets pass.
6. Promote native/cache optimizations independently from semantic authority.
7. Deprecate or remove the optional TUI frontend only after core/headless/server
   selectors pass the extracted-boundary gate and the accepted window closes.

## Rollback Contract

- Route selected sessions/invocations to the legacy affine adapter/generation.
- Stop admission to the new session actor generation and drain/cancel safely.
- Preserve compatible session data or terminate only according to declared
  durability behavior.
- Disable cache/native selectors independently.
- Host-service facades remain usable even if an internal implementation rolls
  back.

## Handoff to R4

R4 receives:

- classified session/run-unit state and explicit safe/suspension points;
- host-service idempotency/transaction boundaries;
- bounded worker/session behavior;
- event and identity model;
- checkpoint candidates and non-migratable affine adapter inventory;
- accepted recovery and capacity objectives.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D3.1–D3.6 delivered for promoted selectors and accepted profiles.
- [ ] All exit criteria and scale-up targets passed.
- [ ] Overload, failure, and rollback exercised.
- [ ] Compatibility, operations, security, and performance owners approved.
- [ ] R4 owner accepted the handoff.
- [ ] Contract status changed to **Completed**.
