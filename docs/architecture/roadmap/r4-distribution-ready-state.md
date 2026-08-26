# R4 Contract — Distribution-Ready State

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **9–15 months**
Authority change: **Durable interfaces become authoritative on one active worker pool**
Phase plan: [R4 Phase Plan](phases/r4-phase-plan.md)

## Contract Outcome

R4 removes node-local authority assumptions from workloads intended to move or
recover. Execution, session, checkpoint, artifact, event, plugin-generation,
and work ownership have versioned durable contracts. Work claims use leases and
attempt identity; mutating effects use idempotency, transactions, or explicit
non-retryable policy. Gateways and coordinators no longer own irreplaceable
session/execution state.

R4 deliberately proves the durable interfaces with one active worker pool
before enabling horizontal execution.
The proof applies to a named deployment profile; an unselected optional DRDA,
3270, TUI, or other compatibility adapter neither blocks the wave nor inherits
a durability claim.

## Dependencies

- Completed [R3 Strong Single Node](r3-strong-single-node.md) boundedness,
  session actors, host-service boundaries, and state classification.
- Stable R1 execution identity/events and R2 artifact/plugin generation
  identity.
- Accepted durability, recovery, and consistency objectives for selected
  workload classes.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r4--distribution-ready-state)
- [Execution state and durability](../execution-backend.md#state-checkpointing-and-durability)
- [Artifact and cache contracts](../plugin-ir-architecture.md#artifacts-caching-and-reproducibility)
- [Workspace product profiles](../workspace-convergence.md#product-profiles)

## Scope

### In scope

- Formal state classification by scope and mobility.
- Store interfaces and one production-capable durable implementation for
  execution records/events, durable work, sessions/checkpoints, artifacts, and
  plugin generation metadata as required by promoted workloads.
- Versioned checkpoint/session schemas and compatibility policy.
- Lease claims, heartbeat, expiry, attempt, ownership, safe points, and
  dead-letter behavior.
- Idempotency/effect sequencing for mutating host services.
- Stateless/reconstructible gateway and coordinator behavior.
- Gateway migration for every interface selected in the promoted deployment
  profile, including optional compatibility adapters only when explicitly named.
- Restart, worker-kill, coordinator-kill, slow-store, and recovery tests.
- Operating the durable path on a single active worker pool.

### Out of scope

- Multiple active worker nodes processing the same global queue.
- Cross-region active-active consistency.
- Exactly-once execution claims.
- Migrating explicitly node-affine legacy adapters without safe checkpoints.
- Stable public third-party plugin ABI.
- Making an unselected optional compatibility adapter durable merely because it
  exists in the workspace.

## Entry Contract

R4 may enter **In Progress** when:

- R3 proves bounded single-node behavior and identifies safe suspension points.
- Every selected state object has a scope, owner, size/lifetime bound, and
  durability requirement.
- Mutating host services document retry/idempotency/transaction behavior.
- Recovery time and data-loss objectives are defined per workload class.
- A durable technology choice is treated as an implementation of store
  contracts, not exposed directly to plugins.
- Migration/rollback of existing in-memory state is planned.

## Mandatory Deliverables

### D4.1 State catalog

Catalog at least:

```text
state type
scope: invocation | run-unit | session | region | singleton | artifact
authoritative owner
mutable or immutable
size and lifetime bounds
durability level
checkpoint/safe points
retry behavior
schema version
security classification
migration status
```

State not safe to move is marked node-affine with explicit capacity and
availability consequences.

### D4.2 Durable store contracts

Define versioned interfaces for:

- execution records and lifecycle events;
- durable work queue and dead-letter records;
- session and run-unit checkpoints;
- immutable source/executable artifacts;
- plugin generations, compatibility, and draining metadata;
- idempotency/effect records required for promoted services.

Provide bounded in-memory implementations for development and production-capable
durable implementations for the promoted deployment profile.

### D4.3 Lease and ownership protocol

Every movable work item contains:

- execution/run-unit ID;
- immutable input/artifact references;
- required capability, target, and plugin generation constraints;
- owner lease ID, worker ID, heartbeat, and expiry;
- monotonically increasing attempt;
- deadline and cancellation state;
- checkpoint/restart policy;
- idempotency/effect sequence;
- terminal or dead-letter outcome.

Lease expiry does not itself prove an external effect did not occur.

### D4.4 Checkpoint and session schemas

- Define safe points for running, suspended, transferring, and completing work.
- Version checkpoint envelopes and plugin-specific state.
- Declare compatible restore version ranges and upgrade/rejection behavior.
- Encrypt or protect sensitive state according to classification.
- Bound checkpoint size and creation/restore time.
- Validate artifact and plugin generation requirements before restore.

### D4.5 Idempotent and transactional effects

For each promoted mutating service operation:

- define idempotency-key use or transaction boundary;
- define replay, unknown-outcome, and non-retryable behavior;
- record effect sequence when needed;
- test crash before, during, and after the external effect;
- prevent infrastructure retry from becoming silent duplicate business action.

### D4.6 Stateless gateway/coordinator migration

- Remove authoritative promoted execution/session data from handler-local maps.
- Gateways authenticate and translate, then use coordinator/store contracts.
- Coordinator caches are bounded and reconstructible.
- Restarting gateways/coordinator does not lose authoritative queued/suspended
  work.
- Sticky routing is optional for movable workloads.
- Selected DRDA/3270/other compatibility listeners translate through the same
  service/coordinator contracts; absent adapters are not started or advertised.

### D4.7 Recovery and operations

- Recovery controller for expired claims and orphaned work.
- Operator-visible retry, poison, dead-letter, cancellation, and manual recovery
  state.
- Store/queue health, latency, saturation, lease, checkpoint, and recovery
  metrics.
- Backup/restore and schema-migration procedures for durable metadata.

### D4.8 Supported-Profile State Coverage

- Map every capability advertised by the promoted distribution-ready profile to
  its authoritative state class, owner, durability level, restore/restart
  behavior, and non-movable limitation.
- Prove excluded optional packs are absent or explicitly unsupported rather than
  hiding local authority behind a stateless gateway claim.
- Carry unselected capabilities forward only with an owned profile/adoption or
  retirement decision; they gain no durability claim from workspace membership.

## Invariants

- A control-plane cache is never authoritative.
- Every durable mutation is tied to execution identity and schema version.
- Every movable work item has exactly one valid owner lease at a time, while
  delivery remains at least once.
- Retry does not imply an external effect is safe; service policy decides.
- Immutable artifacts are content addressed and never modified in place.
- Checkpoint restore validates plugin/artifact/host-interface compatibility.
- Durable queues and stores have quotas, retention, and backpressure.
- Node-affine workloads are declared, observable, and capacity limited.
- Security identity and permissions survive persistence and restore.

## Exit Criteria

- All selected movable state appears in the approved state catalog.
- No promoted gateway/coordinator local map is the sole authority for execution
  or suspended session state.
- Every interface advertised by the accepted deployment profile passes the
  gateway restart/state-authority gate; excluded optional adapters remain
  explicitly unsupported for that profile.
- A single active worker pool runs entirely through durable work/state/artifact
  interfaces.
- Killing worker/coordinator during queued, running, suspended, checkpointing,
  and completing states produces the declared recovery/dead-letter outcome.
- Recovery meets the accepted workload-specific window.
- Mutating service crash/retry tests show zero unaccounted duplicate effects.
- Checkpoint compatibility, upgrade, incompatible rejection, size, and restore
  limits pass.
- Store/queue saturation produces visible backpressure and bounded application
  memory.
- Backup/restore and rollback exercises complete successfully.
- Every advertised capability appears in the accepted state/profile matrix with
  tested ownership and failure semantics.

## Required Evidence

- Approved state catalog.
- Store and schema API/version review.
- Lease state-machine and model/property tests.
- Crash-point/idempotency reports for mutating services.
- Worker/coordinator kill and recovery reports.
- Checkpoint compatibility matrix and restore benchmarks.
- Store/queue overload and retention evidence.
- Security review for persisted identity/sensitive state.
- Backup, restore, rollout, and rollback exercise.
- Accepted deployment-profile and gateway/adapter matrix.
- Full selected-capability state-coverage and excluded-capability report.

## Stop-the-Line Conditions

R4 enters remediation when:

- A supposedly movable workload still requires undisclosed process-local state.
- Lease expiry can create untracked concurrent owners.
- Retried mutations can duplicate effects without an observable unknown outcome.
- A checkpoint restores against incompatible code/artifact silently.
- Coordinator restart loses authoritative work.
- Durable store/queue saturation causes unbounded process memory.
- Sensitive state is persisted without required protection or retention policy.

## Rollout Contract

1. Dual-write durable state in shadow mode where consistency can be checked.
2. Read/compare without using durable data as authority.
3. Move one workload class to durable read authority.
4. Exercise restart/recovery before increasing coverage.
5. Run the full selected workload on one active durable worker pool.
6. Retain in-memory fallback only while reverse synchronization is proven.

## Rollback Contract

- Stop admitting work that requires the new durable schema.
- Drain or checkpoint according to the accepted safe-point policy.
- Restore previous reader/writer generation only when schema compatibility is
  proven.
- Do not blindly copy newer checkpoint state into older code.
- Immutable artifacts remain valid if their compatibility requirements hold.
- Preserve durable evidence and dead-letter records through rollback.

## Handoff to R5

R5 receives:

- durable queue/store interfaces proven under single-active operation;
- lease/heartbeat/attempt protocol;
- idempotency and transaction policies;
- checkpoint-safe movable workloads and node-affine inventory;
- recovery windows, saturation signals, and operator runbooks;
- plugin generation and placement requirements.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D4.1–D4.8 delivered.
- [ ] All exit criteria passed on durable single-active operation.
- [ ] Crash, saturation, backup/restore, and rollback exercised.
- [ ] Compatibility, operations, security, and data owners approved.
- [ ] R5 owner accepted the handoff.
- [ ] Contract status changed to **Completed**.
