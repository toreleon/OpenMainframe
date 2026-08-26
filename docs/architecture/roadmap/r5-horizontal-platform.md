# R5 Contract — Horizontal Platform

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **12–20 months**
Authority change: **Multiple active worker nodes and generation-aware placement**
Phase plan: [R5 Phase Plan](phases/r5-phase-plan.md)

## Contract Outcome

R5 activates horizontal execution across multiple workers. The scheduler places
work by capability, target, resource, service class, artifact locality,
isolation, session affinity, and plugin generation. Worker loss is recovered
through R4 leases/checkpoints; rolling generations drain without stopping
unrelated work; load tests demonstrate useful scale rather than merely more
processes.

## Dependencies

- Completed [R4 Distribution-Ready State](r4-distribution-ready-state.md).
- R3 single-node overload, lane isolation, and capacity baselines.
- Durable queue/store deployment with accepted operational ownership.
- Defined multi-node consistency and failure model.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r5--horizontal-platform)
- [Execution distributed mode](../execution-backend.md#distributed-mode)
- [Execution lifecycle state machines](../execution-backend.md#lifecycle-state-machines)
- [Workspace product profiles](../workspace-convergence.md#product-profiles)

## Scope

### In scope

- Worker registration, capability/target inventory, health, heartbeat, and
  generation availability.
- Multi-node bounded admission and WLM/service-class scheduling.
- Placement by capability, target, resources, isolation, locality, affinity, and
  generation.
- Lease ownership, takeover, safe-point resume, and node-affine routing.
- Artifact replication/locality and cold-start behavior.
- Generation canary, drain, rollback, and worker maintenance.
- One/two/four-worker load, worker-loss, partition, saturation, and mixed-load
  tests.
- Stateless gateway scale and reconstruction of coordinator caches.
- Horizontal validation for every interface actually advertised by the accepted
  multi-worker profile; optional adapters remain profile-scoped.

### Out of scope

- Exactly-once execution guarantees.
- Cross-region active-active deployment.
- Transparent migration of workloads classified node-affine in R4.
- Public third-party plugin compatibility before R6 gate.
- Hiding durable store/queue bottlenecks with unbounded application queues.
- Claiming horizontal support for DRDA, TUI/3270, or another optional adapter
  that was excluded from the validation profile.

## Entry Contract

R5 may enter **In Progress** when:

- R4 runs promoted workloads on durable interfaces with one active worker pool.
- Lease recovery and idempotency tests pass.
- Worker resource/capability descriptors and placement policy are versioned.
- One-node benchmark and saturation profile is accepted.
- Network partition, store unavailability, and worker loss policies are defined.
- Rollback can return to a single active worker pool without data conversion.

## Mandatory Deliverables

### D5.1 Worker and capability protocol

Workers publish:

- stable worker/node identity and zone/placement metadata;
- supported plugin generations, capabilities, targets, and isolation adapters;
- available resource classes and configured capacities;
- health/readiness and heartbeat;
- draining/maintenance state;
- artifact/cache locality hints that are never authoritative.

Stale workers are not eligible for new claims.

### D5.2 Global admission and scheduling

- Enforce global, per-principal/tenant, per-service-class, per-plugin, and
  per-provider bounds.
- Translate WLM policy into priority/fairness without starvation.
- Select only workers satisfying capability/version/target/resource constraints.
- Preserve deadlines and cancellation across queue and worker boundaries.
- Reject or queue work explicitly when no compatible capacity exists.
- Record scheduling and placement decisions.

### D5.3 Placement and affinity

- Prefer artifact/data locality when it improves measured cost.
- Maintain session/run-unit affinity while useful, with lease-based takeover at
  safe points.
- Route node-affine legacy workloads only to compatible capacity and declare
  reduced availability.
- Avoid correctness dependency on sticky gateway routing.
- Bound artifact prefetch and cold-start concurrency.

### D5.4 Recovery and retry

- Reclaim expired worker leases according to R4 policy.
- Restore checkpoints only on compatible workers/generations.
- Distinguish retryable, unknown-effect, non-retryable, cancelled, and poisoned
  work.
- Prevent two live workers from committing the same fenced ownership epoch.
- Emit operator-visible recovery and dead-letter events.

### D5.5 Generation lifecycle

- Route canary work to new plugin/worker generations.
- Stop new admission to draining generations.
- Let compatible work finish or checkpoint/migrate at safe points.
- Reject incompatible checkpoints explicitly.
- Roll selectors back without restarting unrelated generations.
- Maintain artifact/executor compatibility during mixed-generation operation.

### D5.6 Multi-node observability

Expose:

- global and per-worker queue/active/suspended counts;
- scheduling and placement latency;
- compatible capacity and no-placement reasons;
- lease age/expiry/reclaim and attempt counts;
- checkpoint restore and worker-loss recovery time;
- artifact transfer/cold-start/cache locality;
- store/queue/provider saturation;
- generation canary, drain, and rollback progress.

### D5.7 Horizontal validation suite

Test:

- one, two, and four workers under non-store-bound request and batch workloads;
- mixed request, interactive, batch, blocking, and event-hook load;
- worker crash during each execution state;
- gateway/coordinator restart;
- network delay/partition and store/queue throttling;
- rolling worker and plugin generation upgrades;
- artifact cold start and cache loss;
- worker pool scale-up/down.
- selected optional gateway traffic only when that interface is advertised by
  the deployment profile.

### D5.8 Complete Profile Topology and Conformance

- Generate the exact dependency, capability, configuration, state, placement,
  artifact, and plugin-generation closure for each advertised multi-worker profile.
- Exercise every selected language, subsystem, protocol, and gateway capability
  through its public boundary under placement, failure, recovery, and upgrade.
- Verify explicit absence behavior for every unselected optional pack and ensure
  excluded implementations are not present in the deployed package closure.

## Invariants

- Global and local queues remain bounded.
- One valid fenced owner may commit a work attempt at a time.
- Delivery remains at least once; service idempotency/transactions protect
  effects.
- Scheduler caches and locality hints are not authoritative.
- Placement never violates capability, version, target, permission, or resource
  constraints.
- Node-affine limitations are explicit and observable.
- New work never routes to draining/unready generations.
- Scale-out cannot weaken principal, deadline, cancellation, audit, or limits.

## Exit Criteria

- Stateless/request and batch workloads achieve at least 70% scaling efficiency
  from one to four workers on an accepted non-store-bound benchmark.
- Adding workers does not introduce unbounded queue/task/memory growth.
- One worker loss recovers eligible leased work within the accepted
  service-class recovery window.
- Mixed workload classes preserve accepted fairness and latency isolation.
- No accounted crash/retry test produces an untracked duplicate external effect.
- Session affinity/takeover matches declared durability; node-affine paths fail
  or wait according to policy.
- Canary, drain, mixed-generation execution, and rollback complete without
  stopping unrelated work.
- Store/queue/provider bottlenecks appear as named saturation metrics and
  backpressure.
- The deployment can return to one active worker pool without schema rollback.
- Support/status metadata identifies the exact core and optional interfaces
  covered by the horizontal evidence.
- Each advertised horizontal profile passes its complete manifest; no isolated
  workspace crate or untested optional selector is implied to share that support.

## Required Evidence

- Worker protocol and scheduler policy review.
- One/two/four-worker benchmark report and efficiency calculation.
- Worker-loss, partition, reclaim, and fenced-ownership tests.
- Mixed-load fairness/isolation report.
- Artifact locality/cold-start report.
- Generation canary/drain/rollback exercise.
- Store/queue saturation and backpressure evidence.
- Security review of cross-node identity, credentials, and worker trust.
- Per-profile topology/package closure and full selected-capability conformance report.

## Stop-the-Line Conditions

R5 enters remediation when:

- Multiple live owners can commit the same fenced work epoch.
- Adding workers increases unbounded application buffering.
- Scheduler routes work to incompatible or unready generations.
- A worker failure loses authoritative movable state.
- Network/store failure causes silent success or untracked duplicate effects.
- Global admission can be bypassed by adding workers.
- Rollback requires destructive durable-schema reversal.

## Rollout Contract

1. Add passive workers for health/capability observation.
2. Route stateless canary work to a second worker.
3. Enable bounded batch work with lease recovery.
4. Enable checkpointed interactive takeover only after safe-point tests.
5. Add workers one at a time and compare scaling/saturation evidence.
6. Exercise generation drain before regular rolling upgrades.

## Rollback Contract

- Stop new claims on additional workers and mark them draining.
- Return admission to one compatible active worker pool.
- Let owned work finish, checkpoint, expire, or cancel according to policy.
- Preserve durable attempts, effects, and dead-letter records.
- Do not reverse durable schemas as part of routine scale rollback.
- Keep stateless gateways horizontally scaled if they remain compatible.

## Handoff to R6

R6 receives:

- stable plugin generation and lifecycle semantics;
- worker isolation/placement adapters;
- compatibility and capability registry behavior;
- resource, permission, cancellation, and failure containment;
- canary/drain/rollback mechanisms;
- cross-node artifact and interface version evidence.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D5.1–D5.8 delivered.
- [ ] All exit and scaling criteria passed.
- [ ] Worker loss, partition, saturation, generation rollback exercised.
- [ ] Compatibility, operations, security, and performance owners approved.
- [ ] R6 owner accepted the handoff.
- [ ] Contract status changed to **Completed**.
