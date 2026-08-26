# Platform Roadmap Wave Contracts

Status: **Proposed**
Contract family: **OpenMainframe Platform Transformation**
Contract version: **0.3**
Date: **2026-08-26**

## Purpose

This directory turns the high-level [OpenMainframe Platform Roadmap](../platform-roadmap.md)
into executable delivery contracts. Each wave contract defines what must be true
before work begins, what may change, what must remain compatible, which evidence
is required, and what must be demonstrated before the next wave is promoted.

The contracts coordinate the detailed designs in:

- [Scalable Execution Backend](../execution-backend.md)
- [Plugin-Oriented Compiler and Multi-Level IR Architecture](../plugin-ir-architecture.md)
- [Workspace Convergence and Sustainable Architecture](../workspace-convergence.md)
- [Architecture Overview](../overview.md)

## Contract Index

The [phase-plan index](phases/README.md) decomposes each wave into architectural
checkpoints sized for later agent goal contracts.

| Wave | Wave contract | Phase plan | Primary outcome | Promotion dependency |
|---|---|---|---|---|
| R0 | [Truthful System](r0-truthful-system.md) | [R0 phases](phases/r0-phase-plan.md) | Measured compatibility, explicit profiles, and no silent semantic loss | Current baseline |
| R1 | [One Execution Spine](r1-one-execution-spine.md) | [R1 phases](phases/r1-phase-plan.md) | One bounded program path with clean runtime/tool/adapter boundaries | R0 evidence pack |
| R2 | [One Semantic Spine](r2-one-semantic-spine.md) | [R2 phases](phases/r2-phase-plan.md) | Shared IR proven through execution, assessment, and symbolic consumers | R0 contracts and R1 artifact boundary |
| R3 | [Strong Single Node](r3-strong-single-node.md) | [R3 phases](phases/r3-phase-plan.md) | Typed host services, neutral session boundary, predictable vertical scaling | R1 kernel and R2 vertical slice |
| R4 | [Distribution-Ready State](r4-distribution-ready-state.md) | [R4 phases](phases/r4-phase-plan.md) | Movable state, leases, idempotency, and recovery | R3 bounded single-node gate |
| R5 | [Horizontal Platform](r5-horizontal-platform.md) | [R5 phases](phases/r5-phase-plan.md) | Multi-node scheduling, recovery, and generation lifecycle | R4 durability gate |
| R6 | [Long-Lived Plugin Ecosystem](r6-long-lived-ecosystem.md) | [R6 phases](phases/r6-phase-plan.md) | Stable isolated plugin contracts and maintenance governance | Stable built-in contracts and R5 lifecycle |
| R7 | [Converged Product Portfolio](r7-converged-product-portfolio.md) | [R7 phases](phases/r7-phase-plan.md) | Whole-workspace profile, dependency, authority, and retirement certification | Completed R0–R6 convergence evidence |

## Contract Status Model

Each wave contract moves through these states:

```text
Draft -> Proposed -> Accepted -> In Progress -> Gate Review -> Completed
                                      |              |
                                      v              v
                                   Blocked       Remediation

Any state -> Superseded by a versioned replacement
```

- **Draft:** incomplete and not a planning commitment.
- **Proposed:** complete enough for architecture and delivery review.
- **Accepted:** scope, owners, gates, and evidence obligations are approved.
- **In Progress:** implementation is active under the accepted contract.
- **Gate Review:** all claimed deliverables and evidence are frozen for review.
- **Remediation:** exit evidence is insufficient; production authority does not
  move.
- **Completed:** exit gate passed and handoff obligations were accepted.
- **Blocked:** an external decision or prerequisite prevents safe progress.
- **Superseded:** a newer contract explicitly replaces this version.

## Required Roles

Names may vary by organization, but every accepted wave identifies:

| Role | Accountability |
|---|---|
| Executive sponsor | Confirms product priority, resources, and acceptable risk |
| Architecture owner | Protects cross-wave contracts and dependency direction |
| Wave lead | Owns scope, delivery, evidence, and gate-review readiness |
| Compatibility owner | Owns fixtures and observable-behavior decisions |
| Quality gate owner | Independently verifies functional and regression evidence |
| Operations gate owner | Verifies limits, overload, recovery, telemetry, and rollback |
| Security owner | Verifies capability, identity, isolation, and audit obligations |
| Workstream owners | Own implementation and maintenance for their contract surfaces |

A role may be held by the same person in a small team, but gate evidence cannot
be approved solely by the author of the implementation it validates.

## Common Invariants

Every wave must preserve these rules:

1. Existing public behavior remains authoritative until a named selector moves
   authority after a passed gate.
2. Unsupported executable behavior is explicit; it is never silently discarded
   or converted to success.
3. Every queue, worker pool, output buffer, recursion boundary, and externally
   supplied IR/source structure introduced by the wave is bounded.
4. Source, artifact, plugin generation, execution, and trace identity remain
   observable across new boundaries.
5. Authentication and principal context are not weakened during migration.
6. New contracts have owners, versions, tests, documentation, and rollback.
7. Migration adapters are inventoried and include removal criteria.
8. No phase gate is passed using calendar pressure as evidence.
9. A wave may reduce scope, but may not waive correctness, security, boundedness,
   or rollback obligations.
10. New durable schemas and public contracts are versioned before production
    data or third-party dependencies rely on them.
11. Every current component is classified as core, compatibility adapter,
    tooling, test infrastructure, or legacy implementation before a phase may
    expand or remove it.
12. Optional components outside the accepted deployment profile do not gate a
    wave, but they also cannot be advertised as supported or used as evidence
    for that profile.
13. Tool-only dependencies do not remain in server/runtime build closures, and
    a mixed component is split before its optional surface is removed.
14. Every crate and public selector has an owner, support state, product
    profile, target boundary, and retention or retirement rule.
15. A supported profile contains no excluded UI/tool/test/adapter implementation
    and no mock, unconditional, or generic-success production path.
16. Protocol, configuration, capability-resolution, and durable-state authority
    is unique for each promoted selector; temporary duplication has an owner and
    expiry gate.

## Component Lifecycle Policy

The authoritative initial disposition is recorded in the
[platform roadmap](../platform-roadmap.md#current-component-disposition). Each
phase maintains a component inventory with:

```text
component and selectors
classification: core | compatibility adapter | tooling | test infrastructure | legacy
accepted support profiles
owner and compatibility fixtures
dependency closure and state authority
public support level and protocol/configuration authority
deprecation state and compatibility window
replacement boundary
removal or long-term retention gate
```

Removal is permitted only after consumers have migrated, selected observable
behavior is covered, rollback or replacement has been exercised, and the
accepted compatibility window has closed. A test harness may be replaced only
by equivalent or stronger reproducible evidence. Runtime-neutral state must be
extracted before a UI or protocol frontend that currently owns it is removed.

## Common Evidence Pack

Every gate review includes:

- A signed checklist of entry and exit criteria.
- Links to implementation changes and contract/API documentation.
- Compatibility and regression results.
- Failure, cancellation, and overload results applicable to the wave.
- Security and authorization evidence applicable to new boundaries.
- Performance/resource comparison against the accepted baseline.
- Known gaps with explicit selectors, owners, and remediation dates.
- Rollback procedure and the result of a rollback exercise.
- Updated capability, adapter, and deprecated-contract inventories.
- Updated support-profile, component-disposition, and dependency-closure
  inventories.
- Updated all-crate ownership, public-surface, authority-uniqueness,
  mock/stub-path, and retirement-exception inventories.
- Handoff record accepted by the next wave's owner.

Evidence must identify source revision, configuration, plugin generations,
target, dataset/fixture versions, and the environment required to reproduce it.

## Change Control

- Editorial clarifications that do not change obligations increment the document
  revision without resetting an active gate review.
- Adding/removing scope, weakening a metric, changing authority, or changing a
  compatibility promise requires explicit architecture and wave-owner approval.
- While a contract is **Proposed**, component rescoping is incorporated by
  revising the contract and its phase plan. After **Accepted**, the same change
  requires an approved versioned scope amendment before implementation.
- A missed estimate does not automatically change the contract.
- A contract may be split into smaller accepted increments when each increment
  preserves the same invariants and has an independent rollback boundary.
- Cross-wave work may start early in shadow/prototype mode, but cannot become
  production authority before its prerequisite gate passes.

## Promotion Decision

A wave is promoted only when all mandatory exit criteria pass. The decision is
one of:

- **Promote:** authority and handoff may move as declared.
- **Promote with contained exception:** only when the exception is outside the
  promoted selector, explicitly disabled, owned, and time bounded.
- **Remediate:** keep current authority and correct evidence gaps.
- **Rollback:** restore the prior selector/generation/state authority.
- **Supersede:** approve a new version of the contract before further work.
