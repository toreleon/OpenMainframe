# R6 Contract — Long-Lived Plugin Ecosystem

Status: **Proposed**
Contract version: **0.3**
Indicative horizon: **15–24 months and ongoing**
Authority change: **Stable external plugin contracts and governed ecosystem**
Phase plan: [R6 Phase Plan](phases/r6-phase-plan.md)

## Contract Outcome

R6 turns the internal plugin architecture into a maintainable extension
ecosystem. Public contracts are declared stable only after multiple independent
implementations, conformance tests, lifecycle/rollback evidence, ownership, and
compatibility policy exist. External plugins run through sandboxed WebAssembly
components or supervised processes with explicit permissions and limits; native
Rust dynamic-library ABI is not public.

Maintenance governance begins in earlier waves. R6 is the gate at which it
becomes a supported ecosystem promise.

## Dependencies

- Stable built-in execution/compiler/plugin contracts exercised by at least two
  independent implementations where public stability is proposed.
- Completed [R5 Horizontal Platform](r5-horizontal-platform.md) generation,
  draining, placement, resource, and failure-isolation behavior for distributed
  deployments, or an explicitly accepted single-node external-plugin product
  profile.
- Versioned IR/runtime/artifact/checkpoint contracts from prior waves.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r6--long-lived-plugin-ecosystem)
- [Execution isolation tiers](../execution-backend.md#isolation-tiers)
- [IR plugin contract model](../plugin-ir-architecture.md#plugin-contract-model)
- [Workspace convergence and retirement](../workspace-convergence.md#retirement-protocol)

## Scope

### In scope

- Public plugin SDK, manifest schema, operation/type schema tools, generated
  bindings, and conformance kit.
- Approved WIT worlds for compiler/execution/runtime capabilities.
- Supervised process protocol for native/JVM/proprietary runtimes.
- Signing, provenance, trust, permission, admission, resource, and audit policy.
- Installation, validation, readiness, canary, generation upgrade, drain,
  rollback, removal, and compatibility lifecycle.
- Sample frontend/dialect, lowering/pass, runtime provider, and executor/backend
  implementations as applicable.
- A sample IR analysis/assessment pass using the R2 analysis result contract,
  without importing the legacy standalone analyzer implementation.
- Semantic versioning, compatibility matrices, deprecation windows, upgrade
  passes, ownership, release trains, and generated documentation.
- Incremental migration of additional languages and typed subsystem dialects
  based on product value.
- Schema/registry-generated platform documentation; a separately retained
  portfolio wiki is tooling and is not evidence for this obligation.

### Out of scope

- Native Rust `.so`/`.dylib` trait-object ABI as a supported boundary.
- Ambient filesystem, network, environment, clock, randomness, credentials, or
  global application state for untrusted plugins.
- Stability promises for contracts exercised by only one implementation.
- Unlimited backward compatibility without deprecation or security policy.
- Plugin marketplace/business processes unless separately approved.

## Entry Contract

R6 may declare a contract **In Progress toward Stable** when:

- The logical contract is already used by built-in production paths.
- At least two independent implementations or adapters exercise its meaningful
  variation points.
- Contract ownership, versioning, permission model, and threat model exist.
- Conformance, failure, resource, lifecycle, and rollback tests exist.
- Serialization/WIT/process schemas are versioned.
- Host behavior for unknown/incompatible versions is explicit.
- The proposed support and deprecation window is approved.

## Mandatory Deliverables

### D6.1 Public plugin SDK

Publish:

- manifest and capability descriptors;
- execution/compiler phase request/result bindings;
- diagnostics, events, artifacts, IR bytecode/text, and runtime interface IDs;
- operation/type declaration generator and typed wrappers;
- host capability client bindings;
- test host, deterministic fixtures, and conformance runner;
- packaging, signing, validation, and local development tools;
- examples and migration guides.

SDK versions are independent from the OpenMainframe product version where
appropriate.

### D6.2 WebAssembly component boundary

Define approved WIT packages with:

- explicit exports and granted imports;
- typed resources, values, errors, conditions, outcomes, and streams;
- cancellation/deadline/resource propagation;
- no ambient authority by default;
- memory/table/instance/fuel or epoch limits;
- bounded stdout/stderr and payloads;
- trap-to-structured-failure mapping;
- version compatibility and feature negotiation.

### D6.3 Supervised process boundary

Provide a versioned protocol for plugins requiring native libraries, JVMs,
proprietary runtimes, or stronger fault isolation:

- authenticated worker/plugin identity;
- short-lived capability tokens instead of raw long-lived credentials;
- invocation, events, cancellation, health, checkpoint, and outcome semantics;
- bounded transport messages and backpressure;
- crash/restart and generation behavior;
- protocol version negotiation and incompatibility rejection.

### D6.4 Trust and security policy

- Plugin signature/provenance and publisher identity.
- Requested versus deployment-granted capabilities.
- Isolation-tier and worker-placement policy.
- Resource and concurrency quotas.
- Secret-reference and host-service authorization rules.
- Vulnerability, revocation, quarantine, and emergency-disable procedures.
- Audit events for install, permission, activation, invocation, upgrade, drain,
  failure, and removal.

### D6.5 Lifecycle and compatibility

- Install into non-ready generation, validate, then publish ready snapshot.
- Canary new generation without replacing active generation in place.
- Drain or checkpoint old work according to compatibility.
- Reject incompatible artifacts/checkpoints clearly.
- Roll selector/generation back without restarting unrelated work.
- Remove only after no active/dependent artifacts or an approved orphan policy.
- Generate host/plugin/dialect/runtime/artifact/checkpoint compatibility matrix.

### D6.6 Maintenance governance

- Semantic versioning rules for every public contract family.
- Major-change and upgrade-pass requirements.
- Deprecation window and warning mechanism.
- Primary/backup owners and CODEOWNERS or equivalent.
- Architecture dependency checks and compatibility CI.
- Release train, canary, drain, rollback, and long-term support policy.
- Public support-status and known-limitations documentation.

### D6.7 Independent reference plugins

Provide enough independent implementations to validate the SDK, for example:

- a sample language frontend/dialect or analysis pass;
- an assessment/inspection example over public IR analysis contracts when that
  contract family is proposed as stable;
- a sample typed runtime provider;
- a sample executor/backend or workflow adapter;
- at least one Wasm component and one process worker when both adapters are
  declared supported.

Examples must use only public contracts and pass the same conformance suite as
third-party plugins.

### D6.8 Sustainable expansion

Migrate additional languages, backends, and service dialects only when:

- product value and ownership are clear;
- semantic coverage and compatibility fixtures exist;
- the extension uses public/stable contracts or is explicitly experimental;
- unsupported behavior remains explicit;
- generated capability documentation is updated.
- no reference implementation reaches through a deprecated `assess`, `wiki`,
  TUI frontend, or protocol-adapter implementation to bypass the public SDK.

### D6.9 Portfolio Pluginization and Retirement

- Move every retained optional language, subsystem, protocol, analysis tool,
  harness, and delivery tool to a governed plugin/provider, test-only, or
  independently packaged boundary.
- Require every leaf product or experiment to have an owner, entry point,
  support level, fixtures, compatibility policy, and maintenance budget.
- Execute approved removals only after consumer, profile, fixture, rollback,
  documentation, and compatibility-window gates pass.
- Hand R7 a finite ledger of long-term compatibility boundaries and unresolved
  exceptions; broad unplanned implementation work is not an acceptable handoff.

## Invariants

- External plugins have no ambient authority by default.
- Public contract stability requires two independent implementations.
- Plugin registration does not mutate an active registry snapshot.
- New work never routes to unready, incompatible, revoked, or draining
  generations.
- Plugin failure is contained to the declared invocation/run-unit/worker
  boundary.
- Permissions are deployment grants, not trusted manifest assertions.
- Unknown contract versions fail explicitly.
- Breaking semantic/effect/ABI changes require a major version or upgrade path.
- Deprecated contracts remain observable and owned until removed.
- Documentation is generated from the same schemas used by validation where
  possible.
- Legacy portfolio-tool output cannot substitute for reproducible operation,
  capability, permission, and compatibility documentation.

## Exit Criteria

- At least two independent implementations pass conformance for every contract
  declared stable.
- An external sample plugin installs, validates, runs within limits, upgrades,
  drains, rolls back, and uninstalls without restarting unrelated workloads.
- Wasm plugins demonstrate no ambient filesystem/network/clock/random access
  without grants.
- Process plugins demonstrate authenticated identity, bounded transport,
  cancellation, crash containment, and version negotiation.
- Permission denial, resource exhaustion, trap/crash, incompatible version,
  revocation, and quarantine tests pass.
- Compatibility CI covers every currently supported host/plugin version pair.
- Generated operation/capability/permission/support documentation matches
  registry/schema evidence.
- Every stable contract has primary/backup owners and deprecation authority.
- A major-version sample upgrade demonstrates warning, coexistence, migration,
  and removal policy.
- Every component is on an accepted profile/plugin/tool/test boundary or has
  completed retirement; all remaining R7 exceptions are finite and owned.

## Required Evidence

- Public SDK/API and threat-model review.
- WIT/process schema and compatibility tests.
- Independent reference-plugin conformance reports.
- Resource, permission, malicious-input, trap/crash, and isolation reports.
- Install/canary/drain/rollback/revocation/removal exercises.
- Generated compatibility/support documentation.
- Ownership, versioning, deprecation, and release policy approval.
- External developer usability feedback from the sample/plugin pilot.
- Portfolio pluginization, independent-tool packaging, archive, and exception ledger.

## Stop-the-Line Conditions

R6 enters remediation when:

- A public contract has only one effective implementation.
- An external plugin can obtain ambient authority or bypass host authorization.
- Plugin trap/crash can terminate unrelated in-process workloads beyond the
  declared isolation tier.
- Registration mutates active behavior nondeterministically.
- Breaking changes are shipped without version/migration policy.
- Revoked or draining plugins continue receiving new work.
- Compatibility documentation cannot be generated or reproduced from evidence.
- A public contract has no accountable owner.

## Rollout Contract

1. Mark contracts experimental and built-in only.
2. Implement the second independent adapter/plugin.
3. Publish preview SDK and conformance kit.
4. Run a bounded external pilot with no production authority.
5. Canary signed plugins in a restricted capability profile.
6. Declare stable only after exit evidence and support ownership.
7. Expand permissions and workload selectors independently.

## Rollback and Revocation Contract

- Remove a generation from ready snapshots and stop new admission.
- Drain, checkpoint, cancel, or terminate active work according to policy.
- Revoke credentials/capability tokens and quarantine artifacts if required.
- Route selectors to the previous compatible generation or built-in adapter.
- Preserve audit, artifacts, and failure evidence.
- Contract rollback does not silently reinterpret stored IR/artifacts/checkpoints
  under an older incompatible version.

## Ongoing Maintenance Contract

After R6 completion:

- Compatibility CI runs for supported versions on every release train.
- Security advisories and revocation are tested operational procedures.
- Deprecated contracts have removal milestones and usage telemetry.
- Plugin SDK and generated docs release together with contract changes.
- Ecosystem metrics track active generations, compatibility failures,
  deprecated use, crash/trap rate, and support ownership.
- New contract families repeat the R6 two-implementation stability gate.

## Definition of Done

- [ ] Entry contract satisfied for each stable contract family.
- [ ] D6.1–D6.9 delivered for the supported ecosystem profile and portfolio.
- [ ] All exit criteria passed.
- [ ] External lifecycle, malicious-input, failure, rollback, and revocation exercised.
- [ ] Compatibility, operations, security, architecture, and SDK owners approved.
- [ ] Ongoing maintenance ownership accepted.
- [ ] Contract status changed to **Completed** for the declared stable profile.
