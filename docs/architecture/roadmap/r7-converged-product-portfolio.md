# R7 Wave Contract — Converged Product Portfolio

Status: **Proposed**
Contract version: **0.3**
Date: **2026-08-26**
Parent roadmap: [OpenMainframe Platform Roadmap](../platform-roadmap.md)
Phase plan: [R7 Phase Plan](phases/r7-phase-plan.md)

Related specification:

- [Workspace Convergence and Sustainable Architecture](../workspace-convergence.md)

## Wave Intent

Certify that the portfolio-wide refactor performed throughout R0–R6 has
converged. R7 closes exceptions, retires superseded authorities, and proves each
supported product profile as a reproducible dependency and capability closure.
It is not a delayed cleanup wave and does not authorize a big-bang rewrite.

## Entry Criteria

- R0–R6 exit evidence is available for every authority used by an advertised
  profile.
- The workspace portfolio manifest and product-profile manifests are current.
- Every remaining migration adapter, dependency exception, mock/stub path,
  duplicated authority, deprecated selector, and unowned component is listed
  with an owning prior wave.
- No new foundational architecture is required to close the list. If it is,
  the relevant earlier wave contract is reopened and passed first.
- The final conformance environment can reproduce core, compatibility,
  language, subsystem, analysis/test, and operations profile builds.

## Mandatory Deliverables

### D7.1 Portfolio manifest

- Classification, owner, support level, profiles, public surface, entry points,
  consumers, state authority, test evidence, and retirement rule for every crate.
- No leaf crate is treated as dead solely because it lacks an internal consumer.
- Newly added contract crates justify their dependency, ownership, versioning,
  isolation, or release boundary.

### D7.2 Dependency and public-API closure

- Mechanically verified workspace dependency graph and accepted profile closures.
- Removal of unused and reverse-layer edges, with no core dependency on UI,
  analysis, test, deployment, or concrete optional-adapter implementations.
- Every public surface labelled supported, beta, internal, deprecated, or
  archived with a compatibility policy.

### D7.3 Authority uniqueness

- One observable authority per protocol, configuration family, capability
  resolution family, program path, and durable state class.
- TN3270 protocol state is neutral and shared by listener/frontend adapters.
- Retained DRDA selectors use typed DB2/security services and contain no mock
  production success.
- User-facing configuration formats adapt to one versioned product schema.

### D7.4 Product-profile completeness

- Reproducible manifests and build/test/package/document commands for every
  advertised profile.
- Compatibility, security, malformed-input, overload, cancellation, durability,
  recovery, and plugin-generation evidence appropriate to each capability.
- Explicit absence and diagnostic behavior for unselected optional capabilities.

### D7.5 Legacy retirement and archive ledger

- Superseded authorities, adapters, selectors, crates, features, configuration,
  deployment assets, and documentation are removed after their declared gates.
- Long-term compatibility adapters have an explicit owner, version contract,
  fixture set, and maintenance budget.
- Archived experiments retain provenance and migration guidance without
  remaining in production build closures.

### D7.6 Sustainable governance

- Ownership and backup-reviewer map for crates and cross-crate authorities.
- Maintainability reviews for high-churn or mixed-authority modules.
- CI checks for architecture direction, feature/profile matrices, API policy,
  deterministic fixtures, documentation, and exception expiry.
- A repeatable quarterly portfolio review that prevents convergence drift.

## Required Evidence

- Source-revision-stamped portfolio and profile manifests.
- Workspace and per-profile dependency graphs, build results, test results,
  package contents, and generated documentation.
- Zero-result reports for prohibited dependency edges, unclassified public
  surfaces, expired exceptions, unsupported mock-success paths, and unowned
  components.
- Protocol/configuration/registry authority map and conformance results.
- Removal ledger with compatibility-window and rollback evidence.
- Resource, security, recovery, and upgrade comparison against the accepted
  baselines for every supported profile.

## Exit Criteria

R7 completes only when all of the following are true:

1. Every workspace crate has an accepted owner, classification, support state,
   profile membership, boundary, and retention/retirement rule.
2. Every advertised profile builds, tests, documents, and packages from its
   manifest without excluded components in its dependency closure.
3. There are no unverified internal dependency edges, prohibited cycles,
   reverse-layer dependencies, expired exceptions, or unowned leaf products.
4. Protocol, configuration, capability/registry, program-resolution, and state
   authority is unique for every supported selector.
5. No production-supported path returns mock, unconditional, or generic success.
6. Language and subsystem adoption records are complete; retained independent
   semantics still use common lifecycle, effects, limits, and support contracts.
7. Migration adapters and legacy authorities are removed or accepted as
   versioned long-term compatibility components.
8. High-risk modules and public APIs have owners, evidence, review boundaries,
   and deprecation policies.
9. The final portfolio evidence pack is reproducible and independently accepted.

## Stop-the-Line Conditions

- A profile passes only by importing an excluded tool, harness, UI, or adapter.
- A mock/stub path is reachable from a supported production selector.
- Two components can become observable authority for the same selector without
  an explicit generation or migration selector.
- A removal lacks consumer inventory, compatibility evidence, or rollback.
- Certification uncovers a missing foundational contract that would require an
  unreviewed authority change.

## Rollback and Remediation

R7 itself moves no broad new authority. A failed criterion reopens remediation
under the wave that owns the dependency, semantic, host/state, distribution, or
plugin boundary. Removed components follow their recorded rollback or archive
procedure; profile promotion remains blocked until the repeated evidence passes.

## Handoff

On completion, the portfolio manifests, ownership map, architecture rules,
conformance matrix, exception policy, and retirement ledger become ongoing
release-governance inputs. Any later crate or public capability must enter
through the same classification and profile gates.

## Definition of Done

- [ ] Entry criteria satisfied on the frozen source revision.
- [ ] D7.1–D7.6 delivered and independently reproduced.
- [ ] All exit criteria pass without expired or unowned exception.
- [ ] All failed findings were remediated under their owning earlier wave.
- [ ] Architecture, product, compatibility, quality, operations, security, and
      maintenance owners accepted the final evidence pack.
