# R7 Phase Plan — Converged Product Portfolio

Status: **Proposed**
Plan version: **0.3**
Date: **2026-08-26**
Parent contract: [R7 — Converged Product Portfolio](../r7-converged-product-portfolio.md)
Authority result: **Whole-workspace product-profile certification**
Expected agent goals: **8–14**

## Phase Outcome

Certify the whole workspace after convergence work performed in R0–R6. These
phases close bounded exception families and collect final evidence. A failed
phase routes remediation to the owning earlier wave; R7 does not invent new
foundational contracts or perform an atomic rewrite.

## Authority and Scope Rules

- No phase may broaden a supported selector merely to make a profile appear
  complete.
- Cleanup follows the existing authority ladder and retirement protocol.
- Each exception is owned by exactly one phase and one earlier wave.
- Portfolio counts are regenerated from the reviewed revision; the 44-crate
  baseline is not assumed to remain static.
- A profile is certified only as a complete build, test, package, configuration,
  and runtime capability closure.

## R7.P0 — Portfolio Freeze and Exception Burn-Down Plan

**Primary outcome:** one reviewable manifest and finite closure ledger.

**Scope:**

- regenerate crate, dependency, profile, public-API, owner, test, mock/stub,
  authority, configuration, registry, and migration-adapter inventories;
- assign every gap to R1 dependency, R2 semantic, R3 host/protocol, R4 state,
  R5 topology, or R6 plugin/retirement remediation;
- freeze the profile set and the exact evidence required for certification.

**Exit evidence:** no unclassified component or unowned exception; every item
has an expiry condition and a phase owner.

**Authority:** none.

## R7.P1 — Dependency and Public-API Closure

**Primary outcome:** accepted architecture graph and support-surface policy.

**Scope:**

- remove verified unused and reverse-layer dependency edges;
- prove core profiles exclude UI, analysis, test, deployment, and optional
  adapter implementations;
- label and document all public surfaces and verify feature/profile matrices;
- close or explicitly accept every dependency exception.

**Exit evidence:** reproducible dependency graphs, zero prohibited/expired edges,
profile build matrix, and API support report.

**Rollback:** restore only the individual edge or adapter through its recorded
migration selector; profile certification remains blocked.

## R7.P2 — Protocol, Configuration, and Registry Closure

**Primary outcome:** unique cross-crate authorities.

**Scope:**

- prove neutral TN3270 protocol state with transport and presentation adapters;
- prove retained DRDA uses typed DB2/security services with no mock success;
- verify all user-facing formats map to one versioned configuration schema;
- verify capability identity/lifecycle resolution is globally coherent while
  valid private provider registries remain encapsulated.

**Exit evidence:** authority map, conformance suites, malformed/failure results,
configuration round trips, and duplicate-authority zero report.

**Rollback:** route affected selector to its already accepted authority; do not
restore competing production authorities.

## R7.P3 — Language, Subsystem, and Tool Portfolio Closure

**Primary outcome:** every optional or leaf component is a governed product,
plugin/provider, test tool, or archive.

**Scope:**

- complete language and subsystem adoption records;
- verify typed effects, lifecycle, limits, fixtures, and support state for each
  retained pack;
- decouple Gym, Assess, Wiki, deploy tooling, symbolic analysis, and other tools
  from production-private implementation state;
- execute approved retirement/archive decisions.

**Exit evidence:** no unowned leaf, no unlabeled experimental public selector,
conformance reports for retained packs, and accepted archive/removal ledger.

**Rollback:** restore a removed selector only through its declared compatibility
route; otherwise return remediation to R2 or R6.

## R7.P4 — Supported-Profile Conformance

**Primary outcome:** every advertised profile is independently reproducible.

**Scope:**

- build, test, document, configure, package, start, exercise, upgrade, and roll
  back every profile from its manifest;
- verify capability absence behavior for unselected packs;
- run security, cancellation, overload, malformed-input, durability, recovery,
  and multi-generation tests required by each selected capability.

**Exit evidence:** source-stamped profile evidence packs with no excluded
component in their dependency or package closures.

**Authority:** existing accepted profile authorities only.

## R7.P5 — Legacy Retirement and Governance Activation

**Primary outcome:** no temporary architecture remains accidentally permanent.

**Scope:**

- remove superseded adapters, paths, feature flags, configuration keys,
  deployment assets, docs, and test-only production seams after their gates;
- accept any intentional long-term adapter with owner, version, fixtures, and
  maintenance budget;
- enable CI architecture/profile/API/exception-expiry checks and ownership
  review policy.

**Exit evidence:** zero expired migration items, final retirement ledger, active
CI policy, and a scheduled repeatable portfolio review.

**Rollback:** use per-component archive or compatibility procedure; never
reintroduce an unversioned duplicate authority.

## R7.P6 — Final Convergence Gate

**Primary outcome:** independent acceptance of the sustainable product
portfolio.

**Scope:** replay R7 evidence on the frozen revision; sample profile builds and
failure cases independently; verify all parent-wave handoffs and sign the final
portfolio manifest.

**Exit evidence:** all R7 contract exit criteria pass. Any declared long-term
compatibility boundary satisfies its explicit ownership, versioning, fixture,
and maintenance acceptance criteria and is therefore not an exception waiver.

**Authority:** certification only. Failure returns to a named remediation wave
and blocks portfolio completion.

## Expected Agent Goal Sizing

R7 is expected to require **8–14 bounded agent goals**. The range assumes most
cleanup occurred in R0–R6. If R7 needs broad implementation work, the owning
earlier wave was not complete and must be remediated rather than hiding the work
inside certification.
