# R2 Contract — One Semantic Spine

Status: **Proposed**
Contract version: **0.3**
Indicative duration: **3–7 months**
Authority change: **Selected COBOL/CardDemo selectors after differential parity**
Phase plan: [R2 Phase Plan](phases/r2-phase-plan.md)

## Contract Outcome

R2 proves that OpenMainframe can share one semantic compiler representation
across a real COBOL/CICS workload. It introduces a dependency-light multi-level
IR, typed operation schemas, full legality checks, a COBOL/CICS vertical slice,
and a shared concrete interpreter. Selected authority moves only after shadow
and differential evidence demonstrates parity with the legacy path.

R2 also proves that non-executable consumers can share the same semantic spine:
accepted assessment fields move through an analysis-mode HIR/IR consumer before
the standalone assessment implementation may lose authority.

R2 optimizes semantic integrity and development scalability. It does not promise
complete COBOL coverage, immediate native speed, broad language migration, or
multi-node execution.

## Dependencies

- Completed [R0 Truthful System](r0-truthful-system.md) semantic coverage and
  fixtures.
- R1 compiler/executor/artifact contracts accepted and stable enough for an IR
  artifact and executor.
- Named CardDemo selectors that can be routed independently.

Related specifications:

- [Platform Roadmap](../platform-roadmap.md#r2--one-semantic-spine)
- [Plugin-Oriented Compiler and Multi-Level IR Architecture](../plugin-ir-architecture.md)
- [Language and subsystem convergence rules](../workspace-convergence.md#language-and-subsystem-convergence-rules)
- [R1 One Execution Spine](r1-one-execution-spine.md)

## Scope

### In scope

- Generic arena-based IR object model, locations, regions, blocks, values,
  types, attributes, verification, textual dump, and versioned schema registry.
- Compiler phase envelopes, diagnostics, target legality, pass manager, lowering
  planner, and legalization report.
- Minimal `om.core`, `om.cf`, `om.mem`, `om.decimal`, COBOL HIR, and CICS service
  dialects.
- COBOL storage/layout, literals, MOVE, selected arithmetic, IF, PERFORM, CALL,
  and selected EXEC CICS operations.
- COPY/precompiler source provenance.
- Shadow lowering and differential comparison with `SimpleProgram`.
- Shared Core MIR concrete interpreter for the vertical slice.
- IR executable artifact and executor capability.
- Analysis-mode IR/HIR consumer for accepted assessment metrics, features,
  dependencies, diagnostics, and completeness metadata.
- Symbolic interfaces and initial migration for operations covered by shared
  MIR.

### Out of scope

- Complete COBOL, PL/I, REXX, JCL, HLASM, or 4GL migration.
- Using one universal enum for all language/subsystem operations.
- Making LLVM/native artifacts the default.
- Replacing every CICS command and host-service path.
- Stable external IR bytecode/plugin compatibility commitment.
- Removing the legacy interpreter for non-migrated selectors.
- Rebuilding the portfolio wiki or treating handwritten/generated portfolio
  pages as the authoritative R6 schema documentation system.

## Entry Contract

R2 may enter **In Progress** when:

- R0 semantic inventory identifies the selected vertical-slice constructs and
  expected behavior.
- R1 defines the artifact media type, executor selection, event identity, and
  rollback selector needed by an IR executor.
- The vertical slice has named source programs, copybooks, BMS/data fixtures,
  and expected observable outcomes.
- IR core/compiler API dependency rules are accepted.
- Each proposed operation/type schema has an owner.
- The legacy path remains authoritative during shadow lowering.

## Mandatory Deliverables

### D2.1 IR and compiler foundation

Provide:

- generic operation, region, block, value, type, attribute, and location arenas;
- namespaced/versioned dialect and operation identity;
- generated typed builders/wrappers for registered schemas;
- declarative and custom verifier dispatch;
- source, fused, expansion, and generated provenance;
- deterministic textual dump and round-trip representation for the selected
  dialect set;
- compiler phase envelopes and diagnostics;
- immutable dialect/pass registry snapshots;
- legality profiles, conversion descriptors, and full legalization.

Foundation crates must not depend on COBOL, CICS, z/OSMF, LLVM, or mutable
runtime subsystem implementations.

### D2.2 Mainframe semantic core

Define the minimum reusable semantics for:

- CFG/regions, calls, returns, conditions, and program-control outcomes;
- storage regions, offsets, extents, aliases, group layout, and references;
- packed/zoned/display/binary decimal precision, scale, rounding, and overflow;
- strings/bytes with length, encoding, and collation;
- parameter modes and shared storage identity;
- typed service effects, conditions, suspension, and transfer.

Language semantics remain in COBOL dialect operations when reuse is not proven.

### D2.3 COBOL HIR vertical slice

The initial slice includes at least:

- selected DATA DIVISION storage/layout required by fixtures;
- literals and qualified/subscripted/reference-modified values used by fixtures;
- MOVE and selected arithmetic;
- IF and selected EVALUATE if required;
- PERFORM and paragraph/control flow used by fixtures;
- CALL and parameter modes used by fixtures;
- EXEC CICS SEND/RECEIVE/READ and program-control operations required by one
  complete CardDemo transaction;
- explicit unsupported diagnostics for encountered non-slice operations.

Every emitted executable HIR operation has a registered schema, source
provenance, effects, verification, and an execution/lowering route.

### D2.4 Typed CICS service operations

Replace stringly semantics inside the selected IR path with typed operations
that declare:

- operand and result types;
- storage-reference ownership/alias behavior;
- EIBRESP/EIBRESP2 and condition mapping;
- terminal/file/program effects;
- suspension or transfer behavior;
- required runtime-provider capability/version.

Adapters may call existing bridge/runtime code, but raw global state is not
exposed to compiler passes.

### D2.5 Shadow lowering and differential harness

For selected programs, produce both:

```text
COBOL AST -> legacy lowering -> SimpleProgram -> legacy execution
COBOL AST -> COBOL HIR -> Core MIR -> shared execution
```

Compare:

- symbols and layouts;
- initial/final storage values;
- control-flow and program-transfer outcomes;
- terminal screens and field data;
- file/queue/service effect sequence;
- EIB response/condition behavior;
- stdout/stderr/return code;
- diagnostics and source locations.

Shadow execution must suppress or isolate duplicate external side effects.

### D2.6 Shared concrete interpreter

- Execute Core MIR and selected service operations.
- Enforce type, storage, call-depth, output, and step/resource limits.
- Preserve explicit condition, suspension, transfer, and ABEND outcomes.
- Publish an immutable IR executable artifact with declared dialect/import
  requirements.
- Emit phase/runtime events tied to R1 execution identity.

### D2.7 Assessment and Inspection Convergence

- Define a versioned analysis result envelope over verified HIR/IR, including
  source coverage, diagnostic completeness, semantic approximation, and report
  provenance.
- Reproduce the accepted R0 assessment oracle fields through IR analyses or
  explicitly version approved differences.
- Keep analyze-mode partial modules incapable of being published as executable
  artifacts.
- Route one named assessment selector/report through the new consumer with
  rollback to the standalone analyzer.
- Record which legacy heuristics remain useful adapters and which duplicate
  semantic logic can be removed after parity.

### D2.8 Symbolic convergence foundation

- Define symbolic operation interfaces over shared IR.
- Implement selected arithmetic, comparison, branches, storage, and calls.
- Provide sound service abstractions or explicit unsupported/approximation
  results.
- Ensure unsupported operations cannot produce a complete proof claim.

### D2.9 Language and Analysis Adoption Matrix

For COBOL, JCL, HLASM, PL/I, REXX, CLIST, Easytrieve, Natural, FOCUS,
precompilers, assessment, symbolic analysis, and future frontends:

- record owner/profile, diagnostic contract, HIR/MIR or independent-model
  decision, typed effects, host capabilities, artifact/execution route,
  fixtures, limits, support state, and retirement gate;
- require common lifecycle, diagnostics, artifacts, typed effects, limits, and
  plugin metadata even where forced shared lowering would distort semantics;
- prevent a retained local interpreter/analyzer from silently becoming a second
  authority for a migrated selector; and
- assign implementation to the appropriate R2 or R6 phase without claiming
  that the initial vertical slice has migrated every language.

## Invariants

- Frontend AST types do not become cross-plugin compiler contracts.
- No executable operation is emitted without a registered schema and route.
- Full legalization runs before an executable IR artifact is published.
- Unsupported operations are not dropped or converted to `Nop` in production.
- Effects, aliasing, conditions, and provenance survive lowering.
- Generic IR storage is paired with generated typed access for built-in code.
- Pass selection/order is deterministic for registry snapshot and target.
- The legacy path remains available until selector-specific authority passes.
- Source and copybook edits affect the next artifact resolution.
- Analysis reports identify partial/unknown semantics and cannot imply complete
  executable or proof coverage from a recoverable partial module.

## Exit Criteria

- IR core can build, verify, serialize/dump, parse/restore, transform, and reject
  invalid/illegal synthetic modules deterministically.
- The selected COBOL/CICS vertical slice produces no unknown executable
  operations.
- Full legalization failure reports operation, source location, attempted
  routes, and missing capability.
- One complete selected CardDemo transaction matches legacy behavior for screen,
  storage, file effects, EIB responses, conditions, and program control.
- Differential fixtures pass across the agreed normal and failure/condition
  paths.
- `--emit-ir` and `--explain-pipeline` account for selected compilation phases,
  passes, imports, and fallback.
- The shared interpreter respects configured resource bounds.
- Accepted assessment report fields are produced through the analysis pipeline
  with deterministic provenance, completeness, and approved parity/differences.
- Migrated symbolic operations consume shared MIR and report proof completeness.
- Canary selectors can move to the IR executor and roll back without data
  repair.
- D2.9 contains an accepted adoption/plugin/tool/retirement decision for every
  language and analysis component in the portfolio.

## Required Evidence

- IR/compiler dependency graph review.
- Schema, verifier, round-trip, malformed-input, and legality tests.
- Operation/type/effect reference generated from schemas.
- Vertical-slice support matrix.
- Shadow/differential reports for every promoted selector.
- Source-provenance examples through COPY/precompiler/lowering.
- Interpreter resource-limit and failure tests.
- Assessment oracle parity, deterministic report, and rollback evidence.
- Symbolic completeness/approximation reports.
- Language and analysis adoption matrix with explicit independent-model rationale.
- Canary rollout and rollback exercise.

## Stop-the-Line Conditions

R2 enters remediation when:

- A lowering loses observable effects, conditions, aliases, or source
  provenance.
- A new central enum/match must be edited for every external dialect operation.
- An unsupported operation can publish an executable artifact.
- Differential mismatch is normalized without a compatibility decision.
- Shadow execution duplicates production external effects.
- IR/plugin registration or pass ordering is nondeterministic.
- Malformed IR can trigger unbounded allocation or host access.

## Rollout Contract

1. Run HIR/MIR generation in analysis-only shadow mode.
2. Compare one accepted assessment report through the IR analysis consumer.
3. Enable shared interpreter with mocked/isolated service effects.
4. Run differential execution in CI and controlled environments.
5. Canary one named transaction/program selector.
6. Promote normal and failure/condition paths independently only when covered.
7. Retain legacy execution and assessment routing for at least one release.

## Rollback Contract

- Route selectors back to the legacy executable artifact/executor.
- Stop admission to the affected IR executor generation.
- Preserve IR artifacts and legalization/differential evidence.
- Do not migrate persistent subsystem data as part of an executor switch.
- Mark any incompatible cached IR artifact generation unavailable rather than
  rewriting it in place.

## Handoff

R3 receives:

- typed CICS and program-service operations;
- shared execution outcomes and service effects;
- Core MIR interpreter workloads for performance/isolation tests;
- operation/resource metrics and adapter boundaries needing host-service
  convergence.

The symbolic/native tracks receive:

- stable selected Core MIR semantics;
- legality and target-profile infrastructure;
- differential fixtures using the shared interpreter as reference.

Assessment/tooling owners receive the versioned analysis result envelope,
accepted parity report, and the residual legacy-heuristic inventory. The
portfolio wiki is not a prerequisite or replacement for this contract.

## Definition of Done

- [ ] Entry contract satisfied.
- [ ] D2.1–D2.9 delivered for the selected slice and complete adoption matrix.
- [ ] All exit criteria passed.
- [ ] No silent unsupported behavior exists in promoted selectors.
- [ ] Canary and rollback exercised.
- [ ] Compatibility, quality, operations, security, and IR owners approved.
- [ ] R3 owner accepted the handoff.
- [ ] Contract status changed to **Completed**.
