# Workspace Convergence and Sustainable Architecture

Status: **Proposed**
Date: **2026-08-26**
Roadmap contract family: **0.3**

## Decision Summary

OpenMainframe will treat the next roadmap cycle as a portfolio-wide architectural
refactor, not as a sequence of feature additions followed by cleanup. Every
workspace crate participates from R0 onward and must end in one of four states:

1. a supported product-profile component behind an owned, versioned boundary;
2. an optional plugin, provider, protocol adapter, or independently packaged tool;
3. internal test infrastructure with deterministic public test seams; or
4. deprecated and removed or archived after its compatibility obligations close.

This is one refactor program, but it is not one atomic rewrite or one changeset.
Contracts are introduced first; implementations run in shadow or through adapters;
authority moves at named gates; legacy paths are removed only after rollback and
compatibility evidence exists. R0 through R6 perform the convergence work. R7 is
the portfolio certification and closure gate, not the first time cleanup occurs.

## Why This Specification Exists

The execution and IR designs deliberately migrate selected paths first. That is
necessary for safe authority changes, but it does not by itself guarantee that the
whole workspace is coherent when R6 completes. The repository also contains
protocol duplication, broad composition roots, isolated language and subsystem
models, overlapping configuration, optional products in default dependency
closures, and crates with no current workspace consumer.

This specification owns the missing portfolio contract: product profiles, crate
disposition, dependency direction, authority uniqueness, maintainability, and
retirement. Detailed runtime and compiler semantics remain owned by the
[execution backend](execution-backend.md) and
[compiler/IR architecture](plugin-ir-architecture.md).

## Point-in-Time Audit Baseline

The planning audit on 2026-08-26 found:

- 44 workspace crates, approximately 330,000 lines of Rust, and approximately
  7,000 Rust test attributes. These are sizing signals, not quality scores, and
  must be regenerated at each portfolio gate.
- `open-mainframe-zosmf` directly composes a wide set of subsystem instances and
  has 15 internal dependencies, making it both gateway and application container.
- 20 crates have no incoming dependency from another workspace crate. A leaf is
  not automatically dead: it may be a binary, an intended plugin, or an
  independently packaged product. It does require an explicit owner, profile,
  consumer or entry point, and support decision.
- A static source-reference scan identified roughly 17 suspicious internal
  dependency edges, including edges around DRDA, TUI, Wiki, z/OSMF, dataset,
  COBOL, and MVS. Static scans can miss build scripts, macros, feature-specific
  code, and intentional facade re-exports, so R1 must verify each edge
  mechanically before removal.
- TN3270/TN3270E behavior exists in TUI, networking, and z/OSMF code. The target
  is one protocol-state authority with listener and frontend adapters.
- DRDA and the z/OSMF DB2 route include mock or generic-success behavior. No mock
  success may remain on a production-supported selector.
- Only a subset of language implementations currently use
  `open-mainframe-lang-core`; language parsers, values, interpreters, and errors
  are otherwise largely local. Convergence therefore requires an adoption
  matrix, not an assumption that every language can immediately share one IR.
- Configuration is represented by many local `Config` types and at least two
  user-facing formats with overlapping concerns. Formats may remain plural, but
  the supported product schema and precedence rules need one authority.
- Several registries are local to their subsystem. Local implementation
  registries may remain, but capability discovery and lifecycle cannot depend on
  competing global authorities.
- Large modules exist in runtime and language parsers. File length is not a
  defect by itself; unowned public surfaces, mixed responsibilities, excessive
  compile fan-out, and changes that cannot be reviewed independently are defects.
- Test maturity is uneven. Supported profile gates must be based on behavior,
  failure, security, and compatibility evidence rather than aggregate test count.

These findings are hypotheses to validate in R0 and R1, not permission to delete
code merely because a scan reports no consumer.

### Audit candidates requiring mechanical confirmation

The following crates had no incoming workspace dependency in the point-in-time
Cargo graph:

```text
open-mainframe-adabas       open-mainframe-clist       open-mainframe-crypto
open-mainframe-deploy       open-mainframe-easytrieve  open-mainframe-focus
open-mainframe-gym          open-mainframe-hlasm       open-mainframe-idms
open-mainframe-ims          open-mainframe-ispf        open-mainframe-mq
open-mainframe-mvs          open-mainframe-natural     open-mainframe-networking
open-mainframe-pgmmgmt      open-mainframe-pli         open-mainframe-symbolic
open-mainframe-syscmd       open-mainframe-uss
```

Several are binaries, libraries intended for future composition, or valid
optional product candidates. R0 must inspect package targets and external users;
R1 decides packaging or retirement only after that evidence exists.

The static source-reference scan flagged these Cargo edges for verification:

| Dependent | Candidate dependency with no direct source reference in the scan |
|---|---|
| `open-mainframe` | `open-mainframe-precompilers` |
| `open-mainframe-cobol` | `open-mainframe-encoding`, `open-mainframe-runtime` |
| `open-mainframe-dataset` | `open-mainframe-encoding` |
| `open-mainframe-drda` | `open-mainframe-db2`, `open-mainframe-racf` |
| `open-mainframe-mvs` | `open-mainframe-dataset`, `open-mainframe-racf` |
| `open-mainframe-tui` | `open-mainframe-cobol`, `open-mainframe-runtime` |
| `open-mainframe-wiki` | `open-mainframe-cics`, `open-mainframe-cobol`, `open-mainframe-dataset`, `open-mainframe-encoding`, `open-mainframe-jcl` |
| `open-mainframe-zosmf` | `open-mainframe-encoding`, `open-mainframe-smf` |

Before removing an edge, R1 must check all targets, features, tests, examples,
build scripts, macro/generated code, intentional facade re-exports, and the full
accepted profile matrix. A verified intentional edge is documented rather than
forced out to make the count reach zero.

## Target Outcomes

At convergence:

- every crate and public selector has a classification, owner, support state,
  profile membership, version policy, and retirement rule;
- core profiles build without optional UIs, compatibility protocols, portfolio
  tools, evaluation harnesses, or mock implementations;
- each protocol, configuration family, public capability family, and durable
  state class has one observable authority;
- language and subsystem reuse occurs at stable contracts, typed effects, host
  services, artifacts, and conformance tests without erasing valid semantic
  differences;
- all verified unused dependency edges and reverse-layer edges are gone;
- supported profiles have reproducible build, test, documentation, security,
  overload, and recovery evidence; and
- extension does not require adding broad state to z/OSMF or another composition
  root.

## Non-Goals

- A big-bang rewrite or simultaneous authority switch across 44 crates.
- A single giant crate, registry, configuration struct, or universal interpreter.
- Forced lowering of every language or subsystem into identical internal types.
- Splitting files or creating crates to satisfy arbitrary size targets.
- Retaining every current crate merely because it exists, or deleting a leaf
  merely because it lacks a current internal consumer.
- Treating R7 as time reserved to repair architecture deferred by earlier waves.

## Product Profiles

Profiles are named, reproducible dependency and capability closures. R0 approves
their exact manifests; the following model is the planning baseline.

| Profile family | Intended contents | Excluded by default |
|---|---|---|
| Core server | z/OSMF gateway, execution contracts, required language/workflow paths, JES/dataset/security and explicitly selected core services | TUI, DRDA, Wiki, Gym, deployment tooling, unsupported language/subsystem packs |
| Compatibility | DRDA, TN3270 listeners, terminal clients and compatibility facades selected by deployment | Mock handlers and unowned protocols |
| Language packs | COBOL plus individually selected PL/I, REXX, CLIST, HLASM, Easytrieve, Natural, FOCUS and precompiler capabilities | Unselected frontends and tool-only analyzers |
| Subsystem packs | Individually selected CICS, DB2, IMS, IDMS, ADABAS, MQ, USS, ISPF, MVS and related providers | Unselected emulators and protocol frontends |
| Analysis and test | Assessment, symbolic analysis, Gym fixtures, differential and conformance harnesses | Production authority and ambient access to private server internals |
| Operations and delivery | Deployment generation, configuration format adapters, packaging and operator tools | Runtime business logic and authoritative in-process state |

Every advertised distribution names its profile manifest, feature set, plugin
generations, configuration-schema version, and conformance evidence. Cargo
features may implement part of a profile, but a profile is a product contract,
not merely a feature flag.

## Workspace Portfolio Matrix

The matrix gives every current crate an initial target boundary. R0 confirms or
amends these decisions with owners and evidence. “Optional” means absent from the
core-server closure unless explicitly selected.

| Crate | Initial classification | Target boundary and disposition |
|---|---|---|
| `open-mainframe` | Product composition and CLI | Thin composition root and CLI over public execution/program APIs; no dependency on analysis, test, or optional adapter implementations |
| `open-mainframe-adabas` | Optional subsystem provider | Database capability plugin with conformance fixtures; integrate into a selected pack or archive if no owned consumer exists |
| `open-mainframe-assess` | Analysis tooling | IR/HIR analysis consumer; retire duplicated language semantics after accepted report parity |
| `open-mainframe-cics` | Online transaction provider | Typed execution and host-service capabilities; no stringly global dispatch or gateway-owned business state |
| `open-mainframe-clist` | Optional language pack | Frontend/runtime plugin using shared contracts where valid; support with fixtures or archive as an independent experiment |
| `open-mainframe-cobol` | Primary language pack | Reference frontend and HIR/MIR adopter with explicit legality and backend contracts |
| `open-mainframe-crypto` | Optional security provider | Capability-limited cryptographic service; no ambient key or process-global authority |
| `open-mainframe-dataset` | Core host-service provider | Typed dataset/catalog authority separated from local-filesystem storage adapters |
| `open-mainframe-db2` | Database provider | Typed database service and execution semantics; DRDA and REST remain adapters, never alternate SQL authorities |
| `open-mainframe-deploy` | Operations and delivery tool | Outside runtime closure; consumes canonical profile/config schema and emits deployment formats |
| `open-mainframe-drda` | Optional protocol adapter | Default-off adapter to typed DB2 and security services; implement and test real semantics or retire |
| `open-mainframe-easytrieve` | Optional language pack | Independent frontend behind compiler/runtime contracts; retain only with owned profile and fixtures |
| `open-mainframe-encoding` | Foundation | Dependency-light canonical encoding/data representation service |
| `open-mainframe-focus` | Optional language/data pack | Plugin boundary with explicit database/runtime effects; integrate with an owned profile or archive |
| `open-mainframe-gym` | Test and evaluation infrastructure | Deterministic black-box harness over public APIs; never a production dependency or authority |
| `open-mainframe-hlasm` | Optional language pack | Frontend/object producer integrated through artifact and program-management contracts |
| `open-mainframe-idms` | Optional subsystem provider | Database plugin with typed effects and conformance fixtures; integrate or archive if unowned |
| `open-mainframe-ims` | Optional subsystem provider | IMS DB/TM capabilities behind host/execution contracts and a selected profile |
| `open-mainframe-ispf` | Optional interactive subsystem | Dialog/panel provider over neutral terminal/session contracts; no terminal protocol ownership |
| `open-mainframe-jcl` | Core workflow frontend | JCL/JES orchestration through program, artifact, host-service, and bounded execution contracts |
| `open-mainframe-jes2` | Core batch provider | Job/spool lifecycle capability with durable ownership and scheduler integration |
| `open-mainframe-lang-core` | Foundation contract | Dependency-light shared frontend diagnostics/source/contracts; adoption is explicit per language |
| `open-mainframe-mq` | Optional subsystem provider | Typed queue host service and plugin; selected profile determines durability obligations |
| `open-mainframe-mvs` | Optional system-service provider | Supervisor/allocation/console effects behind typed host capabilities, not direct cross-subsystem ownership |
| `open-mainframe-natural` | Optional language pack | Language frontend/runtime with typed database effects; owned support or archive decision |
| `open-mainframe-networking` | Protocol and transport provider | Own listeners/transports; shared TN3270 protocol state lives in one dependency-light authority |
| `open-mainframe-parmlib` | Configuration/system provider | Typed system-parameter capability; local symbol registries remain internal to the provider |
| `open-mainframe-pgmmgmt` | Artifact/program provider | Binder, loader, OBJ/LMOD services behind the program/artifact boundary |
| `open-mainframe-pli` | Optional language pack | Frontend/runtime plugin with explicit shared-contract adoption and compatibility fixtures |
| `open-mainframe-precompilers` | Compiler phase plugins | Precompile transforms registered in the compiler pipeline; absent from unrelated runtime closures |
| `open-mainframe-racf` | Core security provider | Canonical identity, SAF, authentication and authorization host service |
| `open-mainframe-rexx` | Interactive language pack | REXX plugin selected by TSO/automation profiles through execution contracts |
| `open-mainframe-runtime` | Legacy execution implementation | Decompose behind dependency-light execution API/host contracts; cease owning cross-language public IR |
| `open-mainframe-smf` | Observability/accounting provider | Typed event sink/source behind audit and telemetry contracts |
| `open-mainframe-sort` | Program/service provider | Shared sort capability with explicit record/data contracts and bounded resource use |
| `open-mainframe-symbolic` | Analysis backend | Shared-IR symbolic backend with bounded outcomes; no second COBOL semantic authority |
| `open-mainframe-syscmd` | Operations subsystem provider | Typed command capability over system services; optional operations profile |
| `open-mainframe-tso` | Interactive coordinator | Session and command orchestration over neutral terminal, JES, language, and dataset capabilities |
| `open-mainframe-tui` | Optional client frontend | Ratatui/Crossterm client only; terminal model and TN3270 protocol state move to neutral contracts |
| `open-mainframe-uss` | Optional subsystem provider | Filesystem/process host capabilities with explicit isolation and persistence policy |
| `open-mainframe-utilities` | Program provider | Versioned utility capabilities resolved through the common program service |
| `open-mainframe-wiki` | Standalone portfolio tool | Independently packaged consumer of public analysis/schema outputs, or deprecated and archived |
| `open-mainframe-wlm` | Scheduling policy provider | Policy/classification capability consumed through the scheduler contract |
| `open-mainframe-zosmf` | API gateway and composition root | Stateless route adapters plus profile composition; shrink broad `AppState` to scoped capabilities |

An additional crate is justified only when it enforces dependency direction,
ownership, versioning, isolation, or independent release. Candidate boundaries
such as an execution API/host, compiler IR, terminal protocol core, or canonical
configuration schema are architectural roles, not a mandate to create one crate
per noun.

## Dependency and Packaging Rules

1. Foundation and public contract crates remain dependency-light and do not
   depend on product composition, UI, test, deployment, or concrete adapter code.
2. Core-server builds do not pull optional protocol/UI adapters, analysis tools,
   evaluation harnesses, or deployment generators.
3. Every internal dependency edge must be evidenced by compiled source use,
   intentional public re-export, generated code, build integration, or an
   approved time-bounded migration exception.
4. Dependency cycles and reverse-layer edges are prohibited. CI checks the
   accepted architecture graph and profile feature matrix.
5. Public types cross boundaries through contract crates or owned provider APIs,
   not by exposing a gateway's application-state container.
6. Private registries may implement a provider, but global discovery,
   lifecycle, identity, and health use the logical capability registry.
7. Feature/profile combinations fail explicitly when required providers are
   absent. Optional capability absence is not converted to generic success.
8. Each exception records owner, reason, affected profiles, removal condition,
   and expiry gate.

## Authority Convergence

### Protocols and terminals

- TN3270/TN3270E negotiation, framing, field semantics, AID values, and protocol
  state have one neutral authority. Networking owns transport/listeners; z/OSMF
  owns REST/WebSocket adaptation; TUI owns local presentation.
- DRDA, if retained, is a protocol adapter over typed DB2, identity, and
  authorization services. It does not own a second SQL engine and cannot return
  mock success on a supported profile.
- Protocol conformance, malformed-input, authentication, cancellation, and
  bounded-resource fixtures gate support.

### Configuration

One versioned product configuration schema owns fields, validation, defaults,
precedence, secret references, profile selection, and deprecation. TOML, YAML,
environment variables, Helm/Kubernetes values, and future formats are adapters
to that schema. Provider-private tuning may remain local when it is namespaced,
owned, and cannot contradict product-level policy.

### Registries

OpenMainframe does not need one giant registry implementation. It needs one
logical authority for capability identity, version, lifecycle, health, trust,
and resolution. Subsystem registries may remain private implementation details
when they cannot shadow global resolution or leak unversioned behavior across a
public boundary.

## Language and Subsystem Convergence Rules

Each language and subsystem receives an adoption record covering:

```text
profile and owner
source/parser and diagnostic contracts
HIR/MIR or independent semantic model decision
typed effects and required host capabilities
artifact and execution route
compatibility/conformance fixtures
state, isolation, and resource limits
public/beta/internal support level
retention or retirement gate
```

Shared HIR/MIR adoption is required where it eliminates competing semantics and
has proven expressive parity. Independent representations may remain where the
domain semantics differ, provided they use common diagnostics, artifacts,
execution lifecycle, typed host effects, limits, and plugin metadata. “Common
platform” therefore means common contracts and observable authority, not forced
internal uniformity.

## Maintainability and Ownership Gates

- Every crate, public module, protocol/config authority, and supported capability
  has a named owner and backup reviewer.
- Large or high-churn modules have an ownership note and a decomposition review
  when they mix authorities, create excessive compile fan-out, or cannot be
  changed and tested independently. Line count triggers review; it does not
  require arbitrary splitting.
- Public APIs state support level, compatibility policy, and deprecation window.
- Dependency cost, build time, binary contribution, unsafe code, and feature
  combinations are measured per profile.
- New cross-crate APIs require contract tests and generated or linked
  documentation. Duplication is allowed temporarily only through an inventoried
  migration adapter.

## Test and Support Gates

A capability is **supported** only when the selected profile demonstrates:

- contract and compatibility behavior, including negative and unsupported paths;
- integration through its public boundary rather than private state injection;
- authentication/authorization evidence when applicable;
- cancellation, overload, malformed-input, and resource-limit behavior;
- state recovery and idempotency when it owns durable effects;
- deterministic fixtures and versioned test data; and
- absence of mock, unconditional, or generic-success production paths.

Test count alone is never promotion evidence. A crate may remain experimental
with narrower evidence only when it is excluded from supported profiles and is
labelled accordingly.

## Retirement Protocol

A component or selector can be removed only when:

1. its consumers, entry points, configuration, and public selectors are
   inventoried;
2. replacement or removal behavior is covered by accepted fixtures;
3. selected profiles no longer depend on it, including optional feature builds;
4. authority has moved and rollback has been exercised, or product removal was
   explicitly approved;
5. deprecation notice and compatibility window have completed where applicable;
6. documentation, examples, deployment assets, and generated manifests no
   longer advertise it; and
7. archived source or migration notes are retained when required for provenance.

## Wave Obligations

| Wave | Workspace-convergence obligation |
|---|---|
| R0 | Generate the 44-crate portfolio manifest, product profiles, owner/support matrix, mock inventory, dependency graph, and authority-duplication catalog |
| R1 | Establish dependency-light contracts and composition boundaries; remove verified unused/reverse edges; make optional packaging real |
| R2 | Approve and begin every language/analysis adoption record; prove shared semantics on the selected vertical slice without creating a second authority |
| R3 | Converge host-service, protocol, terminal/session, configuration, and scoped gateway boundaries for supported single-node profiles |
| R4 | Classify and externalize authoritative state for every advertised capability in distribution-ready profiles |
| R5 | Prove complete selected-profile topology, placement, recovery, and generation behavior across nodes |
| R6 | Move optional languages/subsystems/tools to governed plugin or independent-product boundaries and execute approved retirements |
| R7 | Burn down exceptions, verify the whole portfolio, retire remaining legacy authorities, and certify sustainable profile manifests |

## R7 Portfolio Exit Contract

R7 passes only when:

- every current or newly added crate is present in the portfolio manifest with
  owner, classification, support state, profiles, boundary, and retirement rule;
- there are no unverified internal dependency edges, prohibited cycles,
  reverse-layer edges, or unowned leaf products;
- every supported profile builds, tests, documents, and packages reproducibly
  without excluded components in its closure;
- protocol, configuration, registry/capability, program resolution, and durable
  state authority is unique for each accepted selector;
- no supported production path returns mock, unconditional, or generic success;
- all public surfaces are marked supported, beta, internal, deprecated, or
  archived, with corresponding evidence and policy;
- all migration adapters and exceptions are removed or explicitly retained as
  versioned long-term compatibility boundaries; and
- the final dependency graph, profile manifests, ownership map, conformance
  matrix, generated documentation, and retirement ledger are accepted.

If evidence fails, work returns to the wave that owns the deficient authority.
R7 does not invent a replacement architecture during certification.
