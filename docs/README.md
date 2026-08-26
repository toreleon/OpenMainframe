# OpenMainframe Documentation

This directory is the documentation hub for the OpenMainframe workspace. It
explains how the 44 Rust crates fit together, how to run the system, and where
to find the canonical details for a subsystem.

## Start Here

- [Architecture overview](architecture/overview.md) — system boundaries,
  dependency layers, and the main execution paths.
- [Scalable execution backend](architecture/execution-backend.md) — proposed
  plugin contracts, scheduling, isolation, state model, and migration plan.
- [Platform roadmap](architecture/platform-roadmap.md) — R0–R7 gated refactor
  sequence, promotion evidence, scale, and ecosystem milestones.
- [Workspace convergence](architecture/workspace-convergence.md) — all-crate
  product profiles, dependency/authority rules, ownership, and retirement gates.
- [Crate map](architecture/crate-map.md) — all workspace crates grouped by
  responsibility, with links to their source-level documentation.
- [Getting started](guides/getting-started.md) — build, run, and verify a local
  OpenMainframe server.
- [Configuration reference](reference/configuration.md) — configuration sources,
  precedence, and the supported TOML sections.
- [z/OSMF API reference](reference/zosmf-api.md) — REST surface and the crates
  behind each endpoint family.
- [Documentation guide](contributing/documentation.md) — ownership, templates,
  link rules, and the review checklist.

## Information Architecture

OpenMainframe documentation has four layers:

| Layer | Location | Purpose | Canonical for |
|---|---|---|---|
| Product | [`README.md`](../README.md) | Project value, quickest successful run, top-level navigation | First contact |
| System | [`docs/`](.) | Cross-crate concepts, runtime flows, operations, and API maps | How components work together |
| Crate | [`crates/*/README.md`](../crates) | Public surface, internal modules, behavior, and extension points | One crate's implementation |
| API | Rustdoc from source | Items, signatures, types, and examples close to code | Exact programmatic interface |

Documentation should point downward instead of duplicating lower-level detail.
For example, an architecture page describes why CICS depends on dataset and
encoding services, while the CICS README documents its modules and commands.
Rustdoc remains authoritative for an individual function signature.

## Documentation Areas

```text
docs/
├── README.md                  # This navigation hub and ownership model
├── architecture/             # Stable cross-crate boundaries and flows
│   ├── overview.md
│   ├── crate-map.md
│   ├── execution-backend.md
│   ├── plugin-ir-architecture.md
│   ├── workspace-convergence.md
│   ├── platform-roadmap.md
│   └── roadmap/
├── guides/                   # Goal-oriented procedures
│   └── getting-started.md
├── reference/                # Factual lookup material
│   ├── configuration.md
│   └── zosmf-api.md
└── contributing/             # Maintainer conventions
    └── documentation.md
```

The structure is intentionally small. Add a new page only when its subject
crosses crate boundaries or would otherwise make a crate README difficult to
navigate.
