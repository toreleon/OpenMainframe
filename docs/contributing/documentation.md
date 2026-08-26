# Documentation Guide

This guide keeps OpenMainframe documentation navigable and tied to the code it
describes.

## Ownership

- Keep the root `README.md` concise and focused on evaluating and starting the
  product.
- Put cross-crate design, runtime flows, operational procedures, and REST maps
  under `docs/`.
- Keep crate-specific behavior in `crates/<name>/README.md`.
- Put exact item-level API documentation in Rustdoc comments beside the item.
- Treat source code, tests, `Cargo.toml`, and `zosmf.toml` as the evidence for
  documentation claims.

## Crate README Template

Every workspace member should have a README with the following sections when
they apply:

1. Purpose — the crate boundary and the z/OS concept it models.
2. Capabilities — behavior implemented today, stated without roadmap claims.
3. Architecture — important modules and the control or data flow between them.
4. Public API — primary types, traits, functions, binaries, or commands.
5. Integration — internal workspace dependencies and known consumers.
6. Examples — a minimal compiling or test-backed usage example.
7. Testing — focused commands and important test locations.
8. Limitations — material compatibility gaps visible in the implementation.
9. Related documentation — links to the crate map and relevant system pages.

Omit empty sections. Prefer a short, verified README over an exhaustive feature
list that becomes stale.

## Style and Links

- Use repository-relative Markdown links so they work on GitHub and in local
  checkouts.
- Link to a directory or source file instead of quoting large code blocks.
- Name Rust types and modules exactly as they appear in source.
- Distinguish implemented behavior from compatibility goals.
- Use Mermaid only when a relationship or flow is materially clearer as a
  diagram.
- Avoid copying endpoint, option, or feature lists into multiple pages; link to
  their canonical reference instead.

## Review Checklist

Before merging documentation changes:

- Confirm every workspace member is present in the crate map and has a README.
- Check relative links and anchors.
- Compare commands with the current Cargo package and binary names.
- Compare configuration keys with the deserialization structs and example TOML.
- Compare REST routes with the Axum router definitions.
- Run `cargo fmt --check` when Rust examples or doc comments changed.
- Run focused doctests or package tests for any executable examples.
