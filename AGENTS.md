# Repository Guidelines

## Project Structure & Module Organization

This Rust 2021 workspace keeps subsystems in `crates/open-mainframe-*`; shared
language and runtime foundations are in
`open-mainframe-lang-core`, `open-mainframe-encoding`, and
`open-mainframe-runtime`. The `open-mainframe` crate provides the main CLI, while
`open-mainframe-zosmf` provides the REST server. Keep crate-specific documentation
in that crate's `README.md`.

Integration tests belong in `crates/<crate>/tests/`; smaller unit tests stay beside
their implementation in `#[cfg(test)]` modules. Use `data/` for fixtures,
`examples/` for samples, `docs/` for cross-crate documentation, and `deploy/` or
`packaging/` for distribution assets. Never commit `target/` or runtime data.

## Build, Test, and Development Commands

- `cargo build --workspace` — compile every workspace member in debug mode.
- `cargo build --release` — produce optimized CLI and service binaries.
- `cargo run --release -p open-mainframe-zosmf --bin zosmf-server` — start the
  local z/OSMF-compatible server on the configured endpoint.
- `cargo test --workspace` — run the complete unit and integration suite.
- `cargo test -p open-mainframe-jcl` — iterate on one crate's tests.
- `cargo fmt --all -- --check` — verify formatting without modifying files.
- `cargo clippy --all-targets -- -D warnings` — run the same strict lint policy as CI.
- `cargo doc --no-deps` — validate workspace API documentation.

Rust 1.82 plus the `rustfmt` and `clippy` components are pinned in
`rust-toolchain.toml`.

## Coding Style & Naming Conventions

Run `cargo fmt --all` before submitting; formatting uses a 100-column maximum.
Follow standard Rust naming: `snake_case` for modules, functions, and tests;
`UpperCamelCase` for types and traits; `SCREAMING_SNAKE_CASE` for constants.
Document public APIs, prefer typed errors over panics, and preserve crate boundaries.

## Testing Guidelines

Use Rust's built-in test harness and `#[tokio::test]` for asynchronous behavior.
Name tests after observable behavior, for example `submit_job_returns_spool_files`.
Add regression coverage for behavior changes and representative fixtures for COBOL,
JCL, or mainframe data. No numeric coverage threshold is enforced; CI requires all
workspace tests, lints, formatting, and docs to pass.

## Commit & Pull Request Guidelines

Recent history uses short, imperative, sentence-case subjects such as
`Handle UTF-8 fixed-format COBOL lexing`; keep commits focused on one concern.
Pull requests should explain the problem and solution, list affected crates, link
issues, and report validation commands. Include screenshots for TUI changes and
request/response examples for REST changes. Update documentation when public
behavior or configuration changes.

## Security & Configuration

Treat credentials in `zosmf.toml` and README examples as development-only. Do not
commit real secrets, host-specific paths, generated datasets, or authentication
tokens; inject deployment values through environment-specific configuration.
