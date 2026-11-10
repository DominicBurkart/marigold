# marigold workspace

Marigold is an imperative DSL for async stream pipelines, usable standalone or inside Rust via the `m!` macro. This is a Cargo workspace with these crates: `marigold` (public facade, features such as `tokio`, `io`, `otel`, `async-std`, `cli`), `marigold-macros`, `marigold-grammar`, `marigold-lsp`, `marigold-impl`, plus `tests/` and `examples/`.

The Rust toolchain is the stable channel from `rust-toolchain.toml` with rustfmt and clippy. Set `MARIGOLD_WORKSPACE_PATH` to the `marigold` crate directory (`/workspace/marigold` in the container); the image sets it.

Checks the project enforces:
- `cargo fmt --all -- --check`
- `cargo clippy --all-features -- -D warnings`
- `cargo test --all-features --doc`
- `cargo test --all-features`
- CI also runs `cargo test` under feature sets `tokio,io`, `otel`, `tokio,io,otel`, `otel-otlp`, `tokio` and `async-std`.

`README.md` and `marigold/README.md` must stay byte-identical.

Do not edit `.inchworm/**`, `.github/**`, `.cargo-husky/**`, `deny.toml`, `codecov.yml`, `flake.nix` or `.devcontainer/**`. Prefer the container tools `cargo_check` and `cargo_test` while iterating, and keep changes small and covered by tests.
