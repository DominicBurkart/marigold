# Marigold – agent / contributor quick-start

## Dev environment setup (one-liner)

From the repo root:

```sh
sh dev-install.sh
```

This installs Rust (if absent), updates to MSRV if needed, adds `rustfmt` and
`clippy`, installs `cargo-audit` and `cargo-deny`, then does a full workspace
build to verify everything works.

## Common commands

| Task | Command |
|------|---------|
| Run all tests | `cargo test --all-features` |
| Run tests (tokio + io) | `cargo test --features tokio,io` |
| Check formatting | `cargo fmt --all -- --check` |
| Lint | `cargo clippy --all-features -- -D warnings` |
| Security audit | `cargo audit` |
| Licence compliance | `cargo deny check` |
| WASM build | `cargo build --target wasm32-unknown-unknown` |
| Benchmarks | `cargo bench --bench keep_first_n --features tokio` (in `marigold-impl/`) |

## Notes

- `rust-toolchain.toml` pins the channel to **stable** with `rustfmt` and
  `clippy` components — `rustup` picks this up automatically.
- The `README.md` at the workspace root and `marigold/README.md` must stay
  identical; CI checks this with `cmp`.
- Markdown files are linted by `npx markdownlint-cli2`; run
  `npx --yes markdownlint-cli2 "**/*.md" "#node_modules" "#target"` locally
  if you edit docs.
