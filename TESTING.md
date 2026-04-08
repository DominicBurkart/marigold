# Testing

> Agents: this file is your source of truth for verification decisions
> in this repo. Read it before generating tests.

Marigold uses a tiered verification strategy. Tests ship in the same PR as the
code they cover: `codecov.yml` enforces 100% patch coverage, so new code
without new tests will not merge. Pick the lowest tier that actually proves
the property you care about, and add higher tiers when they apply.

## Decision tree

- **Unit test** — pure deterministic function, single input maps to single
  output, no IO.
  - Location: inline `#[cfg(test)] mod tests` in the same file.
  - Run: `cargo test --all-features`.
- **Proptest** — pure function, input space too large to enumerate, an
  algebraic property (roundtrip, idempotence, monotonicity) holds.
  - Pattern: `marigold-grammar/src/complexity.rs` has a `mod proptests` with
    `test_serde_roundtrip_proptest` and
    `test_exact_complexity_serde_roundtrip_proptest`.
  - Regression seeds: `marigold-grammar/proptest-regressions/`.
  - Run: `cargo test --all-features`.
- **Integration test** — end-to-end DSL behaviour, fixture-driven.
  - Location: `marigold-grammar/tests/` (see `e2e_cardinality.rs`,
    `e2e_complexity.rs`, `proptest_analyze.rs`, `tests/programs/` fixtures).
  - Run: `cargo test --all-features`.
- **Criterion benchmark** — perf regression matters for this code path.
  - Location: `marigold-impl/benches/` (see `keep_first_n.rs`,
    `pipeline_stages.rs`, and peers).
  - Run: `cargo bench`.
  - CI: `.github/workflows/bench.yaml`.
- **Aeneas + Lean formal proof** — a property must hold for all inputs in an
  unbounded space, AND the function is pure-sync safe Rust (no async, no
  `unsafe`, no tokio, no trait objects, no complex lifetimes).
  - Location: `dev_docs/aeneas/proofs/`.
  - Architecture: see `dev_docs/aeneas/README.md` for the shell/core split.
  - Run: `cd dev_docs/aeneas/proofs && lake build`.
  - CI: `dev_docs/aeneas/ci/aeneas.yaml` (template; install manually
    into `.github/workflows/`).
- **Out of scope (future)**: concurrency invariants belong in loom; memory
  safety of `unsafe` belongs in miri. Neither is wired up yet.

## When NOT to reach for formal proof

Formal proof via Aeneas is expensive and narrow. Do not attempt it for:

- `async`/`await` code or any `Future`-returning function.
- Anything touching `tokio`, `csv_async`, `flate2`, or `async_compression`.
- Anything containing `unsafe` blocks or raw pointers.
- Anything with `dyn Trait` trait objects or complex HRTBs (`for<'a> ...`).
- Anything where a proptest gives 99% or greater confidence cheaply.

The authoritative list of unsupported Rust features lives in
[`dev_docs/aeneas/limitations.md`](dev_docs/aeneas/limitations.md). Consult
it before opening a formal-verification task.

## Pre-commit hook

`.cargo-husky/hooks/pre-commit` is enforced locally. It runs fmt, clippy,
test, cargo-audit, cargo-deny, claude-security-review, readme-sync, and
tarpaulin-coverage. Do not bypass it with `--no-verify`. If the hook fails,
fix the underlying issue and create a new commit.

## Agent-loop contract

When adding new code, classify it under the decision tree above and add at
least the tier the tree mandates. If multiple tiers apply, add all of them:

- A pure sync function on an unbounded domain may want unit tests, a
  proptest, AND an Aeneas proof.
- A perf-sensitive hot path wants both a correctness tier and a criterion
  benchmark.

Do not hand-wave past the formal-proof tier by claiming "proptest is
enough" unless the function is genuinely outside the Aeneas supported
subset. Check [`dev_docs/aeneas/limitations.md`](dev_docs/aeneas/limitations.md)
first, then document the decision in the PR description.
