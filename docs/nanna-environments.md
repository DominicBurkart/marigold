# Nanna card environments

Each marigold card runs in the smallest container that holds the tools it needs.
Every environment is a Nix-built image defined in `flake.nix`. The Lean
toolchain is present only in the `lean` image, and only `marigold-lean-prover`
is mapped to it.

## Requirements

Building an environment needs Nix with flakes enabled on the host. There is no
Containerfile fallback: `validate.py` rejects a `containerfile` key.

## Environments

Declared under `.nanna/environments/<name>/environment.toml`. The shape follows
the draft in nanna-coder #1041 and the decision in nanna-coder #1147 (schema 1,
`[build] flake_output`, `[[capability]]`). That design is unmerged, so treat the
field names as provisional.

`flake_output` must be `devContainerImage-<name>`, where `<name>` is the
environment's directory name, and `flake.nix` must define that environment.

- `readonly` (`devContainerImage-readonly`): git, ripgrep, coreutils. Cards:
  leads, reviewers, researchers, auditor, docs-writer, pr-shepherd, reviewer.
- `rust-test` (`devContainerImage-rust-test`): the Rust toolchain from
  `rust-toolchain.toml`, a C toolchain, openssl. Cards: scout, test designer,
  proptest designer, e2e designer.
- `rust-build` (`devContainerImage-rust-build`): the same package set as
  `rust-test`. Cards: bugfix implementer, feature designer, rust-implementer.
- `rust-cov` (`devContainerImage-rust-cov`): Rust plus cargo-tarpaulin. Card:
  coverage closer.
- `kani` (`devContainerImage-kani`): Rust plus cargo-kani and its pinned
  nightly. Cards: kani prover, formal-methods designer.
- `lean` (`devContainerImage-lean`): Rust, Lean 4, Mathlib, aeneas, charon.
  Card: `marigold-lean-prover` only.

`rust-test` and `rust-build` share one package set and differ only in the
capability set.

## Build one environment

```text
nix build path:.#devContainerImage-<name>
```

The `path:` form is used because `flake.nix` and `flake.lock` may carry
uncommitted changes in a worktree. The existing `devContainerImage` output is
unchanged.

## Reproducibility

- Every input is pinned by `flake.lock`. The Rust toolchain comes from
  `rust-toolchain.toml` through rust-overlay.
- Anything downloaded at build time is a fixed-output derivation with a pinned
  hash (for example the Kani release bundle per platform). A changed download
  fails the build instead of changing the image.
- Images are assembled by one `mkImage` function, so environments share store
  paths and layers.
- Nothing is fetched when the container runs. The images carry everything the
  card needs, so task containers can run with `--network=none`.

## Why Lean is isolated

`marigold-lean-prover` is the only card mapped to `lean`, and `validate.py`
rejects a second card mapped to it. `flake.nix` exports a manifest of the
package names of every image, built from the same specs passed to `mkImage`, so
anything added when images are generated is included:

```text
nix eval --json path:.#lib.<system>.environmentManifest
```

When `nix` is on `PATH`, `validate.py` evaluates that attribute (about 3 s warm
cache). It rejects any environment other than `lean` whose packages match lean,
aeneas, charon, elan, lake, mathlib, batteries, fstar or hol4, and it requires
the `lean` environment to contain lean and aeneas. A failing evaluation is an
error, not a fallback.

Without `nix`, `validate.py` prints a text fallback warning and parses
`flake.nix` itself: comments and string contents are ignored, the bindings an
environment references (and the image-generating bindings) are followed, quoted
or dynamic attribute access is refused because it cannot be analysed, and
structure it cannot parse fails closed. The fallback is weaker than the
evaluated manifest.

## Card to environment map

Nanna's card schema rejects unknown fields today, so cards cannot carry an
`[environment]` table yet. `.nanna/environments/cards.toml` maps every catalog
card to one environment. When the per-identity design lands in nanna-coder, each
mapping moves into its card.

## Capabilities

A listed capability is a provided capability. Each environment lists only tools
nanna has today (`cargo_check`, `cargo_test`, `cargo_build`), and the set must
equal the container tools the card lists. `exec` is omitted, so the built-in
tool runs inside the environment's container. `coverage_report`, `kani_verify`,
`aeneas_extract` and `lean_check` are added to the `rust-cov`, `kani` and `lean`
environments and to their cards together, once nanna-coder issues 1049, 1050 and
1051 land.

## Validation

```text
python3 -I .nanna/environments/validate.py .
cd .nanna/environments && python3 -m unittest test_validate
```

The validator checks that every catalog card is mapped, every mapped environment
declares a well-formed `flake_output` that the flake defines, no unknown keys
appear in `environment.toml`, the card's cargo tools equal the environment's
available capabilities, and Lean stays confined to its card. The tests also
assert that every inchworm flow forbids edits to `flake.nix`, `flake.lock` and
`.nanna/environments/**`, and that `.nanna/protected.toml` lists them. It is not
yet run in CI.

## Proofs are unchecked drafts for now

No tool can run `kani`, `lean`, `lake`, `aeneas` or `charon` yet, because the
cards list only `cargo_check` and `cargo_test`. Until nanna-coder #1049 to #1051
land, `prove-kani` and `prove-lean` produce unchecked draft proofs only: they
record `proof_attempt` and never `proven`.

When those issues land, these capability ids are added to the environment and to
its card together, so the equality rule keeps holding:

- `coverage_report` to `rust-cov` and `marigold-coverage-closer`.
- `kani_verify` to `kani` and `marigold-kani-prover`.
- `aeneas_extract` and `lean_check` to `lean` and `marigold-lean-prover`.

## Known gaps

- The `lean` image is about 28 GiB.
- Aeneas's Lean library is absent from the `lean` image. It needs Lean 4.36 and
  nixpkgs provides Lean 4.35, so Lean generated by Aeneas cannot compile yet.
  The previous Containerfile image (commit a9af248 on the #466 history) built
  Lean 4.36.0 and the Aeneas library. The follow-up is a lean4 flake input
  pinned at v4.36.0.
- `cargo-tarpaulin` is 0.37.5 from nixpkgs, where the Containerfile pinned
  0.37.6.
- Only the evaluation for aarch64 has been checked. Images were built on x86_64
  only.
- No image has been tested inside a loaded `--network=none` container.
