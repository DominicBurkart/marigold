# Nanna card environments

Each marigold card runs in the smallest container that holds the tools it
needs. The Lean toolchain is built only when a card that needs it is spawned.

## Environments

Declared under `.nanna/environments/<name>/` as an `environment.toml` and a
`Containerfile`. The shape follows the draft in nanna-coder #1041 (schema 1,
`[build].containerfile`, `[[capability]]`). That design is unmerged, so treat
the field names as provisional.

| Environment | Contents | Cards |
|---|---|---|
| `readonly` | git, ripgrep | leads, reviewers, researchers, auditor, docs-writer, pr-shepherd, reviewer |
| `rust-test` | Rust stable, rustfmt, clippy | scout, test designer, proptest designer, e2e designer |
| `rust-build` | same image as `rust-test` | bugfix implementer, feature designer, rust-implementer |
| `rust-cov` | Rust plus cargo-tarpaulin | coverage closer |
| `kani` | Rust plus cargo-kani | kani prover, formal-methods designer |
| `lean` | Rust, Lean 4, charon, aeneas, Lean libraries | `marigold-lean-prover` only |

`rust-build` symlinks the `rust-test` Containerfile, so the two share one
image and differ only in the capability set.

## Why Lean is isolated

`marigold-lean-prover` is the only card mapped to `lean`. `validate.py`
rejects any other environment whose Containerfile builds Lean, charon or
aeneas, and rejects a second card mapped to `lean`. Spawning any other card
never builds that image. Lean is built from the Aeneas commit pinned in the
Containerfile (`AENEAS_COMMIT`); charon is checked out at the commit named by
Aeneas's `charon-pin`.

## Card to environment map

Nanna's card schema rejects unknown fields today, so cards cannot carry an
`[environment]` table yet. `.nanna/environments/cards.toml` maps every catalog
card to one environment. When the per-identity design lands, each mapping moves
into its card.

## Capabilities

A listed capability is a provided capability. Each environment lists only
tools nanna has today (`cargo_check`, `cargo_test`, `cargo_build`), and the
set must equal the container tools the card lists. `exec` is omitted, so the
built-in tool runs inside the environment's container. `coverage_report`,
`kani_verify`, `aeneas_extract` and `lean_check` are added to the `rust-cov`,
`kani` and `lean` environments and to their cards together, once nanna-coder
#1049, #1050 and #1051 land.

## Validation

```text
python3 -I .nanna/environments/validate.py .
cd .nanna/environments && python3 -m unittest test_validate
```

The validator checks that every catalog card is mapped, every mapped
environment exists with a Containerfile, the card's cargo tools equal the
environment's available capabilities, and Lean stays confined to its card. It
is not yet run in CI.
