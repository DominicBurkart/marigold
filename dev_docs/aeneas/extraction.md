# Aeneas extraction

Exact local commands for running Aeneas extraction and checking the Lean
proofs. Agents and humans both use this file.

## Toolchain prerequisites

### Aeneas and Charon

Aeneas does **not** consume a standalone `.rs` file. Extraction is a
two-step pipeline:

1. [Charon](https://github.com/AeneasVerif/charon) lowers a Rust crate
   (with its `Cargo.toml`) to an `.llbc` intermediate.
2. Aeneas then translates the `.llbc` to Lean 4.

Aeneas is pinned by SHA in [`aeneas-version.txt`](aeneas-version.txt).
Charon is pulled in as an Aeneas submodule / dependency at the same
revision. To install from source:

```sh
git clone https://github.com/AeneasVerif/aeneas.git
cd aeneas
git checkout "$(cat ../dev_docs/aeneas/aeneas-version.txt)"
make setup-charon   # builds Charon as part of the Aeneas build
make
```

Then add both `aeneas/bin` and the resulting Charon binary to your
`PATH`.

### Lean via elan

Lean 4 is managed through `elan`. Install the toolchain pinned for this
project:

```sh
curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
  | sh -s -- -y --default-toolchain "$(cat dev_docs/aeneas/proofs/lean-toolchain)"
```

## Extraction command

Two steps, both run from the repo root:

```sh
# 1. Charon: lower the marigold-impl crate to LLBC.
(cd marigold-impl && charon cargo --preset=aeneas)

# 2. Aeneas: translate the produced .llbc to Lean.
aeneas -backend lean \
  marigold-impl/target/llbc/marigold_impl.llbc \
  -o dev_docs/aeneas/proofs/generated/
```

The first command reads `marigold-impl/src/csv_core.rs` (plus the rest
of the crate, though only `csv_core` is what we prove against) and
writes an `.llbc` file under the crate's build output. The second
command lowers that intermediate to Lean 4 source files in
`dev_docs/aeneas/proofs/generated/`. The exact `.llbc` path may vary
with Charon releases; check `marigold-impl/target/` if the path above
does not resolve.

## Checking proofs

```sh
cd dev_docs/aeneas/proofs && lake build
```

## Expected output

- On a clean build `lake build` exits 0.
- Any `sorry`-stubbed theorems produce a warning. Warnings are
  non-fatal under the MVP configuration but are tracked as follow-up
  issues and must be discharged before graduating the theorem.

## Committing generated output

The contents of `dev_docs/aeneas/proofs/generated/` MUST be committed to
version control alongside the Rust source that produced them. CI runs
`git diff --exit-code` on this directory after re-running extraction and
fails the build if the local extraction drifts from what was committed.
This keeps the Lean proofs honest with respect to the current Rust code.
