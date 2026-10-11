# Aeneas agent workflow

Three runbooks for agents working on the verified CSV core. Each one is a
short step-by-step. Do not improvise outside these procedures without
updating the doc first.

## Procedure 1: Adding a function to `csv_core`

1. Read [`limitations.md`](limitations.md) and confirm the function only
   uses the supported subset.
2. Write the function in `marigold-impl/src/csv_core.rs` with
   `#![forbid(unsafe_code)]` preserved at the crate or module level.
3. Add a unit test and a proptest in the `#[cfg(test)] mod tests` at the
   bottom of `csv_core.rs`.
4. Run `cargo test -p marigold-impl --all-features`.
5. Re-run extraction (two steps: Charon lowers the crate to LLBC, then
   Aeneas translates LLBC to Lean; see
   [`extraction.md`](extraction.md) for the canonical commands):

   ```sh
   (cd marigold-impl && charon cargo --preset=aeneas)
   aeneas -backend lean \
     marigold-impl/target/llbc/marigold_impl.llbc \
     -o dev_docs/aeneas/proofs/generated/
   ```

6. Commit the updated `dev_docs/aeneas/proofs/generated/` output alongside
   your Rust code in the same commit.
7. Add or update a theorem in
   [`proofs/CsvCore.lean`](proofs/CsvCore.lean) matching the new
   invariant.
8. Run `cd dev_docs/aeneas/proofs && lake build`.
9. If any step fails and the root cause is tooling, go to Procedure 3.

## Procedure 2: Changing a `csv_core` public type signature

1. Same pre-check as Procedure 1: confirm the new signature stays in the
   supported subset.
2. Update the signature in `marigold-impl/src/csv_core.rs`.
3. Re-run extraction. The Lean side will break at this point; that is
   expected.
4. Update [`proofs/CsvCore.lean`](proofs/CsvCore.lean) to match the new
   generated definitions. If a theorem no longer applies, either restate
   it against the new signature or mark it `sorry` with a `-- TODO(#N)`
   comment and open a follow-up issue.
5. Update [`invariants.md`](invariants.md) to reflect the new guarantees
   in English.
6. Commit the Rust change, the regenerated `generated/` files, the Lean
   update, and the English invariant update atomically in one commit.

## Procedure 3: Aeneas extraction or `lake build` failed in CI

1. Pull the failing CI log. If the failure is plain Rust compilation of
   `marigold-impl/src/csv_core.rs`, this is an ordinary code bug: fix
   locally and repush.
2. If the failure is inside the `aeneas` extraction step itself (panic,
   unsupported-feature error), suspect either upstream tooling drift or
   that your new code uses an unsupported feature. Cross-check
   [`limitations.md`](limitations.md).
3. If upstream drift: pin
   [`aeneas-version.txt`](aeneas-version.txt) to the last-known-good SHA.
   Use `git log dev_docs/aeneas/aeneas-version.txt` to find one. Retry
   extraction locally before repushing.
4. If the new code genuinely needs an unsupported feature, open a GitHub
   issue with label `aeneas-blocked` and fall back to the next-best
   verification tier from [`../../TESTING.md`](../../TESTING.md), usually
   a proptest. An `aeneas-blocked` PR requires main-loop human review
   before it can merge.
5. If the failure is in `lake build` because a theorem does not close, it
   is acceptable for MVP only to add `sorry` with a `-- TODO(#N)` comment
   and open a follow-up issue. Do not delete theorems silently.
