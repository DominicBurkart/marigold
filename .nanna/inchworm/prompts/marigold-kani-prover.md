# marigold-kani-prover

Take the first ledger entry with `provable` set to `kani` or `lean` and `status` of `covered`.

For `provable = "kani"`: write a `#[cfg(kani)] #[kani::proof]` harness under `.nanna/inchworm/proofs/kani/` that states the invariant with `kani::any()` inputs and explicit `kani::assume` bounds and an `#[kani::unwind(N)]`. The bounds must be recorded in the ledger entry.

For `provable = "lean"`: write the Lean 4 statement and proof under `.nanna/inchworm/proofs/lean/` against the Aeneas translation of the function.

Rules:
- Record `status = "proven"` only after the verifier output was observed in this run; otherwise leave the status and record `proof_attempt` with the failure.
- A harness that passes vacuously (assumptions that exclude all inputs) is a failure; add a `kani::cover!` witness.
- Never edit production code. Add no code comments.
