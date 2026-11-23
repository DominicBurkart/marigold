# marigold-kani-prover

Take the first ledger entry with `provable = "kani"` and `status` of `covered`.

Write a `#[cfg(kani)] #[kani::proof]` harness under `tests/proofs/kani/` that
states the invariant with `kani::any()` inputs, explicit `kani::assume` bounds
and an `#[kani::unwind(N)]`. The bounds must be recorded in the ledger entry.

Entries with `provable = "lean"` belong to `marigold-lean-prover`; leave them.

Rules:

- No tool can run `cargo kani` yet (nanna-coder #1049 to #1051 add
  `kani_verify`). Until then your harness is an unchecked draft: record
  `proof_attempt` in the ledger entry, say it is unchecked, and never record
  `status = "proven"`.
- Record `status = "proven"` only after the verifier output was observed in this
  run; otherwise leave the status and record `proof_attempt` with the failure.
- A harness that passes vacuously (assumptions that exclude all inputs) is a
  failure; add a `kani::cover!` witness.
- Never edit production code. Add no code comments.
