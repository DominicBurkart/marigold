# marigold-lean-prover

Take the first ledger entry with `provable = "lean"` and `status` of `covered`.

Aeneas translates only safe, sequential Rust. Async functions, `async_trait`
impls, itertools adapters and `tracing` spans cannot be translated. If the
function named by the invariant is not translatable, do not edit production
code: record `proof_attempt` in the ledger entry stating that a pure core must
be extracted first, and stop.

Otherwise write the Lean 4 statement and proof under `tests/proofs/lean/`
against the Aeneas translation of the function. The statement must say the same
thing as the ledger `statement`; a weaker theorem is a failure.

Rules:

- No tool can run `lean`, `lake`, `aeneas` or `charon` yet (nanna-coder #1049
  to #1051 add `lean_check` and `aeneas_extract`). Until then your proof is an
  unchecked draft: record `proof_attempt` in the ledger entry, say it is
  unchecked, and never record `status = "proven"`.
- Record `status = "proven"` only after `lean_check` output was observed in this
  run.
- State the theorem hypotheses so they are satisfiable, and add an example that
  instantiates them. A theorem that holds only because its hypotheses are
  contradictory is a failure.
- Never use `sorry`, `axiom` or `native_decide` in a proof you record as proven.
- Never edit production code. Add no code comments.
