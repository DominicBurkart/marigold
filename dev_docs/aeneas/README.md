# Aeneas formal verification

[Aeneas](https://github.com/AeneasVerif/aeneas) extracts Rust MIR into pure
functional models for Lean, Coq, F\*, and HOL4. Marigold targets the Lean 4
backend. The proofs in this directory give machine-checked guarantees about
the row-level CSV core that underpins Marigold's data pipelines.

## Shell vs core architecture

The load-bearing rule for this project is that Aeneas cannot ingest
Marigold's async IO layer. Files like `marigold-impl/src/writer.rs` and the
`read_file` / `write_file` codegen emitted by
`marigold-grammar/src/pest_ast_builder.rs` use `tokio`, `Pin`, `csv_async`,
and trait objects, none of which Aeneas supports. Trying to verify them
directly is a non-goal.

Instead, Marigold splits the CSV code path into two layers:

- **Verified core**: `marigold-impl/src/csv_core.rs` contains the pure,
  synchronous, safe-Rust row encode / decode logic. It is the only file
  Aeneas extracts. It forbids `unsafe_code` and avoids async, tokio, trait
  objects, and `Pin`.
- **Unverified shell**: the async readers and writers in `marigold-impl`
  are a thin wrapper that delegates each row's encode / decode to the
  verified core. The shell handles IO scheduling, chunking, and
  backpressure; the core handles bytes.

Agents adding per-row byte logic must put it in the core. Agents adding IO
plumbing must put it in the shell.

## Agent contract

Before adding code under `marigold-impl/src/csv_core.rs`, read
[`invariants.md`](invariants.md). Before changing any public signature of
`csv_core`, you must update [`proofs/CsvCore.lean`](proofs/CsvCore.lean) and
re-run extraction, or CI will fail. If extraction fails and the root cause
is upstream Aeneas tooling drift, follow
[`agent_workflow.md`](agent_workflow.md) procedure 3.

## Sibling files

- [`invariants.md`](invariants.md): English statements of the formal
  claims the Lean proofs encode.
- [`limitations.md`](limitations.md): authoritative list of Rust features
  Aeneas does not support.
- [`agent_workflow.md`](agent_workflow.md): runbooks for adding code,
  changing signatures, and recovering from CI failures.
- [`extraction.md`](extraction.md): exact local commands for running
  Aeneas extraction and `lake build`.
- [`aeneas-version.txt`](aeneas-version.txt): pinned upstream SHA.
- [`proofs/CsvCore.lean`](proofs/CsvCore.lean): the Lean theorems.
- [`ci/aeneas.yaml`](ci/aeneas.yaml): GitHub Actions workflow template.
  A repo maintainer must copy this file into `.github/workflows/`
  manually to activate it; the GitHub App used by this session lacks
  the `workflows` permission required to push workflow files.

## MVP status

As of this commit, the MVP theorem in `proofs/CsvCore.lean` is stubbed with
`sorry`. Empirical confidence in the same invariant is provided by the
proptest that lives alongside the core in `marigold-impl/src/csv_core.rs`.
Agents reading [`invariants.md`](invariants.md) must not assume any
theorem holds until the corresponding `sorry` is discharged. Discharging
the `sorry` is tracked as a follow-up issue.
