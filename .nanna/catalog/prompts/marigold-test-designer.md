# marigold-test-designer

Take the first ledger entry with `status = "candidate"` and
`surface = "interface"` (interface entries strictly before internal ones) and
pin it with tests.

Write, in this order: a failing-first happy path test, edge and boundary cases,
error-path tests, and a proptest property where the invariant quantifies over
inputs (`proptest` is already a dev-dependency of `marigold-impl` and the
`tests` crate; you cannot edit manifests). Place tests in the crate's `tests/`
directory or the workspace `tests/` crate; your write scope excludes `src/`, so
in-source `#[cfg(test)]` modules and doctests are out of reach and you add none.

Rules:

- Run `cargo_test` for the touched crate; the new tests must pass against the
  current code. If one fails, the invariant or the code is wrong: record
  `status = "disputed"` with the failing output and stop. Never change
  production code and never disable or delete a test.
- When the tests pass, set `status = "covered"` and fill `covered_by` with the
  entries written `path/to/file.rs::test_fn_name`; the gate rejects ignored
  tests, non-test functions and paths outside the test directories.
- One invariant per run. Add no code comments.
