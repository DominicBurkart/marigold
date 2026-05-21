import Lake
open Lake DSL

package «marigold-aeneas-proofs» where
  -- MVP configuration: allow `sorry` as a warning, not an error, so the
  -- CI job stays green while the round-trip proof is developed. Flipping
  -- this to error is tracked as a follow-up in
  -- dev_docs/aeneas/invariants.md.
  leanOptions := #[
    ⟨`warningAsError, false⟩,
    ⟨`autoImplicit, false⟩
  ]

-- The Aeneas-generated Lean code lives in `generated/`. Once the first
-- extraction has run and populated that directory, this lakefile should
-- add a dependency on it (either via `require` from a local path or by
-- including it as a subdirectory). For the MVP stub commit, the
-- `generated/` directory is empty (only `.gitkeep`), so we do not
-- require it yet.

@[default_target]
lean_lib CsvCore where
  -- Build the proof module at the root of this lake project.
  roots := #[`CsvCore]
