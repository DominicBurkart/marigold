# marigold inchworm test designer

Source of truth for the marigold test-design inchworm: four nanna identities, an invariant ledger, a gate, and an inchworm process/schedule file.

| Identity | Model | Job |
|---|---|---|
| marigold-invariant-scout | gemma4:e4b | finds untested interface invariants, writes the ledger |
| marigold-test-designer | gemma4:e4b | pins one `untested` invariant per run with tests |
| marigold-coverage-closer | gemma4:e4b | closes remaining coverage, only after the interface gate passes |
| marigold-kani-prover | leanstral-1-5 | Kani harnesses (bounded) and Lean 4 via Aeneas (unbounded) |

## Ordering is enforced by the gate

`python3 .nanna/inchworm/gate.py ledger` validates every `covered`/`proven` entry links to existing test paths. `python3 .nanna/inchworm/gate.py coverage` additionally fails while any `surface = "interface"` invariant is not `covered` or `proven`. The `close-coverage` process runs only when that exits 0.

## Schedule

- Saturday 03:00 `identify-provable-invariants`: scout re-examines the interface for new invariants and marks `provable = kani|lean|none`.
- Sunday 03:00 `prove-invariants`: prover works the ledger.

## Install

nanna repo overrides (`.nanna/agents/`) can only narrow identities that already exist in the global catalog, so these files cannot live there. Copy them into the global catalog:

    cp .nanna/inchworm/identities/*.toml ~/.config/nanna/agents/
    cp .nanna/inchworm/prompts/*.md ~/.config/nanna/agents/prompts/

then `nanna agents list` shows all four.

## Not yet runnable end to end

- inchworm is a stub (`fn main` only). `inchworm.toml` is a proposed schema, not a parsed format.
- The pi broker (nanna #962) is unmerged, so nothing launches identities in containers yet.
- nanna exposes no kani, tarpaulin, or lean tools, and the identity schema has no container image field. The image must provide `cargo-kani`, `cargo-tarpaulin`, Lean 4 and Aeneas.
- Leanstral must run locally (decided). Candidate: SEBK4C/LEANSTRAL-WHITEDWARF `Leanstral-WhiteDwarf-seed-v3.gguf` (49.9 GB, IQ2_XXS experts, Lean-domain imatrix; 29/120 vs 38/120 for FP8 on a miniF2F slice, an early checkpoint). It needs llama.cpp (bec4772 or newer) with an explicit chat template, and about 50 GB of free disk. The `leanstral-1-5` model string in `marigold-kani-prover.toml` is a placeholder for the hosted id and must be replaced with the local model name once it is registered and a reachability test exists; pi does not reject unknown model names.
