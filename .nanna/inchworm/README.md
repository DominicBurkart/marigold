# marigold test-design inchworm

Four nanna identities, an invariant ledger, a gate, and three inchworm flows that find untested invariants of marigold's interfaces, pin them with tests, then close coverage to 100%, and prove what can be proven. Everything runs on local models.

| Identity (`.nanna/catalog/`) | Model | Job |
|---|---|---|
| marigold-invariant-scout | gemma4:e4b | finds untested interface invariants, writes the ledger |
| marigold-test-designer | gemma4:e4b | pins one `untested` invariant per run with tests |
| marigold-coverage-closer | gemma4:e4b | closes remaining coverage, only after the interface gate passes |
| marigold-kani-prover | leanstral-whitedwarf | Kani harnesses (bounded) and Lean 4 via Aeneas (unbounded) |

## Flows

The root `inchworm.toml` holds the repository's `[auditor]` table, the existing `issue` flow and these three flows, in inchworm's merged schema (`inchworm validate` reports 0 errors and 0 warnings). `inchworm.toml` is listed in `.nanna/protected.toml`. Each flow is label-triggered, ends in a draft PR, and forbids edits to CI, release scripts, cards, the gate and manifests.

| Flow | Label | Steps |
|---|---|---|
| test-design | `inchworm:test-design` | scout, design |
| close-coverage | `inchworm:coverage` | close |
| prove | `inchworm:prove` | prove |

## Ordering is enforced by the gate

`python3 .nanna/inchworm/gate.py ledger` validates that every `covered` or `proven` ledger entry links to existing test paths. `python3 .nanna/inchworm/gate.py coverage` additionally fails while any `surface = "interface"` invariant is not `covered` or `proven`. The coverage closer's prompt requires that exit 0 before it starts. Inchworm cannot run a repo command as a gate step yet (inchworm issue 14), so until then the identity enforces it itself.

## Schedule

Inchworm has no time trigger yet (inchworm issue 13). Until it lands, apply `inchworm:test-design` weekly and `inchworm:prove` after invariants are covered.

## Install

```sh
cp -r .nanna/catalog/. ~/.config/nanna/agents/
.nanna/inchworm/leanstral/import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf
.nanna/inchworm/check_models.sh
```

`check_models.sh` exits non-zero when any card's model is not served by Ollama or fails to generate. Run it before dispatching a flow: pi accepts unknown model names and exits 0 on a gateway failure, so a missing model would otherwise look like a successful run.

## Local Leanstral

Leanstral 1.5 (119B total, 6.5B active, Apache-2.0) is run from SEBK4C's `Leanstral-WhiteDwarf-seed-v3.gguf` (49.9 GB, IQ2_XXS experts, sha256 `1058b791...afa0`), an early-stopped community quantization that kept 76% of the FP8 score on a miniF2F slice. On a 54 GB RAM, CPU-only machine it loads in about 35 s and decodes about 10 tokens per second.

`ollama create` from the CLI fails with this model ("neither 'from' or 'files' was specified"), so `import.sh` uploads the blob and calls `/api/create` directly with the Go-template translation of Mistral's chat template (`leanstral/Modelfile`; the original Jinja is `leanstral/chat_template.jinja`). The reasoning effort is fixed to `none` in the template.

## Not yet runnable end to end

- The pi broker (nanna-coder #962) is unmerged, so nothing launches identities in containers yet.
- nanna has no kani, lean or tarpaulin tools beyond the coverage tool in nanna-coder #1049, and identities have no container image field (nanna-coder #1041, #1012). The image must provide `cargo-kani`, `cargo-tarpaulin`, Lean 4 and Aeneas.
- Identities list `cargo_check` and `cargo_test`; the kani prover needs a kani tool (nanna-coder #1047).
