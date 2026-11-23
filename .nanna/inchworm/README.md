# marigold test-design inchworm

Five nanna identities, an invariant ledger, a gate, and four inchworm flows that
find untested invariants of marigold's interfaces, pin them with tests, then
close coverage to 100%, and prove what can be proven. Everything runs on local
models. The catalog holds 26 identities in all; these five belong to this
inchworm.

## Identities

Under `.nanna/catalog/`:

- `marigold-invariant-scout` (gemma4:e4b): finds untested interface invariants,
  writes the ledger.
- `marigold-test-designer` (gemma4:e4b): pins one `untested` invariant per run
  with tests.
- `marigold-coverage-closer` (gemma4:e4b): closes remaining coverage, only after
  the interface gate passes.
- `marigold-kani-prover` (leanstral-whitedwarf): Kani harnesses (bounded).
- `marigold-lean-prover` (leanstral-whitedwarf): Lean 4 proofs over Aeneas
  translations (unbounded).

## Flows

The root `inchworm.toml` holds the repository's `[auditor]` table, the existing
`issue` flow and these four flows, in inchworm's merged schema (15 flows in all
with the test-lead, implementer and research flows; `inchworm validate` reports
0 errors and 0 warnings). `inchworm.toml` is listed in `.nanna/protected.toml`.
Each flow is label-triggered, ends in a draft PR, and forbids edits to CI,
release scripts, cards, the gate, manifests and the image definitions
(`flake.nix`, `flake.lock`, `.nanna/environments/**`).

- `test-design`: label `inchworm:test-design`, steps scout then design.
- `close-coverage`: label `inchworm:coverage`, step close.
- `prove-kani`: label `inchworm:prove-kani`, step prove.
- `prove-lean`: label `inchworm:prove-lean`, step prove.

## Proofs are unchecked drafts for now

No tool can run `kani`, `lean`, `lake`, `aeneas` or `charon` yet: the cards list
only `cargo_check` and `cargo_test`. Until nanna-coder #1049 to #1051 land
(`kani_verify`, `coverage_report`, `aeneas_extract`, `lean_check`), `prove-kani`
and `prove-lean` produce unchecked draft proofs only. They record
`proof_attempt` in the ledger and never `proven`, and a human or later run must
check them. The `lean` image also lacks the Aeneas Lean library (see
`docs/nanna-environments.md`), so generated Lean cannot compile yet.

## Ordering is enforced by the gate

`python3 .nanna/inchworm/gate.py ledger` validates that every `covered` or
`proven` ledger entry links to existing test paths. `python3
.nanna/inchworm/gate.py coverage` additionally fails while any `surface =
"interface"` invariant is not `covered` or `proven`. The coverage closer's
prompt requires that exit 0 before it starts. Inchworm cannot run a repo command
as a gate step yet (inchworm issue 14), so until then the identity enforces it
itself.

## Schedule

Inchworm has no time trigger yet (inchworm issue 13). Until it lands, apply
`inchworm:test-design` weekly and `inchworm:prove-kani` and
`inchworm:prove-lean` after invariants are covered.

## Install

```sh
cp -r .nanna/catalog/. ~/.config/nanna/agents/
.nanna/inchworm/leanstral/import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf
.nanna/inchworm/check_models.sh
```

`check_models.sh` exits non-zero when any card's model is not served by Ollama
or fails to generate. Run it before dispatching a flow: pi accepts unknown model
names and exits 0 on a gateway failure, so a missing model would otherwise look
like a successful run.

## Local Leanstral

Leanstral 1.5 (119B total, 6.5B active, Apache-2.0) is run from SEBK4C's
`Leanstral-WhiteDwarf-seed-v3.gguf` (49.9 GB, IQ2_XXS experts, sha256
`1058b791...afa0`), an early-stopped community quantization that kept 76% of the
FP8 score on a miniF2F slice. On a 54 GB RAM, CPU-only machine it loads in about
35 s and decodes about 10 tokens per second.

`ollama create` from the CLI fails with this model ("neither 'from' or 'files'
was specified"), so `import.sh` uploads the blob and calls `/api/create`
directly with the Go-template translation of Mistral's chat template
(`leanstral/Modelfile`; the original Jinja is `leanstral/chat_template.jinja`).
The reasoning effort is fixed to `none` in the template.

## Not yet runnable end to end

- The pi broker (nanna-coder #962) is unmerged, so nothing launches identities
  in containers yet.
- nanna has no kani, lean or tarpaulin tools beyond the coverage tool in
  nanna-coder #1049, and identities have no container image field (nanna-coder
  #1041, #1012). The images are Nix-defined (`docs/nanna-environments.md`) and
  provide `cargo-kani`, `cargo-tarpaulin`, Lean 4 and Aeneas.
- Identities list `cargo_check` and `cargo_test`; the kani prover needs a kani
  tool (nanna-coder #1047).
