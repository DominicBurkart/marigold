# marigold test-design inchworm

Four nanna identities, an invariant ledger, a gate, and three inchworm flows
that find interface invariants without a pinning test, pin them with tests, then
close coverage, and prove what can be proven. Everything is meant to run on
local models.

| Identity (`.nanna/catalog/`) | Model | Write scope | Job |
|---|---|---|---|
| marigold-invariant-scout | gemma4:e4b | `tests/invariants.toml` | finds invariants without a pinning test, writes the ledger |
| marigold-test-designer | gemma4:e4b | `tests/**` directories only | pins one `candidate` invariant per run with tests |
| marigold-coverage-closer | gemma4:e4b | `tests/**` directories only | closes remaining coverage, only after the interface gate passes |
| marigold-kani-prover | leanstral-whitedwarf | `tests/proofs/**`, `tests/invariants.toml` | Kani harnesses (bounded) and Lean 4 via Aeneas (unbounded) |

## Flows

The root `inchworm.toml` holds the repository's `[offramp]`, `[auditor]` and
`[merge]` tables, the existing `issue` flow and these three flows.
`inchworm.toml` is listed in `.nanna/protected.toml`. The `inchworm` binary is
not installed on the machine that edited this file, so the file is checked only
as valid TOML; run `inchworm validate` before relying on it. Each flow is
label-triggered and ends in a draft PR.

| Flow | Label | Steps | Can run today |
|---|---|---|---|
| test-design | `inchworm:test-design` | scout, design | once the pi broker (nanna-coder #962) lands |
| close-coverage | `inchworm:coverage` | close | no: needs the coverage tool (nanna-coder #1049-#1051) |
| prove | `inchworm:prove` | prove | no: needs the kani and lean tools (nanna-coder #1049-#1051) |

The coverage tool and the kani and lean tools are not available yet (nanna-coder
#1049-#1051), so the closer and prover identities list only the tools nanna has
(`read_file`, `list_directory`, `search`, `git_status`, `git_diff`,
`write_file`, `cargo_check`, `cargo_test`). Their prompts name the missing
tools; until those exist, `close-coverage` and `prove` cannot do their job and
must not be dispatched.

## Enforcement of "no production edits"

The designer, closer and prover are meant to add tests and proofs, never change
production code. Two layers enforce this:

- each card's `scope.paths` lists only test directories (
  `marigold-impl/tests/**`, `marigold-grammar/tests/**`, `marigold/tests/**`,
  `tests/**`; the prover only `tests/proofs/**` and the ledger);
- each flow's diff judge forbids `marigold/src/**`, `marigold-*/src/**`,
  `marigold-*/benches/**`, `**/build.rs`, `scripts/**`, `examples/**`, every
  `Cargo.toml`, CI, release scripts, `codecov.yml` and `deny.toml`. The `issue`
  flow forbids the broader set listed in `inchworm.toml`.

Residual risk: `forbid_paths` is a path blocklist and cannot express "only test
code". Test code that lives inside `src/` files (`#[cfg(test)]` modules,
doctests) is out of scope for these identities, and a path outside the listed
globs can still be edited by a judge-passing diff (for example `tests/src/**`,
the helper crate shared by tests). Dev-dependencies such as `proptest` are added
by hand, because the judges forbid `Cargo.toml`; `proptest = "1"` is already a
dev-dependency of `marigold-impl` and the `tests` crate. `local only` is a
property of the card models, and nanna does not enforce it (nanna-coder #1048).

## Ordering is enforced by the gate

`python3 -I .nanna/inchworm/gate.py ledger` validates `tests/invariants.toml`:

- required fields, unique ids, and the enums `status` (`candidate`, `covered`,
  `proven`, `disputed`; `untested` is a legacy alias of `candidate`), `surface`
  (`interface`, `internal`), `provable` (`none`, `kani`, `lean`);
- every `covered_by` entry is a repository-relative file under `tests/`,
  `marigold/` or `marigold-*/` (no absolute paths, no `..`, no symlink escapes);
  for `covered` and `proven`, an entry written `path::name` must contain
  `fn name`;
- `proven` needs `provable` other than `none` and a `proof` file under
  `tests/proofs/`;
- `proof_blocker` is an optional string saying why `provable = "none"` for code
  that is provable in principle.

`python3 -I .nanna/inchworm/gate.py coverage` additionally fails while any
`surface = "interface"` invariant is not `covered` or `proven`. Exit codes: 0
pass, 1 ledger problems, 2 usage. Inchworm cannot run a repo command as a gate
step yet (inchworm issue 14), so until then the identity enforces the order
itself. Tests: `python3 -I .nanna/inchworm/test_gate.py` and
`python3 -I .nanna/inchworm/test_import.py`.

## Ledger status

`covered` means an existing test pins the statement as written, including its
preconditions. `disputed` means the statement was falsified against the code.
Currently `grammar-parse-total` is a `candidate` and `keep-first-n-tie-break` is
`disputed`: with the tokio or async-std implementation, tied items that span
concurrently processed chunks can keep a later item in place of an earlier one.
This is a production bug in `marigold-impl/src/keep_first_n.rs` and is not fixed
here. The keep-first-n, permutations and combinations invariants are
`provable = "none"` because their code is async or itertools-based and Aeneas
and Kani cannot translate it; each carries
`proof_blocker = "needs a pure sync core extracted from the async wrapper"`.

## Schedule

Inchworm has no time trigger yet (inchworm issue 13). Until it lands, apply
`inchworm:test-design` weekly and `inchworm:prove` after invariants are covered.

## Install

```sh
cp -r .nanna/catalog/. ~/.config/nanna/agents/
.nanna/inchworm/leanstral/import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf
.nanna/inchworm/check_models.sh
```

`check_models.sh` exits non-zero when any card's model is not served by Ollama,
fails to load within `LOAD_TIMEOUT_SECS` (default 900, reported as a timeout),
or fails to generate. It probes with the Modelfile's `num_ctx` so the model is
not reloaded for a different context size. Run it before dispatching a flow: pi
accepts unknown model names and exits 0 on a gateway failure, so a missing model
would otherwise look like a successful run.

## Local Leanstral

Leanstral 1.5 (119B total, 6.5B active, Apache-2.0) is run from SEBK4C's
`Leanstral-WhiteDwarf-seed-v3.gguf` (49.9 GB, IQ2_XXS experts, sha256
`1058b791...afa0`), an early-stopped community quantization that kept 76% of the
FP8 score on a miniF2F slice. On a 54 GB RAM, CPU-only machine it loads in about
35 s and decodes about 10 tokens per second.

`ollama create` from the CLI fails with this model ("neither 'from' or 'files'
was specified"), so `import.sh` streams the blob with `curl -T` (skipping the
upload when `HEAD /api/blobs/sha256:<sum>` returns 200), then calls
`/api/create` with the Go-template translation of Mistral's chat template (
`leanstral/Modelfile`; the original Jinja is `leanstral/chat_template.jinja`).
The template and the `PARAMETER` lines are read from the Modelfile, and an
`error` in the create response fails the script. `LEANSTRAL_SHA256` overrides
the expected checksum, which the script test uses with a small stub file against
a fake HTTP server; the real import was not re-run. The reasoning effort is
fixed to `none` in the template.

Double BOS risk, not verified: the template begins with a literal `<s>`, as does
Mistral's original Jinja (`{{- '<s>' }}`), which assumes the caller adds no BOS
of its own. If the GGUF sets `tokenizer.ggml.add_bos_token` to true and Ollama
also tokenizes the rendered prompt with special tokens added, the prompt starts
with two BOS tokens. The GGUF is not on this machine, so this was not confirmed
either way and the template is unchanged. To check on the machine that has the
model: `curl -s localhost:11434/api/show -d '{"model":"leanstral-whitedwarf"}'`
and read `model_info["tokenizer.ggml.add_bos_token"]`; if it is true, drop the
leading `<s>` from the Modelfile TEMPLATE.

## Not yet runnable end to end

- The pi broker (nanna-coder #962) is unmerged, so nothing launches identities
  in containers yet.
- The coverage tool and the kani and lean tools do not exist (nanna-coder
  #1049-#1051), and identities have no container image field (nanna-coder #1041,
  #1012). The image must provide `cargo-kani`, `cargo-tarpaulin`, Lean 4 and
  Aeneas. `close-coverage` and `prove` cannot run until these land.
- `local only` is not enforced by nanna (nanna-coder #1048).
