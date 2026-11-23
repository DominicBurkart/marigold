# marigold test-design inchworm

Four nanna identities, an invariant ledger, a manual gate, and three inchworm
flows that find interface invariants without a pinning test, pin them with
tests, then close coverage, and prove what can be proven. Everything is meant to
run on local models.

## Identities

Cards live in `.nanna/catalog/`.

- `marigold-invariant-scout` (gemma4:e4b). Writes only
  `tests/invariants.toml`. Finds invariants without a pinning test and records
  them in the ledger.
- `marigold-test-designer` (gemma4:e4b). Writes only test directories. Pins one
  `candidate` invariant per run with tests.
- `marigold-coverage-closer` (gemma4:e4b). Writes only test directories. Closes
  remaining coverage, only after the interface gate passes.
- `marigold-kani-prover` (leanstral-whitedwarf). Writes only `tests/proofs/**`
  and `tests/invariants.toml`. Kani harnesses (bounded) and Lean 4 via Aeneas
  (unbounded).

## Flows

The root `inchworm.toml` holds the repository's `[offramp]`, `[auditor]` and
`[merge]` tables, the existing `issue` flow and these three flows.
`inchworm.toml` is listed in `.nanna/protected.toml`. The `inchworm` binary is
not installed on the machine that edited this file, so the file is checked only
as valid TOML; run `inchworm validate` before relying on it. Each flow is
label-triggered, ends with the same `publish` step (`pr-shepherd`) as the
`issue` flow, and ends in a draft PR.

- `test-design`, label `inchworm:test-design`. Steps: scout, design, publish.
  Can run once the pi broker (nanna-coder #962) lands.
- `close-coverage`, label `inchworm:coverage`. Steps: close, publish. Cannot
  run: needs the coverage tool (nanna-coder #1049 to #1051).
- `prove`, label `inchworm:prove`. Steps: prove, publish. Cannot run: needs the
  kani and lean tools (nanna-coder #1049 to #1051).

No flow can reach `pr_open` yet. The flows are configuration only until nanna
creates the branch and the PR.

The coverage tool and the kani and lean tools are not available yet
(nanna-coder #1049 to #1051), so the closer and prover identities list only the
tools nanna has (`read_file`, `list_directory`, `search`, `git_status`,
`git_diff`, `write_file`, `cargo_check`, `cargo_test`). Their prompts name the
missing tools; until those exist, `close-coverage` and `prove` cannot do their
job and must not be dispatched.

## Enforcement of "no production edits"

The designer, closer and prover are meant to add tests and proofs, never change
production code. Two layers enforce this:

- Each card's `scope.paths` lists only test directories
  (`marigold-impl/tests/**`, `marigold-grammar/tests/**`, `marigold/tests/**`,
  `tests/**`; the prover only `tests/proofs/**` and the ledger).
- Each flow's diff judge uses the `issue` flow's `forbid_paths` (CI, release
  scripts, `.nanna/**`, `.inchworm/**`, `inchworm.toml`, `deny.toml`,
  `codecov.yml`, `flake.nix`, `flake.lock`, `.devcontainer/**`,
  `.cargo-husky/**`) plus `install.sh`, every `Cargo.toml`, `examples/**`,
  `marigold/src/**`, `marigold-*/src/**`, `marigold-*/benches/**`,
  `**/build.rs` and `scripts/**`.

Residual risk: `forbid_paths` is a path blocklist and cannot express "only test
code". Test code that lives inside `src/` files (`#[cfg(test)]` modules,
doctests) is out of scope for these identities, and a path outside the listed
globs can still be edited by a judge-passing diff (for example `tests/src/**`,
the helper crate shared by tests). The designer cannot add dev-dependencies,
because the judges forbid every `Cargo.toml`; that is why this PR adds
`proptest = "1"` by hand to `marigold-impl` and the `tests` crate, where
`marigold-impl/tests/count_properties.rs` uses it. A new dev-dependency needs a
manual change. `local only` is a property of the
card models, and nanna does not enforce it (nanna-coder #1048).

## The gate is manual

`python3 -I .nanna/inchworm/gate.py ledger` checks `tests/invariants.toml`. It
is not invoked by any flow, CI job or git hook yet: inchworm cannot run a repo
command as a gate step (inchworm issue 14), and wiring it and its tests into CI
is tracked in [issue 470](https://github.com/DominicBurkart/marigold/issues/470).
Until then run it by hand, and the identities are told to run it themselves.

There are two modes.

- Default: a fast, offline static LINT. It is not a proof that a cited test
  runs: it reads Rust with a hand-written scanner (comments, string literals
  including raw, byte and C strings, braces, attributes) and can be fooled by
  constructs it does not model.
- `--run` (for example `CARGO_TARGET_DIR=/var/tmp/gate-target python3 -I
  .nanna/inchworm/gate.py ledger --run`): after the lint passes, it runs cargo
  for every `covered` or `proven` citation. It needs `CARGO_TARGET_DIR` in the
  environment, scrubs `GIT_*` variables, and for each citation runs
  `cargo test -p <crate> (--lib | --test <file>) [--features ...] -- --list` to
  require exactly one collected test of that name (and module path), then runs
  it with `-- --exact` and requires exactly one passing test. Features come from
  the `cfg(feature = ...)` gates the lint saw and from an optional `features`
  list on the ledger entry. This is the source of truth; the lint is only a
  pre-filter.

What the lint checks:

- Required fields, unique ids (compared after Unicode NFC normalisation, case
  folding and trimming), and the enums `status` (`candidate`, `covered`,
  `proven`, `disputed`), `surface` (`interface`, `internal`) and `provable`
  (`none`, `kani`, `lean`); a value of the wrong type or an invalid UTF-8
  ledger is a clean error.
- Every `covered_by` entry is a repository-relative `.rs` file that cargo
  compiles as a test: `<crate>/tests/*.rs` directly (no subdirectories, no
  `benches/`, no `tests/proofs/`), or a `<crate>/src` file declared by a `mod`
  chain from `lib.rs` or `main.rs`, for the crates `marigold`, `marigold-impl`,
  `marigold-grammar`, `marigold-lsp`, `marigold-macros` and `tests`. No absolute
  paths, no `..`, no symlink escapes.
- For `covered` and `proven`, entries must be written `path::name`, with
  optional module segments before the name. Exactly one `fn` of that name must
  match (the module segments must be the innermost enclosing modules; without
  them the name must be unique in the file), so a same-named test elsewhere
  cannot satisfy a citation.
- That fn must carry `#[test]`, `#[tokio::test(...)]`, `#[async_std::test]` or
  `#[rstest::rstest]`, including inside `proptest!` (a bare fn there is a plain
  function, so it still needs `#[test]`; the macro head must be `proptest` or
  `proptest::proptest`). Only `should_panic`, `allow`, `doc` and `cfg` are
  accepted besides, where a `cfg` predicate must be `test`,
  `feature = "tokio"`, `feature = "async-std"` or an `any(...)` of the two
  features. The same `cfg` rule applies to every enclosing module, to inner
  `#![...]` attributes at the head of a file or body, and to the `mod`
  declaration of a cited `src` file. An inner attribute anywhere else,
  `#[ignore]`, any other `cfg`, `cfg_attr` and any other attribute are
  rejected, as is an fn nested in another fn, inside `macro_rules!` or in any
  other block.
- `proven` needs `provable` other than `none`, and a `proof` file under
  `tests/proofs/` (the directory must not be a symlink and the resolved path
  must stay inside it), used by one id only. For `kani` it is a `.rs` file with
  a `#[kani::proof]` fn (not in a macro, a nested fn or an unsupported cfg
  gate; `cfg(kani)` is fine); for `lean` it is a `.lean` file with a `theorem`
  or `lemma` and no `sorry`, `axiom` or `admit` outside comments.
- `proof_blocker` is an optional string saying why `provable = "none"` for code
  that is provable in principle.

What `proven` means: a proof artifact of the right kind exists, is wired to one
ledger id and passes this lint. It does not mean Kani or Lean were run or that
the harness proves the statement; the gate does not execute either tool, and a
reviewer must read the proof.

`python3 -I .nanna/inchworm/gate.py coverage` additionally fails while any
`surface = "interface"` invariant is not `covered` or `proven`. Both phases take
`--run`. Exit codes: 0 pass, 1 ledger problems, 2 usage. Tests (the run mode
uses a fake `cargo` on `PATH`): `python3 -I .nanna/inchworm/test_gate.py`,
`test_import.py` and `test_check_models.py` in the same directory.

## Ledger status

`covered` means an existing test pins the statement as written, including its
preconditions. `disputed` means the statement was falsified against the code.
Currently `grammar-parse-total` is a `candidate` and `keep-first-n-tie-break` is
`disputed`: with the tokio or async-std implementation, tied items that span
concurrently processed chunks can keep a later item in place of an earlier one.
This is a production bug in `marigold-impl/src/keep_first_n.rs`, tracked in
[issue 461](https://github.com/DominicBurkart/marigold/issues/461) with a fix
proposed in [PR 463](https://github.com/DominicBurkart/marigold/pull/463); it is
not fixed here. `keep-first-n-bounded-descending` is stated for the tokio
feature only, because no async-std test exists, and so is
`multi-consumer-fanout`, whose cited test is written with `#[tokio::test]`. The
keep-first-n, permutations and combinations invariants are
`provable = "none"` because their code is async or itertools-based and Aeneas
and Kani cannot translate it; each carries
`proof_blocker = "needs a pure sync core extracted from the async wrapper"`.

## Schedule

Inchworm has no time trigger yet (inchworm issue 13). Until it lands, apply
`inchworm:test-design` weekly and `inchworm:prove` after invariants are
covered.

## Install

Do not copy `.nanna/catalog` over a host catalog: it is shared across
repositories and a recursive copy would overwrite its cards. Merge only the
marigold cards by hand into the host catalog (see `docs/nanna-agents.md`): the
four `.nanna/catalog/marigold-*.toml` cards and their
`.nanna/catalog/prompts/marigold-*.md` prompts. To try them without touching a
host catalog, copy just those files into a scratch directory and point
`NANNA_CONFIG_DIR` at it.

```sh
.nanna/inchworm/leanstral/import.sh /path/to/Leanstral-WhiteDwarf-seed-v3.gguf
.nanna/inchworm/check_models.sh
```

`check_models.sh` exits non-zero when any card's model is not served by Ollama,
fails to load within `LOAD_TIMEOUT_SECS` (default 900, reported as a timeout),
or fails to generate. It probes each model with its own context size: the
Leanstral model with the `num_ctx` from its Modelfile (8192), so it is not
reloaded for a different size, and every other model with Ollama's default. Run
it before dispatching a flow: pi accepts unknown model names and exits 0 on a
gateway failure, so a missing model would otherwise look like a successful run.

## Local Leanstral

Leanstral 1.5 (119B total, 6.5B active, Apache-2.0) is run from SEBK4C's
`Leanstral-WhiteDwarf-seed-v3.gguf` (49.9 GB, IQ2_XXS experts, sha256
`1058b791...afa0`), an early-stopped community quantization that kept 76% of the
FP8 score on a miniF2F slice. On a 54 GB RAM, CPU-only machine it loads in about
35 s and decodes about 10 tokens per second.

`ollama create` from the CLI fails with this model ("neither 'from' or 'files'
was specified"), so `import.sh` streams the blob with `curl -T` (skipping the
upload when `HEAD /api/blobs/sha256:<sum>` returns 200), then calls
`/api/create` with the Go-template translation of Mistral's chat template
(`leanstral/Modelfile`; the original Jinja is `leanstral/chat_template.jinja`).
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
  in containers yet, and no flow can reach `pr_open`.
- The coverage tool and the kani and lean tools do not exist (nanna-coder #1049
  to #1051), and identities have no container image field (nanna-coder #1041,
  #1012). The image must provide `cargo-kani`, `cargo-tarpaulin`, Lean 4 and
  Aeneas. `close-coverage` and `prove` cannot run until these land.
- `local only` is not enforced by nanna (nanna-coder #1048).
