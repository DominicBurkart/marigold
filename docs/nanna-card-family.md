# nanna card family

Seventeen identity cards beyond the four test-design cards: leads, specific
designers and implementers, adversarial reviewers and researchers.
Each has a base card in `.nanna/catalog/` (with its prompt under
`.nanna/catalog/prompts/`) and a narrowing card in `.nanna/agents/`. Names
match between the two. This is configuration only.

## Capability tiers

Tools are capabilities granted by exact name in `scope.tools`.

- None: every reviewer. Ceiling `none`, no write paths, tools `read_file`,
  `list_directory`, `search`, `git_status`, `git_diff`. No `cargo_*`,
  `run_command`, `write_file` or forge tool.
- Plan only: the three leads (test, implementer, research). Ceiling
  `workspace`, tools are the read tools plus `write_file`, no cargo. The test
  and implementer leads write `docs/plans/**` only and the research lead
  `docs/research/**` only. They need one write path because inchworm fails any
  non-final step that changes no files, and a later step only sees an earlier
  step's patch. Plans and findings use the `.txt` extension so the markdown
  lint hook does not apply to them.
- Findings only: the five researchers. Ceiling `workspace` (a card that writes
  files needs it), the read tools plus `write_file`, no cargo, and exactly one
  write path each, `docs/research/<area>.txt`, so the answer lands in the
  draft PR instead of only in the run summary.
- Light build: designers and implementers. Ceiling `workspace`, read and write
  tools plus `cargo_check` and `cargo_test`; the bugfix implementer and
  feature designer also get `cargo_build`.
- Heavy: kani and Aeneas with Lean belong only to the formal-methods designer,
  never to implementers or reviewers. No such tool exists in nanna yet, so
  that card can today only read, write proof drafts and run light cargo. It
  does not claim a proof ran.

No card has a network or forge tool (`github_*`, `git_commit`,
`git_push_branch`). Pull requests are opened outside nanna.

## Cards

Loop is `middle` for leads, reviewers and researchers and `inner` for
writers. Every card has `max_concurrent = 1`.

- `marigold-test-lead`: scopes a test effort and writes the plan.
  Writes `docs/plans/**`.
- `marigold-proptest-designer`: property tests. Writes only test locations:
  `marigold-impl/tests/**`, `marigold-grammar/tests/**` and `tests/**` (base
  also `marigold/tests/**`), never a `src` directory.
- `marigold-e2e-designer`: user-journey integration tests. Writes `tests/**`
  (base also `marigold/tests/**`).
- `marigold-formal-methods-designer`: drafts Kani harnesses and Lean files.
  Writes `tests/proofs/**` and `tests/invariants.toml`. Model
  `leanstral-whitedwarf`. See "Formal methods designer" below.
- `marigold-implementer-lead`: scopes a feature or bug and writes the plan.
  Writes `docs/plans/**`.
- `marigold-bugfix-implementer`: failing test first, then the minimal fix.
  Writes the five crates and `tests/**`.
- `marigold-feature-designer`: journey-based interface, test first. Writes
  the five crates and `tests/**`.
- `marigold-review-alignment`, `-duplication`, `-scope`, `-secondary-impact`:
  one concern each, read only. `-scope` also covers proof harnesses.
- `marigold-research-lead`: turns a research question into sub-questions and
  sources. Writes `docs/research/**`.
- `marigold-research-grammar`, `-runtime`, `-analyzer-lsp`, `-integrations`,
  `-docs`: investigators, each with its own `read_paths`. Each writes only its
  findings file, `docs/research/<area>.txt`.

## Reviewers are advisory

Nothing parses a reviewer's `VERDICT` line: inchworm does not read it, a
reviewer that finishes normally never fails the attempt, and the flow's
judges (diff and `nanna_clean`) are the only gates. A reviewer is also the
last step, and each step sees only the previous step's summary and patch, so
it reads the tree as it was before the change and cannot open the changed
files. The reviewer prompts therefore tell it to cite patch hunks (path and
`@@` header) and to cite file and line only for code it opened. The report is
for a human reading the run summary.

## Tests pin behaviour, bugfixes fail first

The test flows (`test-lead-*`) ask the designers for tests that pass against
current code; a designer that finds a divergence reports it and stops. The
test lead therefore writes pin-behaviour acceptance criteria, never
failing-first ones. Failing-first is only for `implement-bugfix` and
`implement-feature`, whose implementers write the failing test first, and
their lead plans for that. The implementers and designers cannot edit any
`Cargo.toml` (every flow forbids it), so they report a needed dependency
instead. `proptest` is a dev-dependency of `marigold-grammar`,
`marigold-impl` and the `tests` crate (not the `marigold` crate), and the
`tests` crate also has futures, tokio, async-std, serde, num-bigint and
opentelemetry_sdk. The proptest and e2e designers can do their job inside
those crates under `forbid_paths`; what they cannot do is add a dependency.

## Formal methods designer

Kani harnesses are `#[cfg(kani)]` files under `tests/proofs/kani` and Lean
files live under `tests/proofs/lean`. Cargo has no manifest target for them,
so `cargo_check` and `cargo_test` do not build or check them; the card has to
be read, not compiled. The ledger statuses are `candidate`, `covered`,
`proven` and `disputed` (see `.nanna/inchworm/gate.py`). The designer leaves
the status at `candidate` or `covered`, sets `provable` to `kani` or `lean`,
and never sets `proven`: that needs a `proof` file under `tests/proofs` and
real tool output, which only `gate.py ledger --run` and real tools can give
and which nanna cannot run yet. `proof_blocker` explains `provable = "none"`.
The Kani environment and a Lean prover identity are not on this branch; this
card does not reference them.

## Reconciling with the existing cards

- `marigold-test-designer` is unchanged and still pins one ledger invariant
  per run in the `test-design` flow. The new orchestrator is named
  `marigold-test-lead` so that flow keeps working.
- `marigold-kani-prover` is unchanged and still serves the `prove` flow. The
  new `marigold-formal-methods-designer` owns design and iteration of harnesses
  for the test lead; the prover stays the per-invariant proof step.

## Researcher areas

The areas follow the workspace layout. `grammar` covers the pest grammar,
parser, resolver and emitter plus `marigold-macros` and the `marigold`
facade, which only call the grammar. `runtime` is `marigold-impl`.
`analyzer-lsp` is `marigold-lsp` plus the grammar modules it imports
(diagnostics, symbols, spans, source maps, complexity, node ids).
`integrations` is the `integrations/` directory and the install and publish
scripts. `docs` is `docs/`, the READMEs, grammar docs and `examples/`.

## Flows

Delegation is expressed as inchworm flows in the root `inchworm.toml`, one
per label, each ending in a draft PR with diff and `nanna_clean` judges.

- `test-lead-proptest`, `test-lead-e2e`, `test-lead-formal`: plan by the test
  lead, the matching designer, then one reviewer.
- `implement-bugfix`, `implement-feature`: plan by the implementer lead, the
  bugfix implementer or feature designer, then one reviewer.
- `research-grammar`, `-runtime`, `-analyzer-lsp`, `-integrations`, `-docs`:
  a plan step by `marigold-research-lead`, then one researcher that writes
  `docs/research/<area>.txt`.

## Not yet expressible

- Dynamic delegation. A lead cannot spawn or name another card, so a human
  picks the flow by label after reading the lead's plan.
- More than one reviewer per flow. A step that changes no files fails as
  `empty_patch` unless it is the last step, so only one read-only step fits.
- Reviewer findings driving a retry or a change. Reviewers run last, nothing
  parses their verdict, and inchworm passes only a failure summary to the next
  attempt, so no implementer ever sees a review.
- Handing the plan to later steps by file. Every step is dispatched on the
  flow's base branch and inchworm forwards only the previous step's patch and
  summary, so the plan reaches the next step only as that patch. Whether
  earlier steps' files reach the pull request depends on nanna publishing the
  branch, which it cannot yet.
- Research that writes nothing. A single-step read-only flow is not allowed,
  so the research flows use a lead step and a researcher that writes its
  findings file.
- Publishing the pull request: nanna cannot yet, so the existing note in
  `.nanna/inchworm/README.md` applies to these flows too.
- Heavy proof tools, per-card images and enforcing that a delegate stays
  inside its delegator's scope are tracked upstream in nanna.

## Installing the base catalog

nanna requires each repo-local card to share a name with a global card and
only narrow it, so the base cards must be installed on the host first:

```sh
cp .nanna/catalog/marigold-*.toml ~/.config/nanna/agents/
cp .nanna/catalog/prompts/marigold-*.md ~/.config/nanna/agents/prompts/
scripts/check_nanna_agents.sh
```

Copy only the `marigold-*` cards: the host catalog is shared and a recursive
copy would overwrite its other cards. Until they are installed the host check
in `scripts/check_nanna_agents.sh` reports that a repo-local card has no
global identity to narrow, while the check against the repo catalog passes.

## Verification

`scripts/check_nanna_agents.sh` also fails when a reviewer card (base or
repo-local) lists an execution, write or forge tool, a ceiling other than
`none` or any write path, when a researcher lists an execution or forge tool,
has a ceiling other than `workspace` or may write anything but its own
`docs/research` file (the lead: `docs/research/**`), when a name differs from
its file name, or when any `marigold-*` card has a forge or shell tool or may
write `.nanna`, `inchworm`, manifests or `.github`.

`python3 -I scripts/tests/test_card_family.py` is hermetic and needs no nanna
binary. It loads every catalog and repo-local card and `inchworm.toml` and
asserts that each repo-local card narrows its base (exact-string subsets of
tools, paths, read paths and repos, the same ceiling, limits at or below), that
a card with a write tool has a write path and a non-`none` ceiling, that every
flow identity exists, that flow names, labels and step names are unique, that
every non-final step can write files, that every non-legacy flow identity
has a narrowing card, that `blocking` is `required`, that `retry_on` holds only
known values, that `max_lines` is positive, and that every system prompt file
exists. The label comparison ignores case. Each assertion has a mutation test
that must be flagged.

Two checks are approximations. Forbid globs are matched with a small
translation (`**/` is an optional directory prefix, `**` crosses directories,
`*` stays inside one segment), not the judge's own matcher. For each card write
glob the test samples a source, bench, `build.rs` and `Cargo.toml` file under
the glob's crate root and requires the flow's forbid globs to cover every
sample the card could write; the legacy `issue` flow is skipped and the
implementer flows may write `src`. The test designers (proptest, e2e,
formal-methods) must not be able to write those production samples at all.
The equal-`max_effect` assertion is intentionally stricter than nanna's own
narrowing rule, which would allow a lower ceiling; a lower ceiling here would
mean a card silently lost its write tool. A scope key the test does not know
(for example `message_reach`) may not widen its base.

The research flows allow 400 changed lines in the diff judge: the lead's plan
file plus the researcher's findings file, each of which can be a few hundred
lines of wrapped text.

The cargo-running designer and implementer narrowing cards keep the base
iteration and wall-clock limits, because a cold build of the workspace does
not fit in half the time. Narrowing allows limits equal to the base.
