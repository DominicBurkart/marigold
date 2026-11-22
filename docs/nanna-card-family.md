# nanna card family

Sixteen identity cards beyond the four test-design cards: leads, specific
designers and implementers, adversarial reviewers and read-only researchers.
Each has a base card in `.nanna/catalog/` (with its prompt under
`.nanna/catalog/prompts/`) and a narrowing card in `.nanna/agents/`. Names
match between the two. This is configuration only.

## Capability tiers

Tools are capabilities granted by exact name in `scope.tools`.

- None: every reviewer and researcher. Ceiling `none`, no write paths, tools
  `read_file`, `list_directory`, `search`, `git_status`, `git_diff`. No
  `cargo_*`, `run_command`, `write_file` or forge tool.
- Plan only: the two leads. Ceiling `workspace`, tools are the read tools plus
  `write_file`, write path `docs/plans/**` only, no cargo. They need one write
  path because inchworm fails any non-final step that changes no files, and a
  later step only sees an earlier step's patch. Plans use the `.txt`
  extension so the markdown lint hook does not apply to them.
- Light build: designers and implementers. Ceiling `workspace`, read and write
  tools plus `cargo_check` and `cargo_test`; the bugfix implementer and
  feature designer also get `cargo_build`.
- Heavy: kani and Aeneas with Lean belong only to the formal-methods designer,
  never to implementers or reviewers. No such tool exists in nanna yet, so
  that card can today only read, write proof harness files and run light
  cargo. It does not claim a proof ran.

No card has a network or forge tool (`github_*`, `git_commit`,
`git_push_branch`). Pull requests are opened outside nanna.

## Cards

Loop is `middle` for leads, reviewers and researchers and `inner` for
writers. Every card has `max_concurrent = 1`.

- `marigold-test-lead`: scopes a test effort and writes the plan.
  Writes `docs/plans/**`.
- `marigold-proptest-designer`: property tests. Writes the grammar and impl
  crates and `tests/**` (base also `marigold/**`).
- `marigold-e2e-designer`: user-journey integration tests. Writes `tests/**`
  (base also `marigold/tests/**`).
- `marigold-formal-methods-designer`: Kani and Aeneas harness design. Writes
  `tests/proofs/**` and `tests/invariants.toml`. Model `leanstral-whitedwarf`.
- `marigold-implementer-lead`: scopes a feature or bug and writes the plan.
  Writes `docs/plans/**`.
- `marigold-bugfix-implementer`: failing test first, then the minimal fix.
  Writes the five crates and `tests/**`.
- `marigold-feature-designer`: journey-based interface, test first. Writes
  the five crates and `tests/**`.
- `marigold-review-alignment`, `-duplication`, `-scope`, `-secondary-impact`:
  one concern each, read only.
- `marigold-research-grammar`, `-runtime`, `-analyzer-lsp`, `-integrations`,
  `-docs`: read-only investigators, each with its own `read_paths`.

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
  a plan step then one researcher.

## Not yet expressible

- Dynamic delegation. A lead cannot spawn or name another card, so a human
  picks the flow by label after reading the lead's plan.
- More than one reviewer per flow. A step that changes no files fails as
  `empty_patch` unless it is the last step, so only one read-only step fits.
- Reviewer findings driving a retry of the implementer. A reviewer that
  finishes normally does not fail the attempt, and inchworm passes only a
  failure summary to the next attempt.
- Handing the plan to later steps by file. Each step gets a fresh worktree and
  sees only the previous step's summary and patch, so the plan reaches the
  next step only as that patch.
- Research results. A researcher's answer survives only in the run summary
  in inchworm's store, and a single-step read-only flow is not allowed.
- Publishing the pull request: nanna cannot yet, so the existing note in
  `.nanna/inchworm/README.md` applies to these flows too.
- Heavy proof tools, per-card images and enforcing that a delegate stays
  inside its delegator's scope are tracked upstream in nanna.

## Installing the base catalog

nanna requires each repo-local card to share a name with a global card and
only narrow it, so the base cards must be installed on the host first:

```sh
cp -r .nanna/catalog/. ~/.config/nanna/agents/
scripts/check_nanna_agents.sh
```

## Verification

`scripts/check_nanna_agents.sh` now also fails when a reviewer or researcher
card (base or repo-local) lists an execution, write or forge tool, a ceiling
other than `none` or any write path, when a name differs from its file name,
or when any `marigold-*` card has a forge or shell tool or may write `.nanna`,
`inchworm`, manifests or `.github`.
