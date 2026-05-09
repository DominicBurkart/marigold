# CRON job FNrH6 — fingerprint and dispositions

- job-id: FNrH6
- oldest-issue-id: `one_track#2` (cross-repo run)
- branch: `claude/sharp-knuth-FNrH6`
- base: `main`
- scope: #68
- fingerprint date (UTC): 2026-05-09

## Per-issue disposition

| issue | title | canonical PR | action |
|------:|-------|-------------:|--------|
| #68 | solve gpg miscconfiguration | **#215** (also #192, #159) | NO-OP — defer (3 converging PRs); workflow-file residual escalated |

## Convergence on the Rust-side fix

Three open draft PRs converge on the same approach: replace
`git config commit.gpgsign false` with `--no-gpg-sign` on the `git commit` invocation in:

- `marigold-grammar/tests/bisect_cardinality.rs:50`
- `marigold-grammar/tests/bisect_complexity.rs:50`

PR **#215** (cron-nnkMo) is the freshest, rebased on current main, and ships a
regression-guard test (`marigold-grammar/tests/no_gpg_config_workaround.rs`).

Janitor: pick **#215** (newest, has regression guard); close **#192** and **#159** as
superseded.

## Workflow-file residual (cross-repo workflows-token escalation)

`.github/workflows/badges.yaml:47` calls `git commit -m "update badges"` without
`--no-gpg-sign`. All three Rust-side PRs explicitly defer this because their bot token
lacks the `workflows` scope.

This is the same root cause as nanna-coder PRs #247 and #254 (see the nanna-coder
cron-FNrH6 manifest). Action for janitor: a human with `workflows`-scope token adds
`--no-gpg-sign` to that one git commit invocation. Single-line change.

## Phase 2 — Self-review

- Issue body precisely describes the Rust call sites; convergent fix shipped 3x.
- No PII / no key material involved (the fix only disables signing for ephemeral test
  repos).
- Workflow residual is precisely flagged in the prior issue comment (2026-04-22) and in
  this manifest's escalation block.

## Phase 3 — Implementation

This branch contains only this fingerprint document.

## Promotion / janitor

Per CRON contract clause (3): planner jobs never promote. This PR is intentionally NOT
labeled `ready-for-review`.

## Idempotency / fingerprint

Re-running this CRON produces the same no-op.

## Tags

- job-id: FNrH6
- oldest-issue-id: `one_track#2`
