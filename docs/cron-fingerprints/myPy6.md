# CRON fingerprint — job myPy6 (marigold slice)

- job-id: myPy6
- oldest-issue-id: one_track#2
- generated: 2026-05-21

Stateless CRON `myPy6` (LIFO issue-owner routine) owns the oldest 15
`author:DominicBurkart` open issues across the managed repositories. This
branch records the **marigold** slice: issues #68, #85, #94, #103.

## Per-issue disposition

| issue | title | canonical PR | head SHA | disposition |
|------:|-------|-------------:|----------|-------------|
| #68 | solve gpg misconfiguration | #192 | `12d771ff` | NO-OP — defer to #192 |
| #85 | `take` stream function | #160 | `bf346cc5` | NO-OP — defer to #160 |
| #94 | move cache into a temporary directory | _(this PR)_ | `07684ac` | **IMPLEMENTED** |
| #103 | `take_while` stream function | #115 | `bae1abad` | NO-OP — defer to #115 |

Issue #94 had no prior open PR, so this job took ownership and implemented it
on this branch (commit `07684ac` — see the PR description). It is also assigned
to @DominicBurkart per the cron contract (unassigned + no prior work). The
other three issues are already covered by canonical agent PRs and are deferred
per fingerprint rule (1) — *"the same trigger produces the same no-op on
re-run"*.

## Janitor carry-over

- #192 (#68): supersedes the older #159; janitor dedups.
- #115 (#103): long-running canonical PR (43 commits, 27 review comments) —
  needs rebase / review attention, not a competing PR.
- Close superseded marigold fingerprint PRs from prior crons once this snapshot
  lands (detectable via the `agent-job:*` / `oldest:one_track-2` tags).

## Promote-to-human

Per CRON contract clause (3), planner jobs never promote. This PR is **not**
labeled `ready-for-review`; the janitor decides promotion.

## Tags

- agent-job:myPy6
- oldest:one_track-2
