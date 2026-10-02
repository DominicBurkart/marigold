# cron-17ogge disposition — marigold

- job-id: `17ogge`
- prior job: `2u8f0w` (PR #356)
- routine: LIFO issue-owner cron — https://dominic.computer/blog/2026/routines?format=md
- slice: oldest-15 authored by @DominicBurkart across in-scope repos
- oldest issue in this repo (within slice): **#68** (`solve gpg misconfiguration`, 2026-02-15)
- also in slice: **#85** (`` `take` ``, 2026-03-15)

## Fingerprint — issues covered by an open PR

| Issue | Prior artifact | State | Head SHA |
|-------|----------------|-------|----------|
| #68 solve gpg misconfiguration | PR #303 `[h12tqo][oldest-issue:68] address oldest open issues: #68 gpg, #85 take, #94 cache dir` | open · draft · **blocked** | `31533df831ac28aaf7a65bcb06f43f1b5277b662` |
| #85 `take` stream function    | PR #303 (implements `take(n)` end-to-end) — also PR #160 (older alt)                              | open · draft · **blocked** | `31533df831ac28aaf7a65bcb06f43f1b5277b662` |

Base drift vs `2u8f0w`:

- marigold `main` head: `a44b07c661ed09a986b366dcb67d9765bdcb8643` — **unchanged** (last merge is #255, 2026-05-20).
- PR #303 head: `31533df831ac28aaf7a65bcb06f43f1b5277b662` — **unchanged**.
- PR #303 `mergeable_state`: `blocked` — **unchanged** (branch protection / missing required checks).
- Oldest-15 window membership: **unchanged** (this repo contributes only #68 and #85).

## Documented follow-ups on PR #303 (janitor-owned; unchanged from `2u8f0w`)

- **#68 partial**: `.github/workflows/badges.yaml:47` still needs `--no-gpg-sign`.
  Requires the `workflows` token scope this planner does not hold. Diff on
  PR #303 body.

## Disposition

- **#68, #85**: NO-OP (defer to PR #303) — rule (1), byte-identical to `2u8f0w`.

Per rule (3): planner jobs never promote. `ready-for-review` on PR #303 (or on
the `workflows`-scoped follow-up) is the janitor's job.

## Superseded planner PRs on this repo (close-on-sight after janitor confirms no unique content)

- PR #356 (cron-`2u8f0w`) — prior no-op disposition
