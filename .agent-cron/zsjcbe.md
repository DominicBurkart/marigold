# cron `zsjcbe` disposition (marigold)

- job-id: `zsjcbe`
- oldest-issue-id: 68
- prior tick: [`17ogge` (#358)](https://github.com/DominicBurkart/marigold/pull/358)
- routine: https://dominic.computer/blog/2026/routines?format=md

## Byte-identical to `17ogge`

Nothing has changed since `17ogge` closed the last tick at 2026-08-11 19:20 UTC:

| axis                          | at `17ogge`                                                | at `zsjcbe`                                                | Δ |
|-------------------------------|------------------------------------------------------------|------------------------------------------------------------|---|
| marigold `main` head          | `a44b07c661ed09a986b366dcb67d9765bdcb8643` (PR #255 merge) | `a44b07c661ed09a986b366dcb67d9765bdcb8643` (PR #255 merge) | none |
| PR #303 head                  | `31533df831ac28aaf7a65bcb06f43f1b5277b662`                 | `31533df831ac28aaf7a65bcb06f43f1b5277b662`                 | none |
| PR #303 `mergeable_state`     | `blocked`                                                  | `blocked`                                                  | none |
| oldest-15 window (this repo)  | `#68 #85`                                                  | `#68 #85`                                                  | none |

Per contract clause (1) ("the same trigger produces the same no-op on re-run"),
this tick opens no impl PRs and touches no impl branches.

## Documented follow-up on PR #303 (janitor-owned; unchanged from `17ogge`)

- **#68 partial**: `.github/workflows/badges.yaml:47` still needs `--no-gpg-sign`; requires the `workflows` token scope this planner does not hold. One-line diff already in the PR #303 body.

## Disposition

- **#68, #85**: NO-OP (defer to PR #303) — rule (1), byte-identical to `17ogge`.

Per rule (3): planner jobs never promote. `ready-for-review` on PR #303 (or the `workflows`-scoped follow-up) is the janitor's job.

## Superseded planner PRs on this repo

- #358 (cron-`17ogge`), #356 (cron-`2u8f0w`)
