# cron-hewbx1 — marigold disposition

**Job**: `agent-job:hewbx1`
**Oldest open issue authored by @DominicBurkart**: #68 (solve gpg misconfiguration)
**Run date**: 2026-08-09
**Trigger**: LIFO issue-owner cron (https://dominic.computer/blog/2026/routines?format=md)

## Fingerprint

| Issue | Prior artifact | State | Head SHA |
|-------|----------------|-------|----------|
| #68 gpg misconfiguration | PR #303 `address oldest open issues: #68 gpg, #85 take, #94 cache dir` | open · draft · **blocked** | `31533df831ac28aaf7a65bcb06f43f1b5277b662` |
| #85 `take` | PR #303 (same) | open · draft · **blocked** | `31533df831ac28aaf7a65bcb06f43f1b5277b662` |
| #94 move marigold cache into temp dir | PR #303 (same) | open · draft · **blocked** | `31533df831ac28aaf7a65bcb06f43f1b5277b662` |

Head SHA is unchanged since the previous cron run (`7ph5vy`, 2026-08-05). No new commits.

## Disposition: NO-OP (defer to PR #303)

PR #303 implements all three of #68, #85, #94 in one bundle with tests. The bottleneck is on merge-time CI / review, not on implementation.

Per rule (1) — "same trigger produces the same no-op on re-run" — this planner run does not spawn duplicate implementations while an open PR exists for the same issues.

## Not promoted

Per rule (3): planner jobs never promote. Applying the `ready-for-review` label on PR #303 is the janitor job's responsibility.

## Prior fingerprints of the same disposition

- PR #349 (`cron-7ph5vy`, 2026-08-05)
- PR #346 (`cron-wy8egd`, 2026-08-02)
- PR #344 (`cron-ugztum`, 2026-08-01)
- PR #342 (`cron-ymaxuw`, 2026-07-31)
- PR #340 (`cron-vu6b6e`, 2026-07-30)
- PR #336 (`cron-1k1xe2`, 2026-07-27)
- PR #331 (`cron-io3sot`, 2026-07-23)
