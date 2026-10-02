# Cron tick `lrfgv5` — fingerprint disposition (marigold)

**Job**: `lrfgv5`
**Prior tick**: `cgy7tc` (PR #376, 2026-08-23)
**Semantic delta since prior tick**: none.
**In-scope for this session** (per harness repo-scope): 8 repos, excluding `1-Track/one_track`. Without one_track#6/#7 slots 1–2, this session's slice grows to include marigold#94 in addition to #68 and #85 (previously the marigold two-issue slice under the global rule).

## Fingerprint (issue → open-PR → last-commit-sha)

| Issue | Open lead PR | Last-commit SHA | Base SHA | State | Delta vs `cgy7tc` |
|---|---|---|---|---|---|
| #68 gpg misconfiguration | #303 | `31533df831ac28aaf7a65bcb06f43f1b5277b662` | `a44b07c661ed09a986b366dcb67d9765bdcb8643` | draft, `mergeable_state: blocked` | byte-identical |
| #85 `take` stream fn      | #303 | `31533df831ac28aaf7a65bcb06f43f1b5277b662` | `a44b07c661ed09a986b366dcb67d9765bdcb8643` | draft, `mergeable_state: blocked` | byte-identical |
| #94 cache dir             | #303 (primary) / #267 (alt) | `31533df831ac28aaf7a65bcb06f43f1b5277b662` | `a44b07c661ed09a986b366dcb67d9765bdcb8643` | draft, `mergeable_state: blocked` | not previously fingerprinted; already covered by lead PR |

Base `origin/main = a44b07c661ed09a986b366dcb67d9765bdcb8643` — unchanged since `cgy7tc`. #94 is a duplicate workstream (older PR #267 open alongside the umbrella #303); the janitor should keep the newer #303 and close the older #267 once #303 lands. Per the routine's fingerprint rule, this tick is a **no-op**: no new impl PR spawns for #68/#85/#94. Promote-to-human lives on PR #303; planner jobs never promote.

## For the janitor
Prior `chore(cron-*): fingerprint disposition for #68 #85 …` PRs (`wy8egd` → `cgy7tc`) can be closed on sight.
