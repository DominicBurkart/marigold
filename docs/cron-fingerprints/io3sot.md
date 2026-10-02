# CRON fingerprint — `io3sot`

- job-id: `io3sot`
- run-date: 2026-07-23
- oldest-issue-id: `marigold#68`
- routine: LIFO issue-owner (oldest-15)

## Oldest-15 slice for this repo

Issues in scope for this run (this repo's slice of the global oldest-15
window): **#68 #85**.

(Marigold also has `#94` open in the trio addressed by lead PR #303,
but it falls just outside the global oldest-15 slice this tick.)

## Disposition

**No-op.** Defer to open draft PR **#303** (job `h12tqo`,
`[h12tqo][oldest-issue:68] address oldest open issues: #68 gpg,
#85 take, #94 cache dir`).

| # | title (short) | canonical PR | head SHA | disposition |
|---|---|---|---|---|
| 68 | solve gpg misconfiguration | #303 | `31533df8` | NO-OP — defer to lead draft PR #303 (job `h12tqo`) |
| 85 | `take` stream function | #303 | `31533df8` | NO-OP — defer (implemented in #303 per its summary) |

## State change since last oldest-15 fingerprint (`iweqer`, 2026-07-19)

- **PR #303** unchanged (head still `31533df8`, unchanged since
  2026-06-29).
- **`origin/main`** unchanged (`a44b07c661`) — PR #303 does not need
  a rebase.
- `mergeable_state=blocked` — CI still shows `pending` on
  `31533df8`, same pattern flagged in prior ticks; likely stale
  queued checks.

## Prior-cron lineage

`h12tqo` (2026-06-29 lead PR #303) → `2gn8t0` → `2hig33` → `kgg7th`
→ `2hvj7t` → `8pbk21` (#314) → `ttvnp6` (#316) → `93356f` →
`aj35ca` → `npj82t` → `vkst81` → `bcksoq` → `iweqer` (2026-07-19,
PR #326) → **`io3sot`** (this run).

## Recommended follow-up (janitor, not this job)

1. Inspect the check-run set for `31533df8` on #303. If any checks
   are still `queued` after >24h they are almost certainly stale —
   re-trigger via the Actions UI or `gh workflow run` on the branch
   head.
2. Once CI turns green (or is manually cleared) and review is done,
   promote #303 out of draft. It closes #68, #85, and #94 in one
   shot.
3. Close redundant `chore(cron-*)` fingerprint PRs from prior CRON
   runs.

## Promote-to-human

This PR is intentionally **NOT** labeled `ready-for-review`. Planner
jobs never promote (author-stipulation 3). Janitor decides.

## Tags

- `agent-job:io3sot`
- `oldest-issue:68`
