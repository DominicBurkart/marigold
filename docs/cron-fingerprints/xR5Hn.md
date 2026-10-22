# cron-xR5Hn — fingerprint manifest (marigold slice)

```
job-id:           xR5Hn
oldest-issue-id:  one_track#2
fingerprint-date: 2026-05-10
prior-crons:      nnkMo (PR #215), QO4Td (PR #192), original (PR #159)
```

## Run scope

Cross-repo run covering the 15 oldest `author:DominicBurkart` open
issues across managed repos. With one_track#7 SKIPPED as BLOCKED, the
15th unblocked oldest issue is **marigold#68** (solve gpg
misconfiguration). Companion manifests live in `one_track`,
`velib-mcp`, and `nanna-coder` on branches `claude/<name>-xR5Hn`.

## Per-issue disposition

| issue | canonical PR | head sha | disposition |
|------:|-------------:|----------|-------------|
| #68   | **#215**     | (current head of `claude/inspiring-feynman-nnkMo`) | NO-OP — defer to #215 (also delivers #85) |

## Why #215 over #192 and #159

Three open PRs target #68:

- **#215** (cron-nnkMo) — replaces `git config commit.gpgsign false`
  with `--no-gpg-sign` flag on git commit invocations in
  `marigold-grammar/tests/bisect_*.rs`, adds
  `no_gpg_config_workaround.rs` regression guard (3 tests), **and**
  delivers #85 (`take(n)` stream function with full
  grammar/AST/codegen/cardinality/test coverage). Most comprehensive
  scope.
- **#192** (cron-QO4Td) — same Rust-side fix as #215 but **without** the
  #85 delivery. Labeled `oldest:one_track-2`, `agent-job:QO4Td`.
- **#159** (original) — earliest version of the same Rust-side fix.

All three carry the same `.github/workflows/badges.yaml` deferral
(updating the `git commit` line requires `workflows` token scope, which
no cron agent has). A repo admin must apply that one-line change
directly outside the agent flow.

**Recommended**: promote #215 (broadest scope), close #192 and #159 as
duplicates. The `--no-gpg-sign` flag approach is consistent across all
three; #215 simply ships more.

## Promote-to-human

Per CRON contract clause (3) — **planner jobs never promote**. This
PR is intentionally **NOT** labeled `ready-for-review`. The janitor
job decides promotion based on PR #215.

## Tags

- `agent-job:xR5Hn`
- `oldest:one_track-2`
