# Marigold change rubric

Score the diff from 0 to 10 against the linked issue. Reply with the score
and a one-line reason.

Start at 10 and subtract:

- Scope: 3 to 5 points if the change does more or less than the issue asks.
- Tests: 3 points if behavior changes without a new or updated test that
  would fail without the change.
- Unrelated edits: 2 points for each drive-by refactor, formatting sweep, or
  file the issue does not need.
- README rule: 3 points if `README.md` and `marigold/README.md` differ after
  the change. They must stay byte-identical.
- Dependencies: 2 points for each new dependency added without a stated
  reason, or any dependency added to a default language crate build.

Scores of 7 or more are acceptable. Score 0 for a change that edits CI
workflows, `publish.sh`, or `.inchworm/`.
