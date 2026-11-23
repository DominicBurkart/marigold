# Development: pre-commit hook

The hook in `.cargo-husky/hooks/pre-commit` starts with a local security
review of the staged diff, then runs the cargo checks and the coverage gate.
cargo-husky copies the hook into `.git/hooks` when it installs it, so an
installed copy goes stale: re-install it after the hook changes.

## Prerequisites

- `nanna` on `PATH`.
- A running ollama with the `gemma4:e4b` model pulled
  (`ollama pull gemma4:e4b`).

## How the review works

`scripts/nanna_security_review.sh` reads the staged diff on stdin and runs two
layers.

1. A deterministic pre-scan of every added line, including lockfiles and other
   files excluded from the model. It blocks private-key blocks, cloud and
   forge tokens (AWS, GitHub, GitLab, Slack, Google, Stripe, Twilio, Azure,
   Hugging Face, npm, JWTs), `Authorization: Bearer` headers, URLs with
   credentials, and generic `password`, `secret`, `token` or `api_key`
   assignments with a quoted value of 8 or more characters. It reports the
   file and line, never the matched value. Append `nanna-review: allow` to a
   verified false positive line to suppress the pre-scan for that line only.
2. A best-effort model review (`nanna chat -m gemma4:e4b --temperature 0`)
   for secrets, tokens, sensitive internals and PII. The model is a second
   layer behind the pre-scan, not a guarantee. The diff is wrapped in random
   per-run delimiters and marked as data, and only the first non-empty line of
   the answer counts: it must be `VERDICT: SAFE` or `VERDICT: UNSAFE`.

Lockfiles, `*.svg`, `development_metadata/badges/**` and binary files are
excluded from the model review only, and are listed in the output.

## Fail-closed behaviour

The commit is blocked when the diff contains NUL bytes, the pre-scan matches,
nanna is missing, nanna fails or times out, the first output line is not a
valid verdict, or the diff is too large. The model review splits the diff into
6000 byte chunks (one chunk fits ollama's default 4096 token context) and
allows at most 10 chunks. Lines longer than a chunk are split and marked with
`[[continued]]`. Nothing is skipped silently: a larger diff is rejected, so
split the commit.

Each nanna call is limited to 120 seconds and the review stops at the first
UNSAFE chunk, so the worst case is 10 chunks times 120 seconds, 20 minutes.

## No bypass

There is no bypass: do not use `--no-verify`. The environment overrides
(`NANNA_BIN`, `NANNA_REVIEW_MODEL`, `NANNA_REVIEW_CHUNK_BYTES`,
`NANNA_REVIEW_MAX_CHUNKS`, `NANNA_REVIEW_TIMEOUT`) only take effect when
`NANNA_REVIEW_TEST=1` is set, which the contract tests do. Run the tests with
`python3 -I scripts/tests/test_nanna_security_review.py`; CI runs them in the
style workflow.

## Privacy

The diff is sent to the Ollama host that nanna is configured to use, as part
of the prompt. The prompt is passed to `nanna chat` on its command line
(`-p`), because nanna has no stdin or file prompt input, so other local users
can see it in the process list while the review runs.
