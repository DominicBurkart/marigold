# Development: pre-commit hook

The hook in `.cargo-husky/hooks/pre-commit` starts with a local security
review of the staged diff, then runs the cargo checks and the coverage gate.
cargo-husky copies the hook into `.git/hooks` when it installs it, so an
installed copy goes stale: re-install it after the hook changes.

## Threat model

This hook is accident prevention, not a security boundary. The script is part
of the checked-out tree, so a commit can change it, `--no-verify` exists for
anyone who types it, and CI is the only enforcement that cannot be skipped
locally. The goal is to stop honest mistakes such as pasting a token or a
private key into a commit.

## Prerequisites

- `nanna` on `PATH`.
- A running ollama with the `gemma4:e4b` model pulled
  (`ollama pull gemma4:e4b`).
- GNU `grep` (with `-a`, `-o` and `-E`), `awk`, `sed`, `tr`, `cut`, `sort`,
  `od`, `cmp`, `mktemp` and coreutils `timeout`. The tests also pass with
  `gawk --posix`; other awk implementations such as mawk are untested.

## How the review works

`scripts/nanna_security_review.sh` reads the staged diff on stdin (the hook
uses `--no-color --no-ext-diff --no-textconv`), strips ANSI color codes and
fails closed if an ESC byte or a NUL byte remains. It then runs two layers.

1. A deterministic pre-scan of every added line and of the diff file header
   lines, including files excluded from the model. It blocks PEM private and
   public key blocks, cloud and forge tokens (AWS, GitHub, GitLab, Slack and
   Slack webhooks, Google, Stripe live and test keys, Twilio, Azure, SendGrid,
   Hugging Face, npm, `sk-` keys, JWTs), `Authorization: Bearer` headers, URLs
   with credentials, and generic assignments of a `password`, `secret`,
   `token`, `api_key`, `access_key`, `private_key` or `credential` name to a
   quoted value of 8 or more characters, or to an unquoted value that
   contains a digit. Values starting with `<` or `$`, and values containing
   `dummy`, `example`, `offline-ci` or `changeme`, are treated as
   placeholders. The `sk-` family skips only the documented dummy markers and
   requires a left boundary, so words like `disk-usage-monitoring` pass. The
   scan reports the file and line (or `header`), never the matched value. It
   only sees lines the diff shows: a PEM body whose BEGIN line is unchanged
   context is not detected.
2. A best-effort model review (`nanna chat -m gemma4:e4b --temperature 0
   --no-ensure-pod`) for secrets, tokens, sensitive internals and PII. The
   model is a second layer behind the pre-scan, not a guarantee. The diff is
   wrapped in random per-run delimiters and marked as data. Leading blank and
   tracing log lines that nanna prints on stdout are skipped, then the first
   line of the answer must be `VERDICT: SAFE` or `VERDICT: UNSAFE`, optionally
   after an `Assistant:` prefix. Control characters are stripped from
   anything printed.

### Allow marker

A verified false positive can be suppressed by ending that added line with
`nanna-review: allow <pattern-name>`, for example
`nanna-review: allow aws-access-key`, or several comma separated names. The
marker must be the last token of the line, only the named patterns are
suppressed on that line, a marker in a file path or inside a string literal is
ignored, and `private-key-block` can never be suppressed. The pattern name is
printed in each pre-scan report.

### Excluded from the model

Lockfiles (`*.lock`, `package-lock.json`, `pnpm-lock.yaml`, `go.sum`), the
`development_metadata/badges/` directory and files git reports as binary are
excluded from the model review only, and are listed in the output.

## Fail-closed behaviour and the deliberate degradation

The commit is blocked when the diff contains NUL or unstrippable escape bytes,
the pre-scan matches, nanna is missing, nanna fails or times out, or the first
output line is not a valid verdict.

The model review splits the diff into 3500 byte chunks on line boundaries
(dense base64 or hex can approach ollama's default 4096 token context at larger
sizes) and reviews at most the first 10. Lines longer than a chunk are split
and marked with `[[continued]]`. If the diff has more chunks, the remainder is
reviewed by the pre-scan only: the script prints a loud WARNING and still exits
0 when the pre-scan and every reviewed chunk passed. This is a deliberate
degradation so that one large new file does not make a commit impossible.

Each nanna call is limited to 120 seconds (then killed after 5 more) and the
review stops at the first UNSAFE chunk, so the worst case is 10 chunks times
120 seconds, 20 minutes.

## No bypass in the script

Do not use `--no-verify`. The environment overrides (`NANNA_BIN`,
`NANNA_REVIEW_MODEL`, `NANNA_REVIEW_CHUNK_BYTES`, `NANNA_REVIEW_MAX_CHUNKS`,
`NANNA_REVIEW_TIMEOUT`) only take effect when `NANNA_REVIEW_TEST=1` is set,
which the contract tests do. Run the tests with
`python3 -I scripts/tests/test_nanna_security_review.py`; CI runs them in the
style workflow.

## Privacy

The diff is sent to the Ollama host that nanna is configured to use, as part
of the prompt. The prompt is passed to `nanna chat` on its command line
(`-p`), because nanna has no stdin or file prompt input, so other local users
can see it in the process list while the review runs. The model may quote
diff text in its findings, which are printed to your terminal.
