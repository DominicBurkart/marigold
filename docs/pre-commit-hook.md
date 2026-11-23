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
  `od`, `cmp`, `mktemp`, bash 4 and coreutils `timeout`.
- The contract tests pass with gawk 5, `gawk --posix`, mawk 1.3.4 and the
  busybox awk applet (reproduced with nix-shell). Other awks are untested.

## How the review works

`scripts/nanna_security_review.sh` reads the staged diff on stdin (the hook
uses `--no-color --no-ext-diff --no-textconv`), strips ANSI color codes and
fails closed if an ESC byte or a NUL byte remains. It then runs two layers.

1. A deterministic pre-scan of every added line and of the diff file header
   lines, including files excluded from the model. It reports the file and
   line (or `header`), never the matched value. Pattern families:
   - PEM key blocks (any `BEGIN ... KEY` line, so public keys match too, and
     PGP private key blocks). `BEGIN CERTIFICATE` is not blocked.
   - Cloud and forge tokens: AWS, GitHub, GitLab, Slack and Slack webhooks,
     Google, Stripe live and test keys, Twilio, Azure, SendGrid, Hugging
     Face, npm, `sk-` keys and JWTs.
   - `Authorization: Bearer` and `Authorization: Basic` headers, and URLs
     with credentials.
   - Generic credential assignments, best effort: a `password`, `secret`,
     `token`, `api_key`, `access_key`, `private_key` or `credential` name
     (with any prefix or suffix, in JSON, YAML, env or code, including
     JSON-escaped quotes, nested quotes and Rust raw strings) assigned a
     quoted value of 6 or more characters, or an unquoted value of 12 or more
     characters that is not followed by `(` or `.`.
   - Placeholder exemptions apply to the value only: a whole `${VAR}`,
     `$(...)` or `$VAR`, an angle placeholder such as `<your-api-key-here>`,
     a value starting with `dummy`, `example`, `changeme`, `your-`, `xxx` or
     `todo`, an `sk-` value containing `dummy`, `example` or `offline-ci`,
     and, for unquoted values, a pure PascalCase type name. A variable name or
     the middle of a longer value never exempts a match. The `?token=` query
     of an `https://codecov.io/` badge URL is exempt.
   - The `sk-` family requires a left boundary, so words like
     `disk-usage-monitoring` pass.
   - A PEM body whose BEGIN line is unchanged context is not detected, and
     secrets in files git reports as binary (NUL bytes or a `-diff`
     attribute) are not seen by either layer.
2. A best-effort model review (`nanna chat -m gemma4:e4b --temperature 0
   --no-ensure-pod`) for secrets, tokens, sensitive internals and PII. The
   model is a second layer behind the pre-scan, not a guarantee. The diff is
   wrapped in random per-run delimiters and marked as data. Leading blank and
   tracing log lines that nanna prints on stdout are skipped. The first line
   of the answer must then be exactly `VERDICT: SAFE` or `VERDICT: UNSAFE`
   (an `Assistant:` prefix and trailing whitespace are tolerated), and a SAFE
   answer is rejected if any later line contains the word UNSAFE in any case.
   Control characters are stripped from anything printed.

### Allow marker

A verified false positive can be suppressed by ending that added line with
`nanna-review: allow <pattern-name>`, for example
`nanna-review: allow aws-access-key`, or several comma separated names. The
marker must be the last token of the line (a trailing CR from CRLF files is
tolerated). It applies per line and per pattern name: every match of a named
pattern on that line is suppressed, including a second secret of the same
pattern, and nothing else. A marker in a file path, or followed by anything
else such as the closing quote of a string literal, is ignored, and
`private-key-block` can never be suppressed. The pattern name is printed in
each pre-scan report.

### Excluded from the model

The known lockfiles `Cargo.lock`, `yarn.lock`, `package-lock.json`,
`pnpm-lock.yaml`, `poetry.lock`, `go.sum`, `Gemfile.lock`, `composer.lock` and
`flake.lock` at any directory depth, anything under
`development_metadata/badges/`, and files git reports as binary are excluded
from the model review only, and are listed in the output. Other `*.lock`,
`*.svg` and image or document files are reviewed.

## Fail-closed behaviour and the deliberate degradation

The commit is blocked when the diff contains NUL or unstrippable escape bytes,
the pre-scan matches, nanna is missing, nanna fails or times out, or the
output has no valid verdict or contradicts itself.

The model review splits the diff into 3500 byte chunks on line boundaries
(dense base64 or hex can approach ollama's default 4096 token context at larger
sizes) and reviews at most the first 10. Files are ordered smallest first so
that a large new file does not push small ones out of the reviewed chunks.
Lines longer than a chunk are split and marked with `[[continued]]`.

If the diff has more chunks, the remainder is reviewed by the pre-scan only:
the script prints a loud WARNING and still exits 0 when the pre-scan and every
reviewed chunk passed. This is a deliberate degradation so that one large new
file does not make a commit impossible. In those unreviewed chunks only the
deterministic patterns are checked: PII, internal hostnames and every other
shape that only the model could catch are not, and padding can push such
content out of the reviewed chunks. Set `NANNA_REVIEW_STRICT=1` in your
environment to fail closed on oversized diffs instead. It is honoured outside
test mode too, because it only makes the hook stricter.

Each nanna call is limited to 120 seconds (then killed after 5 more) and the
review stops at the first UNSAFE chunk, so the worst case is 10 chunks times
120 seconds, 20 minutes.

## No bypass in the script

Do not use `--no-verify`. The other environment overrides (`NANNA_BIN`,
`NANNA_REVIEW_MODEL`, `NANNA_REVIEW_CHUNK_BYTES`, `NANNA_REVIEW_MAX_CHUNKS`,
`NANNA_REVIEW_TIMEOUT`) only take effect when `NANNA_REVIEW_TEST=1` is set,
which the contract tests do. Run the tests with
`python3 -I scripts/tests/test_nanna_security_review.py`; CI runs them in the
style workflow. One test runs the pre-scan over this change's own files, the
READMEs and the repo's dummy-key lines, so a fixture that looks like a secret
breaks the build: assemble such fixtures from fragments.

## Privacy

The diff is sent to the Ollama host that nanna is configured to use, as part
of the prompt. The prompt is passed to `nanna chat` on its command line
(`-p`), because nanna has no stdin or file prompt input, so other local users
can see it in the process list while the review runs. The model may quote
diff text in its findings, which are printed to your terminal.
