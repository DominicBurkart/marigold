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
   - `Authorization: Bearer` and `Authorization: Basic` headers, also in
     JSON, dict and call forms (`"Authorization": "Bearer ..."`,
     `("Authorization", "Basic ...")`), standalone `Bearer <20+ chars>`
     tokens, Google OAuth `ya29.` tokens, and URLs with credentials
     (including an empty user name before the colon).
   - Generic credential assignments, best effort: a `password`, `passwd`,
     `passphrase`, `secret`, `token`, `api_key`, `access_key`, `private_key`,
     `signing_key`, `encryption_key`, `master_key`, `hmac_key` or
     `credential` name (with any prefix or suffix), or a standalone word
     `pass`, `pwd` or `creds`, followed by `:`, `=` or `=>` in JSON, YAML,
     env or code. Quoted values of 6 or more characters match, including
     JSON-escaped quotes, nested quotes, apostrophes inside the value and Rust
     raw strings. Unquoted values of 12 or more characters match when not
     followed by `(` or `.`, and so do `$`-prefixed values that are not a
     plain variable reference.
   - Placeholder exemptions apply to the whole value only: `${VAR}`, `$(...)`
     or an upper-case `$ENV_NAME`; a lower-case angle placeholder of at most
     24 characters that contains key, token, secret, password, here, value or
     your (for example `<your-api-key-here>`); and a value that is entirely
     `dummy`, `example`, `changeme`, `your-<words>`, `xxx...` or `todo`
     (optionally followed by up to two digits) or `dummy-` or `example-`
     followed by lower-case words, and an `sk-` value containing `dummy`,
     `example` or `offline-ci`. A variable name or a prefix of a longer value
     never exempts a match. For unquoted `name: Type` annotations a PascalCase
     type is exempt only before `,`, `)`, `>` or `;`, or when it is one of
     `CancellationToken`, `TokenStream`, `TokenTree`, `SecretString` or
     `SecretBox`. A PascalCase value assigned to a password name in YAML or
     env files still matches.
   - The only URL exemption is the `token=` value of an
     `https://codecov.io/.../badge.svg?` URL that stands alone (not inside
     another URL's query). Other parameters in that URL are still scanned, and
     other codecov URLs such as `/upload?token=` match.
   - The `sk-` family requires a left boundary, so words like
     `disk-usage-monitoring` pass.
   - Known best-effort misses: multi-line PEM bodies and YAML block scalars,
     hex-escaped or split strings, fullwidth look-alike characters, a raw AWS
     secret key with no key name next to it, and base64 blobs.
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
   answer is rejected if any later line contains `unsafe` in any case.
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
environment to fail closed on oversized diffs instead. Only the literal value
`1` enables it, and it is honoured outside test mode too, because it only
makes the hook stricter. This PR's own diff needs well over 10 chunks, so with
the strict setting it could not be committed without splitting.

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
