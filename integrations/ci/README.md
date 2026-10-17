# Harness CI

Optional test infrastructure. It checks the claim "a coding agent's edit of a
`.marigold` file receives `marigold-lsp` diagnostics" inside an isolated
rootless podman container, with a scripted fake model server. It needs no
credentials and makes no network model calls. Nothing here affects building or
using Marigold, and no Rust crate depends on it.

## Layout

- `mock-model/mock-model.mjs`: dependency-free Node (20 or newer) server that
  replays a scripted list of assistant turns, in streaming and non-streaming
  form, and logs every request as one JSON line. Only `--protocol anthropic`
  is implemented. `openai-chat` and `openai-responses` exit non-zero with a
  `NotImplemented` error, to be filled in by the opencode, Codex and pi
  harnesses.
- `assert/assert-diagnostic.mjs`: reads the request log and checks that a
  request logged after a given turn contains (or does not contain) a string.
- `scripts/`: shared model scripts. `broken.json` writes a file with
  `x = range(0, 3).map(undefined_fn)` then reads it twice. `valid.json` is
  the control and writes a file with no diagnostics. The placeholders
  `{cwd}` and `{file}` are filled in from `--var`.
- `lib/in-container.sh`: shell helpers shared by every harness (start the
  mock, run the assertions).
- `claude/`: the Claude Code harness. `VERSION` pins the Claude Code release,
  `Containerfile` installs it, `run.sh` is the host entry point and
  `in-container.sh` runs one scenario inside the container.

## Run locally

Requirements: podman (rootless), a built `marigold-lsp`, and network access
the first time, to build the image.

```bash
cargo build -p marigold-lsp --release
MARIGOLD_LSP_BIN=target/release/marigold-lsp bash integrations/ci/claude/run.sh
```

The container gets only three mounts: this checkout read-only at `/repo`,
the `marigold-lsp` binary read-only, and a fresh tmpfs at `/work`. No home
directory or host credential is mounted. Set `KEEP=1` to keep the containers
and their named volumes for debugging; otherwise they are removed on exit.

Run the unit tests of the helpers with:

```bash
node --test integrations/ci/mock-model/test.mjs integrations/ci/assert/test.mjs
```

## What is asserted

Claude Code runs non-interactively (`claude -p`) against the mock, which
plays the script: write the file, read it, read it again, then finish. Claude
Code delivers LSP diagnostics asynchronously on the model's next turns inside
a `<new-diagnostics>` block, which is why the script reads the file twice.

- Broken scenario: some request logged after turn 1 contains `undefined-fn`,
  the diagnostic code of the server.
- Control scenario: after writing a valid file, no request after turn 1
  contains `undefined-fn`.

The run prints a PASS or FAIL summary and exits non-zero on failure.

## Adding a harness

Add a directory next to `claude/` with a `Containerfile`, a `run.sh` and an
in-container script that reuse `mock-model`, `assert`, `scripts` and `lib`.
Implement the protocol the harness speaks in `mock-model.mjs`.
