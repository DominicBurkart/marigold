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
  and `--protocol openai-responses` (in `protocol-openai-responses.mjs`,
  loaded by the dispatcher) are implemented. `openai-chat` exits non-zero
  with a `NotImplemented` error, to be filled in by the opencode and pi
  harnesses. A tool call turn may carry a `namespace` (Responses protocol).
- `assert/assert-diagnostic.mjs`: reads the request log and checks that a
  request logged after a given turn contains (or does not contain) a string.
  With `--outputs-only` only tool results are searched, so text in the
  instructions or tool descriptions cannot satisfy or break an assertion.
- `scripts/`: shared model scripts. `broken.json` writes a file with
  `x = range(0, 3).map(undefined_fn)` then reads it twice. `valid.json` is
  the control and writes a file with no diagnostics. The placeholders
  `{cwd}` and `{file}` are filled in from `--var`.
- `lib/in-container.sh`: shell helpers shared by every harness (start the
  mock, run the assertions).
- `claude/`: the Claude Code harness. `VERSION` pins the Claude Code release,
  `Containerfile` installs it, `run.sh` is the host entry point and
  `in-container.sh` runs one scenario inside the container.
- `codex/`: the Codex CLI harness, same layout. See below.

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
node --test integrations/ci/mock-model/test.mjs \
  integrations/ci/mock-model/test-openai-responses.mjs \
  integrations/ci/assert/test.mjs
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

## Codex CLI

```bash
MARIGOLD_LSP_BIN=target/release/marigold-lsp bash integrations/ci/codex/run.sh
```

The container, mounts and `KEEP=1` are the same as for Claude Code, with
`@openai/codex` pinned in `codex/VERSION`. Codex is configured only through a
`CODEX_HOME` inside the container. `in-container.sh` writes a `config.toml`
with a `mock` model provider (`wire_api = "responses"`, a dummy `env_key`,
no retries), `approval_policy = "never"`, `danger-full-access` (the container
is the sandbox) and analytics and feedback off, then appends the real
`integrations/codex/config.toml`. The binary is on the `PATH` as
`marigold-lsp`, so the snippet is used unmodified. The real
`integrations/codex/AGENTS.md` is copied into the throwaway project, which is
not a git repository (`codex exec --skip-git-repo-check`).

Every scenario asserts that:

- `codex mcp list` shows the `marigold` server as enabled.
- The first model request carries the `AGENTS.md` text in a user message
  headed `# AGENTS.md instructions for /work/project`.
- The tools array offers the MCP tools in the namespace `mcp__marigold`.

The scripts are `codex-broken.json`, `codex-valid.json` and
`codex-error.json`. Turn 1 writes `hello_world.marigold` with `exec_command`,
turn 2 calls `marigold_check` (namespace `mcp__marigold`) with the file path
and turn 3 is the final text. Codex sends the MCP result back as the
`function_call_output` of the next request, as the text `Wall time: ...`
followed by the JSON the server returned, so the assertions only look at tool
results (`--outputs-only`) after turn 2:

- Broken scenario: the result contains `"code":"undefined-fn"` and
  `"severity":"information"` (`ok` stays true).
- Control scenario: a valid file gives `"ok":true` and no `undefined-fn`.
- Error scenario: `range(Colour).return` gives `"code":"undefined-enum"` and
  `"severity":"error"`.

## Adding a harness

Add a directory next to `claude/` with a `Containerfile`, a `run.sh` and an
in-container script that reuse `mock-model`, `assert`, `scripts` and `lib`.
Implement the protocol the harness speaks in `mock-model.mjs`.
