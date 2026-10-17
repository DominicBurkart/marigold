# Harness CI

Optional test infrastructure. It checks the claim "a coding agent's edit of a
`.marigold` file receives `marigold-lsp` diagnostics" inside an isolated
rootless podman container, with a scripted fake model server. It needs no
credentials and makes no network model calls. Nothing here affects building or
using Marigold, and no Rust crate depends on it.

## Layout

- `mock-model/mock-model.mjs`: dependency-free Node (20 or newer) server that
  replays a scripted list of assistant turns, in streaming and non-streaming
  form, and logs every request as one JSON line. The protocols are
  `anthropic`, `openai-chat` and `openai-responses`. The last two live in
  `mock-model/protocol-<name>.mjs` and are loaded on demand by the
  dispatcher. A tool call turn may carry a `namespace` (Responses protocol).
- `assert/assert-diagnostic.mjs`: reads the request log and checks that a
  request logged after a given turn contains (or does not contain) a string.
  `--scope tool-results` restricts the search to tool results (chat `tool`
  messages, Anthropic `tool_result` blocks and Responses `*_output` items),
  so text in the system prompt, instructions, tool descriptions or the
  model's own calls cannot satisfy or break an assertion. `--outputs-only`
  is an alias of `--scope tool-results`.
- `scripts/`: shared model scripts. `broken.json` writes a file with
  `x = range(0, 3).map(undefined_fn)` then reads it twice. `valid.json` is
  the control and writes a file with no diagnostics. The placeholders
  `{cwd}` and `{file}` are filled in from `--var`.
- `scripts/opencode-*.json`: turn scripts for the opencode harness. Each
  calls opencode's `write` tool once, then answers with text.
- `lib/in-container.sh`: shell helpers shared by every harness (start the
  mock, run the assertions).
- `claude/`: the Claude Code harness. `VERSION` pins the Claude Code release,
  `Containerfile` installs it, `run.sh` is the host entry point and
  `in-container.sh` runs one scenario inside the container.
- `codex/`: the Codex CLI harness, same layout. See below.
- `opencode/`: the opencode harness, same file layout. See below.

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
node --test integrations/ci/mock-model/test*.mjs integrations/ci/assert/test*.mjs
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
results (`--scope tool-results`, or its alias `--outputs-only`) after turn 2:

- Broken scenario: the result contains `"code":"undefined-fn"` and
  `"severity":"information"` (`ok` stays true).
- Control scenario: a valid file gives `"ok":true` and no `undefined-fn`.
- Error scenario: `range(Colour).return` gives `"code":"undefined-enum"` and
  `"severity":"error"`.

## opencode

`bash integrations/ci/opencode/run.sh` runs the real opencode CLI
(`opencode-ai`, pinned in `opencode/VERSION`) with `opencode run --model
mock/mock-model` against the mock speaking `openai-chat`. The container has
the same three mounts as above, and `HOME` and all four `XDG_*` directories
point into the tmpfs, so no host state is read or written.

The generated `opencode.json` merges the `lsp` block read from the real
`integrations/opencode/opencode.json` with a custom provider `mock`
(`@ai-sdk/openai-compatible`, `baseURL` the mock, dummy `apiKey`). Auto
update and sharing are disabled and the environment sets
`OPENCODE_DISABLE_AUTOUPDATE`, `OPENCODE_DISABLE_MODELS_FETCH` and
`OPENCODE_DISABLE_LSP_DOWNLOAD`. The integration's `AGENTS.md` is copied into
the project directory, and the request log confirms opencode sends it to the
model. No credentials are used. Network access is still needed: the image
build downloads opencode, and opencode installs the provider package
`@ai-sdk/openai-compatible` from npm on first run.

### Assertions

All assertions look only at tool result messages sent back to the model after
the model's `write` call.

- `error`: writing `range(Colour).return` yields a tool result containing the
  diagnostic text `is not a declared enum`.
- `valid` (control): a valid file yields a plain "Wrote file successfully".

opencode formats a diagnostic as `ERROR [line:col] message` and does not print
the diagnostic code. The code `undefined-enum` therefore never reaches the
model, so the assertion matches the message text.

### Severity behaviour

Characterization of opencode 1.x behaviour, update if opencode changes.
These scenarios pin what opencode 1.19.3 does today. They are not a contract
of Marigold. If opencode starts surfacing more severities, they fail on
purpose and the expectations and the `AGENTS.md` advice should be revised.

- `info`: `x = range(0, 3).map(undefined_fn)` publishes an Information
  diagnostic (`undefined-fn`). opencode does NOT append it: the tool result is
  only "Wrote file successfully."
- `warning`: a stream variable read before its declaration publishes a
  Warning. opencode does NOT append it either.
- `mixed`: one file with an error, a warning and an information diagnostic.
  The tool result contains only the `ERROR` entry, which shows the others are
  dropped by opencode and not merely late.

Conclusion: after `write` opencode shows
the model Error-severity diagnostics only, under the header "LSP errors
detected in this file, please fix:". Only `write` is exercised here.

## Adding a harness

Add a directory next to `claude/` with a `Containerfile`, a `run.sh` and an
in-container script that reuse `mock-model`, `assert`, `scripts` and `lib`.
Implement the protocol the harness speaks in `mock-model.mjs`.
