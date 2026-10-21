# Tested harness versions

The "oldest tested" version is a tested lower bound for the scripted
scenarios in this directory. It is not a vendor-documented minimum. Versions
between a passing and a failing entry that are not listed were not run and are
untested. The search assumed that pass or fail is monotonic in the version,
which was only spot-checked. All runs used a release `marigold-lsp` built from
`main`, one scripted fake model, and `HARNESS_VERSION=<x>` (see the README).

A FAIL is classified as one of:

- integration: the diagnostics do not reach the model on that version.
- test infrastructure: the integration works but the scripted scenario or an
  assertion is written for a newer CLI.
- install: the package cannot be installed or started.

## Claude Code (`@anthropic-ai/claude-code`)

A version counts as passing only if it passes 3 consecutive fresh runs of the
full scenario set. Earlier single-run results were superseded by the runs
below, all on 2026-10-21 without any scenario delay.

- Pinned: 2.1.289, 3/3 runs.
- Oldest 3/3: 2.1.16 (3/3). Also 3/3: 2.1.53 and 2.1.289.
- Latest (2.1.302): PASS in a single run on 2026-10-18, not repeated.
- FLAKY: 2.1.6 is not a lower bound. It passed 5/5 runs on an idle machine,
  5/6 runs under CPU load, and 2/4 runs for the reviewer.
- Failing every run, just below: 2.1.5, 2.1.4, 2.1.2, 2.0.76, 2.0.60
  (single runs). Reason, integration: `claude -p --plugin-dir` exits 0 with
  no flag error and the valid control passes, but no later request contains
  `undefined-fn`. The changelog lists the LSP tool as added in 2.0.74.
- Race: Claude Code delivers plugin LSP diagnostics asynchronously on a later
  model turn. The mock answers instantly, so on a slow or loaded machine all
  four scripted turns finish before the server's diagnostics arrive. In the
  failing 2.1.6 run the log shows 4 requests, none with `undefined-fn`.
  Versions that pass 3/3 may still be racy, only less often.

## Codex CLI (`@openai/codex`)

- Pinned: 0.163.0.
- Latest (0.163.0, 2026-10-18): PASS, the pin is the latest release.
- Oldest tested passing with the default scripts: 0.140.0 (2026-10-18).
  0.120.0 fails: Codex answers `unsupported call: mcp__marigold:marigold_check`,
  so the scripted namespaced MCP call is not understood. This is test
  infrastructure, since the scripts target the newer `exec_command` tool and
  namespaced MCP tools. Versions 0.121.0 to 0.139.0 are untested.
- Oldest tested passing with the legacy scripts: 0.55.0 (2026-10-18).
  Run with `CODEX_TOOL_STYLE=legacy` to use `scripts/codex-legacy-*.json`,
  which call the older `shell` tool (`command` array) and the flat MCP tool
  name `mcp__marigold__marigold_check`. All assertions are unchanged. Also
  passing: 0.56.0, 0.60.1, 0.81.0. 0.81.0 fails with the default scripts
  (`unsupported call: exec_command`, test infrastructure only).
- Failing below the legacy bound:
  - 0.54.0: install. The npm package has no vendored binary
    (`spawn .../vendor/.../codex ENOENT` on `codex --version`).
  - 0.52.0: test infrastructure. The MCP result carries the diagnostics, but
    the assertion for the `# AGENTS.md instructions for` header fails because
    older Codex wraps `AGENTS.md` differently. The text of `AGENTS.md` is
    present in the request.
  - 0.41.0: test infrastructure, same header difference, plus the scripted
    MCP tool name (`unsupported call: mcp__marigold__marigold_check`) is not
    the one that version exposes.
  - 0.53.0 is untested.

## opencode (`opencode-ai`)

- Pinned: 1.19.3.
- Latest (1.19.3, 2026-10-18): PASS, the pin is the latest release.
- Oldest tested passing: 1.1.16 (tested 2026-10-21), full scenario set
  including the `edit` scenarios. Also passing: 1.1.19, 1.1.39, 1.2.15.
- Failing below: 1.1.15, 1.1.14, 1.1.9, 1.0.223 (2026-10-21) and 1.0.142
  (2026-10-18). On all of them the error scenarios and the mixed scenario
  pass, so the integration works: the Error diagnostic reaches the model.
  The failing assertions are characterization pins that need the text "Wrote
  file successfully" in the tool result; these versions return an empty
  result for a successful write. This is a difference in opencode output
  text, not a failure of the integration. On 1.0.142 an Information
  diagnostic is also shown to the model in a `<file_diagnostics>` block, so
  the "information is dropped" pin does not hold there either.
- Tested lower bound for the integration itself (error diagnostic reaches the
  model after `write` and `edit`): 1.0.142 or older, not searched.
