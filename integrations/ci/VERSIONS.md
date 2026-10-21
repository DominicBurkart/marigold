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

- Pinned: 2.1.289.
- Oldest tested passing: 2.1.6 (tested 2026-10-18).
- Latest (2.1.302, 2026-10-18): PASS, so the pin is 13 patch releases behind.
- Other passing versions: 2.1.16, 2.1.53, 2.1.136.
- Failing versions just below: 2.1.5, 2.1.4, 2.1.2, 2.0.76, 2.0.60.
  Reason, integration: `claude -p --plugin-dir` runs and exits 0 without any
  flag error and the valid control passes, but no later request contains the
  `undefined-fn` diagnostic. The cause inside Claude Code is not established.
  The changelog lists the LSP tool as added in 2.0.74, so diagnostics
  delivery from plugin language servers evidently arrived later on this
  scenario. Untested: 2.1.3 and 2.0.x other than the two listed.

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
- Oldest tested passing: 1.1.39 (tested 2026-10-21), full scenario set
  including the `edit` scenarios. Also passing: 1.2.15.
- Failing below: 1.0.223 and 1.0.142.
  - 1.0.142 (2026-10-18): the error scenarios pass, so the integration works.
    The characterization scenarios fail: a successful write returns an empty
    result instead of "Wrote file successfully.", and an Information
    diagnostic is shown to the model in a `<file_diagnostics>` block. The
    pinned behaviour is therefore version specific, not an integration
    failure.
  - 1.0.223 (2026-10-21): error scenarios pass and the mixed scenario passes;
    the assertions that need the text "Wrote file successfully" fail.
- Versions from 1.0.224 to 1.1.38 are untested.
