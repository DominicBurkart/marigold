# Marigold config for OpenAI Codex CLI

Codex has no LSP client, so this optional integration exposes Marigold's
checks as an MCP tool instead. Marigold itself never needs it.

## Status

The config below runs `marigold-lsp mcp`. The `mcp` subcommand and the
`marigold_check` tool land in a later pull request; until then the server will
not start. Do not install this integration before that release.

## Prerequisites

- The `marigold-lsp` binary on your `PATH`, built from a version that includes
  the `mcp` subcommand.

## Install

Append `config.toml` from this directory to `~/.codex/config.toml`:

```toml
[mcp_servers.marigold]
command = "marigold-lsp"
args = ["mcp"]
```

Then copy the contents of `AGENTS.md` into your project's `AGENTS.md` (or into
`~/.codex/AGENTS.md` to apply it everywhere). It tells the agent to call
`marigold_check` after editing `.marigold` files.

## Schema

Per the Codex docs, `[mcp_servers.<name>]` requires `command` and accepts
`args`, `env`, `env_vars`, `cwd`, `startup_timeout_sec`, `tool_timeout_sec`,
`enabled`, `required`, `enabled_tools`, and `disabled_tools`. Codex reads
`~/.codex/config.toml`, and the docs also mention a project-scoped
`.codex/config.toml`.

## AGENTS.md convention

Codex reads `~/.codex/AGENTS.md` (or `AGENTS.override.md`) first, then each
`AGENTS.md` from the Git root down to the working directory, concatenated so
that closer files take precedence. The combined size is capped at 32 KiB by
default (`project_doc_max_bytes`).

## Verify

Ask the agent to add `x.map(undefined_fn)` to a `.marigold` file and check
that it calls `marigold_check` and reports the diagnostic.

## Version notes

- Sources: Codex MCP and AGENTS.md docs, read 2026-10-05 via
  learn.chatgpt.com (redirected from developers.openai.com/codex).
- Minimum Codex CLI version: UNVERIFIED.
- Whether project-scoped `.codex/config.toml` requires a trusted project:
  UNVERIFIED.
- The `marigold_check` tool name and `mcp` subcommand are planned, not yet
  shipped, and cannot be verified against a running server.
