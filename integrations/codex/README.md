# Marigold config for OpenAI Codex CLI

Codex has no LSP client as of v0.125.0 (a third-party report; re-check before
relying on it), so this optional integration exposes Marigold's checks as an
MCP tool instead. Marigold itself never needs it.

## Tools

The config below runs `marigold-lsp mcp`, a read-only MCP server over stdio
that exposes these tools. Lines and columns are 1-based and counted in
characters. Only `.marigold` files up to 10 MiB can be read.

- `marigold_check`: diagnostics for a file or a source string.
- `marigold_symbols`: declarations and references in a file.
- `marigold_definition`: the declaration for the symbol at a position.
- `marigold_references`: every reference to the symbol at a position.
- `marigold_complexity`: cardinality, time and space for each stream.

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

Per the Codex docs (not tested here), the global file is
`~/.codex/AGENTS.override.md` if present, otherwise `~/.codex/AGENTS.md`. Each
directory from the Git root down to the working directory is then checked for
`AGENTS.override.md`, then `AGENTS.md`, then any
`project_doc_fallback_filenames`. The files are concatenated, closer ones
winning, and the combined size is capped at 32 KiB by default
(`project_doc_max_bytes`).

## Verify

Ask the agent to add `x.map(undefined_fn)` to a `.marigold` file and check
that it calls `marigold_check` and reports the diagnostic.

## Version notes

- Sources: Codex MCP and AGENTS.md docs, read 2026-10-05 via
  learn.chatgpt.com (redirected from developers.openai.com/codex).
- Minimum Codex CLI version: UNVERIFIED.
- Project-scoped `.codex/config.toml` is honored only for trusted projects,
  per learn.chatgpt.com/docs/extend/mcp.
- The `AGENTS.md` lookup order above comes from a search snippet of the Codex
  AGENTS.md guide and was not read in full or tested.
- The server implements MCP revision 2025-11-25 and answers older
  handshake-based revisions. It has been tested against its own test suite,
  not against a running Codex CLI: UNVERIFIED with Codex itself.
