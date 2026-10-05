# Marigold LSP plugin for Claude Code

This optional plugin connects Claude Code to `marigold-lsp`, so Claude sees
diagnostics after it edits a `.marigold` file and can use go-to-definition and
hover through the built-in `LSP` tool. Marigold itself never needs this plugin.

## Prerequisites

- Claude Code with plugin LSP support (see Version notes).
- The `marigold-lsp` binary on the `PATH` of the shell that starts `claude`.
  Build it from this repository with `cargo install --path marigold-lsp`.

## Install

From a clone of this repository, inside Claude Code:

```text
/plugin marketplace add ./
/plugin install marigold-lsp@marigold
```

Or from a shell:

```bash
claude plugin marketplace add ./
claude plugin install marigold-lsp@marigold
```

To try the plugin for one session without installing it:

```bash
claude --plugin-dir ./integrations/claude
```

The marketplace manifest is `.claude-plugin/marketplace.json` at the
repository root. It lists one plugin, sourced from `./integrations/claude`.

## What it configures

`.lsp.json` maps the `.marigold` extension to the `marigold` language id and
starts `marigold-lsp` over stdio. It deliberately sets none of the optional
tuning fields (`restartOnCrash`, `shutdownTimeout`, and similar), so the
defaults apply.

## Verify

Ask Claude to add `x.map(undefined_fn)` to a `.marigold` file. A line reading
`Found N new diagnostic issues` should appear; press Ctrl+O to expand it. If
nothing appears, open `/plugin` and check the Errors tab for
`Executable not found in $PATH`.

## Version notes

- Sources: the Claude Code plugin reference and plugin marketplace docs at
  code.claude.com, and the Claude Code CHANGELOG, all read 2026-10-05.
- The changelog first mentions the LSP tool in 2.0.74. The documentation does
  not state a minimum version for plugin `lspServers`. Minimum version:
  UNVERIFIED.
- Current documentation lists `restartOnCrash` and `shutdownTimeout` as valid
  optional fields. The claim that older versions skip a whole server when they
  are set is UNVERIFIED; the fields are omitted to stay safe.
