# Marigold LSP plugin for Claude Code

This optional plugin connects Claude Code to `marigold-lsp`, so Claude is
shown diagnostics after it edits a `.marigold` file and can use
go-to-definition and hover through the built-in `LSP` tool. Diagnostics arrive
on the model's next turn, not in the edit result (see Verify). Marigold itself
never needs this plugin.

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

Ask Claude to add `x.map(undefined_fn)` to a `.marigold` file and then to keep
working, for example by reading the file back. That yields an information
diagnostic, `undefined-fn`. To see a warning, use a stream variable that is
never declared, which yields `undefined-stream-variable`. Claude Code is
documented to
show a line reading `Found N new diagnostic issues`, with Ctrl+O to expand it;
both are UNVERIFIED here. To check the plugin without relying on that line,
run `claude plugin details marigold-lsp`, open `/plugin` and read the Errors
tab (an `Executable not found in $PATH` message means `marigold-lsp` is not on
the `PATH`), and run `claude plugin validate ./integrations/claude`.

Diagnostics arrive on the model's next turn, not in the edit result (observed
on Claude Code 2.1.289). Whether a model that ends its turn immediately after
the edit acts on them is UNVERIFIED. As a backstop, have the agent run
`marigold check` before it finishes.

Diagnostics have three severities. Errors mean the program is invalid. The
only warning is `undefined-stream-variable`, which is almost always a real
mistake. `undefined-fn` and `undefined-struct` are information, because
Marigold fns and structs normally live in Rust code surrounding the program
inside `m!()`; the message says the name may be a Rust item in scope and can be
ignored in that case. `resolver-diagnostics-truncated` is also information and
appears when more than 50 resolver diagnostics were suppressed. `marigold check`
exits 0 for warnings and information and 1 on any error, and `marigold_check`
returns `ok: true` with `error_count`, `warning_count` and `info_count`. The
Claude Code LSP client shows information diagnostics at information level; how
Claude Code surfaces them to the model is UNVERIFIED. The plugin ships a
`marigold` skill that tells Claude this.

## Checking from the command line and MCP

`marigold check` needs the CLI built with the `cli` feature:
`cargo install marigold -F cli` (or `cargo install --path marigold -F cli`
from a clone).

The `marigold_check` tool is exposed only by `marigold-lsp mcp`. This plugin
registers the language server, not the MCP server. To add the MCP server to
Claude Code as well, run:

```bash
claude mcp add marigold -- marigold-lsp mcp
```

Add `--scope project` or `--scope user` before the server name to change where
the registration is stored. The syntax was checked against the Claude Code MCP
docs.

## Binary not on the PATH

If `marigold-lsp` is not on the `PATH`, put its absolute path in `command` in
`integrations/claude/.lsp.json`. The docs say `command` may contain spaces
only when it starts with `/`.

```json
{
  "marigold": {
    "command": "/home/you/.cargo/bin/marigold-lsp",
    "extensionToLanguage": {
      ".marigold": "marigold"
    }
  }
}
```

For `claude mcp add`, use the same absolute path after `--`.

## Windows

UNVERIFIED: this integration was not tested on Windows. Build with
`cargo install --path marigold-lsp`, which should place `marigold-lsp.exe` in
`%USERPROFILE%\.cargo\bin`. If Claude Code does not resolve the bare
command, put the full path to the `.exe` in `command`, with forward slashes or
escaped backslashes in the JSON.

## Uninstall

```bash
claude plugin uninstall marigold-lsp@marigold
claude plugin marketplace remove marigold
```

If you registered the MCP server, also run `claude mcp remove marigold`
(UNVERIFIED: the subcommand was not checked against the docs).

## Version notes

- Sources: the Claude Code plugin reference and plugin marketplace docs at
  code.claude.com, and the Claude Code CHANGELOG, all read 2026-10-05.
- Plugin LSP servers predate 2.1.31 (the first changelog mention); the exact
  introduction version is not documented. `requestTimeout` needs 2.1.288 or
  later. This plugin sets neither. Minimum version: UNVERIFIED.
- Diagnostics were observed arriving on the model's next turn with Claude Code
  2.1.289, loaded with `--plugin-dir`.
- Current documentation lists `restartOnCrash` and `shutdownTimeout` as valid
  optional fields. The claim that older versions skip a whole server when they
  are set is UNVERIFIED; the fields are omitted to stay safe.
- The plugin version in `.claude-plugin/plugin.json` is not tied to the crate
  version. Because `version` is set, installed users stay pinned to it until it
  changes (per the plugin manifest reference). Bump it by hand in every
  change that alters the plugin, then users run `claude plugin update`
  (UNVERIFIED: the update command was not checked).
