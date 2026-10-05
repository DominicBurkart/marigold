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
working, for example by reading the file back. A line reading
`Found N new diagnostic issues` should appear; press Ctrl+O to expand it. If
nothing appears, open `/plugin` and check the Errors tab for
`Executable not found in $PATH`.

Diagnostics are delivered asynchronously, on the model's next turn after the
edit. In testing, a model that ended its turn right after the edit never saw
them. As a backstop, have the agent run `marigold check` (or the
`marigold_check` tool of `marigold-lsp mcp`) on the file before it finishes.

An undefined function, stream variable or struct is reported as a warning,
not an error, because the name may be defined in Rust code that surrounds the
program. `marigold check` still exits 0 and `marigold_check` returns `ok: true`
with a nonzero `warning_count`, so read the warnings as well as the errors.

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
