# Marigold LSP config for opencode

This optional snippet registers `marigold-lsp` as a custom language server in
opencode. Marigold itself never needs it.

## Prerequisites

- The `marigold-lsp` binary on your `PATH`. Build it from this repository with
  `cargo install --path marigold-lsp`.

## Install

Merge the `lsp` block from `opencode.json` in this directory into your
project's `opencode.json` (or your global opencode config):

```json
{
  "lsp": {
    "marigold": {
      "command": ["marigold-lsp"],
      "extensions": [".marigold"]
    }
  }
}
```

opencode starts the server the first time it touches a file whose extension
matches.

Then copy the contents of `AGENTS.md` into your project's `AGENTS.md` (or
your global opencode `AGENTS.md`). opencode 1.19.3 loads `AGENTS.md` from the
project directory (verified in an isolated container). The text tells the
agent that opencode reports only errors, so it must check by hand for the
warnings and information it never sees.

## Binary not on the PATH

Use an absolute path as the first element of `command`:

```json
{
  "lsp": {
    "marigold": {
      "command": ["/home/you/.cargo/bin/marigold-lsp"],
      "extensions": [".marigold"]
    }
  }
}
```

## Windows

UNVERIFIED: this integration was not tested on Windows. Build with
`cargo install --path marigold-lsp` and, if the bare command is not found, use
the full path to `marigold-lsp.exe` in `command`, with escaped backslashes or
forward slashes.

## Uninstall

Delete the `lsp.marigold` block from your `opencode.json` and remove the text
you copied from `AGENTS.md`.

## Schema

Each entry under `lsp` accepts these keys, per the opencode docs:

- `command`: array of strings, the command that starts the server.
- `extensions`: array of strings, the file extensions the server handles.
- `env`: object of environment variables for the server process.
- `initialization`: object sent as the LSP initialization options.
- `disabled`: boolean, set to true to turn the server off.

## Verify

Ask the agent to write a `.marigold` file that uses an undeclared enum. On
opencode 1.19.3, with a scripted model writing the file through the `write`
tool, the tool result read `Wrote file successfully.` followed by
`LSP errors detected in this file, please fix:` and a `<diagnostics>` block
with an entry such as `ERROR [1:7] range(Colour): 'Colour' is not a declared
enum...`. `edit` was not tested.

Only error diagnostics reached the model. Information (`undefined-fn`) and
warning (a variable read before it is declared) diagnostics were not shown,
even when mixed with an error in the same file, and the diagnostic code was
never shown, only the message. `opencode debug lsp diagnostics <file>` shows
what the server reports.

## Warnings and information

Because opencode does not show warnings or information to the model, have the
agent run the `marigold check` CLI on the file for those. It needs the CLI
built with the `cli` feature: `cargo install marigold -F cli`. Registering the
`marigold-lsp mcp` server in opencode is UNTESTED here.

## Version notes

- Source: opencode LSP docs at opencode.ai/docs/lsp, read 2026-10-05.
- With opencode 1.19.3 in an isolated container and a scripted model, a
  `write` of a `.marigold` file returned only error-severity diagnostics to the
  model (see Verify). Behaviour with `edit` and with real models: UNVERIFIED.
- The `lsp.marigold` snippet was checked with opencode 1.3.17 under isolated
  XDG directories: the server starts and reported `undefined-fn` with
  severity 2 (warning). That observation was made before the severity change;
  `undefined-fn` is now severity 3 (information) and has not been re-run. An
  agent edit run end to end was not observed.
- Minimum opencode version: UNVERIFIED.
