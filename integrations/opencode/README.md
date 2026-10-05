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

## Schema

Each entry under `lsp` accepts these keys, per the opencode docs:

- `command`: array of strings, the command that starts the server.
- `extensions`: array of strings, the file extensions the server handles.
- `env`: object of environment variables for the server process.
- `initialization`: object sent as the LSP initialization options.
- `disabled`: boolean, set to true to turn the server off.

## Verify

Ask the agent to add `x.map(undefined_fn)` to a `.marigold` file and check that
the resulting diagnostic is reported back to it.

## Version notes

- Source: opencode LSP docs at opencode.ai/docs/lsp, read 2026-10-05.
- How and when diagnostics are surfaced to the agent after an edit: UNVERIFIED.
  The docs say diagnostics feed back to the agent but do not describe the
  mechanism.
- Minimum opencode version: UNVERIFIED.
