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
your global opencode `AGENTS.md`). It tells the agent to act on warnings and
to read information diagnostics as well as errors. That `AGENTS.md` is read by
opencode is UNVERIFIED here.

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

Ask the agent to add `x.map(undefined_fn)` to a `.marigold` file and check that
the resulting diagnostic is reported back to it. That yields an information
diagnostic, `undefined-fn`; a variable read before it is declared (`a = a`)
yields a warning, `undefined-stream-variable`, while an undeclared stream
variable yields the same code as information. Neither is an error, so tell the
agent to fix
warnings and read information diagnostics too. How opencode surfaces
information diagnostics to the model is UNVERIFIED. `opencode debug lsp
diagnostics <file>` shows what the server reports.

## Version notes

- Source: opencode LSP docs at opencode.ai/docs/lsp, read 2026-10-05.
- How and when diagnostics are surfaced to the agent after an edit: UNVERIFIED.
  The docs say diagnostics feed back to the agent but do not describe the
  mechanism.
- The `lsp.marigold` snippet was checked with opencode 1.3.17 under isolated
  XDG directories: the server starts and reported `undefined-fn` with
  severity 2 (warning). That observation was made before the severity change;
  `undefined-fn` is now severity 3 (information) and has not been re-run. An
  agent edit run end to end was not observed.
- Minimum opencode version: UNVERIFIED.
