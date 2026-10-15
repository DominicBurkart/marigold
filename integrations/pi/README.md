# pi-marigold

Marigold language support for the [pi](https://pi.dev) coding agent.

- After every `edit` or `write` to a `.marigold` file, the file is checked
  by `marigold-lsp` and the diagnostics are appended to the tool result
  the model sees.
- A `marigold_check` tool re-checks any `.marigold` file on demand.
- Navigation tools query the language server: `marigold_definition`,
  `marigold_references`, `marigold_hover` (signature and complexity),
  `marigold_symbols` (file outline), `marigold_workspace_symbols` and
  `marigold_rename`.
- A `marigold` skill explains the diagnostic codes, the feedback loop and
  when to use each tool.

## Diagnostic severities

- `error`: fix it.
- `warning`: review and fix it; the only warning is
  `undefined-stream-variable` for a variable read before the line that
  declares it, such as `a = a`.
- `information`: `undefined-stream-variable` for a name that is not
  declared, `undefined-fn` and `undefined-struct`; fix them unless the name
  is a Rust item in scope inside `m!()` (a stream variable may be a Rust
  binding with a `get()` method). `resolver-diagnostics-truncated`
  reports that more than 50 undefined-name diagnostics were found for a
  file and only the first 50 are reported.

## Navigation tools

Tools that take a position use 1-based `line` and `column`; columns count
UTF-16 code units, like the locations in diagnostics. All tools accept only
`.marigold` files of at most 10 MiB and report explicitly when nothing was
found.

`marigold_rename` previews by default: it prints the proposed edits per
file and does not touch the disk. With `apply: true` it writes the edits,
but only for `.marigold` files inside the working directory (symlinks that
leave it are refused), only if the file is unchanged since it was read,
and atomically through a temporary file and a rename. The server rejects
invalid identifiers and names that clash with an existing declaration, and
the message is returned to the model.

## Install

```bash
cargo install marigold-lsp
pi install npm:pi-marigold
```

From a checkout of this repository, `pi install ./integrations/pi`
installs the local copy instead. The package has no runtime dependencies
beyond what pi provides.

Set `MARIGOLD_LSP_COMMAND` if `marigold-lsp` is not on your `PATH`.

## Development

```bash
cargo build -p marigold-lsp
cd integrations/pi
npm ci
npm run typecheck
npm test
```

The tests run against the real `marigold-lsp` binary
(`target/debug/marigold-lsp`, or `MARIGOLD_LSP_BIN`).
`test/pi-e2e.test.ts` starts a real pi session with a scripted model that
writes a broken file, and asserts that the diagnostics reach the model; a
second scenario has the model call `marigold_definition`.
