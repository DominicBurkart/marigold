# pi-marigold

Marigold language support for the [pi](https://pi.dev) coding agent.

- After every `edit` or `write` to a `.marigold` file, the file is checked
  by `marigold-lsp` and the diagnostics are appended to the tool result
  the model sees.
- A `marigold_check` tool re-checks any `.marigold` file on demand.
- A `marigold` skill explains the diagnostic codes and the feedback loop.

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
writes a broken file, and asserts that the diagnostics reach the model.
