import { existsSync, mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { fileURLToPath } from "node:url";
import { join, resolve } from "node:path";

export const lspBinary =
  process.env.MARIGOLD_LSP_BIN ??
  resolve(fileURLToPath(new URL("../../../target/debug/marigold-lsp", import.meta.url)));

if (!existsSync(lspBinary)) {
  throw new Error(
    `marigold-lsp binary not found at ${lspBinary}; run \`cargo build -p marigold-lsp\` or set MARIGOLD_LSP_BIN`,
  );
}

export const fakeServer = fileURLToPath(new URL("./fixtures/fake-server.mjs", import.meta.url));

export function tempDir(prefix = "pi-marigold-"): string {
  return mkdtempSync(join(tmpdir(), prefix));
}
