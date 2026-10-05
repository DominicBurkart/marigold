import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";

export const lspBinary =
  process.env.MARIGOLD_LSP_BIN ??
  resolve(fileURLToPath(new URL("../../../target/debug/marigold-lsp", import.meta.url)));

if (!existsSync(lspBinary)) {
  throw new Error(
    `marigold-lsp binary not found at ${lspBinary}; run \`cargo build -p marigold-lsp\` or set MARIGOLD_LSP_BIN`,
  );
}
