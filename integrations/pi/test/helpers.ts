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

type Handler = (event: any, ctx: any) => any;

export function fakePi() {
  const handlers = new Map<string, Handler[]>();
  const tools = new Map<string, any>();
  return {
    api: {
      on(event: string, handler: Handler) {
        handlers.set(event, [...(handlers.get(event) ?? []), handler]);
        return () => {};
      },
      registerTool(tool: any) {
        tools.set(tool.name, tool);
      },
    },
    async emit(event: string, payload: any, ctx: any) {
      let result: any;
      for (const h of handlers.get(event) ?? []) result = (await h(payload, ctx)) ?? result;
      return result;
    },
    tools,
  };
}
