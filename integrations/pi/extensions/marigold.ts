import { readFile } from "node:fs/promises";
import { isAbsolute, relative, resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { Type } from "typebox";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { formatDiagnostics, type LspDiagnostic } from "../src/format.ts";
import { LspClient } from "../src/lsp-client.ts";

export interface MarigoldExtensionOptions {
  command?: string;
  args?: string[];
  timeoutMs?: number;
}

const EDIT_TOOLS = new Set(["edit", "write"]);

export function createMarigoldExtension(options: MarigoldExtensionOptions = {}) {
  const command = options.command ?? process.env.MARIGOLD_LSP_COMMAND ?? "marigold-lsp";
  const args = options.args ?? [];

  return function marigold(pi: ExtensionAPI) {
    let client: Promise<LspClient> | undefined;

    const getClient = (cwd: string) => {
      client ??= LspClient.start(command, args, pathToFileURL(cwd).href, options.timeoutMs).catch((e) => {
        client = undefined;
        throw e;
      });
      return client;
    };

    const check = async (cwd: string, path: string) => {
      const absolute = isAbsolute(path) ? path : resolve(cwd, path);
      let text: string;
      try {
        text = await readFile(absolute, "utf8");
      } catch (e) {
        throw new Error(`cannot read ${path}: ${(e as Error).message}`);
      }
      const lsp = await getClient(cwd);
      const diagnostics = await lsp.check(pathToFileURL(absolute).href, text);
      const shown = relative(cwd, absolute) || absolute;
      return { diagnostics, text: formatDiagnostics(shown.startsWith("..") ? absolute : shown, diagnostics) };
    };

    pi.on("tool_result", async (event, ctx) => {
      if (!EDIT_TOOLS.has(event.toolName) || event.isError) return;
      const path = (event.input as { path?: unknown }).path;
      if (typeof path !== "string" || !path.endsWith(".marigold")) return;
      let text: string;
      try {
        text = (await check(ctx.cwd, path)).text;
      } catch (e) {
        text = `marigold: could not check ${path}: ${(e as Error).message}`;
      }
      return { content: [...event.content, { type: "text" as const, text }] };
    });

    pi.on("session_shutdown", async () => {
      const current = client;
      client = undefined;
      await current?.then((c) => c.shutdown()).catch(() => {});
    });

    pi.registerTool({
      name: "marigold_check",
      label: "Marigold check",
      description:
        "Check a .marigold file with the marigold language server and return its diagnostics (syntax errors, undefined enums, invalid bounded types) with 1-based line:column locations.",
      parameters: Type.Object({
        path: Type.String({ description: "Path to the .marigold file (relative to the working directory or absolute)" }),
      }),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const { diagnostics, text } = await check(ctx.cwd, params.path);
        return {
          content: [{ type: "text", text: formatDiagnostics(params.path, diagnostics, Infinity) }],
          details: { diagnostics: diagnostics as LspDiagnostic[], summary: text.split("\n")[0] },
        };
      },
    });
  };
}

export default createMarigoldExtension();
