import { readFile, stat } from "node:fs/promises";
import { homedir } from "node:os";
import { isAbsolute, join, relative, resolve } from "node:path";
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
const MAX_RESTARTS = 3;
const MAX_FILE_BYTES = 10 * 1024 * 1024;

export function normalizePath(cwd: string, path: string): string {
  let p = path.startsWith("@") ? path.slice(1) : path;
  if (p === "~") p = homedir();
  else if (p.startsWith("~/")) p = join(homedir(), p.slice(2));
  return isAbsolute(p) ? p : resolve(cwd, p);
}

class NotChecked extends Error {}

export function createMarigoldExtension(options: MarigoldExtensionOptions = {}) {
  const command = options.command ?? process.env.MARIGOLD_LSP_COMMAND ?? "marigold-lsp";
  const args = options.args ?? [];

  return function marigold(pi: ExtensionAPI) {
    let client: Promise<LspClient> | undefined;
    let spawns = 0;

    const getClient = async (cwd: string): Promise<LspClient> => {
      for (;;) {
        const current = client;
        if (current) {
          try {
            const c = await current;
            if (!c.stopped) return c;
          } catch {}
          if (client === current) client = undefined;
          continue;
        }
        if (spawns > MAX_RESTARTS) {
          throw new NotChecked(
            `marigold-lsp failed ${spawns} times this session, giving up; this file was NOT checked, do not assume it is valid`,
          );
        }
        spawns++;
        const started = LspClient.start(command, args, pathToFileURL(cwd).href, options.timeoutMs).catch((e) => {
          throw new NotChecked(`${(e as Error).message}; this file was NOT checked, do not assume it is valid`);
        });
        client = started;
        return started.catch((e) => {
          if (client === started) client = undefined;
          throw e;
        });
      }
    };

    const check = async (cwd: string, path: string, limit = 20) => {
      const absolute = normalizePath(cwd, path);
      if (!absolute.endsWith(".marigold")) {
        throw new Error(`refusing to check ${path}: only .marigold files can be checked`);
      }
      let text: string;
      try {
        const { size } = await stat(absolute);
        if (size > MAX_FILE_BYTES) {
          throw new Error(`file is too large to check (${size} bytes, limit ${MAX_FILE_BYTES})`);
        }
        text = await readFile(absolute, "utf8");
      } catch (e) {
        throw new Error(`cannot read ${path}: ${(e as Error).message}`);
      }
      const lsp = await getClient(cwd);
      let diagnostics: LspDiagnostic[];
      try {
        diagnostics = await lsp.check(pathToFileURL(absolute).href, text);
      } catch (e) {
        throw new NotChecked(`${(e as Error).message}; this file was NOT checked, do not assume it is valid`);
      }
      const relativePath = relative(cwd, absolute) || absolute;
      const shown = relativePath.startsWith("..") ? absolute : relativePath;
      return { diagnostics, text: formatDiagnostics(shown, diagnostics, limit, text) };
    };

    pi.on("tool_result", async (event, ctx) => {
      if (!EDIT_TOOLS.has(event.toolName) || event.isError) return;
      const path = (event.input as { path?: unknown }).path;
      if (typeof path !== "string") return;
      if (!normalizePath(ctx.cwd, path).endsWith(".marigold")) return;
      let text: string;
      let diagnostics: LspDiagnostic[] | undefined;
      try {
        ({ text, diagnostics } = await check(ctx.cwd, path));
      } catch (e) {
        const message = (e as Error).message;
        text = `marigold: could not check ${path}: ${message}`;
        if (!(e instanceof NotChecked)) text += "; this file was NOT checked, do not assume it is valid";
      }
      const base =
        event.details !== null && typeof event.details === "object" ? (event.details as Record<string, unknown>) : {};
      return {
        content: [...event.content, { type: "text" as const, text }],
        ...(diagnostics ? { details: { ...base, diagnostics } } : {}),
      };
    });

    pi.on("session_shutdown", async () => {
      const current = client;
      client = undefined;
      spawns = 0;
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
        const { diagnostics, text } = await check(ctx.cwd, params.path, Infinity);
        return {
          content: [{ type: "text", text: text }],
          details: { diagnostics, summary: text.split("\n")[0] },
        };
      },
    });
  };
}

export default createMarigoldExtension();
