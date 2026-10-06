import { chmod, readFile, realpath, rename, stat, unlink, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { sep, basename, dirname, isAbsolute, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { Type } from "typebox";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import {
  applyEdits,
  editsOf,
  formatDiagnostics,
  formatLocations,
  formatRename,
  formatSymbols,
  hoverText,
  locationsOf,
  symbolsOf,
  type LspDiagnostic,
  type LspWorkspaceEdit,
} from "../src/format.ts";
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

async function readBounded(absolute: string, action: string): Promise<string> {
  const { size } = await stat(absolute);
  if (size > MAX_FILE_BYTES) {
    throw new Error(`file is too large to ${action} (${size} bytes, limit ${MAX_FILE_BYTES})`);
  }
  return readFile(absolute, "utf8");
}

const NOT_CHECKED_SUFFIX = "; this file was NOT checked, do not assume it is valid";

function position(params: { line: number; column: number }) {
  for (const key of ["line", "column"] as const) {
    if (!Number.isInteger(params[key]) || params[key] < 1) {
      throw new Error(`${key} must be an integer >= 1 (1-based), got ${String(params[key])}`);
    }
  }
  return { line: params.line - 1, character: params.column - 1 };
}

function isInside(root: string, target: string): boolean {
  const rel = relative(root, target);
  return rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel);
}

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

    const readSource = async (path: string, cwd: string, action: string) => {
      const absolute = normalizePath(cwd, path);
      if (!absolute.endsWith(".marigold")) {
        throw new Error(`refusing to ${action} ${path}: only .marigold files are supported`);
      }
      try {
        return { absolute, text: await readBounded(absolute, action) };
      } catch (e) {
        throw new Error(`cannot read ${path}: ${(e as Error).message}`);
      }
    };

    const check = async (cwd: string, path: string, limit = 20) => {
      const { absolute, text } = await readSource(path, cwd, "check");
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
        "Check a .marigold file with the marigold language server and return its diagnostics (syntax errors, undefined enums, invalid bounded types) with 1-based line:column locations. Errors and warnings (a stream variable read before it is declared) should be fixed; undefined stream variables, functions and structs are information and can be ignored if they are Rust items in scope inside m!() (a stream variable may be a Rust binding with a get() method).",
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

    const show = (cwd: string) => (uri: string) => {
      let absolute: string;
      try {
        absolute = fileURLToPath(uri);
      } catch {
        return uri;
      }
      const rel = relative(cwd, absolute);
      return rel === "" || !isInside(cwd, absolute) ? absolute : rel;
    };

    const navigate = async (
      cwd: string,
      uri: string | undefined,
      text: string | undefined,
      method: string,
      params: unknown,
    ): Promise<unknown> => {
      try {
        const lsp = await getClient(cwd);
        return await lsp.query(uri, text, method, params);
      } catch (e) {
        const message = (e as Error).message;
        throw new Error(e instanceof NotChecked ? message.replace(NOT_CHECKED_SUFFIX, "") : message);
      }
    };

    const withPosition = async (cwd: string, p: { path: string; line: number; column: number }, action: string) => {
      const pos = position(p);
      const { absolute, text } = await readSource(p.path, cwd, action);
      const uri = pathToFileURL(absolute).href;
      const where = `${show(cwd)(uri)}:${p.line}:${p.column}`;
      return { absolute, text, uri, pos, where, sources: new Map([[uri, text]]) };
    };

    const positionSchema = {
      path: Type.String({ description: "Path to the .marigold file (relative to the working directory or absolute)" }),
      line: Type.Integer({ minimum: 1, description: "1-based line number" }),
      column: Type.Integer({ minimum: 1, description: "1-based column, counted in UTF-16 code units like marigold_check output" }),
    };

    const columns = "line and column are 1-based; columns count UTF-16 code units, the same as in marigold_check diagnostics.";

    pi.registerTool({
      name: "marigold_definition",
      label: "Marigold definition",
      description: `Find where the stream variable, function, struct or enum under the cursor in a .marigold file is declared. ${columns}`,
      parameters: Type.Object(positionSchema),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const r = await withPosition(ctx.cwd, params, "query");
        const result = await navigate(ctx.cwd, r.uri, r.text, "textDocument/definition", {
          textDocument: { uri: r.uri },
          position: r.pos,
        });
        const locations = locationsOf(result);
        const text =
          locations.length === 0
            ? `marigold: no definition found at ${r.where}`
            : formatLocations("definition", locations, show(ctx.cwd), r.sources);
        return { content: [{ type: "text", text }], details: { locations } };
      },
    });

    pi.registerTool({
      name: "marigold_references",
      label: "Marigold references",
      description: `Find every use of the symbol under the cursor in a .marigold file, optionally including its declaration. ${columns}`,
      parameters: Type.Object({
        ...positionSchema,
        include_declaration: Type.Optional(Type.Boolean({ description: "Include the declaration itself (default true)" })),
      }),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const r = await withPosition(ctx.cwd, params, "query");
        const result = await navigate(ctx.cwd, r.uri, r.text, "textDocument/references", {
          textDocument: { uri: r.uri },
          position: r.pos,
          context: { includeDeclaration: params.include_declaration ?? true },
        });
        const locations = locationsOf(result);
        const text =
          locations.length === 0
            ? `marigold: no references found at ${r.where}`
            : formatLocations("reference", locations, show(ctx.cwd), r.sources);
        return { content: [{ type: "text", text }], details: { locations } };
      },
    });

    pi.registerTool({
      name: "marigold_hover",
      label: "Marigold hover",
      description: `Show the signature of the symbol or stream under the cursor in a .marigold file, with its complexity (cardinality, time, space, whether it collects input). ${columns}`,
      parameters: Type.Object(positionSchema),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const r = await withPosition(ctx.cwd, params, "query");
        const result = await navigate(ctx.cwd, r.uri, r.text, "textDocument/hover", {
          textDocument: { uri: r.uri },
          position: r.pos,
        });
        const markdown = hoverText(result);
        const text = markdown === "" ? `marigold: no hover information at ${r.where}` : markdown;
        return { content: [{ type: "text", text }], details: { markdown } };
      },
    });

    pi.registerTool({
      name: "marigold_symbols",
      label: "Marigold symbols",
      description:
        "List the declarations (enums, structs, functions, stream variables) of a .marigold file with 1-based line:column locations (columns count UTF-16 code units).",
      parameters: Type.Object({ path: positionSchema.path }),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const { absolute, text: source } = await readSource(params.path, ctx.cwd, "query");
        const uri = pathToFileURL(absolute).href;
        const result = await navigate(ctx.cwd, uri, source, "textDocument/documentSymbol", { textDocument: { uri } });
        const symbols = symbolsOf(result, uri);
        const name = show(ctx.cwd)(uri);
        const text =
          symbols.length === 0
            ? `marigold: no symbols in ${name}`
            : formatSymbols(`${symbols.length} symbol${symbols.length === 1 ? "" : "s"} in ${name}`, symbols, () => name);
        return { content: [{ type: "text", text }], details: { symbols } };
      },
    });

    pi.registerTool({
      name: "marigold_workspace_symbols",
      label: "Marigold workspace symbols",
      description:
        "Search declarations by name across all .marigold files under the working directory. Returns path:line:column (1-based, UTF-16 columns), kind and name.",
      parameters: Type.Object({
        query: Type.String({ description: "Name or part of a name to search for" }),
      }),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const result = await navigate(ctx.cwd, undefined, undefined, "workspace/symbol", { query: params.query });
        const symbols = symbolsOf(result);
        const label = `symbol${symbols.length === 1 ? "" : "s"} matching ${JSON.stringify(params.query)}`;
        const text =
          symbols.length === 0
            ? `marigold: no symbols matching ${JSON.stringify(params.query)}`
            : formatSymbols(`${symbols.length} ${label}`, symbols, show(ctx.cwd));
        return { content: [{ type: "text", text }], details: { symbols } };
      },
    });

    pi.registerTool({
      name: "marigold_rename",
      label: "Marigold rename",
      description: `Rename the stream variable, function, struct or enum under the cursor everywhere in a .marigold file. By default only previews the edits and changes nothing; call again with apply=true to write them (only .marigold files inside the working directory are modified, and only if the file is unchanged since it was read). ${columns}`,
      parameters: Type.Object({
        ...positionSchema,
        new_name: Type.String({ description: "New identifier: ASCII letters, digits and underscores, not starting with a digit" }),
        apply: Type.Optional(Type.Boolean({ description: "Write the edits to disk (default false: preview only)" })),
      }),
      async execute(_toolCallId, params, _signal, _onUpdate, ctx) {
        const r = await withPosition(ctx.cwd, params, "rename in");
        const apply = params.apply === true;
        let real = r.absolute;
        if (apply) {
          real = await realpath(r.absolute);
          if (!isInside(await realpath(ctx.cwd), real)) {
            throw new Error(`refusing to modify ${params.path}: it is outside the working directory; preview without apply instead`);
          }
        }
        const result = (await navigate(ctx.cwd, r.uri, r.text, "textDocument/rename", {
          textDocument: { uri: r.uri },
          position: r.pos,
          newName: params.new_name,
        })) as LspWorkspaceEdit | null;
        if (result === null || result === undefined) {
          return {
            content: [{ type: "text", text: `marigold: no renamable symbol at ${r.where}` }],
            details: { applied: false },
          };
        }
        const edits = editsOf(result);
        if (edits.size === 0) {
          return {
            content: [{ type: "text", text: `marigold: renaming to ${JSON.stringify(params.new_name)} would make no changes` }],
            details: { applied: false },
          };
        }
        if (!apply) {
          return {
            content: [{ type: "text", text: formatRename(edits, show(ctx.cwd), r.sources, false) }],
            details: { applied: false },
          };
        }
        for (const uri of edits.keys()) {
          let target: string;
          try {
            target = await realpath(fileURLToPath(uri));
          } catch {
            throw new Error(`refusing to apply: the server proposed edits to an unreadable file ${uri}`);
          }
          if (target !== real) {
            throw new Error(`refusing to apply: the server proposed edits to ${uri}, which is not ${params.path}`);
          }
        }
        const updated = applyEdits(r.text, edits.get(r.uri) ?? [...edits.values()][0]);
        let current: string;
        try {
          current = await readBounded(real, "rename in");
        } catch (e) {
          throw new Error(`cannot re-read ${params.path} before writing: ${(e as Error).message}`);
        }
        if (current !== r.text) {
          throw new Error(`${params.path} changed since it was read; nothing was written, retry the rename`);
        }
        const { mode } = await stat(real);
        const temp = join(dirname(real), `.${basename(real)}.${process.pid}.${Date.now().toString(36)}.tmp`);
        try {
          await writeFile(temp, updated, { flag: "wx" });
          await chmod(temp, mode & 0o7777);
          await rename(temp, real);
        } catch (e) {
          await unlink(temp).catch(() => {});
          throw new Error(`cannot write ${params.path}: ${(e as Error).message}`);
        }
        const lines = [formatRename(edits, show(ctx.cwd), r.sources, true)];
        try {
          const lsp = await getClient(ctx.cwd);
          const diagnostics = await lsp.check(r.uri, updated);
          lines.push(formatDiagnostics(show(ctx.cwd)(r.uri), diagnostics, 20, updated));
        } catch (e) {
          lines.push(`marigold: could not re-check ${params.path}: ${(e as Error).message}`);
        }
        return { content: [{ type: "text", text: lines.join("\n") }], details: { applied: true } };
      },
    });
  };
}

export default createMarigoldExtension();
