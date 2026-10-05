import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createMarigoldExtension } from "../extensions/marigold.ts";
import { lspBinary } from "./helpers.ts";

type Handler = (event: any, ctx: any) => any;

function fakePi() {
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

let dir: string;
let pi: ReturnType<typeof fakePi>;
let ctx: { cwd: string };

beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "pi-marigold-"));
  ctx = { cwd: dir };
  pi = fakePi();
  createMarigoldExtension({ command: lspBinary })(pi.api as any);
});

afterEach(async () => {
  await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
});

const writeResult = (path: string, isError = false) => ({
  type: "tool_result",
  toolName: "write",
  toolCallId: "1",
  input: { path, content: "" },
  content: [{ type: "text", text: "Wrote file" }],
  isError,
  details: undefined,
});

describe("tool_result hook", () => {
  it("appends diagnostics after writing a broken .marigold file", async () => {
    writeFileSync(join(dir, "a.marigold"), "range(Colour).return\n");
    const result = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    const texts = result.content.map((c: any) => c.text);
    expect(texts[0]).toBe("Wrote file");
    expect(texts[1]).toContain("a.marigold:1:7: error[undefined-enum]");
  });

  it("confirms a clean file after edit", async () => {
    writeFileSync(join(dir, "ok.marigold"), "range(0, 1).return\n");
    const event = { ...writeResult(join(dir, "ok.marigold")), toolName: "edit" };
    const result = await pi.emit("tool_result", event, ctx);
    expect(result.content[1].text).toContain("no diagnostics");
  });

  it("ignores other files, other tools, and failed writes", async () => {
    writeFileSync(join(dir, "a.rs"), "fn main() {}");
    writeFileSync(join(dir, "a.marigold"), "range(Colour).return\n");
    expect(await pi.emit("tool_result", writeResult("a.rs"), ctx)).toBeUndefined();
    expect(await pi.emit("tool_result", { ...writeResult("a.marigold"), toolName: "read" }, ctx)).toBeUndefined();
    expect(await pi.emit("tool_result", writeResult("a.marigold", true), ctx)).toBeUndefined();
  });
});

describe("marigold_check tool", () => {
  it("is registered with a path parameter", () => {
    const tool = pi.tools.get("marigold_check");
    expect(tool).toBeDefined();
    expect(tool.parameters.properties.path).toBeDefined();
  });

  it("returns formatted diagnostics for a file", async () => {
    writeFileSync(join(dir, "b.marigold"), "range(0, 1).retur");
    const tool = pi.tools.get("marigold_check");
    const result = await tool.execute("id", { path: "b.marigold" }, undefined, undefined, ctx);
    expect(result.content[0].text).toContain("error[syntax-error]");
    expect(result.details.diagnostics).toHaveLength(1);
  });

  it("fails with a clear error for a missing file", async () => {
    const tool = pi.tools.get("marigold_check");
    await expect(tool.execute("id", { path: "missing.marigold" }, undefined, undefined, ctx)).rejects.toThrow(
      /missing\.marigold/,
    );
  });
});
