import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { createMarigoldExtension } from "../extensions/marigold.ts";
import { fakePi, fakeServer, lspBinary, tempDir } from "./helpers.ts";

let dir: string;
let pi: ReturnType<typeof fakePi>;
let ctx: { cwd: string };

beforeEach(() => {
  dir = tempDir();
  ctx = { cwd: dir };
  pi = fakePi();
  createMarigoldExtension({ command: lspBinary })(pi.api as any);
});

afterEach(async () => {
  await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
  rmSync(dir, { recursive: true, force: true });
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
    expect(texts[1]).toContain("1 | range(Colour).return");
    expect(result.details.diagnostics).toHaveLength(1);
  });

  it("strips a leading @ from the path", async () => {
    writeFileSync(join(dir, "a.marigold"), "range(Colour).return\n");
    const result = await pi.emit("tool_result", writeResult("@a.marigold"), ctx);
    expect(result.content[1].text).toContain("error[undefined-enum]");
  });

  it("expands ~ to the home directory", async () => {
    const home = process.env.HOME;
    process.env.HOME = dir;
    try {
      writeFileSync(join(dir, "h.marigold"), "range(Colour).return\n");
      const result = await pi.emit("tool_result", writeResult("~/h.marigold"), ctx);
      expect(result.content[1].text).toContain("error[undefined-enum]");
    } finally {
      process.env.HOME = home;
    }
  });

  it("confirms a clean file after edit", async () => {
    writeFileSync(join(dir, "ok.marigold"), "range(0, 1).return\n");
    const event = { ...writeResult(join(dir, "ok.marigold")), toolName: "edit" };
    const result = await pi.emit("tool_result", event, ctx);
    expect(result.content[1].text).toContain("marigold: no diagnostics");
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

  it("refuses paths that are not .marigold files without reading them", async () => {
    writeFileSync(join(dir, "secret.txt"), "range(0, 1).return");
    const tool = pi.tools.get("marigold_check");
    await expect(tool.execute("id", { path: "secret.txt" }, undefined, undefined, ctx)).rejects.toThrow(
      /only .*\.marigold/,
    );
    await expect(tool.execute("id", { path: "~/.ssh/id_rsa" }, undefined, undefined, ctx)).rejects.toThrow(
      /only .*\.marigold/,
    );
  });

  it("refuses files above the size limit before reading them", async () => {
    writeFileSync(join(dir, "huge.marigold"), " ".repeat(10 * 1024 * 1024 + 1));
    const tool = pi.tools.get("marigold_check");
    await expect(tool.execute("id", { path: "huge.marigold" }, undefined, undefined, ctx)).rejects.toThrow(
      /too large/,
    );
  });

  it("fails with a clear error for a missing file", async () => {
    const tool = pi.tools.get("marigold_check");
    await expect(tool.execute("id", { path: "missing.marigold" }, undefined, undefined, ctx)).rejects.toThrow(
      /missing\.marigold/,
    );
  });
});

describe("failure reporting", () => {
  const withFake = async (mode: string, extra: string[] = [], timeoutMs = 2000) => {
    await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
    pi = fakePi();
    createMarigoldExtension({ command: process.execPath, args: [fakeServer, mode, ...extra], timeoutMs })(pi.api as any);
  };

  it("says the file was not checked when the server cannot start", async () => {
    pi = fakePi();
    createMarigoldExtension({ command: "/nonexistent/marigold-lsp" })(pi.api as any);
    writeFileSync(join(dir, "a.marigold"), "x");
    const result = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    expect(result.content[1].text).toMatch(/could not check/);
    expect(result.content[1].text).toMatch(/NOT checked/);
  });

  it("includes server stderr in could-not-check notes", async () => {
    await withFake("crash");
    writeFileSync(join(dir, "a.marigold"), "x");
    const result = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    expect(result.content[1].text).toContain("could not check");
    expect(result.content[1].text).toContain("boom: fake server crashed");
  });

  it("respawns the server after it dies", async () => {
    await withFake("die-once", [join(dir, "marker")]);
    writeFileSync(join(dir, "a.marigold"), "x");
    const first = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    expect(first.content[1].text).toContain("could not check");
    const second = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    expect(second.content[1].text).toContain("marigold: no diagnostics");
  });

  it("gives up after repeated crashes with a clear note", async () => {
    await withFake("crash");
    writeFileSync(join(dir, "a.marigold"), "x");
    let text = "";
    for (let i = 0; i < 7; i++) {
      text = (await pi.emit("tool_result", writeResult("a.marigold"), ctx)).content[1].text;
    }
    expect(text).toContain("could not check");
    expect(text).toMatch(/giving up|too many/);
  });

  it("stops waiting on a hung server once the restart cap is reached", async () => {
    await withFake("no-publish", [], 150);
    writeFileSync(join(dir, "a.marigold"), "x");
    for (let i = 0; i < 4; i++) await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    const started = Date.now();
    const result = await pi.emit("tool_result", writeResult("a.marigold"), ctx);
    expect(Date.now() - started).toBeLessThan(100);
    expect(result.content[1].text).toContain("could not check");
  });
});
