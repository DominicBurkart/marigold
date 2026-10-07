import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { rmSync } from "node:fs";
import { join } from "node:path";
import { pathToFileURL } from "node:url";
import { LspClient } from "../src/lsp-client.ts";
import { fakeServer, lspBinary, tempDir } from "./helpers.ts";

let dir: string;
let uri: string;
let rootUri: string;
let client: LspClient | undefined;

beforeEach(() => {
  dir = tempDir();
  rootUri = pathToFileURL(dir).href;
  uri = pathToFileURL(join(dir, "example.marigold")).href;
});

afterEach(async () => {
  await client?.shutdown();
  client = undefined;
  rmSync(dir, { recursive: true, force: true });
});

const startFake = (mode: string, timeoutMs = 2000, extra: string[] = []) =>
  LspClient.start(process.execPath, [fakeServer, mode, ...extra], rootUri, timeoutMs);

describe("LspClient against marigold-lsp", () => {
  it("returns diagnostics with LSP positions", async () => {
    client = await LspClient.start(lspBinary, [], rootUri);
    const diags = await client.check(uri, "range(0, 1).return\nrange(Colour).return\n");
    expect(diags).toHaveLength(1);
    expect(diags[0].code).toBe("undefined-enum");
    expect(diags[0].range.start).toEqual({ line: 1, character: 6 });
  });

  it("tracks document versions across repeated checks", async () => {
    client = await LspClient.start(lspBinary, [], rootUri);
    expect(await client.check(uri, "range(0, 1).retur")).toHaveLength(1);
    expect(await client.check(uri, "range(0, 1).return")).toEqual([]);
    expect(await client.check(uri, "range(Colour).return")).toHaveLength(1);
  });

  it("serializes concurrent checks of the same uri", async () => {
    client = await LspClient.start(lspBinary, [], rootUri);
    const [a, b, c] = await Promise.all([
      client.check(uri, "range(Colour).return"),
      client.check(uri, "range(0, 1).return"),
      client.check(uri, "range(0, 1).retur"),
    ]);
    expect(a).toHaveLength(1);
    expect(b).toEqual([]);
    expect(c).toHaveLength(1);
  });

  it("shuts down cleanly and is idempotent", async () => {
    client = await LspClient.start(lspBinary, [], rootUri);
    await client.shutdown();
    await client.shutdown();
  });

  it("rejects when the server command does not exist", async () => {
    await expect(LspClient.start("/nonexistent/marigold-lsp", [], rootUri)).rejects.toThrow(/marigold-lsp/);
  });
});

describe("LspClient failure handling", () => {
  it("rejects pending checks when the server exits", async () => {
    client = await startFake("crash");
    await expect(client.check(uri, "x")).rejects.toThrow(/marigold-lsp exited/);
    expect(client.stopped).toBe(true);
  });

  it("includes the server stderr tail in the exit error", async () => {
    client = await startFake("crash");
    await expect(client.check(uri, "x")).rejects.toThrow(/boom: fake server crashed/);
    expect(client.stderrTail()).toContain("boom");
  });

  it("rejects immediately once stopped without an unhandled error", async () => {
    client = await startFake("crash");
    await client.check(uri, "x").catch(() => {});
    const started = Date.now();
    await expect(client.check(uri, "y")).rejects.toThrow(/marigold-lsp exited/);
    expect(Date.now() - started).toBeLessThan(500);
  });

  it("rejects initialize that never answers and kills the child", async () => {
    const started = Date.now();
    await expect(startFake("silent", 300)).rejects.toThrow(/initialize/);
    expect(Date.now() - started).toBeLessThan(3000);
  });

  it("times out diagnostics once, then fails fast", async () => {
    client = await startFake("no-publish", 300);
    await expect(client.check(uri, "x")).rejects.toThrow(/did not publish/);
    expect(client.stopped).toBe(true);
    const started = Date.now();
    await expect(client.check(uri, "y")).rejects.toThrow(/marigold-lsp exited/);
    expect(Date.now() - started).toBeLessThan(200);
  });

  it("fails pending checks on a malformed frame header", async () => {
    client = await startFake("garbage");
    await expect(client.check(uri, "x")).rejects.toThrow(/malformed|exited/);
    expect(client.stopped).toBe(true);
  });

  it("fails pending checks on invalid JSON", async () => {
    client = await startFake("badjson");
    await expect(client.check(uri, "x")).rejects.toThrow(/malformed|exited/);
    expect(client.stopped).toBe(true);
  });

  it("caps the receive buffer", async () => {
    client = await startFake("flood", 5000);
    await expect(client.check(uri, "x")).rejects.toThrow(/buffer|exited/);
    expect(client.stopped).toBe(true);
  });

  it("keeps waiting checks per uri independent", async () => {
    client = await startFake("delayed", 3000);
    const other = pathToFileURL(join(dir, "other.marigold")).href;
    const [a, b] = await Promise.all([client.check(uri, "a"), client.check(other, "b")]);
    expect(a).toEqual([]);
    expect(b).toEqual([]);
  });
});
