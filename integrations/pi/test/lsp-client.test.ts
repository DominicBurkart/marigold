import { afterEach, describe, expect, it } from "vitest";
import { pathToFileURL } from "node:url";
import { LspClient } from "../src/lsp-client.ts";
import { lspBinary } from "./helpers.ts";

const uri = pathToFileURL("/tmp/example.marigold").href;
let client: LspClient | undefined;

afterEach(async () => {
  await client?.shutdown();
  client = undefined;
});

describe("LspClient against marigold-lsp", () => {
  it("returns diagnostics with LSP positions", async () => {
    client = await LspClient.start(lspBinary, [], pathToFileURL("/tmp").href);
    const diags = await client.check(uri, "range(0, 1).return\nrange(Colour).return\n");
    expect(diags).toHaveLength(1);
    expect(diags[0].code).toBe("undefined-enum");
    expect(diags[0].range.start).toEqual({ line: 1, character: 6 });
  });

  it("tracks document versions across repeated checks", async () => {
    client = await LspClient.start(lspBinary, [], pathToFileURL("/tmp").href);
    expect(await client.check(uri, "range(0, 1).retur")).toHaveLength(1);
    expect(await client.check(uri, "range(0, 1).return")).toEqual([]);
    expect(await client.check(uri, "range(Colour).return")).toHaveLength(1);
  });

  it("shuts down cleanly and is idempotent", async () => {
    client = await LspClient.start(lspBinary, [], pathToFileURL("/tmp").href);
    await client.shutdown();
    await client.shutdown();
  });

  it("rejects when the server command does not exist", async () => {
    await expect(
      LspClient.start("/nonexistent/marigold-lsp", [], pathToFileURL("/tmp").href),
    ).rejects.toThrow(/marigold-lsp/);
  });
});
