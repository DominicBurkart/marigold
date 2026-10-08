import { describe, expect, it } from "vitest";
import { spawnSync } from "node:child_process";
import { mkdtempSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { lspBinary } from "./helpers.ts";

const root = fileURLToPath(new URL("..", import.meta.url));
const piBin = join(root, "node_modules", ".bin", "pi");

describe("real pi session with a scripted model", () => {
  it("feeds marigold diagnostics back to the model after a write", () => {
    const cwd = mkdtempSync(join(tmpdir(), "pi-marigold-e2e-"));
    const run = spawnSync(
      piBin,
      [
        "--mode", "json", "-p", "--offline", "--no-session", "-ne", "-nc", "-na",
        "-e", join(root, "test", "fixtures", "scripted-model.ts"),
        "-e", join(root, "extensions", "marigold.ts"),
        "--provider", "scripted", "--model", "scripted/writer",
        "write it",
      ],
      {
        cwd,
        input: "",
        encoding: "utf8",
        timeout: 60_000,
        env: { ...process.env, HOME: cwd, MARIGOLD_LSP_COMMAND: lspBinary },
      },
    );
    expect(run.status, run.stderr).toBe(0);
    expect(readFileSync(join(cwd, "broken.marigold"), "utf8")).toBe("range(Colour).return\n");

    const toolResults = run.stdout
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line))
      .filter((e) => e.type === "message_end" && e.message.role === "toolResult");
    expect(toolResults).toHaveLength(1);
    const texts = toolResults[0].message.content.map((c: { text: string }) => c.text);
    expect(texts[1]).toContain("broken.marigold:1:7: error[undefined-enum]");
  }, 90_000);

  it("lets the model call marigold_definition", () => {
    const cwd = mkdtempSync(join(tmpdir(), "pi-marigold-e2e-"));
    const run = spawnSync(
      piBin,
      [
        "--mode", "json", "-p", "--offline", "--no-session", "-ne", "-nc", "-na",
        "-e", join(root, "test", "fixtures", "scripted-nav.ts"),
        "-e", join(root, "extensions", "marigold.ts"),
        "--provider", "scripted", "--model", "scripted/navigator",
        "navigate",
      ],
      {
        cwd,
        input: "",
        encoding: "utf8",
        timeout: 60_000,
        env: { ...process.env, HOME: cwd, MARIGOLD_LSP_COMMAND: lspBinary },
      },
    );
    expect(run.status, run.stderr).toBe(0);
    const toolResults = run.stdout
      .split("\n")
      .filter(Boolean)
      .map((line) => JSON.parse(line))
      .filter((e) => e.type === "message_end" && e.message.role === "toolResult");
    expect(toolResults).toHaveLength(2);
    const text = toolResults[1].message.content.map((c: { text: string }) => c.text).join("\n");
    expect(toolResults[1].message.isError).toBe(false);
    expect(text).toContain("nav.marigold:1:4");
    expect(text).toContain("1 | fn double(x: i32) -> i32 { x * 2 }");
  }, 90_000);
});
