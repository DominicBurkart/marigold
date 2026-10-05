import { describe, expect, it } from "vitest";
import { formatDiagnostics } from "../src/format.ts";

const diag = (line: number, character: number, code: string, message: string) => ({
  range: { start: { line, character }, end: { line, character: character + 1 } },
  severity: 1,
  code,
  source: "marigold",
  message,
});

describe("formatDiagnostics", () => {
  it("reports a clean file explicitly", () => {
    expect(formatDiagnostics("a.marigold", [])).toBe("marigold: a.marigold has no diagnostics");
  });

  it("uses 1-based line and column with code", () => {
    expect(formatDiagnostics("a.marigold", [diag(1, 6, "undefined-enum", "bad enum")])).toBe(
      "marigold: 1 diagnostic in a.marigold\na.marigold:2:7: error[undefined-enum]: bad enum",
    );
  });

  it("indents continuation lines such as help text", () => {
    const out = formatDiagnostics("a.marigold", [diag(0, 0, "x", "first\nhelp: second")]);
    expect(out).toContain("a.marigold:1:1: error[x]: first\n    help: second");
  });

  it("truncates long lists and says how many were omitted", () => {
    const many = Array.from({ length: 25 }, (_, i) => diag(i, 0, "x", `m${i}`));
    const out = formatDiagnostics("a.marigold", many, 10);
    expect(out.split("\n").filter((l) => l.startsWith("a.marigold:"))).toHaveLength(10);
    expect(out).toContain("25 diagnostics");
    expect(out).toContain("15 more not shown");
  });
});
