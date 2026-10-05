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
    expect(formatDiagnostics("a.marigold", [])).toBe("marigold: no diagnostics in a.marigold");
  });

  it("uses 1-based line and column with code", () => {
    expect(formatDiagnostics("a.marigold", [diag(1, 6, "undefined-enum", "bad enum")])).toBe(
      "marigold: 1 diagnostic in a.marigold\na.marigold:2:7: error[undefined-enum]: bad enum\n    range 2:7-2:8",
    );
  });

  it("indents continuation lines such as help text", () => {
    const out = formatDiagnostics("a.marigold", [diag(0, 0, "x", "first\nhelp: second")]);
    expect(out).toContain("a.marigold:1:1: error[x]: first\n    help: second\n    range 1:1-1:2");
  });

  it("truncates long lists and says how many were omitted", () => {
    const many = Array.from({ length: 25 }, (_, i) => diag(i, 0, "x", `m${i}`));
    const out = formatDiagnostics("a.marigold", many, 10);
    expect(out.split("\n").filter((l) => l.startsWith("a.marigold:"))).toHaveLength(10);
    expect(out).toContain("25 diagnostics");
    expect(out).toContain("15 more not shown");
  });

  it("uses the lowercase severity of each diagnostic", () => {
    const warn = { ...diag(0, 0, "w", "careful"), severity: 2 };
    const hint = { ...diag(0, 0, "h", "psst"), severity: 4 };
    const out = formatDiagnostics("a.marigold", [warn, hint]);
    expect(out).toContain("a.marigold:1:1: warning[w]: careful");
    expect(out).toContain("a.marigold:1:1: hint[h]: psst");
  });

  it("defaults to error when severity is absent", () => {
    const { severity: _, ...rest } = diag(0, 0, "x", "m");
    expect(formatDiagnostics("a.marigold", [rest])).toContain("a.marigold:1:1: error[x]: m");
  });

  it("shows the end position and a caret snippet when source is given", () => {
    const source = "range(0, 1).return\nrange(Colour).return\n";
    const d = {
      ...diag(1, 6, "undefined-enum", "bad enum"),
      range: { start: { line: 1, character: 6 }, end: { line: 1, character: 12 } },
    };
    expect(formatDiagnostics("a.marigold", [d], 20, source)).toBe(
      [
        "marigold: 1 diagnostic in a.marigold",
        "a.marigold:2:7: error[undefined-enum]: bad enum",
        "    range 2:7-2:13",
        "    2 | range(Colour).return",
        "      |       ^^^^^^",
      ].join("\n"),
    );
  });

  it("extends the caret to the end of the line for multi-line ranges", () => {
    const d = {
      ...diag(0, 2, "x", "m"),
      range: { start: { line: 0, character: 2 }, end: { line: 1, character: 1 } },
    };
    const out = formatDiagnostics("a.marigold", [d], 20, "abcdef\ng\n");
    expect(out).toContain("    range 1:3-2:2");
    expect(out).toContain("    1 | abcdef\n      |   ^^^^");
  });

  it("keeps tabs in the caret padding and tolerates out-of-range lines", () => {
    const out = formatDiagnostics("a.marigold", [diag(0, 1, "x", "m"), diag(9, 0, "x", "n")], 20, "\tab\n");
    expect(out).toContain("1 | \tab\n      | \t^");
    expect(out).toContain("a.marigold:10:1: error[x]: n");
  });
});
