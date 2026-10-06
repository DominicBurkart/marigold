import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { mkdirSync, readFileSync, readdirSync, rmSync, statSync, symlinkSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { createMarigoldExtension } from "../extensions/marigold.ts";
import { fakePi, fakeServer, lspBinary, tempDir } from "./helpers.ts";

const PROGRAM =
  "enum Color { Red, Green }\nstruct Row { a: int[0, 5] }\nfn double(x: i32) -> i32 { x * 2 }\nx = range(0, 3).map(double)\nx.return\ny = x.map(double)\ny.return\nrange(Color).return\n";

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

const call = async (name: string, params: Record<string, unknown>) => {
  const result = await pi.tools.get(name).execute("id", params, undefined, undefined, ctx);
  return result.content[0].text as string;
};

const at = (text: string, needle: string, nth = 0) => {
  let index = -1;
  for (let i = 0; i <= nth; i++) index = text.indexOf(needle, index + 1);
  const before = text.slice(0, index).split("\n");
  return { line: before.length, column: before[before.length - 1].length + 1 };
};

const write = (name: string, text = PROGRAM) => {
  writeFileSync(join(dir, name), text);
  return name;
};

describe("registration", () => {
  it("registers every navigation tool with the documented parameters", () => {
    const expected: Record<string, string[]> = {
      marigold_definition: ["path", "line", "column"],
      marigold_references: ["path", "line", "column", "include_declaration"],
      marigold_hover: ["path", "line", "column"],
      marigold_symbols: ["path"],
      marigold_workspace_symbols: ["query"],
      marigold_rename: ["path", "line", "column", "new_name", "apply"],
    };
    for (const [name, keys] of Object.entries(expected)) {
      const tool = pi.tools.get(name);
      expect(tool, name).toBeDefined();
      expect(Object.keys(tool.parameters.properties).sort(), name).toEqual([...keys].sort());
      expect(tool.description.length).toBeGreaterThan(40);
    }
    expect(pi.tools.get("marigold_check")).toBeDefined();
  });

  it("documents the severities in the check tool and skill", () => {
    const description = pi.tools.get("marigold_check").description;
    expect(description).toMatch(/read before it is declared/);
    expect(description).toMatch(/get\(\) method/);
    expect(description).toMatch(/information/);
    expect(description).toMatch(/Rust items in scope/);
    const skill = readFileSync(join(__dirname, "..", "skills", "marigold", "SKILL.md"), "utf8");
    const warning = skill.slice(skill.indexOf("Warning:"), skill.indexOf("Information:"));
    const information = skill.slice(skill.indexOf("Information:"), skill.indexOf("## Complexity"));
    expect(warning).toContain("undefined-stream-variable");
    expect(warning).not.toContain("undefined-fn");
    for (const code of ["undefined-stream-variable", "undefined-fn", "undefined-struct", "resolver-diagnostics-truncated"]) {
      expect(information).toContain(code);
    }
  });

  it("documents 1-based UTF-16 columns", () => {
    expect(pi.tools.get("marigold_definition").description).toMatch(/1-based/);
    expect(pi.tools.get("marigold_definition").description).toMatch(/UTF-16/);
  });
});

describe("marigold_definition", () => {
  it("finds the declaration of a function from a use", async () => {
    write("a.marigold");
    const { line, column } = at(PROGRAM, "double)");
    const text = await call("marigold_definition", { path: "a.marigold", line, column });
    expect(text).toContain("a.marigold:3:4");
    expect(text).toContain("3 | fn double(x: i32) -> i32 { x * 2 }");
  });

  it("says so when there is no definition", async () => {
    write("a.marigold");
    const text = await call("marigold_definition", { path: "a.marigold", line: 1, column: 2 });
    expect(text).toMatch(/no definition found/);
  });

  it("counts columns in UTF-16 units after astral characters", async () => {
    const source = 'struct Row { a: int[0, 5] }\nread_file("\u{1F600}\u{e9}.csv", csv, struct=Row).return\n';
    write("u.marigold", source);
    const { line, column } = at(source, "Row).return");
    expect(column).toBe(source.split("\n")[1].indexOf("Row).return") + 1);
    const text = await call("marigold_definition", { path: "u.marigold", line, column });
    expect(text).toContain("u.marigold:1:8");
    const off = await call("marigold_definition", { path: "u.marigold", line, column: column - 1 });
    expect(off).toMatch(/no definition found/);
  });

  it("refuses files that are not .marigold without reading them", async () => {
    writeFileSync(join(dir, "secret.txt"), PROGRAM);
    await expect(call("marigold_definition", { path: "secret.txt", line: 1, column: 1 })).rejects.toThrow(
      /only .*\.marigold/,
    );
  });

  it("refuses oversized files and missing files", async () => {
    write("huge.marigold", " ".repeat(10 * 1024 * 1024 + 1));
    await expect(call("marigold_definition", { path: "huge.marigold", line: 1, column: 1 })).rejects.toThrow(
      /too large/,
    );
    await expect(call("marigold_definition", { path: "none.marigold", line: 1, column: 1 })).rejects.toThrow(
      /none\.marigold/,
    );
  });

  it("rejects invalid positions", async () => {
    write("a.marigold");
    await expect(call("marigold_definition", { path: "a.marigold", line: 0, column: 1 })).rejects.toThrow(/line/);
    await expect(call("marigold_definition", { path: "a.marigold", line: 1, column: 1.5 })).rejects.toThrow(/column/);
  });

  it("uses the current file contents on every call", async () => {
    write("a.marigold");
    const first = at(PROGRAM, "double)");
    await call("marigold_definition", { path: "a.marigold", ...first });
    const edited = "\n" + PROGRAM;
    write("a.marigold", edited);
    const text = await call("marigold_definition", { path: "a.marigold", line: first.line + 1, column: first.column });
    expect(text).toContain("a.marigold:4:4");
  });
});

describe("marigold_references", () => {
  it("lists every use including the declaration by default", async () => {
    write("a.marigold");
    const { line, column } = at(PROGRAM, "double)");
    const text = await call("marigold_references", { path: "a.marigold", line, column });
    expect(text).toContain("3 references");
    expect(text).toContain("a.marigold:3:4");
    expect(text).toContain("a.marigold:4:21");
    expect(text).toContain("a.marigold:6:11");
  });

  it("omits the declaration when asked", async () => {
    write("a.marigold");
    const { line, column } = at(PROGRAM, "double)");
    const text = await call("marigold_references", { path: "a.marigold", line, column, include_declaration: false });
    expect(text).toContain("2 references");
    expect(text).not.toContain("a.marigold:3:4");
  });

  it("says so when nothing is found", async () => {
    write("a.marigold");
    const text = await call("marigold_references", { path: "a.marigold", line: 1, column: 2 });
    expect(text).toMatch(/no references found/);
  });

  it("refuses non-.marigold paths", async () => {
    writeFileSync(join(dir, "a.txt"), PROGRAM);
    await expect(call("marigold_references", { path: "a.txt", line: 1, column: 1 })).rejects.toThrow(
      /only .*\.marigold/,
    );
  });
});

describe("marigold_hover", () => {
  it("returns the signature and complexity markdown", async () => {
    const source = "x = range(0, 5)\nx.return\n";
    write("h.marigold", source);
    const text = await call("marigold_hover", { path: "h.marigold", line: 1, column: 1 });
    expect(text).toContain("x = range(0, 5)");
    expect(text).toContain("cardinality: 5");
    expect(text).toContain("time: O(1)");
  });

  it("says so when there is nothing to show", async () => {
    write("h.marigold", "x = range(0, 5)\nx.return\n");
    const text = await call("marigold_hover", { path: "h.marigold", line: 1, column: 2 });
    expect(text).toMatch(/no hover information/);
  });

  it("refuses non-.marigold paths", async () => {
    writeFileSync(join(dir, "a.txt"), "x");
    await expect(call("marigold_hover", { path: "a.txt", line: 1, column: 1 })).rejects.toThrow(/only .*\.marigold/);
  });
});

describe("marigold_symbols", () => {
  it("lists declarations with 1-based line:column", async () => {
    write("a.marigold");
    const text = await call("marigold_symbols", { path: "a.marigold" });
    expect(text).toContain("a.marigold:1:1: enum Color");
    expect(text).toContain("a.marigold:2:1: struct Row");
    expect(text).toContain("a.marigold:3:1: function double");
    expect(text).toMatch(/variable x/);
  });

  it("says so for a file without symbols", async () => {
    write("e.marigold", "range(0, 1).return\n");
    expect(await call("marigold_symbols", { path: "e.marigold" })).toMatch(/no symbols/);
  });

  it("refuses non-.marigold paths", async () => {
    writeFileSync(join(dir, "a.txt"), "x");
    await expect(call("marigold_symbols", { path: "a.txt" })).rejects.toThrow(/only .*\.marigold/);
  });
});

describe("marigold_workspace_symbols", () => {
  it("finds symbols across files in the working directory", async () => {
    write("a.marigold");
    mkdirSync(join(dir, "sub"));
    writeFileSync(join(dir, "sub", "b.marigold"), "fn doubler(x: i32) -> i32 { x }\nrange(0, 1).map(doubler).return\n");
    const text = await call("marigold_workspace_symbols", { query: "doub" });
    expect(text).toContain("a.marigold:3:4: function double");
    expect(text).toContain("sub/b.marigold:1:4: function doubler");
  });

  it("says so when nothing matches", async () => {
    write("a.marigold");
    expect(await call("marigold_workspace_symbols", { query: "zzzzqqq" })).toMatch(/no symbols matching "zzzzqqq"/);
  });
});

describe("marigold_rename", () => {
  const rename = (extra: Record<string, unknown> = {}, name = "a.marigold") => {
    const { line, column } = at(PROGRAM, "double)");
    return call("marigold_rename", { path: name, line, column, new_name: "twice", ...extra });
  };

  it("previews edits per file without touching the disk", async () => {
    write("a.marigold");
    const before = statSync(join(dir, "a.marigold")).mtimeMs;
    const text = await rename();
    expect(text).toContain("3 edits in 1 file");
    expect(text).toContain("a.marigold");
    expect(text).toContain("3:4-3:10");
    expect(text).toContain('"double" -> "twice"');
    expect(text).toMatch(/not applied/);
    expect(readFileSync(join(dir, "a.marigold"), "utf8")).toBe(PROGRAM);
    expect(statSync(join(dir, "a.marigold")).mtimeMs).toBe(before);
    expect(readdirSync(dir)).toEqual(["a.marigold"]);
  });

  it("applies the edits when apply is true", async () => {
    write("a.marigold");
    const text = await rename({ apply: true });
    expect(text).toMatch(/applied 3 edits/);
    expect(readFileSync(join(dir, "a.marigold"), "utf8")).toBe(PROGRAM.replaceAll("double", "twice"));
    expect(readdirSync(dir)).toEqual(["a.marigold"]);
    const refs = await call("marigold_symbols", { path: "a.marigold" });
    expect(refs).toContain("function twice");
  });

  it("applies edits correctly after astral characters on the same line", async () => {
    const source = 'struct Row { a: int[0, 5] }\nread_file("\u{1F600}\u{e9}.csv", csv, struct=Row).return\n';
    write("u.marigold", source);
    const { line, column } = at(source, "Row).return");
    await call("marigold_rename", { path: "u.marigold", line, column, new_name: "Record", apply: true });
    expect(readFileSync(join(dir, "u.marigold"), "utf8")).toBe(source.replaceAll("Row", "Record"));
  });

  it("preserves CRLF line endings", async () => {
    const source = PROGRAM.replaceAll("\n", "\r\n");
    write("c.marigold", source);
    const { line, column } = at(PROGRAM, "double)");
    await call("marigold_rename", { path: "c.marigold", line, column, new_name: "twice", apply: true });
    expect(readFileSync(join(dir, "c.marigold"), "utf8")).toBe(source.replaceAll("double", "twice"));
  });

  it("refuses to apply outside the working directory but still previews", async () => {
    const outside = tempDir("pi-marigold-outside-");
    try {
      writeFileSync(join(outside, "o.marigold"), PROGRAM);
      const path = join(outside, "o.marigold");
      await expect(rename({ apply: true }, path)).rejects.toThrow(/outside the working directory/);
      expect(readFileSync(path, "utf8")).toBe(PROGRAM);
      expect(await rename({}, path)).toContain("3 edits");
    } finally {
      rmSync(outside, { recursive: true, force: true });
    }
  });

  it("refuses to apply through a symlink that leaves the working directory", async () => {
    const outside = tempDir("pi-marigold-outside-");
    try {
      writeFileSync(join(outside, "o.marigold"), PROGRAM);
      symlinkSync(join(outside, "o.marigold"), join(dir, "link.marigold"));
      await expect(rename({ apply: true }, "link.marigold")).rejects.toThrow(/outside the working directory/);
      expect(readFileSync(join(outside, "o.marigold"), "utf8")).toBe(PROGRAM);
    } finally {
      rmSync(outside, { recursive: true, force: true });
    }
  });

  it("refuses non-.marigold paths in both modes", async () => {
    writeFileSync(join(dir, "a.txt"), PROGRAM);
    await expect(rename({}, "a.txt")).rejects.toThrow(/only .*\.marigold/);
    await expect(rename({ apply: true }, "a.txt")).rejects.toThrow(/only .*\.marigold/);
  });

  it("refuses oversized files", async () => {
    write("huge.marigold", " ".repeat(10 * 1024 * 1024 + 1));
    await expect(rename({}, "huge.marigold")).rejects.toThrow(/too large/);
  });

  it("surfaces name clashes from the server", async () => {
    const source = "fn a(x: i32) -> i32 { x }\nfn b(x: i32) -> i32 { x }\nrange(0, 3).map(a).map(b).return\n";
    write("k.marigold", source);
    const { line, column } = at(source, "a)");
    await expect(call("marigold_rename", { path: "k.marigold", line, column, new_name: "b" })).rejects.toThrow(
      /already declared/,
    );
  });

  it("surfaces invalid identifiers from the server", async () => {
    write("a.marigold");
    await expect(rename({ new_name: "1bad" })).rejects.toThrow(/invalid identifier/);
  });

  it("says so when there is no symbol at the position", async () => {
    write("a.marigold");
    const text = await call("marigold_rename", { path: "a.marigold", line: 1, column: 2, new_name: "z" });
    expect(text).toMatch(/no renamable symbol/);
  });

  it("reports a rename to the same name as no changes", async () => {
    write("a.marigold");
    const text = await rename({ new_name: "double", apply: true });
    expect(text).toMatch(/no changes/);
    expect(readFileSync(join(dir, "a.marigold"), "utf8")).toBe(PROGRAM);
  });
});

describe("stale file protection", () => {
  const staleServer = async (mutate: () => void) => {
    await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
    pi = fakePi();
    createMarigoldExtension({
      command: process.execPath,
      args: [fakeServer, "rename-edit", join(dir, "marker")],
      timeoutMs: 2000,
    })(pi.api as any);
    void mutate;
  };

  it("does not write when the file differs from the text the edits were computed on", async () => {
    await staleServer(() => {});
    writeFileSync(join(dir, "s.marigold"), "abc\n");
    const tool = pi.tools.get("marigold_rename");
    const running = tool.execute(
      "id",
      { path: "s.marigold", line: 1, column: 1, new_name: "zzz", apply: true },
      undefined,
      undefined,
      ctx,
    );
    const deadline = Date.now() + 3000;
    while (Date.now() < deadline) {
      try {
        readFileSync(join(dir, "marker"));
        break;
      } catch {
        await new Promise((r) => setTimeout(r, 10));
      }
    }
    writeFileSync(join(dir, "s.marigold"), "xyz\n");
    writeFileSync(join(dir, "release"), "");
    await expect(running).rejects.toThrow(/changed since/);
    expect(readFileSync(join(dir, "s.marigold"), "utf8")).toBe("xyz\n");
    expect(readdirSync(dir).filter((f) => f.endsWith(".tmp") || f.includes(".marigold."))).toEqual([]);
  });
});

describe("server failures", () => {
  it("reports a navigation timeout and respawns", async () => {
    await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
    pi = fakePi();
    createMarigoldExtension({
      command: process.execPath,
      args: [fakeServer, "nav-hang-once", join(dir, "marker")],
      timeoutMs: 300,
    })(pi.api as any);
    write("a.marigold");
    await expect(call("marigold_definition", { path: "a.marigold", line: 1, column: 1 })).rejects.toThrow(
      /did not answer/,
    );
    expect(await call("marigold_definition", { path: "a.marigold", line: 1, column: 1 })).toMatch(
      /no definition found/,
    );
  });

  it("surfaces JSON-RPC error messages", async () => {
    await pi.emit("session_shutdown", { type: "session_shutdown" }, ctx);
    pi = fakePi();
    createMarigoldExtension({
      command: process.execPath,
      args: [fakeServer, "nav-error"],
      timeoutMs: 2000,
    })(pi.api as any);
    write("a.marigold");
    await expect(call("marigold_hover", { path: "a.marigold", line: 1, column: 1 })).rejects.toThrow(/fake nav failure/);
  });

  it("does not claim the file was checked when the server cannot start", async () => {
    pi = fakePi();
    createMarigoldExtension({ command: "/nonexistent/marigold-lsp" })(pi.api as any);
    write("a.marigold");
    const error = await call("marigold_hover", { path: "a.marigold", line: 1, column: 1 }).catch((e: Error) => e);
    expect((error as Error).message).toMatch(/failed to start marigold-lsp/);
    expect((error as Error).message).not.toMatch(/NOT checked/);
  });
});
