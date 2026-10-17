import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { check, parseArgs } from "./assert-diagnostic.mjs";

const here = path.dirname(fileURLToPath(import.meta.url));
const entries = [
  { index: 1, turn: 1, request: { messages: ["start"] } },
  { index: 2, turn: 2, request: { messages: ["<new-diagnostics>undefined-fn</new-diagnostics>"] } },
  { index: 3, turn: null, request: { messages: ["undefined-fn aux"] } },
];

test("finds substring after the given turn", () => {
  assert.equal(check(entries, { after: 1, contains: "undefined-fn" }).ok, true);
});

test("ignores requests at or before the turn and auxiliary ones", () => {
  assert.equal(check(entries, { after: 2, contains: "undefined-fn" }).ok, false);
  assert.equal(check([entries[0], entries[2]], { after: 0, contains: "undefined-fn" }).ok, false);
});

test("absent substring fails when present", () => {
  assert.equal(check(entries, { after: 1, contains: "new-diagnostics", absent: "undefined-fn" }).ok, false);
  assert.equal(check(entries, { after: 1, contains: "new-diagnostics", absent: "zzz" }).ok, true);
});

test("parseArgs validates", () => {
  assert.throws(() => parseArgs(["f"]));
  assert.throws(() => parseArgs(["f", "--after", "1"]));
  assert.equal(parseArgs(["f", "--after", "1", "--contains", "x"]).after, 1);
});

test("CLI exit codes", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "ad-"));
  const f = path.join(dir, "r.jsonl");
  fs.writeFileSync(f, entries.map((e) => JSON.stringify(e)).join("\n") + "\n");
  const run = (...a) =>
    spawnSync(process.execPath, [path.join(here, "assert-diagnostic.mjs"), f, ...a], { encoding: "utf8" });
  assert.equal(run("--after", "1", "--contains", "undefined-fn").status, 0);
  const bad = run("--after", "1", "--contains", "nonexistent-code");
  assert.equal(bad.status, 1);
  assert.match(bad.stderr, /FAIL/);
  assert.equal(run("--after", "1", "--contains", "undefined-fn", "--absent", "undefined-fn").status, 1);
});
