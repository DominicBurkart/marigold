import fs from "node:fs";
import { fileURLToPath } from "node:url";

export function parseArgs(argv) {
  const o = { after: null, contains: null, absent: null, scope: null, file: null };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--after") o.after = Number(argv[++i]);
    else if (a === "--contains") o.contains = argv[++i];
    else if (a === "--absent") o.absent = argv[++i];
    else if (a === "--scope") o.scope = argv[++i];
    else if (a === "--outputs-only") o.scope = "tool-results";
    else if (a.startsWith("--")) throw new Error(`unknown argument ${a}`);
    else o.file = a;
  }
  if (!o.file) throw new Error("usage: assert-diagnostic.mjs requests.jsonl ...");
  if (!Number.isInteger(o.after)) throw new Error("--after <turn> is required");
  if (o.scope !== null && o.scope !== "tool-results") {
    throw new Error('--scope must be "tool-results"');
  }
  if (!o.contains) throw new Error("--contains <substring> is required");
  return o;
}

export function readLog(text) {
  return text
    .split("\n")
    .filter((l) => l.trim())
    .map((l) => JSON.parse(l));
}

function textOf(v) {
  return typeof v === "string" ? v : JSON.stringify(v);
}

export function toolResults(request) {
  const out = [];
  for (const m of request?.messages ?? []) {
    if (m.role === "tool") out.push(textOf(m.content));
    if (Array.isArray(m.content)) {
      for (const b of m.content) {
        if (b && b.type === "tool_result") out.push(textOf(b.content));
      }
    }
  }
  const items = Array.isArray(request?.input) ? request.input : [];
  for (const i of items) {
    if (i && typeof i.type === "string" && i.type.endsWith("_output")) {
      out.push(textOf(i.output));
    }
  }
  return out;
}

function bodyOf(entry, scope = null) {
  if (scope === "tool-results") return toolResults(entry.request).join("\n");
  if (typeof entry.request === "string") return entry.request;
  return JSON.stringify(entry.request ?? entry);
}

export function check(entries, { after, contains, absent, scope = null }) {
  const later = entries.filter(
    (e) => Number.isInteger(e.turn) && e.turn > after,
  );
  const hits = later.filter((e) => bodyOf(e, scope).includes(contains));
  const bad = absent ? later.filter((e) => bodyOf(e, scope).includes(absent)) : [];
  const problems = [];
  if (later.length === 0) {
    problems.push(`no request was logged after turn ${after}`);
  } else if (hits.length === 0) {
    problems.push(`no request after turn ${after} contains "${contains}"`);
  }
  if (bad.length > 0) {
    problems.push(
      `request(s) ${bad.map((e) => e.index).join(",")} contain forbidden "${absent}"`,
    );
  }
  return { ok: problems.length === 0, problems, hits, later };
}

function snippet(entry, needle, scope = null) {
  const s = bodyOf(entry, scope);
  const at = s.indexOf(needle);
  if (at < 0) return s.slice(-300);
  return s.slice(Math.max(0, at - 150), at + needle.length + 150);
}

function closest(entries, contains, scope = null) {
  const words = contains.split(/[^A-Za-z0-9]+/).filter(Boolean);
  let best = null;
  for (const e of entries) {
    const s = bodyOf(e, scope);
    const score = words.filter((w) => s.includes(w)).length;
    if (!best || score > best.score) best = { e, score };
  }
  return best;
}

function main() {
  let o;
  try {
    o = parseArgs(process.argv.slice(2));
  } catch (e) {
    process.stderr.write(`${e.message}\n`);
    process.exit(2);
  }
  let entries = readLog(fs.readFileSync(o.file, "utf8"));
  const r = check(entries, o);
  if (r.ok) {
    const h = r.hits[0];
    console.log(
      `ok: request ${h.index} (answered turn ${h.turn}) contains "${o.contains}"` +
        (o.absent ? `; "${o.absent}" absent after turn ${o.after}` : ""),
    );
    return;
  }
  for (const p of r.problems) console.error(`FAIL: ${p}`);
  const turns = entries.map((e) => `${e.index}:${e.turn ?? "aux"}`).join(" ");
  console.error(`logged requests (index:turn): ${turns || "none"}`);
  if (r.later.length > 0) {
    const bad = o.absent ? r.later.find((e) => bodyOf(e, o.scope).includes(o.absent)) : null;
    if (bad) {
      console.error(`evidence: ...${snippet(bad, o.absent, o.scope)}...`);
    } else {
      const c = closest(r.later, o.contains, o.scope);
      console.error(
        `closest request ${c.e.index} (turn ${c.e.turn}), tail: ...${snippet(c.e, o.contains, o.scope)}`,
      );
    }
  }
  process.exit(1);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main();
