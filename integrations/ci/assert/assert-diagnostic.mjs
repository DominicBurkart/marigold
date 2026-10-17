import fs from "node:fs";
import { fileURLToPath } from "node:url";

export function parseArgs(argv) {
  const o = { after: null, contains: null, absent: null, file: null };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--after") o.after = Number(argv[++i]);
    else if (a === "--contains") o.contains = argv[++i];
    else if (a === "--absent") o.absent = argv[++i];
    else if (a.startsWith("--")) throw new Error(`unknown argument ${a}`);
    else o.file = a;
  }
  if (!o.file) throw new Error("usage: assert-diagnostic.mjs requests.jsonl ...");
  if (!Number.isInteger(o.after)) throw new Error("--after <turn> is required");
  if (!o.contains) throw new Error("--contains <substring> is required");
  return o;
}

export function readLog(text) {
  return text
    .split("\n")
    .filter((l) => l.trim())
    .map((l) => JSON.parse(l));
}

function bodyOf(entry) {
  return JSON.stringify(entry.request ?? entry);
}

export function check(entries, { after, contains, absent }) {
  const later = entries.filter(
    (e) => Number.isInteger(e.turn) && e.turn > after,
  );
  const hits = later.filter((e) => bodyOf(e).includes(contains));
  const bad = absent ? later.filter((e) => bodyOf(e).includes(absent)) : [];
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

function snippet(entry, needle) {
  const s = bodyOf(entry);
  const at = s.indexOf(needle);
  if (at < 0) return s.slice(-300);
  return s.slice(Math.max(0, at - 150), at + needle.length + 150);
}

function closest(entries, contains) {
  const words = contains.split(/[^A-Za-z0-9]+/).filter(Boolean);
  let best = null;
  for (const e of entries) {
    const s = bodyOf(e);
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
  const entries = readLog(fs.readFileSync(o.file, "utf8"));
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
    const bad = o.absent ? r.later.find((e) => bodyOf(e).includes(o.absent)) : null;
    if (bad) {
      console.error(`evidence: ...${snippet(bad, o.absent)}...`);
    } else {
      const c = closest(r.later, o.contains);
      console.error(
        `closest request ${c.e.index} (turn ${c.e.turn}), tail: ...${snippet(c.e, o.contains)}`,
      );
    }
  }
  process.exit(1);
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main();
