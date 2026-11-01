import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { createServer, substitute, parseArgs, PROTOCOLS } from "./mock-model.mjs";

const here = path.dirname(fileURLToPath(import.meta.url));
const SCRIPT = [
  { tool_call: { name: "Write", arguments: { file_path: "{file}", content: "a {cwd}" } } },
  { text: "done {cwd}" },
];
const VARS = { cwd: "/w", file: "/w/h.marigold" };
const MAIN = { model: "m", tools: [{ name: "Write" }], messages: [] };

async function start() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mm-"));
  const logPath = path.join(dir, "log.jsonl");
  const server = createServer({ protocol: "anthropic", script: SCRIPT, vars: VARS, logPath });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const base = `http://127.0.0.1:${server.address().port}`;
  const log = () =>
    fs.readFileSync(logPath, "utf8").split("\n").filter(Boolean).map(JSON.parse);
  return { server, base, log };
}

const post = (base, body, p = "/v1/messages?beta=true") =>
  fetch(base + p, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });

function parseSse(text) {
  return text
    .split("\n\n")
    .filter(Boolean)
    .map((b) => {
      const ev = /^event: (.*)$/m.exec(b)[1];
      const data = JSON.parse(/^data: (.*)$/m.exec(b)[1]);
      return { ev, data };
    });
}

test("substitute replaces placeholders recursively and leaves unknown", () => {
  assert.deepEqual(substitute({ a: ["{cwd}/x", 1, "{nope}"] }, { cwd: "/w" }), {
    a: ["/w/x", 1, "{nope}"],
  });
});

test("parseArgs reads flags and vars, rejects bad input", () => {
  const o = parseArgs(["--protocol", "anthropic", "--script", "s", "--log", "l", "--var", "cwd=/a=b"]);
  assert.equal(o.vars.cwd, "/a=b");
  assert.equal(o.port, 0);
  assert.throws(() => parseArgs(["--protocol", "anthropic"]));
  assert.throws(() => parseArgs(["--bogus"]));
});

test("anthropic non-streaming: tool_use then text, with logging", async () => {
  const { server, base, log } = await start();
  try {
    const r1 = await (await post(base, MAIN)).json();
    assert.equal(r1.stop_reason, "tool_use");
    assert.equal(r1.content[0].type, "tool_use");
    assert.equal(r1.content[0].name, "Write");
    assert.deepEqual(r1.content[0].input, { file_path: "/w/h.marigold", content: "a /w" });
    const r2 = await (await post(base, MAIN)).json();
    assert.equal(r2.stop_reason, "end_turn");
    assert.equal(r2.content[0].text, "done /w");
    const entries = log();
    assert.deepEqual(entries.map((e) => [e.index, e.turn]), [[1, 1], [2, 2]]);
    assert.deepEqual(entries[0].request, MAIN);
  } finally {
    server.close();
  }
});

test("anthropic streaming: full event sequence for tool_use and text", async () => {
  const { server, base } = await start();
  try {
    const t1 = await (await post(base, { ...MAIN, stream: true })).text();
    const e1 = parseSse(t1).filter((e) => e.ev !== "ping");
    assert.deepEqual(
      e1.map((e) => e.ev).filter((v, i, a) => a.indexOf(v) === i),
      ["message_start", "content_block_start", "content_block_delta", "content_block_stop", "message_delta", "message_stop"],
    );
    assert.equal(e1[1].data.content_block.type, "tool_use");
    const json = e1
      .filter((e) => e.ev === "content_block_delta")
      .map((e) => e.data.delta.partial_json)
      .join("");
    assert.deepEqual(JSON.parse(json), { file_path: "/w/h.marigold", content: "a /w" });
    assert.equal(e1.at(-2).data.delta.stop_reason, "tool_use");
    const t2 = await (await post(base, { ...MAIN, stream: true })).text();
    const e2 = parseSse(t2);
    const text = e2.filter((e) => e.ev === "content_block_delta").map((e) => e.data.delta.text).join("");
    assert.equal(text, "done /w");
    assert.equal(e2.find((e) => e.ev === "message_delta").data.delta.stop_reason, "end_turn");
  } finally {
    server.close();
  }
});

test("unrelated requests do not consume script turns", async () => {
  const { server, base, log } = await start();
  try {
    assert.equal((await fetch(base, { method: "HEAD" })).status, 200);
    assert.equal((await fetch(base + "/")).status, 200);
    const models = await (await fetch(base + "/v1/models")).json();
    assert.equal(models.data[0].id, "mock-model");
    const ct = await (await post(base, { messages: [] }, "/v1/messages/count_tokens")).json();
    assert.equal(ct.input_tokens, 1);
    const title = await post(base, { model: "h", stream: true, tools: [], messages: [] });
    assert.equal(title.status, 200);
    await title.text();
    const r = await (await post(base, MAIN)).json();
    assert.equal(r.content[0].name, "Write");
    const entries = log();
    assert.equal(entries[0].turn, null);
    assert.equal(entries[0].kind, "auxiliary");
    assert.equal(entries[1].turn, 1);
  } finally {
    server.close();
  }
});

test("script exhaustion keeps answering with text and a later turn number", async () => {
  const { server, base, log } = await start();
  try {
    await post(base, MAIN);
    await post(base, MAIN);
    const r = await (await post(base, MAIN)).json();
    assert.equal(r.stop_reason, "end_turn");
    assert.equal(log()[2].kind, "exhausted");
    assert.equal(log()[2].turn, 3);
  } finally {
    server.close();
  }
});

test("invalid scripts are rejected", () => {
  for (const bad of [[], [{}], [{ text: "a", tool_call: { name: "x", arguments: {} } }], [{ tool_call: { name: "x" } }]]) {
    assert.throws(() => createServer({ protocol: "anthropic", script: bad }));
  }
});

test("all protocols load and unknown protocols are rejected", () => {
  for (const protocol of ["anthropic", "openai-chat", "openai-responses"]) {
    assert.ok(PROTOCOLS[protocol], protocol);
    assert.doesNotThrow(() => createServer({ protocol, script: SCRIPT, vars: VARS }));
  }
  assert.throws(() => createServer({ protocol: "nope", script: SCRIPT }), /unknown protocol/);
});

test("unknown protocol makes the CLI exit non-zero", () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mm-"));
  const script = path.join(dir, "s.json");
  fs.writeFileSync(script, JSON.stringify(SCRIPT));
  const r = spawnSync(
    process.execPath,
    [path.join(here, "mock-model.mjs"), "--protocol", "nope", "--script", script, "--log", path.join(dir, "l")],
    { encoding: "utf8" },
  );
  assert.notEqual(r.status, 0);
  assert.match(r.stderr, /unknown protocol/);
});

test("CLI prints PORT= for port 0 and serves", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mm-"));
  const script = path.join(dir, "s.json");
  fs.writeFileSync(script, JSON.stringify(SCRIPT));
  const { spawn } = await import("node:child_process");
  const p = spawn(process.execPath, [
    path.join(here, "mock-model.mjs"), "--protocol", "anthropic", "--script", script,
    "--log", path.join(dir, "l"), "--port", "0", "--var", "cwd=/w", "--var", "file=/w/h.marigold",
  ]);
  try {
    const port = await new Promise((resolve, reject) => {
      let buf = "";
      p.stdout.on("data", (d) => {
        buf += d;
        const m = /PORT=(\d+)/.exec(buf);
        if (m) resolve(Number(m[1]));
      });
      p.on("exit", () => reject(new Error("exited")));
    });
    const r = await (await post(`http://127.0.0.1:${port}`, MAIN)).json();
    assert.equal(r.content[0].input.file_path, "/w/h.marigold");
  } finally {
    p.kill();
  }
});

const CHAT_MAIN = { model: "mock-model", stream: true, tools: [{ type: "function", function: { name: "write" } }], messages: [] };

async function startChat() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mm-"));
  const logPath = path.join(dir, "log.jsonl");
  const server = createServer({ protocol: "openai-chat", script: SCRIPT, vars: VARS, logPath });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const base = `http://127.0.0.1:${server.address().port}`;
  const log = () => fs.readFileSync(logPath, "utf8").split("\n").filter(Boolean).map(JSON.parse);
  return { server, base, log };
}

function parseChatSse(text) {
  return text.split("\n\n").filter(Boolean).map((b) => b.replace(/^data: /, ""));
}

test("openai-chat streams tool call chunks then [DONE]", async () => {
  const { server, base, log } = await startChat();
  try {
    const r = await post(base, CHAT_MAIN, "/v1/chat/completions");
    assert.match(r.headers.get("content-type"), /text\/event-stream/);
    const frames = parseChatSse(await r.text());
    assert.equal(frames.at(-1), "[DONE]");
    const chunks = frames.slice(0, -1).map(JSON.parse);
    assert.equal(chunks[0].choices[0].delta.role, "assistant");
    const calls = chunks.flatMap((c) => c.choices[0]?.delta?.tool_calls ?? []);
    assert.equal(calls[0].id, "call_mock_1");
    assert.equal(calls[0].function.name, "Write");
    const args = JSON.parse(calls.map((c) => c.function.arguments).join(""));
    assert.deepEqual(args, { file_path: "/w/h.marigold", content: "a /w" });
    assert.ok(calls.length >= 3);
    const finals = chunks.map((c) => c.choices[0]?.finish_reason).filter(Boolean);
    assert.deepEqual(finals, ["tool_calls"]);
    assert.equal(log()[0].turn, 1);
  } finally {
    server.close();
  }
});

test("openai-chat streams final text with finish_reason stop", async () => {
  const { server, base } = await startChat();
  try {
    await (await post(base, CHAT_MAIN, "/v1/chat/completions")).text();
    const r = await post(base, CHAT_MAIN, "/v1/chat/completions");
    const frames = parseChatSse(await r.text());
    assert.equal(frames.at(-1), "[DONE]");
    const chunks = frames.slice(0, -1).map(JSON.parse);
    const text = chunks.map((c) => c.choices[0]?.delta?.content ?? "").join("");
    assert.equal(text, "done /w");
    assert.deepEqual(chunks.map((c) => c.choices[0]?.finish_reason).filter(Boolean), ["stop"]);
  } finally {
    server.close();
  }
});

test("openai-chat non-streaming tool call and text", async () => {
  const { server, base } = await startChat();
  try {
    const a = await (await post(base, { ...CHAT_MAIN, stream: false }, "/v1/chat/completions")).json();
    assert.equal(a.choices[0].finish_reason, "tool_calls");
    const tc = a.choices[0].message.tool_calls[0];
    assert.equal(tc.type, "function");
    assert.deepEqual(JSON.parse(tc.function.arguments), { file_path: "/w/h.marigold", content: "a /w" });
    const b = await (await post(base, { ...CHAT_MAIN, stream: false }, "/v1/chat/completions")).json();
    assert.equal(b.choices[0].finish_reason, "stop");
    assert.equal(b.choices[0].message.content, "done /w");
  } finally {
    server.close();
  }
});

test("openai-chat tool-less request is auxiliary and consumes no turn", async () => {
  const { server, base, log } = await startChat();
  try {
    const r = await post(base, { model: "mock-model", stream: false, messages: [] }, "/v1/chat/completions");
    assert.equal((await r.json()).choices[0].message.content, '{"title":"mock"}');
    const e = log();
    assert.equal(e[0].turn, null);
    const m = await (await post(base, { ...CHAT_MAIN, stream: false }, "/v1/chat/completions")).json();
    assert.equal(m.choices[0].finish_reason, "tool_calls");
  } finally {
    server.close();
  }
});

test("openai-chat tolerates model listing and unknown probes", async () => {
  const { server, base } = await startChat();
  try {
    const m = await (await fetch(base + "/v1/models")).json();
    assert.equal(m.data[0].id, "mock-model");
    assert.equal((await fetch(base + "/v1/other")).status, 200);
  } finally {
    server.close();
  }
});

test("turnDelayMs delays replies after the first scripted turn only", async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mm-"));
  const server = createServer({
    protocol: "anthropic",
    script: SCRIPT,
    vars: VARS,
    logPath: path.join(dir, "log.jsonl"),
    turnDelayMs: 300,
  });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const base = `http://127.0.0.1:${server.address().port}`;
  const t0 = Date.now();
  await (await post(base, MAIN)).text();
  const first = Date.now() - t0;
  const t1 = Date.now();
  await (await post(base, MAIN)).text();
  const second = Date.now() - t1;
  server.close();
  assert.ok(first < 250, `first reply took ${first}ms`);
  assert.ok(second >= 290, `second reply took ${second}ms`);
});

test("parseArgs accepts --turn-delay-ms and rejects a negative value", () => {
  const base = ["--protocol", "anthropic", "--script", "s", "--log", "l"];
  assert.equal(parseArgs([...base, "--turn-delay-ms", "50"]).turnDelayMs, 50);
  assert.throws(() => parseArgs([...base, "--turn-delay-ms", "-1"]));
});
