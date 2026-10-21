import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { createServer } from "./mock-model.mjs";

const SCRIPT = [
  { tool_call: { name: "shell", arguments: { command: ["echo", "{cwd}"] } } },
  { tool_call: { name: "marigold_check", namespace: "mcp__marigold__", arguments: { path: "{file}" } } },
  { text: "done {cwd}" },
];
const VARS = { cwd: "/w", file: "/w/h.marigold" };
const MAIN = { model: "m", tools: [{ type: "function", name: "shell" }], input: [] };

async function start() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "mmr-"));
  const logPath = path.join(dir, "log.jsonl");
  const server = createServer({ protocol: "openai-responses", script: SCRIPT, vars: VARS, logPath });
  await new Promise((r) => server.listen(0, "127.0.0.1", r));
  const base = `http://127.0.0.1:${server.address().port}`;
  const log = () =>
    fs.readFileSync(logPath, "utf8").split("\n").filter(Boolean).map(JSON.parse);
  return { server, base, log };
}

const post = (base, body, p = "/v1/responses") =>
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

test("responses non-streaming: function_call, namespaced call, then message", async () => {
  const { server, base, log } = await start();
  try {
    const r1 = await (await post(base, MAIN)).json();
    assert.equal(r1.object, "response");
    assert.equal(r1.status, "completed");
    assert.equal(r1.output[0].type, "function_call");
    assert.equal(r1.output[0].name, "shell");
    assert.deepEqual(JSON.parse(r1.output[0].arguments), { command: ["echo", "/w"] });
    assert.ok(r1.output[0].call_id);
    assert.ok(r1.usage.total_tokens >= 0);
    const r2 = await (await post(base, MAIN)).json();
    assert.equal(r2.output[0].namespace, "mcp__marigold__");
    assert.deepEqual(JSON.parse(r2.output[0].arguments), { path: "/w/h.marigold" });
    const r3 = await (await post(base, MAIN)).json();
    assert.equal(r3.output[0].type, "message");
    assert.equal(r3.output[0].content[0].text, "done /w");
    assert.deepEqual(log().map((e) => [e.index, e.turn]), [[1, 1], [2, 2], [3, 3]]);
    assert.deepEqual(log()[0].request, MAIN);
  } finally {
    server.close();
  }
});

test("responses streaming: event sequence for function_call and message", async () => {
  const { server, base } = await start();
  try {
    const t1 = await (await post(base, { ...MAIN, stream: true })).text();
    const e1 = parseSse(t1);
    assert.equal(e1[0].ev, "response.created");
    assert.equal(e1.at(-1).ev, "response.completed");
    assert.ok(e1.every((e, i) => e.data.sequence_number === i && e.data.type === e.ev));
    const added = e1.find((e) => e.ev === "response.output_item.added");
    assert.equal(added.data.item.type, "function_call");
    const args = e1
      .filter((e) => e.ev === "response.function_call_arguments.delta")
      .map((e) => e.data.delta)
      .join("");
    assert.deepEqual(JSON.parse(args), { command: ["echo", "/w"] });
    const done = e1.find((e) => e.ev === "response.output_item.done");
    assert.equal(done.data.item.name, "shell");
    assert.deepEqual(JSON.parse(done.data.item.arguments), { command: ["echo", "/w"] });
    const completed = e1.at(-1).data.response;
    assert.equal(completed.status, "completed");
    assert.equal(completed.usage.output_tokens, 1);
    await (await post(base, { ...MAIN, stream: true })).text();
    const e3 = parseSse(await (await post(base, { ...MAIN, stream: true })).text());
    assert.equal(e3.find((e) => e.ev === "response.output_item.added").data.item.type, "message");
    const text = e3
      .filter((e) => e.ev === "response.output_text.delta")
      .map((e) => e.data.delta)
      .join("");
    assert.equal(text, "done /w");
    assert.equal(e3.find((e) => e.ev === "response.output_item.done").data.item.content[0].text, "done /w");
  } finally {
    server.close();
  }
});

test("responses: probes are harmless and do not consume turns", async () => {
  const { server, base, log } = await start();
  try {
    assert.equal((await fetch(base, { method: "HEAD" })).status, 200);
    assert.equal((await fetch(base + "/")).status, 200);
    const models = await (await fetch(base + "/v1/models")).json();
    assert.equal(models.data[0].id, "mock-model");
    const aux = await post(base, { model: "h", stream: true, tools: [], input: [] });
    assert.equal(aux.status, 200);
    await aux.text();
    const r = await (await post(base, MAIN)).json();
    assert.equal(r.output[0].name, "shell");
    assert.equal(log()[0].kind, "auxiliary");
    assert.equal(log()[1].turn, 1);
  } finally {
    server.close();
  }
});
