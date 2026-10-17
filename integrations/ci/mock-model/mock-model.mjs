import http from "node:http";
import fs from "node:fs";
import { fileURLToPath } from "node:url";

export class NotImplementedError extends Error {
  constructor(protocol) {
    super(`NotImplemented: protocol "${protocol}" is not implemented yet`);
    this.name = "NotImplementedError";
  }
}

export function substitute(value, vars) {
  if (typeof value === "string") {
    return value.replace(/\{(\w+)\}/g, (m, k) =>
      Object.prototype.hasOwnProperty.call(vars, k) ? vars[k] : m,
    );
  }
  if (Array.isArray(value)) return value.map((v) => substitute(v, vars));
  if (value && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([k, v]) => [k, substitute(v, vars)]),
    );
  }
  return value;
}

export function validateScript(script) {
  if (!Array.isArray(script) || script.length === 0) {
    throw new Error("script must be a non-empty JSON array of turns");
  }
  script.forEach((t, i) => {
    const hasTool = !!(t && typeof t === "object" && t.tool_call);
    const hasText = !!(t && typeof t === "object" && typeof t.text === "string");
    if (hasTool === hasText) {
      throw new Error(`turn ${i + 1}: exactly one of tool_call or text`);
    }
    if (hasTool && (typeof t.tool_call.name !== "string" || !t.tool_call.arguments)) {
      throw new Error(`turn ${i + 1}: tool_call needs name and arguments`);
    }
  });
}

function sse(res, events) {
  res.writeHead(200, {
    "content-type": "text/event-stream",
    "cache-control": "no-cache",
    connection: "keep-alive",
  });
  for (const [event, data] of events) {
    res.write(`event: ${event}\ndata: ${JSON.stringify(data)}\n\n`);
  }
  res.end();
}

function json(res, status, body) {
  const payload = JSON.stringify(body);
  res.writeHead(status, {
    "content-type": "application/json",
    "content-length": Buffer.byteLength(payload),
  });
  res.end(payload);
}

function splitJson(s) {
  const mid = Math.max(1, Math.floor(s.length / 2));
  return s.length > 1 ? [s.slice(0, mid), s.slice(mid)] : [s];
}

const anthropic = {
  isModelRequest: (method, path) =>
    method === "POST" && path === "/v1/messages",
  isMain: (body) => Array.isArray(body.tools) && body.tools.length > 0,
  reply(res, body, turn, id) {
    const model = body.model || "mock-model";
    const isTool = !!turn.tool_call;
    const callId = `toolu_mock_${id}`;
    const usage = { input_tokens: 1, output_tokens: 1 };
    const stop = isTool ? "tool_use" : "end_turn";
    const block = isTool
      ? { type: "tool_use", id: callId, name: turn.tool_call.name, input: {} }
      : { type: "text", text: "" };
    if (!body.stream) {
      const content = isTool
        ? [{ ...block, input: turn.tool_call.arguments }]
        : [{ type: "text", text: turn.text }];
      return json(res, 200, {
        id: `msg_mock_${id}`,
        type: "message",
        role: "assistant",
        model,
        content,
        stop_reason: stop,
        stop_sequence: null,
        usage,
      });
    }
    const delta = isTool
      ? splitJson(JSON.stringify(turn.tool_call.arguments)).map((p) => ({
          type: "input_json_delta",
          partial_json: p,
        }))
      : [{ type: "text_delta", text: turn.text }];
    const events = [
      [
        "message_start",
        {
          type: "message_start",
          message: {
            id: `msg_mock_${id}`,
            type: "message",
            role: "assistant",
            model,
            content: [],
            stop_reason: null,
            stop_sequence: null,
            usage: { input_tokens: 1, output_tokens: 0 },
          },
        },
      ],
      [
        "content_block_start",
        { type: "content_block_start", index: 0, content_block: block },
      ],
      ["ping", { type: "ping" }],
      ...delta.map((d) => [
        "content_block_delta",
        { type: "content_block_delta", index: 0, delta: d },
      ]),
      ["content_block_stop", { type: "content_block_stop", index: 0 }],
      [
        "message_delta",
        {
          type: "message_delta",
          delta: { stop_reason: stop, stop_sequence: null },
          usage: { output_tokens: 1 },
        },
      ],
      ["message_stop", { type: "message_stop" }],
    ];
    return sse(res, events);
  },
  harmless(res, body, id) {
    return this.reply(res, body, { text: '{"title":"mock"}' }, `aux${id}`);
  },
  other(method, path, res) {
    if (path.startsWith("/v1/messages/count_tokens")) {
      return json(res, 200, { input_tokens: 1 });
    }
    if (path.startsWith("/v1/models")) {
      return json(res, 200, {
        data: [{ id: "mock-model", type: "model", display_name: "Mock" }],
        has_more: false,
        first_id: "mock-model",
        last_id: "mock-model",
      });
    }
    return json(res, 200, {});
  },
};

export const PROTOCOLS = {
  anthropic,
  "openai-chat": null,
  "openai-responses": (await import("./protocol-openai-responses.mjs")).default,
};

export function createServer({ protocol, script, vars = {}, logPath }) {
  if (!Object.prototype.hasOwnProperty.call(PROTOCOLS, protocol)) {
    throw new Error(`unknown protocol "${protocol}"`);
  }
  const proto = PROTOCOLS[protocol];
  if (!proto) throw new NotImplementedError(protocol);
  validateScript(script);
  const turns = script.map((t) => substitute(t, vars));
  let next = 0;
  let index = 0;
  const append = (entry) => {
    if (logPath) fs.appendFileSync(logPath, JSON.stringify(entry) + "\n");
  };
  return http.createServer((req, res) => {
    const chunks = [];
    req.on("data", (c) => chunks.push(c));
    req.on("end", () => {
      const path = req.url.split("?")[0];
      const raw = Buffer.concat(chunks).toString("utf8");
      let body = {};
      try {
        body = raw ? JSON.parse(raw) : {};
      } catch {
        body = { _unparsed: raw };
      }
      if (!proto.isModelRequest(req.method, path)) {
        return proto.other(req.method, path, res);
      }
      index += 1;
      if (!proto.isMain(body)) {
        append({ index, turn: null, kind: "auxiliary", path, request: body });
        return proto.harmless(res, body, index);
      }
      const exhausted = next >= turns.length;
      const turn = exhausted ? { text: "script exhausted" } : turns[next];
      next += 1;
      append({
        index,
        turn: next,
        kind: exhausted ? "exhausted" : "scripted",
        path,
        request: body,
      });
      return proto.reply(res, body, turn, index);
    });
  });
}

export function parseArgs(argv) {
  const opts = { vars: {}, port: 0 };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    const val = () => {
      if (i + 1 >= argv.length) throw new Error(`missing value for ${a}`);
      return argv[++i];
    };
    if (a === "--protocol") opts.protocol = val();
    else if (a === "--script") opts.script = val();
    else if (a === "--log") opts.log = val();
    else if (a === "--port") opts.port = Number(val());
    else if (a === "--var") {
      const kv = val();
      const eq = kv.indexOf("=");
      if (eq < 1) throw new Error(`--var expects name=value, got "${kv}"`);
      opts.vars[kv.slice(0, eq)] = kv.slice(eq + 1);
    } else throw new Error(`unknown argument ${a}`);
  }
  for (const r of ["protocol", "script", "log"]) {
    if (!opts[r]) throw new Error(`--${r} is required`);
  }
  if (!Number.isInteger(opts.port) || opts.port < 0) {
    throw new Error("--port must be a non-negative integer");
  }
  return opts;
}

function main() {
  let server;
  try {
    const opts = parseArgs(process.argv.slice(2));
    const script = JSON.parse(fs.readFileSync(opts.script, "utf8"));
    fs.writeFileSync(opts.log, "");
    server = createServer({
      protocol: opts.protocol,
      script,
      vars: opts.vars,
      logPath: opts.log,
    });
    server.listen(opts.port, "0.0.0.0", () => {
      process.stdout.write(`PORT=${server.address().port}\n`);
    });
    for (const sig of ["SIGINT", "SIGTERM"]) {
      process.on(sig, () => server.close(() => process.exit(0)));
    }
  } catch (e) {
    process.stderr.write(`mock-model: ${e.message}\n`);
    process.exit(e instanceof NotImplementedError ? 3 : 2);
  }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) main();
