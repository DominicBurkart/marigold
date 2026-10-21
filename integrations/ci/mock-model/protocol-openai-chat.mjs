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

const USAGE = { prompt_tokens: 1, completion_tokens: 1, total_tokens: 2 };

function base(id, model, created) {
  return { id: `chatcmpl-mock-${id}`, object: "chat.completion.chunk", created, model };
}

export default {
  isModelRequest: (method, path) =>
    method === "POST" &&
    (path === "/v1/chat/completions" || path === "/chat/completions"),
  isMain: (body) => Array.isArray(body.tools) && body.tools.length > 0,
  reply(res, body, turn, id) {
    const model = body.model || "mock-model";
    const created = Math.floor(Date.now() / 1000);
    const isTool = !!turn.tool_call;
    const callId = `call_mock_${id}`;
    const finish = isTool ? "tool_calls" : "stop";
    const args = isTool ? JSON.stringify(turn.tool_call.arguments) : "";
    if (!body.stream) {
      const message = isTool
        ? {
            role: "assistant",
            content: null,
            tool_calls: [
              {
                id: callId,
                type: "function",
                function: { name: turn.tool_call.name, arguments: args },
              },
            ],
          }
        : { role: "assistant", content: turn.text };
      return json(res, 200, {
        id: `chatcmpl-mock-${id}`,
        object: "chat.completion",
        created,
        model,
        choices: [{ index: 0, message, finish_reason: finish }],
        usage: USAGE,
      });
    }
    const chunk = (delta, finish_reason = null) => ({
      ...base(id, model, created),
      choices: [{ index: 0, delta, finish_reason }],
    });
    const chunks = [chunk({ role: "assistant", content: isTool ? null : "" })];
    if (isTool) {
      chunks.push(
        chunk({
          tool_calls: [
            {
              index: 0,
              id: callId,
              type: "function",
              function: { name: turn.tool_call.name, arguments: "" },
            },
          ],
        }),
      );
      for (const p of splitJson(args)) {
        chunks.push(
          chunk({ tool_calls: [{ index: 0, function: { arguments: p } }] }),
        );
      }
    } else {
      chunks.push(chunk({ content: turn.text }));
    }
    chunks.push(chunk({}, finish));
    chunks.push({ ...base(id, model, created), choices: [], usage: USAGE });
    res.writeHead(200, {
      "content-type": "text/event-stream",
      "cache-control": "no-cache",
      connection: "keep-alive",
    });
    for (const c of chunks) res.write(`data: ${JSON.stringify(c)}\n\n`);
    res.write("data: [DONE]\n\n");
    res.end();
  },
  harmless(res, body, id) {
    return this.reply(res, body, { text: '{"title":"mock"}' }, `aux${id}`);
  },
  other(method, path, res) {
    if (method === "GET" && path.replace(/\/$/, "").endsWith("/models")) {
      return json(res, 200, {
        object: "list",
        data: [
          { id: "mock-model", object: "model", created: 0, owned_by: "mock" },
        ],
      });
    }
    return json(res, 200, {});
  },
};
