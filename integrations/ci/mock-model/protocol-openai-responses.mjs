function sse(res, events) {
  res.writeHead(200, {
    "content-type": "text/event-stream",
    "cache-control": "no-cache",
    connection: "keep-alive",
  });
  let seq = 0;
  for (const data of events) {
    const withSeq = { ...data, sequence_number: seq++ };
    res.write(`event: ${data.type}\ndata: ${JSON.stringify(withSeq)}\n\n`);
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

function splitString(s) {
  const mid = Math.max(1, Math.floor(s.length / 2));
  return s.length > 1 ? [s.slice(0, mid), s.slice(mid)] : [s];
}

const USAGE = {
  input_tokens: 1,
  input_tokens_details: { cached_tokens: 0 },
  output_tokens: 1,
  output_tokens_details: { reasoning_tokens: 0 },
  total_tokens: 2,
};

function envelope(id, model, status, output) {
  return {
    id: `resp_mock_${id}`,
    object: "response",
    created_at: 0,
    status,
    model,
    output,
    usage: status === "completed" ? USAGE : null,
  };
}

function items(turn, id) {
  if (turn.tool_call) {
    const args = JSON.stringify(turn.tool_call.arguments);
    const base = {
      type: "function_call",
      id: `fc_mock_${id}`,
      call_id: `call_mock_${id}`,
      name: turn.tool_call.name,
    };
    if (turn.tool_call.namespace) base.namespace = turn.tool_call.namespace;
    return {
      added: { ...base, arguments: "", status: "in_progress" },
      done: { ...base, arguments: args, status: "completed" },
      args,
    };
  }
  const part = (text) => ({ type: "output_text", text, annotations: [] });
  return {
    added: {
      type: "message",
      id: `msg_mock_${id}`,
      role: "assistant",
      status: "in_progress",
      content: [],
    },
    done: {
      type: "message",
      id: `msg_mock_${id}`,
      role: "assistant",
      status: "completed",
      content: [part(turn.text)],
    },
    text: turn.text,
  };
}

export default {
  isModelRequest: (method, path) =>
    method === "POST" && /^\/v1\/responses\/?$/.test(path),
  isMain: (body) => Array.isArray(body.tools) && body.tools.length > 0,
  reply(res, body, turn, id) {
    const model = body.model || "mock-model";
    const it = items(turn, id);
    if (!body.stream) {
      return json(res, 200, envelope(id, model, "completed", [it.done]));
    }
    const events = [
      { type: "response.created", response: envelope(id, model, "in_progress", []) },
      { type: "response.in_progress", response: envelope(id, model, "in_progress", []) },
      { type: "response.output_item.added", output_index: 0, item: it.added },
    ];
    if (turn.tool_call) {
      for (const delta of splitString(it.args)) {
        events.push({
          type: "response.function_call_arguments.delta",
          item_id: it.done.id,
          output_index: 0,
          delta,
        });
      }
      events.push({
        type: "response.function_call_arguments.done",
        item_id: it.done.id,
        output_index: 0,
        arguments: it.args,
      });
    } else {
      const empty = { type: "output_text", text: "", annotations: [] };
      events.push({
        type: "response.content_part.added",
        item_id: it.done.id,
        output_index: 0,
        content_index: 0,
        part: empty,
      });
      for (const delta of splitString(it.text)) {
        events.push({
          type: "response.output_text.delta",
          item_id: it.done.id,
          output_index: 0,
          content_index: 0,
          delta,
        });
      }
      events.push(
        {
          type: "response.output_text.done",
          item_id: it.done.id,
          output_index: 0,
          content_index: 0,
          text: it.text,
        },
        {
          type: "response.content_part.done",
          item_id: it.done.id,
          output_index: 0,
          content_index: 0,
          part: it.done.content[0],
        },
      );
    }
    events.push(
      { type: "response.output_item.done", output_index: 0, item: it.done },
      { type: "response.completed", response: envelope(id, model, "completed", [it.done]) },
    );
    return sse(res, events);
  },
  harmless(res, body, id) {
    return this.reply(res, body, { text: '{"title":"mock"}' }, `aux${id}`);
  },
  other(method, path, res) {
    if (path.startsWith("/v1/models")) {
      return json(res, 200, {
        object: "list",
        data: [{ id: "mock-model", object: "model", owned_by: "mock" }],
        models: [],
      });
    }
    return json(res, 200, {});
  },
};
