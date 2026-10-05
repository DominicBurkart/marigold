import { existsSync, writeFileSync } from "node:fs";

const [mode, marker] = process.argv.slice(2);
let buffer = Buffer.alloc(0);

const send = (message) => {
  const body = Buffer.from(JSON.stringify(message));
  process.stdout.write(`Content-Length: ${body.length}\r\n\r\n`);
  process.stdout.write(body);
};

const handle = (message) => {
  if (message.method === "initialize") {
    if (mode === "silent") return;
    send({ jsonrpc: "2.0", id: message.id, result: { capabilities: {} } });
  } else if (message.method === "shutdown") {
    send({ jsonrpc: "2.0", id: message.id, result: null });
  } else if (message.method === "exit") {
    process.exit(0);
  } else if (message.method === "textDocument/didOpen" || message.method === "textDocument/didChange") {
    const uri = message.params.textDocument.uri;
    const version = message.params.textDocument.version;
    if (mode === "no-publish") return;
    if (mode === "garbage") return void process.stdout.write("this is not an lsp frame\r\n\r\n");
    if (mode === "badjson") return void process.stdout.write("Content-Length: 5\r\n\r\n{nope");
    if (mode === "flood") {
      process.stdout.write("Content-Length: 999999999\r\n\r\n");
      const chunk = Buffer.alloc(1024 * 1024, "a");
      const pump = () => {
        while (process.stdout.write(chunk));
        process.stdout.once("drain", pump);
      };
      return pump();
    }
    if (mode === "crash" || (mode === "die-once" && !existsSync(marker))) {
      if (mode === "die-once") writeFileSync(marker, "x");
      process.stderr.write("boom: fake server crashed");
      process.exit(3);
    }
    setTimeout(() => {
      send({ jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: { uri, version, diagnostics: [] } });
    }, mode === "delayed" ? 150 : 0);
  }
};

process.stdin.on("data", (chunk) => {
  buffer = Buffer.concat([buffer, chunk]);
  for (;;) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd < 0) return;
    const length = Number(/Content-Length: (\d+)/i.exec(buffer.subarray(0, headerEnd).toString())[1]);
    const start = headerEnd + 4;
    if (buffer.length < start + length) return;
    const message = JSON.parse(buffer.subarray(start, start + length).toString());
    buffer = buffer.subarray(start + length);
    handle(message);
  }
});
process.stdout.on("error", () => {});
