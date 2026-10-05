import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import type { LspDiagnostic } from "./format.ts";

interface Pending {
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
}

interface Waiter {
  version: number;
  resolve: (diagnostics: LspDiagnostic[]) => void;
}

export class LspClient {
  private buffer = Buffer.alloc(0);
  private nextId = 0;
  private pending = new Map<number, Pending>();
  private waiters = new Map<string, Waiter>();
  private versions = new Map<string, number>();
  private closed: Promise<void>;
  private stopped = false;

  private constructor(
    private readonly child: ChildProcessWithoutNullStreams,
    private readonly timeoutMs: number,
  ) {
    child.stdout.on("data", (chunk: Buffer) => this.onData(chunk));
    child.stderr.resume();
    this.closed = new Promise((resolve) => child.once("close", () => resolve()));
    child.once("close", () => {
      this.stopped = true;
      for (const p of this.pending.values()) p.reject(new Error("marigold-lsp exited"));
      this.pending.clear();
    });
  }

  static async start(command: string, args: string[], rootUri: string, timeoutMs = 10_000): Promise<LspClient> {
    const child = spawn(command, args, { stdio: ["pipe", "pipe", "pipe"] });
    await new Promise<void>((resolve, reject) => {
      child.once("spawn", resolve);
      child.once("error", (e) => reject(new Error(`failed to start marigold-lsp (${command}): ${e.message}`)));
    });
    const client = new LspClient(child, timeoutMs);
    await client.request("initialize", { processId: process.pid, rootUri, capabilities: {} });
    client.notify("initialized", {});
    return client;
  }

  async check(uri: string, text: string): Promise<LspDiagnostic[]> {
    const previous = this.versions.get(uri);
    const version = (previous ?? 0) + 1;
    this.versions.set(uri, version);
    const published = new Promise<LspDiagnostic[]>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.waiters.delete(uri);
        reject(new Error(`marigold-lsp did not publish diagnostics for ${uri} within ${this.timeoutMs}ms`));
      }, this.timeoutMs);
      this.waiters.set(uri, {
        version,
        resolve: (diagnostics) => {
          clearTimeout(timer);
          resolve(diagnostics);
        },
      });
    });
    if (previous === undefined) {
      this.notify("textDocument/didOpen", {
        textDocument: { uri, languageId: "marigold", version, text },
      });
    } else {
      this.notify("textDocument/didChange", {
        textDocument: { uri, version },
        contentChanges: [{ text }],
      });
    }
    return published;
  }

  async shutdown(): Promise<void> {
    if (this.stopped) return;
    this.stopped = true;
    try {
      await this.request("shutdown", null);
      this.notify("exit", null);
    } catch {
      this.child.kill();
    }
    await this.closed;
  }

  private request(method: string, params: unknown): Promise<unknown> {
    const id = ++this.nextId;
    const result = new Promise((resolve, reject) => this.pending.set(id, { resolve, reject }));
    this.send({ jsonrpc: "2.0", id, method, params });
    return result;
  }

  private notify(method: string, params: unknown): void {
    this.send({ jsonrpc: "2.0", method, params });
  }

  private send(message: unknown): void {
    const body = Buffer.from(JSON.stringify(message), "utf8");
    this.child.stdin.write(`Content-Length: ${body.length}\r\n\r\n`);
    this.child.stdin.write(body);
  }

  private onData(chunk: Buffer): void {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    for (;;) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd < 0) return;
      const match = /Content-Length: (\d+)/i.exec(this.buffer.subarray(0, headerEnd).toString("ascii"));
      if (!match) throw new Error("marigold-lsp sent a message without Content-Length");
      const start = headerEnd + 4;
      const end = start + Number(match[1]);
      if (this.buffer.length < end) return;
      const message = JSON.parse(this.buffer.subarray(start, end).toString("utf8"));
      this.buffer = this.buffer.subarray(end);
      this.dispatch(message);
    }
  }

  private dispatch(message: any): void {
    if (message.id !== undefined && message.method === undefined) {
      const pending = this.pending.get(message.id);
      this.pending.delete(message.id);
      if (message.error) pending?.reject(new Error(message.error.message));
      else pending?.resolve(message.result);
      return;
    }
    if (message.method === "textDocument/publishDiagnostics") {
      const { uri, version, diagnostics } = message.params;
      const waiter = this.waiters.get(uri);
      if (waiter && (version === undefined || version === null || version >= waiter.version)) {
        this.waiters.delete(uri);
        waiter.resolve(diagnostics);
      }
    }
  }
}
