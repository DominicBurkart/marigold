import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import type { LspDiagnostic } from "./format.ts";

const MAX_BUFFER = 16 * 1024 * 1024;
const MAX_HEADER = 8 * 1024;
const STDERR_TAIL = 4096;

interface Pending {
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
}

interface Waiter {
  version: number;
  resolve: (diagnostics: LspDiagnostic[]) => void;
  reject: (error: Error) => void;
}

export class LspClient {
  private buffer = Buffer.alloc(0);
  private nextId = 0;
  private pending = new Map<number, Pending>();
  private waiters = new Map<string, Waiter>();
  private versions = new Map<string, number>();
  private queues = new Map<string, Promise<unknown>>();
  private closed: Promise<void>;
  private exited = false;
  private stopping = false;
  private killed = false;
  private stderr = "";

  private constructor(
    private readonly child: ChildProcessWithoutNullStreams,
    private readonly timeoutMs: number,
  ) {
    child.stdout.on("data", (chunk: Buffer) => this.onData(chunk));
    child.stdin.on("error", () => {});
    child.stdout.on("error", () => {});
    child.on("error", () => {});
    child.stderr.on("data", (chunk: Buffer) => {
      this.stderr = (this.stderr + chunk.toString("utf8")).slice(-STDERR_TAIL);
    });
    this.closed = new Promise((resolve) => child.once("close", () => resolve()));
    child.once("close", () => {
      this.exited = true;
      this.failAll(this.error("marigold-lsp exited"));
    });
  }

  static async start(command: string, args: string[], rootUri: string, timeoutMs = 10_000): Promise<LspClient> {
    const child = spawn(command, args, { stdio: ["pipe", "pipe", "pipe"] });
    await new Promise<void>((resolve, reject) => {
      child.once("spawn", resolve);
      child.once("error", (e) => reject(new Error(`failed to start marigold-lsp (${command}): ${e.message}`)));
    });
    const client = new LspClient(child, timeoutMs);
    try {
      await client.request("initialize", { processId: process.pid, rootUri, capabilities: {} }, timeoutMs);
    } catch (e) {
      client.kill();
      await client.closed;
      throw e;
    }
    client.notify("initialized", {});
    return client;
  }

  get stopped(): boolean {
    return this.exited || this.stopping;
  }

  stderrTail(): string {
    return this.stderr;
  }

  check(uri: string, text: string): Promise<LspDiagnostic[]> {
    const previous = this.queues.get(uri) ?? Promise.resolve();
    const run = previous.then(() => this.checkNow(uri, text));
    const tail = run.catch(() => {});
    this.queues.set(uri, tail);
    void tail.then(() => {
      if (this.queues.get(uri) === tail) this.queues.delete(uri);
    });
    return run;
  }

  async shutdown(): Promise<void> {
    if (this.exited) return;
    if (this.stopping) {
      await this.closed;
      return;
    }
    this.stopping = true;
    try {
      await this.request("shutdown", null, this.timeoutMs);
      this.notify("exit", null);
    } catch {
      this.kill();
    }
    await this.closed;
  }

  private async checkNow(uri: string, text: string): Promise<LspDiagnostic[]> {
    if (this.stopped) throw this.error("marigold-lsp exited");
    const previous = this.versions.get(uri);
    const version = (previous ?? 0) + 1;
    this.versions.set(uri, version);
    const published = new Promise<LspDiagnostic[]>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.waiters.delete(uri);
        const error = this.error(`marigold-lsp did not publish diagnostics for ${uri} within ${this.timeoutMs}ms`);
        this.kill();
        reject(error);
      }, this.timeoutMs);
      this.waiters.set(uri, {
        version,
        resolve: (diagnostics) => {
          clearTimeout(timer);
          resolve(diagnostics);
        },
        reject: (error) => {
          clearTimeout(timer);
          reject(error);
        },
      });
    });
    try {
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
    } catch (e) {
      this.waiters.get(uri)?.reject(e as Error);
      this.waiters.delete(uri);
    }
    return published;
  }

  private error(message: string): Error {
    const tail = this.stderr.trim();
    return new Error(tail ? `${message}; server stderr: ${tail}` : message);
  }

  private kill(): void {
    this.stopping = true;
    this.killed = true;
    this.child.kill();
  }

  private failAll(error: Error): void {
    const pending = [...this.pending.values()];
    const waiters = [...this.waiters.values()];
    this.pending.clear();
    this.waiters.clear();
    for (const p of pending) p.reject(error);
    for (const w of waiters) w.reject(error);
  }

  private request(method: string, params: unknown, timeoutMs?: number): Promise<unknown> {
    const id = ++this.nextId;
    const result = new Promise((resolve, reject) => {
      const timer =
        timeoutMs === undefined
          ? undefined
          : setTimeout(() => {
              this.pending.delete(id);
              reject(this.error(`marigold-lsp did not answer ${method} within ${timeoutMs}ms`));
            }, timeoutMs);
      this.pending.set(id, {
        resolve: (value) => {
          clearTimeout(timer);
          resolve(value);
        },
        reject: (error) => {
          clearTimeout(timer);
          reject(error);
        },
      });
    });
    try {
      this.send({ jsonrpc: "2.0", id, method, params });
    } catch (e) {
      this.pending.delete(id);
      return Promise.reject(e);
    }
    return result;
  }

  private notify(method: string, params: unknown): void {
    this.send({ jsonrpc: "2.0", method, params });
  }

  private send(message: unknown): void {
    if (this.exited || this.child.stdin.destroyed || !this.child.stdin.writable) {
      throw this.error("marigold-lsp exited");
    }
    const body = Buffer.from(JSON.stringify(message), "utf8");
    this.child.stdin.write(Buffer.concat([Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "ascii"), body]));
  }

  private protocolError(message: string): void {
    const error = this.error(`marigold-lsp sent a malformed message: ${message}`);
    this.buffer = Buffer.alloc(0);
    this.kill();
    this.failAll(error);
  }

  private onData(chunk: Buffer): void {
    if (this.killed) return;
    this.buffer = Buffer.concat([this.buffer, chunk]);
    if (this.buffer.length > MAX_BUFFER) {
      this.protocolError("receive buffer exceeded 16MB");
      return;
    }
    for (;;) {
      const headerEnd = this.buffer.indexOf("\r\n\r\n");
      if (headerEnd < 0) {
        if (this.buffer.length > MAX_HEADER) this.protocolError("oversized header");
        return;
      }
      const match = /Content-Length: (\d+)/i.exec(this.buffer.subarray(0, headerEnd).toString("ascii"));
      if (!match) {
        this.protocolError("missing Content-Length");
        return;
      }
      const length = Number(match[1]);
      if (length > MAX_BUFFER) {
        this.protocolError("message exceeds 16MB buffer limit");
        return;
      }
      const start = headerEnd + 4;
      const end = start + length;
      if (this.buffer.length < end) return;
      let message: any;
      try {
        message = JSON.parse(this.buffer.subarray(start, end).toString("utf8"));
      } catch {
        this.protocolError("invalid JSON");
        return;
      }
      this.buffer = this.buffer.subarray(end);
      try {
        this.dispatch(message);
      } catch (e) {
        this.protocolError((e as Error).message);
        return;
      }
    }
  }

  private dispatch(message: any): void {
    if (message === null || typeof message !== "object") throw new Error("not an object");
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
