export interface Position {
  line: number;
  character: number;
}

export interface Range {
  start: Position;
  end: Position;
}

export interface LspDiagnostic {
  range: Range;
  severity?: number;
  code?: string | number;
  source?: string;
  message: string;
}

const SEVERITY = ["", "error", "warning", "information", "hint"];

function snippet(source: string[], range: Range): string[] {
  const { start, end } = range;
  const text = source[start.line];
  if (text === undefined) return [];
  const sameLine = end.line === start.line;
  const stop = sameLine ? end.character : text.length;
  const width = Math.max(1, stop - start.character);
  const number = String(start.line + 1);
  const pad = text.slice(0, start.character).replace(/[^\t]/g, " ");
  return [`    ${number} | ${text}`, `    ${" ".repeat(number.length)} | ${pad}${"^".repeat(width)}`];
}

export function formatDiagnostics(
  path: string,
  diagnostics: LspDiagnostic[],
  limit = 20,
  source?: string,
): string {
  if (diagnostics.length === 0) return `marigold: no diagnostics in ${path}`;
  const sourceLines = source?.split(/\r?\n/);
  const noun = diagnostics.length === 1 ? "diagnostic" : "diagnostics";
  const lines = [`marigold: ${diagnostics.length} ${noun} in ${path}`];
  for (const d of diagnostics.slice(0, limit)) {
    const { start, end } = d.range;
    const severity = SEVERITY[d.severity ?? 1] || "error";
    const code = d.code === undefined ? "" : `[${d.code}]`;
    const message = d.message.split("\n").join("\n    ");
    lines.push(`${path}:${start.line + 1}:${start.character + 1}: ${severity}${code}: ${message}`);
    lines.push(`    range ${start.line + 1}:${start.character + 1}-${end.line + 1}:${end.character + 1}`);
    if (sourceLines) lines.push(...snippet(sourceLines, d.range));
  }
  if (diagnostics.length > limit) {
    lines.push(`(${diagnostics.length - limit} more not shown; call marigold_check for the full list)`);
  }
  return lines.join("\n");
}

export interface LspLocation {
  uri: string;
  range: Range;
}

export interface LspTextEdit {
  range: Range;
  newText: string;
}

export interface LspWorkspaceEdit {
  changes?: Record<string, LspTextEdit[]>;
  documentChanges?: { textDocument: { uri: string }; edits: LspTextEdit[] }[];
}

export type PathOf = (uri: string) => string;

const SYMBOL_KINDS: Record<number, string> = {
  2: "module",
  5: "class",
  6: "method",
  8: "field",
  10: "enum",
  11: "interface",
  12: "function",
  13: "variable",
  14: "constant",
  22: "enum-member",
  23: "struct",
};

const at = (p: Position) => `${p.line + 1}:${p.character + 1}`;

const plural = (n: number, noun: string) => `${n} ${noun}${n === 1 ? "" : "s"}`;

export function locationsOf(result: unknown): LspLocation[] {
  const items = Array.isArray(result) ? result : result ? [result] : [];
  const out: LspLocation[] = [];
  for (const item of items as any[]) {
    if (item?.uri && item.range) out.push({ uri: item.uri, range: item.range });
    else if (item?.targetUri) out.push({ uri: item.targetUri, range: item.targetSelectionRange ?? item.targetRange });
  }
  return out;
}

export function formatLocations(
  noun: string,
  locations: LspLocation[],
  show: PathOf,
  sources: Map<string, string>,
  limit = 50,
): string {
  const lines = [`marigold: ${plural(locations.length, noun)}`];
  const split = new Map<string, string[]>();
  for (const loc of locations.slice(0, limit)) {
    lines.push(`${show(loc.uri)}:${at(loc.range.start)}`);
    const source = sources.get(loc.uri);
    if (source === undefined) continue;
    if (!split.has(loc.uri)) split.set(loc.uri, source.split(/\r?\n/));
    lines.push(...snippet(split.get(loc.uri)!, loc.range));
  }
  if (locations.length > limit) lines.push(`(${locations.length - limit} more not shown)`);
  return lines.join("\n");
}

export interface FlatSymbol {
  name: string;
  kind: number;
  uri?: string;
  position: Position;
}

export function symbolsOf(result: unknown, defaultUri?: string): FlatSymbol[] {
  const out: FlatSymbol[] = [];
  const visit = (item: any) => {
    if (!item || typeof item.name !== "string") return;
    if (item.location) out.push({ name: item.name, kind: item.kind, uri: item.location.uri, position: item.location.range.start });
    else if (item.range) out.push({ name: item.name, kind: item.kind, uri: defaultUri, position: (item.selectionRange ?? item.range).start });
    for (const child of item.children ?? []) visit(child);
  };
  if (Array.isArray(result)) result.forEach(visit);
  return out;
}

export function formatSymbols(header: string, symbols: FlatSymbol[], show: PathOf, limit = 100): string {
  const lines = [`marigold: ${header}`];
  for (const s of symbols.slice(0, limit)) {
    const path = s.uri === undefined ? "" : `${show(s.uri)}:`;
    lines.push(`${path}${at(s.position)}: ${SYMBOL_KINDS[s.kind] ?? "symbol"} ${s.name}`);
  }
  if (symbols.length > limit) lines.push(`(${symbols.length - limit} more not shown; narrow the query)`);
  return lines.join("\n");
}

export function hoverText(result: unknown): string {
  const contents = (result as { contents?: unknown } | null)?.contents;
  const parts = Array.isArray(contents) ? contents : contents === undefined ? [] : [contents];
  return parts
    .map((part: any) => (typeof part === "string" ? part : part?.language ? `\`\`\`${part.language}\n${part.value}\n\`\`\`` : (part?.value ?? "")))
    .filter((part) => part !== "")
    .join("\n\n");
}

export function editsOf(edit: LspWorkspaceEdit | null | undefined): Map<string, LspTextEdit[]> {
  const out = new Map<string, LspTextEdit[]>();
  const add = (uri: string, edits: LspTextEdit[]) => {
    if (edits.length > 0) out.set(uri, [...(out.get(uri) ?? []), ...edits]);
  };
  for (const [uri, edits] of Object.entries(edit?.changes ?? {})) add(uri, edits);
  for (const change of edit?.documentChanges ?? []) add(change.textDocument.uri, change.edits);
  return out;
}

function lineStarts(text: string): number[] {
  const starts = [0];
  for (let i = text.indexOf("\n"); i >= 0; i = text.indexOf("\n", i + 1)) starts.push(i + 1);
  return starts;
}

function offsetIn(text: string, starts: number[], p: Position): number {
  if (p.line < 0 || p.line >= starts.length || p.character < 0) {
    throw new Error(`edit position ${at(p)} is outside the file`);
  }
  const start = starts[p.line];
  let end = p.line + 1 < starts.length ? starts[p.line + 1] - 1 : text.length;
  if (end > start && text[end - 1] === "\r") end--;
  return Math.min(start + p.character, end);
}

export function applyEdits(text: string, edits: LspTextEdit[]): string {
  const starts = lineStarts(text);
  const spans = edits
    .map((e) => ({ from: offsetIn(text, starts, e.range.start), to: offsetIn(text, starts, e.range.end), text: e.newText }))
    .sort((a, b) => b.from - a.from || b.to - a.to);
  let out = text;
  let limit = text.length;
  for (const span of spans) {
    if (span.to < span.from || span.to > limit) throw new Error("rename edits overlap or are out of order");
    out = out.slice(0, span.from) + span.text + out.slice(span.to);
    limit = span.from;
  }
  return out;
}

export function formatRename(
  edits: Map<string, LspTextEdit[]>,
  show: PathOf,
  sources: Map<string, string>,
  applied: boolean,
): string {
  const total = [...edits.values()].reduce((n, e) => n + e.length, 0);
  const summary = `${plural(total, "edit")} in ${plural(edits.size, "file")}`;
  const lines = [
    applied
      ? `marigold: applied ${summary}`
      : `marigold: rename would make ${summary} (not applied; call marigold_rename again with apply=true to write them)`,
  ];
  for (const [uri, list] of edits) {
    lines.push(show(uri));
    const source = sources.get(uri);
    const starts = source === undefined ? [] : lineStarts(source);
    for (const e of [...list].sort((a, b) => a.range.start.line - b.range.start.line || a.range.start.character - b.range.start.character)) {
      let old = "";
      if (source !== undefined) {
        try {
          old = `"${source.slice(offsetIn(source, starts, e.range.start), offsetIn(source, starts, e.range.end))}" -> `;
        } catch {}
      }
      lines.push(`  ${at(e.range.start)}-${at(e.range.end)} ${old}"${e.newText}"`);
    }
  }
  return lines.join("\n");
}
