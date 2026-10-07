export interface Position {
  line: number;
  character: number;
}

export interface LspDiagnostic {
  range: { start: Position; end: Position };
  severity?: number;
  code?: string | number;
  source?: string;
  message: string;
}

const SEVERITY = ["", "error", "warning", "info", "hint"];

function snippet(source: string[], d: LspDiagnostic): string[] {
  const { start, end } = d.range;
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
    if (sourceLines) lines.push(...snippet(sourceLines, d));
  }
  if (diagnostics.length > limit) {
    lines.push(`(${diagnostics.length - limit} more not shown; call marigold_check for the full list)`);
  }
  return lines.join("\n");
}
