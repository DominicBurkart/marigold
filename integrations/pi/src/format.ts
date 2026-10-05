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

export function formatDiagnostics(path: string, diagnostics: LspDiagnostic[], limit = 20): string {
  if (diagnostics.length === 0) return `marigold: ${path} has no diagnostics`;
  const noun = diagnostics.length === 1 ? "diagnostic" : "diagnostics";
  const lines = [`marigold: ${diagnostics.length} ${noun} in ${path}`];
  for (const d of diagnostics.slice(0, limit)) {
    const { line, character } = d.range.start;
    const severity = SEVERITY[d.severity ?? 1] ?? "error";
    const code = d.code === undefined ? "" : `[${d.code}]`;
    const message = d.message.split("\n").join("\n    ");
    lines.push(`${path}:${line + 1}:${character + 1}: ${severity}${code}: ${message}`);
  }
  if (diagnostics.length > limit) {
    lines.push(`(${diagnostics.length - limit} more not shown; call marigold_check for the full list)`);
  }
  return lines.join("\n");
}
