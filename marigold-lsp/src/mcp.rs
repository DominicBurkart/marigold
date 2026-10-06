//! Model Context Protocol server over newline-delimited JSON-RPC 2.0.
//!
//! Implements MCP revision 2025-11-25 (stdio transport, `initialize` handshake,
//! `tools/list`, `tools/call`) as a thin adapter over the same functions the
//! language server uses. All tools are read-only. Lines and columns are
//! 1-based and counted in characters.
//!
//! The server is deliberately lenient about lifecycle: because every tool is
//! stateless and read-only, `tools/list` and `tools/call` are answered even
//! before `initialize` and between `initialize` and
//! `notifications/initialized`. Blank lines are ignored without a response;
//! syntactically invalid JSON gets -32700 and valid JSON that is not a request
//! object gets -32600, both with a null id.
//!
//! ```
//! use std::io::Cursor;
//!
//! let request = r#"{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"marigold_check","arguments":{"source":"range(Colour).return"}}}"#;
//! let mut out = Vec::new();
//! marigold_lsp::mcp::serve_mcp(Cursor::new(format!("{request}\n")), &mut out).unwrap();
//! let response: serde_json::Value = serde_json::from_slice(&out).unwrap();
//! assert_eq!(response["id"], 1);
//! assert_eq!(response["result"]["isError"], false);
//! assert_eq!(response["result"]["structuredContent"]["error_count"], 1);
//! ```

use crate::hover::{complexity_line, function_use_ranges, time_estimate};
use crate::nav;
use crate::position::LineIndex;
use lsp_types::{Position, Uri};
use marigold_grammar::diagnostics::{ByteRange, Severity, MAX_CHECK_INPUT_BYTES};
use serde_json::{json, Map, Value};
use std::io::{self, BufRead, Read, Write};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::Path;
use std::str::FromStr;

pub const MAX_LINE_BYTES: usize = 16 * 1024 * 1024;

pub const PROTOCOL_VERSION: &str = "2025-11-25";

const SUPPORTED_VERSIONS: [&str; 4] = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"];

const PARSE_ERROR: i64 = -32700;
const INVALID_REQUEST: i64 = -32600;
const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_PARAMS: i64 = -32602;

const REVIEW_RULE_INSTRUCTIONS: &str = "Call marigold_check on every .marigold file after creating or editing it: fix all errors AND review every warning (a stream variable read before it is declared). Undefined stream variables, functions and structs are reported as information and can be ignored if they are Rust items in scope in m!() (a stream variable may be a Rust binding with a get() method returning a stream). ok: true does not mean there are no warnings; check warning_count and info_count. All tools are read-only.";

const MAX_LISTED_DIAGNOSTICS: usize = 50;

const POSITION_NOTE: &str = "Lines and columns are 1-based and counted in characters (Unicode scalar values), not bytes or UTF-16 units.";

struct RpcError {
    code: i64,
    message: String,
}

fn rpc_error(code: i64, message: impl Into<String>) -> RpcError {
    RpcError {
        code,
        message: message.into(),
    }
}

enum Line {
    Eof,
    TooLong,
    Data(Vec<u8>),
}

fn read_line<R: BufRead>(reader: &mut R, max: usize) -> io::Result<Line> {
    let mut buf = Vec::new();
    let mut too_long = false;
    let mut any = false;
    loop {
        let chunk = match reader.fill_buf() {
            Ok(chunk) => chunk,
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        };
        if chunk.is_empty() {
            break;
        }
        any = true;
        let newline = chunk.iter().position(|&b| b == b'\n');
        let take = newline.unwrap_or(chunk.len());
        if !too_long {
            if buf.len() + take > max {
                too_long = true;
                buf = Vec::new();
            } else {
                buf.extend_from_slice(&chunk[..take]);
            }
        }
        reader.consume(take + usize::from(newline.is_some()));
        if newline.is_some() {
            break;
        }
    }
    Ok(match (any, too_long) {
        (false, _) => Line::Eof,
        (true, true) => Line::TooLong,
        (true, false) => Line::Data(buf),
    })
}

/// Serves MCP requests read line by line from `reader`, writing one JSON
/// message per line to `writer`, until `reader` reaches end of input.
///
/// Malformed input yields JSON-RPC error responses rather than failures; only
/// I/O errors are returned.
///
/// ```
/// use std::io::Cursor;
///
/// let input = "{\"jsonrpc\":\"2.0\",\"id\":\"a\",\"method\":\"ping\"}\nnot json\n";
/// let mut out = Vec::new();
/// marigold_lsp::mcp::serve_mcp(Cursor::new(input), &mut out).unwrap();
/// let lines: Vec<serde_json::Value> = String::from_utf8(out)
///     .unwrap()
///     .lines()
///     .map(|l| serde_json::from_str(l).unwrap())
///     .collect();
/// assert_eq!(lines[0]["id"], "a");
/// assert_eq!(lines[1]["error"]["code"], -32700);
/// ```
pub fn serve_mcp<R: BufRead, W: Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    loop {
        let response = match read_line(&mut reader, MAX_LINE_BYTES)? {
            Line::Eof => return Ok(()),
            Line::TooLong => Some(error_response(
                Value::Null,
                INVALID_REQUEST,
                "request line exceeds the 16 MiB limit",
            )),
            Line::Data(bytes) => handle_line(&bytes),
        };
        if let Some(response) = response {
            let mut encoded = serde_json::to_vec(&response).map_err(io::Error::other)?;
            encoded.push(b'\n');
            writer.write_all(&encoded)?;
            writer.flush()?;
        }
    }
}

/// Runs [`serve_mcp`] on the process's standard input and output.
pub fn serve_stdio() -> io::Result<()> {
    serve_mcp(io::stdin().lock(), io::stdout().lock())
}

fn ok_response(id: Value, result: Value) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "result": result})
}

fn error_response(id: Value, code: i64, message: impl Into<String>) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "error": {"code": code, "message": message.into()}})
}

fn handle_line(bytes: &[u8]) -> Option<Value> {
    let trimmed = bytes.trim_ascii();
    if trimmed.is_empty() {
        return None;
    }
    let value: Value = match serde_json::from_slice(trimmed) {
        Ok(value) => value,
        Err(e) => {
            return Some(error_response(
                Value::Null,
                PARSE_ERROR,
                format!("parse error: {e}"),
            ))
        }
    };
    let Value::Object(message) = value else {
        return Some(error_response(
            Value::Null,
            INVALID_REQUEST,
            "expected a single JSON-RPC request object",
        ));
    };
    handle_message(&message)
}

fn handle_message(message: &Map<String, Value>) -> Option<Value> {
    if !message.contains_key("method")
        && (message.contains_key("result") || message.contains_key("error"))
    {
        return None;
    }
    let id = match message.get("id") {
        None => None,
        Some(id @ Value::String(_)) => Some(id.clone()),
        Some(id @ Value::Number(n)) if n.is_i64() || n.is_u64() => Some(id.clone()),
        Some(_) => {
            return Some(error_response(
                Value::Null,
                INVALID_REQUEST,
                "id must be a string or an integer",
            ))
        }
    };
    let reply_id = id.clone().unwrap_or(Value::Null);
    if message.get("jsonrpc") != Some(&Value::String("2.0".into())) {
        return Some(error_response(
            reply_id,
            INVALID_REQUEST,
            "jsonrpc must be \"2.0\"",
        ));
    }
    let Some(method) = message.get("method").and_then(Value::as_str) else {
        return Some(error_response(
            reply_id,
            INVALID_REQUEST,
            "method must be a string",
        ));
    };
    let id = id?;
    Some(match handle_request(method, message.get("params")) {
        Ok(result) => ok_response(id, result),
        Err(e) => error_response(id, e.code, e.message),
    })
}

fn handle_request(method: &str, params: Option<&Value>) -> Result<Value, RpcError> {
    match method {
        "initialize" => initialize(params),
        "ping" => Ok(json!({})),
        "tools/list" => Ok(json!({"tools": tool_definitions()})),
        "tools/call" => call_tool(params),
        other => Err(rpc_error(
            METHOD_NOT_FOUND,
            format!("method not found: {other}"),
        )),
    }
}

fn initialize(params: Option<&Value>) -> Result<Value, RpcError> {
    let requested = params
        .and_then(|p| p.get("protocolVersion"))
        .and_then(Value::as_str)
        .ok_or_else(|| {
            rpc_error(
                INVALID_PARAMS,
                "initialize requires a protocolVersion string",
            )
        })?;
    let version = SUPPORTED_VERSIONS
        .iter()
        .find(|v| **v == requested)
        .copied()
        .unwrap_or(PROTOCOL_VERSION);
    Ok(json!({
        "protocolVersion": version,
        "capabilities": {"tools": {"listChanged": false}},
        "serverInfo": {
            "name": "marigold-lsp",
            "title": "Marigold language intelligence",
            "version": env!("CARGO_PKG_VERSION"),
        },
        "instructions": REVIEW_RULE_INSTRUCTIONS,
    }))
}

fn tool_definitions() -> Value {
    let path = json!({"type": "string", "description": "Path to a .marigold file, absolute or relative to the server's working directory."});
    let line = json!({"type": "integer", "minimum": 1, "description": "1-based line number."});
    let column = json!({"type": "integer", "minimum": 1, "description": "1-based column, counted in characters."});
    let read_only = json!({"readOnlyHint": true, "destructiveHint": false, "idempotentHint": true, "openWorldHint": false});
    json!([
        {
            "name": "marigold_check",
            "title": "Check Marigold source",
            "description": format!("Check a .marigold file (path) or a source string (source) and return every diagnostic with severity, code, message, optional help and a range. Provide exactly one of path or source. Call it after every edit: fix all errors AND review every warning (a stream variable read before it is declared). Undefined stream variables, functions and structs are reported as information and can be ignored if they are Rust items in scope in m!() (a stream variable may be a Rust binding with a get() method returning a stream). ok: true does not mean there are no warnings; check warning_count and info_count. {POSITION_NOTE}"),
            "inputSchema": {
                "type": "object",
                "properties": {
                    "path": path,
                    "source": {"type": "string", "description": "Marigold source text to check instead of a file."},
                },
                "additionalProperties": false,
            },
            "annotations": read_only,
        },
        {
            "name": "marigold_symbols",
            "title": "List Marigold symbols",
            "description": format!("List every declaration (stream variables, functions, structs, enums) and reference in a .marigold file. Works on files with syntax errors. {POSITION_NOTE}"),
            "inputSchema": {
                "type": "object",
                "properties": {"path": path},
                "required": ["path"],
                "additionalProperties": false,
            },
            "annotations": read_only,
        },
        {
            "name": "marigold_definition",
            "title": "Go to Marigold definition",
            "description": format!("Find the declaration of the symbol at a position in a .marigold file. {POSITION_NOTE}"),
            "inputSchema": {
                "type": "object",
                "properties": {"path": path, "line": line, "column": column},
                "required": ["path", "line", "column"],
                "additionalProperties": false,
            },
            "annotations": read_only,
        },
        {
            "name": "marigold_references",
            "title": "Find Marigold references",
            "description": format!("Find all references to the symbol at a position in a .marigold file, optionally including its declaration (default true). {POSITION_NOTE}"),
            "inputSchema": {
                "type": "object",
                "properties": {
                    "path": path,
                    "line": line,
                    "column": column,
                    "include_declaration": {"type": "boolean", "default": true, "description": "Whether to include the declaration itself."},
                },
                "required": ["path", "line", "column"],
                "additionalProperties": false,
            },
            "annotations": read_only,
        },
        {
            "name": "marigold_complexity",
            "title": "Marigold stream complexity",
            "description": format!("Report cardinality, time and space complexity and whether input is collected for every stream in a .marigold file. The file must parse. Time is the analyzer's class per whole stream and is reported as 'not estimated' when cardinality is unknown or above 1000000, or the chain calls user functions, since their cost is not modelled. {POSITION_NOTE}"),
            "inputSchema": {
                "type": "object",
                "properties": {"path": path},
                "required": ["path"],
                "additionalProperties": false,
            },
            "annotations": read_only,
        },
    ])
}

struct ToolOutput {
    text: String,
    structured: Value,
}

type ToolResult = Result<ToolOutput, String>;

fn call_tool(params: Option<&Value>) -> Result<Value, RpcError> {
    let params = params
        .and_then(Value::as_object)
        .ok_or_else(|| rpc_error(INVALID_PARAMS, "tools/call params must be an object"))?;
    let name = params
        .get("name")
        .and_then(Value::as_str)
        .ok_or_else(|| rpc_error(INVALID_PARAMS, "tools/call requires a string name"))?;
    let empty = Map::new();
    let args = match params.get("arguments") {
        None | Some(Value::Null) => &empty,
        Some(Value::Object(args)) => args,
        Some(_) => {
            return Err(rpc_error(
                INVALID_PARAMS,
                "tools/call arguments must be an object",
            ))
        }
    };
    let tool: fn(&Map<String, Value>) -> ToolResult = match name {
        "marigold_check" => check_tool,
        "marigold_symbols" => symbols_tool,
        "marigold_definition" => definition_tool,
        "marigold_references" => references_tool,
        "marigold_complexity" => complexity_tool,
        other => return Err(rpc_error(INVALID_PARAMS, format!("Unknown tool: {other}"))),
    };
    let outcome = catch_unwind(AssertUnwindSafe(|| tool(args)))
        .unwrap_or_else(|_| Err("internal error while analysing the program".to_string()));
    Ok(match outcome {
        Ok(out) => json!({
            "content": [{"type": "text", "text": out.text}],
            "structuredContent": out.structured,
            "isError": false,
        }),
        Err(message) => json!({
            "content": [{"type": "text", "text": message}],
            "isError": true,
        }),
    })
}

fn string_arg<'a>(args: &'a Map<String, Value>, key: &str) -> Result<Option<&'a str>, String> {
    match args.get(key) {
        None | Some(Value::Null) => Ok(None),
        Some(Value::String(s)) => Ok(Some(s)),
        Some(_) => Err(format!("{key} must be a string")),
    }
}

fn required_path(args: &Map<String, Value>) -> Result<&str, String> {
    string_arg(args, "path")?.ok_or_else(|| "path is required".to_string())
}

fn one_based_arg(args: &Map<String, Value>, key: &str) -> Result<u32, String> {
    let value = args
        .get(key)
        .ok_or_else(|| format!("{key} is required"))?
        .as_u64()
        .filter(|n| (1..=u64::from(u32::MAX)).contains(n))
        .ok_or_else(|| format!("{key} must be an integer >= 1"))?;
    Ok(value as u32)
}

fn read_marigold(path: &str) -> Result<String, String> {
    let is_marigold = |p: &Path| p.extension().is_some_and(|e| e == "marigold");
    if !is_marigold(Path::new(path)) {
        return Err(format!(
            "refusing to read {path}: only .marigold files can be read"
        ));
    }
    let resolved = std::fs::canonicalize(path).map_err(|e| format!("cannot read {path}: {e}"))?;
    if !is_marigold(&resolved) {
        return Err(format!(
            "refusing to read {path}: it resolves to a file that is not a .marigold file"
        ));
    }
    let meta = std::fs::metadata(&resolved).map_err(|e| format!("cannot read {path}: {e}"))?;
    if !meta.is_file() {
        return Err(format!("{path} is not a regular file"));
    }
    if meta.len() > MAX_CHECK_INPUT_BYTES as u64 {
        return Err(format!(
            "{path} is {} bytes, which exceeds the 10 MiB limit",
            meta.len()
        ));
    }
    let file = std::fs::File::open(&resolved).map_err(|e| format!("cannot read {path}: {e}"))?;
    let mut bytes = Vec::new();
    file.take(MAX_CHECK_INPUT_BYTES as u64 + 1)
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read {path}: {e}"))?;
    if bytes.len() > MAX_CHECK_INPUT_BYTES {
        return Err(format!("{path} exceeds the 10 MiB limit"));
    }
    String::from_utf8(bytes).map_err(|_| format!("{path} is not valid UTF-8"))
}

fn span(index: &LineIndex, range: ByteRange) -> Map<String, Value> {
    let (line, column) = index.char_position(range.start);
    let (end_line, end_column) = index.char_position(range.end);
    let mut m = Map::new();
    m.insert("line".into(), json!(line + 1));
    m.insert("column".into(), json!(column + 1));
    m.insert("end_line".into(), json!(end_line + 1));
    m.insert("end_column".into(), json!(end_column + 1));
    m
}

fn lsp_range_to_bytes(index: &LineIndex, range: lsp_types::Range) -> ByteRange {
    ByteRange {
        start: index.offset(range.start),
        end: index.offset(range.end),
    }
}

fn locate(index: &LineIndex, args: &Map<String, Value>) -> Result<Position, String> {
    let line = one_based_arg(args, "line")?;
    let column = one_based_arg(args, "column")?;
    let offset = index.offset_of_char(line - 1, column - 1).ok_or_else(|| {
        format!(
            "position {line}:{column} is outside the file ({} lines)",
            index.line_count()
        )
    })?;
    Ok(index.position(offset))
}

fn display_range(s: &Map<String, Value>) -> String {
    format!(
        "{}:{}-{}:{}",
        s["line"], s["column"], s["end_line"], s["end_column"]
    )
}

fn format_check(
    label: &str,
    text: &str,
    diags: &[marigold_grammar::diagnostics::Diagnostic],
) -> String {
    if diags.is_empty() {
        return format!("marigold: no diagnostics in {label}");
    }
    let index = LineIndex::new(text);
    let noun = if diags.len() == 1 {
        "diagnostic"
    } else {
        "diagnostics"
    };
    let tally = |severity: Severity| diags.iter().filter(|d| d.severity == severity).count();
    let (errors, warnings, infos) = (
        tally(Severity::Error),
        tally(Severity::Warning),
        tally(Severity::Information),
    );
    let mut lines = vec![format!(
        "marigold: {} {noun} in {label} ({errors} {}, {warnings} {}, {infos} information)",
        diags.len(),
        if errors == 1 { "error" } else { "errors" },
        if warnings == 1 { "warning" } else { "warnings" },
    )];
    for d in diags.iter().take(MAX_LISTED_DIAGNOSTICS) {
        let s = span(&index, d.range);
        let message = d.message.replace('\n', "\n    ");
        lines.push(format!(
            "{label}:{}:{}: {}[{}]: {message}",
            s["line"],
            s["column"],
            d.severity.as_str(),
            d.code
        ));
        if let Some(help) = &d.help {
            lines.push(format!("    help: {}", help.replace('\n', "\n    ")));
        }
        lines.push(format!("    range {}", display_range(&s)));
    }
    if diags.len() > MAX_LISTED_DIAGNOSTICS {
        lines.push(format!(
            "({} more in structuredContent)",
            diags.len() - MAX_LISTED_DIAGNOSTICS
        ));
    }
    lines.join("\n")
}

fn check_tool(args: &Map<String, Value>) -> ToolResult {
    let (label, text) = match (string_arg(args, "path")?, string_arg(args, "source")?) {
        (Some(_), Some(_)) => return Err("provide only one of path and source".into()),
        (None, None) => return Err("provide either path or source".into()),
        (Some(path), None) => (path.to_string(), read_marigold(path)?),
        (None, Some(source)) => ("<source>".to_string(), source.to_string()),
    };
    let diags = marigold_grammar::marigold_check(&text);
    let index = LineIndex::new(&text);
    let count = |severity: Severity| diags.iter().filter(|d| d.severity == severity).count();
    let listed: Vec<Value> = diags
        .iter()
        .map(|d| {
            let mut m = span(&index, d.range);
            m.insert("severity".into(), json!(d.severity.as_str()));
            m.insert("code".into(), json!(d.code));
            m.insert("message".into(), json!(d.message));
            if let Some(help) = &d.help {
                m.insert("help".into(), json!(help));
            }
            Value::Object(m)
        })
        .collect();
    Ok(ToolOutput {
        text: format_check(&label, &text, &diags),
        structured: json!({
            "source": label,
            "ok": count(Severity::Error) == 0,
            "error_count": count(Severity::Error),
            "warning_count": count(Severity::Warning),
            "info_count": count(Severity::Information),
            "diagnostics": listed,
        }),
    })
}

fn symbols_tool(args: &Map<String, Value>) -> ToolResult {
    let path = required_path(args)?;
    let text = read_marigold(path)?;
    let index = LineIndex::new(&text);
    let symbols = marigold_grammar::marigold_symbols(&text);
    let mut decls: Vec<_> = symbols.declarations.iter().collect();
    decls.sort_by_key(|d| d.range.start);
    let mut refs: Vec<_> = symbols.references.iter().collect();
    refs.sort_by_key(|r| r.range.start);
    let entry = |name: &str, kind: marigold_grammar::symbols::SymbolKind, range: ByteRange| {
        let mut m = span(&index, range);
        m.insert("name".into(), json!(name));
        m.insert(
            "kind".into(),
            serde_json::to_value(kind).unwrap_or(Value::Null),
        );
        m
    };
    let decl_entries: Vec<Map<String, Value>> = decls
        .iter()
        .map(|d| entry(&d.name, d.kind, d.range))
        .collect();
    let ref_entries: Vec<Map<String, Value>> = refs
        .iter()
        .map(|r| entry(&r.name, r.kind, r.range))
        .collect();
    let mut lines = vec![format!(
        "marigold: {} declarations and {} references in {path}",
        decl_entries.len(),
        ref_entries.len()
    )];
    for (role, entries) in [("declaration", &decl_entries), ("reference", &ref_entries)] {
        for e in entries {
            lines.push(format!(
                "{path}:{}:{}: {} {role} {}",
                e["line"],
                e["column"],
                e["kind"].as_str().unwrap_or("symbol"),
                e["name"].as_str().unwrap_or("")
            ));
        }
    }
    Ok(ToolOutput {
        text: lines.join("\n"),
        structured: json!({
            "path": path,
            "declarations": decl_entries,
            "references": ref_entries,
        }),
    })
}

fn placeholder_uri() -> Result<Uri, String> {
    Uri::from_str("file:///document.marigold").map_err(|e| e.to_string())
}

fn definition_tool(args: &Map<String, Value>) -> ToolResult {
    let path = required_path(args)?;
    let line = one_based_arg(args, "line")?;
    let column = one_based_arg(args, "column")?;
    let text = read_marigold(path)?;
    let index = LineIndex::new(&text);
    let position = locate(&index, args)?;
    let found = nav::definition(&placeholder_uri()?, &text, position)
        .map(|loc| span(&index, lsp_range_to_bytes(&index, loc.range)));
    Ok(match found {
        Some(def) => ToolOutput {
            text: format!(
                "{path}:{}:{}: definition ({})",
                def["line"],
                def["column"],
                display_range(&def)
            ),
            structured: json!({"found": true, "definition": def}),
        },
        None => ToolOutput {
            text: format!("no definition found for the symbol at {path}:{line}:{column}"),
            structured: json!({"found": false}),
        },
    })
}

fn references_tool(args: &Map<String, Value>) -> ToolResult {
    let path = required_path(args)?;
    let line = one_based_arg(args, "line")?;
    let column = one_based_arg(args, "column")?;
    let include_declaration = match args.get("include_declaration") {
        None | Some(Value::Null) => true,
        Some(Value::Bool(b)) => *b,
        Some(_) => return Err("include_declaration must be a boolean".into()),
    };
    let text = read_marigold(path)?;
    let index = LineIndex::new(&text);
    let position = locate(&index, args)?;
    let found = nav::references(&placeholder_uri()?, &text, position, include_declaration);
    Ok(match found {
        Some(locations) => {
            let spans: Vec<Map<String, Value>> = locations
                .iter()
                .map(|l| span(&index, lsp_range_to_bytes(&index, l.range)))
                .collect();
            let mut lines = vec![format!("{} references", spans.len())];
            lines.extend(
                spans
                    .iter()
                    .map(|s| format!("{path}:{}:{}", s["line"], s["column"])),
            );
            ToolOutput {
                text: lines.join("\n"),
                structured: json!({"found": true, "count": spans.len(), "references": spans}),
            }
        }
        None => ToolOutput {
            text: format!("no symbol at {path}:{line}:{column}"),
            structured: json!({"found": false, "count": 0, "references": []}),
        },
    })
}

fn complexity_tool(args: &Map<String, Value>) -> ToolResult {
    let path = required_path(args)?;
    let text = read_marigold(path)?;
    let nodes = marigold_grammar::marigold_stream_complexities(&text).map_err(|_| {
        format!("{path} does not parse, so complexity is unavailable; run marigold_check first")
    })?;
    let index = LineIndex::new(&text);
    let function_uses = function_use_ranges(&text);
    let mut lines = vec![format!("marigold: {} streams in {path}", nodes.len())];
    let streams: Vec<Value> = nodes
        .iter()
        .map(|n| {
            let c = &n.complexity;
            let mut m = span(&index, n.range);
            lines.push(format!(
                "{path}:{}:{}: {}{}",
                m["line"],
                m["column"],
                n.name
                    .as_deref()
                    .map(|name| format!("{name}: "))
                    .unwrap_or_default(),
                complexity_line(n, &function_uses)
            ));
            m.insert("name".into(), json!(n.name));
            m.insert("description".into(), json!(c.description));
            m.insert("cardinality".into(), json!(c.cardinality.to_string()));
            m.insert(
                "time".into(),
                json!(time_estimate(n, &function_uses)
                    .unwrap_or_else(|_| "not estimated".to_string())),
            );
            m.insert("space".into(), json!(c.space_class.to_string()));
            m.insert("collects_input".into(), json!(c.collects_input));
            Value::Object(m)
        })
        .collect();
    Ok(ToolOutput {
        text: lines.join("\n"),
        structured: json!({"path": path, "streams": streams}),
    })
}
