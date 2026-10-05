use proptest::prelude::*;
use serde_json::{json, Value};
use std::io::Cursor;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

fn run_bytes(input: &[u8]) -> Vec<Value> {
    let mut out = Vec::new();
    marigold_lsp::mcp::serve_mcp(Cursor::new(input.to_vec()), &mut out).unwrap();
    let text = String::from_utf8(out).expect("stdout is utf-8");
    assert!(
        text.is_empty() || text.ends_with('\n'),
        "output must be newline terminated"
    );
    text.lines()
        .map(|l| serde_json::from_str(l).expect("every output line is JSON"))
        .collect()
}

fn run(lines: &[Value]) -> Vec<Value> {
    let mut input = String::new();
    for l in lines {
        input.push_str(&l.to_string());
        input.push('\n');
    }
    run_bytes(input.as_bytes())
}

fn request(id: Value, method: &str, params: Value) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params})
}

fn call(name: &str, args: Value) -> Value {
    let out = run(&[request(
        json!(1),
        "tools/call",
        json!({"name": name, "arguments": args}),
    )]);
    assert_eq!(out.len(), 1, "{out:?}");
    assert_eq!(out[0]["id"], 1);
    assert!(out[0].get("error").is_none(), "{:?}", out[0]);
    out[0]["result"].clone()
}

fn text_of(result: &Value) -> String {
    let content = result["content"].as_array().unwrap();
    assert_eq!(content.len(), 1);
    assert_eq!(content[0]["type"], "text");
    content[0]["text"].as_str().unwrap().to_string()
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

struct Dir(PathBuf);

impl Dir {
    fn new() -> Self {
        let dir = std::env::temp_dir().join(format!(
            "marigold-mcp-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::SeqCst)
        ));
        std::fs::create_dir_all(&dir).unwrap();
        Dir(dir)
    }

    fn write(&self, name: &str, content: &str) -> String {
        let path = self.0.join(name);
        std::fs::write(&path, content).unwrap();
        path.to_string_lossy().into_owned()
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn initialize_handshake_negotiates_version() {
    let out = run(&[
        request(
            json!(1),
            "initialize",
            json!({"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "t", "version": "0"}}),
        ),
        json!({"jsonrpc": "2.0", "method": "notifications/initialized"}),
        request(json!(2), "ping", json!({})),
    ]);
    assert_eq!(out.len(), 2);
    assert_eq!(out[0]["jsonrpc"], "2.0");
    assert_eq!(out[0]["id"], 1);
    assert_eq!(out[0]["result"]["protocolVersion"], "2025-06-18");
    assert_eq!(out[0]["result"]["serverInfo"]["name"], "marigold-lsp");
    assert!(out[0]["result"]["capabilities"]["tools"].is_object());
    assert_eq!(out[1]["id"], 2);
    assert_eq!(out[1]["result"], json!({}));
}

#[test]
fn initialize_with_unknown_version_answers_latest_supported() {
    let out = run(&[request(
        json!(1),
        "initialize",
        json!({"protocolVersion": "1999-01-01", "capabilities": {}}),
    )]);
    assert_eq!(out[0]["result"]["protocolVersion"], "2025-11-25");
}

#[test]
fn initialize_without_params_is_invalid_params() {
    let out = run(&[request(json!(1), "initialize", json!({}))]);
    assert_eq!(out[0]["error"]["code"], -32602);
}

#[test]
fn tools_list_names_and_schemas() {
    let out = run(&[request(json!(1), "tools/list", json!({}))]);
    let tools = out[0]["result"]["tools"].as_array().unwrap();
    let names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
    assert_eq!(
        names,
        [
            "marigold_check",
            "marigold_symbols",
            "marigold_definition",
            "marigold_references",
            "marigold_complexity"
        ]
    );
    for tool in tools {
        assert!(!tool["description"].as_str().unwrap().is_empty());
        assert_eq!(tool["inputSchema"]["type"], "object");
        assert_eq!(tool["annotations"]["readOnlyHint"], true);
    }
    let definition = &tools[2];
    assert_eq!(
        definition["inputSchema"]["required"],
        json!(["path", "line", "column"])
    );
    assert!(definition["description"]
        .as_str()
        .unwrap()
        .contains("1-based"));
    assert_eq!(tools[1]["inputSchema"]["required"], json!(["path"]));
    assert_eq!(
        tools[3]["inputSchema"]["properties"]["include_declaration"]["type"],
        "boolean"
    );
}

#[test]
fn check_source_reports_one_based_diagnostics() {
    let result = call("marigold_check", json!({"source": "range(Colour).return"}));
    assert_eq!(result["isError"], false);
    let structured = &result["structuredContent"];
    assert_eq!(structured["error_count"], 1);
    let d = &structured["diagnostics"][0];
    assert_eq!(d["severity"], "error");
    assert_eq!(d["line"], 1);
    assert_eq!(d["column"], 7);
    assert_eq!(d["end_line"], 1);
    assert!(d["code"].as_str().unwrap().len() > 3);
    assert!(!d["message"].as_str().unwrap().is_empty());
    let text = text_of(&result);
    assert!(
        text.starts_with("marigold: 1 diagnostic in <source>"),
        "{text}"
    );
    assert!(text.contains("<source>:1:7: error["), "{text}");
    assert!(text.contains("range 1:7-1:"), "{text}");
}

#[test]
fn check_clean_source_has_no_diagnostics() {
    let result = call("marigold_check", json!({"source": "range(0, 3).return"}));
    assert_eq!(result["isError"], false);
    assert_eq!(result["structuredContent"]["diagnostics"], json!([]));
    assert_eq!(result["structuredContent"]["ok"], true);
    assert_eq!(text_of(&result), "marigold: no diagnostics in <source>");
}

#[test]
fn check_file_by_path_and_help_text() {
    let dir = Dir::new();
    let path = dir.write("a.marigold", "x = range(0, 3)\nrange(Colour).return");
    let result = call("marigold_check", json!({"path": path}));
    let structured = &result["structuredContent"];
    let d = &structured["diagnostics"][0];
    assert_eq!(d["line"], 2);
    assert_eq!(d["column"], 7);
    let text = text_of(&result);
    assert!(text.contains(&format!("{path}:2:7: error[")), "{text}");
    if d.get("help").is_some() {
        assert!(text.contains("\n    help: "), "{text}");
    }
}

#[test]
fn check_matches_core_diagnostics() {
    let src = "range(Colour).return\nrange(Shade).return";
    let core = marigold_grammar::marigold_check(src);
    let result = call("marigold_check", json!({"source": src}));
    let got = result["structuredContent"]["diagnostics"]
        .as_array()
        .unwrap();
    assert_eq!(got.len(), core.len());
    for (g, c) in got.iter().zip(&core) {
        assert_eq!(g["code"], c.code);
        assert_eq!(g["message"], c.message.as_str());
    }
}

#[test]
fn check_columns_count_characters_not_utf16() {
    let src = "x = range(0, 1)\n\u{1F600}\u{1F600} range(Colour).return";
    let core = marigold_grammar::marigold_check(src);
    assert!(!core.is_empty());
    let result = call("marigold_check", json!({"source": src}));
    let d = &result["structuredContent"]["diagnostics"][0];
    let line_text = src.lines().nth(1).unwrap();
    let byte_in_line = core[0].range.start - src.find('\n').unwrap() - 1;
    let expected = line_text[..byte_in_line].chars().count() + 1;
    assert_eq!(d["line"], 2);
    assert_eq!(d["column"], expected);
}

#[test]
fn check_handles_crlf() {
    let result = call(
        "marigold_check",
        json!({"source": "x = range(0, 3)\r\nrange(Colour).return\r\n"}),
    );
    let d = &result["structuredContent"]["diagnostics"][0];
    assert_eq!(d["line"], 2);
    assert_eq!(d["column"], 7);
}

#[test]
fn check_syntax_error_is_reported_not_failed() {
    let result = call("marigold_check", json!({"source": "range(0, 1).retur"}));
    assert_eq!(result["isError"], false);
    assert_eq!(result["structuredContent"]["ok"], false);
    assert_eq!(
        result["structuredContent"]["diagnostics"][0]["code"],
        "syntax-error"
    );
}

#[test]
fn check_requires_exactly_one_of_path_and_source() {
    let neither = call("marigold_check", json!({}));
    assert_eq!(neither["isError"], true);
    let both = call(
        "marigold_check",
        json!({"path": "a.marigold", "source": ""}),
    );
    assert_eq!(both["isError"], true);
    assert!(text_of(&both).contains("only one"));
}

#[test]
fn rejects_non_marigold_paths() {
    let dir = Dir::new();
    let path = dir.write("secret.txt", "range(0, 1).return");
    for tool in ["marigold_check", "marigold_symbols", "marigold_complexity"] {
        let result = call(tool, json!({"path": path}));
        assert_eq!(result["isError"], true, "{tool}");
        assert!(text_of(&result).contains(".marigold"), "{tool}");
    }
    let result = call(
        "marigold_definition",
        json!({"path": path, "line": 1, "column": 1}),
    );
    assert_eq!(result["isError"], true);
}

#[cfg(unix)]
#[test]
fn rejects_symlink_to_non_marigold_file() {
    let dir = Dir::new();
    let target = dir.write("target.txt", "range(0, 1).return");
    let link = dir.0.join("link.marigold");
    std::os::unix::fs::symlink(&target, &link).unwrap();
    let result = call("marigold_check", json!({"path": link.to_string_lossy()}));
    assert_eq!(result["isError"], true);
}

#[test]
fn rejects_missing_and_directory_paths() {
    let dir = Dir::new();
    let missing = dir.0.join("nope.marigold");
    let result = call("marigold_check", json!({"path": missing.to_string_lossy()}));
    assert_eq!(result["isError"], true);
    let sub = dir.0.join("d.marigold");
    std::fs::create_dir(&sub).unwrap();
    let result = call("marigold_check", json!({"path": sub.to_string_lossy()}));
    assert_eq!(result["isError"], true);
}

#[test]
fn rejects_oversized_file_before_reading() {
    let dir = Dir::new();
    let path = dir.0.join("big.marigold");
    let file = std::fs::File::create(&path).unwrap();
    file.set_len(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES as u64 + 1)
        .unwrap();
    let result = call("marigold_check", json!({"path": path.to_string_lossy()}));
    assert_eq!(result["isError"], true);
    assert!(text_of(&result).contains("10 MiB"), "{}", text_of(&result));
}

#[test]
fn rejects_invalid_utf8_file() {
    let dir = Dir::new();
    let path = dir.0.join("bin.marigold");
    std::fs::write(&path, [0xff, 0xfe, 0x00]).unwrap();
    let result = call("marigold_check", json!({"path": path.to_string_lossy()}));
    assert_eq!(result["isError"], true);
}

const PROGRAM: &str = "x = range(0, 5)\ny = x.map(double)\ny.return";

#[test]
fn symbols_lists_declarations_and_references() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", PROGRAM);
    let result = call("marigold_symbols", json!({"path": path}));
    assert_eq!(result["isError"], false);
    let s = &result["structuredContent"];
    let decls = s["declarations"].as_array().unwrap();
    let core = marigold_grammar::marigold_symbols(PROGRAM);
    assert_eq!(decls.len(), core.declarations.len());
    assert_eq!(decls[0]["name"], "x");
    assert_eq!(decls[0]["kind"], "stream_variable");
    assert_eq!(decls[0]["line"], 1);
    assert_eq!(decls[0]["column"], 1);
    assert_eq!(decls[0]["end_column"], 2);
    assert_eq!(decls[1]["name"], "y");
    assert_eq!(decls[1]["line"], 2);
    let refs = s["references"].as_array().unwrap();
    assert_eq!(refs.len(), core.references.len());
    assert!(refs.iter().any(|r| r["name"] == "double"));
    let text = text_of(&result);
    assert!(
        text.contains(":1:1: stream_variable declaration x"),
        "{text}"
    );
}

#[test]
fn symbols_survive_broken_programs() {
    let dir = Dir::new();
    let path = dir.write("b.marigold", "x = range(0, 5)\nrange(0, 1).retur\nx.return");
    let result = call("marigold_symbols", json!({"path": path}));
    assert_eq!(result["isError"], false);
    assert_eq!(result["structuredContent"]["declarations"][0]["name"], "x");
    assert_eq!(result["structuredContent"]["references"][0]["line"], 3);
}

#[test]
fn definition_finds_declaration() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "x = range(0, 3)\nx.return");
    let result = call(
        "marigold_definition",
        json!({"path": path, "line": 2, "column": 1}),
    );
    assert_eq!(result["isError"], false);
    let s = &result["structuredContent"];
    assert_eq!(s["found"], true);
    assert_eq!(s["definition"]["line"], 1);
    assert_eq!(s["definition"]["column"], 1);
    assert_eq!(s["definition"]["end_column"], 2);
    assert!(text_of(&result).contains(":1:1"));
}

#[test]
fn definition_off_symbol_is_not_found_but_not_an_error() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "x = range(0, 3)\nx.return");
    let result = call(
        "marigold_definition",
        json!({"path": path, "line": 1, "column": 6}),
    );
    assert_eq!(result["isError"], false);
    assert_eq!(result["structuredContent"]["found"], false);
}

#[test]
fn definition_after_multibyte_comment_line() {
    let dir = Dir::new();
    let path = dir.write("u.marigold", "// \u{1F600}\nx = range(0, 3)\nx.return");
    let result = call(
        "marigold_definition",
        json!({"path": path, "line": 3, "column": 1}),
    );
    assert_eq!(result["structuredContent"]["found"], true, "{result}");
    assert_eq!(result["structuredContent"]["definition"]["line"], 2);
    assert_eq!(result["structuredContent"]["definition"]["column"], 1);
}

#[test]
fn definition_handles_crlf_files() {
    let dir = Dir::new();
    let path = dir.write("c.marigold", "x = range(0, 3)\r\nx.return\r\n");
    let result = call(
        "marigold_definition",
        json!({"path": path, "line": 2, "column": 1}),
    );
    assert_eq!(result["structuredContent"]["found"], true);
    assert_eq!(result["structuredContent"]["definition"]["end_column"], 2);
}

#[test]
fn definition_rejects_bad_positions() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "x = range(0, 3)\nx.return");
    for args in [
        json!({"path": path, "line": 0, "column": 1}),
        json!({"path": path, "line": 1, "column": 0}),
        json!({"path": path, "line": 99, "column": 1}),
        json!({"path": path, "line": 1, "column": 99}),
        json!({"path": path, "line": "1", "column": 1}),
        json!({"path": path, "line": 1.5, "column": 1}),
        json!({"path": path, "line": 1}),
        json!({"path": path, "line": -1, "column": 1}),
    ] {
        let result = call("marigold_definition", args.clone());
        assert_eq!(result["isError"], true, "{args}");
    }
}

#[test]
fn references_with_and_without_declaration() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "x = range(0, 3)\nx.return");
    let with = call(
        "marigold_references",
        json!({"path": path, "line": 2, "column": 1}),
    );
    assert_eq!(with["structuredContent"]["count"], 2);
    let without = call(
        "marigold_references",
        json!({"path": path, "line": 2, "column": 1, "include_declaration": false}),
    );
    assert_eq!(without["structuredContent"]["count"], 1);
    assert_eq!(without["structuredContent"]["references"][0]["line"], 2);
    let off = call(
        "marigold_references",
        json!({"path": path, "line": 1, "column": 6}),
    );
    assert_eq!(off["isError"], false);
    assert_eq!(off["structuredContent"]["found"], false);
}

#[test]
fn complexity_per_stream() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "x = range(0, 5)\nx.return");
    let result = call("marigold_complexity", json!({"path": path}));
    assert_eq!(result["isError"], false);
    let streams = result["structuredContent"]["streams"].as_array().unwrap();
    assert_eq!(streams.len(), 2);
    assert_eq!(streams[0]["name"], "x");
    assert_eq!(streams[0]["cardinality"], "5");
    assert_eq!(streams[0]["time"], "O(1)");
    assert_eq!(streams[0]["space"], "O(n)");
    assert_eq!(streams[0]["collects_input"], false);
    assert_eq!(streams[0]["line"], 1);
    assert_eq!(streams[1]["name"], Value::Null);
    assert!(text_of(&result).contains(
        "analyzer estimate for the whole chain \u{b7} cardinality: 5 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: no"
    ));
}

#[test]
fn complexity_of_unparseable_program_is_tool_error() {
    let dir = Dir::new();
    let path = dir.write("p.marigold", "range(0,");
    let result = call("marigold_complexity", json!({"path": path}));
    assert_eq!(result["isError"], true);
    assert!(text_of(&result).contains("marigold_check"));
}

#[test]
fn unknown_tool_is_invalid_params() {
    let out = run(&[request(
        json!(1),
        "tools/call",
        json!({"name": "nope", "arguments": {}}),
    )]);
    assert_eq!(out[0]["error"]["code"], -32602);
    assert!(out[0]["error"]["message"]
        .as_str()
        .unwrap()
        .contains("nope"));
}

#[test]
fn malformed_call_params_are_invalid_params() {
    for params in [
        json!({}),
        json!({"name": 5}),
        json!({"name": "marigold_check", "arguments": []}),
        json!("x"),
    ] {
        let out = run(&[request(json!(1), "tools/call", params.clone())]);
        assert_eq!(out[0]["error"]["code"], -32602, "{params}");
    }
}

#[test]
fn unknown_method_is_method_not_found() {
    let out = run(&[request(json!(7), "resources/list", json!({}))]);
    assert_eq!(out[0]["id"], 7);
    assert_eq!(out[0]["error"]["code"], -32601);
}

#[test]
fn malformed_json_is_parse_error_with_null_id_and_server_continues() {
    let out = run_bytes(b"{not json\n{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"ping\"}\n");
    assert_eq!(out.len(), 2);
    assert_eq!(out[0]["error"]["code"], -32700);
    assert_eq!(out[0]["id"], Value::Null);
    assert_eq!(out[1]["id"], 3);
    assert_eq!(out[1]["result"], json!({}));
}

#[test]
fn invalid_utf8_line_is_parse_error() {
    let out = run_bytes(b"\xff\xfe\n");
    assert_eq!(out[0]["error"]["code"], -32700);
}

#[test]
fn invalid_requests_are_rejected_with_32600() {
    let lines: [&[u8]; 7] = [
        b"[]\n",
        b"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}]\n",
        b"42\n",
        b"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"ping\"}\n",
        b"{\"jsonrpc\":\"2.0\",\"id\":1}\n",
        b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":5}\n",
        b"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"ping\"}\n",
    ];
    for line in lines {
        let out = run_bytes(line);
        assert_eq!(out.len(), 1, "{}", String::from_utf8_lossy(line));
        assert_eq!(out[0]["error"]["code"], -32600);
        let has_valid_id = line.starts_with(b"{\"jsonrpc\":\"1.0\"")
            || line == b"{\"jsonrpc\":\"2.0\",\"id\":1}\n"
            || line.starts_with(b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":5");
        let expected = if has_valid_id { json!(1) } else { Value::Null };
        assert_eq!(out[0]["id"], expected);
    }
}

#[test]
fn fractional_and_object_ids_are_invalid() {
    for id in ["1.5", "{}", "true"] {
        let line = format!("{{\"jsonrpc\":\"2.0\",\"id\":{id},\"method\":\"ping\"}}\n");
        let out = run_bytes(line.as_bytes());
        assert_eq!(out[0]["error"]["code"], -32600, "{id}");
    }
}

#[test]
fn notifications_get_no_response() {
    let out = run(&[
        json!({"jsonrpc": "2.0", "method": "notifications/initialized"}),
        json!({"jsonrpc": "2.0", "method": "notifications/cancelled", "params": {"requestId": 1}}),
        json!({"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "marigold_check", "arguments": {"source": ""}}}),
        json!({"jsonrpc": "2.0", "method": "no/such/thing"}),
    ]);
    assert!(out.is_empty(), "{out:?}");
}

#[test]
fn client_responses_are_ignored() {
    let out = run(&[json!({"jsonrpc": "2.0", "id": 9, "result": {}})]);
    assert!(out.is_empty());
}

#[test]
fn ids_are_echoed_with_their_type() {
    let out = run(&[
        request(json!(42), "ping", json!({})),
        request(json!("abc-1"), "ping", json!({})),
        request(json!(0), "ping", json!({})),
        request(json!(-5), "ping", json!({})),
        request(json!(u64::MAX), "ping", json!({})),
        request(json!(""), "ping", json!({})),
    ]);
    assert_eq!(out[0]["id"], json!(42));
    assert_eq!(out[1]["id"], json!("abc-1"));
    assert_eq!(out[2]["id"], json!(0));
    assert_eq!(out[3]["id"], json!(-5));
    assert_eq!(out[4]["id"], json!(u64::MAX));
    assert_eq!(out[5]["id"], json!(""));
}

#[test]
fn blank_lines_and_crlf_framing_are_tolerated() {
    let out = run_bytes(b"\n  \r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\r\n");
    assert_eq!(out.len(), 1);
    assert_eq!(out[0]["id"], 1);
}

#[test]
fn last_line_without_newline_is_processed() {
    let out = run_bytes(b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}");
    assert_eq!(out.len(), 1);
}

#[test]
fn empty_input_exits_cleanly_with_no_output() {
    assert!(run_bytes(b"").is_empty());
}

#[test]
fn oversized_line_is_rejected_and_server_recovers() {
    let mut input = vec![b'a'; marigold_lsp::mcp::MAX_LINE_BYTES + 1];
    input.push(b'\n');
    input.extend_from_slice(b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n");
    let out = run_bytes(&input);
    assert_eq!(out.len(), 2);
    assert_eq!(out[0]["error"]["code"], -32600);
    assert_eq!(out[0]["id"], Value::Null);
    assert_eq!(out[1]["id"], 1);
}

#[test]
fn line_exactly_at_limit_is_parsed() {
    let mut input = vec![b' '; marigold_lsp::mcp::MAX_LINE_BYTES - 2];
    input.extend_from_slice(b"42");
    input.push(b'\n');
    let out = run_bytes(&input);
    assert_eq!(out[0]["error"]["code"], -32600);
}

#[test]
fn output_lines_never_contain_embedded_newlines() {
    let result = call(
        "marigold_check",
        json!({"source": "range(Colour).return\n\nrange(Shade).return"}),
    );
    assert!(text_of(&result).contains('\n'));
    let mut out = Vec::new();
    marigold_lsp::mcp::serve_mcp(
        Cursor::new(
            request(
                json!(1),
                "tools/call",
                json!({"name": "marigold_check", "arguments": {"source": "range(Colour).return\nrange(Shade).return"}}),
            )
            .to_string()
            .into_bytes(),
        ),
        &mut out,
    )
    .unwrap();
    assert_eq!(out.iter().filter(|&&b| b == b'\n').count(), 1);
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(128))]

    #[test]
    fn arbitrary_lines_get_well_formed_answers(lines in proptest::collection::vec(proptest::collection::vec(any::<u8>(), 0..200), 0..8)) {
        let mut input = Vec::new();
        for l in &lines {
            input.extend(l.iter().copied().filter(|&b| b != b'\n'));
            input.push(b'\n');
        }
        let mut out = Vec::new();
        marigold_lsp::mcp::serve_mcp(Cursor::new(input), &mut out).unwrap();
        let text = String::from_utf8(out).unwrap();
        for line in text.lines() {
            let v: Value = serde_json::from_str(line).unwrap();
            prop_assert_eq!(&v["jsonrpc"], "2.0");
            prop_assert!(v.get("result").is_some() ^ v.get("error").is_some());
            prop_assert!(v.get("id").is_some());
            if let Some(e) = v.get("error") {
                prop_assert!(e["code"].is_i64());
                prop_assert!(e["message"].is_string());
            }
        }
    }

    #[test]
    fn arbitrary_json_values_never_panic(
        method in "[a-z/_]{0,20}",
        id in prop_oneof![Just(json!(1)), Just(json!("s")), Just(Value::Null), Just(json!(1.5))],
        name in "[a-z_]{0,20}",
        source in ".{0,80}",
    ) {
        let msg = json!({"jsonrpc": "2.0", "id": id, "method": method, "params": {"name": name, "arguments": {"source": source, "path": source, "line": 1, "column": 1}}});
        let out = run(&[msg]);
        prop_assert_eq!(out.len(), 1);
        prop_assert!(out[0].get("result").is_some() ^ out[0].get("error").is_some());
    }

    #[test]
    fn check_tool_never_fails_on_arbitrary_source(source in ".{0,200}") {
        let result = call("marigold_check", json!({"source": source}));
        prop_assert_eq!(&result["isError"], &json!(false));
        for d in result["structuredContent"]["diagnostics"].as_array().unwrap() {
            prop_assert!(d["line"].as_u64().unwrap() >= 1);
            prop_assert!(d["column"].as_u64().unwrap() >= 1);
        }
    }
}

#[test]
fn syntactically_invalid_json_is_parse_error() {
    let inputs: [&[u8]; 12] = [
        b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"\n",
        b"hello\n",
        b"{\n",
        b"[\n",
        b"{\"a\":1}garbage\n",
        b"1 2\n",
        b"nul\n",
        b"{'jsonrpc':'2.0'}\n",
        b"{\"a\":}\n",
        b"\"unterminated\n",
        b"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",}\n",
        b"\x80\n",
    ];
    for input in inputs {
        let out = run_bytes(input);
        assert_eq!(out.len(), 1, "{}", String::from_utf8_lossy(input));
        assert_eq!(
            out[0]["error"]["code"],
            -32700,
            "{}",
            String::from_utf8_lossy(input)
        );
        assert_eq!(out[0]["id"], Value::Null);
    }
}

#[test]
fn valid_json_that_is_not_a_request_is_invalid_request() {
    let inputs: [&[u8]; 8] = [
        b"\"x\"\n",
        b"42\n",
        b"null\n",
        b"true\n",
        b"[]\n",
        b"[1,2]\n",
        b"{}\n",
        b"{\"jsonrpc\":\"2.0\"}\n",
    ];
    for input in inputs {
        let out = run_bytes(input);
        assert_eq!(out.len(), 1, "{}", String::from_utf8_lossy(input));
        assert_eq!(
            out[0]["error"]["code"],
            -32600,
            "{}",
            String::from_utf8_lossy(input)
        );
        assert_eq!(out[0]["id"], Value::Null);
    }
}

#[test]
fn empty_and_whitespace_lines_get_no_response() {
    assert!(run_bytes(b"\n").is_empty());
    assert!(run_bytes(b"   \t\r\n\n").is_empty());
}

#[test]
fn instructions_and_check_description_require_reviewing_warnings() {
    let init = run(&[request(
        json!(1),
        "initialize",
        json!({"protocolVersion": "2025-11-25", "capabilities": {}}),
    )]);
    let instructions = init[0]["result"]["instructions"].as_str().unwrap();
    let listed = run(&[request(json!(2), "tools/list", json!({}))]);
    let description = listed[0]["result"]["tools"][0]["description"]
        .as_str()
        .unwrap();
    for text in [instructions, description] {
        assert!(text.contains("warning"), "{text}");
        assert!(
            text.contains("undefined stream variables, functions and structs"),
            "{text}"
        );
        assert!(text.contains("ok: true"), "{text}");
        assert!(
            text.contains("does not mean there are no warnings"),
            "{text}"
        );
    }
    assert!(instructions.contains("fix all errors"));
    assert!(description.contains("fix all errors"));
}

#[test]
fn tools_work_before_initialize() {
    let out = run(&[
        request(json!(1), "tools/list", json!({})),
        request(
            json!(2),
            "tools/call",
            json!({"name": "marigold_check", "arguments": {"source": "range(0, 1).return"}}),
        ),
    ]);
    assert_eq!(out[0]["result"]["tools"].as_array().unwrap().len(), 5);
    assert_eq!(out[1]["result"]["isError"], false);
}

#[test]
fn tools_work_between_initialize_and_initialized() {
    let out = run(&[
        request(
            json!(1),
            "initialize",
            json!({"protocolVersion": "2025-11-25", "capabilities": {}}),
        ),
        request(
            json!(2),
            "tools/call",
            json!({"name": "marigold_check", "arguments": {"source": "range(Colour).return"}}),
        ),
        json!({"jsonrpc": "2.0", "method": "notifications/initialized"}),
    ]);
    assert_eq!(out.len(), 2);
    assert_eq!(out[1]["result"]["structuredContent"]["error_count"], 1);
}
