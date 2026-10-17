#![cfg(feature = "telemetry")]

use marigold_grammar::marigold_node_ids;
use marigold_lsp::mcp::serve_mcp_with_telemetry;
use marigold_lsp::telemetry::{NodeStats, StaticSource, TelemetryError};
use serde_json::{json, Value};
use std::io::Cursor;
use std::time::Duration;

const MCP: &str = include_str!("golden/mcp_list_and_initialize.jsonl");
const SRC: &str = "x = range(0, 5).map(f)\nx.return\n";

fn run(source: Option<&StaticSource>, requests: &[Value]) -> Vec<Value> {
    let input: String = requests.iter().map(|r| format!("{r}\n")).collect();
    let mut out = Vec::new();
    serve_mcp_with_telemetry(
        Cursor::new(input),
        &mut out,
        source.map(|s| s as &dyn marigold_lsp::telemetry::TelemetrySource),
    )
    .unwrap();
    String::from_utf8(out)
        .unwrap()
        .lines()
        .map(|l| serde_json::from_str(l).unwrap())
        .collect()
}

fn call(name: &str, args: Value) -> Value {
    json!({"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":name,"arguments":args}})
}

fn file(name: &str, text: &str) -> String {
    let dir = std::env::temp_dir().join(format!("marigold-telemetry-mcp-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, text).unwrap();
    path.to_string_lossy().into_owned()
}

fn source_for(program: &str, hash: Option<String>) -> StaticSource {
    let nodes = marigold_node_ids(SRC, program);
    StaticSource::new(vec![NodeStats {
        node_id: nodes[2].id.clone(),
        observed_inputs: 12,
        observed_runs: 4,
        errors: 1,
        last_seen: Some(1_700_000_000),
        window: Duration::from_secs(86_400),
        content_hash: hash,
    }])
}

#[test]
fn feature_on_without_a_source_is_byte_identical() {
    let input = [
        json!({"jsonrpc":"2.0","id":1,"method":"tools/list"}),
        json!({"jsonrpc":"2.0","id":2,"method":"initialize","params":{"protocolVersion":"2025-11-25"}}),
    ];
    let out: String = run(None, &input).iter().map(|v| format!("{v}\n")).collect();
    let mut direct = Vec::new();
    let raw: String = input.iter().map(|r| format!("{r}\n")).collect();
    serve_mcp_with_telemetry(Cursor::new(raw), &mut direct, None).unwrap();
    assert_eq!(String::from_utf8(direct).unwrap(), MCP);
    assert_eq!(out.len(), MCP.len());
}

#[test]
fn tool_is_listed_only_with_a_source_and_is_read_only() {
    let list = json!({"jsonrpc":"2.0","id":1,"method":"tools/list"});
    let without = run(None, std::slice::from_ref(&list));
    let names = |v: &Value| -> Vec<String> {
        v["result"]["tools"]
            .as_array()
            .unwrap()
            .iter()
            .map(|t| t["name"].as_str().unwrap().to_string())
            .collect()
    };
    assert!(!names(&without[0]).contains(&"marigold_telemetry".to_string()));
    let source = StaticSource::default();
    let with = run(Some(&source), &[list]);
    let tool = with[0]["result"]["tools"]
        .as_array()
        .unwrap()
        .iter()
        .find(|t| t["name"] == "marigold_telemetry")
        .unwrap()
        .clone();
    assert_eq!(tool["annotations"]["readOnlyHint"], true);
    assert_eq!(tool["inputSchema"]["required"], json!(["path"]));
    assert_eq!(names(&with[0]).len(), names(&without[0]).len() + 1);
}

#[test]
fn calling_without_a_source_is_an_unknown_tool() {
    let out = run(
        None,
        &[call("marigold_telemetry", json!({"path": "a.marigold"}))],
    );
    assert_eq!(out[0]["error"]["code"], -32602);
}

#[test]
fn returns_per_node_json() {
    let path = file("mcp_one.marigold", SRC);
    let program = "mcp_one";
    let source = source_for(program, None);
    let out = run(
        Some(&source),
        &[call("marigold_telemetry", json!({"path": path}))],
    );
    let result = &out[0]["result"];
    assert_eq!(result["isError"], false);
    let nodes = result["structuredContent"]["nodes"].as_array().unwrap();
    assert_eq!(nodes.len(), 1);
    let n = &nodes[0];
    assert_eq!(n["observed_inputs"], 12);
    assert_eq!(n["observed_runs"], 4);
    assert_eq!(n["errors"], 1);
    assert_eq!(n["stale"], false);
    assert_eq!(n["kind"], "stream_function");
    assert_eq!(
        (n["line"].as_u64(), n["column"].as_u64()),
        (Some(1), Some(17))
    );
    assert_eq!(result["structuredContent"]["window_seconds"], 86_400);
    assert!(result["content"][0]["text"]
        .as_str()
        .unwrap()
        .contains("observed: 12 inputs \u{b7} 4 runs \u{b7} 1 errors (last 24h)"));
}

#[test]
fn reports_stale_nodes() {
    let path = file("mcp_two.marigold", SRC);
    let source = source_for("mcp_two", Some("ffffffffffffffff".into()));
    let out = run(
        Some(&source),
        &[call("marigold_telemetry", json!({"path": path}))],
    );
    assert_eq!(
        out[0]["result"]["structuredContent"]["nodes"][0]["stale"],
        true
    );
    assert!(out[0]["result"]["content"][0]["text"]
        .as_str()
        .unwrap()
        .contains("(stale)"));
}

#[test]
fn source_errors_are_reported_without_secrets() {
    let path = file("mcp_three.marigold", SRC);
    let source = StaticSource::failing(TelemetryError::Unavailable("down".into()));
    let out = run(
        Some(&source),
        &[call("marigold_telemetry", json!({"path": path}))],
    );
    assert_eq!(out[0]["result"]["isError"], true);
    assert!(out[0]["result"]["content"][0]["text"]
        .as_str()
        .unwrap()
        .contains("down"));
}

#[test]
fn validates_arguments_and_paths() {
    let source = StaticSource::default();
    let out = run(
        Some(&source),
        &[
            call("marigold_telemetry", json!({})),
            call("marigold_telemetry", json!({"path": "/etc/passwd"})),
            call(
                "marigold_telemetry",
                json!({"path": "/nonexistent/x.marigold"}),
            ),
        ],
    );
    for response in out {
        assert_eq!(response["result"]["isError"], true);
    }
}
