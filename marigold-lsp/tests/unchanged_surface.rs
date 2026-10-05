use std::io::Cursor;

const CAPABILITIES: &str = include_str!("golden/capabilities.json");
const MCP: &str = include_str!("golden/mcp_list_and_initialize.jsonl");

#[test]
fn capabilities_are_byte_identical_to_the_baseline() {
    let now = serde_json::to_string(&marigold_lsp::capabilities()).unwrap();
    assert_eq!(now, CAPABILITIES);
}

#[test]
fn mcp_tool_list_and_initialize_are_byte_identical_to_the_baseline() {
    let input = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}\n{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\"}}\n";
    let mut out = Vec::new();
    marigold_lsp::mcp::serve_mcp(Cursor::new(input), &mut out).unwrap();
    assert_eq!(String::from_utf8(out).unwrap(), MCP);
}
