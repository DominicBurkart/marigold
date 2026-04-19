//! End-to-end parse tests for `read_file(..., json|jsonl, ...)` and
//! `write_file(..., json|jsonl)`.
//!
//! These verify that the grammar + AST builder accept every supported
//! shape of the new I/O formats and that the emitted generated code
//! references the expected symbols in `marigold_impl::json_io`.
//!
//! Runtime round-trip behavior (actually reading/writing files and
//! asserting byte-equality) is exercised in
//! `marigold-impl/src/json_io.rs` unit tests and in the `examples/csv`
//! test suite (which is the sole crate configured with `io + tokio`).

use marigold_grammar::marigold_parse;

fn assert_parses(source: &str) -> String {
    marigold_parse(source)
        .unwrap_or_else(|e| panic!("failed to parse source:\n{source}\n-- error:\n{e}"))
}

#[test]
fn parse_read_file_json() {
    let src = r#"read_file("data.json", json, struct=Data).return"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("json_io :: read_json_array")
            || emitted.contains("json_io::read_json_array"),
        "emitted code should reference read_json_array; got:\n{emitted}"
    );
}

#[test]
fn parse_read_file_jsonl() {
    let src = r#"read_file("events.jsonl", jsonl, struct=Event).return"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("json_io :: read_jsonl") || emitted.contains("json_io::read_jsonl"),
        "emitted code should reference read_jsonl; got:\n{emitted}"
    );
}

#[test]
fn parse_read_file_json_gz() {
    let src = r#"read_file("data.json.gz", json, struct=Data).return"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("GzipDecoder"),
        "gzip inputs should wrap with GzipDecoder; got:\n{emitted}"
    );
}

#[test]
fn parse_read_file_jsonl_explicit_no_compression() {
    let src =
        r#"read_file("events.jsonl.gz", jsonl, struct=Event, infer_compression=false).return"#;
    let emitted = assert_parses(src);
    assert!(
        !emitted.contains("GzipDecoder"),
        "infer_compression=false should disable gzip even on .gz path; got:\n{emitted}"
    );
}

#[test]
fn parse_write_file_json() {
    let src = r#"range(0, 3).write_file("out.json", json)"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("JsonArrayWriter"),
        "json output should use JsonArrayWriter; got:\n{emitted}"
    );
}

#[test]
fn parse_write_file_jsonl() {
    let src = r#"range(0, 3).write_file("out.jsonl", jsonl)"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("JsonlWriter"),
        "jsonl output should use JsonlWriter; got:\n{emitted}"
    );
}

#[test]
fn parse_write_file_json_gz() {
    let src = r#"range(0, 3).write_file("out.json.gz", json)"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("GzipEncoder"),
        "gz output should wrap with GzipEncoder; got:\n{emitted}"
    );
    assert!(
        emitted.contains("JsonArrayWriter"),
        "json output should still use JsonArrayWriter; got:\n{emitted}"
    );
}

#[test]
fn parse_write_file_jsonl_explicit_compression() {
    let src = r#"range(0, 3).write_file("out.jsonl", jsonl, compression=gz)"#;
    let emitted = assert_parses(src);
    assert!(
        emitted.contains("GzipEncoder"),
        "explicit compression=gz should wrap with GzipEncoder; got:\n{emitted}"
    );
}

#[test]
fn reject_write_file_unknown_format() {
    let result = marigold_parse(r#"range(0, 3).write_file("out.xml", xml)"#);
    assert!(
        result.is_err(),
        "xml is not a supported write_file format and should be rejected"
    );
}
