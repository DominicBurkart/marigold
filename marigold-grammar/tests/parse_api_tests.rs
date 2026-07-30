//! Tests for the top-level marigold_parse and marigold_analyze public API.

#[test]
fn parse_simple_range_return() {
    let result = marigold_grammar::marigold_parse("range(0, 10).return");
    assert!(
        result.is_ok(),
        "Simple range.return should parse: {:?}",
        result
    );
    let code = result.unwrap();
    // The generated code should contain marigold_impl references
    assert!(
        code.contains("marigold_impl"),
        "Generated code should reference marigold_impl"
    );
}

#[test]
fn parse_range_with_map() {
    let result = marigold_grammar::marigold_parse("range(0, 5).map(double).return");
    assert!(
        result.is_ok(),
        "range.map.return should parse: {:?}",
        result
    );
}

#[test]
fn parse_range_with_filter() {
    let result = marigold_grammar::marigold_parse("range(0, 100).filter(is_even).return");
    assert!(
        result.is_ok(),
        "range.filter.return should parse: {:?}",
        result
    );
}

#[test]
fn parse_range_with_fold() {
    let result = marigold_grammar::marigold_parse("range(0, 10).fold(0, add).return");
    assert!(
        result.is_ok(),
        "range.fold.return should parse: {:?}",
        result
    );
}

#[test]
fn parse_inclusive_range() {
    let result = marigold_grammar::marigold_parse("range(0, =10).return");
    assert!(result.is_ok(), "Inclusive range should parse: {:?}", result);
}

#[test]
fn parse_select_all() {
    let result = marigold_grammar::marigold_parse("select_all(range(0, 5), range(10, 15)).return");
    assert!(result.is_ok(), "select_all should parse: {:?}", result);
}

#[test]
fn parse_permutations() {
    let result = marigold_grammar::marigold_parse("range(0, 5).permutations(3).return");
    assert!(result.is_ok(), "permutations should parse: {:?}", result);
}

#[test]
fn parse_combinations() {
    let result = marigold_grammar::marigold_parse("range(0, 10).combinations(2).return");
    assert!(result.is_ok(), "combinations should parse: {:?}", result);
}

#[test]
fn parse_keep_first_n() {
    let result = marigold_grammar::marigold_parse("range(0, 100).keep_first_n(5, cmp).return");
    assert!(result.is_ok(), "keep_first_n should parse: {:?}", result);
}

#[test]
fn parse_empty_input_succeeds() {
    // The parser intentionally returns Ok for empty programs (generates a trivial async wrapper).
    let result = marigold_grammar::marigold_parse("");
    assert!(
        result.is_ok(),
        "Empty input should parse successfully per documented contract"
    );
}

#[test]
fn parse_error_no_terminal() {
    let result = marigold_grammar::marigold_parse("range(0, 10)");
    assert!(
        result.is_err(),
        "Missing terminal (.return or .write_file) should fail"
    );
}

#[test]
fn parse_error_invalid_syntax() {
    let result = marigold_grammar::marigold_parse("this is not valid marigold code!!!");
    assert!(result.is_err(), "Invalid syntax should produce an error");
}

#[test]
fn analyze_simple_range() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).return");
    assert!(
        result.is_ok(),
        "Analyze should succeed for simple range: {:?}",
        result
    );
    let analysis = result.unwrap();
    assert_eq!(analysis.streams.len(), 1);
}

#[test]
fn analyze_empty_input_succeeds() {
    // The parser intentionally returns Ok for empty programs, so analyze should also succeed.
    let result = marigold_grammar::marigold_analyze("");
    assert!(
        result.is_ok(),
        "Analyze should succeed on empty input per documented contract"
    );
}

#[test]
fn analyze_error_invalid_program() {
    let result = marigold_grammar::marigold_analyze("not a program");
    assert!(result.is_err(), "Analyze should fail on invalid program");
}

#[test]
fn parse_fn_declaration_and_usage() {
    let program = r#"fn double(i: i32) -> i32 { i * 2 }
range(0, 5).map(double).return"#;
    let result = marigold_grammar::marigold_parse(program);
    assert!(
        result.is_ok(),
        "fn declaration + usage should parse: {:?}",
        result
    );
    // End-to-end correctness of fn declarations is validated in
    // tests/src/lib.rs::test_fn_declaration_in_macro. We only verify here
    // that parse succeeds and returns non-empty output.
    let code = result.unwrap();
    assert!(!code.is_empty(), "Generated code should be non-empty");
}

#[test]
fn parse_chained_operations() {
    let result =
        marigold_grammar::marigold_parse("range(0, 10).filter(is_even).map(double).return");
    assert!(
        result.is_ok(),
        "Chained filter.map should parse: {:?}",
        result
    );
}

#[test]
fn analyze_reports_collects_input_for_permutations() {
    let result = marigold_grammar::marigold_analyze("range(0, 10).permutations(2).return");
    assert!(result.is_ok());
    let analysis = result.unwrap();
    assert!(
        analysis.streams[0].collects_input,
        "permutations should report collects_input=true"
    );
}

#[test]
fn analyze_streaming_does_not_collect() {
    let result = marigold_grammar::marigold_analyze("range(0, 10).map(double).return");
    assert!(result.is_ok());
    let analysis = result.unwrap();
    assert!(
        !analysis.streams[0].collects_input,
        "map should not report collects_input"
    );
}

#[test]
fn parse_read_file_csv_plain_no_gzip() {
    let result =
        marigold_grammar::marigold_parse("read_file(\"data.csv\", csv, struct=Data).ok().return");
    assert!(result.is_ok(), "read_file without .gz should parse: {:?}", result);
    let code = result.unwrap();
    assert!(!code.contains("GzipDecoder"), "non-.gz path must not use GzipDecoder");
}

#[test]
fn parse_read_file_csv_gz_extension_auto_detects_gzip() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv.gz\", csv, struct=Data).ok().return",
    );
    assert!(
        result.is_ok(),
        "read_file with .gz extension should parse: {:?}",
        result
    );
    let code = result.unwrap();
    assert!(
        code.contains("GzipDecoder"),
        "auto-detected gzip path must use GzipDecoder"
    );
}

#[test]
fn parse_read_file_csv_infer_compression_false_skips_gzip() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv.gz\", csv, struct=Data, infer_compression=false).ok().return",
    );
    assert!(
        result.is_ok(),
        "read_file with infer_compression=false should parse: {:?}",
        result
    );
    let code = result.unwrap();
    assert!(
        !code.contains("GzipDecoder"),
        "explicit infer_compression=false must skip GzipDecoder"
    );
}

#[test]
fn parse_read_file_csv_infer_compression_true_still_checks_extension() {
    // infer_compression=true falls into the same branch as None (auto-detect from extension).
    // Without a .gz extension the result is the same as the plain non-gz case.
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv\", csv, struct=Data, infer_compression=true).ok().return",
    );
    assert!(
        result.is_ok(),
        "read_file with infer_compression=true should parse: {:?}",
        result
    );
    let code = result.unwrap();
    // No .gz extension → no GzipDecoder despite infer_compression=true
    assert!(
        !code.contains("GzipDecoder"),
        "infer_compression=true + non-.gz extension must not use GzipDecoder"
    );
}

#[test]
fn parse_write_file_csv_plain_no_gzip() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv\", csv, struct=Data).ok().write_file(\"out.csv\", csv)",
    );
    assert!(result.is_ok(), "write_file without compression should parse: {:?}", result);
    let code = result.unwrap();
    assert!(!code.contains("GzipEncoder"), "uncompressed write_file must not use GzipEncoder");
}

#[test]
fn parse_write_file_csv_gz_extension_auto_detects_gzip() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv\", csv, struct=Data).ok().write_file(\"out.csv.gz\", csv)",
    );
    assert!(
        result.is_ok(),
        "write_file with .gz path should parse: {:?}",
        result
    );
    let code = result.unwrap();
    assert!(
        code.contains("GzipEncoder"),
        "auto-detected gzip write_file must use GzipEncoder"
    );
}

#[test]
fn parse_write_file_csv_explicit_gz_compression() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv\", csv, struct=Data).ok().write_file(\"out.csv\", csv, compression=gz)",
    );
    assert!(
        result.is_ok(),
        "write_file with compression=gz should parse: {:?}",
        result
    );
    let code = result.unwrap();
    assert!(
        code.contains("GzipEncoder"),
        "explicit compression=gz must use GzipEncoder"
    );
}

#[test]
fn parse_write_file_csv_explicit_none_compression() {
    let result = marigold_grammar::marigold_parse(
        "read_file(\"data.csv\", csv, struct=Data).ok().write_file(\"out.csv\", csv, compression=none)",
    );
    assert!(
        result.is_ok(),
        "write_file with compression=none should parse: {:?}",
        result
    );
    let code = result.unwrap();
    assert!(!code.contains("GzipEncoder"), "compression=none must not use GzipEncoder");
}

#[test]
fn parse_write_file_unsupported_format_errors() {
    let result = marigold_grammar::marigold_parse(
        "range(0, 5).write_file(\"out.json\", json)",
    );
    assert!(
        result.is_err(),
        "write_file with unsupported format should fail"
    );
}
