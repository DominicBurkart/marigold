//! Tests for the top-level marigold_parse and marigold_analyze public API.

#[test]
fn parse_simple_range_return() {
    let result = marigold_grammar::marigold_parse("range(0, 10).return");
    assert!(result.is_ok(), "Simple range.return should parse: {:?}", result);
    let code = result.unwrap();
    // The generated code should contain marigold_impl references
    assert!(code.contains("marigold_impl"), "Generated code should reference marigold_impl");
}

#[test]
fn parse_range_with_map() {
    let result = marigold_grammar::marigold_parse("range(0, 5).map(double).return");
    assert!(result.is_ok(), "range.map.return should parse: {:?}", result);
}

#[test]
fn parse_range_with_filter() {
    let result = marigold_grammar::marigold_parse("range(0, 100).filter(is_even).return");
    assert!(result.is_ok(), "range.filter.return should parse: {:?}", result);
}

#[test]
fn parse_range_with_fold() {
    let result = marigold_grammar::marigold_parse("range(0, 10).fold(0, add).return");
    assert!(result.is_ok(), "range.fold.return should parse: {:?}", result);
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
fn parse_error_empty_input() {
    let result = marigold_grammar::marigold_parse("");
    assert!(result.is_err(), "Empty input should fail to parse");
}

#[test]
fn parse_error_no_terminal() {
    let result = marigold_grammar::marigold_parse("range(0, 10)");
    assert!(result.is_err(), "Missing terminal (.return or .write_file) should fail");
}

#[test]
fn parse_error_invalid_syntax() {
    let result = marigold_grammar::marigold_parse("this is not valid marigold code!!!");
    assert!(result.is_err(), "Invalid syntax should produce an error");
}

#[test]
fn analyze_simple_range() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).return");
    assert!(result.is_ok(), "Analyze should succeed for simple range: {:?}", result);
    let analysis = result.unwrap();
    assert_eq!(analysis.streams.len(), 1);
}

#[test]
fn analyze_error_invalid_input() {
    let result = marigold_grammar::marigold_analyze("");
    assert!(result.is_err(), "Analyze should fail on empty input");
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
    assert!(result.is_ok(), "fn declaration + usage should parse: {:?}", result);
    // End-to-end correctness of fn declarations is validated in
    // tests/src/lib.rs::test_fn_declaration_in_macro. We only verify here
    // that parse succeeds and returns non-empty output.
    let code = result.unwrap();
    assert!(!code.is_empty(), "Generated code should be non-empty");
}

#[test]
fn parse_chained_operations() {
    let result = marigold_grammar::marigold_parse(
        "range(0, 10).filter(is_even).map(double).return"
    );
    assert!(result.is_ok(), "Chained filter.map should parse: {:?}", result);
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
