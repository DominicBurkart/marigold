//! Tests for the public `marigold_parse` and `marigold_analyze` APIs.
//!
//! These verify that the top-level entry points accept valid programs
//! and reject malformed input, without going through the m! macro.

use marigold_grammar::{marigold_analyze, marigold_parse};

// -- Happy-path parsing --------------------------------------------------

#[test]
fn parse_simple_range_return() {
    let code = marigold_parse("range(0, 10).return").unwrap();
    assert!(!code.is_empty(), "parsed code should be non-empty");
    // The output should contain Rust stream infrastructure
    assert!(code.contains("marigold_impl"), "output should reference marigold_impl");
}

#[test]
fn parse_range_with_map() {
    let code = marigold_parse("range(0, 5).map(double).return").unwrap();
    assert!(code.contains("map"), "output should contain map call");
}

#[test]
fn parse_range_with_filter() {
    let code = marigold_parse("range(0, 100).filter(is_even).return").unwrap();
    assert!(code.contains("filter"), "output should contain filter call");
}

#[test]
fn parse_inclusive_range() {
    let code = marigold_parse("range(0, =10).return").unwrap();
    assert!(!code.is_empty());
}

#[test]
fn parse_permutations() {
    let code = marigold_parse("range(0, 5).permutations(2).return").unwrap();
    assert!(code.contains("permutations"));
}

#[test]
fn parse_combinations() {
    let code = marigold_parse("range(0, 5).combinations(3).return").unwrap();
    assert!(code.contains("combinations"));
}

#[test]
fn parse_fold() {
    // Fold in Marigold grammar uses named function references, not inline closures
    let code = marigold_parse("range(0, 10).fold(0, add).return").unwrap();
    assert!(code.contains("fold") || code.contains("marifold"));
}

#[test]
fn parse_keep_first_n() {
    let code = marigold_parse("range(0, 100).keep_first_n(5, sorter).return").unwrap();
    assert!(code.contains("keep_first_n"));
}

#[test]
fn parse_select_all() {
    let code = marigold_parse("select_all(range(0, 5), range(10, 15)).return").unwrap();
    assert!(code.contains("select"));
}

#[test]
fn parse_chained_operations() {
    let code = marigold_parse("range(0, 10).map(double).filter(is_even).return").unwrap();
    assert!(code.contains("map"));
    assert!(code.contains("filter"));
}

#[test]
fn parse_fn_declaration_and_use() {
    let program = r#"
fn double(i: i32) -> i32 {
  i * 2
}

range(0, 5).map(double).return
"#;
    let code = marigold_parse(program).unwrap();
    assert!(code.contains("double"));
}

#[test]
fn parse_write_file_csv() {
    let code = marigold_parse(r#"range(0, 5).write_file("/dev/null", csv)"#).unwrap();
    assert!(!code.is_empty());
}

#[test]
fn parse_permutations_with_replacement() {
    let code = marigold_parse("range(0, 5).permutations_with_replacement(2).return").unwrap();
    assert!(code.contains("permutations_with_replacement"));
}

// -- Sad-path parsing ----------------------------------------------------

#[test]
fn parse_garbage_fails() {
    let result = marigold_parse("not a valid program !@#$%");
    assert!(result.is_err(), "garbage input should fail to parse");
}

#[test]
fn parse_missing_return_and_output_fails() {
    // A stream with no terminal (.return or .write_file) should fail
    let result = marigold_parse("range(0, 10)");
    assert!(result.is_err(), "stream with no terminal should fail");
}

#[test]
fn parse_unclosed_parenthesis_fails() {
    let result = marigold_parse("range(0, 10.return");
    assert!(result.is_err(), "unclosed parenthesis should fail");
}

#[test]
fn parse_unknown_stream_function_fails() {
    let result = marigold_parse("range(0, 10).nonexistent_op(foo).return");
    assert!(result.is_err(), "unknown stream function should fail");
}

// -- Analysis API --------------------------------------------------------

#[test]
fn analyze_simple_range() {
    let result = marigold_analyze("range(0, 100).return").unwrap();
    assert_eq!(result.streams.len(), 1);
    assert!(!result.streams[0].description.is_empty());
}

#[test]
fn analyze_returns_correct_stream_count() {
    let program = r#"
fn is_odd(i: &i32) -> bool { i.wrapping_rem(2) == 1 }

digits = range(0, 10)
digits.map(double).return
digits.filter(is_odd).return
"#;
    let result = marigold_analyze(program).unwrap();
    assert_eq!(
        result.streams.len(),
        2,
        "two terminal streams should produce two stream analyses"
    );
}

#[test]
fn analyze_map_preserves_cardinality() {
    let result = marigold_analyze("range(0, 50).map(double).return").unwrap();
    let card = &result.streams[0].cardinality;
    assert_eq!(card.to_string(), "50");
}

#[test]
fn analyze_filter_produces_bounded_cardinality() {
    let result = marigold_analyze("range(0, 50).filter(f).return").unwrap();
    let card = &result.streams[0].cardinality;
    // Filter should produce a bounded (not exact) cardinality
    assert_ne!(card.to_string(), "50", "filter should not preserve exact cardinality");
    assert_ne!(card.to_string(), "?", "filter on known input should not be unknown");
}

#[test]
fn analyze_fold_cardinality_is_one() {
    let result = marigold_analyze("range(0, 10).fold(0, add).return").unwrap();
    assert_eq!(
        result.streams[0].cardinality.to_string(),
        "1",
        "fold always produces exactly one output element"
    );
}
