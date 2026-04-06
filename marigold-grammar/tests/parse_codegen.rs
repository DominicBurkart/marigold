//! Direct unit tests for the `marigold_parse()` code-generation entry point.
//!
//! These tests exercise the parse → code-generation path end-to-end and are
//! intentionally kept simple: we check that valid programs succeed and that
//! the generated output contains the expected Rust constructs, and that
//! clearly-invalid input returns an `Err`.

use marigold_grammar::marigold_parse;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Assert that `marigold_parse(src)` succeeds and that every expected
/// substring appears somewhere in the generated Rust code.
fn assert_parse_contains(src: &str, expected_fragments: &[&str]) {
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected parse to succeed for program:\n{}\nGot error: {:?}",
        src,
        result.err()
    );
    let code = result.unwrap();
    for fragment in expected_fragments {
        assert!(
            code.contains(fragment),
            "Expected generated code to contain {:?} for program:\n{}\nGenerated code:\n{}",
            fragment,
            src,
            code
        );
    }
}

/// Assert that `marigold_parse(src)` returns an `Err`.
fn assert_parse_fails(src: &str) {
    let result = marigold_parse(src);
    assert!(
        result.is_err(),
        "Expected parse to fail for program:\n{}\nBut got Ok:",
        src,
    );
}

// ---------------------------------------------------------------------------
// Valid-program tests
// ---------------------------------------------------------------------------

/// A simple range + map pipeline should produce code that references `map`.
#[test]
fn parse_simple_range_map() {
    assert_parse_contains("range(0, 5).map(double).return", &["map"]);
}

/// A range + filter pipeline should produce code that references `filter`.
#[test]
fn parse_filter() {
    assert_parse_contains("range(0, 10).filter(is_positive).return", &["filter"]);
}

/// A range + fold pipeline should produce code that references `fold`.
#[test]
fn parse_fold() {
    assert_parse_contains("range(0, 10).fold(0, accumulate).return", &["fold"]);
}

/// Minimal range-to-return pipeline — the simplest possible valid program.
#[test]
fn parse_range_return() {
    // Should succeed; no particular fragment required beyond a successful parse.
    let result = marigold_parse("range(0, 1).return");
    assert!(
        result.is_ok(),
        "Expected minimal range(0,1).return to parse successfully; got: {:?}",
        result.err()
    );
}

/// Inclusive-range syntax (`range(0, =3).return`) should parse successfully.
#[test]
fn parse_inclusive_range() {
    let result = marigold_parse("range(0, =3).return");
    assert!(
        result.is_ok(),
        "Expected inclusive range syntax to parse successfully; got: {:?}",
        result.err()
    );
}

/// Stream variable syntax: `my_stream = range(0, 5)` followed by `my_stream.return`.
/// Both lines form a single Marigold program (whitespace/newlines are ignored by
/// the grammar, so the two expressions can be separated by a newline).
#[test]
fn parse_stream_var() {
    let src = "my_stream = range(0, 5)\nmy_stream.return";
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected stream-variable program to parse successfully; got: {:?}",
        result.err()
    );
}

/// Stream variable with a map in the output chain.
#[test]
fn parse_stream_var_with_map() {
    let src = "data = range(0, 10)\ndata.map(double).return";
    assert_parse_contains(src, &["map"]);
}

/// A `struct` declaration should parse successfully and produce code that
/// declares the struct by name.
#[test]
fn parse_struct_decl() {
    let src = "struct Point { x: i32, y: i32 }\nrange(0, 1).return";
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected struct declaration to parse successfully; got: {:?}",
        result.err()
    );
    // The generated code should contain the struct name.
    let code = result.unwrap();
    assert!(
        code.contains("Point"),
        "Expected generated code to reference 'Point'; got:\n{}",
        code
    );
}

/// An `enum` declaration should parse and the variant names should appear in
/// the generated code.
#[test]
fn parse_enum_decl() {
    let src = "enum Color { Red = \"red\", Green = \"green\" }\nrange(0, 1).return";
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected enum declaration to parse successfully; got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    assert!(
        code.contains("Color"),
        "Expected generated code to reference 'Color'; got:\n{}",
        code
    );
}

/// A function declaration followed by a stream that uses it should parse and
/// the function name should appear in the generated code.
#[test]
fn parse_fn_decl() {
    let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 5).map(double).return";
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected fn declaration to parse successfully; got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    assert!(
        code.contains("double"),
        "Expected generated code to reference 'double'; got:\n{}",
        code
    );
}

/// `select_all` combining two ranges should parse and the generated code
/// should compile a merged stream.
#[test]
fn parse_select_all() {
    let result = marigold_parse("select_all(range(0, 3), range(10, 13)).return");
    assert!(
        result.is_ok(),
        "Expected select_all program to parse successfully; got: {:?}",
        result.err()
    );
}

/// Permutations pipeline should parse successfully.
#[test]
fn parse_permutations() {
    let result = marigold_parse("range(0, 3).permutations(2).return");
    assert!(
        result.is_ok(),
        "Expected permutations program to parse successfully; got: {:?}",
        result.err()
    );
}

/// Combinations pipeline should parse successfully.
#[test]
fn parse_combinations() {
    let result = marigold_parse("range(0, 4).combinations(2).return");
    assert!(
        result.is_ok(),
        "Expected combinations program to parse successfully; got: {:?}",
        result.err()
    );
}

/// filter_map pipeline should produce code that references `filter_map`.
#[test]
fn parse_filter_map() {
    assert_parse_contains("range(0, 10).filter_map(to_even).return", &["filter_map"]);
}

/// A multi-consumer program: one stream variable consumed by two output streams.
/// This exercises the stream-variable fan-out path in the code generator.
#[test]
fn parse_multi_consumer() {
    let src = "data = range(0, 5)\ndata.return\ndata.return";
    let result = marigold_parse(src);
    assert!(
        result.is_ok(),
        "Expected multi-consumer program to parse successfully; got: {:?}",
        result.err()
    );
}

// ---------------------------------------------------------------------------
// Invalid-input tests
// ---------------------------------------------------------------------------

/// A completely non-Marigold string should be rejected.
#[test]
fn parse_invalid_syntax_fails() {
    assert_parse_fails("definitely_not_valid");
}

/// A stream with no output function should fail (bare `range(0,5)` is not a
/// complete stream — it has no `.return` or `.write_file`).
#[test]
fn parse_bare_range_fails() {
    // `range(0, 5)` alone is not a valid top-level expression: it is an
    // input_function but not a stream (which requires an output function).
    assert_parse_fails("range(0, 5)");
}

/// A pipeline that is missing its output function should fail.
#[test]
fn parse_missing_output_fails() {
    assert_parse_fails("range(0, 5).map(double)");
}
