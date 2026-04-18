//! Negative tests for the Marigold parser.
//!
//! These tests verify that malformed or invalid Marigold programs return errors
//! rather than panicking or silently producing incorrect output.

use marigold_grammar::parser::parse_marigold;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Call parse_marigold and assert it returns an Err.
macro_rules! assert_parse_err {
    ($input:expr) => {{
        let result = parse_marigold($input);
        assert!(
            result.is_err(),
            "expected parse error for input {:?}, but got Ok({:?})",
            $input,
            result.unwrap()
        );
    }};
}

// ---------------------------------------------------------------------------
// Missing terminal
// ---------------------------------------------------------------------------

/// A pipeline that never terminates with `.return` or a write terminal should
/// be rejected by the parser.
#[test]
fn missing_return_terminal() {
    // A bare range expression without .return is not a complete program.
    assert_parse_err!("range(0, 10)");
}

#[test]
fn pipeline_without_terminal() {
    // map without return
    assert_parse_err!("range(0, 10).map(double)");
}

// ---------------------------------------------------------------------------
// Unclosed / mismatched punctuation
// ---------------------------------------------------------------------------

#[test]
fn unclosed_parenthesis_in_range() {
    assert_parse_err!("range(0, 10");
}

#[test]
fn unclosed_parenthesis_in_filter() {
    assert_parse_err!("range(0, 10).filter(is_even.return");
}

#[test]
fn extra_closing_parenthesis() {
    assert_parse_err!("range(0, 10)).return");
}

// ---------------------------------------------------------------------------
// Unknown / misspelled function names
// ---------------------------------------------------------------------------

#[test]
fn unknown_source_function() {
    // `xrange` is not a valid Marigold source function.
    assert_parse_err!("xrange(0, 10).return");
}

#[test]
fn unknown_chained_function() {
    // `frobnicate` is not a valid stream combinator.
    assert_parse_err!("range(0, 10).frobnicate(foo).return");
}

// ---------------------------------------------------------------------------
// Malformed numeric literals / arguments
// ---------------------------------------------------------------------------

#[test]
fn range_with_non_numeric_arguments() {
    assert_parse_err!("range(a, b).return");
}

// ---------------------------------------------------------------------------
// Structural errors
// ---------------------------------------------------------------------------

#[test]
fn bare_dot_chain() {
    // Starting with a dot is syntactically invalid.
    assert_parse_err!(".return");
}

#[test]
fn return_without_source() {
    // `.return` alone, without a preceding stream source, is invalid.
    assert_parse_err!("return");
}
