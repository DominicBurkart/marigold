//! Negative tests for the Marigold parser — public API surface.
//!
//! These tests call `parse_marigold` from outside the crate to exercise the
//! public error paths. They focus on cases not already covered by the
//! `negative_tests` module inside `marigold-grammar/src/parser.rs`:
//! unclosed punctuation, unknown function names, non-integer range arguments,
//! and a bare identifier that is not a complete program.
//!
//! Each test asserts both that the result is an `Err` *and* that the error
//! message contains a meaningful fragment, so that a regression that silently
//! swallows the error cannot slip through.

use marigold_grammar::parser::parse_marigold;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Call parse_marigold, assert it returns an Err, and assert the error message
/// contains `$needle`.
macro_rules! assert_parse_err {
    ($input:expr, $needle:expr) => {{
        let result = parse_marigold($input);
        let err = result.expect_err(&format!(
            "expected parse error for input {:?}, but got Ok",
            $input
        ));
        assert!(
            err.0.contains($needle),
            "error {:?} did not contain expected fragment {:?}",
            err.0,
            $needle
        );
    }};
}

// ---------------------------------------------------------------------------
// Unclosed / mismatched punctuation
// ---------------------------------------------------------------------------

#[test]
fn unclosed_parenthesis_in_range() {
    // Missing `)` — the grammar cannot match the `range` argument list.
    assert_parse_err!("range(0, 10", "Parse error");
}

#[test]
fn unclosed_parenthesis_in_filter() {
    // The filter argument list is never closed before `.return`.
    assert_parse_err!("range(0, 10).filter(is_even.return", "Parse error");
}

// ---------------------------------------------------------------------------
// Unknown / misspelled function names
// ---------------------------------------------------------------------------

#[test]
fn unknown_source_function() {
    // `xrange` is not in the grammar's fixed set of source functions.
    assert_parse_err!("xrange(0, 10).return", "Parse error");
}

#[test]
fn unknown_chained_function() {
    // `stream_function` is a closed alternation; any name not in that set
    // fails the grammar alternation rather than a runtime lookup.
    assert_parse_err!("range(0, 10).frobnicate(foo).return", "Parse error");
}

// ---------------------------------------------------------------------------
// Malformed numeric literals / arguments
// ---------------------------------------------------------------------------

#[test]
fn range_with_float_arguments() {
    // `free_text_literal` matches ASCII_DIGIT+ or a letter-starting identifier.
    // A float like `1.5` is not a valid free_text_literal: the digit sequence
    // `1` is consumed, but the remaining `.5` prevents the required comma from
    // matching, so the entire parse fails.
    assert_parse_err!("range(1.5, 2.5).return", "Parse error");
}

// ---------------------------------------------------------------------------
// Structural errors
// ---------------------------------------------------------------------------

#[test]
fn bare_identifier_is_not_a_program() {
    // A single identifier (here the word "return") is not a complete Marigold
    // program — the grammar requires at least a source stream plus a terminal.
    assert_parse_err!("return", "Parse error");
}
