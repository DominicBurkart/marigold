//! Extended cardinality tests covering edge cases not in the original suite.

use marigold_grammar::complexity::Cardinality;
use num_bigint::BigUint;
use num_traits::Zero;

fn analyze(source: &str) -> marigold_grammar::complexity::ProgramComplexity {
    marigold_grammar::marigold_analyze(source)
        .unwrap_or_else(|e| panic!("Failed to analyze: {e}"))
}

fn analyze_file(path: &str) -> marigold_grammar::complexity::ProgramComplexity {
    let source =
        std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    analyze(&source)
}

#[test]
fn empty_range_has_zero_cardinality() {
    let result = analyze_file("tests/programs/card_empty_range.marigold");
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::zero()),
        "range(0,0) should have cardinality 0"
    );
}

#[test]
fn single_element_range_has_cardinality_one() {
    let result = analyze_file("tests/programs/card_single_element.marigold");
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64)),
    );
}

#[test]
fn permutations_with_replacement_cardinality() {
    // range(0,3) has 3 elements, permutations_with_replacement(2) = 3^2 = 9
    let result = analyze_file("tests/programs/card_permutations_with_replacement.marigold");
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(9u64)),
    );
}

#[test]
fn inline_range_return_parse_and_analyze() {
    let result = analyze("range(0, 100).return");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(100u64)),
    );
}

#[test]
fn inline_filter_bounded() {
    let result = analyze("range(0, 100).filter(f).return");
    assert!(
        matches!(result.streams[0].cardinality, Cardinality::Bounded(_)),
        "filter should produce bounded cardinality, got: {:?}",
        result.streams[0].cardinality
    );
}

#[test]
fn inline_fold_cardinality_is_one() {
    // Fold in grammar uses named function references
    let result = analyze("range(0, 10).fold(0, add).return");
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64)),
        "fold always produces exactly one output element"
    );
}

#[test]
fn program_cardinality_is_max_of_streams() {
    // Two streams: range(0,10) exact=10, and range(0,100) exact=100
    // Program cardinality should be max = 100
    let result = analyze(
        r#"
digits = range(0, 10)
digits.return

hundreds = range(0, 100)
hundreds.return
"#,
    );
    assert_eq!(result.streams.len(), 2);
    assert_eq!(
        result.program_cardinality,
        Cardinality::Exact(BigUint::from(100u64)),
        "program cardinality should be max of all streams"
    );
}
