use marigold_grammar::nodes::{InputCount, InputVariability};

// We can't directly call the private type_aggregation module, but we can
// test the public API that uses it (complexity analysis) to validate
// aggregation behavior through e2e tests.

/// Verify that select_all properly aggregates cardinalities by checking
/// the analyze output.
#[test]
fn select_all_aggregates_input_counts() {
    let source = "select_all(range(0, 10), range(0, 20)).return";
    let result = marigold_grammar::marigold_analyze(source).unwrap();
    // 10 + 20 = 30
    let cardinality = &result.program_cardinality;
    assert_eq!(
        format!("{}", cardinality),
        "30",
        "select_all should aggregate input counts"
    );
}

#[test]
fn single_range_cardinality() {
    let source = "range(0, 50).return";
    let result = marigold_grammar::marigold_analyze(source).unwrap();
    assert_eq!(format!("{}", result.program_cardinality), "50");
}

#[test]
fn chained_map_preserves_cardinality() {
    let source = "range(0, 10).map(double).map(inc).return";
    let result = marigold_grammar::marigold_analyze(source).unwrap();
    assert_eq!(format!("{}", result.program_cardinality), "10");
}
