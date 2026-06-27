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

/// select_all with a read_file source has InputVariability::Variable and InputCount::Unknown,
/// so aggregate_input_variability returns Variable and aggregate_input_count returns Unknown.
/// Both branches of those functions are exercised here.
#[test]
fn select_all_with_variable_stream_has_unknown_cardinality() {
    let source = "select_all(range(0, 10), read_file(\"data.csv\", csv, struct=Data)).return";
    let result = marigold_grammar::marigold_analyze(source).unwrap();
    assert_eq!(
        format!("{}", result.program_cardinality),
        "?",
        "select_all containing a read_file source should yield unknown cardinality"
    );
}

/// select_all with an enum range (range(MyEnum)) exercises the InputCount::Enum branch in
/// aggregate_input_count, which immediately returns InputCount::Unknown.
#[test]
fn select_all_with_enum_range_has_unknown_cardinality() {
    let source = "select_all(range(MyEnum), range(0, 10)).return";
    let result = marigold_grammar::marigold_analyze(source).unwrap();
    assert_eq!(
        format!("{}", result.program_cardinality),
        "?",
        "select_all containing an enum range should yield unknown cardinality"
    );
}
