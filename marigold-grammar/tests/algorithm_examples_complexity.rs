use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};

fn analyze_example(name: &str) -> ProgramComplexity {
    let path = format!(
        "{}/../examples/algorithms/programs/{name}.marigold",
        env!("CARGO_MANIFEST_DIR")
    );
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

#[test]
fn gcd() {
    let result = analyze_example("gcd");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(!result.streams[0].collects_input);
}

fn assert_constant_fold_pipeline(name: &str, exact: &str, description: &str) {
    let result = analyze_example(name);
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.description, description);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(stream.exact_time, ExactComplexity::from_str(exact).unwrap());
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str(exact).unwrap()
    );
    assert!(!stream.collects_input);
}

#[test]
fn tsp_nearest_neighbour() {
    assert_constant_fold_pipeline(
        "tsp_nearest_neighbour",
        "O(2)",
        "input.fold(...).map(...).return",
    );
}

#[test]
fn tsp_christofides() {
    assert_constant_fold_pipeline(
        "tsp_christofides",
        "O(5)",
        "input.fold(...).map(...).map(...).map(...).map(...).return",
    );
}

#[test]
fn tsp_lkh() {
    assert_constant_fold_pipeline("tsp_lkh", "O(2)", "input.fold(...).map(...).return");
}
