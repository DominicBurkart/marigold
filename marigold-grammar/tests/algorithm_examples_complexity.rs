use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};
use num_bigint::BigUint;

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

#[test]
fn tsp_simulated_annealing() {
    let result = analyze_example("tsp_simulated_annealing");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(!result.streams[0].collects_input);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64))
    );
}

#[test]
fn tsp_genetic() {
    let result = analyze_example("tsp_genetic");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(!result.streams[0].collects_input);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64))
    );
}

#[test]
fn tsp_ant_colony() {
    let result = analyze_example("tsp_ant_colony");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(!result.streams[0].collects_input);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64))
    );
}
