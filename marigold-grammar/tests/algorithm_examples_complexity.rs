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

fn cardinality_of(source: &str) -> Cardinality {
    marigold_grammar::marigold_analyze(source)
        .unwrap_or_else(|e| panic!("Failed to analyze {source}: {e}"))
        .streams[0]
        .cardinality
        .clone()
}

#[test]
fn dijkstra() {
    let result = analyze_example("dijkstra");
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
    assert_eq!(
        cardinality_of("range(0, 9).return"),
        Cardinality::Exact(BigUint::from(9u64))
    );
}

#[test]
fn kruskal() {
    let result = analyze_example("kruskal");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(4)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(result.streams[0].collects_input);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64))
    );
    assert_eq!(
        cardinality_of("range(0, 8).combinations(2).return"),
        Cardinality::Exact(BigUint::from(28u64))
    );
    assert_eq!(
        cardinality_of("range(0, 8).combinations(2).map(weigh).return"),
        Cardinality::Exact(BigUint::from(28u64))
    );
    assert_eq!(
        cardinality_of("range(0, 8).combinations(2).map(weigh).keep_first_n(28, by_weight).return"),
        Cardinality::Exact(BigUint::from(28u64))
    );
}
