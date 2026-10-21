use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};
use num_bigint::BigUint;

fn analyze_prefix(name: &str, stream: &str) -> ProgramComplexity {
    let path = format!(
        "{}/../examples/algorithms/programs/{name}.marigold",
        env!("CARGO_MANIFEST_DIR")
    );
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    let body = source.trim_end().rsplit_once('\n').unwrap().0;
    marigold_grammar::marigold_analyze(&format!("{body}\n{stream}\n"))
        .unwrap_or_else(|e| panic!("Failed to analyze prefix of {path}: {e}"))
}

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
fn tsp_brute_force() {
    let result = analyze_example("tsp_brute_force");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert!(result.streams[0].collects_input);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u64))
    );

    let permuted = analyze_prefix("tsp_brute_force", "range(1, 10).permutations(9).return");
    assert_eq!(
        permuted.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(362_880u64))
    );
}

#[test]
fn tsp_held_karp() {
    let result = analyze_example("tsp_held_karp");
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

    let masks = analyze_prefix("tsp_held_karp", "range(1, 512).return");
    assert_eq!(
        masks.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(511u64))
    );
}
