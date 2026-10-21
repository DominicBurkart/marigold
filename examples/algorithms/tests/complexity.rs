use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};

fn analyze_example(name: &str) -> ProgramComplexity {
    let path = format!("{}/programs/{name}.marigold", env!("CARGO_MANIFEST_DIR"));
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

#[test]
fn gcd() {
    let result = analyze_example("gcd");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn gcd_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OLogN;
    let result = analyze_example("gcd");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_simulated_annealing() {
    let result = analyze_example("tsp_simulated_annealing");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_simulated_annealing_diverges_from_canonical_class() {
    let canonical = ComplexityClass::ON;
    let result = analyze_example("tsp_simulated_annealing");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_genetic() {
    let result = analyze_example("tsp_genetic");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_genetic_diverges_from_canonical_class() {
    let canonical = ComplexityClass::ON;
    let result = analyze_example("tsp_genetic");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_ant_colony() {
    let result = analyze_example("tsp_ant_colony");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_ant_colony_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OPolynomial(2);
    let result = analyze_example("tsp_ant_colony");
    assert_ne!(result.streams[0].time_class, canonical);
}
