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

fn analyze_source(source: &str) -> ProgramComplexity {
    marigold_grammar::marigold_analyze(source).unwrap_or_else(|e| panic!("Failed to analyze: {e}"))
}

fn program_with_last_line(name: &str, last: &str) -> String {
    let path = format!("{}/programs/{name}.marigold", env!("CARGO_MANIFEST_DIR"));
    let source = std::fs::read_to_string(&path).unwrap();
    let body: Vec<&str> = source.lines().collect();
    let mut lines = body[..body.len() - 1].to_vec();
    lines.push(last);
    lines.join("\n")
}

#[test]
fn knapsack() {
    let result = analyze_example("knapsack");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert!(!stream.collects_input);
}

#[test]
fn knapsack_intermediate_cardinalities() {
    let items = analyze_source(&program_with_last_line("knapsack", "range(0, 14).return"));
    assert_eq!(
        items.streams[0].cardinality,
        Cardinality::Exact(14u32.into())
    );
    let folded = analyze_source(&program_with_last_line(
        "knapsack",
        "range(0, 14).fold(start, step).return",
    ));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
}

#[test]
fn knapsack_diverges_from_canonical_class() {
    let result = analyze_example("knapsack");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(2)
    );
    assert_ne!(result.streams[0].space_class, ComplexityClass::ON);
}

#[test]
fn floyd_warshall() {
    let result = analyze_example("floyd_warshall");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert!(!stream.collects_input);
}

#[test]
fn floyd_warshall_intermediate_cardinalities() {
    let vertices = analyze_source(&program_with_last_line(
        "floyd_warshall",
        "range(0, 8).return",
    ));
    assert_eq!(
        vertices.streams[0].cardinality,
        Cardinality::Exact(8u32.into())
    );
}

#[test]
fn floyd_warshall_diverges_from_canonical_class() {
    let result = analyze_example("floyd_warshall");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(3)
    );
    assert_ne!(
        result.streams[0].space_class,
        ComplexityClass::OPolynomial(2)
    );
}
