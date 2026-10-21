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
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
    assert_eq!(
        cardinality_of("range(0, 9).return"),
        Cardinality::Exact(9u32.into())
    );
}

#[test]
fn dijkstra_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OPolynomial(2);
    let result = analyze_example("dijkstra");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn kruskal() {
    let result = analyze_example("kruskal");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::OCombinatorial(2));
    let mut expected = ExactComplexity::new();
    expected.add_work(ComplexityClass::O1, 3);
    expected.add_work(ComplexityClass::OCombinatorial(2), 2);
    assert_eq!(stream.exact_time, expected);
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(stream.collects_input);
}

#[test]
fn kruskal_diverges_from_canonical_class() {
    let result = analyze_example("kruskal");
    assert_ne!(result.streams[0].time_class, ComplexityClass::ONLogN);
}

#[test]
fn kruskal_candidate_counts_match_canonical() {
    assert_eq!(
        cardinality_of("range(0, 8).combinations(2).return"),
        Cardinality::Exact(28u32.into())
    );
    assert_eq!(
        cardinality_of("range(0, 8).combinations(2).map(weigh).return"),
        Cardinality::Exact(28u32.into())
    );
    assert_eq!(
        cardinality_of(
            "range(0, 8).combinations(2).map(weigh).filter(is_edge).keep_first_n(13, by_weight).return"
        )
        .to_string(),
        "min(\u{2264}C(8, 2), 13)"
    );
}
