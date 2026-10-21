use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};

fn analyze_prefix(name: &str, stream: &str) -> ProgramComplexity {
    let path = format!("{}/programs/{name}.marigold", env!("CARGO_MANIFEST_DIR"));
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    let body = source.trim_end().rsplit_once('\n').unwrap().0;
    marigold_grammar::marigold_analyze(&format!("{body}\n{stream}\n"))
        .unwrap_or_else(|e| panic!("Failed to analyze prefix of {path}: {e}"))
}

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
fn tsp_brute_force() {
    let result = analyze_example("tsp_brute_force");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(stream.collects_input);

    let permuted = analyze_prefix("tsp_brute_force", "range(1, 10).permutations(9).return");
    assert_eq!(
        permuted.streams[0].cardinality,
        Cardinality::Exact(362_880u32.into())
    );
}

#[test]
fn tsp_brute_force_diverges_from_canonical_class() {
    let result = analyze_example("tsp_brute_force");
    assert_ne!(result.streams[0].time_class, ComplexityClass::OFactorial);
}

#[test]
fn tsp_held_karp() {
    let result = analyze_example("tsp_held_karp");
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

    let masks = analyze_prefix("tsp_held_karp", "range(1, 512).return");
    assert_eq!(
        masks.streams[0].cardinality,
        Cardinality::Exact(511u32.into())
    );
}

#[test]
fn tsp_held_karp_diverges_from_canonical_class() {
    let result = analyze_example("tsp_held_karp");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OCombinatorial(9)
    );
}
