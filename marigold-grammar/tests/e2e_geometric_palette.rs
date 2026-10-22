use std::str::FromStr;

use marigold_grammar::complexity::{Cardinality, ComplexityClass, ExactComplexity};
use num_bigint::BigUint;

const GEOMETRIC_EXAMPLE: &str = "../examples/color-palette-picker-geometric.marigold";
const BRUTE_FORCE_EXAMPLE: &str = "../examples/color-palette-picker.marigold";
const GEOMETRIC_PROGRAM: &str = "tests/programs/geometric_palette.marigold";

fn read(path: &str) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"))
}

fn analyze_source(source: &str) -> marigold_grammar::complexity::StreamComplexity {
    let mut result = marigold_grammar::marigold_analyze(source)
        .unwrap_or_else(|e| panic!("Failed to analyze {source}: {e}"));
    assert_eq!(result.streams.len(), 1);
    result.streams.remove(0)
}

fn analyze_file(path: &str) -> marigold_grammar::complexity::StreamComplexity {
    analyze_source(&read(path))
}

fn exact(n: u64) -> Cardinality {
    Cardinality::Exact(BigUint::from(n))
}

fn choose_three(n: u64) -> u64 {
    n * (n - 1) * (n - 2) / 6
}

#[test]
fn test_program_mirrors_example() {
    assert_eq!(read(GEOMETRIC_PROGRAM), read(GEOMETRIC_EXAMPLE));
}

#[test]
fn geometric_estimate_is_pinned() {
    let geometric = analyze_file(GEOMETRIC_PROGRAM);
    assert_eq!(geometric.time_class, ComplexityClass::O1);
    assert_eq!(
        geometric.exact_time,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert_eq!(geometric.space_class, ComplexityClass::O1);
    assert_eq!(
        geometric.exact_space,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert!(geometric.collects_input);
    assert_eq!(geometric.cardinality, exact(5));
}

#[test]
fn geometric_intermediate_cardinalities_match_canonical_counts() {
    let rotations = 12 / 3;
    let rings = 4u64;
    let polygons = rotations * rings.pow(3);

    assert_eq!(analyze_source("range(0, 4).return").cardinality, exact(4));

    let enumerated = analyze_source("range(0, 4).permutations_with_replacement(4).return");
    assert_eq!(enumerated.cardinality, exact(polygons));
    assert_eq!(enumerated.cardinality, exact(4u64.pow(4)));
    assert_eq!(enumerated.time_class, ComplexityClass::O1);
    assert!(enumerated.collects_input);

    let kept = analyze_source(
        "range(0, 4).permutations_with_replacement(4).keep_first_n(5, compare_polygons).return",
    );
    assert_eq!(kept.cardinality, exact(polygons.min(5)));

    let mapped = analyze_source(
        "range(0, 4).permutations_with_replacement(4).keep_first_n(5, compare_polygons).map(to_palette).return",
    );
    assert_eq!(mapped.cardinality, exact(polygons.min(5)));
}

#[test]
fn unrestricted_wheel_brute_force_has_canonical_cardinality_but_same_class_as_geometric() {
    let wheel_points = 12 * 4;
    let all_triples = analyze_source("range(0, 48).combinations(3).return");
    assert_eq!(all_triples.cardinality, exact(choose_three(wheel_points)));
    assert_eq!(all_triples.time_class, ComplexityClass::O1);

    let top = analyze_source("range(0, 48).combinations(3).keep_first_n(5, compare).return");
    let geometric = analyze_file(GEOMETRIC_PROGRAM);
    assert_eq!(top.cardinality, geometric.cardinality);
    assert_eq!(top.time_class, geometric.time_class);
    assert_eq!(top.space_class, geometric.space_class);
    assert!(choose_three(wheel_points) > 4u64.pow(4));
}

#[test]
fn brute_force_estimate_is_pinned_and_ranks_above_geometric() {
    let brute = analyze_file(BRUTE_FORCE_EXAMPLE);
    let geometric = analyze_file(GEOMETRIC_PROGRAM);

    assert_eq!(brute.time_class, ComplexityClass::OCombinatorial(3));
    assert_eq!(brute.exact_time.to_string(), "O(2C(n,3) + n^3)");
    assert_eq!(brute.space_class, ComplexityClass::OPolynomial(3));
    assert_eq!(brute.exact_space.to_string(), "O(n^3 + n)");
    assert!(brute.collects_input);

    assert!(geometric.time_class < brute.time_class);
    assert!(geometric.space_class < brute.space_class);

    let canonical_candidates = 6u64.pow(3);
    let canonical_palettes = choose_three(canonical_candidates);
    assert_eq!(canonical_palettes, 1_656_360);

    let enumerated = analyze_source(
        "range(0, 256).filter(mod_fifty_one).permutations_with_replacement(3).combinations(3).return",
    );
    let analyzer_bound = match &enumerated.cardinality {
        Cardinality::Bounded(symbolic) => symbolic.upper_bound().expect("bounded"),
        other => panic!("expected a bounded cardinality, got {other:?}"),
    };
    let n = BigUint::from(256u64.pow(3));
    assert_eq!(
        analyzer_bound,
        &n * (&n - 1u32) * (&n - 2u32) / BigUint::from(6u32)
    );
    assert!(analyzer_bound > BigUint::from(canonical_palettes));
}

#[test]
fn filter_hides_constant_input_from_the_analyzer() {
    let filtered = analyze_source(
        "range(0, 256).filter(mod_fifty_one).permutations_with_replacement(3).return",
    );
    assert_eq!(filtered.time_class, ComplexityClass::OPolynomial(3));
    let bound = match &filtered.cardinality {
        Cardinality::Bounded(symbolic) => symbolic.upper_bound().expect("bounded"),
        other => panic!("expected a bounded cardinality, got {other:?}"),
    };
    assert_eq!(bound, BigUint::from(256u64.pow(3)));
    assert!(bound > BigUint::from(6u64.pow(3)));
}
