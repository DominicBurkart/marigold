use std::str::FromStr;

use marigold_grammar::complexity::{Cardinality, ComplexityClass, ExactComplexity};
use num_bigint::BigUint;

mod left_right {
    use super::*;

    #[test]
    fn linear_same_simplified_different_exact() {
        let left = analyze_file("tests/programs/left_linear.marigold");
        let right = analyze_file("tests/programs/right_linear.marigold");

        assert_eq!(left.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(right.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(left.streams[0].time_class, right.streams[0].time_class);

        assert_eq!(
            left.streams[0].exact_time,
            ExactComplexity::from_str("O(1)").unwrap()
        );
        assert_eq!(
            right.streams[0].exact_time,
            ExactComplexity::from_str("O(3)").unwrap()
        );
        assert_ne!(left.streams[0].exact_time, right.streams[0].exact_time);
    }

    #[test]
    fn collecting_same_simplified_different_exact() {
        let left = analyze_file("tests/programs/left_quadratic.marigold");
        let right = analyze_file("tests/programs/right_quadratic.marigold");

        // Both pipelines use range(0, 100) — a literal constant source — so the
        // entire pipeline is O(1): no variable n means all work is bounded by a
        // fixed constant regardless of how many permutation steps follow.
        // The exact_time coefficients still differ (1 map vs 3 ops), making them
        // useful for comparing constant-factor cost even within the same O(1) class.
        assert_eq!(left.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(right.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(left.streams[0].time_class, right.streams[0].time_class);

        assert_eq!(left.streams[0].exact_time.to_string(), "O(2)");
        assert_eq!(right.streams[0].exact_time.to_string(), "O(4)");
        assert_ne!(left.streams[0].exact_time, right.streams[0].exact_time);

        assert!(left.streams[0].collects_input);
        assert!(right.streams[0].collects_input);
    }

    #[test]
    fn space_streaming_vs_collecting() {
        let left = analyze_file("tests/programs/left_space.marigold");
        let right = analyze_file("tests/programs/right_space.marigold");

        assert_eq!(left.streams[0].space_class, ComplexityClass::O1);
        assert_eq!(right.streams[0].space_class, ComplexityClass::ON);
        assert_ne!(left.streams[0].exact_space, right.streams[0].exact_space);
    }
}

fn analyze_file(path: &str) -> marigold_grammar::complexity::ProgramComplexity {
    let source =
        std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

#[test]
fn streaming_pipeline() {
    let result = analyze_file("tests/programs/streaming_pipeline.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
}

#[test]
fn color_palette() {
    let result = analyze_file("tests/programs/color_palette.marigold");
    assert_eq!(result.streams.len(), 1);
    // range(0, 255) is a literal constant source, so n is not variable — the whole
    // pipeline is O(1) even though it involves combinations(5). "n choose k is O(1)"
    // is correct here because n=255 is fixed at compile time; there is no asymptotic
    // growth. The constant-factor cost (which can be large) is visible in exact_time.
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert!(result.streams[0].collects_input);
}

#[test]
fn stateful_fold() {
    let result = analyze_file("tests/programs/stateful_fold.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
}

#[test]
fn multi_consumer() {
    let result = analyze_file("tests/programs/multi_consumer.marigold");
    assert_eq!(result.streams.len(), 2);

    // Stream 0: `digits.filter(is_even).return`
    // `digits = range(0, 10)` has a constant cardinality, so the filter step
    // operates on a fixed-size input → O(1) time.
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(1)").unwrap()
    );

    // Stream 1: `odd_digits.map(doubled_plus_ten).return`
    // `odd_digits = digits.filter(is_odd)` produces `Symbolic::Filtered(Constant(10))`,
    // which `try_evaluate()` returns None for (the filtered count is unknown at
    // compile time). The filter step over constant `digits` is O(1), but the map
    // step over the unknown-size `odd_digits` is O(n) — giving exact O(n) rather
    // than the old O(2n) (which incorrectly counted the filter as O(n) too).
    assert_eq!(result.streams[1].time_class, ComplexityClass::ON);
    assert_eq!(result.streams[1].exact_time.to_string(), "O(n)");
}

#[test]
fn select_all() {
    let result = analyze_file("tests/programs/select_all.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(30u64))
    );
}

#[test]
fn map_reports_o1_space() {
    let result = analyze_file("tests/programs/card_map.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_space,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    // range(0, 100) is a constant source; the map step is O(1) time.
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
}

#[test]
fn chained_maps() {
    let result = analyze_file("tests/programs/chained_maps.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
}

#[test]
fn var_permutations() {
    let result = analyze_file("tests/programs/var_permutations.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(
        result.streams[0].time_class,
        ComplexityClass::OPermutational(2)
    );
    assert!(result.streams[0].space_class > ComplexityClass::O1);
}
