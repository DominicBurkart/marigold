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
    // color_palette.marigold sources from range(0, 255), which has a compile-time-constant
    // cardinality. Even though the pipeline contains combinations(5) (n-choose-k), there is
    // no variable n -- the input size is a fixed constant. By the definition of big-O,
    // any function of a constant is O(1). The exact coefficient (ExactComplexity) still
    // captures the concrete step count for profiling, but the asymptotic class is O(1).
    let result = analyze_file("tests/programs/color_palette.marigold");
    assert_eq!(result.streams.len(), 1);
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
    // multi_consumer.marigold defines:
    //   digits = range(0, 10)                            -- constant-cardinality source
    //   stream[0]: digits.filter(is_even).return         -- O(1): constant-input early return
    //   stream[1]: digits.filter(is_odd).map(...).return -- O(n): map on Filtered cardinality
    //
    // stream[0] is O(1) because the entire pipeline resolves to a compile-time constant:
    // range(0,10) has a known cardinality, and step_work_class short-circuits to O(1) via
    // the constant-input early return (try_evaluate().is_some()).
    //
    // stream[1] is O(n) because odd_digits = digits.filter(is_odd) has Symbolic::Filtered
    // cardinality -- try_evaluate() returns None for Filtered, so the early return does NOT
    // apply. The downstream map step therefore inherits the O(n) class from the filtered
    // (unknown-at-compile-time) source.
    //
    // The exact_time for stream[1] is O(n), not O(2n): the filter step operates on the
    // constant-cardinality range(0,10) and is correctly classified O(1); only the map step
    // on the filtered (Symbolic::Filtered) output contributes an O(n) term. This is expected
    // behaviour introduced by the constant-input fix and is not a regression.
    let result = analyze_file("tests/programs/multi_consumer.marigold");
    assert_eq!(result.streams.len(), 2);

    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(1)").unwrap()
    );

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
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
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
