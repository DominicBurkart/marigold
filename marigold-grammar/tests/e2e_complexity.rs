use std::str::FromStr;

use marigold_grammar::complexity::{Cardinality, ComplexityClass, ExactComplexity};
use num_bigint::BigUint;

mod left_right {
    use super::*;

    #[test]
    fn linear_same_simplified_different_exact() {
        let left = analyze_file("tests/programs/left_linear.marigold");
        let right = analyze_file("tests/programs/right_linear.marigold");

        assert_eq!(left.streams[0].time_class, ComplexityClass::ON);
        assert_eq!(right.streams[0].time_class, ComplexityClass::ON);
        assert_eq!(left.streams[0].time_class, right.streams[0].time_class);

        assert_eq!(
            left.streams[0].exact_time,
            ExactComplexity::from_str("O(n)").unwrap()
        );
        assert_eq!(
            right.streams[0].exact_time,
            ExactComplexity::from_str("O(3n)").unwrap()
        );
        assert_ne!(left.streams[0].exact_time, right.streams[0].exact_time);
    }

    #[test]
    fn collecting_same_simplified_different_exact() {
        let left = analyze_file("tests/programs/left_quadratic.marigold");
        let right = analyze_file("tests/programs/right_quadratic.marigold");

        assert_eq!(
            left.streams[0].time_class,
            ComplexityClass::OPermutational(2)
        );
        assert_eq!(
            right.streams[0].time_class,
            ComplexityClass::OPermutational(2)
        );
        assert_eq!(left.streams[0].time_class, right.streams[0].time_class);

        assert_eq!(left.streams[0].exact_time.to_string(), "O(n!/(n-2)! + n)");
        assert_eq!(right.streams[0].exact_time.to_string(), "O(n!/(n-2)! + 3n)");
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
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2n)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
}

#[test]
fn color_palette() {
    let result = analyze_file("tests/programs/color_palette.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(
        result.streams[0].time_class,
        ComplexityClass::OCombinatorial(5)
    );
    assert!(result.streams[0].collects_input);
}

#[test]
fn stateful_fold() {
    let result = analyze_file("tests/programs/stateful_fold.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(n)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
}

#[test]
fn multi_consumer() {
    let result = analyze_file("tests/programs/multi_consumer.marigold");
    assert_eq!(result.streams.len(), 2);

    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(n)").unwrap()
    );

    assert_eq!(result.streams[1].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[1].exact_time,
        ExactComplexity::from_str("O(2n)").unwrap()
    );
}

#[test]
fn select_all() {
    let result = analyze_file("tests/programs/select_all.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
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
fn take_while_reports_o1_space() {
    let result = analyze_file("tests/programs/card_take_while.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    assert_eq!(
        result.streams[0].exact_space,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
}

#[test]
fn take_while_pipeline() {
    // Note: `take_while_pipeline.marigold` and `card_take_while_map.marigold` contain
    // identical program source by coincidence (same operations, different range).
    // They are read by different test harnesses (complexity vs. cardinality) and are
    // intentionally kept as separate files — a content change to one does not imply
    // a change to the other.
    let result = analyze_file("tests/programs/take_while_pipeline.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2n)").unwrap()
    );
    assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    // exact_space is O(2) because both take_while and map each contribute O(1)
    assert_eq!(
        result.streams[0].exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
}

#[test]
fn chained_maps() {
    let result = analyze_file("tests/programs/chained_maps.marigold");
    assert_eq!(result.streams.len(), 1);
    assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
    assert_eq!(
        result.streams[0].exact_time,
        ExactComplexity::from_str("O(2n)").unwrap()
    );
}
