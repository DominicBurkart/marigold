use marigold_grammar::complexity::Cardinality;
use num_bigint::BigUint;
use num_traits::One;

fn analyze_file(path: &str) -> marigold_grammar::complexity::ProgramComplexity {
    let source =
        std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

mod exact {
    use super::*;

    #[test]
    fn range() {
        let result = analyze_file("tests/programs/card_range.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(100u64))
        );
        assert_eq!(
            result.program_cardinality,
            Cardinality::Exact(BigUint::from(100u64))
        );
    }

    #[test]
    fn permutations() {
        let result = analyze_file("tests/programs/card_permutations.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(90u64))
        );
    }

    #[test]
    fn combinations() {
        let result = analyze_file("tests/programs/card_combinations.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(120u64))
        );
    }

    #[test]
    fn fold() {
        let result = analyze_file("tests/programs/card_fold.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::one())
        );
    }

    #[test]
    fn select_all() {
        let result = analyze_file("tests/programs/card_select_all.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(30u64))
        );
    }

    #[test]
    fn keep_first_n() {
        let result = analyze_file("tests/programs/card_keep_first_n.marigold");
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(5u64))
        );
    }
}

mod bounded {
    use super::*;

    #[test]
    fn filter() {
        let result = analyze_file("tests/programs/card_filter.marigold");
        assert!(
            matches!(result.streams[0].cardinality, Cardinality::Bounded(_)),
            "filter should produce bounded cardinality, got {:?}",
            result.streams[0].cardinality
        );
    }

    #[test]
    fn filter_map() {
        let result = analyze_file("tests/programs/card_filter_map.marigold");
        assert!(
            matches!(result.streams[0].cardinality, Cardinality::Bounded(_)),
            "filter_map should produce bounded cardinality, got {:?}",
            result.streams[0].cardinality
        );
    }

    #[test]
    fn filter_then_keep_first_n() {
        let result = analyze_file("tests/programs/card_filter_keep_first_n.marigold");
        assert!(
            matches!(result.streams[0].cardinality, Cardinality::Bounded(_)),
            "filter+keep_first_n should produce bounded cardinality, got {:?}",
            result.streams[0].cardinality
        );
    }
}

mod unknown {
    use super::*;

    #[test]
    fn read_file() {
        let result = analyze_file("tests/programs/card_read_file.marigold");
        assert_eq!(result.streams[0].cardinality, Cardinality::Unknown);
    }

    #[test]
    fn read_file_map_propagates() {
        let result = analyze_file("tests/programs/card_read_file_map.marigold");
        assert_eq!(result.streams[0].cardinality, Cardinality::Unknown);
    }
}

mod left_right {
    use super::*;

    #[test]
    fn same_ops_different_input_sizes() {
        let left = analyze_file("tests/programs/card_left_exact.marigold");
        let right = analyze_file("tests/programs/card_right_exact.marigold");

        assert_eq!(
            left.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(50u64))
        );
        assert_eq!(
            right.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(200u64))
        );
        assert_ne!(left.streams[0].cardinality, right.streams[0].cardinality);
    }

    #[test]
    fn filter_reduces_exact_to_bounded() {
        let exact = analyze_file("tests/programs/card_range.marigold");
        let bounded = analyze_file("tests/programs/card_filter.marigold");

        assert!(matches!(
            exact.streams[0].cardinality,
            Cardinality::Exact(_)
        ));
        assert!(matches!(
            bounded.streams[0].cardinality,
            Cardinality::Bounded(_)
        ));
    }
}

mod multi_consumer {
    use super::*;

    #[test]
    fn stream_variable_cardinality_tracking() {
        let result = analyze_file("tests/programs/card_multi_consumer.marigold");
        assert_eq!(result.streams.len(), 2);

        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(10u64))
        );

        assert!(
            matches!(result.streams[1].cardinality, Cardinality::Bounded(_)),
            "filtered stream should be bounded, got {:?}",
            result.streams[1].cardinality
        );
    }

    #[test]
    fn program_max_cardinality() {
        let result = analyze_file("tests/programs/card_multi_consumer.marigold");
        assert!(
            matches!(result.program_cardinality, Cardinality::Bounded(_)),
            "program cardinality should be bounded (max of exact and bounded), got {:?}",
            result.program_cardinality
        );
    }
}
