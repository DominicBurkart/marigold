use std::str::FromStr;
use std::time::{Duration, Instant};

use marigold_grammar::complexity::{Cardinality, Symbolic};

const DEPTH: usize = 16;
const BUDGET: Duration = Duration::from_secs(2);

fn nested(open: &str, leaf: &str, close: &str, depth: usize) -> String {
    format!("{}{leaf}{}", open.repeat(depth), close.repeat(depth))
}

fn parse_within_budget(text: &str) -> Symbolic {
    let start = Instant::now();
    let parsed = Symbolic::from_str(text).unwrap_or_else(|e| panic!("{text}: {e}"));
    let cardinality = Cardinality::from_str(text).unwrap_or_else(|e| panic!("{text}: {e}"));
    let elapsed = start.elapsed();
    assert!(
        elapsed < BUDGET,
        "parsing a depth-{DEPTH} expression took {elapsed:?}; each nesting level must be parsed once, not retried"
    );
    assert_eq!(cardinality, Cardinality::Bounded(parsed.clone()));
    parsed
}

#[test]
fn deeply_nested_min_permutations_and_combinations_parse_in_linear_time() {
    for (open, close) in [("min(", ", 1)"), ("P(", ", 2)"), ("C(", ", 2)")] {
        for leaf in ["1", "$n", "?"] {
            let text = nested(open, leaf, close, DEPTH);
            let parsed = parse_within_budget(&text);
            assert_eq!(parsed.to_string(), text);
        }
    }
}

#[test]
fn deeply_nested_powers_sums_and_filters_parse_in_linear_time() {
    for text in [
        nested("min(", "$n", ", 1)^2", DEPTH),
        nested("P(", "$n", " + 1, 2)", DEPTH),
        nested("C(\u{2264}", "$n", ", 2)^2", DEPTH / 2),
        nested("min(1 + ", "$n", ", 3 + 4)", DEPTH),
    ] {
        let parsed = parse_within_budget(&text);
        assert_eq!(parsed.to_string(), text);
    }
}

#[test]
fn a_nested_expression_parses_into_the_expected_tree() {
    let parsed = parse_within_budget(&nested("P(", "$n", ", 2)", 3));
    let mut expected: Symbolic = "$n".parse().unwrap();
    for _ in 0..3 {
        expected = Symbolic::Permutations {
            n: Box::new(expected),
            k: 2,
        };
    }
    assert_eq!(parsed, expected);
    assert_eq!(
        Symbolic::from_str("min(1, 2)^2^3").unwrap(),
        Symbolic::PermutationsWithReplacement {
            n: Box::new(Symbolic::PermutationsWithReplacement {
                n: Box::new(Symbolic::from_str("min(1, 2)").unwrap()),
                k: 2,
            }),
            k: 3,
        }
    );
    assert!(matches!(
        Symbolic::from_str("1 + 2 + 3").unwrap(),
        Symbolic::Sum(parts) if parts.len() == 3
    ));
}
