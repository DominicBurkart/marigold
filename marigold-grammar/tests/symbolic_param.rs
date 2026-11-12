use std::collections::BTreeMap;
use std::str::FromStr;

use marigold_grammar::complexity::{Cardinality, ComplexityClass, SizeBindings, Symbolic};
use num_bigint::BigUint;
use proptest::prelude::*;

fn param(name: &str) -> Symbolic {
    Symbolic::Param(name.to_string())
}

fn constant(value: u64) -> Symbolic {
    Symbolic::Constant(BigUint::from(value))
}

fn bind(pairs: &[(&str, u64)]) -> SizeBindings {
    pairs
        .iter()
        .map(|(name, value)| ((*name).to_string(), BigUint::from(*value)))
        .collect()
}

fn big(value: u64) -> Option<BigUint> {
    Some(BigUint::from(value))
}

fn permutations(n: Symbolic, k: u64) -> Symbolic {
    Symbolic::Permutations { n: Box::new(n), k }
}

fn combinations(n: Symbolic, k: u64) -> Symbolic {
    Symbolic::Combinations { n: Box::new(n), k }
}

fn with_replacement(n: Symbolic, k: u64) -> Symbolic {
    Symbolic::PermutationsWithReplacement { n: Box::new(n), k }
}

fn min(a: Symbolic, b: Symbolic) -> Symbolic {
    Symbolic::Min(Box::new(a), Box::new(b))
}

fn filtered(inner: Symbolic) -> Symbolic {
    Symbolic::Filtered(Box::new(inner))
}

#[test]
fn a_param_displays_with_a_sigil() {
    assert_eq!(param("n").to_string(), "$n");
    assert_eq!(param("rows_2").to_string(), "$rows_2");
}

#[test]
fn param_notation_round_trips_through_every_combinator() {
    for text in [
        "$n",
        "$rows",
        "P($n, 3)",
        "C($n, 2)",
        "$n^3",
        "min($n, 5)",
        "\u{2264}$n",
        "\u{2264}P($n, 2)",
        "$n + 4",
        "min(P($n, 3), 100)",
        "C(min($n, 9), 4)",
        "P(\u{2264}$n, 2)",
    ] {
        let parsed = Symbolic::from_str(text).unwrap_or_else(|e| panic!("{text}: {e}"));
        assert_eq!(parsed.to_string(), text);
    }
}

#[test]
fn param_parses_into_the_expected_tree() {
    assert_eq!(Symbolic::from_str("$n").unwrap(), param("n"));
    assert_eq!(
        Symbolic::from_str("P($n, 3)").unwrap(),
        permutations(param("n"), 3)
    );
    assert_eq!(
        Symbolic::from_str("$n^3").unwrap(),
        with_replacement(param("n"), 3)
    );
    assert_eq!(
        Symbolic::from_str("min($n, 5)").unwrap(),
        min(param("n"), constant(5))
    );
}

#[test]
fn malformed_param_notation_is_rejected() {
    for text in ["$", "$1n", "$ n", "n", "$n^", "$$n", "$n-", "P($, 3)"] {
        assert!(
            Symbolic::from_str(text).is_err(),
            "{text:?} should not parse"
        );
    }
}

#[test]
fn existing_notation_still_parses_to_the_same_trees() {
    assert_eq!(Symbolic::from_str("5").unwrap(), constant(5));
    assert_eq!(Symbolic::from_str("?").unwrap(), Symbolic::Unknown);
    assert_eq!(
        Symbolic::from_str("\u{2264}3").unwrap(),
        filtered(constant(3))
    );
    assert_eq!(
        Symbolic::from_str("P(10, 3)").unwrap(),
        permutations(constant(10), 3)
    );
    assert_eq!(
        Symbolic::from_str("2^8").unwrap(),
        with_replacement(constant(2), 8)
    );
    assert_eq!(
        Symbolic::from_str("3 + 4").unwrap(),
        Symbolic::Sum(vec![constant(3), constant(4)])
    );
    assert_eq!(
        Symbolic::from_str("C(\u{2264}7, 2)").unwrap(),
        combinations(filtered(constant(7)), 2)
    );
}

#[test]
fn an_unbound_param_does_not_evaluate_and_a_bound_one_does() {
    assert_eq!(param("n").evaluate(&SizeBindings::new()), None);
    assert_eq!(param("n").evaluate(&bind(&[("m", 4)])), None);
    assert_eq!(param("n").evaluate(&bind(&[("n", 4)])), big(4));
}

#[test]
fn evaluation_matches_hand_computed_counts_for_small_sizes() {
    let cases: Vec<(Symbolic, [u64; 6])> = vec![
        (permutations(param("n"), 3), [0, 0, 0, 6, 24, 60]),
        (combinations(param("n"), 2), [0, 0, 1, 3, 6, 10]),
        (with_replacement(param("n"), 3), [0, 1, 8, 27, 64, 125]),
        (min(param("n"), constant(3)), [0, 1, 2, 3, 3, 3]),
        (
            Symbolic::Sum(vec![param("n"), constant(2)]),
            [2, 3, 4, 5, 6, 7],
        ),
        (permutations(param("n"), 0), [1, 1, 1, 1, 1, 1]),
        (combinations(param("n"), 0), [1, 1, 1, 1, 1, 1]),
    ];
    for (expression, expected) in cases {
        for (n, want) in expected.iter().enumerate() {
            assert_eq!(
                expression.evaluate(&bind(&[("n", n as u64)])),
                big(*want),
                "{expression} at n={n}"
            );
        }
    }
}

#[test]
fn a_filtered_size_has_an_upper_bound_but_no_exact_value() {
    let expression = filtered(param("n"));
    let bindings = bind(&[("n", 7)]);
    assert_eq!(expression.evaluate(&bindings), None);
    assert_eq!(expression.upper_bound_with(&bindings), big(7));
    assert_eq!(expression.upper_bound(), None);
}

#[test]
fn unknown_stays_unknown_under_bindings() {
    let expression = Symbolic::Sum(vec![param("n"), Symbolic::Unknown]);
    let bindings = bind(&[("n", 3)]);
    assert_eq!(expression.evaluate(&bindings), None);
    assert_eq!(expression.upper_bound_with(&bindings), None);
}

#[test]
fn a_min_upper_bound_takes_the_smaller_side_under_bindings() {
    let expression = min(filtered(param("n")), constant(4));
    assert_eq!(expression.upper_bound_with(&bind(&[("n", 3)])), big(3));
    assert_eq!(expression.upper_bound_with(&bind(&[("n", 9)])), big(4));
    assert_eq!(expression.upper_bound_with(&bind(&[("n", 4)])), big(4));
}

#[test]
fn min_with_an_unbounded_side_is_bounded_by_the_other_side() {
    let expression = min(Symbolic::Unknown, constant(4));
    assert_eq!(expression.upper_bound_with(&SizeBindings::new()), big(4));
    assert_eq!(expression.evaluate(&SizeBindings::new()), None);
    let bounded_by_param = min(param("n"), constant(4));
    assert_eq!(
        bounded_by_param.upper_bound_with(&SizeBindings::new()),
        big(4)
    );
}

#[test]
fn a_param_classifies_like_a_filtered_constant_not_like_an_unknown() {
    let n = param("n");
    assert_eq!(n.classify_as_time(), ComplexityClass::O1);
    assert!(!n.contains_unknown());
    assert_eq!(n.upper_bound(), None);
    assert_eq!(n.try_evaluate(), None);
    assert_eq!(n.classify_as_cardinality(), Cardinality::Bounded(n.clone()));
    assert_eq!(
        permutations(n.clone(), 3).classify_as_time(),
        ComplexityClass::OPermutational(3)
    );
    assert_eq!(
        combinations(n.clone(), 2).classify_as_time(),
        ComplexityClass::OCombinatorial(2)
    );
    assert_eq!(
        with_replacement(n, 4).classify_as_time(),
        ComplexityClass::OPolynomial(4)
    );
}

#[test]
fn the_existing_entry_points_agree_with_evaluation_under_empty_bindings() {
    let empty = SizeBindings::new();
    for expression in [
        constant(7),
        Symbolic::Unknown,
        filtered(constant(7)),
        permutations(constant(5), 2),
        combinations(constant(6), 3),
        with_replacement(constant(3), 3),
        min(constant(3), filtered(constant(9))),
        Symbolic::Sum(vec![constant(2), filtered(constant(5))]),
    ] {
        assert_eq!(expression.evaluate(&empty), expression.try_evaluate());
        assert_eq!(
            expression.upper_bound_with(&empty),
            expression.upper_bound()
        );
    }
}

fn param_names() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("n".to_string()),
        Just("m".to_string()),
        Just("rows".to_string()),
        Just("x_1".to_string()),
    ]
}

fn leaf() -> impl Strategy<Value = Symbolic> {
    prop_oneof![
        (0u64..500).prop_map(constant),
        param_names().prop_map(Symbolic::Param),
        Just(Symbolic::Unknown),
    ]
}

fn replacement_base() -> impl Strategy<Value = Symbolic> {
    prop_oneof![
        (0u64..30).prop_map(constant),
        param_names().prop_map(Symbolic::Param),
    ]
}

fn atom(inner: BoxedStrategy<Symbolic>) -> BoxedStrategy<Symbolic> {
    prop_oneof![
        leaf(),
        (replacement_base(), 0u64..5).prop_map(|(n, k)| with_replacement(n, k)),
        (inner.clone(), 0u64..5).prop_map(|(n, k)| permutations(n, k)),
        (inner.clone(), 0u64..5).prop_map(|(n, k)| combinations(n, k)),
        (inner.clone(), inner).prop_map(|(a, b)| min(a, b)),
    ]
    .boxed()
}

fn canonical_symbolic() -> BoxedStrategy<Symbolic> {
    let leaf_expression = leaf().boxed();
    leaf_expression
        .prop_recursive(3, 24, 3, |inner| {
            let atoms = atom(inner);
            prop_oneof![
                atoms.clone(),
                atoms.clone().prop_map(filtered),
                prop::collection::vec(atoms, 2..4).prop_map(Symbolic::Sum),
            ]
        })
        .boxed()
}

fn substitute(expression: &Symbolic, bindings: &SizeBindings) -> Symbolic {
    match expression {
        Symbolic::Param(name) => match bindings.get(name) {
            Some(value) => Symbolic::Constant(value.clone()),
            None => expression.clone(),
        },
        Symbolic::Constant(_) | Symbolic::Unknown => expression.clone(),
        Symbolic::Filtered(inner) => filtered(substitute(inner, bindings)),
        Symbolic::Permutations { n, k } => permutations(substitute(n, bindings), *k),
        Symbolic::PermutationsWithReplacement { n, k } => {
            with_replacement(substitute(n, bindings), *k)
        }
        Symbolic::Combinations { n, k } => combinations(substitute(n, bindings), *k),
        Symbolic::Min(a, b) => min(substitute(a, bindings), substitute(b, bindings)),
        Symbolic::Sum(parts) => {
            Symbolic::Sum(parts.iter().map(|p| substitute(p, bindings)).collect())
        }
    }
}

fn bindings_strategy() -> impl Strategy<Value = SizeBindings> {
    prop::collection::btree_map(param_names(), (0u64..9).prop_map(BigUint::from), 0..4)
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(512))]

    #[test]
    fn display_then_parse_is_the_identity_on_canonical_trees(expression in canonical_symbolic()) {
        let text = expression.to_string();
        let parsed = Symbolic::from_str(&text)
            .unwrap_or_else(|e| panic!("{text}: {e}"));
        prop_assert_eq!(parsed, expression);
    }

    #[test]
    fn evaluating_equals_substituting_then_evaluating(
        expression in canonical_symbolic(),
        bindings in bindings_strategy(),
    ) {
        let substituted = substitute(&expression, &bindings);
        prop_assert_eq!(expression.evaluate(&bindings), substituted.try_evaluate());
        prop_assert_eq!(
            expression.upper_bound_with(&bindings),
            substituted.upper_bound()
        );
    }

    #[test]
    fn empty_bindings_agree_with_the_original_entry_points(expression in canonical_symbolic()) {
        let empty: SizeBindings = BTreeMap::new();
        prop_assert_eq!(expression.evaluate(&empty), expression.try_evaluate());
        prop_assert_eq!(expression.upper_bound_with(&empty), expression.upper_bound());
    }

    #[test]
    fn a_bound_value_never_exceeds_its_upper_bound(
        expression in canonical_symbolic(),
        bindings in bindings_strategy(),
    ) {
        if let (Some(exact), Some(bound)) = (
            expression.evaluate(&bindings),
            expression.upper_bound_with(&bindings),
        ) {
            prop_assert!(exact <= bound);
        }
    }

    #[test]
    fn evaluation_is_monotone_in_the_size(
        expression in canonical_symbolic(),
        low in 0u64..9,
        step in 0u64..5,
    ) {
        let smaller: SizeBindings = ["n", "m", "rows", "x_1"]
            .iter()
            .map(|name| ((*name).to_string(), BigUint::from(low)))
            .collect();
        let larger: SizeBindings = ["n", "m", "rows", "x_1"]
            .iter()
            .map(|name| ((*name).to_string(), BigUint::from(low + step)))
            .collect();
        if let (Some(a), Some(b)) = (expression.evaluate(&smaller), expression.evaluate(&larger)) {
            prop_assert!(a <= b);
        }
        if let (Some(a), Some(b)) = (
            expression.upper_bound_with(&smaller),
            expression.upper_bound_with(&larger),
        ) {
            prop_assert!(a <= b);
        }
        if expression.upper_bound_with(&smaller).is_none() {
            prop_assert!(expression.upper_bound_with(&larger).is_none());
        }
    }
}
