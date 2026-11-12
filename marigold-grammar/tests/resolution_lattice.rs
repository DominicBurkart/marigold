use marigold_grammar::complexity::{SizeBindings, SizeName, Symbolic};
use marigold_grammar::nodes::StreamFunctionKind;
use marigold_grammar::sizes::{InvalidResolution, ReasonCode, Resolution, ResolutionKind};
use num_bigint::BigUint;
use num_traits::{One, ToPrimitive, Zero};
use proptest::prelude::*;
use serde_json::json;

fn constant(value: u64) -> Symbolic {
    Symbolic::Constant(BigUint::from(value))
}

fn param(name: &str) -> Symbolic {
    Symbolic::Param(SizeName::new(name).unwrap())
}

fn bind(n: u64) -> SizeBindings {
    SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(n))])
}

fn exact(value: u64) -> Resolution {
    Resolution::of_source(&constant(value))
}

fn symbolic_n() -> Resolution {
    Resolution::of_source(&param("n"))
}

fn unbound() -> Resolution {
    Resolution::of_source(&Symbolic::Unknown)
}

fn undeclared() -> Resolution {
    Resolution::undeclared()
}

fn big(value: u64) -> Option<BigUint> {
    Some(BigUint::from(value))
}

fn all_kinds() -> Vec<StreamFunctionKind> {
    vec![
        StreamFunctionKind::Map,
        StreamFunctionKind::OkOrPanic,
        StreamFunctionKind::Filter,
        StreamFunctionKind::FilterMap,
        StreamFunctionKind::Ok,
        StreamFunctionKind::Permutations(3),
        StreamFunctionKind::PermutationsWithReplacement(2),
        StreamFunctionKind::Combinations(2),
        StreamFunctionKind::KeepFirstN(5),
        StreamFunctionKind::Fold,
    ]
}

#[test]
fn every_stream_function_kind_has_a_transfer_rule() {
    for kind in all_kinds() {
        match kind {
            StreamFunctionKind::Map
            | StreamFunctionKind::OkOrPanic
            | StreamFunctionKind::Filter
            | StreamFunctionKind::FilterMap
            | StreamFunctionKind::Ok
            | StreamFunctionKind::Permutations(_)
            | StreamFunctionKind::PermutationsWithReplacement(_)
            | StreamFunctionKind::Combinations(_)
            | StreamFunctionKind::KeepFirstN(_)
            | StreamFunctionKind::Fold => {}
        }
        for source in [exact(4), symbolic_n(), unbound(), undeclared()] {
            let _ = source.after(&kind);
        }
    }
}

#[test]
fn sources_resolve_as_documented() {
    let constant_source = exact(7);
    assert_eq!(constant_source.kind(), ResolutionKind::Exact);
    assert_eq!(constant_source.lo(), &constant(7));
    assert_eq!(constant_source.hi(), Some(&constant(7)));
    assert!(constant_source.reasons().is_empty());

    let sized = symbolic_n();
    assert_eq!(sized.kind(), ResolutionKind::Symbolic);
    assert_eq!(sized.lo(), &param("n"));
    assert_eq!(sized.hi(), Some(&param("n")));
    assert!(sized.reasons().is_empty());

    let missing = unbound();
    assert_eq!(missing.kind(), ResolutionKind::Unresolvable);
    assert_eq!(missing.lo(), &constant(0));
    assert_eq!(missing.hi(), None);
    assert_eq!(missing.reasons(), &[ReasonCode::UnboundInput]);

    let ghost = undeclared();
    assert_eq!(ghost.kind(), ResolutionKind::Unresolvable);
    assert_eq!(ghost.hi(), None);
    assert_eq!(ghost.reasons(), &[ReasonCode::UndeclaredStreamVariable]);
}

#[test]
fn measure_preserving_stages_pass_the_resolution_through() {
    for source in [exact(4), symbolic_n(), unbound(), undeclared()] {
        assert_eq!(source.after(&StreamFunctionKind::Map), source);
        assert_eq!(source.after(&StreamFunctionKind::OkOrPanic), source);
    }
}

#[test]
fn filtering_stages_keep_the_upper_bound_and_lose_the_lower_bound() {
    for (kind, reason) in [
        (StreamFunctionKind::Filter, ReasonCode::OpaquePredicate),
        (StreamFunctionKind::FilterMap, ReasonCode::OpaquePredicate),
        (StreamFunctionKind::Ok, ReasonCode::FallibleItems),
    ] {
        let from_constant = exact(9).after(&kind);
        assert_eq!(from_constant.kind(), ResolutionKind::Bounded);
        assert_eq!(from_constant.lo(), &constant(0));
        assert_eq!(from_constant.hi(), Some(&constant(9)));
        assert_eq!(from_constant.reasons(), &[reason]);

        let from_symbolic = symbolic_n().after(&kind);
        assert_eq!(from_symbolic.kind(), ResolutionKind::Bounded);
        assert_eq!(from_symbolic.lo(), &constant(0));
        assert_eq!(from_symbolic.hi(), Some(&param("n")));
        assert_eq!(from_symbolic.reasons(), &[reason]);

        let from_unbound = unbound().after(&kind);
        assert_eq!(from_unbound.kind(), ResolutionKind::Unresolvable);
        assert_eq!(from_unbound.hi(), None);
        assert_eq!(from_unbound.reasons(), &[ReasonCode::UnboundInput, reason]);
    }
}

#[test]
fn reasons_are_deduplicated_and_ordered() {
    let twice = exact(9)
        .after(&StreamFunctionKind::Filter)
        .after(&StreamFunctionKind::Filter)
        .after(&StreamFunctionKind::Ok);
    assert_eq!(
        twice.reasons(),
        &[ReasonCode::OpaquePredicate, ReasonCode::FallibleItems]
    );
}

#[test]
fn reasons_are_ordered_by_code_not_by_arrival() {
    let fallible_first = exact(9)
        .after(&StreamFunctionKind::Ok)
        .after(&StreamFunctionKind::Filter);
    assert_eq!(
        fallible_first.reasons(),
        &[ReasonCode::OpaquePredicate, ReasonCode::FallibleItems]
    );
    let built = Resolution::checked(
        constant(0),
        None,
        vec![
            ReasonCode::FallibleItems,
            ReasonCode::UnboundInput,
            ReasonCode::OpaquePredicate,
            ReasonCode::UndeclaredStreamVariable,
        ],
    )
    .unwrap();
    assert_eq!(
        built.reasons(),
        &[
            ReasonCode::UnboundInput,
            ReasonCode::UndeclaredStreamVariable,
            ReasonCode::OpaquePredicate,
            ReasonCode::FallibleItems,
        ]
    );
}

#[test]
fn a_resolution_serializes_its_fields_in_a_fixed_order() {
    let filtered = symbolic_n().after(&StreamFunctionKind::Filter);
    assert_eq!(
        serde_json::to_string(&filtered).unwrap(),
        r#"{"kind":"bounded","lo":"0","hi":"$n","reasons":["opaque_predicate"]}"#
    );
}

#[test]
fn combinatorial_stages_apply_to_both_bounds() {
    let p = symbolic_n().after(&StreamFunctionKind::Permutations(3));
    assert_eq!(p.kind(), ResolutionKind::Symbolic);
    assert_eq!(
        p.lo(),
        &Symbolic::Permutations {
            n: Box::new(param("n")),
            k: 3
        }
    );
    assert_eq!(p.lo(), p.hi().unwrap());

    let filtered = symbolic_n()
        .after(&StreamFunctionKind::Filter)
        .after(&StreamFunctionKind::Combinations(2));
    assert_eq!(filtered.kind(), ResolutionKind::Bounded);
    assert_eq!(filtered.lo(), &constant(0));
    assert_eq!(
        filtered.hi(),
        Some(&Symbolic::Combinations {
            n: Box::new(param("n")),
            k: 2
        })
    );
    assert_eq!(filtered.reasons(), &[ReasonCode::OpaquePredicate]);

    let concrete = exact(5).after(&StreamFunctionKind::PermutationsWithReplacement(2));
    assert_eq!(concrete.kind(), ResolutionKind::Exact);
    assert_eq!(concrete.evaluate(&SizeBindings::new()).lo, big(25));

    for kind in [
        StreamFunctionKind::Permutations(3),
        StreamFunctionKind::PermutationsWithReplacement(2),
        StreamFunctionKind::Combinations(2),
    ] {
        let unresolved = unbound().after(&kind);
        assert_eq!(unresolved.kind(), ResolutionKind::Unresolvable);
        assert_eq!(unresolved.hi(), None);
        assert_eq!(unresolved.reasons(), &[ReasonCode::UnboundInput]);
    }
}

#[test]
fn keep_first_n_caps_the_upper_bound_even_when_the_input_is_unbounded() {
    let capped = unbound().after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(capped.kind(), ResolutionKind::Bounded);
    assert_eq!(capped.lo(), &constant(0));
    assert_eq!(capped.hi(), Some(&constant(5)));
    assert_eq!(capped.reasons(), &[ReasonCode::UnboundInput]);

    let over_symbolic = symbolic_n().after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(over_symbolic.kind(), ResolutionKind::Symbolic);
    let min_n_5 = Symbolic::Min(Box::new(param("n")), Box::new(constant(5)));
    assert_eq!(over_symbolic.lo(), &min_n_5);
    assert_eq!(over_symbolic.hi(), Some(&min_n_5));

    let over_constant = exact(3).after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(over_constant.kind(), ResolutionKind::Exact);
    assert_eq!(over_constant.evaluate(&SizeBindings::new()).lo, big(3));
    let over_big_constant = exact(30).after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(over_big_constant.evaluate(&SizeBindings::new()).hi, big(5));
}

#[test]
fn fold_is_exactly_one_item_whatever_the_input_and_regains_precision() {
    for source in [
        exact(0),
        exact(4),
        symbolic_n(),
        unbound(),
        undeclared(),
        symbolic_n().after(&StreamFunctionKind::Filter),
    ] {
        let folded = source.after(&StreamFunctionKind::Fold);
        assert_eq!(folded.kind(), ResolutionKind::Exact);
        assert_eq!(folded.lo(), &constant(1));
        assert_eq!(folded.hi(), Some(&constant(1)));
        assert!(folded.reasons().is_empty());
    }
}

#[test]
fn an_unbounded_input_stays_unbounded_except_through_a_stated_rule() {
    for kind in all_kinds() {
        let after = unbound().after(&kind);
        let stated_rule = matches!(
            kind,
            StreamFunctionKind::KeepFirstN(_) | StreamFunctionKind::Fold
        );
        assert_eq!(after.hi().is_some(), stated_rule, "{kind:?}");
    }
}

#[test]
fn evaluation_gives_ranges_under_bindings() {
    let filtered = symbolic_n().after(&StreamFunctionKind::Filter);
    let range = filtered.evaluate(&bind(7));
    assert_eq!((range.lo, range.hi), (big(0), big(7)));

    let unbound_range = filtered.evaluate(&SizeBindings::new());
    assert_eq!((unbound_range.lo, unbound_range.hi), (big(0), None));

    let permuted = symbolic_n().after(&StreamFunctionKind::Permutations(3));
    for (n, want) in [(0, 0), (1, 0), (2, 0), (3, 6), (5, 60)] {
        let range = permuted.evaluate(&bind(n));
        assert_eq!((range.lo, range.hi), (big(want), big(want)), "n={n}");
    }
}

#[test]
fn the_machine_readable_codes_are_stable_strings() {
    assert_eq!(
        serde_json::to_value(ReasonCode::UnboundInput).unwrap(),
        json!("unbound_input")
    );
    assert_eq!(
        serde_json::to_value(ReasonCode::UndeclaredStreamVariable).unwrap(),
        json!("undeclared_stream_variable")
    );
    assert_eq!(
        serde_json::to_value(ReasonCode::OpaquePredicate).unwrap(),
        json!("opaque_predicate")
    );
    assert_eq!(
        serde_json::to_value(ReasonCode::FallibleItems).unwrap(),
        json!("fallible_items")
    );
    assert_eq!(
        serde_json::to_value(ResolutionKind::Exact).unwrap(),
        json!("exact")
    );
    assert_eq!(
        serde_json::to_value(ResolutionKind::Symbolic).unwrap(),
        json!("symbolic")
    );
    assert_eq!(
        serde_json::to_value(ResolutionKind::Bounded).unwrap(),
        json!("bounded")
    );
    assert_eq!(
        serde_json::to_value(ResolutionKind::Unresolvable).unwrap(),
        json!("unresolvable")
    );
}

#[test]
fn a_resolution_serializes_with_its_kind_bounds_and_reasons() {
    let filtered = symbolic_n().after(&StreamFunctionKind::Filter);
    assert_eq!(
        serde_json::to_value(&filtered).unwrap(),
        json!({
            "kind": "bounded",
            "lo": "0",
            "hi": "$n",
            "reasons": ["opaque_predicate"],
        })
    );
    assert_eq!(
        serde_json::to_value(unbound()).unwrap(),
        json!({
            "kind": "unresolvable",
            "lo": "0",
            "hi": null,
            "reasons": ["unbound_input"],
        })
    );
    assert_eq!(
        serde_json::to_value(exact(12)).unwrap(),
        json!({
            "kind": "exact",
            "lo": "12",
            "hi": "12",
            "reasons": [],
        })
    );
}

fn contains_forbidden(expression: &Symbolic) -> bool {
    match expression {
        Symbolic::Filtered(_) | Symbolic::Unknown | Symbolic::Sum(_) => true,
        Symbolic::Constant(_) | Symbolic::Param(_) => false,
        Symbolic::Permutations { n, .. }
        | Symbolic::PermutationsWithReplacement { n, .. }
        | Symbolic::Combinations { n, .. } => contains_forbidden(n),
        Symbolic::Min(a, b) => contains_forbidden(a) || contains_forbidden(b),
    }
}

fn kind_strategy() -> impl Strategy<Value = StreamFunctionKind> {
    prop_oneof![
        Just(StreamFunctionKind::Map),
        Just(StreamFunctionKind::OkOrPanic),
        Just(StreamFunctionKind::Filter),
        Just(StreamFunctionKind::FilterMap),
        Just(StreamFunctionKind::Ok),
        (1u64..4).prop_map(StreamFunctionKind::Permutations),
        (1u64..4).prop_map(StreamFunctionKind::PermutationsWithReplacement),
        (1u64..4).prop_map(StreamFunctionKind::Combinations),
        (1u64..7).prop_map(StreamFunctionKind::KeepFirstN),
        Just(StreamFunctionKind::Fold),
    ]
}

fn source_strategy() -> impl Strategy<Value = Resolution> {
    prop_oneof![
        (0u64..13).prop_map(exact),
        Just(symbolic_n()),
        Just(unbound()),
        Just(undeclared()),
    ]
}

fn reference_falling_factorial(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    let mut factor = n.clone();
    for _ in 0..k {
        if factor.is_zero() {
            return BigUint::zero();
        }
        result *= &factor;
        factor -= 1u32;
    }
    result
}

fn reference_binomial(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    for i in 0..k {
        if n < &BigUint::from(i) {
            return BigUint::zero();
        }
        let numerator = n - BigUint::from(i);
        if numerator.is_zero() {
            return BigUint::zero();
        }
        result = result * numerator / BigUint::from(i + 1);
    }
    result
}

fn reference_power(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    for _ in 0..k {
        result *= n;
    }
    result
}

fn reference_count(count: &BigUint, kind: &StreamFunctionKind, choice: u64) -> BigUint {
    match kind {
        StreamFunctionKind::Map | StreamFunctionKind::OkOrPanic => count.clone(),
        StreamFunctionKind::Filter | StreamFunctionKind::FilterMap | StreamFunctionKind::Ok => {
            BigUint::from(choice) % (count + 1u32)
        }
        StreamFunctionKind::Permutations(k) => reference_falling_factorial(count, *k),
        StreamFunctionKind::PermutationsWithReplacement(k) => reference_power(count, *k),
        StreamFunctionKind::Combinations(k) => reference_binomial(count, *k),
        StreamFunctionKind::KeepFirstN(k) => count.clone().min(BigUint::from(*k)),
        StreamFunctionKind::Fold => BigUint::one(),
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1024))]

    #[test]
    fn every_possible_selectivity_lands_inside_the_resolved_range(
        stages in prop::collection::vec((kind_strategy(), any::<u64>()), 0..5),
        n in 0u64..13,
    ) {
        let mut resolution = symbolic_n();
        let mut count = BigUint::from(n);
        for (kind, choice) in &stages {
            resolution = resolution.after(kind);
            count = reference_count(&count, kind, *choice);
        }
        let range = resolution.evaluate(&bind(n));
        let lo = range.lo.expect("a pipeline over a bound size always has a lower bound");
        let hi = range.hi.expect("a pipeline over a bound size always has an upper bound");
        prop_assert!(lo <= count, "lo {lo} > count {count}");
        prop_assert!(count <= hi, "count {count} > hi {hi}");
        if matches!(resolution.kind(), ResolutionKind::Exact | ResolutionKind::Symbolic) {
            prop_assert_eq!(lo, count.clone());
            prop_assert_eq!(hi, count);
        }
    }

    #[test]
    fn a_pipeline_without_filtering_stages_is_never_bounded(
        stages in prop::collection::vec(kind_strategy(), 0..5),
    ) {
        let filters = stages.iter().any(|k| matches!(
            k,
            StreamFunctionKind::Filter | StreamFunctionKind::FilterMap | StreamFunctionKind::Ok
        ));
        let mut resolution = symbolic_n();
        for kind in &stages {
            resolution = resolution.after(kind);
        }
        let ends_in_fold = matches!(stages.last(), Some(StreamFunctionKind::Fold));
        if !filters || ends_in_fold {
            prop_assert!(matches!(
                resolution.kind(),
                ResolutionKind::Exact | ResolutionKind::Symbolic
            ));
        }
        prop_assert!(resolution.reasons().is_empty() || filters);
    }

    #[test]
    fn resolved_bounds_only_contain_monotone_constructors(
        source in source_strategy(),
        stages in prop::collection::vec(kind_strategy(), 0..5),
    ) {
        let mut resolution = source;
        for kind in &stages {
            resolution = resolution.after(kind);
            prop_assert!(!contains_forbidden(resolution.lo()));
            if let Some(hi) = resolution.hi() {
                prop_assert!(!contains_forbidden(hi));
            }
        }
    }

    #[test]
    fn widening_the_input_never_narrows_the_output(
        low in 0u64..9,
        grow_down in 0u64..5,
        grow_up in 0u64..5,
        stages in prop::collection::vec(kind_strategy(), 0..5),
    ) {
        let tight_lo = low;
        let tight_hi = low + 3;
        let wide_lo = tight_lo.saturating_sub(grow_down);
        let wide_hi = tight_hi + grow_up;
        let mut tight = Resolution::checked(constant(tight_lo), Some(constant(tight_hi)), vec![]).unwrap();
        let mut wide = Resolution::checked(constant(wide_lo), Some(constant(wide_hi)), vec![]).unwrap();
        for kind in &stages {
            tight = tight.after(kind);
            wide = wide.after(kind);
            let tight_range = tight.evaluate(&SizeBindings::new());
            let wide_range = wide.evaluate(&SizeBindings::new());
            prop_assert!(wide_range.lo.unwrap() <= tight_range.lo.unwrap());
            prop_assert!(tight_range.hi.unwrap() <= wide_range.hi.unwrap());
        }
    }

    #[test]
    fn evaluating_the_resolution_matches_a_ground_walk_over_a_constant_source(
        stages in prop::collection::vec((kind_strategy(), any::<u64>()), 0..5),
        n in 0u64..13,
    ) {
        let mut symbolic = symbolic_n();
        let mut ground = exact(n);
        for (kind, _) in &stages {
            symbolic = symbolic.after(kind);
            ground = ground.after(kind);
        }
        let from_symbolic = symbolic.evaluate(&bind(n));
        let from_ground = ground.evaluate(&SizeBindings::new());
        prop_assert_eq!(from_symbolic.lo, from_ground.lo);
        prop_assert_eq!(from_symbolic.hi, from_ground.hi);
    }
}

#[test]
fn the_reference_counters_agree_with_known_values() {
    let n = |v: u64| BigUint::from(v);
    assert_eq!(reference_falling_factorial(&n(5), 3).to_u64(), Some(60));
    assert_eq!(reference_falling_factorial(&n(2), 3).to_u64(), Some(0));
    assert_eq!(reference_binomial(&n(5), 2).to_u64(), Some(10));
    assert_eq!(reference_binomial(&n(1), 2).to_u64(), Some(0));
    assert_eq!(reference_binomial(&n(7), 0).to_u64(), Some(1));
    assert_eq!(reference_power(&n(3), 4).to_u64(), Some(81));
}

#[test]
fn a_constant_bound_prints_and_compares_as_the_constant() {
    let filtered_then_permuted = symbolic_n()
        .after(&StreamFunctionKind::Filter)
        .after(&StreamFunctionKind::Permutations(3));
    assert_eq!(filtered_then_permuted.lo(), &constant(0));
    assert_eq!(filtered_then_permuted.lo().to_string(), "0");
    assert_eq!(filtered_then_permuted.hi().unwrap().to_string(), "P($n, 3)");
    assert_eq!(
        serde_json::to_value(&filtered_then_permuted).unwrap()["lo"],
        json!("0")
    );

    let concrete = exact(5).after(&StreamFunctionKind::Permutations(3));
    assert_eq!(concrete.lo(), &constant(60));
    assert_eq!(concrete.hi(), Some(&constant(60)));
    assert_eq!(concrete.kind(), ResolutionKind::Exact);

    let capped = exact(30).after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(capped.lo(), &constant(5));

    let chained = exact(4)
        .after(&StreamFunctionKind::Combinations(2))
        .after(&StreamFunctionKind::PermutationsWithReplacement(2));
    assert_eq!(chained.lo(), &constant(36));
}

#[test]
fn a_symbolic_bound_is_not_simplified() {
    let permuted = symbolic_n().after(&StreamFunctionKind::Permutations(3));
    assert_eq!(permuted.lo().to_string(), "P($n, 3)");
    let capped = symbolic_n().after(&StreamFunctionKind::KeepFirstN(5));
    assert_eq!(capped.lo().to_string(), "min($n, 5)");
}

#[test]
fn the_checked_constructor_accepts_valid_intervals_and_normalizes_them() {
    let valid = Resolution::checked(
        Symbolic::Permutations {
            n: Box::new(constant(0)),
            k: 3,
        },
        Some(param("n")),
        vec![ReasonCode::OpaquePredicate, ReasonCode::OpaquePredicate],
    )
    .unwrap();
    assert_eq!(valid.lo(), &constant(0));
    assert_eq!(valid.reasons(), &[ReasonCode::OpaquePredicate]);
    assert_eq!(valid.kind(), ResolutionKind::Bounded);

    let open = Resolution::checked(constant(0), None, vec![ReasonCode::UnboundInput]).unwrap();
    assert_eq!(open.kind(), ResolutionKind::Unresolvable);

    let point = Resolution::checked(param("n"), Some(param("n")), vec![]).unwrap();
    assert_eq!(point.kind(), ResolutionKind::Symbolic);
}

#[test]
fn the_checked_constructor_rejects_bounds_that_break_the_lattice() {
    let filtered = Symbolic::Filtered(Box::new(param("n")));
    let sum = Symbolic::Sum(vec![param("n"), constant(1)]);
    for bad in [filtered.clone(), Symbolic::Unknown, sum.clone()] {
        assert_eq!(
            Resolution::checked(bad.clone(), Some(param("n")), vec![]),
            Err(InvalidResolution::NonMonotoneBound),
            "lower bound {bad}"
        );
        assert_eq!(
            Resolution::checked(constant(0), Some(bad.clone()), vec![]),
            Err(InvalidResolution::NonMonotoneBound),
            "upper bound {bad}"
        );
        for nested in [
            Symbolic::Permutations {
                n: Box::new(bad.clone()),
                k: 2,
            },
            Symbolic::PermutationsWithReplacement {
                n: Box::new(bad.clone()),
                k: 2,
            },
            Symbolic::Combinations {
                n: Box::new(bad.clone()),
                k: 2,
            },
        ] {
            assert_eq!(
                Resolution::checked(nested.clone(), Some(param("n")), vec![]),
                Err(InvalidResolution::NonMonotoneBound),
                "nested {nested}"
            );
        }
        let capped = Symbolic::Min(Box::new(bad.clone()), Box::new(constant(3)));
        assert_eq!(
            Resolution::checked(constant(0), Some(capped), vec![]),
            Err(InvalidResolution::NonMonotoneBound)
        );
    }
    assert_eq!(
        Resolution::checked(constant(5), Some(constant(4)), vec![]),
        Err(InvalidResolution::LowerAboveUpper)
    );
    assert!(Resolution::checked(constant(4), Some(constant(4)), vec![]).is_ok());
    assert!(Resolution::checked(constant(4), Some(constant(5)), vec![]).is_ok());
    assert_eq!(
        Resolution::checked(
            Symbolic::Combinations {
                n: Box::new(constant(10)),
                k: 2
            },
            Some(constant(44)),
            vec![]
        ),
        Err(InvalidResolution::LowerAboveUpper)
    );
    assert!(InvalidResolution::NonMonotoneBound.to_string().len() > 10);
    assert!(InvalidResolution::LowerAboveUpper.to_string().len() > 10);
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(512))]

    #[test]
    fn every_resolution_the_api_can_produce_passes_its_own_checked_constructor(
        source in source_strategy(),
        stages in prop::collection::vec(kind_strategy(), 0..5),
    ) {
        let mut resolution = source;
        for kind in &stages {
            resolution = resolution.after(kind);
            let rebuilt = Resolution::checked(
                resolution.lo().clone(),
                resolution.hi().cloned(),
                resolution.reasons().to_vec(),
            );
            prop_assert_eq!(rebuilt, Ok(resolution.clone()));
        }
    }
}
