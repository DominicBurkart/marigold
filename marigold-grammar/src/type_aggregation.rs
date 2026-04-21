use crate::nodes;

pub fn aggregate_input_variability<I: IntoIterator<Item = nodes::InputVariability>>(
    variabilities: I,
) -> nodes::InputVariability {
    if variabilities
        .into_iter()
        .any(|v| v == nodes::InputVariability::Variable)
    {
        return nodes::InputVariability::Variable;
    }
    nodes::InputVariability::Constant
}

pub fn aggregate_input_count<I: IntoIterator<Item = nodes::InputCount>>(
    counts: I,
) -> nodes::InputCount {
    let mut total_count: num_bigint::BigUint = 0_u32.into();
    for count in counts.into_iter() {
        match count {
            nodes::InputCount::Unknown | nodes::InputCount::Enum(_) => {
                return nodes::InputCount::Unknown
            }
            nodes::InputCount::Known(known_count) => total_count += known_count,
        }
    }
    nodes::InputCount::Known(total_count)
}

#[cfg(test)]
mod tests {
    //! Tests pinning the invariants of the stream-input aggregators used by
    //! `select_all` codegen (`pest_ast_builder::build_select_all_input`).
    //!
    //! Invariants validated:
    //!
    //! * `aggregate_input_variability` is the monoid-OR over `Variable`:
    //!   the identity is `Constant`, and a single `Variable` anywhere in the
    //!   iterator poisons the result.
    //! * `aggregate_input_count` is a short-circuiting sum over `Known`
    //!   with an absorbing element `Unknown` (which `Enum(_)` collapses to,
    //!   because enum cardinality is resolved *after* aggregation by the
    //!   symbol table pass). Empty input is the `Known(0)` identity.
    //! * Both aggregators are order-insensitive in the sense that shuffling
    //!   the iterator does not change the result.
    //! * `Enum(_)` is never propagated through the output -- downstream code
    //!   in `pest_ast_builder` relies on this and only ever sees `Known` or
    //!   `Unknown`.
    use super::*;
    use nodes::{InputCount, InputVariability};
    use num_bigint::BigUint;
    use proptest::prelude::*;

    // ---------- aggregate_input_variability ----------

    #[test]
    fn variability_empty_is_constant() {
        let empty: Vec<InputVariability> = Vec::new();
        assert!(aggregate_input_variability(empty) == InputVariability::Constant);
    }

    #[test]
    fn variability_all_constant_is_constant() {
        let all_const = vec![InputVariability::Constant; 5];
        assert!(aggregate_input_variability(all_const) == InputVariability::Constant);
    }

    #[test]
    fn variability_any_variable_is_variable() {
        // Variable at the start, middle, and end -- position must not matter.
        for position in 0..4 {
            let mut v = vec![InputVariability::Constant; 4];
            v[position] = InputVariability::Variable;
            assert!(
                aggregate_input_variability(v) == InputVariability::Variable,
                "Variable at position {position} should poison the aggregate"
            );
        }
    }

    #[test]
    fn variability_single_element_is_identity() {
        assert!(
            aggregate_input_variability([InputVariability::Constant]) == InputVariability::Constant
        );
        assert!(
            aggregate_input_variability([InputVariability::Variable]) == InputVariability::Variable
        );
    }

    fn arb_variability() -> impl Strategy<Value = InputVariability> {
        prop_oneof![
            Just(InputVariability::Constant),
            Just(InputVariability::Variable),
        ]
    }

    proptest! {
        // Property: the aggregate is `Variable` iff any input is `Variable`.
        #[test]
        fn prop_variability_is_any_variable(vs in prop::collection::vec(arb_variability(), 0..16)) {
            let has_variable = vs.contains(&InputVariability::Variable);
            let expected = if has_variable {
                InputVariability::Variable
            } else {
                InputVariability::Constant
            };
            prop_assert!(aggregate_input_variability(vs) == expected);
        }

        // Property: order does not change the aggregate (monoid is commutative).
        #[test]
        fn prop_variability_order_insensitive(
            vs in prop::collection::vec(arb_variability(), 0..16),
            seed in any::<u64>(),
        ) {
            let forward = aggregate_input_variability(vs.clone());
            let mut shuffled = vs;
            // Deterministic Fisher-Yates using a seeded xorshift prng to
            // avoid pulling in `rand` as a dev-dep.
            let mut state = seed | 1;
            for i in (1..shuffled.len()).rev() {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                let j = (state as usize) % (i + 1);
                shuffled.swap(i, j);
            }
            prop_assert!(forward == aggregate_input_variability(shuffled));
        }
    }

    // ---------- aggregate_input_count ----------

    fn known(n: u64) -> InputCount {
        InputCount::Known(BigUint::from(n))
    }

    #[test]
    fn count_empty_is_known_zero() {
        let empty: Vec<InputCount> = Vec::new();
        match aggregate_input_count(empty) {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(0u32)),
            _ => panic!("empty iterator should aggregate to Known(0)"),
        }
    }

    #[test]
    fn count_all_known_sums() {
        let counts = vec![known(1), known(2), known(3), known(4)];
        match aggregate_input_count(counts) {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(10u32)),
            _ => panic!("sum of Known should be Known"),
        }
    }

    #[test]
    fn count_unknown_is_absorbing() {
        let with_unknown = vec![known(1), InputCount::Unknown, known(2)];
        assert!(matches!(
            aggregate_input_count(with_unknown),
            InputCount::Unknown
        ));
    }

    #[test]
    fn count_enum_collapses_to_unknown() {
        // `Enum(_)` represents a cardinality not yet resolved by the symbol
        // table; the aggregator conservatively collapses it to `Unknown`
        // rather than leaking the enum name.
        let with_enum = vec![known(3), InputCount::Enum("Color".into()), known(5)];
        assert!(matches!(
            aggregate_input_count(with_enum),
            InputCount::Unknown
        ));
    }

    #[test]
    fn count_never_emits_enum() {
        // The output of the aggregator is only ever Known or Unknown.
        // This is relied upon by `pest_ast_builder::build_select_all_input`.
        let cases: Vec<Vec<InputCount>> = vec![
            vec![],
            vec![known(0)],
            vec![InputCount::Enum("A".into())],
            vec![InputCount::Unknown, InputCount::Enum("B".into())],
            vec![known(1), known(2), InputCount::Enum("C".into())],
        ];
        for c in cases {
            let out = aggregate_input_count(c);
            assert!(
                !matches!(out, InputCount::Enum(_)),
                "aggregator must never emit Enum"
            );
        }
    }

    #[test]
    fn count_short_circuits_without_consuming_tail() {
        // Prove short-circuit: once we see Unknown, the remaining iterator
        // items must not be pulled. We encode this as a panicking iterator
        // tail -- the test passes only if short-circuit actually happens.
        struct PanickingTail<I: Iterator<Item = InputCount>> {
            head: std::vec::IntoIter<InputCount>,
            tail: I,
        }
        impl<I: Iterator<Item = InputCount>> Iterator for PanickingTail<I> {
            type Item = InputCount;
            fn next(&mut self) -> Option<InputCount> {
                if let Some(x) = self.head.next() {
                    return Some(x);
                }
                self.tail.next()
            }
        }
        let head = vec![known(1), InputCount::Unknown].into_iter();
        let tail = std::iter::from_fn(|| -> Option<InputCount> {
            panic!("tail pulled; short-circuit invariant violated");
        });
        let iter = PanickingTail { head, tail };
        assert!(matches!(aggregate_input_count(iter), InputCount::Unknown));
    }

    fn arb_count() -> impl Strategy<Value = InputCount> {
        prop_oneof![
            (0u64..1_000).prop_map(known),
            Just(InputCount::Unknown),
            "[A-Z][a-zA-Z0-9]{0,8}".prop_map(InputCount::Enum),
        ]
    }

    proptest! {
        // Property: if every input is Known, the output is the arithmetic
        // sum; otherwise the output is Unknown. The aggregator must never
        // emit `Enum`.
        #[test]
        fn prop_count_matches_oracle(cs in prop::collection::vec(arb_count(), 0..16)) {
            let expected: Option<BigUint> = cs.iter().try_fold(BigUint::from(0u32), |acc, c| {
                match c {
                    InputCount::Known(n) => Some(acc + n),
                    _ => None,
                }
            });
            match (aggregate_input_count(cs.clone()), expected) {
                (InputCount::Known(got), Some(want)) => prop_assert_eq!(got, want),
                (InputCount::Unknown, None) => {},
                (InputCount::Enum(_), _) => prop_assert!(false, "aggregator emitted Enum"),
                (got, want) => prop_assert!(
                    false,
                    "mismatch: got {:?}, want {:?}",
                    match got {
                        InputCount::Known(_) => "Known",
                        InputCount::Unknown => "Unknown",
                        InputCount::Enum(_) => "Enum",
                    },
                    want.map(|_| "Known").unwrap_or("Unknown")
                ),
            }
        }

        // Property: permuting a pure-Known input does not change the sum.
        #[test]
        fn prop_count_commutative_over_known(
            ns in prop::collection::vec(0u64..10_000, 0..16),
            seed in any::<u64>(),
        ) {
            let counts: Vec<InputCount> = ns.iter().copied().map(known).collect();
            let mut shuffled = counts.clone();
            let mut state = seed | 1;
            for i in (1..shuffled.len()).rev() {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                let j = (state as usize) % (i + 1);
                shuffled.swap(i, j);
            }
            let lhs = aggregate_input_count(counts);
            let rhs = aggregate_input_count(shuffled);
            match (lhs, rhs) {
                (InputCount::Known(a), InputCount::Known(b)) => prop_assert_eq!(a, b),
                _ => prop_assert!(false, "pure-Known input must aggregate to Known"),
            }
        }
    }
}
