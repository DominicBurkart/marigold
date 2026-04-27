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
    use super::*;
    use crate::nodes::{InputCount, InputVariability};
    use num_bigint::BigUint;

    // ---- aggregate_input_variability invariants ----

    /// Empty iterator is the identity element: aggregating nothing is `Constant`.
    /// (Constant is the absorbing-by-absence default; only `Variable` "taints".)
    #[test]
    fn variability_empty_is_constant() {
        let out = aggregate_input_variability(std::iter::empty());
        assert!(matches!(out, InputVariability::Constant));
    }

    /// Single `Constant` input stays `Constant`.
    #[test]
    fn variability_single_constant_is_constant() {
        let out = aggregate_input_variability([InputVariability::Constant]);
        assert!(matches!(out, InputVariability::Constant));
    }

    /// Single `Variable` input promotes the result.
    #[test]
    fn variability_single_variable_is_variable() {
        let out = aggregate_input_variability([InputVariability::Variable]);
        assert!(matches!(out, InputVariability::Variable));
    }

    /// `Variable` is absorbing: any `Variable` in the input produces `Variable`,
    /// regardless of position or count of `Constant`s around it.
    #[test]
    fn variability_variable_is_absorbing() {
        let out = aggregate_input_variability([
            InputVariability::Constant,
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert!(matches!(out, InputVariability::Variable));
    }

    /// Aggregating only `Constant`s yields `Constant`.
    #[test]
    fn variability_all_constant_is_constant() {
        let out = aggregate_input_variability([
            InputVariability::Constant,
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert!(matches!(out, InputVariability::Constant));
    }

    // ---- aggregate_input_count invariants ----

    /// Empty iterator sums to `Known(0)` — additive identity.
    #[test]
    fn count_empty_is_known_zero() {
        let out = aggregate_input_count(std::iter::empty());
        match out {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(0u32)),
            other => panic!("expected Known(0), got {:?}", debug_kind(&other)),
        }
    }

    /// Sum of `Known` values is the BigUint sum.
    #[test]
    fn count_sums_known_values() {
        let out = aggregate_input_count([
            InputCount::Known(BigUint::from(10u32)),
            InputCount::Known(BigUint::from(20u32)),
            InputCount::Known(BigUint::from(7u32)),
        ]);
        match out {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(37u32)),
            other => panic!("expected Known(37), got {:?}", debug_kind(&other)),
        }
    }

    /// Sums above u64::MAX must not overflow — BigUint must be honoured.
    #[test]
    fn count_sums_handle_large_values() {
        let big = BigUint::from(u64::MAX);
        let out = aggregate_input_count([
            InputCount::Known(big.clone()),
            InputCount::Known(big.clone()),
        ]);
        match out {
            InputCount::Known(n) => assert_eq!(n, &big + &big),
            other => panic!("expected Known(2*u64::MAX), got {:?}", debug_kind(&other)),
        }
    }

    /// `Unknown` short-circuits to `Unknown`, even mixed with valid `Known`s.
    #[test]
    fn count_unknown_short_circuits() {
        let out = aggregate_input_count([
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(99u32)),
        ]);
        assert!(matches!(out, InputCount::Unknown));
    }

    /// Unresolved `Enum` placeholders also short-circuit to `Unknown`
    /// — guarding against silently emitting incorrect cardinalities when
    /// enum-range resolution has not yet run.
    #[test]
    fn count_enum_short_circuits() {
        let out = aggregate_input_count([
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Enum("MyEnum".to_string()),
        ]);
        assert!(matches!(out, InputCount::Unknown));
    }

    /// Short-circuit must trigger on the first `Unknown`/`Enum` and not be
    /// "rescued" by later `Known` entries — `Unknown` is absorbing.
    #[test]
    fn count_unknown_first_remains_unknown() {
        let out = aggregate_input_count([
            InputCount::Unknown,
            InputCount::Known(BigUint::from(1u32)),
            InputCount::Known(BigUint::from(2u32)),
        ]);
        assert!(matches!(out, InputCount::Unknown));
    }

    /// Single `Known(n)` round-trips to `Known(n)`.
    #[test]
    fn count_single_known_passes_through() {
        let out = aggregate_input_count([InputCount::Known(BigUint::from(42u32))]);
        match out {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(42u32)),
            other => panic!("expected Known(42), got {:?}", debug_kind(&other)),
        }
    }

    // Small helper since InputCount does not derive Debug.
    fn debug_kind(c: &InputCount) -> &'static str {
        match c {
            InputCount::Known(_) => "Known",
            InputCount::Enum(_) => "Enum",
            InputCount::Unknown => "Unknown",
        }
    }
}
