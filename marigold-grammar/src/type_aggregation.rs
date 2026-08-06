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

    // ── aggregate_input_variability ──────────────────────────────────────────

    #[test]
    fn variability_empty_is_constant() {
        // An empty iterator has no Variable entries ⇒ result is Constant.
        assert_eq!(
            aggregate_input_variability(Vec::<InputVariability>::new()),
            InputVariability::Constant
        );
    }

    #[test]
    fn variability_all_constant_is_constant() {
        let v = vec![InputVariability::Constant, InputVariability::Constant];
        assert_eq!(aggregate_input_variability(v), InputVariability::Constant);
    }

    #[test]
    fn variability_single_variable_is_variable() {
        let v = vec![InputVariability::Variable];
        assert_eq!(aggregate_input_variability(v), InputVariability::Variable);
    }

    #[test]
    fn variability_mixed_is_variable() {
        // Any Variable entry should short-circuit to Variable.
        let v = vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ];
        assert_eq!(aggregate_input_variability(v), InputVariability::Variable);
    }

    #[test]
    fn variability_all_variable_is_variable() {
        let v = vec![InputVariability::Variable, InputVariability::Variable];
        assert_eq!(aggregate_input_variability(v), InputVariability::Variable);
    }

    // ── aggregate_input_count ────────────────────────────────────────────────

    #[test]
    fn count_empty_is_known_zero() {
        // Empty iterator ⇒ accumulator starts at 0 and is never incremented.
        assert_eq!(
            aggregate_input_count(Vec::<InputCount>::new()),
            InputCount::Known(BigUint::from(0_u32))
        );
    }

    #[test]
    fn count_single_known_is_that_value() {
        let counts = vec![InputCount::Known(BigUint::from(7_u32))];
        assert_eq!(
            aggregate_input_count(counts),
            InputCount::Known(BigUint::from(7_u32))
        );
    }

    #[test]
    fn count_multiple_known_sums() {
        // 10 + 20 + 30 = 60
        let counts = vec![
            InputCount::Known(BigUint::from(10_u32)),
            InputCount::Known(BigUint::from(20_u32)),
            InputCount::Known(BigUint::from(30_u32)),
        ];
        assert_eq!(
            aggregate_input_count(counts),
            InputCount::Known(BigUint::from(60_u32))
        );
    }

    #[test]
    fn count_single_unknown_returns_unknown() {
        let counts = vec![InputCount::Unknown];
        assert_eq!(aggregate_input_count(counts), InputCount::Unknown);
    }

    #[test]
    fn count_known_then_unknown_returns_unknown() {
        // Once Unknown is encountered the function short-circuits.
        let counts = vec![
            InputCount::Known(BigUint::from(5_u32)),
            InputCount::Unknown,
        ];
        assert_eq!(aggregate_input_count(counts), InputCount::Unknown);
    }

    #[test]
    fn count_single_enum_returns_unknown() {
        // Enum variant resolves to Unknown (it needs symbol-table lookup).
        let counts = vec![InputCount::Enum("Color".to_string())];
        assert_eq!(aggregate_input_count(counts), InputCount::Unknown);
    }

    #[test]
    fn count_known_then_enum_returns_unknown() {
        let counts = vec![
            InputCount::Known(BigUint::from(3_u32)),
            InputCount::Enum("Direction".to_string()),
        ];
        assert_eq!(aggregate_input_count(counts), InputCount::Unknown);
    }

    #[test]
    fn count_unknown_before_known_still_returns_unknown() {
        // Short-circuit occurs on first Unknown; the subsequent Known is not
        // visited.
        let counts = vec![
            InputCount::Unknown,
            InputCount::Known(BigUint::from(99_u32)),
        ];
        assert_eq!(aggregate_input_count(counts), InputCount::Unknown);
    }
}
