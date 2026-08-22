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
    fn variability_all_constant_yields_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_empty_yields_constant() {
        let result = aggregate_input_variability(vec![]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_any_variable_yields_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_single_variable_yields_variable() {
        let result = aggregate_input_variability(vec![InputVariability::Variable]);
        assert_eq!(result, InputVariability::Variable);
    }

    // ── aggregate_input_count ───────────────────────────────────────────────

    #[test]
    fn count_empty_yields_zero() {
        let result = aggregate_input_count(vec![]);
        match result {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(0_u32)),
            InputCount::Unknown => panic!("expected Known(0), got Unknown"),
            InputCount::Enum(s) => panic!("expected Known(0), got Enum({:?})", s),
        }
    }

    #[test]
    fn count_single_known_yields_same() {
        let result = aggregate_input_count(vec![InputCount::Known(BigUint::from(7_u32))]);
        match result {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(7_u32)),
            InputCount::Unknown => panic!("expected Known(7), got Unknown"),
            InputCount::Enum(s) => panic!("expected Known(7), got Enum({:?})", s),
        }
    }

    #[test]
    fn count_multiple_known_yields_sum() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(3_u32)),
            InputCount::Known(BigUint::from(5_u32)),
            InputCount::Known(BigUint::from(2_u32)),
        ]);
        match result {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(10_u32)),
            InputCount::Unknown => panic!("expected Known(10), got Unknown"),
            InputCount::Enum(s) => panic!("expected Known(10), got Enum({:?})", s),
        }
    }

    #[test]
    fn count_unknown_short_circuits_to_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(3_u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(5_u32)),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn count_enum_short_circuits_to_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(1_u32)),
            InputCount::Enum("MyEnum".to_string()),
            InputCount::Known(BigUint::from(2_u32)),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn count_all_unknown_yields_unknown() {
        let result = aggregate_input_count(vec![InputCount::Unknown, InputCount::Unknown]);
        assert!(matches!(result, InputCount::Unknown));
    }
}
