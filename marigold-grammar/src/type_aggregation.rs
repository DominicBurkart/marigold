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
    use num_bigint::BigUint;

    // --- aggregate_input_variability ---

    #[test]
    fn variability_all_constant_returns_constant() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Constant,
        ]);
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_any_variable_returns_variable() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Variable,
            nodes::InputVariability::Constant,
        ]);
        assert!(result == nodes::InputVariability::Variable);
    }

    #[test]
    fn variability_single_variable_returns_variable() {
        let result = aggregate_input_variability(vec![nodes::InputVariability::Variable]);
        assert!(result == nodes::InputVariability::Variable);
    }

    #[test]
    fn variability_single_constant_returns_constant() {
        let result = aggregate_input_variability(vec![nodes::InputVariability::Constant]);
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_empty_returns_constant() {
        let result = aggregate_input_variability(std::iter::empty::<nodes::InputVariability>());
        assert!(result == nodes::InputVariability::Constant);
    }

    // --- aggregate_input_count ---

    #[test]
    fn count_empty_returns_zero_known() {
        let result = aggregate_input_count(std::iter::empty::<nodes::InputCount>());
        assert!(result == nodes::InputCount::Known(BigUint::from(0u32)));
    }

    #[test]
    fn count_all_known_sums_them() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(10u32)),
            nodes::InputCount::Known(BigUint::from(20u32)),
            nodes::InputCount::Known(BigUint::from(5u32)),
        ]);
        assert!(result == nodes::InputCount::Known(BigUint::from(35u32)));
    }

    #[test]
    fn count_unknown_short_circuits() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(10u32)),
            nodes::InputCount::Unknown,
            nodes::InputCount::Known(BigUint::from(5u32)),
        ]);
        assert!(result == nodes::InputCount::Unknown);
    }

    #[test]
    fn count_enum_short_circuits() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(3u32)),
            nodes::InputCount::Enum("Color".to_string()),
        ]);
        assert!(result == nodes::InputCount::Unknown);
    }

    #[test]
    fn count_single_known() {
        let result = aggregate_input_count(vec![nodes::InputCount::Known(BigUint::from(42u32))]);
        assert!(result == nodes::InputCount::Known(BigUint::from(42u32)));
    }
}
