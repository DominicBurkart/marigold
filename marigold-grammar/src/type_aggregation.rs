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

    // --- aggregate_input_variability ---

    #[test]
    fn variability_all_constant_yields_constant() {
        let result = aggregate_input_variability([
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn variability_one_variable_yields_variable() {
        let result = aggregate_input_variability([
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn variability_all_variable_yields_variable() {
        let result = aggregate_input_variability([
            InputVariability::Variable,
            InputVariability::Variable,
        ]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn variability_empty_yields_constant() {
        // With no inputs the fold short-circuits to false → Constant.
        let result = aggregate_input_variability(std::iter::empty::<InputVariability>());
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn variability_single_constant_yields_constant() {
        let result = aggregate_input_variability([InputVariability::Constant]);
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn variability_single_variable_yields_variable() {
        let result = aggregate_input_variability([InputVariability::Variable]);
        assert!(result == InputVariability::Variable);
    }

    // --- aggregate_input_count ---

    #[test]
    fn count_empty_yields_known_zero() {
        let result = aggregate_input_count(std::iter::empty::<InputCount>());
        assert!(matches!(result, InputCount::Known(n) if n == BigUint::from(0u32)));
    }

    #[test]
    fn count_all_known_yields_sum() {
        let result = aggregate_input_count([
            InputCount::Known(BigUint::from(3u32)),
            InputCount::Known(BigUint::from(7u32)),
            InputCount::Known(BigUint::from(10u32)),
        ]);
        assert!(matches!(result, InputCount::Known(n) if n == BigUint::from(20u32)));
    }

    #[test]
    fn count_single_known_yields_that_value() {
        let result = aggregate_input_count([InputCount::Known(BigUint::from(42u32))]);
        assert!(matches!(result, InputCount::Known(n) if n == BigUint::from(42u32)));
    }

    #[test]
    fn count_unknown_short_circuits() {
        let result = aggregate_input_count([
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(10u32)),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn count_unknown_first_short_circuits_immediately() {
        let result = aggregate_input_count([InputCount::Unknown]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn count_enum_short_circuits() {
        let result = aggregate_input_count([
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Enum("Color".to_string()),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn count_enum_first_short_circuits_immediately() {
        let result = aggregate_input_count([InputCount::Enum("Direction".to_string())]);
        assert!(matches!(result, InputCount::Unknown));
    }
}
