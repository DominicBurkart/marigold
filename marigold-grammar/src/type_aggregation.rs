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
    fn variability_all_constant_is_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_any_variable_is_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_all_variable_is_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Variable,
            InputVariability::Variable,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_empty_iter_is_constant() {
        // No variabilities to aggregate => nothing is variable => Constant.
        let result = aggregate_input_variability(std::iter::empty());
        assert_eq!(result, InputVariability::Constant);
    }

    // --- aggregate_input_count ---

    #[test]
    fn count_sums_known_values() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(3u32)),
            InputCount::Known(BigUint::from(7u32)),
        ]);
        assert_eq!(result, InputCount::Known(BigUint::from(10u32)));
    }

    #[test]
    fn count_empty_iter_is_zero() {
        let result = aggregate_input_count(std::iter::empty::<InputCount>());
        assert_eq!(result, InputCount::Known(BigUint::from(0u32)));
    }

    #[test]
    fn count_unknown_short_circuits() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(3u32)),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_enum_short_circuits() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(2u32)),
            InputCount::Enum("MyEnum".to_string()),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_single_known_value() {
        let result = aggregate_input_count(vec![InputCount::Known(BigUint::from(42u32))]);
        assert_eq!(result, InputCount::Known(BigUint::from(42u32)));
    }
}
