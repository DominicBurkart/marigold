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
    use nodes::{InputCount, InputVariability};
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
    fn variability_empty_is_constant() {
        let result = aggregate_input_variability(vec![]);
        assert_eq!(result, InputVariability::Constant);
    }

    // --- aggregate_input_count ---

    #[test]
    fn count_known_values_are_summed() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(3u32)),
            InputCount::Known(BigUint::from(7u32)),
        ]);
        assert_eq!(result, InputCount::Known(BigUint::from(10u32)));
    }

    #[test]
    fn count_empty_sums_to_zero() {
        let result = aggregate_input_count(vec![]);
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
    fn count_enum_placeholder_short_circuits() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Enum("Color".to_string()),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }
}
