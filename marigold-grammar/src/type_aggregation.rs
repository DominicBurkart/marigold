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

    #[test]
    fn variability_returns_variable_when_any_variable() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Variable,
            nodes::InputVariability::Constant,
        ]);
        assert!(result == nodes::InputVariability::Variable);
    }

    #[test]
    fn variability_returns_constant_when_all_constant() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Constant,
        ]);
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_returns_constant_for_empty_iterator() {
        let result = aggregate_input_variability(vec![]);
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn count_returns_unknown_when_any_unknown() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(2_u32.into()),
            nodes::InputCount::Unknown,
        ]);
        assert!(matches!(result, nodes::InputCount::Unknown));
    }

    #[test]
    fn count_returns_unknown_when_any_enum() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(5_u32.into()),
            nodes::InputCount::Enum("variants".to_string()),
        ]);
        assert!(matches!(result, nodes::InputCount::Unknown));
    }

    #[test]
    fn count_sums_known_values() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(3_u32.into()),
            nodes::InputCount::Known(7_u32.into()),
        ]);
        assert!(matches!(result, nodes::InputCount::Known(n) if n == 10_u32.into()));
    }

    #[test]
    fn count_returns_zero_for_empty_iterator() {
        let result = aggregate_input_count(vec![]);
        assert!(matches!(result, nodes::InputCount::Known(n) if n == 0_u32.into()));
    }
}
