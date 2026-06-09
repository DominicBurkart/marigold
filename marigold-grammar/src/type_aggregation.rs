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

    #[test]
    fn variability_empty_is_constant() {
        assert!(aggregate_input_variability(vec![]) == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_all_constant() {
        let v = vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Constant,
        ];
        assert!(aggregate_input_variability(v) == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_all_variable() {
        let v = vec![
            nodes::InputVariability::Variable,
            nodes::InputVariability::Variable,
        ];
        assert!(aggregate_input_variability(v) == nodes::InputVariability::Variable);
    }

    #[test]
    fn variability_mixed_returns_variable() {
        let v = vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Variable,
            nodes::InputVariability::Constant,
        ];
        assert!(aggregate_input_variability(v) == nodes::InputVariability::Variable);
    }

    #[test]
    fn count_empty_is_zero() {
        let result = aggregate_input_count(vec![]);
        assert!(result == nodes::InputCount::Known(BigUint::from(0_u32)));
    }

    #[test]
    fn count_known_values_sum() {
        let counts = vec![
            nodes::InputCount::Known(BigUint::from(3_u32)),
            nodes::InputCount::Known(BigUint::from(7_u32)),
        ];
        assert!(aggregate_input_count(counts) == nodes::InputCount::Known(BigUint::from(10_u32)));
    }

    #[test]
    fn count_unknown_short_circuits() {
        let counts = vec![
            nodes::InputCount::Known(BigUint::from(5_u32)),
            nodes::InputCount::Unknown,
            nodes::InputCount::Known(BigUint::from(3_u32)),
        ];
        assert!(aggregate_input_count(counts) == nodes::InputCount::Unknown);
    }

    #[test]
    fn count_enum_short_circuits() {
        let counts = vec![
            nodes::InputCount::Known(BigUint::from(2_u32)),
            nodes::InputCount::Enum("Color".to_string()),
        ];
        assert!(aggregate_input_count(counts) == nodes::InputCount::Unknown);
    }

    #[test]
    fn count_single_known() {
        let counts = vec![nodes::InputCount::Known(BigUint::from(42_u32))];
        assert!(
            aggregate_input_count(counts) == nodes::InputCount::Known(BigUint::from(42_u32))
        );
    }
}
