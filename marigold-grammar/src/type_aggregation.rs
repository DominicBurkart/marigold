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
    fn variability_empty_iter_is_constant() {
        let result = aggregate_input_variability(std::iter::empty());
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_all_constant_is_constant() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Constant,
        ]);
        assert!(result == nodes::InputVariability::Constant);
    }

    #[test]
    fn variability_any_variable_is_variable() {
        let result = aggregate_input_variability(vec![
            nodes::InputVariability::Constant,
            nodes::InputVariability::Variable,
        ]);
        assert!(result == nodes::InputVariability::Variable);
    }

    #[test]
    fn count_empty_iter_is_zero() {
        let result = aggregate_input_count(std::iter::empty());
        match result {
            nodes::InputCount::Known(v) => assert_eq!(v, BigUint::from(0_u32)),
            other => panic!("expected Known(0), got {:?}", std::mem::discriminant(&other)),
        }
    }

    #[test]
    fn count_known_values_sum() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(10_u32)),
            nodes::InputCount::Known(BigUint::from(20_u32)),
        ]);
        match result {
            nodes::InputCount::Known(v) => assert_eq!(v, BigUint::from(30_u32)),
            other => panic!("expected Known(30), got {:?}", std::mem::discriminant(&other)),
        }
    }

    #[test]
    fn count_unknown_short_circuits() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(5_u32)),
            nodes::InputCount::Unknown,
        ]);
        assert!(matches!(result, nodes::InputCount::Unknown));
    }

    #[test]
    fn count_enum_short_circuits() {
        let result = aggregate_input_count(vec![
            nodes::InputCount::Known(BigUint::from(5_u32)),
            nodes::InputCount::Enum("Color".to_string()),
        ]);
        assert!(matches!(result, nodes::InputCount::Unknown));
    }
}
