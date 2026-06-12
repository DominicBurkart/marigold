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

    #[test]
    fn all_constant_variabilities_yield_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn any_variable_variability_yields_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn single_variable_yields_variable() {
        let result = aggregate_input_variability(vec![InputVariability::Variable]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn empty_variability_iterator_yields_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn known_counts_are_summed() {
        let result = aggregate_input_count(vec![
            InputCount::Known(3_u32.into()),
            InputCount::Known(5_u32.into()),
        ]);
        assert!(result == InputCount::Known(8_u32.into()));
    }

    #[test]
    fn unknown_count_short_circuits() {
        let result = aggregate_input_count(vec![
            InputCount::Known(3_u32.into()),
            InputCount::Unknown,
            InputCount::Known(2_u32.into()),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn enum_count_short_circuits_to_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(1_u32.into()),
            InputCount::Enum("MyEnum".to_string()),
        ]);
        assert!(matches!(result, InputCount::Unknown));
    }

    #[test]
    fn empty_count_iterator_yields_zero() {
        let result = aggregate_input_count(Vec::<InputCount>::new());
        assert!(result == InputCount::Known(0_u32.into()));
    }

    #[test]
    fn single_known_count_is_preserved() {
        let result = aggregate_input_count(vec![InputCount::Known(42_u32.into())]);
        assert!(result == InputCount::Known(42_u32.into()));
    }
}
