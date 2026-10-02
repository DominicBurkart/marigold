use crate::nodes;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::{InputCount, InputVariability};

    // --- aggregate_input_variability ---

    #[test]
    fn all_constant_gives_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Constant);
    }

    #[test]
    fn any_variable_gives_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn single_variable_gives_variable() {
        let result = aggregate_input_variability(vec![InputVariability::Variable]);
        assert!(result == InputVariability::Variable);
    }

    #[test]
    fn empty_variabilities_gives_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert!(result == InputVariability::Constant);
    }

    // --- aggregate_input_count ---

    #[test]
    fn known_counts_are_summed() {
        let result = aggregate_input_count(vec![
            InputCount::Known(10_u32.into()),
            InputCount::Known(20_u32.into()),
        ]);
        assert!(result == InputCount::Known(30_u32.into()));
    }

    #[test]
    fn empty_counts_sums_to_zero() {
        let result = aggregate_input_count(Vec::<InputCount>::new());
        assert!(result == InputCount::Known(0_u32.into()));
    }

    #[test]
    fn unknown_count_short_circuits() {
        let result = aggregate_input_count(vec![
            InputCount::Known(5_u32.into()),
            InputCount::Unknown,
            InputCount::Known(3_u32.into()),
        ]);
        assert!(result == InputCount::Unknown);
    }

    #[test]
    fn enum_count_returns_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(5_u32.into()),
            InputCount::Enum("Color".to_string()),
        ]);
        assert!(result == InputCount::Unknown);
    }

    #[test]
    fn single_known_count() {
        let result = aggregate_input_count(vec![InputCount::Known(7_u32.into())]);
        assert!(result == InputCount::Known(7_u32.into()));
    }
}

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
