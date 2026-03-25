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
            nodes::InputCount::Unknown => return nodes::InputCount::Unknown,
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
    fn all_constant_yields_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn any_variable_yields_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn empty_variabilities_yields_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn known_counts_are_summed() {
        let result = aggregate_input_count(vec![
            InputCount::Known(3_u32.into()),
            InputCount::Known(7_u32.into()),
        ]);
        assert_eq!(result, InputCount::Known(10_u32.into()));
    }

    #[test]
    fn any_unknown_count_yields_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(5_u32.into()),
            InputCount::Unknown,
            InputCount::Known(3_u32.into()),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn empty_counts_yields_zero() {
        let result = aggregate_input_count(Vec::<InputCount>::new());
        assert_eq!(result, InputCount::Known(0_u32.into()));
    }
}
