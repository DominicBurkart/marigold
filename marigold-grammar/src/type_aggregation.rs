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

    // -----------------------------------------------------------------------
    // aggregate_input_variability
    // -----------------------------------------------------------------------

    #[test]
    fn all_constant_yields_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert!(matches!(result, InputVariability::Constant));
    }

    #[test]
    fn any_variable_yields_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert!(matches!(result, InputVariability::Variable));
    }

    #[test]
    fn single_variable_yields_variable() {
        let result = aggregate_input_variability(vec![InputVariability::Variable]);
        assert!(matches!(result, InputVariability::Variable));
    }

    #[test]
    fn empty_variability_iterator_yields_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert!(matches!(result, InputVariability::Constant));
    }

    // -----------------------------------------------------------------------
    // aggregate_input_count
    // -----------------------------------------------------------------------

    #[test]
    fn known_counts_are_summed() {
        let counts = vec![
            InputCount::Known(BigUint::from(10_u32)),
            InputCount::Known(BigUint::from(20_u32)),
            InputCount::Known(BigUint::from(5_u32)),
        ];
        match aggregate_input_count(counts) {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(35_u32)),
            _ => panic!("expected Known(35)"),
        }
    }

    #[test]
    fn unknown_count_short_circuits_to_unknown() {
        let counts = vec![
            InputCount::Known(BigUint::from(10_u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(5_u32)),
        ];
        assert!(matches!(aggregate_input_count(counts), InputCount::Unknown));
    }

    #[test]
    fn enum_count_short_circuits_to_unknown() {
        let counts = vec![
            InputCount::Known(BigUint::from(3_u32)),
            InputCount::Enum("Color".to_string()),
        ];
        assert!(matches!(aggregate_input_count(counts), InputCount::Unknown));
    }

    #[test]
    fn empty_count_iterator_yields_zero() {
        match aggregate_input_count(Vec::<InputCount>::new()) {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(0_u32)),
            _ => panic!("expected Known(0)"),
        }
    }

    #[test]
    fn single_known_count() {
        let counts = vec![InputCount::Known(BigUint::from(42_u32))];
        match aggregate_input_count(counts) {
            InputCount::Known(n) => assert_eq!(n, BigUint::from(42_u32)),
            _ => panic!("expected Known(42)"),
        }
    }
}
