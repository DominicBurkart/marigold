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
    use num_bigint::BigUint;

    // --- aggregate_input_variability tests ---

    #[test]
    fn variability_all_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_single_variable_makes_result_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_all_variable() {
        let result = aggregate_input_variability(vec![
            InputVariability::Variable,
            InputVariability::Variable,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_empty_input_is_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_single_constant() {
        let result = aggregate_input_variability(vec![InputVariability::Constant]);
        assert_eq!(result, InputVariability::Constant);
    }

    #[test]
    fn variability_single_variable() {
        let result = aggregate_input_variability(vec![InputVariability::Variable]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_variable_at_start() {
        let result = aggregate_input_variability(vec![
            InputVariability::Variable,
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    #[test]
    fn variability_variable_at_end() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
            InputVariability::Variable,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    // --- aggregate_input_count tests ---

    #[test]
    fn count_all_known_sums_correctly() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(10u64)),
            InputCount::Known(BigUint::from(20u64)),
            InputCount::Known(BigUint::from(30u64)),
        ]);
        assert_eq!(result, InputCount::Known(BigUint::from(60u64)));
    }

    #[test]
    fn count_single_unknown_makes_result_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(10u64)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(30u64)),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_all_unknown() {
        let result = aggregate_input_count(vec![InputCount::Unknown, InputCount::Unknown]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_empty_input_is_zero() {
        let result = aggregate_input_count(Vec::<InputCount>::new());
        assert_eq!(result, InputCount::Known(BigUint::from(0u64)));
    }

    #[test]
    fn count_single_known() {
        let result = aggregate_input_count(vec![InputCount::Known(BigUint::from(42u64))]);
        assert_eq!(result, InputCount::Known(BigUint::from(42u64)));
    }

    #[test]
    fn count_single_unknown() {
        let result = aggregate_input_count(vec![InputCount::Unknown]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_unknown_short_circuits_at_start() {
        let result = aggregate_input_count(vec![
            InputCount::Unknown,
            InputCount::Known(BigUint::from(100u64)),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    #[test]
    fn count_large_values_do_not_overflow() {
        let large = BigUint::from(u64::MAX);
        let result =
            aggregate_input_count(vec![InputCount::Known(large.clone()), InputCount::Known(large.clone())]);
        let expected = &BigUint::from(u64::MAX) + &BigUint::from(u64::MAX);
        assert_eq!(result, InputCount::Known(expected));
    }

    #[test]
    fn count_zeros_sum_to_zero() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(0u64)),
            InputCount::Known(BigUint::from(0u64)),
        ]);
        assert_eq!(result, InputCount::Known(BigUint::from(0u64)));
    }
}
