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

    #[test]
    fn variability_empty_is_constant() {
        assert!(
            aggregate_input_variability(std::iter::empty::<InputVariability>())
                == InputVariability::Constant
        );
    }

    #[test]
    fn variability_all_constant_is_constant() {
        assert!(
            aggregate_input_variability(vec![
                InputVariability::Constant,
                InputVariability::Constant,
                InputVariability::Constant,
            ]) == InputVariability::Constant
        );
    }

    #[test]
    fn variability_any_variable_is_variable() {
        assert!(
            aggregate_input_variability(vec![
                InputVariability::Constant,
                InputVariability::Variable,
                InputVariability::Constant,
            ]) == InputVariability::Variable
        );
    }

    #[test]
    fn count_empty_is_known_zero() {
        let result = aggregate_input_count(std::iter::empty::<InputCount>());
        assert!(result == InputCount::Known(BigUint::from(0_u32)));
    }

    #[test]
    fn count_sums_known_values() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(2_u32)),
            InputCount::Known(BigUint::from(3_u32)),
            InputCount::Known(BigUint::from(7_u32)),
        ]);
        assert!(result == InputCount::Known(BigUint::from(12_u32)));
    }

    /// Encountering Unknown short-circuits regardless of any Known summands.
    #[test]
    fn count_unknown_propagates() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(5_u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(10_u32)),
        ]);
        assert!(result == InputCount::Unknown);
    }

    /// An unresolved `Enum` placeholder behaves like Unknown for the purposes
    /// of compile-time aggregation.
    #[test]
    fn count_unresolved_enum_propagates_as_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(1_u32)),
            InputCount::Enum("Words".to_string()),
        ]);
        assert!(result == InputCount::Unknown);
    }
}
