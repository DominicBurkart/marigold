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

    // ---- aggregate_input_variability ----

    /// All-Constant inputs aggregate to Constant.
    #[test]
    fn variability_all_constant() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Constant);
    }

    /// Any Variable in the input makes the aggregate Variable.
    #[test]
    fn variability_any_variable_dominates() {
        let result = aggregate_input_variability(vec![
            InputVariability::Constant,
            InputVariability::Variable,
            InputVariability::Constant,
        ]);
        assert_eq!(result, InputVariability::Variable);
    }

    /// A single Variable input returns Variable.
    #[test]
    fn variability_single_variable() {
        let result =
            aggregate_input_variability(std::iter::once(InputVariability::Variable));
        assert_eq!(result, InputVariability::Variable);
    }

    /// An empty iterator (no inputs) returns Constant (vacuously true).
    #[test]
    fn variability_empty_is_constant() {
        let result = aggregate_input_variability(Vec::<InputVariability>::new());
        assert_eq!(result, InputVariability::Constant);
    }

    // ---- aggregate_input_count ----

    /// Known counts are summed correctly.
    #[test]
    fn count_known_values_are_summed() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(3u32)),
            InputCount::Known(BigUint::from(7u32)),
        ]);
        assert_eq!(result, InputCount::Known(BigUint::from(10u32)));
    }

    /// A single Unknown in the inputs makes the whole result Unknown.
    #[test]
    fn count_unknown_dominates() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(5u32)),
            InputCount::Unknown,
            InputCount::Known(BigUint::from(3u32)),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    /// An Enum variant (unresolved) also makes the result Unknown.
    #[test]
    fn count_unresolved_enum_returns_unknown() {
        let result = aggregate_input_count(vec![
            InputCount::Known(BigUint::from(2u32)),
            InputCount::Enum("Color".to_string()),
        ]);
        assert_eq!(result, InputCount::Unknown);
    }

    /// An empty iterator returns Known(0).
    #[test]
    fn count_empty_is_zero() {
        let result = aggregate_input_count(Vec::<InputCount>::new());
        assert_eq!(result, InputCount::Known(BigUint::from(0u32)));
    }

    /// A single Known count passes through unchanged.
    #[test]
    fn count_single_known() {
        let result =
            aggregate_input_count(std::iter::once(InputCount::Known(BigUint::from(42u32))));
        assert_eq!(result, InputCount::Known(BigUint::from(42u32)));
    }
}
