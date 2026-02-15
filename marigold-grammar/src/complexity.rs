#![allow(clippy::enum_variant_names)]

use std::fmt;
use std::str::FromStr;

use num_bigint::BigUint;
use num_traits::{One, Zero};
use pest::Parser;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::nodes::{
    InputCount, InputVariability, StreamFunctionKind, TypedExpression,
};

#[derive(pest_derive::Parser)]
#[grammar = "complexity_notation.pest"]
struct ComplexityNotationParser;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComplexityClass {
    O1,
    OLogN,
    ON,
    ONLogK(u64),
    ONLogN,
    OPolynomial(u64),
    OCombinatorial(u64),
    OPermutational(u64),
    OFactorial,
    Unknown,
}

impl ComplexityClass {
    fn ordinal(&self) -> u64 {
        match self {
            ComplexityClass::O1 => 0,
            ComplexityClass::OLogN => 1,
            ComplexityClass::ON => 2,
            ComplexityClass::ONLogK(_) => 3,
            ComplexityClass::ONLogN => 4,
            ComplexityClass::OPolynomial(k) => 5 + *k,
            ComplexityClass::OCombinatorial(_) => 1_000_000,
            ComplexityClass::OPermutational(_) => 1_000_001,
            ComplexityClass::OFactorial => 1_000_002,
            ComplexityClass::Unknown => u64::MAX,
        }
    }

    pub fn max(self, other: ComplexityClass) -> ComplexityClass {
        if self >= other { self } else { other }
    }
}

impl PartialOrd for ComplexityClass {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ComplexityClass {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ordinal().cmp(&other.ordinal())
    }
}

impl fmt::Display for ComplexityClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComplexityClass::O1 => write!(f, "O(1)"),
            ComplexityClass::OLogN => write!(f, "O(log(n))"),
            ComplexityClass::ON => write!(f, "O(n)"),
            ComplexityClass::ONLogK(k) => write!(f, "O(n*log({k}))"),
            ComplexityClass::ONLogN => write!(f, "O(n*log(n))"),
            ComplexityClass::OPolynomial(k) => write!(f, "O(n^{k})"),
            ComplexityClass::OCombinatorial(k) => write!(f, "O(C(n,{k}))"),
            ComplexityClass::OPermutational(k) => write!(f, "O(n!/(n-{k})!)"),
            ComplexityClass::OFactorial => write!(f, "O(n!)"),
            ComplexityClass::Unknown => write!(f, "O(?)"),
        }
    }
}

impl FromStr for ComplexityClass {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        ComplexityNotationParser::parse(Rule::complexity, s)
            .map_err(|e| format!("Invalid complexity notation: {e}"))?;

        if let Some(rest) = s.strip_prefix("O(n!/(n-") {
            if let Some(num_str) = rest.strip_suffix(")!)") {
                let k: u64 = num_str
                    .parse()
                    .map_err(|_| format!("Invalid permutation argument: {num_str}"))?;
                return Ok(ComplexityClass::OPermutational(k));
            }
        }
        if let Some(rest) = s.strip_prefix("O(n^") {
            if let Some(num_str) = rest.strip_suffix(')') {
                let k: u64 = num_str
                    .parse()
                    .map_err(|_| format!("Invalid exponent: {num_str}"))?;
                return Ok(ComplexityClass::OPolynomial(k));
            }
        }
        if let Some(rest) = s.strip_prefix("O(n*log(") {
            if let Some(num_str) = rest.strip_suffix("))") {
                if num_str == "n" {
                    return Ok(ComplexityClass::ONLogN);
                }
                let k: u64 = num_str
                    .parse()
                    .map_err(|_| format!("Invalid log argument: {num_str}"))?;
                return Ok(ComplexityClass::ONLogK(k));
            }
        }
        if let Some(rest) = s.strip_prefix("O(C(n,") {
            if let Some(num_str) = rest.strip_suffix("))") {
                let k: u64 = num_str
                    .parse()
                    .map_err(|_| format!("Invalid combination argument: {num_str}"))?;
                return Ok(ComplexityClass::OCombinatorial(k));
            }
        }

        match s {
            "O(1)" => Ok(ComplexityClass::O1),
            "O(log(n))" => Ok(ComplexityClass::OLogN),
            "O(n)" => Ok(ComplexityClass::ON),
            "O(n!)" => Ok(ComplexityClass::OFactorial),
            "O(?)" => Ok(ComplexityClass::Unknown),
            _ => Err(format!("Could not classify complexity expression: {s}")),
        }
    }
}

impl Serialize for ComplexityClass {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ComplexityClass {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        ComplexityClass::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbolic {
    Constant(BigUint),
    Unknown,
    Filtered(Box<Symbolic>),
    Permutations { n: Box<Symbolic>, k: u64 },
    PermutationsWithReplacement { n: Box<Symbolic>, k: u64 },
    Combinations { n: Box<Symbolic>, k: u64 },
    Min(Box<Symbolic>, Box<Symbolic>),
    Sum(Vec<Symbolic>),
}

impl Symbolic {
    pub fn try_evaluate(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(v) => Some(v.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(_) => None,
            Symbolic::Permutations { n, k } => {
                let n_val = n.try_evaluate()?;
                let k_val = *k;
                Some(falling_factorial(&n_val, k_val))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.try_evaluate()?;
                let k_val = *k;
                Some(n_val.pow(k_val as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.try_evaluate()?;
                let k_val = *k;
                Some(binomial(&n_val, k_val))
            }
            Symbolic::Min(a, b) => {
                let a_val = a.try_evaluate()?;
                let b_val = b.try_evaluate()?;
                Some(a_val.min(b_val))
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for part in parts {
                    total += part.try_evaluate()?;
                }
                Some(total)
            }
        }
    }

    pub fn classify_as_time(&self) -> ComplexityClass {
        match self {
            Symbolic::Constant(_) => ComplexityClass::O1,
            Symbolic::Unknown => ComplexityClass::Unknown,
            Symbolic::Filtered(inner) => inner.classify_as_time(),
            Symbolic::Permutations { k, .. } => ComplexityClass::OPermutational(*k),
            Symbolic::PermutationsWithReplacement { k, .. } => ComplexityClass::OPolynomial(*k),
            Symbolic::Combinations { k, .. } => ComplexityClass::OCombinatorial(*k),
            Symbolic::Min(a, _) => a.classify_as_time(),
            Symbolic::Sum(parts) => {
                let mut max_class = ComplexityClass::O1;
                for part in parts {
                    let c = part.classify_as_time();
                    if c > max_class {
                        max_class = c;
                    }
                }
                max_class
            }
        }
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::Constant(v) => write!(f, "{v}"),
            Symbolic::Unknown => write!(f, "?"),
            Symbolic::Filtered(inner) => write!(f, "\u{2264}{inner}"),
            Symbolic::Permutations { n, k } => write!(f, "P({n}, {k})"),
            Symbolic::PermutationsWithReplacement { n, k } => write!(f, "{n}^{k}"),
            Symbolic::Combinations { n, k } => write!(f, "C({n}, {k})"),
            Symbolic::Min(a, b) => write!(f, "min({a}, {b})"),
            Symbolic::Sum(parts) => {
                let strs: Vec<String> = parts.iter().map(|p| p.to_string()).collect();
                write!(f, "{}", strs.join(" + "))
            }
        }
    }
}

fn falling_factorial(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    let mut current = n.clone();
    for _ in 0..k {
        if current.is_zero() {
            return BigUint::zero();
        }
        result *= &current;
        current -= BigUint::one();
    }
    result
}

fn binomial(n: &BigUint, k: u64) -> BigUint {
    if k == 0 {
        return BigUint::one();
    }
    let numerator = falling_factorial(n, k);
    let mut denominator = BigUint::one();
    for i in 1..=k {
        denominator *= BigUint::from(i);
    }
    numerator / denominator
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StreamComplexity {
    pub description: String,
    pub cardinality: String,
    pub time_class: ComplexityClass,
    pub space_class: ComplexityClass,
    pub collects_input: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub streams: Vec<StreamComplexity>,
    pub program_time: ComplexityClass,
    pub program_space: ComplexityClass,
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match (&inp.variability, &inp.input_count) {
        (InputVariability::Constant, InputCount::Known(n)) => Symbolic::Constant(n.clone()),
        _ => Symbolic::Unknown,
    }
}

fn propagate_cardinality(cardinality: Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Map | StreamFunctionKind::OkOrPanic => cardinality,
        StreamFunctionKind::Filter | StreamFunctionKind::FilterMap | StreamFunctionKind::Ok => {
            Symbolic::Filtered(Box::new(cardinality))
        }
        StreamFunctionKind::Permutations(k) => Symbolic::Permutations {
            n: Box::new(cardinality),
            k: *k,
        },
        StreamFunctionKind::PermutationsWithReplacement(k) => {
            Symbolic::PermutationsWithReplacement {
                n: Box::new(cardinality),
                k: *k,
            }
        }
        StreamFunctionKind::Combinations(k) => Symbolic::Combinations {
            n: Box::new(cardinality),
            k: *k,
        },
        StreamFunctionKind::KeepFirstN(k) => Symbolic::Min(
            Box::new(cardinality),
            Box::new(Symbolic::Constant(BigUint::from(*k))),
        ),
        StreamFunctionKind::Fold => Symbolic::Constant(BigUint::one()),
    }
}

fn space_for_kind(kind: &StreamFunctionKind) -> ComplexityClass {
    match kind {
        StreamFunctionKind::Permutations(_)
        | StreamFunctionKind::PermutationsWithReplacement(_)
        | StreamFunctionKind::Combinations(_) => ComplexityClass::ON,
        StreamFunctionKind::KeepFirstN(_) => ComplexityClass::O1,
        StreamFunctionKind::Map
        | StreamFunctionKind::Filter
        | StreamFunctionKind::FilterMap
        | StreamFunctionKind::Fold
        | StreamFunctionKind::Ok
        | StreamFunctionKind::OkOrPanic => ComplexityClass::O1,
    }
}

fn analyze_stream_fns(
    funs: &[crate::nodes::StreamFunctionNode],
    initial_cardinality: Symbolic,
    description: &str,
) -> StreamComplexity {
    let mut cardinality = initial_cardinality;
    let mut space = ComplexityClass::O1;
    let mut collects = false;

    for f in funs {
        let fn_space = space_for_kind(&f.kind);
        if fn_space > space {
            space = fn_space;
        }
        if matches!(
            f.kind,
            StreamFunctionKind::Permutations(_)
                | StreamFunctionKind::PermutationsWithReplacement(_)
                | StreamFunctionKind::Combinations(_)
        ) {
            collects = true;
        }
        cardinality = propagate_cardinality(cardinality, &f.kind);
    }

    let time = match &cardinality {
        Symbolic::Constant(v) if v <= &BigUint::one() => ComplexityClass::O1,
        _ => {
            let base_time = cardinality.classify_as_time();
            if base_time == ComplexityClass::O1 {
                ComplexityClass::ON
            } else {
                base_time
            }
        }
    };

    StreamComplexity {
        description: description.to_string(),
        cardinality: cardinality.to_string(),
        time_class: time,
        space_class: space,
        collects_input: collects,
    }
}

fn describe_stream_fns(funs: &[crate::nodes::StreamFunctionNode]) -> String {
    funs.iter()
        .map(|f| match &f.kind {
            StreamFunctionKind::Map => "map(...)".to_string(),
            StreamFunctionKind::Filter => "filter(...)".to_string(),
            StreamFunctionKind::FilterMap => "filter_map(...)".to_string(),
            StreamFunctionKind::Permutations(k) => format!("permutations({k})"),
            StreamFunctionKind::PermutationsWithReplacement(k) => {
                format!("permutations_with_replacement({k})")
            }
            StreamFunctionKind::Combinations(k) => format!("combinations({k})"),
            StreamFunctionKind::KeepFirstN(k) => format!("keep_first_n({k}, ...)"),
            StreamFunctionKind::Fold => "fold(...)".to_string(),
            StreamFunctionKind::Ok => "ok()".to_string(),
            StreamFunctionKind::OkOrPanic => "ok_or_panic()".to_string(),
        })
        .collect::<Vec<_>>()
        .join(".")
}

pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    let mut stream_vars: std::collections::HashMap<String, (Symbolic, ComplexityClass)> =
        std::collections::HashMap::new();

    for expr in expressions {
        match expr {
            TypedExpression::StreamVariable(v) => {
                let card = input_cardinality(&v.inp);
                let mut space = ComplexityClass::O1;
                let mut current_card = card;
                for f in &v.funs {
                    let fn_space = space_for_kind(&f.kind);
                    if fn_space > space {
                        space = fn_space;
                    }
                    current_card = propagate_cardinality(current_card, &f.kind);
                }
                space = space.max(ComplexityClass::ON);
                stream_vars.insert(v.variable_name.clone(), (current_card, space));
            }
            TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                let (prior_card, prior_space) = stream_vars
                    .get(&v.prior_stream_variable)
                    .cloned()
                    .unwrap_or((Symbolic::Unknown, ComplexityClass::Unknown));
                let mut space = prior_space;
                let mut current_card = prior_card;
                for f in &v.funs {
                    let fn_space = space_for_kind(&f.kind);
                    if fn_space > space {
                        space = fn_space;
                    }
                    current_card = propagate_cardinality(current_card, &f.kind);
                }
                space = space.max(ComplexityClass::ON);
                stream_vars.insert(v.variable_name.clone(), (current_card, space));
            }
            _ => {}
        }
    }

    let mut streams = Vec::new();
    let mut program_time = ComplexityClass::O1;
    let mut program_space = ComplexityClass::O1;

    for expr in expressions {
        let sc = match expr {
            TypedExpression::UnnamedReturningStream(s) | TypedExpression::UnnamedNonReturningStream(s) => {
                let card = input_cardinality(&s.inp_and_funs.inp);
                let funs_desc = describe_stream_fns(&s.inp_and_funs.funs);
                let out_desc = if s.out.returning { "return" } else { "write_file(...)" };
                let desc = if funs_desc.is_empty() {
                    format!("input.{out_desc}")
                } else {
                    format!("input.{funs_desc}.{out_desc}")
                };
                analyze_stream_fns(&s.inp_and_funs.funs, card, &desc)
            }
            TypedExpression::NamedReturningStream(s) | TypedExpression::NamedNonReturningStream(s) => {
                let (card, var_space) = stream_vars
                    .get(&s.stream_variable)
                    .cloned()
                    .unwrap_or((Symbolic::Unknown, ComplexityClass::Unknown));
                let funs_desc = describe_stream_fns(&s.funs);
                let out_desc = if s.out.returning { "return" } else { "write_file(...)" };
                let desc = if funs_desc.is_empty() {
                    format!("{}.{out_desc}", s.stream_variable)
                } else {
                    format!("{}.{funs_desc}.{out_desc}", s.stream_variable)
                };
                let mut sc = analyze_stream_fns(&s.funs, card, &desc);
                sc.space_class = sc.space_class.max(var_space);
                sc
            }
            _ => continue,
        };

        program_time = program_time.max(sc.time_class.clone());
        program_space = program_space.max(sc.space_class.clone());
        streams.push(sc);
    }

    ProgramComplexity {
        streams,
        program_time,
        program_space,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_o1() {
        assert_eq!(ComplexityClass::from_str("O(1)").unwrap(), ComplexityClass::O1);
    }

    #[test]
    fn test_parse_on() {
        assert_eq!(ComplexityClass::from_str("O(n)").unwrap(), ComplexityClass::ON);
    }

    #[test]
    fn test_parse_ologn() {
        assert_eq!(ComplexityClass::from_str("O(log(n))").unwrap(), ComplexityClass::OLogN);
    }

    #[test]
    fn test_parse_on_squared() {
        assert_eq!(
            ComplexityClass::from_str("O(n^2)").unwrap(),
            ComplexityClass::OPolynomial(2)
        );
    }

    #[test]
    fn test_parse_on_cubed() {
        assert_eq!(
            ComplexityClass::from_str("O(n^3)").unwrap(),
            ComplexityClass::OPolynomial(3)
        );
    }

    #[test]
    fn test_parse_on_factorial() {
        assert_eq!(
            ComplexityClass::from_str("O(n!)").unwrap(),
            ComplexityClass::OFactorial
        );
    }

    #[test]
    fn test_parse_onlogn() {
        assert_eq!(
            ComplexityClass::from_str("O(n*log(n))").unwrap(),
            ComplexityClass::ONLogN
        );
    }

    #[test]
    fn test_parse_onlogk() {
        assert_eq!(
            ComplexityClass::from_str("O(n*log(5))").unwrap(),
            ComplexityClass::ONLogK(5)
        );
    }

    #[test]
    fn test_parse_combinatorial() {
        assert_eq!(
            ComplexityClass::from_str("O(C(n,3))").unwrap(),
            ComplexityClass::OCombinatorial(3)
        );
    }

    #[test]
    fn test_parse_permutational() {
        assert_eq!(
            ComplexityClass::from_str("O(n!/(n-3)!)").unwrap(),
            ComplexityClass::OPermutational(3)
        );
    }

    #[test]
    fn test_parse_unknown() {
        assert_eq!(
            ComplexityClass::from_str("O(?)").unwrap(),
            ComplexityClass::Unknown
        );
    }

    #[test]
    fn test_roundtrip_all_variants() {
        let variants = vec![
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogK(10),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(2),
            ComplexityClass::OPolynomial(5),
            ComplexityClass::OCombinatorial(3),
            ComplexityClass::OPermutational(4),
            ComplexityClass::OFactorial,
            ComplexityClass::Unknown,
        ];
        for v in variants {
            let s = v.to_string();
            let parsed = ComplexityClass::from_str(&s).unwrap();
            assert_eq!(v, parsed, "Roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_reject_invalid() {
        assert!(ComplexityClass::from_str("O()").is_err());
        assert!(ComplexityClass::from_str("O(x)").is_err());
        assert!(ComplexityClass::from_str("n").is_err());
        assert!(ComplexityClass::from_str("").is_err());
    }

    #[test]
    fn test_ordering() {
        assert!(ComplexityClass::O1 < ComplexityClass::OLogN);
        assert!(ComplexityClass::OLogN < ComplexityClass::ON);
        assert!(ComplexityClass::ON < ComplexityClass::ONLogN);
        assert!(ComplexityClass::ONLogN < ComplexityClass::OPolynomial(2));
        assert!(ComplexityClass::OPolynomial(2) < ComplexityClass::OPolynomial(3));
        assert!(ComplexityClass::OPolynomial(3) < ComplexityClass::OFactorial);
    }

    #[test]
    fn test_evaluate_constant() {
        let s = Symbolic::Constant(BigUint::from(100u64));
        assert_eq!(s.try_evaluate(), Some(BigUint::from(100u64)));
    }

    #[test]
    fn test_evaluate_unknown() {
        assert_eq!(Symbolic::Unknown.try_evaluate(), None);
    }

    #[test]
    fn test_evaluate_permutations() {
        let s = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(10u64))),
            k: 3,
        };
        assert_eq!(s.try_evaluate(), Some(BigUint::from(720u64)));
    }

    #[test]
    fn test_evaluate_combinations() {
        let s = Symbolic::Combinations {
            n: Box::new(Symbolic::Constant(BigUint::from(10u64))),
            k: 2,
        };
        assert_eq!(s.try_evaluate(), Some(BigUint::from(45u64)));
    }

    #[test]
    fn test_evaluate_filtered() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert_eq!(s.try_evaluate(), None);
    }

    #[test]
    fn test_evaluate_min() {
        let s = Symbolic::Min(
            Box::new(Symbolic::Constant(BigUint::from(100u64))),
            Box::new(Symbolic::Constant(BigUint::from(5u64))),
        );
        assert_eq!(s.try_evaluate(), Some(BigUint::from(5u64)));
    }

    #[test]
    fn test_evaluate_sum() {
        let s = Symbolic::Sum(vec![
            Symbolic::Constant(BigUint::from(10u64)),
            Symbolic::Constant(BigUint::from(20u64)),
        ]);
        assert_eq!(s.try_evaluate(), Some(BigUint::from(30u64)));
    }

    #[test]
    fn test_constant_range_cardinality() {
        let inp = crate::nodes::InputFunctionNode {
            variability: InputVariability::Constant,
            input_count: InputCount::Known(BigUint::from(100u64)),
            code: String::new(),
        };
        let card = input_cardinality(&inp);
        assert_eq!(card, Symbolic::Constant(BigUint::from(100u64)));
    }

    #[test]
    fn test_map_preserves_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card.clone(), &StreamFunctionKind::Map);
        assert_eq!(result, card);
    }

    #[test]
    fn test_filter_wraps_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card.clone(), &StreamFunctionKind::Filter);
        assert_eq!(result, Symbolic::Filtered(Box::new(card)));
    }

    #[test]
    fn test_permutations_cardinality() {
        let card = Symbolic::Constant(BigUint::from(10u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::Permutations(3));
        assert!(matches!(result, Symbolic::Permutations { k: 3, .. }));
    }

    #[test]
    fn test_combinations_cardinality() {
        let card = Symbolic::Constant(BigUint::from(10u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::Combinations(2));
        assert!(matches!(result, Symbolic::Combinations { k: 2, .. }));
    }

    #[test]
    fn test_fold_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::Fold);
        assert_eq!(result, Symbolic::Constant(BigUint::one()));
    }

    #[test]
    fn test_keep_first_n_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::KeepFirstN(5));
        assert!(matches!(result, Symbolic::Min(_, _)));
        assert_eq!(result.try_evaluate(), Some(BigUint::from(5u64)));
    }

    #[test]
    fn test_chained_filters() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let r1 = propagate_cardinality(card, &StreamFunctionKind::Filter);
        let r2 = propagate_cardinality(r1.clone(), &StreamFunctionKind::Filter);
        assert!(matches!(r2, Symbolic::Filtered(inner) if matches!(*inner, Symbolic::Filtered(_))));
    }

    #[test]
    fn test_streaming_ops_space_o1() {
        assert_eq!(space_for_kind(&StreamFunctionKind::Map), ComplexityClass::O1);
        assert_eq!(space_for_kind(&StreamFunctionKind::Filter), ComplexityClass::O1);
        assert_eq!(space_for_kind(&StreamFunctionKind::FilterMap), ComplexityClass::O1);
        assert_eq!(space_for_kind(&StreamFunctionKind::Fold), ComplexityClass::O1);
        assert_eq!(space_for_kind(&StreamFunctionKind::Ok), ComplexityClass::O1);
        assert_eq!(space_for_kind(&StreamFunctionKind::OkOrPanic), ComplexityClass::O1);
    }

    #[test]
    fn test_permutations_space_on() {
        assert_eq!(space_for_kind(&StreamFunctionKind::Permutations(3)), ComplexityClass::ON);
    }

    #[test]
    fn test_combinations_space_on() {
        assert_eq!(space_for_kind(&StreamFunctionKind::Combinations(2)), ComplexityClass::ON);
    }

    #[test]
    fn test_keep_first_n_space_o1() {
        assert_eq!(space_for_kind(&StreamFunctionKind::KeepFirstN(5)), ComplexityClass::O1);
    }

    #[test]
    fn test_serde_roundtrip() {
        let c = ComplexityClass::OPolynomial(3);
        let json = serde_json::to_string(&c).unwrap();
        assert_eq!(json, r#""O(n^3)""#);
        let parsed: ComplexityClass = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, c);
    }

    #[test]
    fn test_symbolic_display() {
        assert_eq!(Symbolic::Constant(BigUint::from(100u64)).to_string(), "100");
        assert_eq!(Symbolic::Unknown.to_string(), "?");
        assert_eq!(
            Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64)))).to_string(),
            "\u{2264}100"
        );
    }

    #[test]
    fn test_analyze_streaming_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 100).map(double).filter(is_odd).return")
                .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
        assert!(!result.streams[0].collects_input);
    }

    #[test]
    fn test_analyze_collecting_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 100).permutations(3).return").unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::ON);
        assert!(result.streams[0].collects_input);
    }

    #[test]
    fn test_analyze_mixed_pipeline() {
        let result = crate::parser::PestParser::analyze(
            "range(0, 100).filter(is_odd).permutations(3).map(identity).return",
        )
        .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::ON);
    }

    #[test]
    fn test_analyze_fold_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 100).fold(0, add).return").unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].cardinality, "1");
        assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    }

    #[test]
    fn test_analyze_select_all() {
        let result = crate::parser::PestParser::analyze(
            "select_all(range(0, 10), range(0, 20)).return",
        )
        .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].cardinality, "30");
    }

    #[test]
    fn test_analyze_stream_variable() {
        let input = "fn double(x: i32) -> i32 { x * 2 }\ndigits = range(0, 10)\ndigits.map(double).return";
        let result = crate::parser::PestParser::analyze(input).unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::ON);
    }

    #[test]
    fn test_analyze_program_aggregation() {
        let input = "fn identity(x: i32) -> i32 { x }\nrange(0, 10).map(identity).return\nrange(0, 10).permutations(2).return";
        let result = crate::parser::PestParser::analyze(input).unwrap();
        assert_eq!(result.streams.len(), 2);
        assert_eq!(result.program_space, ComplexityClass::ON);
    }

    #[test]
    fn test_analyze_combinations_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 10).combinations(2).return").unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::ON);
        assert!(result.streams[0].collects_input);
    }

    #[test]
    fn test_analyze_keep_first_n_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 100).keep_first_n(5, compare).return")
                .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    }

    #[test]
    fn test_analyze_chained_collectors() {
        let result = crate::parser::PestParser::analyze(
            "range(0, 10).permutations(2).combinations(2).return",
        )
        .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::ON);
        assert!(result.streams[0].collects_input);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn arb_complexity_class() -> impl Strategy<Value = ComplexityClass> {
        prop_oneof![
            Just(ComplexityClass::O1),
            Just(ComplexityClass::OLogN),
            Just(ComplexityClass::ON),
            (1..100u64).prop_map(ComplexityClass::ONLogK),
            Just(ComplexityClass::ONLogN),
            (2..20u64).prop_map(ComplexityClass::OPolynomial),
            (1..20u64).prop_map(ComplexityClass::OCombinatorial),
            (1..20u64).prop_map(ComplexityClass::OPermutational),
            Just(ComplexityClass::OFactorial),
            Just(ComplexityClass::Unknown),
        ]
    }

    proptest! {
        #[test]
        fn test_complexity_class_ordering_is_total(a in arb_complexity_class(), b in arb_complexity_class()) {
            prop_assert!(a.partial_cmp(&b).is_some());
        }
    }

    proptest! {
        #[test]
        fn test_display_fromstr_roundtrip(c in arb_complexity_class()) {
            let s = c.to_string();
            let parsed = ComplexityClass::from_str(&s).unwrap();
            prop_assert_eq!(c, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_space_monotonicity_collecting_op(k in 1..10u64) {
            let collecting_ops = vec![
                StreamFunctionKind::Permutations(k),
                StreamFunctionKind::Combinations(k),
                StreamFunctionKind::PermutationsWithReplacement(k),
            ];
            for op in &collecting_ops {
                let space = space_for_kind(op);
                prop_assert!(space >= ComplexityClass::ON, "Collecting op {:?} should have space >= O(n)", op);
            }
        }
    }

    proptest! {
        #[test]
        fn test_streaming_ops_always_o1_space(_dummy in 0..1i32) {
            let streaming_ops = vec![
                StreamFunctionKind::Map,
                StreamFunctionKind::Filter,
                StreamFunctionKind::FilterMap,
                StreamFunctionKind::Fold,
                StreamFunctionKind::Ok,
                StreamFunctionKind::OkOrPanic,
            ];
            for op in &streaming_ops {
                prop_assert_eq!(space_for_kind(op), ComplexityClass::O1);
            }
        }
    }

    proptest! {
        #[test]
        fn test_serde_roundtrip_proptest(c in arb_complexity_class()) {
            let json = serde_json::to_string(&c).unwrap();
            let parsed: ComplexityClass = serde_json::from_str(&json).unwrap();
            prop_assert_eq!(c, parsed);
        }
    }
}
