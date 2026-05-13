#![allow(clippy::enum_variant_names)]

use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

/// Implement `PartialOrd` and `Ord` for a type that has an `ordinal()` method.
macro_rules! impl_ord_via_ordinal {
    ($ty:ty) => {
        impl PartialOrd for $ty {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for $ty {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.ordinal().cmp(&other.ordinal())
            }
        }
    };
}

use num_bigint::BigUint;
use num_traits::{One, Zero};
use pest::Parser;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::nodes::{InputCount, InputVariability, StreamFunctionKind, TypedExpression};

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
            ComplexityClass::OCombinatorial(k) => 1_000_000 + *k,
            ComplexityClass::OPermutational(k) => 2_000_000 + *k,
            ComplexityClass::OFactorial => 10_000_000,
            ComplexityClass::Unknown => u64::MAX,
        }
    }
}

impl_ord_via_ordinal!(ComplexityClass);

impl Default for ComplexityClass {
    fn default() -> Self {
        ComplexityClass::Unknown
    }
}

impl fmt::Display for ComplexityClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComplexityClass::O1 => write!(f, "O(1)"),
            ComplexityClass::OLogN => write!(f, "O(log n)"),
            ComplexityClass::ON => write!(f, "O(n)"),
            ComplexityClass::ONLogK(k) => write!(f, "O(n log {k})"),
            ComplexityClass::ONLogN => write!(f, "O(n log n)"),
            ComplexityClass::OPolynomial(k) => write!(f, "O(n^{k})"),
            ComplexityClass::OCombinatorial(k) => write!(f, "O(n!/(n-{k})!/{k}!)"),
            ComplexityClass::OPermutational(k) => write!(f, "O(n!/(n-{k})!)"),
            ComplexityClass::OFactorial => write!(f, "O(n!)"),
            ComplexityClass::Unknown => write!(f, "O(?)"),
        }
    }
}

impl FromStr for ComplexityClass {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try the pest grammar first
        match ComplexityNotationParser::parse(Rule::complexity, s) {
            Ok(mut pairs) => {
                let pair = pairs.next().unwrap();
                parse_complexity_pair(pair)
            }
            Err(e) => Err(format!("Failed to parse complexity class '{s}': {e}")),
        }
    }
}

fn parse_complexity_pair(
    pair: pest::iterators::Pair<Rule>,
) -> Result<ComplexityClass, String> {
    match pair.as_rule() {
        Rule::complexity => {
            let inner = pair.into_inner().next().unwrap();
            parse_complexity_pair(inner)
        }
        Rule::o1 => Ok(ComplexityClass::O1),
        Rule::o_log_n => Ok(ComplexityClass::OLogN),
        Rule::o_n => Ok(ComplexityClass::ON),
        Rule::o_n_log_k => {
            let k_str = pair
                .into_inner()
                .next()
                .unwrap()
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("Invalid k in O(n log k): {e}"))?;
            Ok(ComplexityClass::ONLogK(k))
        }
        Rule::o_n_log_n => Ok(ComplexityClass::ONLogN),
        Rule::o_polynomial => {
            let k_str = pair
                .into_inner()
                .next()
                .unwrap()
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("Invalid exponent in O(n^k): {e}"))?;
            Ok(ComplexityClass::OPolynomial(k))
        }
        Rule::o_combinatorial => {
            let k_str = pair
                .into_inner()
                .next()
                .unwrap()
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("Invalid k in O(n!/(n-k)!/k!): {e}"))?;
            Ok(ComplexityClass::OCombinatorial(k))
        }
        Rule::o_permutational => {
            let k_str = pair
                .into_inner()
                .next()
                .unwrap()
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("Invalid k in O(n!/(n-k)!): {e}"))?;
            Ok(ComplexityClass::OPermutational(k))
        }
        Rule::o_factorial => Ok(ComplexityClass::OFactorial),
        Rule::o_unknown => Ok(ComplexityClass::Unknown),
        _ => Err(format!("Unexpected rule: {:?}", pair.as_rule())),
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
pub struct ExactComplexity {
    pub time: Symbolic,
    pub space: Symbolic,
}

impl ExactComplexity {
    pub fn unknown() -> Self {
        ExactComplexity {
            time: Symbolic::Unknown,
            space: Symbolic::Unknown,
        }
    }
}

impl Default for ExactComplexity {
    fn default() -> Self {
        ExactComplexity::unknown()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbolic {
    Constant(BigUint),
    N,
    Filtered(Box<Symbolic>),
    Sum(Vec<Symbolic>),
    Product(Vec<Symbolic>),
    Unknown,
}

impl Symbolic {
    pub fn complexity_class(&self) -> ComplexityClass {
        match self {
            Symbolic::Constant(_) => ComplexityClass::O1,
            Symbolic::N => ComplexityClass::ON,
            Symbolic::Filtered(inner) => {
                // A filter reduces by a constant fraction, keeping O class the same
                inner.complexity_class()
            }
            Symbolic::Sum(terms) => terms
                .iter()
                .map(|t| t.complexity_class())
                .max()
                .unwrap_or(ComplexityClass::O1),
            Symbolic::Product(factors) => {
                // Determine whether product is polynomial, etc.
                let n_count = factors
                    .iter()
                    .filter(|f| matches!(f, Symbolic::N))
                    .count() as u64;
                let has_unknown = factors.iter().any(|f| matches!(f, Symbolic::Unknown));
                if has_unknown {
                    ComplexityClass::Unknown
                } else if n_count >= 2 {
                    ComplexityClass::OPolynomial(n_count)
                } else if n_count == 1 {
                    ComplexityClass::ON
                } else {
                    ComplexityClass::O1
                }
            }
            Symbolic::Unknown => ComplexityClass::Unknown,
        }
    }
}

impl Default for Symbolic {
    fn default() -> Self {
        Symbolic::Unknown
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::Constant(n) => write!(f, "{n}"),
            Symbolic::N => write!(f, "n"),
            Symbolic::Filtered(inner) => write!(f, "filtered({inner})"),
            Symbolic::Sum(terms) => {
                let parts: Vec<String> = terms.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", parts.join(" + "))
            }
            Symbolic::Product(factors) => {
                let parts: Vec<String> = factors.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", parts.join(" * "))
            }
            Symbolic::Unknown => write!(f, "?"),
        }
    }
}

impl FromStr for Symbolic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s == "n" {
            return Ok(Symbolic::N);
        }
        if s == "?" {
            return Ok(Symbolic::Unknown);
        }
        // Try parsing as a BigUint constant
        if let Ok(n) = BigUint::parse_bytes(s.as_bytes(), 10) {
            return Ok(Symbolic::Constant(n));
        }
        // filtered(...)
        if s.starts_with("filtered(") && s.ends_with(')') {
            let inner = &s["filtered(".len()..s.len() - 1];
            return Symbolic::from_str(inner).map(|i| Symbolic::Filtered(Box::new(i)));
        }
        // (... + ...) or (... * ...)
        if s.starts_with('(') && s.ends_with(')') {
            let inner = &s[1..s.len() - 1];
            // Try sum first
            if inner.contains(" + ") {
                let parts: Result<Vec<Symbolic>, _> =
                    inner.split(" + ").map(Symbolic::from_str).collect();
                return parts.map(Symbolic::Sum);
            }
            // Try product
            if inner.contains(" * ") {
                let parts: Result<Vec<Symbolic>, _> =
                    inner.split(" * ").map(Symbolic::from_str).collect();
                return parts.map(Symbolic::Product);
            }
        }
        Err(format!("Cannot parse Symbolic from '{s}'"))
    }
}

impl Serialize for Symbolic {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Symbolic {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Symbolic::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    Unknown,
    Exact(BigUint),
    Bounded(Symbolic),
}

impl_ord_via_ordinal!(Cardinality);

impl Cardinality {
    fn ordinal(&self) -> u64 {
        match self {
            Cardinality::Unknown => u64::MAX,
            Cardinality::Exact(_) => 0,
            Cardinality::Bounded(_) => 1,
        }
    }

    fn to_symbolic(&self) -> Symbolic {
        match self {
            Cardinality::Unknown => Symbolic::Unknown,
            Cardinality::Exact(n) => Symbolic::Constant(n.clone()),
            Cardinality::Bounded(s) => s.clone(),
        }
    }
}

impl Default for Cardinality {
    fn default() -> Self {
        Cardinality::Unknown
    }
}

impl fmt::Display for Cardinality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cardinality::Unknown => write!(f, "?"),
            Cardinality::Exact(n) => write!(f, "{n}"),
            Cardinality::Bounded(s) => write!(f, "{s}"),
        }
    }
}

impl FromStr for Cardinality {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "?" {
            return Ok(Cardinality::Unknown);
        }
        if let Ok(n) = BigUint::parse_bytes(s.as_bytes(), 10) {
            return Ok(Cardinality::Exact(n));
        }
        // Try parsing as Symbolic for Bounded
        Symbolic::from_str(s).map(Cardinality::Bounded)
    }
}

impl Serialize for Cardinality {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Cardinality {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Cardinality::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub program_time: ComplexityClass,
    pub program_exact_time: ExactComplexity,
    pub program_space: ComplexityClass,
    pub program_exact_space: ExactComplexity,
    pub program_cardinality: Cardinality,
    /// When true, the analysis assumes user-defined functions are O(1); set when any
    /// `FnDeclaration` node exists in the program. Callers should treat complexity results
    /// conservatively if this flag is true, because the actual cost of those functions is
    /// not analyzed.
    #[serde(default)]
    pub assumes_o1_user_fns: bool,
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match inp.count {
        InputCount::Unbounded => Symbolic::Unknown,
        InputCount::Bounded => Symbolic::N,
        InputCount::Single => Symbolic::Constant(BigUint::one()),
    }
}

fn output_cardinality(
    inp: Symbolic,
    fun: &crate::nodes::StreamFunctionNode,
    fn_map: &std::collections::HashMap<
        String,
        (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    >,
) -> (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity) {
    match &fun.kind {
        StreamFunctionKind::Map => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::Filter => {
            let filtered = Symbolic::Filtered(Box::new(inp.clone()));
            (
                filtered.clone(),
                filtered.complexity_class(),
                ExactComplexity {
                    time: inp.clone(),
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: filtered,
                },
            )
        }
        StreamFunctionKind::FlatMap => {
            // FlatMap can expand each element; treat as O(n) in worst case per element
            let expanded = Symbolic::Product(vec![inp.clone(), Symbolic::N]);
            (
                expanded.clone(),
                expanded.complexity_class(),
                ExactComplexity {
                    time: expanded.clone(),
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: expanded,
                },
            )
        }
        StreamFunctionKind::Fold => {
            // Fold collapses the stream to a single value
            (
                Symbolic::Constant(BigUint::one()),
                ComplexityClass::O1,
                ExactComplexity {
                    time: inp.clone(),
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: Symbolic::Constant(BigUint::one()),
                },
            )
        }
        StreamFunctionKind::Zip => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::Take => {
            let taken = Symbolic::Filtered(Box::new(inp.clone()));
            (
                taken.clone(),
                taken.complexity_class(),
                ExactComplexity {
                    time: inp,
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: taken,
                },
            )
        }
        StreamFunctionKind::Skip => {
            let skipped = Symbolic::Filtered(Box::new(inp.clone()));
            (
                skipped.clone(),
                skipped.complexity_class(),
                ExactComplexity {
                    time: inp,
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: skipped,
                },
            )
        }
        StreamFunctionKind::Collect => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: inp.clone(),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::Sort => {
            // Sort is O(n log n)
            let sorted_time = Symbolic::Product(vec![
                inp.clone(),
                Symbolic::Filtered(Box::new(inp.clone())),
            ]);
            (
                inp.clone(),
                ComplexityClass::ONLogN,
                ExactComplexity {
                    time: sorted_time,
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: inp,
                },
            )
        }
        StreamFunctionKind::Enumerate => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::UserDefined => {
            // Look up user-defined function cost
            if let Some(fn_name) = &fun.name {
                if let Some((_card, _time_class, time_exact, space_exact)) = fn_map.get(fn_name) {
                    return (
                        inp.clone(),
                        time_exact.time.complexity_class(),
                        ExactComplexity {
                            time: Symbolic::Product(vec![inp.clone(), time_exact.time.clone()]),
                            space: Symbolic::Constant(BigUint::one()),
                        },
                        ExactComplexity {
                            time: Symbolic::Constant(BigUint::zero()),
                            space: Symbolic::Product(vec![inp, space_exact.space.clone()]),
                        },
                    );
                }
            }
            // Default: treat as O(1) per element
            (
                inp.clone(),
                inp.complexity_class(),
                ExactComplexity {
                    time: inp.clone(),
                    space: Symbolic::Constant(BigUint::one()),
                },
                ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: inp,
                },
            )
        }
        StreamFunctionKind::Chain => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::Merge => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
        StreamFunctionKind::Return => (
            inp.clone(),
            inp.complexity_class(),
            ExactComplexity {
                time: inp.clone(),
                space: Symbolic::Constant(BigUint::one()),
            },
            ExactComplexity {
                time: Symbolic::Constant(BigUint::zero()),
                space: inp,
            },
        ),
    }
}

fn combine_complexities(
    a: (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    b: (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
) -> (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity) {
    let card = b.0;
    let time_class = a.1.max(b.1);
    let combined_time = ExactComplexity {
        time: Symbolic::Sum(vec![a.2.time, b.2.time]),
        space: Symbolic::Sum(vec![a.2.space, b.2.space]),
    };
    let combined_space = ExactComplexity {
        time: Symbolic::Sum(vec![a.3.time, b.3.time]),
        space: Symbolic::Sum(vec![a.3.space, b.3.space]),
    };
    (card, time_class, combined_time, combined_space)
}

fn describe_stream_fns(funs: &[crate::nodes::StreamFunctionNode]) -> String {
    funs.iter()
        .map(|f| format!("{:?}", f.kind))
        .collect::<Vec<_>>()
        .join(" -> ")
}

pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    let mut has_fn_declaration = false;
    let mut stream_vars: std::collections::HashMap<
        String,
        (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    > = std::collections::HashMap::new();

    for expr in expressions {
        match expr {
            TypedExpression::FnDeclaration(_) => {
                has_fn_declaration = true;
            }
            TypedExpression::StreamVariable(v) => {
                let card = input_cardinality(&v.inp);
                let mut current_card = card;
                let mut current_time_class = current_card.complexity_class();
                let mut current_exact_time = ExactComplexity {
                    time: current_card.clone(),
                    space: Symbolic::Constant(BigUint::one()),
                };
                let mut current_exact_space = ExactComplexity {
                    time: Symbolic::Constant(BigUint::zero()),
                    space: current_card.clone(),
                };

                for fun in &v.fns {
                    let (new_card, new_time_class, new_exact_time, new_exact_space) =
                        output_cardinality(current_card.clone(), fun, &stream_vars);
                    (current_card, current_time_class, current_exact_time, current_exact_space) =
                        combine_complexities(
                            (
                                current_card,
                                current_time_class,
                                current_exact_time,
                                current_exact_space,
                            ),
                            (
                                new_card,
                                new_time_class,
                                new_exact_time,
                                new_exact_space,
                            ),
                        );
                }

                stream_vars.insert(
                    v.name.clone(),
                    (
                        current_card,
                        current_time_class,
                        current_exact_time,
                        current_exact_space,
                    ),
                );
            }
        }
    }

    // Aggregate all stream variables
    let mut program_time = ComplexityClass::O1;
    let mut program_exact_time = ExactComplexity {
        time: Symbolic::Constant(BigUint::zero()),
        space: Symbolic::Constant(BigUint::zero()),
    };
    let mut program_space = ComplexityClass::O1;
    let mut program_exact_space = ExactComplexity {
        time: Symbolic::Constant(BigUint::zero()),
        space: Symbolic::Constant(BigUint::zero()),
    };
    let mut program_cardinality = Cardinality::Exact(BigUint::zero());

    for (_name, (card, time_class, exact_time, exact_space)) in &stream_vars {
        program_time = program_time.max(time_class.clone());
        program_exact_time.time = Symbolic::Sum(vec![
            program_exact_time.time.clone(),
            exact_time.time.clone(),
        ]);
        program_exact_time.space = Symbolic::Sum(vec![
            program_exact_time.space.clone(),
            exact_time.space.clone(),
        ]);
        program_space = program_space.max(exact_space.space.complexity_class());
        program_exact_space.space =
            Symbolic::Sum(vec![program_exact_space.space.clone(), exact_space.space.clone()]);

        let card_symbolic = match card {
            Symbolic::Unknown => Cardinality::Unknown,
            Symbolic::Constant(n) => Cardinality::Exact(n.clone()),
            _ => Cardinality::Bounded(card.clone()),
        };
        program_cardinality = program_cardinality.max(card_symbolic);
    }

    ProgramComplexity {
        program_time,
        program_exact_time,
        program_space,
        program_exact_space,
        program_cardinality,
        assumes_o1_user_fns: has_fn_declaration,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::BigUint;
    use proptest::prelude::*;

    #[test]
    fn test_o1() {
        assert_eq!(ComplexityClass::O1.to_string(), "O(1)");
        assert_eq!(ComplexityClass::from_str("O(1)").unwrap(), ComplexityClass::O1);
    }

    #[test]
    fn test_ologn() {
        assert_eq!(ComplexityClass::OLogN.to_string(), "O(log n)");
        assert_eq!(
            ComplexityClass::from_str("O(log n)").unwrap(),
            ComplexityClass::OLogN
        );
    }

    #[test]
    fn test_on() {
        assert_eq!(ComplexityClass::ON.to_string(), "O(n)");
        assert_eq!(ComplexityClass::from_str("O(n)").unwrap(), ComplexityClass::ON);
    }

    #[test]
    fn test_on_log_k() {
        assert_eq!(ComplexityClass::ONLogK(3).to_string(), "O(n log 3)");
        assert_eq!(
            ComplexityClass::from_str("O(n log 3)").unwrap(),
            ComplexityClass::ONLogK(3)
        );
    }

    #[test]
    fn test_onlogn() {
        assert_eq!(ComplexityClass::ONLogN.to_string(), "O(n log n)");
        assert_eq!(
            ComplexityClass::from_str("O(n log n)").unwrap(),
            ComplexityClass::ONLogN
        );
    }

    #[test]
    fn test_polynomial() {
        assert_eq!(ComplexityClass::OPolynomial(2).to_string(), "O(n^2)");
        assert_eq!(
            ComplexityClass::from_str("O(n^2)").unwrap(),
            ComplexityClass::OPolynomial(2)
        );
    }

    #[test]
    fn test_polynomial_high() {
        assert_eq!(ComplexityClass::OPolynomial(5).to_string(), "O(n^5)");
        assert_eq!(
            ComplexityClass::from_str("O(n^5)").unwrap(),
            ComplexityClass::OPolynomial(5)
        );
    }

    #[test]
    fn test_combinatorial() {
        assert_eq!(
            ComplexityClass::OCombinatorial(3).to_string(),
            "O(n!/(n-3)!/3!)"
        );
        assert_eq!(
            ComplexityClass::from_str("O(n!/(n-3)!/3!)").unwrap(),
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
            ComplexityClass::ONLogK(2),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(3),
            ComplexityClass::OCombinatorial(4),
            ComplexityClass::OPermutational(5),
            ComplexityClass::OFactorial,
            ComplexityClass::Unknown,
        ];
        for variant in variants {
            let s = variant.to_string();
            let parsed = ComplexityClass::from_str(&s).unwrap();
            assert_eq!(variant, parsed, "Roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_complexity_ordering() {
        assert!(ComplexityClass::O1 < ComplexityClass::OLogN);
        assert!(ComplexityClass::OLogN < ComplexityClass::ON);
        assert!(ComplexityClass::ON < ComplexityClass::ONLogK(2));
        assert!(ComplexityClass::ONLogK(2) < ComplexityClass::ONLogN);
        assert!(ComplexityClass::ONLogN < ComplexityClass::OPolynomial(2));
        assert!(ComplexityClass::OPolynomial(2) < ComplexityClass::OPolynomial(3));
        assert!(ComplexityClass::OPolynomial(10) < ComplexityClass::OCombinatorial(2));
        assert!(ComplexityClass::OCombinatorial(2) < ComplexityClass::OPermutational(2));
        assert!(ComplexityClass::OPermutational(2) < ComplexityClass::OFactorial);
        assert!(ComplexityClass::OFactorial < ComplexityClass::Unknown);
    }

    #[test]
    fn test_default() {
        assert_eq!(ComplexityClass::default(), ComplexityClass::Unknown);
    }

    #[test]
    fn test_o1_serialize() {
        let c = ComplexityClass::O1;
        let json = serde_json::to_string(&c).unwrap();
        assert_eq!(json, r#""O(1)""#);
    }

    #[test]
    fn test_o1_deserialize() {
        let json = r#""O(1)""#;
        let c: ComplexityClass = serde_json::from_str(json).unwrap();
        assert_eq!(c, ComplexityClass::O1);
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
    fn test_complexity_class_from_symbolic() {
        assert_eq!(Symbolic::Constant(BigUint::from(5u64)).complexity_class(), ComplexityClass::O1);
        assert_eq!(Symbolic::N.complexity_class(), ComplexityClass::ON);
        assert_eq!(
            Symbolic::Product(vec![Symbolic::N, Symbolic::N]).complexity_class(),
            ComplexityClass::OPolynomial(2)
        );
        assert_eq!(
            Symbolic::Product(vec![Symbolic::N, Symbolic::N, Symbolic::N]).complexity_class(),
            ComplexityClass::OPolynomial(3)
        );
        assert_eq!(Symbolic::Unknown.complexity_class(), ComplexityClass::Unknown);
    }

    #[test]
    fn test_symbolic_display_constant() {
        assert_eq!(Symbolic::Constant(BigUint::from(42u64)).to_string(), "42");
    }

    #[test]
    fn test_symbolic_display_n() {
        assert_eq!(Symbolic::N.to_string(), "n");
    }

    #[test]
    fn test_symbolic_display_filtered() {
        assert_eq!(
            Symbolic::Filtered(Box::new(Symbolic::N)).to_string(),
            "filtered(n)"
        );
    }

    #[test]
    fn test_symbolic_display_sum() {
        assert_eq!(
            Symbolic::Sum(vec![Symbolic::N, Symbolic::Constant(BigUint::from(1u64))]).to_string(),
            "(n + 1)"
        );
    }

    #[test]
    fn test_symbolic_display_product() {
        assert_eq!(
            Symbolic::Product(vec![Symbolic::N, Symbolic::N]).to_string(),
            "(n * n)"
        );
    }

    #[test]
    fn test_symbolic_display_unknown() {
        assert_eq!(Symbolic::Unknown.to_string(), "?");
    }

    #[test]
    fn test_symbolic_parse_n() {
        assert_eq!(Symbolic::from_str("n").unwrap(), Symbolic::N);
    }

    #[test]
    fn test_symbolic_parse_unknown() {
        assert_eq!(Symbolic::from_str("?").unwrap(), Symbolic::Unknown);
    }

    #[test]
    fn test_symbolic_parse_constant() {
        assert_eq!(
            Symbolic::from_str("42").unwrap(),
            Symbolic::Constant(BigUint::from(42u64))
        );
    }

    #[test]
    fn test_symbolic_parse_filtered() {
        assert_eq!(
            Symbolic::from_str("filtered(n)").unwrap(),
            Symbolic::Filtered(Box::new(Symbolic::N))
        );
    }

    #[test]
    fn test_symbolic_parse_sum() {
        assert_eq!(
            Symbolic::from_str("(n + 1)").unwrap(),
            Symbolic::Sum(vec![Symbolic::N, Symbolic::Constant(BigUint::from(1u64))])
        );
    }

    #[test]
    fn test_symbolic_parse_product() {
        assert_eq!(
            Symbolic::from_str("(n * n)").unwrap(),
            Symbolic::Product(vec![Symbolic::N, Symbolic::N])
        );
    }

    #[test]
    fn test_symbolic_roundtrip_n() {
        let sym = Symbolic::N;
        let s = sym.to_string();
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_symbolic_roundtrip_constant() {
        let sym = Symbolic::Constant(BigUint::from(100u64));
        let s = sym.to_string();
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_symbolic_roundtrip_filtered() {
        let sym = Symbolic::Filtered(Box::new(Symbolic::N));
        let s = sym.to_string();
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_symbolic_roundtrip_sum() {
        let sym = Symbolic::Sum(vec![
            Symbolic::N,
            Symbolic::Constant(BigUint::from(1u64)),
        ]);
        let s = sym.to_string();
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_symbolic_roundtrip_product() {
        let sym = Symbolic::Product(vec![Symbolic::N, Symbolic::N]);
        let s = sym.to_string();
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_symbolic_unknown() {
        let sym = Symbolic::Unknown;
        let s = sym.to_string();
        assert_eq!(s, "?");
        assert_eq!(Symbolic::from_str(&s).unwrap(), sym);
    }

    #[test]
    fn test_cardinality_default() {
        assert_eq!(Cardinality::default(), Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_display_unknown() {
        assert_eq!(Cardinality::Unknown.to_string(), "?");
    }

    #[test]
    fn test_cardinality_display_exact() {
        assert_eq!(
            Cardinality::Exact(BigUint::from(42u64)).to_string(),
            "42"
        );
    }

    #[test]
    fn test_cardinality_display_bounded() {
        let sym = Symbolic::N;
        assert_eq!(Cardinality::Bounded(sym).to_string(), "n");
    }

    #[test]
    fn test_cardinality_from_str_unknown() {
        assert_eq!(Cardinality::from_str("?").unwrap(), Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_from_str_exact() {
        assert_eq!(
            Cardinality::from_str("42").unwrap(),
            Cardinality::Exact(BigUint::from(42u64))
        );
    }

    #[test]
    fn test_cardinality_from_str_bounded() {
        assert_eq!(
            Cardinality::from_str("n").unwrap(),
            Cardinality::Bounded(Symbolic::N)
        );
    }

    #[test]
    fn test_cardinality_ordering() {
        assert!(
            Cardinality::Exact(BigUint::from(5u64)) < Cardinality::Bounded(Symbolic::N)
        );
        assert!(Cardinality::Bounded(Symbolic::N) < Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_bounded_ordering() {
        // Two bounded cardinalities: ordering is equal (both ordinal 1)
        assert!(
            !(Cardinality::Bounded(Symbolic::N) < Cardinality::Bounded(Symbolic::Unknown))
        );
    }

    #[test]
    fn test_cardinality_symbolic_conversion() {
        assert_eq!(
            Cardinality::Exact(BigUint::from(42u64)).to_symbolic(),
            Symbolic::Constant(BigUint::from(42u64))
        );
        assert_eq!(Cardinality::Unknown.to_symbolic(), Symbolic::Unknown);
        assert_eq!(
            Cardinality::Bounded(Symbolic::N).to_symbolic(),
            Symbolic::N
        );
    }

    #[test]
    fn test_cardinality_max() {
        assert_eq!(
            Cardinality::Exact(BigUint::from(5u64)).max(Cardinality::Bounded(Symbolic::N)),
            Cardinality::Bounded(Symbolic::N)
        );
        assert_eq!(
            Cardinality::Bounded(Symbolic::N).max(Cardinality::Unknown),
            Cardinality::Unknown
        );
    }

    #[test]
    fn test_complex_cardinality_ordering() {
        // Test with more complex symbolics in Bounded
        assert!(
            Cardinality::Exact(BigUint::from(0u64))
                < Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Filtered(
                    Box::new(Symbolic::Filtered(Box::new(Symbolic::N)))
                ))))
        );
        assert!(
            Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Filtered(
                Box::new(Symbolic::Filtered(Box::new(Symbolic::N)))
            )))) < Cardinality::Unknown
        );
    }

    #[test]
    fn test_cardinality_serde_roundtrip_exact() {
        let c = Cardinality::Exact(BigUint::from(42u64));
        let json = serde_json::to_string(&c).unwrap();
        assert_eq!(json, r#""42""#);
        let parsed: Cardinality = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, c);
    }

    #[test]
    fn test_cardinality_serde_roundtrip_unknown() {
        let c = Cardinality::Unknown;
        let json = serde_json::to_string(&c).unwrap();
        assert_eq!(json, r#""?""#);
        let parsed: Cardinality = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, c);
    }

    #[test]
    fn test_cardinality_serde_roundtrip_bounded() {
        let c = Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
            BigUint::from(100u64),
        ))));
        let json = serde_json::to_string(&c).unwrap();
        let parsed: Cardinality = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, c);
    }

    #[test]
    fn test_program_complexity_default() {
        let pc = ProgramComplexity::default();
        assert_eq!(pc.program_time, ComplexityClass::Unknown);
        assert_eq!(pc.program_space, ComplexityClass::Unknown);
    }

    #[test]
    fn test_analyze_program_empty() {
        let result = analyze_program(&[]);
        assert_eq!(result.program_time, ComplexityClass::O1);
        assert_eq!(result.program_space, ComplexityClass::O1);
        assert_eq!(result.program_cardinality, Cardinality::Exact(BigUint::zero()));
    }

    proptest! {
        #[test]
        fn test_complexity_class_roundtrip(k in 2u64..=20) {
            let variants = vec![
                ComplexityClass::OPolynomial(k),
                ComplexityClass::OCombinatorial(k),
                ComplexityClass::OPermutational(k),
                ComplexityClass::ONLogK(k),
            ];
            for variant in variants {
                let s = variant.to_string();
                let parsed = ComplexityClass::from_str(&s).unwrap();
                prop_assert_eq!(variant, parsed);
            }
        }

        #[test]
        fn test_symbolic_roundtrip_proptest(n in 0u64..=1_000_000) {
            let sym = Symbolic::Constant(BigUint::from(n));
            let s = sym.to_string();
            let parsed = Symbolic::from_str(&s).unwrap();
            prop_assert_eq!(sym, parsed);
        }

        #[test]
        fn test_symbolic_from_str_proptest(s in "[1-9][0-9]{0,9}") {
            let sym = Symbolic::from_str(&s).unwrap();
            let s2 = sym.to_string();
            let parsed = Symbolic::from_str(&s2).unwrap();
            prop_assert_eq!(sym, parsed);
        }
    }
}
