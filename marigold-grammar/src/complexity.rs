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
            ComplexityClass::OFactorial => 3_000_000,
            ComplexityClass::Unknown => u64::MAX,
        }
    }

    pub fn max(self, other: ComplexityClass) -> ComplexityClass {
        if self >= other {
            self
        } else {
            other
        }
    }
}

impl_ord_via_ordinal!(ComplexityClass);

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
        ComplexityNotationParser::parse(Rule::simple_complexity, s)
            .map_err(|e| format!("Invalid complexity notation: {e}"))?;

        if let Some(rest) = s.strip_prefix("O(n!/(n-") {
            let k: u64 = rest
                .strip_suffix(")!)") // note: the suffix is ")!)", not just ")"
                .and_then(|r| r.parse().ok())
                .ok_or_else(|| format!("Invalid OPermutational notation: {s}"))?;
            return Ok(ComplexityClass::OPermutational(k));
        }

        if let Some(rest) = s.strip_prefix("O(C(n,") {
            let k: u64 = rest
                .strip_suffix("))") // note: strip "))", not just ")"
                .and_then(|r| r.parse().ok())
                .ok_or_else(|| format!("Invalid OCombinatorial notation: {s}"))?;
            return Ok(ComplexityClass::OCombinatorial(k));
        }

        if let Some(rest) = s.strip_prefix("O(n*log(") {
            let inner = rest
                .strip_suffix("))") // strip trailing "))"
                .ok_or_else(|| format!("Invalid ONLogK or ONLogN notation: {s}"))?;
            if inner == "n" {
                return Ok(ComplexityClass::ONLogN);
            }
            let k: u64 = inner
                .parse()
                .map_err(|_| format!("Invalid ONLogK value: {inner}"))?;
            return Ok(ComplexityClass::ONLogK(k));
        }

        if let Some(rest) = s.strip_prefix("O(n^") {
            let k: u64 = rest
                .strip_suffix(')')
                .and_then(|r| r.parse().ok())
                .ok_or_else(|| format!("Invalid OPolynomial notation: {s}"))?;
            return Ok(ComplexityClass::OPolynomial(k));
        }

        match s {
            "O(1)" => Ok(ComplexityClass::O1),
            "O(log(n))" => Ok(ComplexityClass::OLogN),
            "O(n)" => Ok(ComplexityClass::ON),
            "O(n!)" => Ok(ComplexityClass::OFactorial),
            "O(?)" => Ok(ComplexityClass::Unknown),
            _ => Err(format!("Unknown complexity class: {s}")),
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
        Self::from_str(&s).map_err(serde::de::Error::custom)
    }
}

/// A symbolic expression tree representing a stream\'s element count.
///
/// `Symbolic` values arise from static analysis of the program and are not
/// necessarily computable at analysis time (hence the `Unknown` variant and
/// the `try_evaluate` / `upper_bound` helpers).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbolic {
    Constant(BigUint),
    Unknown,
    Filtered(Box<Symbolic>),
    Permutations {
        n: Box<Symbolic>,
        k: u64,
    },
    PermutationsWithReplacement {
        n: Box<Symbolic>,
        k: u64,
    },
    Combinations {
        n: Box<Symbolic>,
        k: u64,
    },
    Min(Box<Symbolic>, Box<Symbolic>),
    Sum(Vec<Symbolic>),
}

impl Symbolic {
    /// Returns the exact value of this symbolic expression, or `None` if it
    /// depends on a runtime-unknown quantity (i.e., contains `Unknown`).
    pub fn try_evaluate(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(n) => Some(n.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.try_evaluate(),
            Symbolic::Permutations { n, k } => {
                let n_val = n.try_evaluate()?;
                Some(permutations(&n_val, *k))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.try_evaluate()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.try_evaluate()?;
                Some(combinations(&n_val, *k))
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

    /// Returns an upper bound on the value of this symbolic expression, or
    /// `None` if no upper bound can be determined statically.
    pub fn upper_bound(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(n) => Some(n.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.upper_bound(),
            Symbolic::Permutations { n, k } => {
                let n_val = n.upper_bound()?;
                Some(permutations(&n_val, *k))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.upper_bound()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.upper_bound()?;
                Some(combinations(&n_val, *k))
            }
            Symbolic::Min(a, b) => {
                let a_val = a.upper_bound()?;
                let b_val = b.upper_bound()?;
                Some(a_val.min(b_val))
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for part in parts {
                    total += part.upper_bound()?;
                }
                Some(total)
            }
        }
    }

    /// Classify this symbolic cardinality as a `ComplexityClass`.
    pub fn classify_as_cardinality(&self) -> Cardinality {
        match self.try_evaluate() {
            Some(v) => Cardinality::Exact(v),
            None => match self {
                Symbolic::Unknown => Cardinality::Unknown,
                other => Cardinality::Bounded(other.clone()),
            },
        }
    }

    pub fn classify_as_time(&self) -> ComplexityClass {
        classify_as_time(self)
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::Constant(n) => write!(f, "{n}"),
            Symbolic::Unknown => write!(f, "?"),
            Symbolic::Filtered(inner) => write!(f, "\u{2264}{inner}"),
            Symbolic::Permutations { n, k } => write!(f, "P({n}, {k})"),
            Symbolic::PermutationsWithReplacement { n, k } => write!(f, "{n}^{k}"),
            Symbolic::Combinations { n, k } => write!(f, "C({n}, {k})"),
            Symbolic::Min(a, b) => write!(f, "min({a}, {b})"),
            Symbolic::Sum(parts) => {
                let mut iter = parts.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{first}")?;
                    for part in iter {
                        write!(f, " + {part}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl FromStr for Symbolic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pairs = ComplexityNotationParser::parse(Rule::symbolic, s)
            .map_err(|e| format!("Parse error: {e}"))?;
        let pair = pairs.next().ok_or_else(|| "Empty parse result".to_string())?;
        parse_symbolic(pair)
    }
}

fn parse_symbolic(pair: pest::iterators::Pair<Rule>) -> Result<Symbolic, String> {
    match pair.as_rule() {
        Rule::symbolic => {
            let inner = pair.into_inner().next().ok_or("Empty symbolic".to_string())?;
            parse_symbolic(inner)
        }
        Rule::sum_expr => {
            let parts: Result<Vec<_>, _> = pair.into_inner().map(parse_symbolic).collect();
            let parts = parts?;
            if parts.len() == 1 {
                Ok(parts.into_iter().next().unwrap())
            } else {
                Ok(Symbolic::Sum(parts))
            }
        }
        Rule::atom => {
            let inner = pair.into_inner().next().ok_or("Empty atom".to_string())?;
            parse_symbolic(inner)
        }
        Rule::perm_rep_expr => {
            let mut parts = pair.into_inner();
            let n = parse_symbolic(parts.next().ok_or("Missing n".to_string())?)?;
            let k: u64 = parts
                .next()
                .ok_or("Missing k".to_string())?
                .as_str()
                .parse()
                .map_err(|_| "Invalid k".to_string())?;
            Ok(Symbolic::PermutationsWithReplacement {
                n: Box::new(n),
                k,
            })
        }
        Rule::filtered_expr => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or("Empty filtered".to_string())?;
            Ok(Symbolic::Filtered(Box::new(parse_symbolic(inner)?)))
        }
        Rule::perm_expr => {
            let mut parts = pair.into_inner();
            let n = parse_symbolic(parts.next().ok_or("Missing n".to_string())?)?;
            let k: u64 = parts
                .next()
                .ok_or("Missing k".to_string())?
                .as_str()
                .parse()
                .map_err(|_| "Invalid k".to_string())?;
            Ok(Symbolic::Permutations {
                n: Box::new(n),
                k,
            })
        }
        Rule::comb_expr => {
            let mut parts = pair.into_inner();
            let n = parse_symbolic(parts.next().ok_or("Missing n".to_string())?)?;
            let k: u64 = parts
                .next()
                .ok_or("Missing k".to_string())?
                .as_str()
                .parse()
                .map_err(|_| "Invalid k".to_string())?;
            Ok(Symbolic::Combinations {
                n: Box::new(n),
                k,
            })
        }
        Rule::min_expr => {
            let mut parts = pair.into_inner();
            let a = parse_symbolic(parts.next().ok_or("Missing a".to_string())?)?;
            let b = parse_symbolic(parts.next().ok_or("Missing b".to_string())?)?;
            Ok(Symbolic::Min(Box::new(a), Box::new(b)))
        }
        Rule::constant => {
            let n: BigUint = pair
                .as_str()
                .parse()
                .map_err(|_| format!("Invalid constant: {}", pair.as_str()))?;
            Ok(Symbolic::Constant(n))
        }
        Rule::unknown => Ok(Symbolic::Unknown),
        rule => Err(format!("Unexpected rule: {rule:?}")),
    }
}

/// The cardinality of a stream — how many elements it is expected to produce.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    /// Exactly this many elements.
    Exact(BigUint),
    /// At most this many elements (e.g., after a filter).
    Bounded(Symbolic),
    /// The element count is not statically known.
    Unknown,
}

impl_ord_via_ordinal!(Cardinality);

impl Cardinality {
    fn ordinal(&self) -> u64 {
        match self {
            Cardinality::Exact(_) => 0,
            Cardinality::Bounded(_) => 1,
            Cardinality::Unknown => 2,
        }
    }

    /// Returns the more conservative (larger ordinal) cardinality of the two.
    pub fn max(self, other: Cardinality) -> Cardinality {
        if self >= other {
            self
        } else {
            other
        }
    }
}

impl fmt::Display for Cardinality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cardinality::Exact(n) => write!(f, "{n}"),
            Cardinality::Bounded(s) => write!(f, "{s}"),
            Cardinality::Unknown => write!(f, "?"),
        }
    }
}

impl FromStr for Cardinality {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try to parse as a plain integer first (Exact variant)
        if let Ok(n) = s.parse::<BigUint>() {
            return Ok(Cardinality::Exact(n));
        }

        // Unknown
        if s == "?" {
            return Ok(Cardinality::Unknown);
        }

        // Try to parse as a Symbolic expression (Bounded variant)
        // Note: Exact BigUint values that don\'t roundtrip through Symbolic are handled above
        match Symbolic::from_str(s) {
            Ok(sym) => match sym {
                // A bare constant parses as Symbolic::Constant, but Display for Cardinality::Exact
                // emits just the number, so roundtrip goes Exact -> number string -> Exact here.
                // This branch handles the Bounded case.
                Symbolic::Unknown => Ok(Cardinality::Unknown),
                other => Ok(Cardinality::Bounded(other)),
            },
            Err(e) => Err(format!("Invalid cardinality: {s}: {e}")),
        }
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
        Self::from_str(&s).map_err(serde::de::Error::custom)
    }
}

/// Aggregated complexity of a stream or program.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StreamComplexity {
    pub time: ComplexityClass,
    pub space: ComplexityClass,
    pub cardinality: Cardinality,
    pub exact_time: ExactComplexity,
    pub exact_space: ExactComplexity,
}

/// The complexity of a complete program (all streams combined).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub time: ComplexityClass,
    pub space: ComplexityClass,
    pub cardinality: Cardinality,
}

/// A mapping from complexity class to coefficient.
///
/// E.g., `{ ON: 2, O1: 5 }` represents `2n + 5` operations.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExactComplexity {
    pub terms: BTreeMap<ComplexityClass, u64>,
}

impl ExactComplexity {
    pub fn zero() -> Self {
        Self {
            terms: BTreeMap::new(),
        }
    }

    pub fn constant(k: u64) -> Self {
        let mut terms = BTreeMap::new();
        terms.insert(ComplexityClass::O1, k);
        Self { terms }
    }

    pub fn of_class(class: ComplexityClass) -> Self {
        let mut terms = BTreeMap::new();
        terms.insert(class, 1);
        Self { terms }
    }

    pub fn add(&mut self, class: ComplexityClass, count: u64) {
        *self.terms.entry(class).or_insert(0) += count;
    }

    pub fn merge(&mut self, other: &ExactComplexity) {
        for (class, count) in &other.terms {
            self.add(class.clone(), *count);
        }
    }

    pub fn simplified(&self) -> ComplexityClass {
        self.terms
            .keys()
            .cloned()
            .max()
            .unwrap_or(ComplexityClass::O1)
    }
}

fn permutations(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    for i in 0..k {
        let factor = n - BigUint::from(i);
        result *= factor;
    }
    result
}

fn combinations(n: &BigUint, k: u64) -> BigUint {
    permutations(n, k) / factorial(k)
}

fn factorial(n: u64) -> BigUint {
    (1..=n).fold(BigUint::one(), |acc, i| acc * BigUint::from(i))
}

fn classify_as_time(sym: &Symbolic) -> ComplexityClass {
    match sym {
        Symbolic::Constant(_) => ComplexityClass::O1,
        Symbolic::Unknown => ComplexityClass::Unknown,
        Symbolic::Filtered(inner) => classify_as_time(inner),
        Symbolic::Permutations { k, .. } => ComplexityClass::OPermutational(*k),
        Symbolic::PermutationsWithReplacement { k, .. } => ComplexityClass::OPolynomial(*k),
        Symbolic::Combinations { k, .. } => ComplexityClass::OCombinatorial(*k),
        Symbolic::Min(a, b) => classify_as_time(a).max(classify_as_time(b)),
        Symbolic::Sum(parts) => parts
            .iter()
            .map(classify_as_time)
            .max()
            .unwrap_or(ComplexityClass::O1),
    }
}

fn propagate_cardinality(cardinality: Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Map | StreamFunctionKind::OkOrPanic => cardinality,
        StreamFunctionKind::Filter
        | StreamFunctionKind::Ok
        | StreamFunctionKind::FilterMap
        | StreamFunctionKind::FlatMap => Symbolic::Filtered(Box::new(cardinality)),
        StreamFunctionKind::Flatten => Symbolic::Unknown,
    }
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match (&inp.variability, &inp.input_count) {
        (InputVariability::Constant, InputCount::Known(n)) => Symbolic::Constant(n.clone()),
        _ => Symbolic::Unknown,
    }
}

fn analyze_stream_fns(
    funs: &[StreamFunctionKind],
    mut cardinality: Symbolic,
    desc: &crate::nodes::OutputFunctionNode,
) -> StreamComplexity {
    let mut exact_time = ExactComplexity::zero();
    let mut exact_space = ExactComplexity::zero();

    for fun in funs {
        let time_class = classify_as_time(&cardinality);
        exact_time.add(time_class.clone(), 1);
        exact_space.add(time_class, 1);
        cardinality = propagate_cardinality(cardinality, fun);
    }

    // Output step
    {
        let time_class = classify_as_time(&cardinality);
        exact_time.add(time_class.clone(), 1);
        exact_space.add(time_class, 1);
    }

    let time_class = exact_time.simplified();
    let space_class = exact_space.simplified();
    let cardinality_class = cardinality.classify_as_cardinality();

    StreamComplexity {
        time: time_class,
        space: space_class,
        cardinality: cardinality_class,
        exact_time,
        exact_space,
    }
}

/// Compute the complexity of a complete Marigold program.
pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    let mut program_time = ComplexityClass::O1;
    let mut program_space = ComplexityClass::O1;
    let mut program_cardinality = Cardinality::Exact(BigUint::zero());

    // Track the exact time and space for stream variables
    let mut var_exact_time = ExactComplexity::zero();
    let mut var_exact_space = ExactComplexity::zero();
    let mut var_space = ComplexityClass::O1;

    for expr in expressions {
        match expr {
            TypedExpression::UnnamedReturningStream(s) => {
                let card = input_cardinality(&s.inp_and_funs.inp);
                let sc = analyze_stream_fns(&s.inp_and_funs.funs, card, &s.out);
                program_time = program_time.max(sc.time);
                program_space = program_space.max(sc.space);
                program_cardinality = program_cardinality.max(sc.cardinality);
            }
            TypedExpression::NamedReturningStream(s) => {
                let card = input_cardinality(&s.inp_and_funs.inp);
                let mut sc = analyze_stream_fns(&s.inp_and_funs.funs, card, &s.out);
                sc.exact_time.merge(&var_exact_time);
                sc.time = sc.exact_time.simplified();
                sc.exact_space.merge(&var_exact_space);
                sc.space = sc.exact_space.simplified().max(var_space.clone());
                program_time = program_time.max(sc.time);
                program_space = program_space.max(sc.space);
                program_cardinality = program_cardinality.max(sc.cardinality);
            }
            TypedExpression::NamedNonReturningStream(s) => {
                let card = input_cardinality(&s.inp_and_funs.inp);
                let sc = analyze_stream_fns(&s.inp_and_funs.funs, card, &s.out);
                var_exact_time.merge(&sc.exact_time);
                var_exact_space.merge(&sc.exact_space);
                var_space = var_space.max(sc.space);
            }
            _ => continue,
        }
    }

    ProgramComplexity {
        time: program_time,
        space: program_space,
        cardinality: program_cardinality,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_map_preserves_cardinality() {
        let input_card = Symbolic::Constant(BigUint::from(10u64));
        let result = propagate_cardinality(input_card.clone(), &StreamFunctionKind::Map);
        assert_eq!(result, input_card, "Map should preserve cardinality exactly");
    }

    #[test]
    fn test_okpanic_preserves_cardinality() {
        let input_card = Symbolic::Constant(BigUint::from(10u64));
        let result = propagate_cardinality(input_card.clone(), &StreamFunctionKind::OkOrPanic);
        assert_eq!(
            result, input_card,
            "OkOrPanic should preserve cardinality exactly"
        );
    }

    #[test]
    fn test_filter_reduces_cardinality() {
        let input_card = Symbolic::Constant(BigUint::from(10u64));
        let result = propagate_cardinality(input_card.clone(), &StreamFunctionKind::Filter);
        assert_eq!(
            result,
            Symbolic::Filtered(Box::new(input_card)),
            "Filter should wrap cardinality in Filtered"
        );
    }

    #[test]
    fn test_analyze_simple_returning_stream() {
        use crate::nodes::*;

        let stream = TypedExpression::UnnamedReturningStream(UnnamedStreamNode {
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: InputFunctionNode {
                    variability: InputVariability::Constant,
                    input_count: InputCount::Known(BigUint::from(10u64)),
                    code: "range(0, 10)".to_string(),
                },
                funs: vec![],
            },
            out: OutputFunctionNode {
                stream_prefix: String::new(),
                stream_postfix: ".return".to_string(),
                returning: true,
            },
        });

        let result = analyze_program(&[stream]);
        assert_eq!(result.time, ComplexityClass::O1);
        assert_eq!(result.space, ComplexityClass::O1);
        assert_eq!(result.cardinality, Cardinality::Exact(BigUint::from(10u64)));
    }

    #[test]
    fn test_analyze_stream_variable() {
        use crate::nodes::*;

        let var_stream = TypedExpression::NamedNonReturningStream(NamedStreamNode {
            name: "digits".to_string(),
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: InputFunctionNode {
                    variability: InputVariability::Constant,
                    input_count: InputCount::Known(BigUint::from(10u64)),
                    code: "range(0, 10)".to_string(),
                },
                funs: vec![],
            },
            out: OutputFunctionNode {
                stream_prefix: String::new(),
                stream_postfix: String::new(),
                returning: false,
            },
        });

        let returning_stream = TypedExpression::NamedReturningStream(NamedStreamNode {
            name: "doubled".to_string(),
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: InputFunctionNode {
                    variability: InputVariability::Constant,
                    input_count: InputCount::Known(BigUint::from(5u64)),
                    code: "range(0, 5)".to_string(),
                },
                funs: vec![StreamFunctionKind::Map],
            },
            out: OutputFunctionNode {
                stream_prefix: String::new(),
                stream_postfix: ".return".to_string(),
                returning: true,
            },
        });

        let result = analyze_program(&[var_stream, returning_stream]);
        assert_eq!(result.time, ComplexityClass::O1);
        assert_eq!(result.space, ComplexityClass::O1);
    }

    #[test]
    fn test_cardinality_max() {
        assert!(
            Cardinality::Unknown > Cardinality::Bounded(Symbolic::Constant(BigUint::from(5u64)))
        );
        assert!(Cardinality::Bounded(Symbolic::Constant(BigUint::from(5u64))) > Cardinality::Exact(BigUint::from(5u64)));
        assert!(Cardinality::Unknown > Cardinality::Exact(BigUint::from(5u64)));
    }

    #[test]
    fn test_complexity_class_order() {
        assert!(ComplexityClass::ON > ComplexityClass::O1);
        assert!(ComplexityClass::ONLogN > ComplexityClass::ON);
        assert!(ComplexityClass::OFactorial > ComplexityClass::ONLogN);
        assert!(ComplexityClass::Unknown > ComplexityClass::OFactorial);
    }

    #[test]
    fn test_cardinality_display_fromstr() {
        let cases = [
            ("42", Cardinality::Exact(BigUint::from(42u64))),
            ("?", Cardinality::Unknown),
        ];
        for (s, expected) in &cases {
            let parsed = Cardinality::from_str(s).unwrap();
            assert_eq!(&parsed, expected);
            assert_eq!(&expected.to_string(), s);
        }
    }

    #[test]
    fn test_exact_complexity_merge() {
        let mut a = ExactComplexity::constant(3);
        let b = ExactComplexity::of_class(ComplexityClass::ON);
        a.merge(&b);
        assert_eq!(*a.terms.get(&ComplexityClass::O1).unwrap(), 3);
        assert_eq!(*a.terms.get(&ComplexityClass::ON).unwrap(), 1);
        assert_eq!(a.simplified(), ComplexityClass::ON);
    }

    #[test]
    fn test_try_evaluate_permutations() {
        let sym = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(20u64)));
    }

    #[test]
    fn test_try_evaluate_combinations() {
        let sym = Symbolic::Combinations {
            n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(10u64)));
    }

    #[test]
    fn test_try_evaluate_perm_with_replacement() {
        let sym = Symbolic::PermutationsWithReplacement {
            n: Box::new(Symbolic::Constant(BigUint::from(3u64))),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(9u64)));
    }

    #[test]
    fn test_try_evaluate_unknown_returns_none() {
        assert_eq!(Symbolic::Unknown.try_evaluate(), None);
        let sym = Symbolic::Permutations {
            n: Box::new(Symbolic::Unknown),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), None);
    }

    #[test]
    fn test_complexity_class_display_fromstr() {
        let cases = [
            "O(1)",
            "O(log(n))",
            "O(n)",
            "O(n*log(2))",
            "O(n*log(n))",
            "O(n^3)",
            "O(C(n,4))",
            "O(n!/(n-5)!)",
            "O(n!)",
            "O(?)",
        ];
        for s in &cases {
            let parsed = ComplexityClass::from_str(s).expect(s);
            assert_eq!(&parsed.to_string(), s);
        }
    }

    #[test]
    fn test_symbolic_display_fromstr() {
        let cases = [
            "42",
            "?",
            "\u{2264}42",
            "P(5, 2)",
            "5^2",
            "C(5, 2)",
            "min(3, 5)",
            "3 + 5",
        ];
        for s in &cases {
            let parsed = Symbolic::from_str(s).expect(s);
            assert_eq!(&parsed.to_string(), s, "roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_symbolic_sum_display_fromstr_roundtrip() {
        let sym = Symbolic::Sum(vec![
            Symbolic::Constant(BigUint::from(3u64)),
            Symbolic::Constant(BigUint::from(5u64)),
            Symbolic::Constant(BigUint::from(7u64)),
        ]);
        let s = sym.to_string();
        let parsed = Symbolic::from_str(&s).unwrap();
        assert_eq!(sym, parsed);
    }

    #[test]
    fn test_symbolic_filtered_display_fromstr_roundtrip() {
        let sym = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(42u64))));
        let s = sym.to_string();
        let parsed = Symbolic::from_str(&s).unwrap();
        assert_eq!(sym, parsed);
    }

    #[test]
    fn test_cardinality_bounded_display_fromstr() {
        let sym = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));
        let c = Cardinality::Bounded(sym);
        let s = c.to_string();
        let parsed = Cardinality::from_str(&s).unwrap();
        assert_eq!(c, parsed);
    }

    #[test]
    fn test_exact_complexity_zero() {
        let ec = ExactComplexity::zero();
        assert!(ec.terms.is_empty());
        assert_eq!(ec.simplified(), ComplexityClass::O1);
    }

    #[test]
    fn test_exact_complexity_add() {
        let mut ec = ExactComplexity::zero();
        ec.add(ComplexityClass::ON, 3);
        ec.add(ComplexityClass::ON, 2);
        assert_eq!(*ec.terms.get(&ComplexityClass::ON).unwrap(), 5);
    }

    #[test]
    fn test_analyze_named_returning_stream_no_var() {
        use crate::nodes::*;

        let stream = TypedExpression::NamedReturningStream(NamedStreamNode {
            name: "result".to_string(),
            inp_and_funs: InputAndMaybeStreamFunctions {
                inp: InputFunctionNode {
                    variability: InputVariability::Constant,
                    input_count: InputCount::Known(BigUint::from(10u64)),
                    code: "range(0, 10)".to_string(),
                },
                funs: vec![],
            },
            out: OutputFunctionNode {
                stream_prefix: String::new(),
                stream_postfix: ".return".to_string(),
                returning: true,
            },
        });

        let result = analyze_program(&[stream]);
        assert_eq!(result.time, ComplexityClass::O1);
    }

    #[test]
    fn test_flatten_produces_unknown_cardinality() {
        let card = Symbolic::Constant(BigUint::from(5u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::Flatten);
        assert_eq!(result, Symbolic::Unknown);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn arb_exact_complexity() -> impl Strategy<Value = ExactComplexity> {
        let classes = vec![
            ComplexityClass::O1,
            ComplexityClass::ON,
            ComplexityClass::ONLogN,
            ComplexityClass::OFactorial,
        ];
        proptest::collection::vec(
            (0..classes.len(), 1u64..100),
            1..5,
        )
        .prop_map(move |pairs| {
            let mut terms = BTreeMap::new();
            for (idx, count) in pairs {
                let class = classes[idx].clone();
                *terms.entry(class).or_insert(0) += count;
            }
            ExactComplexity { terms }
        })
    }

    fn arb_complexity_class() -> impl Strategy<Value = ComplexityClass> {
        prop_oneof![
            Just(ComplexityClass::O1),
            Just(ComplexityClass::OLogN),
            Just(ComplexityClass::ON),
            (1u64..5).prop_map(ComplexityClass::ONLogK),
            Just(ComplexityClass::ONLogN),
            (2u64..5).prop_map(ComplexityClass::OPolynomial),
            (2u64..5).prop_map(ComplexityClass::OCombinatorial),
            (2u64..5).prop_map(ComplexityClass::OPermutational),
            Just(ComplexityClass::OFactorial),
            Just(ComplexityClass::Unknown),
        ]
    }

    proptest! {
        #[test]
        fn test_complexity_class_display_fromstr_roundtrip(c in arb_complexity_class()) {
            let s = c.to_string();
            let parsed = ComplexityClass::from_str(&s).unwrap();
            prop_assert_eq!(c, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_complexity_class_max_commutativity(a in arb_complexity_class(), b in arb_complexity_class()) {
            let ab = a.clone().max(b.clone());
            let ba = b.max(a);
            prop_assert_eq!(ab, ba);
        }
    }

    proptest! {
        #[test]
        fn test_exact_complexity_simplified_is_max_term(ec in arb_exact_complexity()) {
            let simplified = ec.simplified();
            for class in ec.terms.keys() {
                prop_assert!(simplified >= *class);
            }
        }
    }

    fn arb_symbolic_constant() -> impl Strategy<Value = Symbolic> {
        (1u64..10000).prop_map(|n| Symbolic::Constant(BigUint::from(n)))
    }

    /// Weighted leaf: 90% Constant, 10% Unknown.
    fn arb_symbolic_leaf() -> impl Strategy<Value = Symbolic> {
        prop_oneof![
            9 => arb_symbolic_constant(),
            1 => Just(Symbolic::Unknown),
        ]
    }

    fn arb_symbolic() -> impl Strategy<Value = Symbolic> {
        prop_oneof![
            arb_symbolic_leaf(),
            Just(Symbolic::Unknown),
            arb_symbolic_leaf().prop_map(|s| Symbolic::Filtered(Box::new(s))),
            (arb_symbolic_leaf(), 1u64..5)
                .prop_map(|(s, k)| Symbolic::Permutations { n: Box::new(s), k }),
            (arb_symbolic_leaf(), 1u64..5)
                .prop_map(|(s, k)| Symbolic::Combinations { n: Box::new(s), k }),
            // PermutationsWithReplacement display format (`n^k`) requires a constant
            // for the base, since the grammar rule `symbolic_perm_rep` only accepts
            // `symbolic_constant ~ "^" ~ number`.
            (arb_symbolic_constant(), 1u64..4)
                .prop_map(|(s, k)| Symbolic::PermutationsWithReplacement { n: Box::new(s), k }),
            (arb_symbolic_leaf(), arb_symbolic_leaf())
                .prop_map(|(a, b)| Symbolic::Min(Box::new(a), Box::new(b))),
            proptest::collection::vec(arb_symbolic_leaf(), 2..5).prop_map(Symbolic::Sum),
        ]
    }

    fn arb_cardinality() -> impl Strategy<Value = Cardinality> {
        prop_oneof![
            (1u64..10000).prop_map(|n| Cardinality::Exact(BigUint::from(n))),
            arb_symbolic_constant()
                .prop_map(|s| Cardinality::Bounded(Symbolic::Filtered(Box::new(s)))),
            Just(Cardinality::Unknown),
        ]
    }

    proptest! {
        #[test]
        fn test_cardinality_display_fromstr_roundtrip(c in arb_cardinality()) {
            let s = c.to_string();
            let parsed = Cardinality::from_str(&s).unwrap();
            prop_assert_eq!(c, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_cardinality_serde_roundtrip_proptest(c in arb_cardinality()) {
            let json = serde_json::to_string(&c).unwrap();
            let parsed: Cardinality = serde_json::from_str(&json).unwrap();
            prop_assert_eq!(c, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_cardinality_max_commutativity_exact(a_val in 1u64..10000, b_val in 1u64..10000) {
            let a = Cardinality::Exact(BigUint::from(a_val));
            let b = Cardinality::Exact(BigUint::from(b_val));
            let ab = a.clone().max(b.clone());
            let ba = b.max(a);
            prop_assert_eq!(ab, ba);
        }
    }

    proptest! {
        #[test]
        fn test_cardinality_ordering_is_total(a in arb_cardinality(), b in arb_cardinality()) {
            prop_assert!(a.partial_cmp(&b).is_some());
        }
    }

    proptest! {
        #[test]
        fn test_symbolic_upper_bound_ge_try_evaluate(sym in arb_symbolic()) {
            if let (Some(exact), Some(upper)) = (sym.try_evaluate(), sym.upper_bound()) {
                prop_assert!(upper >= exact, "upper_bound ({upper}) must be >= try_evaluate ({exact})");
            }
        }
    }

    proptest! {
        #[test]
        fn test_symbolic_display_fromstr_roundtrip(sym in arb_symbolic()) {
            let s = sym.to_string();
            let parsed = Symbolic::from_str(&s).unwrap();
            prop_assert_eq!(sym, parsed);
        }
    }

    #[test]
    fn test_arb_symbolic_unknown_frequency() {
        use proptest::strategy::ValueTree;
        use proptest::test_runner::TestRunner;

        let mut runner = TestRunner::default();
        let strategy = arb_symbolic_leaf();
        let total = 10_000;
        let mut unknown_count = 0u64;
        for _ in 0..total {
            let value = strategy.new_tree(&mut runner).unwrap().current();
            if value == Symbolic::Unknown {
                unknown_count += 1;
            }
        }
        // arb_symbolic_leaf uses a 9:1 weight (Constant vs Unknown), so ~10% of samples
        // should be Unknown. Assert at least 5% as a conservative floor.
        assert!(
            unknown_count * 100 >= total * 5,
            "Unknown appeared {unknown_count}/{total} times, expected >= 5%"
        );
    }

    proptest! {
        #[test]
        fn test_sum_of_constants_try_evaluate(
            parts in proptest::collection::vec(arb_symbolic_constant(), 2..5)
        ) {
            let expected: BigUint = parts.iter().map(|p| p.try_evaluate().unwrap()).sum();
            let sum = Symbolic::Sum(parts);
            prop_assert_eq!(sum.try_evaluate(), Some(expected));
        }
    }

    proptest! {
        #[test]
        fn test_symbolic_with_unknown_does_not_panic_on_upper_bound(sym in arb_symbolic()) {
            let _ = sym.upper_bound();
            let _ = sym.try_evaluate();
        }
    }

    #[test]
    fn test_analyze_does_not_panic_with_declarations() {
        use crate::nodes::*;

        fn make_struct_decl(n: usize) -> TypedExpression {
            let fields: Vec<(String, Type)> =
                (0..n).map(|i| (format!("field_{i}"), Type::I32)).collect();
            TypedExpression::StructDeclaration(StructDeclarationNode {
                name: "TestStruct".to_string(),
                fields,
            })
        }

        fn make_enum_decl(n: usize) -> TypedExpression {
            let variants: Vec<(String, Option<String>)> =
                (0..n).map(|i| (format!("Variant{i}"), None)).collect();
            TypedExpression::EnumDeclaration(EnumDeclarationNode {
                name: "TestEnum".to_string(),
                variants,
                default_variant: None,
            })
        }

        fn make_fn_decl() -> TypedExpression {
            TypedExpression::FnDeclaration(FnDeclarationNode {
                name: "test_fn".to_string(),
                parameters: vec![("x".to_string(), "i32".to_string())],
                output_type: "i32".to_string(),
                body: "x".to_string(),
            })
        }

        fn make_stream() -> TypedExpression {
            TypedExpression::UnnamedReturningStream(UnnamedStreamNode {
                inp_and_funs: InputAndMaybeStreamFunctions {
                    inp: InputFunctionNode {
                        variability: InputVariability::Constant,
                        input_count: InputCount::Known(BigUint::from(10u64)),
                        code: "range(0, 10)".to_string(),
                    },
                    funs: vec![],
                },
                out: OutputFunctionNode {
                    stream_prefix: String::new(),
                    stream_postfix: ".return".to_string(),
                    returning: true,
                },
            })
        }

        // Iterate over 512 combinations of declaration presence and field/variant counts.
        // Bits 0-4 (5 bits = 32 structural patterns) gate which declaration types appear;
        // bits 5-8 vary field/variant counts within each structural pattern, yielding
        // 512 total iterations that exercise declarations before, after, and around streams.
        for i in 0..512u32 {
            let mut expressions: Vec<TypedExpression> = Vec::new();

            // Add declarations based on bit pattern of i
            if i & 1 != 0 {
                expressions.push(make_struct_decl(((i >> 1) % 3 + 1) as usize));
            }
            if i & 2 != 0 {
                expressions.push(make_enum_decl(((i >> 2) % 3 + 1) as usize));
            }
            if i & 4 != 0 {
                expressions.push(make_fn_decl());
            }
            // Always include at least one stream expression
            expressions.push(make_stream());
            // Sometimes add declarations after stream
            if i & 8 != 0 {
                expressions.push(make_struct_decl(2));
            }
            if i & 16 != 0 {
                expressions.push(make_enum_decl(1));
                expressions.push(make_fn_decl());
            }

            // Must not panic
            let _result = analyze_program(&expressions);
        }
    }

    proptest! {
        #[test]
        fn test_sum_with_unknown_try_evaluate_is_none(
            parts in proptest::collection::vec(arb_symbolic_leaf(), 2..5)
                .prop_filter("must contain Unknown", |v| v.iter().any(|s| s == &Symbolic::Unknown))
        ) {
            prop_assert_eq!(Symbolic::Sum(parts).try_evaluate(), None);
        }
    }
}
