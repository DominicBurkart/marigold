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
            ComplexityClass::OPolynomial(k) => 5 + k,
            ComplexityClass::OCombinatorial(k) => 1_000_000 + k,
            ComplexityClass::OPermutational(k) => 2_000_000 + k,
            ComplexityClass::OFactorial => 3_000_000,
            ComplexityClass::Unknown => u64::MAX,
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
            ComplexityClass::OPermutational(k) => write!(f, "O(P(n,{k}))"),
            ComplexityClass::OFactorial => write!(f, "O(n!)"),
            ComplexityClass::Unknown => write!(f, "O(?)"),
        }
    }
}

impl FromStr for ComplexityClass {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pairs = ComplexityNotationParser::parse(Rule::complexity_class, s)
            .map_err(|e| e.to_string())?;
        let pair = pairs
            .into_iter()
            .next()
            .ok_or_else(|| "empty parse".to_string())?;
        parse_complexity_class(pair)
    }
}

fn parse_complexity_class(pair: pest::iterators::Pair<Rule>) -> Result<ComplexityClass, String> {
    match pair.as_rule() {
        Rule::complexity_class => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "empty complexity_class".to_string())?;
            parse_complexity_class(inner)
        }
        Rule::o1 => Ok(ComplexityClass::O1),
        Rule::ologn => Ok(ComplexityClass::OLogN),
        Rule::on => Ok(ComplexityClass::ON),
        Rule::onlogk => {
            let k_str = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing k in ONLogK".to_string())?
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("invalid k in ONLogK: {e}"))?;
            Ok(ComplexityClass::ONLogK(k))
        }
        Rule::onlogn => Ok(ComplexityClass::ONLogN),
        Rule::opolynomial => {
            let k_str = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing k in OPolynomial".to_string())?
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("invalid k in OPolynomial: {e}"))?;
            Ok(ComplexityClass::OPolynomial(k))
        }
        Rule::ocombinatorial => {
            let k_str = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing k in OCombinatorial".to_string())?
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("invalid k in OCombinatorial: {e}"))?;
            Ok(ComplexityClass::OCombinatorial(k))
        }
        Rule::opermutational => {
            let k_str = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing k in OPermutational".to_string())?
                .as_str();
            let k = k_str
                .parse::<u64>()
                .map_err(|e| format!("invalid k in OPermutational: {e}"))?;
            Ok(ComplexityClass::OPermutational(k))
        }
        Rule::ofactorial => Ok(ComplexityClass::OFactorial),
        Rule::ounknown => Ok(ComplexityClass::Unknown),
        r => Err(format!("unexpected rule {r:?}")),
    }
}

impl Serialize for ComplexityClass {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ComplexityClass {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        ComplexityClass::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExactComplexity {
    pub terms: BTreeMap<ComplexityClass, u64>,
}

impl ExactComplexity {
    pub fn new() -> Self {
        ExactComplexity {
            terms: BTreeMap::new(),
        }
    }

    pub fn add_work(&mut self, class: ComplexityClass, count: u64) {
        *self.terms.entry(class).or_insert(0) += count;
    }

    pub fn merge(&mut self, other: &ExactComplexity) {
        for (class, count) in &other.terms {
            *self.terms.entry(class.clone()).or_insert(0) += count;
        }
    }

    pub fn simplified(&self) -> ComplexityClass {
        self.terms
            .keys()
            .max()
            .cloned()
            .unwrap_or(ComplexityClass::O1)
    }
}

impl Default for ExactComplexity {
    fn default() -> Self {
        ExactComplexity::new()
    }
}

impl fmt::Display for ExactComplexity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts: Vec<String> = self
            .terms
            .iter()
            .map(|(class, count)| format!("{class} => {count}"))
            .collect();
        // Drop O(1) terms when there are higher-complexity terms present
        let max = self.simplified();
        if max != ComplexityClass::O1 {
            parts.retain(|p| !p.starts_with("O(1)"));
        }
        if parts.is_empty() {
            write!(f, "O(1) => 0")
        } else {
            write!(f, "{}", parts.join(" + "))
        }
    }
}

impl FromStr for ExactComplexity {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pairs = ComplexityNotationParser::parse(Rule::exact_complexity, s)
            .map_err(|e| e.to_string())?;
        let mut terms = BTreeMap::new();
        for pair in pairs {
            if pair.as_rule() == Rule::exact_complexity {
                for term in pair.into_inner() {
                    if term.as_rule() == Rule::term {
                        let mut inner = term.into_inner();
                        let class_pair = inner
                            .next()
                            .ok_or_else(|| "missing class in term".to_string())?;
                        let count_str = inner
                            .next()
                            .ok_or_else(|| "missing count in term".to_string())?
                            .as_str();
                        let class = parse_complexity_class(class_pair)?;
                        let count = count_str
                            .parse::<u64>()
                            .map_err(|e| format!("invalid count: {e}"))?;
                        *terms.entry(class).or_insert(0) += count;
                    }
                }
            }
        }
        Ok(ExactComplexity { terms })
    }
}

impl Serialize for ExactComplexity {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ExactComplexity {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        ExactComplexity::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbolic {
    Constant(BigUint),
    Unknown,
    Filtered(Box<Symbolic>),
    Permutations { n: Box<Symbolic>, k: u64 },
    Combinations { n: Box<Symbolic>, k: u64 },
    PermutationsWithReplacement { n: Box<Symbolic>, k: u64 },
    Min(Box<Symbolic>, Box<Symbolic>),
    Sum(Vec<Symbolic>),
}

impl Symbolic {
    pub fn try_evaluate(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(n) => Some(n.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(_) => None,
            Symbolic::Permutations { n, k } => {
                let n_val = n.try_evaluate()?;
                // P(n, k) = n! / (n-k)! = n * (n-1) * ... * (n-k+1)
                let k_big = BigUint::from(*k);
                if n_val < k_big {
                    return Some(BigUint::zero());
                }
                let mut result = BigUint::one();
                for i in 0..*k {
                    result *= &n_val - BigUint::from(i);
                }
                Some(result)
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.try_evaluate()?;
                let k_big = BigUint::from(*k);
                if n_val < k_big {
                    return Some(BigUint::zero());
                }
                // C(n, k) = P(n, k) / k!
                let mut perm = BigUint::one();
                for i in 0..*k {
                    perm *= &n_val - BigUint::from(i);
                }
                let mut k_fact = BigUint::one();
                for i in 1..=*k {
                    k_fact *= BigUint::from(i);
                }
                Some(perm / k_fact)
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.try_evaluate()?;
                // n^k
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Min(a, b) => {
                let a_val = a.try_evaluate()?;
                let b_val = b.try_evaluate()?;
                Some(a_val.min(b_val))
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for p in parts {
                    total += p.try_evaluate()?;
                }
                Some(total)
            }
        }
    }

    pub fn upper_bound(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(n) => Some(n.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.upper_bound(),
            Symbolic::Permutations { n, k } => {
                let n_val = n.upper_bound()?;
                let mut result = BigUint::one();
                for i in 0..*k {
                    result *= &n_val - BigUint::from(i);
                }
                Some(result)
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.upper_bound()?;
                if n_val < BigUint::from(*k) {
                    return Some(BigUint::zero());
                }
                let mut perm = BigUint::one();
                for i in 0..*k {
                    perm *= &n_val - BigUint::from(i);
                }
                let mut k_fact = BigUint::one();
                for i in 1..=*k {
                    k_fact *= BigUint::from(i);
                }
                Some(perm / k_fact)
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.upper_bound()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Min(a, b) => {
                let a_val = a.upper_bound()?;
                let b_val = b.upper_bound()?;
                Some(a_val.min(b_val))
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for p in parts {
                    total += p.upper_bound()?;
                }
                Some(total)
            }
        }
    }

    pub fn classify_as_cardinality(&self) -> Cardinality {
        match self.try_evaluate() {
            Some(n) => Cardinality::Exact(n),
            None => match self {
                Symbolic::Unknown => Cardinality::Unknown,
                _ => Cardinality::Bounded(self.clone()),
            },
        }
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::Constant(n) => write!(f, "{n}"),
            Symbolic::Unknown => write!(f, "?"),
            Symbolic::Filtered(inner) => write!(f, "filtered({inner})"),
            Symbolic::Permutations { n, k } => write!(f, "P({n},{k})"),
            Symbolic::Combinations { n, k } => write!(f, "C({n},{k})"),
            Symbolic::PermutationsWithReplacement { n, k } => write!(f, "PR({n},{k})"),
            Symbolic::Min(a, b) => write!(f, "min({a},{b})"),
            Symbolic::Sum(parts) => {
                let strs: Vec<String> = parts.iter().map(|p| p.to_string()).collect();
                write!(f, "sum({})", strs.join(","))
            }
        }
    }
}

impl FromStr for Symbolic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pairs = ComplexityNotationParser::parse(Rule::symbolic, s)
            .map_err(|e| e.to_string())?;
        let pair = pairs
            .into_iter()
            .next()
            .ok_or_else(|| "empty parse".to_string())?;
        parse_symbolic(pair)
    }
}

fn parse_symbolic(pair: pest::iterators::Pair<Rule>) -> Result<Symbolic, String> {
    match pair.as_rule() {
        Rule::symbolic => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "empty symbolic".to_string())?;
            parse_symbolic(inner)
        }
        Rule::constant => {
            let n = pair
                .as_str()
                .parse::<BigUint>()
                .map_err(|e| format!("invalid constant: {e}"))?;
            Ok(Symbolic::Constant(n))
        }
        Rule::unknown_sym => Ok(Symbolic::Unknown),
        Rule::filtered => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing filtered inner".to_string())?;
            Ok(Symbolic::Filtered(Box::new(parse_symbolic(inner)?)))
        }
        Rule::permutations => {
            let mut inner = pair.into_inner();
            let n = parse_symbolic(inner.next().ok_or_else(|| "missing n".to_string())?)?;
            let k = inner
                .next()
                .ok_or_else(|| "missing k".to_string())?
                .as_str()
                .parse::<u64>()
                .map_err(|e| format!("invalid k: {e}"))?;
            Ok(Symbolic::Permutations {
                n: Box::new(n),
                k,
            })
        }
        Rule::combinations => {
            let mut inner = pair.into_inner();
            let n = parse_symbolic(inner.next().ok_or_else(|| "missing n".to_string())?)?;
            let k = inner
                .next()
                .ok_or_else(|| "missing k".to_string())?
                .as_str()
                .parse::<u64>()
                .map_err(|e| format!("invalid k: {e}"))?;
            Ok(Symbolic::Combinations {
                n: Box::new(n),
                k,
            })
        }
        Rule::perms_with_replacement => {
            let mut inner = pair.into_inner();
            let n = parse_symbolic(inner.next().ok_or_else(|| "missing n".to_string())?)?;
            let k = inner
                .next()
                .ok_or_else(|| "missing k".to_string())?
                .as_str()
                .parse::<u64>()
                .map_err(|e| format!("invalid k: {e}"))?;
            Ok(Symbolic::PermutationsWithReplacement {
                n: Box::new(n),
                k,
            })
        }
        Rule::sym_min => {
            let mut inner = pair.into_inner();
            let a = parse_symbolic(inner.next().ok_or_else(|| "missing a".to_string())?)?;
            let b = parse_symbolic(inner.next().ok_or_else(|| "missing b".to_string())?)?;
            Ok(Symbolic::Min(Box::new(a), Box::new(b)))
        }
        Rule::sym_sum => {
            let parts: Result<Vec<Symbolic>, String> =
                pair.into_inner().map(parse_symbolic).collect();
            Ok(Symbolic::Sum(parts?))
        }
        r => Err(format!("unexpected rule {r:?} in symbolic")),
    }
}

impl Serialize for Symbolic {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Symbolic {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Symbolic::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    Exact(BigUint),
    Bounded(Symbolic),
    Unknown,
}

impl Cardinality {
    pub fn max(self, other: Cardinality) -> Cardinality {
        match (&self, &other) {
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            (Cardinality::Exact(a), Cardinality::Exact(b)) => {
                if a >= b {
                    self
                } else {
                    other
                }
            }
            _ => Cardinality::Unknown,
        }
    }

    pub fn partial_cmp(&self, other: &Cardinality) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Cardinality::Exact(a), Cardinality::Exact(b)) => Some(a.cmp(b)),
            (Cardinality::Unknown, Cardinality::Unknown) => Some(std::cmp::Ordering::Equal),
            (Cardinality::Exact(_), Cardinality::Unknown) => Some(std::cmp::Ordering::Less),
            (Cardinality::Unknown, Cardinality::Exact(_)) => Some(std::cmp::Ordering::Greater),
            _ => None,
        }
    }
}

impl fmt::Display for Cardinality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cardinality::Exact(n) => write!(f, "exact({n})"),
            Cardinality::Bounded(sym) => write!(f, "bounded({sym})"),
            Cardinality::Unknown => write!(f, "unknown"),
        }
    }
}

impl FromStr for Cardinality {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pairs = ComplexityNotationParser::parse(Rule::cardinality, s)
            .map_err(|e| e.to_string())?;
        let pair = pairs
            .into_iter()
            .next()
            .ok_or_else(|| "empty parse".to_string())?;
        parse_cardinality(pair)
    }
}

fn parse_cardinality(pair: pest::iterators::Pair<Rule>) -> Result<Cardinality, String> {
    match pair.as_rule() {
        Rule::cardinality => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "empty cardinality".to_string())?;
            parse_cardinality(inner)
        }
        Rule::cardinality_exact => {
            let n_str = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing n in cardinality_exact".to_string())?
                .as_str();
            let n = n_str
                .parse::<BigUint>()
                .map_err(|e| format!("invalid n: {e}"))?;
            Ok(Cardinality::Exact(n))
        }
        Rule::cardinality_bounded => {
            let sym_pair = pair
                .into_inner()
                .next()
                .ok_or_else(|| "missing sym in cardinality_bounded".to_string())?;
            Ok(Cardinality::Bounded(parse_symbolic(sym_pair)?))
        }
        Rule::cardinality_unknown => Ok(Cardinality::Unknown),
        r => Err(format!("unexpected rule {r:?} in cardinality")),
    }
}

impl Serialize for Cardinality {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Cardinality {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Cardinality::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StreamComplexity {
    pub cardinality: Cardinality,
    pub time: ComplexityClass,
    pub exact_time: ExactComplexity,
    pub space: ComplexityClass,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub streams: Vec<StreamComplexity>,
    pub program_time: ComplexityClass,
    pub program_exact_time: ExactComplexity,
    pub program_space: ComplexityClass,
    pub program_cardinality: Cardinality,
}

pub fn space_for_kind(kind: &StreamFunctionKind) -> ComplexityClass {
    match kind {
        StreamFunctionKind::Map => ComplexityClass::O1,
        StreamFunctionKind::Filter => ComplexityClass::O1,
        StreamFunctionKind::FilterMap => ComplexityClass::O1,
        StreamFunctionKind::Fold => ComplexityClass::O1,
        StreamFunctionKind::Ok => ComplexityClass::O1,
        StreamFunctionKind::OkOrPanic => ComplexityClass::O1,
        StreamFunctionKind::Permutations(k) => ComplexityClass::OPermutational(*k),
        StreamFunctionKind::Combinations(k) => ComplexityClass::OCombinatorial(*k),
        StreamFunctionKind::PermutationsWithReplacement(k) => ComplexityClass::OPolynomial(*k),
        StreamFunctionKind::KeepFirstN(_) => ComplexityClass::ON,
    }
}

pub fn step_work_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    match kind {
        StreamFunctionKind::Map => ComplexityClass::O1,
        StreamFunctionKind::Filter => ComplexityClass::O1,
        StreamFunctionKind::FilterMap => ComplexityClass::O1,
        StreamFunctionKind::Fold => ComplexityClass::O1,
        StreamFunctionKind::Ok => ComplexityClass::O1,
        StreamFunctionKind::OkOrPanic => ComplexityClass::O1,
        StreamFunctionKind::KeepFirstN(_) => ComplexityClass::O1,
        StreamFunctionKind::Permutations(k) => {
            if cardinality.try_evaluate().is_some() {
                ComplexityClass::O1
            } else {
                ComplexityClass::OPermutational(*k)
            }
        }
        StreamFunctionKind::Combinations(k) => {
            if cardinality.try_evaluate().is_some() {
                ComplexityClass::O1
            } else {
                ComplexityClass::OCombinatorial(*k)
            }
        }
        StreamFunctionKind::PermutationsWithReplacement(k) => {
            if cardinality.try_evaluate().is_some() {
                ComplexityClass::O1
            } else {
                ComplexityClass::OPolynomial(*k)
            }
        }
    }
}

fn output_cardinality(input: &Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Map => input.clone(),
        StreamFunctionKind::Filter => Symbolic::Filtered(Box::new(input.clone())),
        StreamFunctionKind::FilterMap => Symbolic::Filtered(Box::new(input.clone())),
        StreamFunctionKind::Fold => Symbolic::Constant(BigUint::one()),
        StreamFunctionKind::Ok => input.clone(),
        StreamFunctionKind::OkOrPanic => input.clone(),
        StreamFunctionKind::KeepFirstN(n) => {
            Symbolic::Min(Box::new(input.clone()), Box::new(Symbolic::Constant(BigUint::from(*n))))
        }
        StreamFunctionKind::Permutations { 0: k } => Symbolic::Permutations {
            n: Box::new(input.clone()),
            k: *k,
        },
        StreamFunctionKind::Combinations { 0: k } => Symbolic::Combinations {
            n: Box::new(input.clone()),
            k: *k,
        },
        StreamFunctionKind::PermutationsWithReplacement { 0: k } => {
            Symbolic::PermutationsWithReplacement {
                n: Box::new(input.clone()),
                k: *k,
            }
        }
    }
}

pub fn annotate_pipeline(
    input_cardinality: Symbolic,
    input_variability: InputVariability,
    funs: &[StreamFunctionKind],
) -> StreamComplexity {
    let mut cardinality = input_cardinality;
    let mut exact_time = ExactComplexity::new();
    let mut space = ComplexityClass::O1;

    for fun in funs {
        let step_work = step_work_class(&cardinality, fun);
        exact_time.add_work(step_work, 1);

        let step_space = space_for_kind(fun);
        if step_space > space {
            space = step_space.clone();
        }

        cardinality = output_cardinality(&cardinality, fun);
    }

    // If input variability is unknown, any O(1)-per-step work may still be
    // O(n) total if the input is unbounded. For now we track per-step work.
    let _ = input_variability;

    let time = exact_time.simplified();
    StreamComplexity {
        cardinality: match cardinality.try_evaluate() {
            Some(n) => Cardinality::Exact(n),
            None => match &cardinality {
                Symbolic::Filtered(_) => Cardinality::Bounded(cardinality),
                Symbolic::Unknown => Cardinality::Unknown,
                _ => Cardinality::Bounded(cardinality),
            },
        },
        time,
        exact_time,
        space,
    }
}

pub fn analyze_program(expr: &TypedExpression) -> ProgramComplexity {
    match expr {
        TypedExpression::UnnamedReturningStream(node) => {
            let funs = node.inp_and_funs.funs.clone();
            let (input_card, input_var) = match &node.inp_and_funs.inp {
                InputCount::Known(n) => (
                    Symbolic::Constant(BigUint::from(*n)),
                    InputVariability::Fixed,
                ),
                InputCount::Enum(variants) => (
                    Symbolic::Constant(BigUint::from(variants.len() as u64)),
                    InputVariability::Fixed,
                ),
                InputCount::Unknown => (Symbolic::Unknown, InputVariability::Unknown),
            };
            let stream = annotate_pipeline(input_card, input_var, &funs);
            let program_cardinality = stream.cardinality.clone();
            let program_time = stream.time.clone();
            let program_exact_time = stream.exact_time.clone();
            let program_space = stream.space.clone();
            ProgramComplexity {
                streams: vec![stream],
                program_time,
                program_exact_time,
                program_space,
                program_cardinality,
            }
        }
        TypedExpression::NamedReturningStream(node) => {
            let funs = node.inp_and_funs.funs.clone();
            let (input_card, input_var) = match &node.inp_and_funs.inp {
                InputCount::Known(n) => (
                    Symbolic::Constant(BigUint::from(*n)),
                    InputVariability::Fixed,
                ),
                InputCount::Enum(variants) => (
                    Symbolic::Constant(BigUint::from(variants.len() as u64)),
                    InputVariability::Fixed,
                ),
                InputCount::Unknown => (Symbolic::Unknown, InputVariability::Unknown),
            };
            let stream = annotate_pipeline(input_card, input_var, &funs);
            let program_cardinality = stream.cardinality.clone();
            let program_time = stream.time.clone();
            let program_exact_time = stream.exact_time.clone();
            let program_space = stream.space.clone();
            ProgramComplexity {
                streams: vec![stream],
                program_time,
                program_exact_time,
                program_space,
                program_cardinality,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_and_parse() {
        let cases = [
            (ComplexityClass::O1, "O(1)"),
            (ComplexityClass::OLogN, "O(log(n))"),
            (ComplexityClass::ON, "O(n)"),
            (ComplexityClass::ONLogK(3), "O(n*log(3))"),
            (ComplexityClass::ONLogN, "O(n*log(n))"),
            (ComplexityClass::OPolynomial(2), "O(n^2)"),
            (ComplexityClass::OCombinatorial(3), "O(C(n,3))"),
            (ComplexityClass::OPermutational(4), "O(P(n,4))"),
            (ComplexityClass::OFactorial, "O(n!)"),
            (ComplexityClass::Unknown, "O(?)"),
        ];
        for (class, expected) in &cases {
            assert_eq!(class.to_string(), *expected, "Display mismatch for {class:?}");
            let parsed = ComplexityClass::from_str(expected).unwrap();
            assert_eq!(&parsed, class, "Parse mismatch for {expected}");
        }
    }

    #[test]
    fn test_serde_roundtrip() {
        let cases = [
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogK(5),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(2),
            ComplexityClass::OCombinatorial(3),
            ComplexityClass::OPermutational(4),
            ComplexityClass::OFactorial,
            ComplexityClass::Unknown,
        ];
        for class in &cases {
            let json = serde_json::to_string(class).unwrap();
            let parsed: ComplexityClass = serde_json::from_str(&json).unwrap();
            assert_eq!(&parsed, class, "Serde roundtrip failed for {class:?}");
        }
    }

    #[test]
    fn test_serde_exact_complexity() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 3);
        ec.add_work(ComplexityClass::OLogN, 1);
        let json = serde_json::to_string(&ec).unwrap();
        let parsed: ExactComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(ec, parsed);
    }

    #[test]
    fn test_roundtrip_all_variants() {
        let variants = vec![
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogK(10),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(3),
            ComplexityClass::OCombinatorial(4),
            ComplexityClass::OPermutational(5),
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
        assert!(ComplexityClass::from_str("O(n^)").is_err());
        assert!(ComplexityClass::from_str("O(C(n,))").is_err());
    }

    #[test]
    fn test_ordering() {
        assert!(ComplexityClass::O1 < ComplexityClass::OLogN);
        assert!(ComplexityClass::OLogN < ComplexityClass::ON);
        assert!(ComplexityClass::ON < ComplexityClass::ONLogN);
        assert!(ComplexityClass::ONLogN < ComplexityClass::OPolynomial(2));
        assert!(ComplexityClass::OPolynomial(2) < ComplexityClass::OPolynomial(3));
        assert!(ComplexityClass::OPolynomial(3) < ComplexityClass::OCombinatorial(2));
        assert!(ComplexityClass::OCombinatorial(2) < ComplexityClass::OPermutational(2));
        assert!(ComplexityClass::OPermutational(2) < ComplexityClass::OFactorial);
        assert!(ComplexityClass::OFactorial < ComplexityClass::Unknown);
    }

    #[test]
    fn test_evaluate_constant() {
        let sym = Symbolic::Constant(BigUint::from(42u64));
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(42u64)));
    }

    #[test]
    fn test_evaluate_unknown() {
        let sym = Symbolic::Unknown;
        assert_eq!(sym.try_evaluate(), None);
    }

    #[test]
    fn test_evaluate_permutations() {
        // P(5, 2) = 5 * 4 = 20
        let sym = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(20u64)));
    }

    #[test]
    fn test_evaluate_combinations() {
        // C(5, 2) = 10
        let sym = Symbolic::Combinations {
            n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
            k: 2,
        };
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(10u64)));
    }

    #[test]
    fn test_evaluate_filtered() {
        let sym = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));
        assert_eq!(sym.try_evaluate(), None);
    }

    #[test]
    fn test_evaluate_min() {
        let sym = Symbolic::Min(
            Box::new(Symbolic::Constant(BigUint::from(3u64))),
            Box::new(Symbolic::Constant(BigUint::from(7u64))),
        );
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(3u64)));
    }

    #[test]
    fn test_evaluate_sum() {
        let sym = Symbolic::Sum(vec![
            Symbolic::Constant(BigUint::from(3u64)),
            Symbolic::Constant(BigUint::from(7u64)),
        ]);
        assert_eq!(sym.try_evaluate(), Some(BigUint::from(10u64)));
    }

    #[test]
    fn test_upper_bound_filtered() {
        let inner = Symbolic::Constant(BigUint::from(42u64));
        let sym = Symbolic::Filtered(Box::new(inner));
        assert_eq!(sym.upper_bound(), Some(BigUint::from(42u64)));
    }

    #[test]
    fn test_annotate_pipeline_map() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let funs = vec![StreamFunctionKind::Map];
        let result = annotate_pipeline(card, InputVariability::Fixed, &funs);
        assert_eq!(result.time, ComplexityClass::O1);
        assert_eq!(result.space, ComplexityClass::O1);
        assert_eq!(result.cardinality, Cardinality::Exact(BigUint::from(100u64)));
    }

    #[test]
    fn test_annotate_pipeline_filter() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let funs = vec![StreamFunctionKind::Filter];
        let result = annotate_pipeline(card, InputVariability::Fixed, &funs);
        assert_eq!(result.time, ComplexityClass::O1);
        assert_eq!(result.space, ComplexityClass::O1);
        // filtered output has unknown cardinality
        assert!(matches!(result.cardinality, Cardinality::Bounded(_)));
    }

    #[test]
    fn test_annotate_pipeline_combinations_known() {
        let card = Symbolic::Constant(BigUint::from(10u64));
        let funs = vec![StreamFunctionKind::Combinations(2)];
        let result = annotate_pipeline(card, InputVariability::Fixed, &funs);
        assert_eq!(result.time, ComplexityClass::O1);
    }

    #[test]
    fn test_annotate_pipeline_combinations_unknown() {
        let card = Symbolic::Unknown;
        let funs = vec![StreamFunctionKind::Combinations(2)];
        let result = annotate_pipeline(card, InputVariability::Unknown, &funs);
        assert_eq!(result.time, ComplexityClass::OCombinatorial(2));
    }

    #[test]
    fn test_annotate_pipeline_fold() {
        let card = Symbolic::Constant(BigUint::from(50u64));
        let funs = vec![StreamFunctionKind::Fold];
        let result = annotate_pipeline(card, InputVariability::Fixed, &funs);
        assert_eq!(result.time, ComplexityClass::O1);
        assert_eq!(result.cardinality, Cardinality::Exact(BigUint::one()));
    }

    #[test]
    fn test_exact_complexity_merge() {
        let mut a = ExactComplexity::new();
        a.add_work(ComplexityClass::ON, 2);
        let mut b = ExactComplexity::new();
        b.add_work(ComplexityClass::OPolynomial(2), 1);
        a.merge(&b);
        assert_eq!(a.simplified(), ComplexityClass::OPolynomial(2));
    }

    #[test]
    fn test_exact_complexity_simplified_empty() {
        let ec = ExactComplexity::new();
        assert_eq!(ec.simplified(), ComplexityClass::O1);
    }

    #[test]
    fn test_cardinality_max() {
        let a = Cardinality::Exact(BigUint::from(5u64));
        let b = Cardinality::Exact(BigUint::from(10u64));
        assert_eq!(a.max(b), Cardinality::Exact(BigUint::from(10u64)));
    }

    #[test]
    fn test_cardinality_max_unknown() {
        let a = Cardinality::Exact(BigUint::from(5u64));
        assert_eq!(a.max(Cardinality::Unknown), Cardinality::Unknown);
    }

    #[test]
    fn test_symbolic_display_roundtrip() {
        let cases = vec![
            Symbolic::Constant(BigUint::from(42u64)),
            Symbolic::Unknown,
            Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64)))),
            Symbolic::Permutations {
                n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
                k: 2,
            },
            Symbolic::Combinations {
                n: Box::new(Symbolic::Constant(BigUint::from(5u64))),
                k: 2,
            },
            Symbolic::PermutationsWithReplacement {
                n: Box::new(Symbolic::Constant(BigUint::from(3u64))),
                k: 2,
            },
            Symbolic::Min(
                Box::new(Symbolic::Constant(BigUint::from(3u64))),
                Box::new(Symbolic::Constant(BigUint::from(7u64))),
            ),
            Symbolic::Sum(vec![
                Symbolic::Constant(BigUint::from(3u64)),
                Symbolic::Constant(BigUint::from(7u64)),
            ]),
        ];
        for sym in cases {
            let s = sym.to_string();
            let parsed = Symbolic::from_str(&s).unwrap();
            assert_eq!(sym, parsed, "Roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_cardinality_display_roundtrip() {
        let cases = vec![
            Cardinality::Exact(BigUint::from(42u64)),
            Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
                BigUint::from(10u64),
            )))),
            Cardinality::Unknown,
        ];
        for card in cases {
            let s = card.to_string();
            let parsed = Cardinality::from_str(&s).unwrap();
            assert_eq!(card, parsed, "Roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_exact_complexity_display_roundtrip() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 2);
        ec.add_work(ComplexityClass::OLogN, 1);
        let s = ec.to_string();
        let parsed = ExactComplexity::from_str(&s).unwrap();
        assert_eq!(ec, parsed);
    }

    #[test]
    fn test_exact_complexity_serde() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 2);
        let json = serde_json::to_string(&ec).unwrap();
        let parsed: ExactComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(ec, parsed);
    }

    #[test]
    fn test_cardinality_serde_roundtrip() {
        let cases = vec![
            Cardinality::Exact(BigUint::from(99u64)),
            Cardinality::Unknown,
        ];
        for card in cases {
            let json = serde_json::to_string(&card).unwrap();
            let parsed: Cardinality = serde_json::from_str(&json).unwrap();
            assert_eq!(card, parsed);
        }
    }

    #[test]
    fn test_classify_as_cardinality_constant() {
        let sym = Symbolic::Constant(BigUint::from(5u64));
        assert_eq!(sym.classify_as_cardinality(), Cardinality::Exact(BigUint::from(5u64)));
    }

    #[test]
    fn test_classify_as_cardinality_unknown() {
        let sym = Symbolic::Unknown;
        assert_eq!(sym.classify_as_cardinality(), Cardinality::Unknown);
    }
}

/// Property-based and deterministic unit tests for `ComplexityClass` ordering
/// and `ExactComplexity` algebra. `ONLogK` is pinned to `k=2` in helpers that
/// exercise antisymmetry; the `Ord`/`Eq` mismatch for distinct k values is
/// tracked in issue #232.
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

        #[test]
        fn test_combinatoric_step_work_constant_is_o1(k in 1..10u64, n_val in 1u64..1000) {
            let cardinality = Symbolic::Constant(BigUint::from(n_val));
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::Permutations(k)),
                ComplexityClass::O1
            );
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::PermutationsWithReplacement(k)),
                ComplexityClass::O1
            );
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::Combinations(k)),
                ComplexityClass::O1
            );
        }

        #[test]
        fn test_combinatoric_step_work_unknown_is_asymptotic(k in 1..10u64) {
            let cardinality = Symbolic::Unknown;
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::Permutations(k)),
                ComplexityClass::OPermutational(k)
            );
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::PermutationsWithReplacement(k)),
                ComplexityClass::OPolynomial(k)
            );
            prop_assert_eq!(
                step_work_class(&cardinality, &StreamFunctionKind::Combinations(k)),
                ComplexityClass::OCombinatorial(k)
            );
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

    fn arb_exact_complexity() -> impl Strategy<Value = ExactComplexity> {
        proptest::collection::btree_map(
            prop_oneof![
                Just(ComplexityClass::O1),
                Just(ComplexityClass::OLogN),
                Just(ComplexityClass::ON),
                Just(ComplexityClass::ONLogN),
                (2..10u64).prop_map(ComplexityClass::OPolynomial),
                (1..10u64).prop_map(ComplexityClass::OPermutational),
                (1..10u64).prop_map(ComplexityClass::OCombinatorial),
            ],
            1..10u64,
            1..4,
        )
        .prop_map(|terms| ExactComplexity { terms })
    }

    fn normalize_exact(ec: &ExactComplexity) -> ExactComplexity {
        let has_higher = ec.terms.keys().any(|k| *k > ComplexityClass::O1);
        ExactComplexity {
            terms: ec
                .terms
                .iter()
                .filter(|(class, _)| !has_higher || **class != ComplexityClass::O1)
                .map(|(c, v)| (c.clone(), *v))
                .collect(),
        }
    }

    proptest! {
        #[test]
        fn test_exact_complexity_display_fromstr_roundtrip(ec in arb_exact_complexity()) {
            let s = ec.to_string();
            let parsed = ExactComplexity::from_str(&s).unwrap();
            let expected = normalize_exact(&ec);
            prop_assert_eq!(expected, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_exact_complexity_serde_roundtrip_proptest(ec in arb_exact_complexity()) {
            let json = serde_json::to_string(&ec).unwrap();
            let parsed: ExactComplexity = serde_json::from_str(&json).unwrap();
            let expected = normalize_exact(&ec);
            prop_assert_eq!(expected, parsed);
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
        (1u64..1000).prop_map(|n| Symbolic::Constant(BigUint::from(n)))
    }

    fn arb_symbolic() -> impl Strategy<Value = Symbolic> {
        prop_oneof![
            arb_symbolic_constant(),
            Just(Symbolic::Unknown),
            arb_symbolic_constant().prop_map(|s| Symbolic::Filtered(Box::new(s))),
            (arb_symbolic_constant(), 1u64..5)
                .prop_map(|(s, k)| Symbolic::Permutations { n: Box::new(s), k }),
            (arb_symbolic_constant(), 1u64..5)
                .prop_map(|(s, k)| Symbolic::Combinations { n: Box::new(s), k }),
            (arb_symbolic_constant(), 1u64..4)
                .prop_map(|(s, k)| Symbolic::PermutationsWithReplacement { n: Box::new(s), k }),
            (arb_symbolic_constant(), arb_symbolic_constant())
                .prop_map(|(a, b)| Symbolic::Min(Box::new(a), Box::new(b))),
            proptest::collection::vec(arb_symbolic_constant(), 2..4).prop_map(Symbolic::Sum),
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

    /// A version of arb_complexity_class that pins ONLogK to a single k=2.
    /// ComplexityClass::Ord maps every ONLogK(_) to ordinal 3 but Eq is
    /// structural, so mixing distinct k's violates antisymmetry. Tracked in #232.
    fn arb_complexity_class_fixed() -> impl Strategy<Value = ComplexityClass> {
        prop_oneof![
            Just(ComplexityClass::O1),
            Just(ComplexityClass::OLogN),
            Just(ComplexityClass::ON),
            Just(ComplexityClass::ONLogK(2)),
            Just(ComplexityClass::ONLogN),
            (2..10u64).prop_map(ComplexityClass::OPolynomial),
            (2..10u64).prop_map(ComplexityClass::OCombinatorial),
            (2..10u64).prop_map(ComplexityClass::OPermutational),
            Just(ComplexityClass::OFactorial),
            Just(ComplexityClass::Unknown),
        ]
    }

    /// A version of arb_exact_complexity that pins ONLogK to k=2 and covers all
    /// 10 ComplexityClass variants. See note above about ONLogK Ord/Eq mismatch.
    fn arb_exact_complexity_full() -> impl Strategy<Value = ExactComplexity> {
        proptest::collection::btree_map(
            prop_oneof![
                Just(ComplexityClass::O1),
                Just(ComplexityClass::OLogN),
                Just(ComplexityClass::ON),
                Just(ComplexityClass::ONLogK(2)),
                Just(ComplexityClass::ONLogN),
                (2..10u64).prop_map(ComplexityClass::OPolynomial),
                (1..10u64).prop_map(ComplexityClass::OPermutational),
                (1..10u64).prop_map(ComplexityClass::OCombinatorial),
                Just(ComplexityClass::OFactorial),
                Just(ComplexityClass::Unknown),
            ],
            1..10u64,
            1..4,
        )
        .prop_map(|terms| ExactComplexity { terms })
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn test_simplify_stable_under_subdominant_work(
            ec in arb_exact_complexity_full(),
            extra in arb_complexity_class_fixed(),
            n in 1u64..100,
        ) {
            // Adding work at a class <= the current dominant term must not
            // change simplified(); adding work above it must raise simplified()
            // to that class.
            let s = ec.simplified();
            let mut ec2 = ec.clone();
            ec2.add_work(extra.clone(), n);
            if extra <= s {
                prop_assert_eq!(s, ec2.simplified());
            } else {
                prop_assert_eq!(extra, ec2.simplified());
            }
        }

        #[test]
        fn test_merge_commutativity(a in arb_exact_complexity_full(), b in arb_exact_complexity_full()) {
            let mut ab = a.clone();
            ab.merge(&b);
            let mut ba = b.clone();
            ba.merge(&a);
            prop_assert_eq!(ab, ba);
        }

        #[test]
        fn test_ordering_transitivity(
            a in arb_complexity_class_fixed(),
            b in arb_complexity_class_fixed(),
            c in arb_complexity_class_fixed(),
        ) {
            // Totality: every pair is comparable
            prop_assert!(a <= b || b <= a,
                "ordering is not total: neither {:?} <= {:?} nor {:?} <= {:?}", a, b, b, a);
            // Antisymmetry: a <= b && b <= a => a == b
            // (Safe because arb_complexity_class_fixed pins ONLogK to k=2, avoiding ordinal
            // collision between distinct ONLogK variants; see issue #232)
            if a <= b && b <= a {
                prop_assert_eq!(a.clone(), b.clone(),
                    "antisymmetry violated: {:?} <= {:?} and {:?} <= {:?} but not equal", a, b, b, a);
            }
            // Transitivity
            prop_assume!(a <= b && b <= c);
            prop_assert!(a <= c);
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]

        #[test]
        fn test_merge_identity_proptest(ec in arb_exact_complexity_full()) {
            // Right-identity: ec.merge(empty) == ec
            let mut right = ec.clone();
            right.merge(&ExactComplexity::new());
            prop_assert_eq!(right, ec.clone(),
                "right-identity failed for {:?}", ec);

            // Left-identity: empty.merge(ec) == ec
            let mut left = ExactComplexity::new();
            left.merge(&ec);
            prop_assert_eq!(left, ec.clone(),
                "left-identity failed for {:?}", ec);
        }
    }

    #[test]
    fn test_identity() {
        assert_eq!(ExactComplexity::new().simplified(), ComplexityClass::O1);

        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);

        // Right-identity: ec.merge(empty) == ec
        let before = ec.clone();
        ec.merge(&ExactComplexity::new());
        assert_eq!(ec, before);

        // Left-identity: empty.merge(ec) == ec
        let mut empty = ExactComplexity::new();
        empty.merge(&before);
        assert_eq!(empty, before);
    }

    #[test]
    fn test_ordering_chain() {
        let chain = [
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogK(1),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(2),
            ComplexityClass::OPolynomial(3),
            ComplexityClass::OCombinatorial(2),
            ComplexityClass::OPermutational(2),
            ComplexityClass::OFactorial,
            ComplexityClass::Unknown,
        ];
        for i in 0..chain.len() {
            for j in (i + 1)..chain.len() {
                assert!(
                    chain[i] < chain[j],
                    "{:?} should be < {:?}",
                    chain[i],
                    chain[j]
                );
            }
        }

        // Parametric sweep: verify OPolynomial(k) < OCombinatorial(2) for a wide
        // range of k. The ordinal boundary is 5+k vs 1_000_002, so this is the
        // most likely place to break if the ordinal arithmetic is changed.
        for k in 0u64..50 {
            assert!(
                ComplexityClass::OPolynomial(k) < ComplexityClass::OCombinatorial(2),
                "OPolynomial({k}) should be < OCombinatorial(2)"
            );
        }
    }
}
