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

impl ComplexityClass {
    fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComplexityClass::O1 => write!(f, "1"),
            ComplexityClass::OLogN => write!(f, "log(n)"),
            ComplexityClass::ON => write!(f, "n"),
            ComplexityClass::ONLogK(k) => write!(f, "n*log({k})"),
            ComplexityClass::ONLogN => write!(f, "n*log(n)"),
            ComplexityClass::OPolynomial(k) => write!(f, "n^{k}"),
            ComplexityClass::OCombinatorial(k) => write!(f, "C(n,{k})"),
            ComplexityClass::OPermutational(k) => write!(f, "n!/(n-{k})!"),
            ComplexityClass::OFactorial => write!(f, "n!"),
            ComplexityClass::Unknown => write!(f, "?"),
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
pub struct ExactComplexity {
    terms: BTreeMap<ComplexityClass, u64>,
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

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }
}

impl Default for ExactComplexity {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ExactComplexity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            return write!(f, "O(1)");
        }

        let has_higher = self.terms.keys().any(|k| *k > ComplexityClass::O1);
        let terms_desc: Vec<_> = self
            .terms
            .iter()
            .rev()
            .filter(|(class, _)| !has_higher || **class != ComplexityClass::O1)
            .collect();

        if terms_desc.is_empty() {
            return write!(f, "O(1)");
        }

        write!(f, "O(")?;
        for (i, (class, coeff)) in terms_desc.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            if **class == ComplexityClass::O1 {
                write!(f, "{coeff}")?;
            } else if **coeff == 1 {
                class.fmt_inner(f)?;
            } else {
                write!(f, "{coeff}")?;
                class.fmt_inner(f)?;
            }
        }
        write!(f, ")")
    }
}

impl FromStr for ExactComplexity {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(simple) = ComplexityClass::from_str(s) {
            let mut ec = ExactComplexity::new();
            ec.add_work(simple, 1);
            return Ok(ec);
        }

        let pairs = ComplexityNotationParser::parse(Rule::exact_complexity, s)
            .map_err(|e| format!("Invalid exact complexity notation: {e}"))?;

        let mut ec = ExactComplexity::new();

        for pair in pairs {
            if pair.as_rule() != Rule::exact_complexity {
                continue;
            }
            for inner in pair.into_inner() {
                if inner.as_rule() != Rule::exact_expr {
                    continue;
                }
                for term in inner.into_inner() {
                    if term.as_rule() != Rule::exact_term {
                        continue;
                    }
                    let mut coeff: u64 = 1;
                    let mut class = ComplexityClass::O1;
                    for part in term.into_inner() {
                        match part.as_rule() {
                            Rule::coefficient => {
                                coeff = part
                                    .as_str()
                                    .parse()
                                    .map_err(|_| "Invalid coefficient".to_string())?;
                            }
                            Rule::base_complexity | Rule::base_complexity_non_constant => {
                                class = parse_base_complexity(part.as_str())?;
                            }
                            Rule::constant_term => {
                                coeff = part
                                    .as_str()
                                    .parse()
                                    .map_err(|_| "Invalid constant".to_string())?;
                                class = ComplexityClass::O1;
                            }
                            _ => {}
                        }
                    }
                    ec.add_work(class, coeff);
                }
            }
        }

        Ok(ec)
    }
}

impl Serialize for ExactComplexity {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for ExactComplexity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        ExactComplexity::from_str(&s).map_err(serde::de::Error::custom)
    }
}

fn parse_base_complexity(s: &str) -> Result<ComplexityClass, String> {
    if let Some(rest) = s.strip_prefix("n!/(n-") {
        if let Some(num_str) = rest.strip_suffix(")!") {
            let k: u64 = num_str
                .parse()
                .map_err(|_| format!("Invalid permutation argument: {num_str}"))?;
            return Ok(ComplexityClass::OPermutational(k));
        }
    }
    if let Some(rest) = s.strip_prefix("n^") {
        let k: u64 = rest
            .parse()
            .map_err(|_| format!("Invalid exponent: {rest}"))?;
        return Ok(ComplexityClass::OPolynomial(k));
    }
    if let Some(rest) = s.strip_prefix("n*log(") {
        if let Some(num_str) = rest.strip_suffix(')') {
            if num_str == "n" {
                return Ok(ComplexityClass::ONLogN);
            }
            let k: u64 = num_str
                .parse()
                .map_err(|_| format!("Invalid log argument: {num_str}"))?;
            return Ok(ComplexityClass::ONLogK(k));
        }
    }
    if let Some(rest) = s.strip_prefix("C(n,") {
        if let Some(num_str) = rest.strip_suffix(')') {
            let k: u64 = num_str
                .parse()
                .map_err(|_| format!("Invalid combination argument: {num_str}"))?;
            return Ok(ComplexityClass::OCombinatorial(k));
        }
    }
    match s {
        "1" => Ok(ComplexityClass::O1),
        "log(n)" => Ok(ComplexityClass::OLogN),
        "n" => Ok(ComplexityClass::ON),
        "n!" => Ok(ComplexityClass::OFactorial),
        "?" => Ok(ComplexityClass::Unknown),
        _ => Err(format!("Could not classify base complexity: {s}")),
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

    pub fn contains_unknown(&self) -> bool {
        match self {
            Symbolic::Unknown => true,
            Symbolic::Constant(_) => false,
            Symbolic::Filtered(inner) => inner.contains_unknown(),
            Symbolic::Permutations { n, .. }
            | Symbolic::PermutationsWithReplacement { n, .. }
            | Symbolic::Combinations { n, .. } => n.contains_unknown(),
            Symbolic::Min(a, b) => a.contains_unknown() || b.contains_unknown(),
            Symbolic::Sum(parts) => parts.iter().any(|p| p.contains_unknown()),
        }
    }

    pub fn upper_bound(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(v) => Some(v.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.upper_bound(),
            Symbolic::Permutations { n, k } => {
                let n_val = n.upper_bound()?;
                Some(falling_factorial(&n_val, *k))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.upper_bound()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.upper_bound()?;
                Some(binomial(&n_val, *k))
            }
            Symbolic::Min(a, b) => match (a.upper_bound(), b.upper_bound()) {
                (Some(av), Some(bv)) => Some(av.min(bv)),
                (Some(v), None) | (None, Some(v)) => Some(v),
                (None, None) => None,
            },
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for part in parts {
                    total += part.upper_bound()?;
                }
                Some(total)
            }
        }
    }

    pub fn classify_as_cardinality(&self) -> Cardinality {
        if self.contains_unknown() {
            return Cardinality::Unknown;
        }
        match self.try_evaluate() {
            Some(v) => Cardinality::Exact(v),
            None => Cardinality::Bounded(self.clone()),
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

impl FromStr for Symbolic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pairs = ComplexityNotationParser::parse(Rule::symbolic, s)
            .map_err(|e| format!("Invalid symbolic notation: {e}"))?;

        let pair = pairs
            .into_iter()
            .next()
            .ok_or_else(|| "Empty parse result".to_string())?;

        parse_symbolic_pair(pair)
    }
}

fn parse_symbolic_pair(pair: pest::iterators::Pair<Rule>) -> Result<Symbolic, String> {
    match pair.as_rule() {
        Rule::symbolic => {
            let inner = pair
                .into_inner()
                .find(|p| p.as_rule() == Rule::symbolic_expr)
                .ok_or_else(|| "Missing symbolic_expr".to_string())?;
            parse_symbolic_pair(inner)
        }
        Rule::symbolic_expr => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "Empty symbolic_expr".to_string())?;
            parse_symbolic_pair(inner)
        }
        Rule::symbolic_sum => {
            let parts: Result<Vec<Symbolic>, String> =
                pair.into_inner().map(parse_symbolic_pair).collect();
            Ok(Symbolic::Sum(parts?))
        }
        Rule::symbolic_atom => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "Empty symbolic_atom".to_string())?;
            parse_symbolic_pair(inner)
        }
        Rule::symbolic_filtered => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "Missing filtered inner".to_string())?;
            Ok(Symbolic::Filtered(Box::new(parse_symbolic_pair(inner)?)))
        }
        Rule::symbolic_min => {
            let mut inner = pair.into_inner();
            let a = inner
                .next()
                .ok_or_else(|| "Missing min first arg".to_string())?;
            let b = inner
                .next()
                .ok_or_else(|| "Missing min second arg".to_string())?;
            Ok(Symbolic::Min(
                Box::new(parse_symbolic_pair(a)?),
                Box::new(parse_symbolic_pair(b)?),
            ))
        }
        Rule::symbolic_perm => {
            let mut inner = pair.into_inner();
            let n_pair = inner.next().ok_or_else(|| "Missing P n arg".to_string())?;
            let k_pair = inner.next().ok_or_else(|| "Missing P k arg".to_string())?;
            let k: u64 = k_pair
                .as_str()
                .parse()
                .map_err(|_| "Invalid P k value".to_string())?;
            Ok(Symbolic::Permutations {
                n: Box::new(parse_symbolic_pair(n_pair)?),
                k,
            })
        }
        Rule::symbolic_perm_rep => {
            let mut inner = pair.into_inner();
            let n_pair = inner
                .next()
                .ok_or_else(|| "Missing n^k n arg".to_string())?;
            let k_pair = inner
                .next()
                .ok_or_else(|| "Missing n^k k arg".to_string())?;
            let k: u64 = k_pair
                .as_str()
                .parse()
                .map_err(|_| "Invalid n^k k value".to_string())?;
            Ok(Symbolic::PermutationsWithReplacement {
                n: Box::new(parse_symbolic_pair(n_pair)?),
                k,
            })
        }
        Rule::symbolic_comb => {
            let mut inner = pair.into_inner();
            let n_pair = inner.next().ok_or_else(|| "Missing C n arg".to_string())?;
            let k_pair = inner.next().ok_or_else(|| "Missing C k arg".to_string())?;
            let k: u64 = k_pair
                .as_str()
                .parse()
                .map_err(|_| "Invalid C k value".to_string())?;
            Ok(Symbolic::Combinations {
                n: Box::new(parse_symbolic_pair(n_pair)?),
                k,
            })
        }
        Rule::symbolic_constant => {
            let v: BigUint = pair
                .as_str()
                .parse()
                .map_err(|_| format!("Invalid constant: {}", pair.as_str()))?;
            Ok(Symbolic::Constant(v))
        }
        Rule::symbolic_unknown => Ok(Symbolic::Unknown),
        r => Err(format!("Unexpected rule: {r:?}")),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    Exact(BigUint),
    Bounded(Symbolic),
    Unknown,
}

impl Cardinality {
    fn ordinal(&self) -> u64 {
        match self {
            Cardinality::Exact(_) => 0,
            Cardinality::Bounded(_) => 1,
            Cardinality::Unknown => 2,
        }
    }

    pub fn meet(self, other: &Cardinality) -> Cardinality {
        match (&self, other) {
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            (Cardinality::Exact(v1), Cardinality::Exact(v2)) => {
                Cardinality::Exact(v1.clone().min(v2.clone()))
            }
            // Bounded(sym).meet(&Exact(v)): if sym is already bounded by v, keep self;
            // otherwise constrain to Min(sym, v).
            (Cardinality::Bounded(sym), Cardinality::Exact(v)) => match sym.upper_bound() {
                Some(ub) if ub <= *v => self,
                _ => Cardinality::Bounded(Symbolic::Min(
                    Box::new(sym.clone()),
                    Box::new(Symbolic::Constant(v.clone())),
                )),
            },
            // Exact(v).meet(&Bounded(sym)): if sym is already bounded by v, return
            // the bounded side; otherwise constrain to Min(sym, v).
            (Cardinality::Exact(v), Cardinality::Bounded(sym)) => match sym.upper_bound() {
                Some(ub) if ub <= *v => other.clone(),
                _ => Cardinality::Bounded(Symbolic::Min(
                    Box::new(sym.clone()),
                    Box::new(Symbolic::Constant(v.clone())),
                )),
            },
            (Cardinality::Bounded(s1), Cardinality::Bounded(s2)) => {
                Cardinality::Bounded(Symbolic::Min(Box::new(s1.clone()), Box::new(s2.clone())))
            }
        }
    }
}

impl_ord_via_ordinal!(Cardinality);

impl fmt::Display for Cardinality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cardinality::Exact(n) => write!(f, "{n}"),
            Cardinality::Bounded(sym) => write!(f, "{sym}"),
            Cardinality::Unknown => write!(f, "?"),
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
        Cardinality::from_str(&s).map_err(serde::de::Error::custom)
    }
}

impl FromStr for Cardinality {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "?" {
            return Ok(Cardinality::Unknown);
        }
        if let Ok(n) = s.parse::<BigUint>() {
            return Ok(Cardinality::Exact(n));
        }
        let sym = Symbolic::from_str(s)?;
        Ok(Cardinality::Bounded(sym))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StreamComplexity {
    pub description: String,
    pub cardinality: Cardinality,
    pub time_class: ComplexityClass,
    pub exact_time: ExactComplexity,
    pub space_class: ComplexityClass,
    pub exact_space: ExactComplexity,
    pub collects_input: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub streams: Vec<StreamComplexity>,
}

fn falling_factorial(n: &BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    for i in 0..k {
        result *= n - BigUint::from(i);
    }
    result
}

fn binomial(n: &BigUint, k: u64) -> BigUint {
    falling_factorial(n, k) / (1..=k).fold(BigUint::one(), |acc, x| acc * BigUint::from(x))
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match (&inp.variability, &inp.input_count) {
        (InputVariability::Variable, _) | (_, InputCount::Unknown) => Symbolic::Unknown,
        (InputVariability::Constant, InputCount::Known(n)) => Symbolic::Constant(n.clone()),
        _ => Symbolic::Unknown,
    }
}

/// Declarative table capturing the semantics of each `StreamFunctionKind`.
///
/// Adding a new variant to `StreamFunctionKind` requires updating exactly one
/// place: the exhaustive match inside [`semantics_for`].
pub(crate) struct StreamFunctionSemantics {
    pub(crate) cardinality_transform: Box<dyn Fn(Symbolic) -> Symbolic>,
    pub(crate) time_class: Box<dyn Fn(&Symbolic) -> ComplexityClass>,
    pub(crate) space_class: Box<dyn Fn(&Symbolic) -> ComplexityClass>,
    pub(crate) collects_input: bool,
    pub(crate) description: Box<dyn Fn() -> String>,
}

/// Returns the full semantic descriptor for the given stream function kind.
///
/// The match is **exhaustive** (no wildcard `_` arm) so that adding a new
/// `StreamFunctionKind` variant produces a compile error until this function
/// is updated.
pub(crate) fn semantics_for(kind: &StreamFunctionKind) -> StreamFunctionSemantics {
    match kind {
        StreamFunctionKind::Map => StreamFunctionSemantics {
            cardinality_transform: Box::new(|card| card),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "map(...)".to_string()),
        },
        StreamFunctionKind::Filter => StreamFunctionSemantics {
            cardinality_transform: Box::new(|card| Symbolic::Filtered(Box::new(card))),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "filter(...)".to_string()),
        },
        StreamFunctionKind::FilterMap => StreamFunctionSemantics {
            cardinality_transform: Box::new(|card| Symbolic::Filtered(Box::new(card))),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "filter_map(...)".to_string()),
        },
        StreamFunctionKind::Permutations(k) => {
            let k = *k;
            StreamFunctionSemantics {
                cardinality_transform: Box::new(move |card| Symbolic::Permutations {
                    n: Box::new(card),
                    k,
                }),
                time_class: Box::new(move |_| ComplexityClass::OPermutational(k)),
                space_class: Box::new(|card| cardinality_to_time_class(card)),
                collects_input: true,
                description: Box::new(move || format!("permutations({k})")),
            }
        }
        StreamFunctionKind::PermutationsWithReplacement(k) => {
            let k = *k;
            StreamFunctionSemantics {
                cardinality_transform: Box::new(move |card| {
                    Symbolic::PermutationsWithReplacement {
                        n: Box::new(card),
                        k,
                    }
                }),
                time_class: Box::new(move |_| ComplexityClass::OPolynomial(k)),
                space_class: Box::new(|card| cardinality_to_time_class(card)),
                collects_input: true,
                description: Box::new(move || format!("permutations_with_replacement({k})")),
            }
        }
        StreamFunctionKind::Combinations(k) => {
            let k = *k;
            StreamFunctionSemantics {
                cardinality_transform: Box::new(move |card| Symbolic::Combinations {
                    n: Box::new(card),
                    k,
                }),
                time_class: Box::new(move |_| ComplexityClass::OCombinatorial(k)),
                space_class: Box::new(|card| cardinality_to_time_class(card)),
                collects_input: true,
                description: Box::new(move || format!("combinations({k})")),
            }
        }
        StreamFunctionKind::KeepFirstN(k) => {
            let k = *k;
            StreamFunctionSemantics {
                cardinality_transform: Box::new(move |card| {
                    Symbolic::Min(
                        Box::new(card),
                        Box::new(Symbolic::Constant(BigUint::from(k))),
                    )
                }),
                time_class: Box::new(|card| cardinality_to_time_class(card)),
                space_class: Box::new(|_| ComplexityClass::O1),
                collects_input: false,
                description: Box::new(move || format!("keep_first_n({k}, ...)")),
            }
        }
        StreamFunctionKind::Fold => StreamFunctionSemantics {
            cardinality_transform: Box::new(|_| Symbolic::Constant(BigUint::one())),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "fold(...)".to_string()),
        },
        StreamFunctionKind::Ok => StreamFunctionSemantics {
            cardinality_transform: Box::new(|card| Symbolic::Filtered(Box::new(card))),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "ok()".to_string()),
        },
        StreamFunctionKind::OkOrPanic => StreamFunctionSemantics {
            cardinality_transform: Box::new(|card| card),
            time_class: Box::new(|card| cardinality_to_time_class(card)),
            space_class: Box::new(|_| ComplexityClass::O1),
            collects_input: false,
            description: Box::new(|| "ok_or_panic()".to_string()),
        },
    }
}

fn propagate_cardinality(cardinality: Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    (semantics_for(kind).cardinality_transform)(cardinality)
}

#[cfg(test)]
fn space_for_kind(kind: &StreamFunctionKind) -> ComplexityClass {
    // Use a non-evaluable cardinality so that cardinality_to_time_class exercises
    // the classify_as_time() path rather than the short-circuit try_evaluate() path.
    let card = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));
    (semantics_for(kind).space_class)(&card)
}

fn cardinality_to_time_class(cardinality: &Symbolic) -> ComplexityClass {
    if let Some(v) = cardinality.try_evaluate() {
        return if v <= BigUint::one() {
            ComplexityClass::O1
        } else {
            ComplexityClass::ON
        };
    }
    let base = cardinality.classify_as_time();
    if base == ComplexityClass::O1 {
        ComplexityClass::ON
    } else {
        base
    }
}

fn step_work_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    (semantics_for(kind).time_class)(cardinality)
}

fn step_space_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    (semantics_for(kind).space_class)(cardinality)
}

fn analyze_stream_fns(
    funs: &[crate::nodes::StreamFunctionNode],
    initial_cardinality: Symbolic,
    description: &str,
) -> StreamComplexity {
    let mut cardinality = initial_cardinality;
    let mut exact_time = ExactComplexity::new();
    let mut exact_space = ExactComplexity::new();
    let mut collects = false;

    for f in funs {
        let time_work = step_work_class(&cardinality, &f.kind);
        exact_time.add_work(time_work, 1);

        let space_work = step_space_class(&cardinality, &f.kind);
        exact_space.add_work(space_work, 1);

        if semantics_for(&f.kind).collects_input {
            collects = true;
        }
        cardinality = propagate_cardinality(cardinality, &f.kind);
    }

    if exact_time.is_empty() {
        exact_time.add_work(cardinality_to_time_class(&cardinality), 1);
    }

    let time = exact_time.simplified();
    let space = exact_space.simplified();

    StreamComplexity {
        description: description.to_string(),
        cardinality: cardinality.classify_as_cardinality(),
        time_class: time,
        exact_time,
        space_class: space,
        exact_space,
        collects_input: collects,
    }
}

fn describe_stream_fns(funs: &[crate::nodes::StreamFunctionNode]) -> String {
    funs.iter()
        .map(|f| (semantics_for(&f.kind).description)())
        .collect::<Vec<_>>()
        .join(".")
}

pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    let mut stream_vars: std::collections::HashMap<
        String,
        (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    > = std::collections::HashMap::new();

    for expr in expressions {
        match expr {
            TypedExpression::StreamVariable(v) => {
                let card = input_cardinality(&v.inp);
                let mut current_card = card;
                let mut var_exact_time = ExactComplexity::new();
                let mut var_exact_space = ExactComplexity::new();
                for f in &v.funs {
                    let time_work = step_work_class(&current_card, &f.kind);
                    var_exact_time.add_work(time_work, 1);
                    let space_work = step_space_class(&current_card, &f.kind);
                    var_exact_space.add_work(space_work, 1);
                    current_card = propagate_cardinality(current_card, &f.kind);
                }
                let space = var_exact_space.simplified().max(ComplexityClass::ON);
                stream_vars.insert(
                    v.variable_name.clone(),
                    (current_card, space, var_exact_time, var_exact_space),
                );
            }
            TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                let (prior_card, prior_space, prior_exact_time, prior_exact_space) = stream_vars
                    .get(&v.prior_stream_variable)
                    .cloned()
                    .unwrap_or((
                        Symbolic::Unknown,
                        ComplexityClass::Unknown,
                        ExactComplexity::new(),
                        ExactComplexity::new(),
                    ));
                let mut current_card = prior_card;
                let mut var_exact_time = prior_exact_time;
                let mut var_exact_space = prior_exact_space;
                for f in &v.funs {
                    let time_work = step_work_class(&current_card, &f.kind);
                    var_exact_time.add_work(time_work, 1);
                    let space_work = step_space_class(&current_card, &f.kind);
                    var_exact_space.add_work(space_work, 1);
                    current_card = propagate_cardinality(current_card, &f.kind);
                }
                let space = var_exact_space.simplified().max(prior_space);
                stream_vars.insert(
                    v.variable_name.clone(),
                    (current_card, space, var_exact_time, var_exact_space),
                );
            }
            _ => {}
        }
    }

    let mut streams = Vec::new();

    for expr in expressions {
        let sc = match expr {
            TypedExpression::UnnamedReturningStream(s)
            | TypedExpression::UnnamedNonReturningStream(s) => {
                let card = input_cardinality(&s.inp_and_funs.inp);
                let funs_desc = describe_stream_fns(&s.inp_and_funs.funs);
                let out_desc = if s.out.returning {
                    "return"
                } else {
                    "write_file(...)"
                };
                let desc = if funs_desc.is_empty() {
                    format!("input.{out_desc}")
                } else {
                    format!("input.{funs_desc}.{out_desc}")
                };
                analyze_stream_fns(&s.inp_and_funs.funs, card, &desc)
            }
            TypedExpression::NamedReturningStream(s)
            | TypedExpression::NamedNonReturningStream(s) => {
                let (card, var_space, var_exact_time, var_exact_space) =
                    stream_vars.get(&s.stream_variable).cloned().unwrap_or((
                        Symbolic::Unknown,
                        ComplexityClass::Unknown,
                        ExactComplexity::new(),
                        ExactComplexity::new(),
                    ));
                let funs_desc = describe_stream_fns(&s.funs);
                let out_desc = if s.out.returning {
                    "return"
                } else {
                    "write_file(...)"
                };
                let desc = if funs_desc.is_empty() {
                    format!("{}.{out_desc}", s.stream_variable)
                } else {
                    format!("{}.{funs_desc}.{out_desc}", s.stream_variable)
                };
                let mut sc = analyze_stream_fns(&s.funs, card, &desc);
                sc.exact_time.merge(&var_exact_time);
                sc.time_class = sc.exact_time.simplified();
                sc.exact_space.merge(&var_exact_space);
                sc.space_class = sc.exact_space.simplified().max(var_space);
                sc
            }
            _ => continue,
        };

        streams.push(sc);
    }

    ProgramComplexity { streams }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_complexity_class_ordering() {
        let classes = [
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogK(2),
            ComplexityClass::ONLogN,
            ComplexityClass::OPolynomial(2),
            ComplexityClass::OPolynomial(3),
            ComplexityClass::OCombinatorial(2),
            ComplexityClass::OPermutational(2),
            ComplexityClass::OFactorial,
        ];
        for i in 0..classes.len() {
            for j in i..classes.len() {
                if i == j {
                    assert_eq!(classes[i], classes[j]);
                } else {
                    assert!(
                        classes[i] < classes[j],
                        "{:?} should be < {:?}",
                        classes[i],
                        classes[j]
                    );
                }
            }
        }
    }

    #[test]
    fn test_complexity_class_max() {
        assert_eq!(
            ComplexityClass::ON.max(ComplexityClass::O1),
            ComplexityClass::ON
        );
        assert_eq!(
            ComplexityClass::O1.max(ComplexityClass::ON),
            ComplexityClass::ON
        );
        assert_eq!(
            ComplexityClass::ON.max(ComplexityClass::ON),
            ComplexityClass::ON
        );
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
    fn test_exact_complexity_display() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 2);
        ec.add_work(ComplexityClass::O1, 3);
        assert_eq!(ec.to_string(), "O(2n + 3)");
    }

    #[test]
    fn test_exact_complexity_simplified() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        ec.add_work(ComplexityClass::O1, 5);
        assert_eq!(ec.simplified(), ComplexityClass::ON);
    }

    #[test]
    fn test_exact_complexity_merge() {
        let mut ec1 = ExactComplexity::new();
        ec1.add_work(ComplexityClass::ON, 1);
        let mut ec2 = ExactComplexity::new();
        ec2.add_work(ComplexityClass::ON, 2);
        ec2.add_work(ComplexityClass::O1, 1);
        ec1.merge(&ec2);
        assert_eq!(ec1.to_string(), "O(3n + 1)");
    }

    #[test]
    fn test_exact_complexity_empty() {
        let ec = ExactComplexity::new();
        assert_eq!(ec.simplified(), ComplexityClass::O1);
        assert_eq!(ec.to_string(), "O(1)");
    }

    #[test]
    fn test_exact_complexity_only_constant() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::O1, 5);
        assert_eq!(ec.to_string(), "O(5)");
    }

    #[test]
    fn test_exact_complexity_fromstr_roundtrip() {
        let cases = [
            "O(1)",
            "O(n)",
            "O(n^2)",
            "O(log(n))",
            "O(n*log(n))",
            "O(n!/(n-3)!)",
            "O(C(n,2))",
            "O(n + 5)",
            "O(2n + n^2)",
        ];
        for s in cases {
            let ec = ExactComplexity::from_str(s).unwrap();
            assert_eq!(ec.to_string(), s, "roundtrip failed for {s}");
        }
    }

    #[test]
    fn test_exact_complexity_fromstr_invalid() {
        assert!(ExactComplexity::from_str("not_valid").is_err());
    }

    #[test]
    fn test_symbolic_constant_evaluate() {
        let s = Symbolic::Constant(BigUint::from(5u64));
        assert_eq!(s.try_evaluate(), Some(BigUint::from(5u64)));
    }

    #[test]
    fn test_symbolic_unknown_evaluate() {
        assert_eq!(Symbolic::Unknown.try_evaluate(), None);
    }

    #[test]
    fn test_symbolic_filtered_evaluate() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(5u64))));
        assert_eq!(s.try_evaluate(), None);
    }

    #[test]
    fn test_symbolic_filter_display() {
        let inner = Symbolic::Constant(BigUint::from(5u64));
        let filtered = Symbolic::Filtered(Box::new(inner));
        assert_eq!(filtered.to_string(), "\u{2264}5");
    }

    #[test]
    fn test_propagate_cardinality_filter() {
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
    fn test_map_complexity() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = step_work_class(&card, &StreamFunctionKind::Map);
        assert_eq!(result, ComplexityClass::ON);
    }

    #[test]
    fn test_filter_space() {
        let result = space_for_kind(&StreamFunctionKind::Filter);
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_permutations_space() {
        let result = space_for_kind(&StreamFunctionKind::Permutations(3));
        assert_eq!(result, ComplexityClass::ON);
    }

    #[test]
    fn test_combinations_space() {
        let result = space_for_kind(&StreamFunctionKind::Combinations(2));
        assert_eq!(result, ComplexityClass::ON);
    }

    #[test]
    fn test_keep_first_n_space() {
        let result = space_for_kind(&StreamFunctionKind::KeepFirstN(5));
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_permutations_time() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = step_work_class(&card, &StreamFunctionKind::Permutations(3));
        assert_eq!(result, ComplexityClass::OPermutational(3));
    }

    #[test]
    fn test_permutations_with_replacement_time() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = step_work_class(&card, &StreamFunctionKind::PermutationsWithReplacement(2));
        assert_eq!(result, ComplexityClass::OPolynomial(2));
    }

    #[test]
    fn test_combinations_time() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = step_work_class(&card, &StreamFunctionKind::Combinations(2));
        assert_eq!(result, ComplexityClass::OCombinatorial(2));
    }

    #[test]
    fn test_empty_stream() {
        let result = analyze_stream_fns(&[], Symbolic::Unknown, "test");
        assert_eq!(result.time_class, ComplexityClass::Unknown);
        assert_eq!(result.space_class, ComplexityClass::O1);
    }

    #[test]
    fn test_stream_with_map() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![StreamFunctionNode {
            kind: StreamFunctionKind::Map,
            code: String::new(),
        }];
        let result = analyze_stream_fns(&funs, Symbolic::Unknown, "test");
        assert_eq!(result.time_class, ComplexityClass::Unknown);
    }

    #[test]
    fn test_collects_input_flag() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![StreamFunctionNode {
            kind: StreamFunctionKind::Permutations(2),
            code: String::new(),
        }];
        let result = analyze_stream_fns(&funs, Symbolic::Unknown, "test");
        assert!(result.collects_input);
    }

    #[test]
    fn test_no_collect_for_map() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![StreamFunctionNode {
            kind: StreamFunctionKind::Map,
            code: String::new(),
        }];
        let result = analyze_stream_fns(&funs, Symbolic::Unknown, "test");
        assert!(!result.collects_input);
    }

    #[test]
    fn test_describe_stream_fns() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![
            StreamFunctionNode {
                kind: StreamFunctionKind::Map,
                code: String::new(),
            },
            StreamFunctionNode {
                kind: StreamFunctionKind::Filter,
                code: String::new(),
            },
        ];
        let desc = describe_stream_fns(&funs);
        assert_eq!(desc, "map(...).filter(...)");
    }

    #[test]
    fn test_cardinality_to_time_class_constant_zero() {
        let card = Symbolic::Constant(BigUint::zero());
        assert_eq!(cardinality_to_time_class(&card), ComplexityClass::O1);
    }

    #[test]
    fn test_cardinality_to_time_class_constant_one() {
        let card = Symbolic::Constant(BigUint::one());
        assert_eq!(cardinality_to_time_class(&card), ComplexityClass::O1);
    }

    #[test]
    fn test_cardinality_to_time_class_constant_large() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        assert_eq!(cardinality_to_time_class(&card), ComplexityClass::ON);
    }

    #[test]
    fn test_cardinality_to_time_class_unknown() {
        assert_eq!(
            cardinality_to_time_class(&Symbolic::Unknown),
            ComplexityClass::Unknown
        );
    }

    #[test]
    fn test_cardinality_to_time_class_filtered() {
        let card = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert_eq!(cardinality_to_time_class(&card), ComplexityClass::ON);
    }

    #[test]
    fn test_symbolic_display_permutations() {
        let s = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
            k: 3,
        };
        assert_eq!(s.to_string(), "P(100, 3)");
    }

    #[test]
    fn test_symbolic_display_pwr() {
        let s = Symbolic::PermutationsWithReplacement {
            n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
            k: 2,
        };
        assert_eq!(s.to_string(), "100^2");
    }

    #[test]
    fn test_symbolic_display_combinations() {
        let s = Symbolic::Combinations {
            n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
            k: 2,
        };
        assert_eq!(s.to_string(), "C(100, 2)");
    }

    #[test]
    fn test_symbolic_display_min() {
        let s = Symbolic::Min(
            Box::new(Symbolic::Constant(BigUint::from(100u64))),
            Box::new(Symbolic::Constant(BigUint::from(5u64))),
        );
        assert_eq!(s.to_string(), "min(100, 5)");
    }

    #[test]
    fn test_symbolic_classify_as_time_filtered() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert_eq!(s.classify_as_time(), ComplexityClass::O1);
    }

    #[test]
    fn test_symbolic_classify_as_time_permutations() {
        let s = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
            k: 3,
        };
        assert_eq!(s.classify_as_time(), ComplexityClass::OPermutational(3));
    }

    #[test]
    fn test_cardinality_meet_exact_exact() {
        let c1 = Cardinality::Exact(BigUint::from(10u64));
        let c2 = Cardinality::Exact(BigUint::from(20u64));
        assert_eq!(c1.meet(&c2), Cardinality::Exact(BigUint::from(10u64)));
    }

    #[test]
    fn test_cardinality_meet_unknown() {
        let c1 = Cardinality::Unknown;
        let c2 = Cardinality::Exact(BigUint::from(10u64));
        assert_eq!(c1.meet(&c2), Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_display() {
        assert_eq!(Cardinality::Exact(BigUint::from(42u64)).to_string(), "42");
        assert_eq!(Cardinality::Unknown.to_string(), "?");
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
    fn test_analyze_program_empty() {
        let result = analyze_program(&[]);
        assert!(result.streams.is_empty());
    }

    #[test]
    fn test_symbolic_contains_unknown() {
        assert!(Symbolic::Unknown.contains_unknown());
        assert!(!Symbolic::Constant(BigUint::from(5u64)).contains_unknown());
        assert!(Symbolic::Filtered(Box::new(Symbolic::Unknown)).contains_unknown());
    }

    #[test]
    fn test_symbolic_upper_bound_constant() {
        let s = Symbolic::Constant(BigUint::from(10u64));
        assert_eq!(s.upper_bound(), Some(BigUint::from(10u64)));
    }

    #[test]
    fn test_symbolic_upper_bound_unknown() {
        assert_eq!(Symbolic::Unknown.upper_bound(), None);
    }

    #[test]
    fn test_symbolic_upper_bound_filtered() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));
        assert_eq!(s.upper_bound(), Some(BigUint::from(10u64)));
    }

    #[test]
    fn test_symbolic_fromstr_roundtrip() {
        let cases = [
            "10",
            "?",
            "\u{2264}10",
            "P(100, 3)",
            "100^2",
            "C(100, 2)",
            "min(100, 5)",
        ];
        for s in cases {
            let sym =
                Symbolic::from_str(s).unwrap_or_else(|e| panic!("parse error for '{s}': {e}"));
            assert_eq!(sym.to_string(), s, "roundtrip failed for '{s}'");
        }
    }

    #[test]
    fn test_cardinality_ordering() {
        assert!(Cardinality::Exact(BigUint::from(5u64)) < Cardinality::Bounded(Symbolic::Unknown));
        assert!(Cardinality::Bounded(Symbolic::Unknown) < Cardinality::Unknown);
    }

    #[test]
    fn test_exact_complexity_display_single_non_constant() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        assert_eq!(ec.to_string(), "O(n)");
    }

    #[test]
    fn test_exact_complexity_display_multiple_non_constant() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 3);
        ec.add_work(ComplexityClass::OPolynomial(2), 1);
        assert_eq!(ec.to_string(), "O(n^2 + 3n)");
    }

    #[test]
    fn test_complexity_class_from_str_all_variants() {
        let cases = [
            ("O(1)", ComplexityClass::O1),
            ("O(log(n))", ComplexityClass::OLogN),
            ("O(n)", ComplexityClass::ON),
            ("O(n!)", ComplexityClass::OFactorial),
            ("O(?)", ComplexityClass::Unknown),
            ("O(n^2)", ComplexityClass::OPolynomial(2)),
            ("O(n*log(n))", ComplexityClass::ONLogN),
            ("O(n*log(3))", ComplexityClass::ONLogK(3)),
            ("O(C(n,2))", ComplexityClass::OCombinatorial(2)),
            ("O(n!/(n-3)!)", ComplexityClass::OPermutational(3)),
        ];
        for (s, expected) in cases {
            let parsed = ComplexityClass::from_str(s).unwrap();
            assert_eq!(parsed, expected, "failed for {s}");
        }
    }

    #[test]
    fn test_complexity_class_from_str_invalid() {
        assert!(ComplexityClass::from_str("invalid").is_err());
        assert!(ComplexityClass::from_str("O(x)").is_err());
    }

    #[test]
    fn test_exact_complexity_serde_roundtrip() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 2);
        ec.add_work(ComplexityClass::O1, 3);
        let json = serde_json::to_string(&ec).unwrap();
        let parsed: ExactComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(ec, parsed);
    }

    #[test]
    fn test_stream_complexity_serde() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![StreamFunctionNode {
            kind: StreamFunctionKind::Map,
            code: String::new(),
        }];
        let sc = analyze_stream_fns(&funs, Symbolic::Constant(BigUint::from(100u64)), "test");
        let json = serde_json::to_string(&sc).unwrap();
        let parsed: StreamComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(sc, parsed);
    }

    #[test]
    fn test_program_complexity_serde() {
        let pc = analyze_program(&[]);
        let json = serde_json::to_string(&pc).unwrap();
        let parsed: ProgramComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(pc, parsed);
    }

    #[test]
    fn test_symbolic_sum_evaluate() {
        let s = Symbolic::Sum(vec![
            Symbolic::Constant(BigUint::from(3u64)),
            Symbolic::Constant(BigUint::from(4u64)),
        ]);
        assert_eq!(s.try_evaluate(), Some(BigUint::from(7u64)));
    }

    #[test]
    fn test_symbolic_sum_unknown() {
        let s = Symbolic::Sum(vec![
            Symbolic::Constant(BigUint::from(3u64)),
            Symbolic::Unknown,
        ]);
        assert_eq!(s.try_evaluate(), None);
    }

    #[test]
    fn test_cardinality_meet_bounded_exact() {
        let sym = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        let c1 = Cardinality::Bounded(sym.clone());
        let c2 = Cardinality::Exact(BigUint::from(50u64));
        let result = c1.meet(&c2);
        // bounded meet exact should produce a bounded (min of the two)
        assert!(matches!(result, Cardinality::Bounded(_)));
    }

    #[test]
    fn test_cardinality_meet_exact_bounded() {
        // Exact(v).meet(&Bounded(sym)) must return the bounded side, not Exact(v).
        // Previously an or-pattern returned `self` (Exact(3)) when the Exact arm
        // fired, giving the wrong result.
        let sym = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        let c_exact = Cardinality::Exact(BigUint::from(50u64));
        let c_bounded = Cardinality::Bounded(sym);
        let result = c_exact.meet(&c_bounded);
        assert!(
            matches!(result, Cardinality::Bounded(_)),
            "Exact.meet(&Bounded) should return Bounded, got {:?}",
            result
        );
    }

    #[test]
    fn test_exact_complexity_add_same_class_twice() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        ec.add_work(ComplexityClass::ON, 2);
        assert_eq!(ec.to_string(), "O(3n)");
    }

    #[test]
    fn test_symbolic_min_evaluate() {
        let s = Symbolic::Min(
            Box::new(Symbolic::Constant(BigUint::from(5u64))),
            Box::new(Symbolic::Constant(BigUint::from(3u64))),
        );
        assert_eq!(s.try_evaluate(), Some(BigUint::from(3u64)));
    }

    #[test]
    fn test_analyze_stream_fns_description() {
        use crate::nodes::StreamFunctionNode;
        let funs = vec![
            StreamFunctionNode {
                kind: StreamFunctionKind::Permutations(2),
                code: String::new(),
            },
            StreamFunctionNode {
                kind: StreamFunctionKind::Filter,
                code: String::new(),
            },
        ];
        let result = analyze_stream_fns(&funs, Symbolic::Constant(BigUint::from(100u64)), "desc");
        assert_eq!(result.description, "desc");
    }

    #[test]
    fn test_symbolic_classify_as_cardinality_exact() {
        let s = Symbolic::Constant(BigUint::from(5u64));
        assert_eq!(
            s.classify_as_cardinality(),
            Cardinality::Exact(BigUint::from(5u64))
        );
    }

    #[test]
    fn test_symbolic_classify_as_cardinality_bounded() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert!(matches!(
            s.classify_as_cardinality(),
            Cardinality::Bounded(_)
        ));
    }

    #[test]
    fn test_symbolic_classify_as_cardinality_unknown() {
        assert_eq!(
            Symbolic::Unknown.classify_as_cardinality(),
            Cardinality::Unknown
        );
    }

    #[test]
    fn test_keep_first_n_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::KeepFirstN(5));
        if let Symbolic::Min(a, b) = result {
            assert!(matches!(*a, Symbolic::Constant(_)));
            assert!(matches!(*b, Symbolic::Constant(_)));
        } else {
            panic!("Expected Symbolic::Min");
        }
    }

    #[test]
    fn test_ok_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::Ok);
        assert!(matches!(result, Symbolic::Filtered(_)));
    }

    #[test]
    fn test_ok_or_panic_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card.clone(), &StreamFunctionKind::OkOrPanic);
        assert_eq!(result, card);
    }

    #[test]
    fn test_filter_map_cardinality() {
        let card = Symbolic::Constant(BigUint::from(100u64));
        let result = propagate_cardinality(card, &StreamFunctionKind::FilterMap);
        assert!(matches!(result, Symbolic::Filtered(_)));
    }

    #[test]
    fn test_pwr_space() {
        let result = space_for_kind(&StreamFunctionKind::PermutationsWithReplacement(3));
        assert_eq!(result, ComplexityClass::ON);
    }

    #[test]
    fn test_fold_space() {
        let result = space_for_kind(&StreamFunctionKind::Fold);
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_map_space() {
        let result = space_for_kind(&StreamFunctionKind::Map);
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_ok_space() {
        let result = space_for_kind(&StreamFunctionKind::Ok);
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_ok_or_panic_space() {
        let result = space_for_kind(&StreamFunctionKind::OkOrPanic);
        assert_eq!(result, ComplexityClass::O1);
    }

    #[test]
    fn test_semantics_table_covers_all_stream_function_kinds() {
        // Construct every variant of StreamFunctionKind and exercise all
        // fields of the returned StreamFunctionSemantics.  This ensures the
        // exhaustive match compiles and every closure is callable.
        let all_kinds = vec![
            StreamFunctionKind::Map,
            StreamFunctionKind::Filter,
            StreamFunctionKind::FilterMap,
            StreamFunctionKind::Permutations(3),
            StreamFunctionKind::PermutationsWithReplacement(3),
            StreamFunctionKind::Combinations(2),
            StreamFunctionKind::KeepFirstN(5),
            StreamFunctionKind::Fold,
            StreamFunctionKind::Ok,
            StreamFunctionKind::OkOrPanic,
        ];

        let card = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));

        for kind in &all_kinds {
            let sem = semantics_for(kind);

            // cardinality_transform must return a valid Symbolic
            let new_card = (sem.cardinality_transform)(card.clone());
            assert!(
                !format!("{new_card}").is_empty(),
                "cardinality_transform returned empty display for {kind:?}"
            );

            // time_class must return a valid ComplexityClass
            let _tc = (sem.time_class)(&card);

            // space_class must return a valid ComplexityClass
            let _sc = (sem.space_class)(&card);

            // description must return a non-empty string
            let desc = (sem.description)();
            assert!(
                !desc.is_empty(),
                "description returned empty string for {kind:?}"
            );

            // collects_input is just a bool -- touch it to prove the field exists
            let _ = sem.collects_input;
        }
    }

    #[test]
    fn test_collects_input_implies_non_o1_space() {
        // Cross-field consistency: if a kind collects input then its space
        // class must NOT be O(1) (it needs at least O(n) to buffer).
        let all_kinds = vec![
            StreamFunctionKind::Map,
            StreamFunctionKind::Filter,
            StreamFunctionKind::FilterMap,
            StreamFunctionKind::Permutations(3),
            StreamFunctionKind::PermutationsWithReplacement(3),
            StreamFunctionKind::Combinations(2),
            StreamFunctionKind::KeepFirstN(5),
            StreamFunctionKind::Fold,
            StreamFunctionKind::Ok,
            StreamFunctionKind::OkOrPanic,
        ];

        let card = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(10u64))));

        for kind in &all_kinds {
            let sem = semantics_for(kind);
            if sem.collects_input {
                let sc = (sem.space_class)(&card);
                assert_ne!(
                    sc,
                    ComplexityClass::O1,
                    "kind {kind:?} collects_input but has O(1) space"
                );
            }
        }
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
            Just(ComplexityClass::ONLogN),
            Just(ComplexityClass::OFactorial),
            Just(ComplexityClass::Unknown),
            (2u64..10u64).prop_map(ComplexityClass::ONLogK),
            (2u64..10u64).prop_map(ComplexityClass::OPolynomial),
            (2u64..10u64).prop_map(ComplexityClass::OCombinatorial),
            (2u64..10u64).prop_map(ComplexityClass::OPermutational),
        ]
    }

    fn arb_biguint() -> impl Strategy<Value = BigUint> {
        (0u64..1_000_000u64).prop_map(BigUint::from)
    }

    fn arb_symbolic() -> impl Strategy<Value = Symbolic> {
        let leaf = prop_oneof![
            arb_biguint().prop_map(Symbolic::Constant),
            Just(Symbolic::Unknown),
        ];
        leaf.prop_recursive(4, 16, 4, |inner| {
            prop_oneof![
                inner.clone().prop_map(|s| Symbolic::Filtered(Box::new(s))),
                (inner.clone(), 1u64..5u64)
                    .prop_map(|(n, k)| Symbolic::Permutations { n: Box::new(n), k }),
                (inner.clone(), 1u64..5u64).prop_map(|(n, k)| {
                    Symbolic::PermutationsWithReplacement { n: Box::new(n), k }
                }),
                (inner.clone(), 1u64..5u64)
                    .prop_map(|(n, k)| Symbolic::Combinations { n: Box::new(n), k }),
                (inner.clone(), inner.clone())
                    .prop_map(|(a, b)| { Symbolic::Min(Box::new(a), Box::new(b)) }),
                proptest::collection::vec(inner, 2..5).prop_map(Symbolic::Sum),
            ]
        })
    }

    proptest! {
        #[test]
        fn test_complexity_class_ord_total(a in arb_complexity_class(), b in arb_complexity_class()) {
            let _ = a.cmp(&b);
        }

        #[test]
        fn test_complexity_class_ord_antisymmetric(a in arb_complexity_class(), b in arb_complexity_class()) {
            if a != b {
                assert_eq!(a.cmp(&b), b.cmp(&a).reverse());
            }
        }

        #[test]
        fn test_exact_complexity_merge_commutative(mut a in any::<u64>().prop_map(|n| {
            let mut ec = ExactComplexity::new();
            ec.add_work(ComplexityClass::ON, n % 10);
            ec
        }), b in any::<u64>().prop_map(|n| {
            let mut ec = ExactComplexity::new();
            ec.add_work(ComplexityClass::O1, n % 10);
            ec
        })) {
            let mut a2 = a.clone();
            a.merge(&b);
            let mut b2 = b.clone();
            b2.merge(&a2);
            // After merging, the simplified complexities should agree
            assert_eq!(a.simplified(), b2.simplified());
        }

        #[test]
        fn test_symbolic_fromstr_roundtrip(sym in arb_symbolic()) {
            let s = sym.to_string();
            let parsed = Symbolic::from_str(&s).unwrap();
            prop_assert_eq!(sym, parsed);
        }
    }
}
