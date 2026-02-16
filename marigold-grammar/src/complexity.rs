#![allow(clippy::enum_variant_names)]

use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

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
            let n_str = inner
                .next()
                .ok_or_else(|| "Missing perm_rep n".to_string())?
                .as_str();
            let k_str = inner
                .next()
                .ok_or_else(|| "Missing perm_rep k".to_string())?
                .as_str();
            let n: BigUint = n_str
                .parse()
                .map_err(|_| "Invalid perm_rep n value".to_string())?;
            let k: u64 = k_str
                .parse()
                .map_err(|_| "Invalid perm_rep k value".to_string())?;
            Ok(Symbolic::PermutationsWithReplacement {
                n: Box::new(Symbolic::Constant(n)),
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
                .map_err(|_| "Invalid constant".to_string())?;
            Ok(Symbolic::Constant(v))
        }
        Rule::symbolic_unknown => Ok(Symbolic::Unknown),
        _ => Err(format!("Unexpected rule: {:?}", pair.as_rule())),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cardinality {
    Exact(BigUint),
    Bounded(Symbolic),
    Unknown,
}

impl Cardinality {
    fn ordinal(&self) -> u8 {
        match self {
            Cardinality::Exact(_) => 0,
            Cardinality::Bounded(_) => 1,
            Cardinality::Unknown => 2,
        }
    }

    pub fn max(self, other: Cardinality) -> Cardinality {
        match (&self, &other) {
            (Cardinality::Exact(a), Cardinality::Exact(b)) => {
                Cardinality::Exact(a.clone().max(b.clone()))
            }
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            (Cardinality::Bounded(a), Cardinality::Bounded(b)) => {
                match (a.upper_bound(), b.upper_bound()) {
                    (Some(av), Some(bv)) => {
                        if av >= bv {
                            self
                        } else {
                            other
                        }
                    }
                    _ => self,
                }
            }
            (Cardinality::Bounded(_), Cardinality::Exact(v)) => match self.clone() {
                Cardinality::Bounded(sym) => match sym.upper_bound() {
                    Some(ub) if ub >= *v => Cardinality::Bounded(sym),
                    _ => Cardinality::Bounded(sym),
                },
                _ => unreachable!(),
            },
            (Cardinality::Exact(v), Cardinality::Bounded(_)) => match other.clone() {
                Cardinality::Bounded(sym) => match sym.upper_bound() {
                    Some(ub) if ub >= *v => Cardinality::Bounded(sym),
                    _ => Cardinality::Bounded(sym),
                },
                _ => unreachable!(),
            },
        }
    }
}

impl PartialOrd for Cardinality {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cardinality {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ordinal().cmp(&other.ordinal())
    }
}

impl fmt::Display for Cardinality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cardinality::Exact(n) => write!(f, "{n}"),
            Cardinality::Bounded(sym) => write!(f, "{sym}"),
            Cardinality::Unknown => write!(f, "?"),
        }
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
    pub cardinality: Cardinality,
    pub time_class: ComplexityClass,
    pub exact_time: ExactComplexity,
    pub space_class: ComplexityClass,
    pub exact_space: ExactComplexity,
    pub collects_input: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub streams: Vec<StreamComplexity>,
    pub program_time: ComplexityClass,
    pub program_exact_time: ExactComplexity,
    pub program_space: ComplexityClass,
    pub program_exact_space: ExactComplexity,
    pub program_cardinality: Cardinality,
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

#[cfg(test)]
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
    let is_concrete = cardinality.try_evaluate().is_some();
    match kind {
        StreamFunctionKind::Permutations(k) => {
            if is_concrete {
                ComplexityClass::O1
            } else {
                ComplexityClass::OPermutational(*k)
            }
        }
        StreamFunctionKind::PermutationsWithReplacement(k) => {
            if is_concrete {
                ComplexityClass::O1
            } else {
                ComplexityClass::OPolynomial(*k)
            }
        }
        StreamFunctionKind::Combinations(k) => {
            if is_concrete {
                ComplexityClass::O1
            } else {
                ComplexityClass::OCombinatorial(*k)
            }
        }
        _ => cardinality_to_time_class(cardinality),
    }
}

fn step_space_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    match kind {
        StreamFunctionKind::Permutations(_)
        | StreamFunctionKind::PermutationsWithReplacement(_)
        | StreamFunctionKind::Combinations(_) => cardinality_to_time_class(cardinality),
        _ => ComplexityClass::O1,
    }
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
    let mut program_time = ComplexityClass::O1;
    let mut program_exact_time = ExactComplexity::new();
    let mut program_space = ComplexityClass::O1;
    let mut program_exact_space = ExactComplexity::new();
    let mut program_cardinality = Cardinality::Exact(BigUint::zero());

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

        program_time = program_time.max(sc.time_class.clone());
        program_exact_time.merge(&sc.exact_time);
        program_space = program_space.max(sc.space_class.clone());
        program_exact_space.merge(&sc.exact_space);
        program_cardinality = program_cardinality.max(sc.cardinality.clone());
        streams.push(sc);
    }

    ProgramComplexity {
        streams,
        program_time,
        program_exact_time,
        program_space,
        program_exact_space,
        program_cardinality,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_o1() {
        assert_eq!(
            ComplexityClass::from_str("O(1)").unwrap(),
            ComplexityClass::O1
        );
    }

    #[test]
    fn test_parse_on() {
        assert_eq!(
            ComplexityClass::from_str("O(n)").unwrap(),
            ComplexityClass::ON
        );
    }

    #[test]
    fn test_parse_ologn() {
        assert_eq!(
            ComplexityClass::from_str("O(log(n))").unwrap(),
            ComplexityClass::OLogN
        );
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
        assert_eq!(
            space_for_kind(&StreamFunctionKind::Map),
            ComplexityClass::O1
        );
        assert_eq!(
            space_for_kind(&StreamFunctionKind::Filter),
            ComplexityClass::O1
        );
        assert_eq!(
            space_for_kind(&StreamFunctionKind::FilterMap),
            ComplexityClass::O1
        );
        assert_eq!(
            space_for_kind(&StreamFunctionKind::Fold),
            ComplexityClass::O1
        );
        assert_eq!(space_for_kind(&StreamFunctionKind::Ok), ComplexityClass::O1);
        assert_eq!(
            space_for_kind(&StreamFunctionKind::OkOrPanic),
            ComplexityClass::O1
        );
    }

    #[test]
    fn test_permutations_space_on() {
        assert_eq!(
            space_for_kind(&StreamFunctionKind::Permutations(3)),
            ComplexityClass::ON
        );
    }

    #[test]
    fn test_combinations_space_on() {
        assert_eq!(
            space_for_kind(&StreamFunctionKind::Combinations(2)),
            ComplexityClass::ON
        );
    }

    #[test]
    fn test_keep_first_n_space_o1() {
        assert_eq!(
            space_for_kind(&StreamFunctionKind::KeepFirstN(5)),
            ComplexityClass::O1
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
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::one())
        );
        assert_eq!(result.streams[0].time_class, ComplexityClass::ON);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    }

    #[test]
    fn test_analyze_select_all() {
        let result =
            crate::parser::PestParser::analyze("select_all(range(0, 10), range(0, 20)).return")
                .unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(
            result.streams[0].cardinality,
            Cardinality::Exact(BigUint::from(30u64))
        );
    }

    #[test]
    fn test_analyze_stream_variable() {
        let input =
            "fn double(x: i32) -> i32 { x * 2 }\ndigits = range(0, 10)\ndigits.map(double).return";
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

    #[test]
    fn test_exact_complexity_new_is_empty() {
        let ec = ExactComplexity::new();
        assert!(ec.is_empty());
        assert_eq!(ec.simplified(), ComplexityClass::O1);
    }

    #[test]
    fn test_exact_complexity_add_work() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        assert!(!ec.is_empty());
        assert_eq!(ec.simplified(), ComplexityClass::ON);
    }

    #[test]
    fn test_exact_complexity_accumulates() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        ec.add_work(ComplexityClass::ON, 1);
        assert_eq!(ec.to_string(), "O(2n)");
    }

    #[test]
    fn test_exact_complexity_multiple_tiers() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::OPermutational(2), 1);
        ec.add_work(ComplexityClass::ON, 2);
        assert_eq!(ec.to_string(), "O(n!/(n-2)! + 2n)");
        assert_eq!(ec.simplified(), ComplexityClass::OPermutational(2));
    }

    #[test]
    fn test_exact_complexity_merge() {
        let mut a = ExactComplexity::new();
        a.add_work(ComplexityClass::ON, 2);
        let mut b = ExactComplexity::new();
        b.add_work(ComplexityClass::ON, 1);
        b.add_work(ComplexityClass::OPolynomial(2), 1);
        a.merge(&b);
        assert_eq!(a.to_string(), "O(n^2 + 3n)");
    }

    #[test]
    fn test_exact_complexity_display_single_coeff_1() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 1);
        assert_eq!(ec.to_string(), "O(n)");
    }

    #[test]
    fn test_exact_complexity_display_o1_only() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::O1, 3);
        assert_eq!(ec.to_string(), "O(3)");
    }

    #[test]
    fn test_exact_complexity_display_omits_o1_when_higher_exists() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::ON, 2);
        ec.add_work(ComplexityClass::O1, 1);
        assert_eq!(ec.to_string(), "O(2n)");
    }

    #[test]
    fn test_exact_complexity_display_empty_is_o1() {
        let ec = ExactComplexity::new();
        assert_eq!(ec.to_string(), "O(1)");
    }

    #[test]
    fn test_exact_complexity_fromstr_simple() {
        let ec = ExactComplexity::from_str("O(n)").unwrap();
        assert_eq!(ec.simplified(), ComplexityClass::ON);
        assert_eq!(ec.to_string(), "O(n)");
    }

    #[test]
    fn test_exact_complexity_fromstr_with_coefficient() {
        let ec = ExactComplexity::from_str("O(2n)").unwrap();
        assert_eq!(ec.to_string(), "O(2n)");
    }

    #[test]
    fn test_exact_complexity_fromstr_multi_term() {
        let ec = ExactComplexity::from_str("O(n^2 + 3n)").unwrap();
        assert_eq!(ec.simplified(), ComplexityClass::OPolynomial(2));
        assert_eq!(ec.to_string(), "O(n^2 + 3n)");
    }

    #[test]
    fn test_exact_complexity_fromstr_permutational() {
        let ec = ExactComplexity::from_str("O(2n!/(n-2)! + n)").unwrap();
        assert_eq!(ec.simplified(), ComplexityClass::OPermutational(2));
        assert_eq!(ec.to_string(), "O(2n!/(n-2)! + n)");
    }

    #[test]
    fn test_exact_complexity_serde_roundtrip() {
        let mut ec = ExactComplexity::new();
        ec.add_work(ComplexityClass::OPermutational(2), 1);
        ec.add_work(ComplexityClass::ON, 3);
        let json = serde_json::to_string(&ec).unwrap();
        let parsed: ExactComplexity = serde_json::from_str(&json).unwrap();
        assert_eq!(ec, parsed);
    }

    #[test]
    fn test_cardinality_display_exact() {
        let c = Cardinality::Exact(BigUint::from(100u64));
        assert_eq!(c.to_string(), "100");
    }

    #[test]
    fn test_cardinality_display_unknown() {
        assert_eq!(Cardinality::Unknown.to_string(), "?");
    }

    #[test]
    fn test_cardinality_display_bounded() {
        let c = Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
            BigUint::from(100u64),
        ))));
        assert_eq!(c.to_string(), "\u{2264}100");
    }

    #[test]
    fn test_cardinality_fromstr_exact() {
        assert_eq!(
            Cardinality::from_str("100").unwrap(),
            Cardinality::Exact(BigUint::from(100u64))
        );
    }

    #[test]
    fn test_cardinality_fromstr_unknown() {
        assert_eq!(Cardinality::from_str("?").unwrap(), Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_fromstr_bounded() {
        let c = Cardinality::from_str("\u{2264}100").unwrap();
        assert!(matches!(c, Cardinality::Bounded(_)));
    }

    #[test]
    fn test_cardinality_ordering() {
        assert!(Cardinality::Exact(BigUint::from(100u64)) < Cardinality::Unknown);
        assert!(
            Cardinality::Exact(BigUint::from(100u64))
                < Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
                    BigUint::from(100u64)
                ))))
        );
        assert!(
            Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
                BigUint::from(100u64)
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
    fn test_symbolic_upper_bound_constant() {
        let s = Symbolic::Constant(BigUint::from(100u64));
        assert_eq!(s.upper_bound(), Some(BigUint::from(100u64)));
    }

    #[test]
    fn test_symbolic_upper_bound_filtered() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert_eq!(s.upper_bound(), Some(BigUint::from(100u64)));
    }

    #[test]
    fn test_symbolic_upper_bound_nested_filtered() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Filtered(Box::new(Symbolic::Constant(
            BigUint::from(100u64),
        )))));
        assert_eq!(s.upper_bound(), Some(BigUint::from(100u64)));
    }

    #[test]
    fn test_symbolic_upper_bound_min_with_filtered() {
        let s = Symbolic::Min(
            Box::new(Symbolic::Filtered(Box::new(Symbolic::Constant(
                BigUint::from(100u64),
            )))),
            Box::new(Symbolic::Constant(BigUint::from(5u64))),
        );
        assert_eq!(s.upper_bound(), Some(BigUint::from(5u64)));
    }

    #[test]
    fn test_symbolic_upper_bound_unknown() {
        assert_eq!(Symbolic::Unknown.upper_bound(), None);
    }

    #[test]
    fn test_symbolic_contains_unknown_false() {
        assert!(!Symbolic::Constant(BigUint::from(100u64)).contains_unknown());
    }

    #[test]
    fn test_symbolic_contains_unknown_true() {
        assert!(Symbolic::Unknown.contains_unknown());
    }

    #[test]
    fn test_symbolic_contains_unknown_nested() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Unknown));
        assert!(s.contains_unknown());
    }

    #[test]
    fn test_classify_as_cardinality_exact() {
        let s = Symbolic::Constant(BigUint::from(100u64));
        assert_eq!(
            s.classify_as_cardinality(),
            Cardinality::Exact(BigUint::from(100u64))
        );
    }

    #[test]
    fn test_classify_as_cardinality_exact_evaluable() {
        let s = Symbolic::Permutations {
            n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
            k: 2,
        };
        assert_eq!(
            s.classify_as_cardinality(),
            Cardinality::Exact(BigUint::from(9900u64))
        );
    }

    #[test]
    fn test_classify_as_cardinality_bounded() {
        let s = Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))));
        assert!(matches!(
            s.classify_as_cardinality(),
            Cardinality::Bounded(_)
        ));
    }

    #[test]
    fn test_classify_as_cardinality_unknown() {
        assert_eq!(
            Symbolic::Unknown.classify_as_cardinality(),
            Cardinality::Unknown
        );
    }

    #[test]
    fn test_cardinality_max_exact_exact() {
        let a = Cardinality::Exact(BigUint::from(10u64));
        let b = Cardinality::Exact(BigUint::from(20u64));
        assert_eq!(a.max(b), Cardinality::Exact(BigUint::from(20u64)));
    }

    #[test]
    fn test_cardinality_max_exact_unknown() {
        let a = Cardinality::Exact(BigUint::from(10u64));
        let b = Cardinality::Unknown;
        assert_eq!(a.max(b), Cardinality::Unknown);
    }

    #[test]
    fn test_cardinality_max_bounded_exact() {
        let a = Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
            BigUint::from(100u64),
        ))));
        let b = Cardinality::Exact(BigUint::from(50u64));
        assert!(matches!(a.max(b), Cardinality::Bounded(_)));
    }

    #[test]
    fn test_symbolic_fromstr_constant() {
        let s = Symbolic::from_str("100").unwrap();
        assert_eq!(s, Symbolic::Constant(BigUint::from(100u64)));
    }

    #[test]
    fn test_symbolic_fromstr_unknown() {
        let s = Symbolic::from_str("?").unwrap();
        assert_eq!(s, Symbolic::Unknown);
    }

    #[test]
    fn test_symbolic_fromstr_filtered() {
        let s = Symbolic::from_str("\u{2264}100").unwrap();
        assert_eq!(
            s,
            Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64))))
        );
    }

    #[test]
    fn test_symbolic_fromstr_permutations() {
        let s = Symbolic::from_str("P(100, 2)").unwrap();
        assert_eq!(
            s,
            Symbolic::Permutations {
                n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
                k: 2
            }
        );
    }

    #[test]
    fn test_symbolic_fromstr_combinations() {
        let s = Symbolic::from_str("C(100, 2)").unwrap();
        assert_eq!(
            s,
            Symbolic::Combinations {
                n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
                k: 2
            }
        );
    }

    #[test]
    fn test_symbolic_fromstr_perm_rep() {
        let s = Symbolic::from_str("100^3").unwrap();
        assert_eq!(
            s,
            Symbolic::PermutationsWithReplacement {
                n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
                k: 3
            }
        );
    }

    #[test]
    fn test_symbolic_fromstr_min() {
        let s = Symbolic::from_str("min(100, 5)").unwrap();
        assert_eq!(
            s,
            Symbolic::Min(
                Box::new(Symbolic::Constant(BigUint::from(100u64))),
                Box::new(Symbolic::Constant(BigUint::from(5u64)))
            )
        );
    }

    #[test]
    fn test_symbolic_fromstr_sum() {
        let s = Symbolic::from_str("10 + 20").unwrap();
        assert_eq!(
            s,
            Symbolic::Sum(vec![
                Symbolic::Constant(BigUint::from(10u64)),
                Symbolic::Constant(BigUint::from(20u64))
            ])
        );
    }

    #[test]
    fn test_symbolic_display_roundtrip() {
        let cases = vec![
            Symbolic::Constant(BigUint::from(100u64)),
            Symbolic::Unknown,
            Symbolic::Filtered(Box::new(Symbolic::Constant(BigUint::from(100u64)))),
            Symbolic::Permutations {
                n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
                k: 2,
            },
            Symbolic::Combinations {
                n: Box::new(Symbolic::Constant(BigUint::from(100u64))),
                k: 3,
            },
            Symbolic::PermutationsWithReplacement {
                n: Box::new(Symbolic::Constant(BigUint::from(10u64))),
                k: 3,
            },
            Symbolic::Min(
                Box::new(Symbolic::Constant(BigUint::from(100u64))),
                Box::new(Symbolic::Constant(BigUint::from(5u64))),
            ),
            Symbolic::Sum(vec![
                Symbolic::Constant(BigUint::from(10u64)),
                Symbolic::Constant(BigUint::from(20u64)),
            ]),
        ];
        for sym in cases {
            let s = sym.to_string();
            let parsed = Symbolic::from_str(&s).unwrap();
            assert_eq!(sym, parsed, "Roundtrip failed for {s}");
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
}
