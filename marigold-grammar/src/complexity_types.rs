#![allow(clippy::enum_variant_names)]

use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;

use num_bigint::BigUint;
use num_traits::{One, Zero};
use pest::Parser;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(pest_derive::Parser)]
#[grammar = "complexity_notation.pest"]
struct ComplexityNotationParser;

// ---------------------------------------------------------------------------
// ComplexityClass
// ---------------------------------------------------------------------------

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
            ComplexityClass::Unknown => write!(f, "O(?)" ),
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
    pub(crate) fn fmt_inner(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

// ---------------------------------------------------------------------------
// ExactComplexity
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExactComplexity {
    pub(crate) terms: BTreeMap<ComplexityClass, u64>,
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

// ---------------------------------------------------------------------------
// Symbolic
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Cardinality
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Arithmetic helpers (used by Symbolic)
// ---------------------------------------------------------------------------

pub(crate) fn falling_factorial(n: &BigUint, k: u64) -> BigUint {
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

pub(crate) fn binomial(n: &BigUint, k: u64) -> BigUint {
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

// ---------------------------------------------------------------------------
// Top-level program complexity structs
// ---------------------------------------------------------------------------

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
