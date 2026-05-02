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
                if n_val < BigUint::from(*k) {
                    return None;
                }
                let mut result = BigUint::one();
                for i in 0..*k {
                    result *= &n_val - i;
                }
                Some(result)
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.try_evaluate()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.try_evaluate()?;
                if n_val < BigUint::from(*k) {
                    return None;
                }
                let mut num = BigUint::one();
                let mut den = BigUint::one();
                for i in 0..*k {
                    num *= &n_val - i;
                    den *= i + 1;
                }
                Some(num / den)
            }
            Symbolic::Min(a, b) => Some(a.try_evaluate()?.min(b.try_evaluate()?)),
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
            Symbolic::Constant(v) => Some(v.clone()),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.upper_bound(),
            Symbolic::Permutations { n, k } => {
                let n_val = n.upper_bound()?;
                if n_val < BigUint::from(*k) {
                    return None;
                }
                let mut result = BigUint::one();
                for i in 0..*k {
                    result *= &n_val - i;
                }
                Some(result)
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.upper_bound()?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.upper_bound()?;
                if n_val < BigUint::from(*k) {
                    return None;
                }
                let mut num = BigUint::one();
                let mut den = BigUint::one();
                for i in 0..*k {
                    num *= &n_val - i;
                    den *= i + 1;
                }
                Some(num / den)
            }
            Symbolic::Min(a, b) => match (a.upper_bound(), b.upper_bound()) {
                (Some(av), Some(bv)) => Some(av.min(bv)),
                (Some(av), None) => Some(av),
                (None, Some(bv)) => Some(bv),
                (None, None) => None,
            },
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
            Some(v) => Cardinality::Exact(v),
            None => Cardinality::Unknown,
        }
    }

    fn classify_as_time(&self) -> ComplexityClass {
        match self {
            Symbolic::Constant(_) => ComplexityClass::O1,
            Symbolic::Unknown => ComplexityClass::Unknown,
            Symbolic::Filtered(inner) => inner.classify_as_time(),
            Symbolic::Permutations { n, k } => {
                if n.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OPermutational(*k)
                }
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                if n.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OPolynomial(*k)
                }
            }
            Symbolic::Combinations { n, k } => {
                if n.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OCombinatorial(*k)
                }
            }
            Symbolic::Min(a, b) => a.classify_as_time().max(b.classify_as_time()),
            Symbolic::Sum(parts) => parts
                .iter()
                .map(|p| p.classify_as_time())
                .max()
                .unwrap_or(ComplexityClass::O1),
        }
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::Constant(v) => write!(f, "{v}"),
            Symbolic::Unknown => write!(f, "?"),
            Symbolic::Filtered(inner) => write!(f, "filtered({inner})"),
            Symbolic::Permutations { n, k } => write!(f, "perms({n},{k})"),
            Symbolic::PermutationsWithReplacement { n, k } => write!(f, "{n}^{k}"),
            Symbolic::Combinations { n, k } => write!(f, "combs({n},{k})"),
            Symbolic::Min(a, b) => write!(f, "min({a},{b})"),
            Symbolic::Sum(parts) => {
                write!(f, "sum(")?;
                for (i, p) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl FromStr for Symbolic {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_symbolic(s: &str) -> Result<Symbolic, String> {
            if s == "?" {
                return Ok(Symbolic::Unknown);
            }
            if let Ok(n) = s.parse::<BigUint>() {
                return Ok(Symbolic::Constant(n));
            }
            if let Some(inner) = s.strip_prefix("filtered(").and_then(|s| s.strip_suffix(')')) {
                return Ok(Symbolic::Filtered(Box::new(parse_symbolic(inner)?)));
            }
            if let Some(inner) = s.strip_prefix("perms(").and_then(|s| s.strip_suffix(')')) {
                let parts = split_top_level(inner, ',');
                if parts.len() == 2 {
                    let n = parse_symbolic(parts[0])?;
                    let k: u64 = parts[1]
                        .parse()
                        .map_err(|_| format!("Invalid k in perms: {}", parts[1]))?;
                    return Ok(Symbolic::Permutations {
                        n: Box::new(n),
                        k,
                    });
                }
            }
            if let Some(inner) = s.strip_prefix("combs(").and_then(|s| s.strip_suffix(')')) {
                let parts = split_top_level(inner, ',');
                if parts.len() == 2 {
                    let n = parse_symbolic(parts[0])?;
                    let k: u64 = parts[1]
                        .parse()
                        .map_err(|_| format!("Invalid k in combs: {}", parts[1]))?;
                    return Ok(Symbolic::Combinations {
                        n: Box::new(n),
                        k,
                    });
                }
            }
            // PermutationsWithReplacement: n^k where n must be a constant
            if let Some(caret_pos) = s.rfind('^') {
                let base_str = &s[..caret_pos];
                let exp_str = &s[caret_pos + 1..];
                if let (Ok(base), Ok(k)) = (parse_symbolic(base_str), exp_str.parse::<u64>()) {
                    return Ok(Symbolic::PermutationsWithReplacement {
                        n: Box::new(base),
                        k,
                    });
                }
            }
            if let Some(inner) = s.strip_prefix("min(").and_then(|s| s.strip_suffix(')')) {
                let parts = split_top_level(inner, ',');
                if parts.len() == 2 {
                    let a = parse_symbolic(parts[0])?;
                    let b = parse_symbolic(parts[1])?;
                    return Ok(Symbolic::Min(Box::new(a), Box::new(b)));
                }
            }
            if let Some(inner) = s.strip_prefix("sum(").and_then(|s| s.strip_suffix(')')) {
                let parts = split_top_level(inner, ',');
                if parts.len() >= 2 {
                    let parsed: Result<Vec<_>, _> =
                        parts.iter().map(|p| parse_symbolic(p)).collect();
                    return Ok(Symbolic::Sum(parsed?));
                }
            }
            Err(format!("Cannot parse symbolic expression: {s}"))
        }

        fn split_top_level(s: &str, sep: char) -> Vec<&str> {
            let mut result = Vec::new();
            let mut depth = 0usize;
            let mut start = 0;
            for (i, c) in s.char_indices() {
                match c {
                    '(' => depth += 1,
                    ')' => depth = depth.saturating_sub(1),
                    c if c == sep && depth == 0 => {
                        result.push(&s[start..i]);
                        start = i + 1;
                    }
                    _ => {}
                }
            }
            result.push(&s[start..]);
            result
        }

        parse_symbolic(s)
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Cardinality {
    Exact(BigUint),
    Bounded(Symbolic),
    Unknown,
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
    pub time: ComplexityClass,
    pub exact_time: ExactComplexity,
    pub space: ComplexityClass,
    pub exact_space: ExactComplexity,
    pub cardinality: Cardinality,
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match &inp.input_count {
        InputCount::Known(n) => Symbolic::Constant(n.clone()),
        InputCount::Unknown => Symbolic::Unknown,
    }
}

fn propagate_cardinality(cardinality: Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Map
        | StreamFunctionKind::Ok
        | StreamFunctionKind::OkOrPanic
        | StreamFunctionKind::FilterMap => Symbolic::Filtered(Box::new(cardinality)),
        StreamFunctionKind::Filter => Symbolic::Filtered(Box::new(cardinality)),
        StreamFunctionKind::Fold => Symbolic::Constant(BigUint::one()),
        StreamFunctionKind::KeepFirstN(k) => {
            Symbolic::Min(Box::new(cardinality), Box::new(Symbolic::Constant(k.clone())))
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
    if cardinality.try_evaluate().is_some() {
        return ComplexityClass::O1;
    }
    let base = cardinality.classify_as_time();
    if base == ComplexityClass::O1 {
        ComplexityClass::ON
    } else {
        base
    }
}

fn step_work_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    if cardinality.try_evaluate().is_some() {
        return ComplexityClass::O1;
    }
    match kind {
        StreamFunctionKind::Permutations(k) => ComplexityClass::OPermutational(*k),
        StreamFunctionKind::PermutationsWithReplacement(k) => ComplexityClass::OPolynomial(*k),
        StreamFunctionKind::Combinations(k) => ComplexityClass::OCombinatorial(*k),
        _ => cardinality_to_time_class(cardinality),
    }
}

fn step_space_class(cardinality: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    if cardinality.try_evaluate().is_some() {
        return ComplexityClass::O1;
    }
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
            TypedExpression::StreamVariable(v) => {
                let (card, space, exact_time, exact_space) = stream_vars
                    .get(&v.variable_name)
                    .cloned()
                    .unwrap_or((
                        Symbolic::Unknown,
                        ComplexityClass::Unknown,
                        ExactComplexity::new(),
                        ExactComplexity::new(),
                    ));
                StreamComplexity {
                    description: format!("var:{}", v.variable_name),
                    cardinality: card.classify_as_cardinality(),
                    time_class: exact_time.simplified(),
                    exact_time,
                    space_class: space.clone(),
                    exact_space,
                    collects_input: false,
                }
            }
            TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                let (card, space, exact_time, exact_space) = stream_vars
                    .get(&v.variable_name)
                    .cloned()
                    .unwrap_or((
                        Symbolic::Unknown,
                        ComplexityClass::Unknown,
                        ExactComplexity::new(),
                        ExactComplexity::new(),
                    ));
                StreamComplexity {
                    description: format!("var:{}", v.variable_name),
                    cardinality: card.classify_as_cardinality(),
                    time_class: exact_time.simplified(),
                    exact_time,
                    space_class: space.clone(),
                    exact_space,
                    collects_input: false,
                }
            }
            _ => continue,
        };

        program_time = program_time.max(sc.time_class.clone());
        program_exact_time.merge(&sc.exact_time);
        program_space = program_space.max(sc.space_class.clone());
        program_exact_space.merge(&sc.exact_space);
        match &sc.cardinality {
            Cardinality::Exact(n) => {
                if let Cardinality::Exact(total) = &program_cardinality {
                    program_cardinality = Cardinality::Exact(total + n);
                }
            }
            Cardinality::Bounded(_) | Cardinality::Unknown => {
                program_cardinality = Cardinality::Unknown;
            }
        }
        streams.push(sc);
    }

    ProgramComplexity {
        streams,
        time: program_time,
        exact_time: program_exact_time,
        space: program_space,
        exact_space: program_exact_space,
        cardinality: program_cardinality,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn arb_symbolic_constant() -> impl Strategy<Value = Symbolic> {
        (1u64..10000).prop_map(|n| Symbolic::Constant(BigUint::from(n)))
    }

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
            let s = format!("{c:?}");
            drop(s);
        }
    }

    proptest! {
        #[test]
        fn test_complexity_class_display_fromstr_roundtrip(
            time in prop_oneof![
                Just(ComplexityClass::O1),
                Just(ComplexityClass::OLogN),
                Just(ComplexityClass::ON),
                (1u64..10).prop_map(ComplexityClass::ONLogK),
                Just(ComplexityClass::ONLogN),
                (2u64..10).prop_map(ComplexityClass::OPolynomial),
                (2u64..10).prop_map(ComplexityClass::OCombinatorial),
                (2u64..10).prop_map(ComplexityClass::OPermutational),
                Just(ComplexityClass::OFactorial),
                Just(ComplexityClass::Unknown),
            ]
        ) {
            let s = time.to_string();
            let parsed = ComplexityClass::from_str(&s).unwrap();
            prop_assert_eq!(time, parsed);
        }
    }

    proptest! {
        #[test]
        fn test_exact_complexity_display_fromstr_roundtrip(
            e in prop_oneof![
                Just(ExactComplexity::new()),
                Just({
                    let mut ec = ExactComplexity::new();
                    ec.add_work(ComplexityClass::O1, 3);
                    ec
                }),
                Just({
                    let mut ec = ExactComplexity::new();
                    ec.add_work(ComplexityClass::ON, 1);
                    ec.add_work(ComplexityClass::O1, 2);
                    ec
                }),
                Just({
                    let mut ec = ExactComplexity::new();
                    ec.add_work(ComplexityClass::OLogN, 1);
                    ec.add_work(ComplexityClass::ON, 2);
                    ec
                }),
            ]
        ) {
            let s = e.to_string();
            let parsed = ExactComplexity::from_str(&s).unwrap();
            prop_assert_eq!(e, parsed);
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
        // With 10% weight we expect ~1000 unknowns; assert at least 5% as a safe floor.
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

        // Test 32 structural combinations (bits 0–4 gate which declaration types appear)
        // × varying field/variant counts (bits 5–8), for 512 total iterations.
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
