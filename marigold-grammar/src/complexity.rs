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
use crate::sizes::{Resolution, StageResolution};

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

/// The name of an input size: an ASCII letter or underscore followed by ASCII letters, digits or
/// underscores. It prints as `$name` inside a [`Symbolic`].
///
/// ```
/// use marigold_grammar::complexity::SizeName;
///
/// assert_eq!(SizeName::new("rows_2").unwrap().as_str(), "rows_2");
/// assert!(SizeName::new("2rows").is_err());
/// assert!("a-b".parse::<SizeName>().is_err());
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SizeName(String);

/// A string that [`SizeName::new`] rejected.
///
/// ```
/// use marigold_grammar::complexity::SizeName;
///
/// let error = SizeName::new("2rows").unwrap_err();
/// assert_eq!(error.name(), "2rows");
/// assert!(error.to_string().starts_with("\"2rows\" is not a valid size name"));
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct InvalidSizeName(String);

impl InvalidSizeName {
    /// The rejected name, as given.
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for InvalidSizeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} is not a valid size name: use an ASCII letter or underscore followed by letters, digits or underscores",
            self.0
        )
    }
}

impl std::error::Error for InvalidSizeName {}

impl SizeName {
    /// Validates `name`, which is given without the `$` sigil.
    ///
    /// ```
    /// use marigold_grammar::complexity::SizeName;
    ///
    /// assert!(SizeName::new("_n").is_ok());
    /// assert!(SizeName::new("").is_err());
    /// assert!(SizeName::new("$n").is_err());
    /// assert!(SizeName::new("n\u{e9}").is_err());
    /// ```
    pub fn new(name: &str) -> Result<SizeName, InvalidSizeName> {
        let mut chars = name.chars();
        let valid = match chars.next() {
            Some(first) if first.is_ascii_alphabetic() || first == '_' => {
                chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            }
            _ => false,
        };
        if valid {
            Ok(SizeName(name.to_string()))
        } else {
            Err(InvalidSizeName(name.to_string()))
        }
    }

    /// The name without the `$` sigil.
    ///
    /// ```
    /// use marigold_grammar::complexity::{SizeName, Symbolic};
    ///
    /// let rows = SizeName::new("rows").unwrap();
    /// assert_eq!(rows.as_str(), "rows");
    /// assert_eq!(Symbolic::Param(rows).to_string(), "$rows");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for SizeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl FromStr for SizeName {
    type Err = InvalidSizeName;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SizeName::new(s)
    }
}

/// Item counts for named sizes, used to evaluate a [`Symbolic`].
///
/// ```
/// use marigold_grammar::complexity::{SizeBindings, SizeName, Symbolic};
/// use num_bigint::BigUint;
///
/// let mut bindings = SizeBindings::new();
/// bindings.insert(SizeName::new("n").unwrap(), BigUint::from(5u32));
/// let permutations: Symbolic = "P($n, 2)".parse().unwrap();
/// assert_eq!(permutations.evaluate(&bindings), Some(BigUint::from(20u32)));
/// ```
pub type SizeBindings = BTreeMap<SizeName, BigUint>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Symbolic {
    Constant(BigUint),
    /// The size of an input bound to this name, printed and parsed as `$name`.
    Param(SizeName),
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
    pub fn try_evaluate(&self) -> Option<BigUint> {
        self.evaluate(&SizeBindings::new())
    }

    /// The exact item count under `bindings`, or `None` when an unbound size or an unknown or
    /// filtered part makes it unknowable.
    ///
    /// ```
    /// use marigold_grammar::complexity::{SizeBindings, SizeName, Symbolic};
    /// use num_bigint::BigUint;
    ///
    /// let capped: Symbolic = "min(C($n, 2), 5)".parse().unwrap();
    /// let at = |n: u32| SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(n))]);
    /// assert_eq!(capped.evaluate(&at(3)), Some(BigUint::from(3u32)));
    /// assert_eq!(capped.evaluate(&at(4)), Some(BigUint::from(5u32)));
    /// assert_eq!(capped.evaluate(&SizeBindings::new()), None);
    ///
    /// let filtered: Symbolic = "\u{2264}$n".parse().unwrap();
    /// assert_eq!(filtered.evaluate(&at(3)), None);
    /// ```
    pub fn evaluate(&self, bindings: &SizeBindings) -> Option<BigUint> {
        match self {
            Symbolic::Constant(v) => Some(v.clone()),
            Symbolic::Param(name) => bindings.get(name).cloned(),
            Symbolic::Unknown => None,
            Symbolic::Filtered(_) => None,
            Symbolic::Permutations { n, k } => {
                let n_val = n.evaluate(bindings)?;
                let k_val = *k;
                Some(falling_factorial(&n_val, k_val))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.evaluate(bindings)?;
                let k_val = *k;
                Some(n_val.pow(k_val as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.evaluate(bindings)?;
                let k_val = *k;
                Some(binomial(&n_val, k_val))
            }
            Symbolic::Min(a, b) => {
                let a_val = a.evaluate(bindings)?;
                let b_val = b.evaluate(bindings)?;
                Some(a_val.min(b_val))
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for part in parts {
                    total += part.evaluate(bindings)?;
                }
                Some(total)
            }
        }
    }

    pub fn classify_as_time(&self) -> ComplexityClass {
        match self {
            Symbolic::Constant(_) | Symbolic::Param(_) => ComplexityClass::O1,
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
            Symbolic::Constant(_) | Symbolic::Param(_) => false,
            Symbolic::Filtered(inner) => inner.contains_unknown(),
            Symbolic::Permutations { n, .. }
            | Symbolic::PermutationsWithReplacement { n, .. }
            | Symbolic::Combinations { n, .. } => n.contains_unknown(),
            Symbolic::Min(a, b) => a.contains_unknown() || b.contains_unknown(),
            Symbolic::Sum(parts) => parts.iter().any(|p| p.contains_unknown()),
        }
    }

    pub fn upper_bound(&self) -> Option<BigUint> {
        self.upper_bound_with(&SizeBindings::new())
    }

    /// An upper bound on the item count under `bindings`: like [`Symbolic::evaluate`], except a
    /// filtered part contributes the bound of what it filters.
    ///
    /// ```
    /// use marigold_grammar::complexity::{SizeBindings, SizeName, Symbolic};
    /// use num_bigint::BigUint;
    ///
    /// let filtered: Symbolic = "\u{2264}$n".parse().unwrap();
    /// let bindings = SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(5u32))]);
    /// assert_eq!(filtered.evaluate(&bindings), None);
    /// assert_eq!(filtered.upper_bound_with(&bindings), Some(BigUint::from(5u32)));
    /// assert_eq!(filtered.upper_bound_with(&SizeBindings::new()), None);
    /// ```
    pub fn upper_bound_with(&self, bindings: &SizeBindings) -> Option<BigUint> {
        match self {
            Symbolic::Constant(v) => Some(v.clone()),
            Symbolic::Param(name) => bindings.get(name).cloned(),
            Symbolic::Unknown => None,
            Symbolic::Filtered(inner) => inner.upper_bound_with(bindings),
            Symbolic::Permutations { n, k } => {
                let n_val = n.upper_bound_with(bindings)?;
                Some(falling_factorial(&n_val, *k))
            }
            Symbolic::PermutationsWithReplacement { n, k } => {
                let n_val = n.upper_bound_with(bindings)?;
                Some(n_val.pow(*k as u32))
            }
            Symbolic::Combinations { n, k } => {
                let n_val = n.upper_bound_with(bindings)?;
                Some(binomial(&n_val, *k))
            }
            Symbolic::Min(a, b) => {
                match (a.upper_bound_with(bindings), b.upper_bound_with(bindings)) {
                    (Some(av), Some(bv)) => Some(av.min(bv)),
                    (Some(v), None) | (None, Some(v)) => Some(v),
                    (None, None) => None,
                }
            }
            Symbolic::Sum(parts) => {
                let mut total = BigUint::zero();
                for part in parts {
                    total += part.upper_bound_with(bindings)?;
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
            Symbolic::Param(name) => write!(f, "${name}"),
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
            let mut parts = pair
                .into_inner()
                .map(parse_symbolic_pair)
                .collect::<Result<Vec<Symbolic>, String>>()?;
            if parts.len() == 1 {
                Ok(parts.remove(0))
            } else {
                Ok(Symbolic::Sum(parts))
            }
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
            let base = inner
                .next()
                .ok_or_else(|| "Missing perm_rep n".to_string())?;
            let mut result = parse_symbolic_pair(base)?;
            for k_pair in inner {
                let k: u64 = k_pair
                    .as_str()
                    .parse()
                    .map_err(|_| "Invalid perm_rep k value".to_string())?;
                result = Symbolic::PermutationsWithReplacement {
                    n: Box::new(result),
                    k,
                };
            }
            Ok(result)
        }
        Rule::symbolic_rep_base => {
            let inner = pair
                .into_inner()
                .next()
                .ok_or_else(|| "Empty perm_rep base".to_string())?;
            parse_symbolic_pair(inner)
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
        Rule::symbolic_param => SizeName::new(&pair.as_str()[1..])
            .map(Symbolic::Param)
            .map_err(|e| e.to_string()),
        Rule::symbolic_unknown => Ok(Symbolic::Unknown),
        _ => Err(format!("Unexpected rule: {:?}", pair.as_rule())),
    }
}

pub(crate) fn fold_program_cardinality(
    cardinalities: impl IntoIterator<Item = Cardinality>,
) -> Cardinality {
    let mut closed = Cardinality::Exact(BigUint::zero());
    let mut open: Option<Cardinality> = None;
    for cardinality in cardinalities {
        if cardinality.mentions_size() {
            open = Some(match open {
                Some(sizes) => sizes.max(cardinality),
                None => cardinality,
            });
        } else {
            closed = closed.max(cardinality);
        }
    }
    match open {
        Some(sizes) => closed.max(sizes),
        None => closed,
    }
}

fn depends_on_bindings(expression: &Symbolic) -> bool {
    match expression {
        Symbolic::Param(_) | Symbolic::Unknown => true,
        Symbolic::Constant(_) => false,
        Symbolic::Filtered(n)
        | Symbolic::Permutations { n, .. }
        | Symbolic::PermutationsWithReplacement { n, .. }
        | Symbolic::Combinations { n, .. } => depends_on_bindings(n),
        Symbolic::Min(a, b) => depends_on_bindings(a) || depends_on_bindings(b),
        Symbolic::Sum(parts) => parts.iter().any(depends_on_bindings),
    }
}

fn canonical_key(expression: &Symbolic) -> (String, String) {
    (expression.to_string(), format!("{expression:?}"))
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

    fn mentions_size(&self) -> bool {
        matches!(self, Cardinality::Bounded(sym) if depends_on_bindings(sym))
    }

    fn summands(self) -> (Vec<Symbolic>, Option<Symbolic>) {
        fn flatten(expression: Symbolic, open: &mut Vec<Symbolic>, closed: &mut Vec<Symbolic>) {
            match expression {
                Symbolic::Sum(parts) => {
                    for part in parts {
                        flatten(part, open, closed);
                    }
                }
                part if depends_on_bindings(&part) => open.push(part),
                Symbolic::Constant(v) if v.is_zero() => {}
                part => closed.push(part),
            }
        }
        let mut open = Vec::new();
        let mut closed = Vec::new();
        match self {
            Cardinality::Exact(v) => flatten(Symbolic::Constant(v), &mut open, &mut closed),
            Cardinality::Bounded(sym) => flatten(sym, &mut open, &mut closed),
            Cardinality::Unknown => open.push(Symbolic::Unknown),
        }
        closed.sort_by_cached_key(canonical_key);
        let closed = match closed.len() {
            0 => None,
            1 => closed.pop(),
            _ => Some(Symbolic::Sum(closed)),
        };
        (open, closed)
    }

    fn join_open(self, other: Cardinality) -> Cardinality {
        let (mut left_open, left_closed) = self.summands();
        let (right_open, right_closed) = other.summands();
        let mut parts = Vec::new();
        for part in right_open {
            match left_open.iter().position(|p| *p == part) {
                Some(index) => parts.push(left_open.swap_remove(index)),
                None => parts.push(part),
            }
        }
        parts.append(&mut left_open);
        let closed = match (left_closed, right_closed) {
            (Some(a), Some(b)) => {
                let key = |s: &Symbolic| (s.upper_bound(), std::cmp::Reverse(canonical_key(s)));
                Some(if key(&a) >= key(&b) { a } else { b })
            }
            (a, b) => a.or(b),
        };
        match closed {
            Some(Symbolic::Sum(closed_parts)) => parts.extend(closed_parts),
            Some(part) => parts.push(part),
            None => {}
        }
        parts.sort_by_cached_key(canonical_key);
        match parts.len() {
            1 => Cardinality::Bounded(parts.remove(0)),
            _ => Cardinality::Bounded(Symbolic::Sum(parts)),
        }
    }

    /// The larger of two stream lengths, used for `program_cardinality`. When either side mentions
    /// a named size or `?`, the result is the sum of both sides' size-dependent terms, counted
    /// once when both sides share them, plus the larger of their remaining number-valued terms:
    /// it does not depend on the order of the arguments and is at least each side under every
    /// binding.
    ///
    /// ```
    /// use marigold_grammar::complexity::Cardinality;
    ///
    /// let linear: Cardinality = "$n".parse().unwrap();
    /// let quadratic: Cardinality = "P($n, 2)".parse().unwrap();
    /// let joined = linear.clone().max(quadratic.clone());
    /// assert_eq!(joined, quadratic.clone().max(linear.clone()));
    /// assert_eq!(joined.to_string(), "$n + P($n, 2)");
    /// assert_eq!(linear.clone().max(linear.clone()), linear);
    /// ```
    pub fn max(self, other: Cardinality) -> Cardinality {
        match (&self, &other) {
            (Cardinality::Exact(a), Cardinality::Exact(b)) => {
                Cardinality::Exact(a.clone().max(b.clone()))
            }
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            _ if self.mentions_size() || other.mentions_size() => {
                Cardinality::join_open(self, other)
            }
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

/// Complexity of one stream expression, located by name and source range.
///
/// `name` is the declared variable for `x = ...` expressions and `None` for
/// output streams such as `range(0, 3).return` or `x.return`.
///
/// ```
/// use marigold_grammar::marigold_stream_complexities;
///
/// let src = "x = range(0, 5)\nx.return";
/// let nodes = marigold_stream_complexities(src).unwrap();
/// assert_eq!(nodes[0].name.as_deref(), Some("x"));
/// assert_eq!(&src[nodes[0].range.start..nodes[0].range.end], "x = range(0, 5)");
/// assert_eq!(nodes[1].name, None);
/// assert_eq!(nodes[1].expr_index, 1);
/// ```
#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct NodeComplexity {
    pub name: Option<String>,
    pub expr_index: usize,
    pub range: crate::diagnostics::ByteRange,
    pub complexity: StreamComplexity,
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

pub(crate) fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
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

#[cfg(test)]
thread_local! {
    static STEPS_APPLIED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[derive(Clone)]
struct PipelineState {
    cardinality: Symbolic,
    unknown_source: bool,
    exact_time: ExactComplexity,
    exact_space: ExactComplexity,
    collects: bool,
    resolution: Resolution,
    stages: Vec<StageResolution>,
}

impl PipelineState {
    fn from_source(cardinality: Symbolic) -> Self {
        let resolution = Resolution::of_source(&cardinality);
        PipelineState::with_resolution(cardinality, resolution)
    }

    fn with_resolution(cardinality: Symbolic, resolution: Resolution) -> Self {
        PipelineState {
            cardinality,
            unknown_source: false,
            exact_time: ExactComplexity::new(),
            exact_space: ExactComplexity::new(),
            collects: false,
            stages: vec![StageResolution {
                stage: "input".to_string(),
                resolution: resolution.clone(),
            }],
            resolution,
        }
    }

    fn undeclared() -> Self {
        PipelineState {
            unknown_source: true,
            ..PipelineState::with_resolution(Symbolic::Unknown, Resolution::undeclared())
        }
    }

    fn step(&mut self, f: &crate::nodes::StreamFunctionNode) {
        #[cfg(test)]
        STEPS_APPLIED.with(|c| c.set(c.get() + 1));

        let time_work = step_work_class(&self.cardinality, &f.kind);
        self.exact_time.add_work(time_work, 1);

        let space_work = if self.unknown_source {
            ComplexityClass::Unknown
        } else {
            step_space_class(&self.cardinality, &f.kind)
        };
        self.exact_space.add_work(space_work, 1);

        if matches!(
            f.kind,
            StreamFunctionKind::Permutations(_)
                | StreamFunctionKind::PermutationsWithReplacement(_)
                | StreamFunctionKind::Combinations(_)
        ) {
            self.collects = true;
        }
        self.cardinality = propagate_cardinality(
            std::mem::replace(&mut self.cardinality, Symbolic::Unknown),
            &f.kind,
        );
        self.resolution = self.resolution.after(&f.kind);
        self.stages.push(StageResolution {
            stage: describe_stream_fn(&f.kind),
            resolution: self.resolution.clone(),
        });
    }

    fn extended(&self, funs: &[crate::nodes::StreamFunctionNode]) -> Self {
        let mut extended = self.clone();
        for f in funs {
            extended.step(f);
        }
        extended
    }

    fn finish(&self, description: &str) -> StreamComplexity {
        let mut exact_time = self.exact_time.clone();
        let mut exact_space = self.exact_space.clone();

        if exact_time.is_empty() {
            exact_time.add_work(cardinality_to_time_class(&self.cardinality), 1);
        }

        if self.unknown_source && exact_space.is_empty() {
            exact_space.add_work(ComplexityClass::Unknown, 1);
        }

        StreamComplexity {
            description: description.to_string(),
            cardinality: self.cardinality.classify_as_cardinality(),
            time_class: exact_time.simplified(),
            exact_time,
            space_class: exact_space.simplified(),
            exact_space,
            collects_input: self.collects,
        }
    }
}

fn describe_stream_fn(kind: &StreamFunctionKind) -> String {
    match kind {
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
    }
}

fn describe_stream_fns(funs: &[crate::nodes::StreamFunctionNode]) -> String {
    funs.iter()
        .map(|f| describe_stream_fn(&f.kind))
        .collect::<Vec<_>>()
        .join(".")
}

/// Analyzes every stream expression in a program.
///
/// A stream variable is analyzed by substitution: each consumer is analyzed as
/// the pipeline obtained by replacing the variable with its definition, so a
/// named stream and the same pipeline written inline report identical
/// cardinality, time, space and `collects_input`. A variable declared from
/// another variable reads the most recent preceding declaration; a consumer
/// reads the last declaration in the program. A variable that is never
/// declared (or is declared only after it is read) is a source the analyzer
/// cannot see, and its streams report unknown (`O(?)`) space and time.
/// For every stream, `space_class` equals `exact_space.simplified()`.
pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    analyze_nodes(expressions).0
}

pub(crate) fn analyze_nodes(
    expressions: &[TypedExpression],
) -> (
    ProgramComplexity,
    Vec<(usize, Option<String>, StreamComplexity)>,
) {
    let analysis = analyze_nodes_with(expressions, &|_, inp| input_cardinality(inp));
    (analysis.program, analysis.nodes)
}

pub(crate) struct TracedAnalysis {
    pub(crate) program: ProgramComplexity,
    pub(crate) nodes: Vec<(usize, Option<String>, StreamComplexity)>,
    pub(crate) traces: Vec<(Option<usize>, Vec<StageResolution>)>,
}

fn expression_input(expr: &TypedExpression) -> Option<&crate::nodes::InputFunctionNode> {
    match expr {
        TypedExpression::StreamVariable(v) => Some(&v.inp),
        TypedExpression::UnnamedReturningStream(s)
        | TypedExpression::UnnamedNonReturningStream(s) => Some(&s.inp_and_funs.inp),
        TypedExpression::StreamVariableFromPriorStreamVariable(_)
        | TypedExpression::NamedReturningStream(_)
        | TypedExpression::NamedNonReturningStream(_)
        | TypedExpression::StructDeclaration(_)
        | TypedExpression::EnumDeclaration(_)
        | TypedExpression::FnDeclaration(_) => None,
    }
}

pub(crate) fn input_nodes(
    expressions: &[TypedExpression],
) -> Vec<&crate::nodes::InputFunctionNode> {
    expressions.iter().filter_map(expression_input).collect()
}

pub(crate) fn analyze_nodes_with(
    expressions: &[TypedExpression],
    source_cardinality: &dyn Fn(usize, &crate::nodes::InputFunctionNode) -> Symbolic,
) -> TracedAnalysis {
    let ordinals: Vec<usize> = expressions
        .iter()
        .scan(0, |seen, expr| {
            let ordinal = *seen;
            *seen += usize::from(expression_input(expr).is_some());
            Some(ordinal)
        })
        .collect();
    let mut nodes: Vec<(usize, Option<String>, StreamComplexity)> = Vec::new();
    let mut traces: Vec<(usize, Option<usize>, Vec<StageResolution>)> = Vec::new();
    let mut declared: std::collections::HashMap<String, PipelineState> =
        std::collections::HashMap::new();

    for (expr_index, expr) in expressions.iter().enumerate() {
        match expr {
            TypedExpression::StreamVariable(v) => {
                let ordinal = ordinals[expr_index];
                let state = PipelineState::from_source(source_cardinality(ordinal, &v.inp))
                    .extended(&v.funs);
                let desc = if v.funs.is_empty() {
                    format!("{} = input", v.variable_name)
                } else {
                    format!(
                        "{} = input.{}",
                        v.variable_name,
                        describe_stream_fns(&v.funs)
                    )
                };
                nodes.push((
                    expr_index,
                    Some(v.variable_name.clone()),
                    state.finish(&desc),
                ));
                traces.push((expr_index, None, state.stages.clone()));
                declared.insert(v.variable_name.clone(), state);
            }
            TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                let state = declared
                    .get(&v.prior_stream_variable)
                    .cloned()
                    .unwrap_or_else(PipelineState::undeclared)
                    .extended(&v.funs);
                let desc = if v.funs.is_empty() {
                    format!("{} = {}", v.variable_name, v.prior_stream_variable)
                } else {
                    format!(
                        "{} = {}.{}",
                        v.variable_name,
                        v.prior_stream_variable,
                        describe_stream_fns(&v.funs)
                    )
                };
                nodes.push((
                    expr_index,
                    Some(v.variable_name.clone()),
                    state.finish(&desc),
                ));
                traces.push((expr_index, None, state.stages.clone()));
                declared.insert(v.variable_name.clone(), state);
            }
            _ => {}
        }
    }

    let mut streams = Vec::new();
    let mut program_time = ComplexityClass::O1;
    let mut program_exact_time = ExactComplexity::new();
    let mut program_space = ComplexityClass::O1;
    let mut program_exact_space = ExactComplexity::new();
    let mut stream_cardinalities = Vec::new();

    for (expr_index, expr) in expressions.iter().enumerate() {
        let (sc, trace) = match expr {
            TypedExpression::UnnamedReturningStream(s)
            | TypedExpression::UnnamedNonReturningStream(s) => {
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
                let ordinal = ordinals[expr_index];
                let state =
                    PipelineState::from_source(source_cardinality(ordinal, &s.inp_and_funs.inp))
                        .extended(&s.inp_and_funs.funs);
                (state.finish(&desc), state.stages)
            }
            TypedExpression::NamedReturningStream(s)
            | TypedExpression::NamedNonReturningStream(s) => {
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
                let state = declared
                    .get(&s.stream_variable)
                    .cloned()
                    .unwrap_or_else(PipelineState::undeclared)
                    .extended(&s.funs);
                (state.finish(&desc), state.stages)
            }
            _ => continue,
        };

        program_time = program_time.max(sc.time_class.clone());
        program_exact_time.merge(&sc.exact_time);
        program_space = program_space.max(sc.space_class.clone());
        program_exact_space.merge(&sc.exact_space);
        stream_cardinalities.push(sc.cardinality.clone());
        nodes.push((expr_index, None, sc.clone()));
        traces.push((expr_index, Some(streams.len()), trace));
        streams.push(sc);
    }
    nodes.sort_by_key(|n| n.0);
    traces.sort_by_key(|t| t.0);
    let program_cardinality = fold_program_cardinality(stream_cardinalities);

    let program = ProgramComplexity {
        streams,
        program_time,
        program_exact_time,
        program_space,
        program_exact_space,
        program_cardinality,
    };
    TracedAnalysis {
        program,
        nodes,
        traces: traces
            .into_iter()
            .map(|(_, program_stream, trace)| (program_stream, trace))
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn every_stream_function_kind() -> Vec<StreamFunctionKind> {
        let kinds = vec![
            StreamFunctionKind::Map,
            StreamFunctionKind::Filter,
            StreamFunctionKind::FilterMap,
            StreamFunctionKind::Permutations(2),
            StreamFunctionKind::PermutationsWithReplacement(2),
            StreamFunctionKind::Combinations(2),
            StreamFunctionKind::KeepFirstN(2),
            StreamFunctionKind::Fold,
            StreamFunctionKind::Ok,
            StreamFunctionKind::OkOrPanic,
        ];
        for kind in &kinds {
            match kind {
                StreamFunctionKind::Map
                | StreamFunctionKind::Filter
                | StreamFunctionKind::FilterMap
                | StreamFunctionKind::Permutations(_)
                | StreamFunctionKind::PermutationsWithReplacement(_)
                | StreamFunctionKind::Combinations(_)
                | StreamFunctionKind::KeepFirstN(_)
                | StreamFunctionKind::Fold
                | StreamFunctionKind::Ok
                | StreamFunctionKind::OkOrPanic => {}
            }
        }
        kinds
    }

    #[test]
    fn awaits_whole_input_is_superset_of_analyzer_collects_input() {
        let mut differing = Vec::new();
        for kind in every_stream_function_kind() {
            let node = crate::nodes::StreamFunctionNode {
                kind: kind.clone(),
                code: String::new(),
            };
            let analyzed = PipelineState::from_source(Symbolic::Unknown)
                .extended(&[node])
                .finish("t");
            if analyzed.collects_input {
                assert!(
                    kind.awaits_whole_input(),
                    "{kind:?} is reported as collecting but would not be deferred"
                );
            }
            if kind.awaits_whole_input() != analyzed.collects_input {
                differing.push(kind);
            }
        }
        assert_eq!(differing, vec![StreamFunctionKind::KeepFirstN(2)]);
    }

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
            layout: vec![],
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
        assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
        assert!(!result.streams[0].collects_input);
    }

    #[test]
    fn test_analyze_collecting_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 100).permutations(3).return").unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
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
        assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
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
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
    }

    #[test]
    fn test_analyze_program_aggregation() {
        let input = "fn identity(x: i32) -> i32 { x }\nrange(0, 10).map(identity).return\nrange(0, 10).permutations(2).return";
        let result = crate::parser::PestParser::analyze(input).unwrap();
        assert_eq!(result.streams.len(), 2);
        assert_eq!(result.program_space, ComplexityClass::O1);
    }

    #[test]
    fn test_analyze_combinations_pipeline() {
        let result =
            crate::parser::PestParser::analyze("range(0, 10).combinations(2).return").unwrap();
        assert_eq!(result.streams.len(), 1);
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
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
        assert_eq!(result.streams[0].space_class, ComplexityClass::O1);
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

    #[test]
    fn test_complexity_class_error_case() {
        assert!(ComplexityClass::from_str("O(invalid)").is_err());
    }

    #[test]
    fn test_exact_complexity_default_is_empty() {
        let ec = ExactComplexity::default();
        assert_eq!(ec.to_string(), "O(1)");
    }

    #[test]
    fn test_exact_complexity_onlogk_factorial_unknown_roundtrip() {
        for (class, expected_multi) in [
            (ComplexityClass::ONLogK(5), "O(n*log(5) + n)"),
            (ComplexityClass::OFactorial, "O(n! + n)"),
            (ComplexityClass::Unknown, "O(? + n)"),
        ] {
            let mut ec = ExactComplexity::new();
            ec.add_work(class.clone(), 1);
            ec.add_work(ComplexityClass::ON, 1);
            assert_eq!(ec.to_string(), expected_multi, "display for {class:?}");
            let parsed = ExactComplexity::from_str(expected_multi).unwrap();
            assert_eq!(
                parsed.to_string(),
                expected_multi,
                "roundtrip for {class:?}"
            );
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

    fn chain_source(declarations: usize) -> String {
        let mut src = String::from("fn f(a: i32) -> i32 { a }\nv0 = range(0, 3)\n");
        for i in 1..=declarations {
            src.push_str(&format!("v{i} = v{}.map(f)\n", i - 1));
        }
        src.push_str(&format!("v{declarations}.return\n"));
        src
    }

    fn steps_applied_for(src: &str) -> usize {
        STEPS_APPLIED.with(|c| c.set(0));
        crate::parser::PestParser::analyze(src).unwrap();
        STEPS_APPLIED.with(|c| c.get())
    }

    #[test]
    fn variable_chain_applies_each_step_once() {
        for declarations in [1, 10, 100, 1000] {
            assert_eq!(
                steps_applied_for(&chain_source(declarations)),
                declarations,
                "{declarations} chained declarations"
            );
        }
    }

    #[test]
    fn consumers_apply_only_their_own_steps_on_top_of_the_variable() {
        let mut src = chain_source(50);
        src.push_str("v50.map(f).map(f).return\nv50.return\n");
        assert_eq!(steps_applied_for(&src), 50 + 2);
    }

    #[test]
    fn step_count_grows_linearly_with_chain_length() {
        let small = steps_applied_for(&chain_source(500));
        let large = steps_applied_for(&chain_source(2000));
        assert_eq!(large, small * 4);
    }
}

#[cfg(test)]
mod cardinality_join {
    use super::*;
    use proptest::prelude::*;

    fn size(name: &str) -> Symbolic {
        Symbolic::Param(SizeName::new(name).unwrap())
    }

    fn tree(with_sizes: bool) -> BoxedStrategy<Symbolic> {
        let leaf = if with_sizes {
            prop_oneof![
                (0u64..20).prop_map(|v| Symbolic::Constant(BigUint::from(v))),
                Just(size("n")),
                Just(size("m")),
            ]
            .boxed()
        } else {
            (0u64..20)
                .prop_map(|v| Symbolic::Constant(BigUint::from(v)))
                .boxed()
        };
        leaf.prop_recursive(3, 12, 3, |inner| {
            prop_oneof![
                inner.clone().prop_map(|n| Symbolic::Filtered(Box::new(n))),
                (inner.clone(), 0u64..3)
                    .prop_map(|(n, k)| Symbolic::Permutations { n: Box::new(n), k }),
                (inner.clone(), 0u64..3).prop_map(|(n, k)| {
                    Symbolic::PermutationsWithReplacement { n: Box::new(n), k }
                }),
                (inner.clone(), 0u64..3)
                    .prop_map(|(n, k)| Symbolic::Combinations { n: Box::new(n), k }),
                (inner.clone(), inner.clone())
                    .prop_map(|(a, b)| Symbolic::Min(Box::new(a), Box::new(b))),
                prop::collection::vec(inner, 2..4).prop_map(Symbolic::Sum),
            ]
        })
        .boxed()
    }

    fn open() -> BoxedStrategy<Cardinality> {
        tree(true)
            .prop_filter("mentions a size", depends_on_bindings)
            .prop_map(Cardinality::Bounded)
            .boxed()
    }

    fn closed() -> BoxedStrategy<Cardinality> {
        prop_oneof![
            (0u64..50).prop_map(|v| Cardinality::Exact(BigUint::from(v))),
            tree(false).prop_map(Cardinality::Bounded),
        ]
        .boxed()
    }

    fn any_cardinality() -> BoxedStrategy<Cardinality> {
        prop_oneof![open(), closed()].boxed()
    }

    fn ceiling(cardinality: &Cardinality, bindings: &SizeBindings) -> Option<BigUint> {
        match cardinality {
            Cardinality::Exact(v) => Some(v.clone()),
            Cardinality::Bounded(sym) => sym.upper_bound_with(bindings),
            Cardinality::Unknown => None,
        }
    }

    fn at(n: u64, m: u64) -> SizeBindings {
        SizeBindings::from([
            (SizeName::new("n").unwrap(), BigUint::from(n)),
            (SizeName::new("m").unwrap(), BigUint::from(m)),
        ])
    }

    fn fold(items: &[Cardinality]) -> Cardinality {
        items
            .iter()
            .cloned()
            .fold(Cardinality::Exact(BigUint::zero()), Cardinality::max)
    }

    #[test]
    fn a_bound_variable_consumed_twice_gives_one_program_cardinality_in_either_order() {
        let linear = Cardinality::Bounded(size("n"));
        let quadratic = Cardinality::Bounded(Symbolic::Permutations {
            n: Box::new(size("n")),
            k: 2,
        });
        let forward = fold_program_cardinality([linear.clone(), quadratic.clone()]);
        let backward = fold_program_cardinality([quadratic, linear]);
        assert_eq!(forward, backward);
        assert_eq!(forward.to_string(), "$n + P($n, 2)");
    }

    #[test]
    fn shared_terms_are_counted_once_and_repeated_terms_keep_their_multiplicity() {
        let twice: Cardinality = "$n + $n".parse().unwrap();
        let once: Cardinality = "$n".parse().unwrap();
        assert_eq!(twice.clone().max(once.clone()), twice);
        assert_eq!(once.clone().max(twice.clone()), twice);
        let joined = "$n + 4"
            .parse::<Cardinality>()
            .unwrap()
            .max("$m + \u{2264}9".parse().unwrap());
        assert_eq!(joined.to_string(), "$m + $n + \u{2264}9");
        assert_eq!(
            Cardinality::Exact(BigUint::from(7u32))
                .max("$n".parse().unwrap())
                .to_string(),
            "$n + 7"
        );
        assert_eq!(
            Cardinality::Exact(BigUint::zero()).max("$n".parse().unwrap()),
            "$n".parse().unwrap()
        );
        assert_eq!(
            Cardinality::Unknown.max("$n".parse().unwrap()),
            Cardinality::Unknown
        );
    }

    #[test]
    fn closed_streams_keep_the_default_fold() {
        let cardinalities = [
            Cardinality::Exact(BigUint::from(100u32)),
            Cardinality::Bounded(Symbolic::Filtered(Box::new(Symbolic::Constant(
                BigUint::from(3u32),
            )))),
        ];
        assert_eq!(
            fold_program_cardinality(cardinalities.clone()),
            fold(&cardinalities)
        );
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(512))]

        #[test]
        fn max_with_a_size_is_commutative_and_bounds_both_sides(
            a in any_cardinality(),
            b in any_cardinality(),
        ) {
            prop_assume!(a.mentions_size() || b.mentions_size());
            let ab = a.clone().max(b.clone());
            let ba = b.clone().max(a.clone());
            prop_assert_eq!(&ab, &ba);
            prop_assert_eq!(ab.clone().max(ab.clone()), ab.clone());
            for n in 0..10 {
                for bindings in [at(n, n), at(n, 9 - n)] {
                    let joined = ceiling(&ab, &bindings).expect("every size is bound");
                    for side in [&a, &b] {
                        let side = ceiling(side, &bindings).expect("every size is bound");
                        prop_assert!(joined >= side, "{} < {} at n={}", ab, side, n);
                    }
                }
            }
        }

        #[test]
        fn max_over_sized_streams_does_not_depend_on_their_order(
            items in prop::collection::vec(open(), 1..5),
            reordered in prop::collection::vec(open(), 1..5).prop_flat_map(|v| Just(v).prop_shuffle()),
            rotation in any::<usize>(),
        ) {
            let mut rotated = items.clone();
            let len = rotated.len();
            rotated.rotate_left(rotation % len);
            rotated.reverse();
            prop_assert_eq!(fold(&items), fold(&rotated));
            let mut sorted = reordered.clone();
            sorted.sort_by_cached_key(|c| c.to_string());
            prop_assert_eq!(fold(&reordered), fold(&sorted));
        }

        #[test]
        fn the_program_fold_depends_only_on_the_order_of_closed_streams(
            sized in prop::collection::vec(open(), 0..4),
            fixed in prop::collection::vec(closed(), 0..4),
            mask in prop::collection::vec(any::<bool>(), 8),
        ) {
            let mut interleaved = Vec::new();
            let (mut s, mut f) = (sized.iter(), fixed.iter());
            for take_sized in &mask {
                let next = if *take_sized { s.next().or_else(|| f.next()) } else { f.next().or_else(|| s.next()) };
                if let Some(item) = next {
                    interleaved.push(item.clone());
                }
            }
            let mut grouped: Vec<Cardinality> = sized.iter().rev().cloned().collect();
            grouped.extend(fixed.iter().cloned());
            prop_assert_eq!(
                fold_program_cardinality(interleaved),
                fold_program_cardinality(grouped)
            );
        }
    }
}
