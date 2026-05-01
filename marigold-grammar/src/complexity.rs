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
            ComplexityClass::OLogN => write!(f, "O(log n)"),
            ComplexityClass::ON => write!(f, "O(n)"),
            ComplexityClass::ONLogK(k) => write!(f, "O(n log {k})"),
            ComplexityClass::ONLogN => write!(f, "O(n log n)"),
            ComplexityClass::OPolynomial(k) => write!(f, "O(n^{k})"),
            ComplexityClass::OCombinatorial(k) => write!(f, "O(C(n,{k}))"),
            ComplexityClass::OPermutational(k) => write!(f, "O(P(n,{k}))"),
            ComplexityClass::OFactorial => write!(f, "O(n!)"),
            ComplexityClass::Unknown => write!(f, "O(?)"),
        }
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

impl FromStr for ComplexityClass {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // parse with complexity_notation.pest
        let mut pairs = ComplexityNotationParser::parse(Rule::complexity_class, s)
            .map_err(|e| e.to_string())?;
        let pair = pairs.next().ok_or_else(|| "empty parse".to_string())?;
        parse_complexity_class(pair).ok_or_else(|| format!("unrecognized complexity: {s}"))
    }
}

fn parse_complexity_class(pair: pest::iterators::Pair<Rule>) -> Option<ComplexityClass> {
    match pair.as_rule() {
        Rule::complexity_class => {
            let inner = pair.into_inner().next()?;
            parse_complexity_class(inner)
        }
        Rule::o1 => Some(ComplexityClass::O1),
        Rule::o_log_n => Some(ComplexityClass::OLogN),
        Rule::o_n => Some(ComplexityClass::ON),
        Rule::o_n_log_k => {
            let k = pair
                .into_inner()
                .next()?
                .as_str()
                .parse::<u64>()
                .ok()?;
            Some(ComplexityClass::ONLogK(k))
        }
        Rule::o_n_log_n => Some(ComplexityClass::ONLogN),
        Rule::o_polynomial => {
            let k = pair
                .into_inner()
                .next()?
                .as_str()
                .parse::<u64>()
                .ok()?;
            Some(ComplexityClass::OPolynomial(k))
        }
        Rule::o_combinatorial => {
            let k = pair
                .into_inner()
                .next()?
                .as_str()
                .parse::<u64>()
                .ok()?;
            Some(ComplexityClass::OCombinatorial(k))
        }
        Rule::o_permutational => {
            let k = pair
                .into_inner()
                .next()?
                .as_str()
                .parse::<u64>()
                .ok()?;
            Some(ComplexityClass::OPermutational(k))
        }
        Rule::o_factorial => Some(ComplexityClass::OFactorial),
        Rule::o_unknown => Some(ComplexityClass::Unknown),
        _ => None,
    }
}

/// A symbolic representation of a stream's cardinality (element count).
///
/// This is used internally during complexity analysis to track how many elements
/// flow through each stage of a pipeline.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbolic {
    /// A compile-time constant number of elements (e.g. `range(0, 10)` → `Constant(10)`).
    Constant(BigUint),
    /// The cardinality depends on a runtime variable (the "n" in big-O notation).
    Variable,
    /// The cardinality is the result of a filter, so it is at most as large as the input
    /// but may be smaller at runtime.
    Filtered(Box<Symbolic>),
    /// The cardinality is the Cartesian product of two streams.
    Product(Box<Symbolic>, Box<Symbolic>),
    /// The cardinality is the number of k-combinations of the input.
    Combinations(Box<Symbolic>, u64),
    /// The cardinality is the number of k-permutations of the input.
    Permutations(Box<Symbolic>, u64),
    /// The cardinality is n! (all permutations).
    Factorial(Box<Symbolic>),
    /// Unknown / not yet computed.
    Unknown,
}

impl Symbolic {
    /// Try to evaluate the symbolic expression to a concrete [`BigUint`] value.
    ///
    /// Returns `None` if the expression contains any variable or unknown term.
    pub fn try_evaluate(&self) -> Option<BigUint> {
        match self {
            Symbolic::Constant(n) => Some(n.clone()),
            Symbolic::Variable => None,
            Symbolic::Filtered(inner) => {
                // A filter *might* pass all elements through, but we cannot
                // statically guarantee that, so we conservatively return None
                // unless the inner cardinality is already zero.
                let n = inner.try_evaluate()?;
                if n.is_zero() {
                    Some(BigUint::zero())
                } else {
                    None
                }
            }
            Symbolic::Product(a, b) => {
                let a = a.try_evaluate()?;
                let b = b.try_evaluate()?;
                Some(a * b)
            }
            Symbolic::Combinations(inner, k) => {
                let n = inner.try_evaluate()?;
                Some(combinations(n, *k))
            }
            Symbolic::Permutations(inner, k) => {
                let n = inner.try_evaluate()?;
                Some(permutations(n, *k))
            }
            Symbolic::Factorial(inner) => {
                let n = inner.try_evaluate()?;
                Some(factorial(n))
            }
            Symbolic::Unknown => None,
        }
    }

    /// Return the "worst-case" [`ComplexityClass`] implied by this symbolic cardinality.
    pub fn complexity_class(&self) -> ComplexityClass {
        match self {
            Symbolic::Constant(_) => ComplexityClass::O1,
            Symbolic::Variable => ComplexityClass::ON,
            Symbolic::Filtered(inner) => inner.complexity_class(),
            Symbolic::Product(a, b) => {
                let ca = a.complexity_class();
                let cb = b.complexity_class();
                ca.max(cb)
            }
            Symbolic::Combinations(inner, k) => {
                if inner.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OCombinatorial(*k)
                }
            }
            Symbolic::Permutations(inner, k) => {
                if inner.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OPermutational(*k)
                }
            }
            Symbolic::Factorial(inner) => {
                if inner.try_evaluate().is_some() {
                    ComplexityClass::O1
                } else {
                    ComplexityClass::OFactorial
                }
            }
            Symbolic::Unknown => ComplexityClass::Unknown,
        }
    }
}

/// Compute n-choose-k (binomial coefficient).
fn combinations(n: BigUint, k: u64) -> BigUint {
    if k == 0 {
        return BigUint::one();
    }
    let k_big = BigUint::from(k);
    if n < k_big {
        return BigUint::zero();
    }
    // C(n, k) = n! / (k! * (n-k)!)
    // Compute iteratively to avoid huge intermediates.
    let mut result = BigUint::one();
    for i in 0..k {
        result = result * (n.clone() - BigUint::from(i)) / BigUint::from(i + 1);
    }
    result
}

/// Compute P(n, k) = n! / (n-k)! (k-permutations).
fn permutations(n: BigUint, k: u64) -> BigUint {
    let mut result = BigUint::one();
    for i in 0..k {
        let term = n.clone() - BigUint::from(i);
        result *= term;
    }
    result
}

/// Compute n! (factorial).
fn factorial(n: BigUint) -> BigUint {
    if n.is_zero() {
        return BigUint::one();
    }
    let mut result = BigUint::one();
    let mut i = BigUint::from(2u64);
    while i <= n {
        result *= i.clone();
        i += BigUint::one();
    }
    result
}

/// An exact (non-asymptotic) complexity value, expressed as a polynomial in n.
///
/// For example, a pipeline with 3 map steps has `ExactComplexity` of `O(3)` (three
/// constant-work steps), while a pipeline that iterates over the full input has `O(n)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExactComplexity {
    /// Coefficients of the polynomial, stored from lowest degree to highest.
    /// `coefficients[i]` is the coefficient for n^i.
    coefficients: Vec<BigUint>,
}

impl ExactComplexity {
    pub fn zero() -> Self {
        ExactComplexity {
            coefficients: vec![BigUint::zero()],
        }
    }

    pub fn constant(c: BigUint) -> Self {
        ExactComplexity {
            coefficients: vec![c],
        }
    }

    pub fn n() -> Self {
        ExactComplexity {
            coefficients: vec![BigUint::zero(), BigUint::one()],
        }
    }

    fn trim(&mut self) {
        while self.coefficients.len() > 1 && self.coefficients.last() == Some(&BigUint::zero()) {
            self.coefficients.pop();
        }
    }
}

impl std::ops::Add for ExactComplexity {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self {
        let max_len = self.coefficients.len().max(rhs.coefficients.len());
        self.coefficients.resize(max_len, BigUint::zero());
        for (i, c) in rhs.coefficients.into_iter().enumerate() {
            self.coefficients[i] += c;
        }
        self.trim();
        self
    }
}

impl fmt::Display for ExactComplexity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Build a human-readable polynomial string.
        let mut terms: Vec<String> = Vec::new();
        for (i, c) in self.coefficients.iter().enumerate().rev() {
            if c.is_zero() {
                continue;
            }
            let term = match i {
                0 => c.to_string(),
                1 => {
                    if *c == BigUint::one() {
                        "n".to_string()
                    } else {
                        format!("{c}n")
                    }
                }
                _ => {
                    if *c == BigUint::one() {
                        format!("n^{i}")
                    } else {
                        format!("{c}n^{i}")
                    }
                }
            };
            terms.push(term);
        }
        if terms.is_empty() {
            write!(f, "O(0)")
        } else {
            write!(f, "O({})", terms.join(" + "))
        }
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

impl FromStr for ExactComplexity {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pairs = ComplexityNotationParser::parse(Rule::exact_complexity, s)
            .map_err(|e| e.to_string())?;
        let pair = pairs.next().ok_or_else(|| "empty parse".to_string())?;
        parse_exact_complexity(pair).ok_or_else(|| format!("unrecognized exact complexity: {s}"))
    }
}

fn parse_exact_complexity(pair: pest::iterators::Pair<Rule>) -> Option<ExactComplexity> {
    match pair.as_rule() {
        Rule::exact_complexity => {
            let inner = pair.into_inner().next()?;
            parse_exact_complexity(inner)
        }
        Rule::exact_zero => Some(ExactComplexity::zero()),
        Rule::exact_poly => {
            let mut coefficients: Vec<BigUint> = Vec::new();
            for term in pair.into_inner() {
                match term.as_rule() {
                    Rule::exact_term_constant => {
                        let c = term.as_str().parse::<BigUint>().ok()?;
                        if coefficients.is_empty() {
                            coefficients.push(c);
                        } else {
                            coefficients[0] += c;
                        }
                    }
                    Rule::exact_term_n => {
                        while coefficients.len() < 2 {
                            coefficients.push(BigUint::zero());
                        }
                        coefficients[1] += BigUint::one();
                    }
                    Rule::exact_term_n_coeff => {
                        let c = term
                            .into_inner()
                            .next()?
                            .as_str()
                            .parse::<BigUint>()
                            .ok()?;
                        while coefficients.len() < 2 {
                            coefficients.push(BigUint::zero());
                        }
                        coefficients[1] += c;
                    }
                    Rule::exact_term_n_pow => {
                        let mut inner = term.into_inner();
                        let coeff_pair = inner.next()?;
                        let exp_pair = inner.next()?;
                        let c: BigUint = coeff_pair.as_str().parse().ok()?;
                        let e: usize = exp_pair.as_str().parse().ok()?;
                        while coefficients.len() <= e {
                            coefficients.push(BigUint::zero());
                        }
                        coefficients[e] += c;
                    }
                    Rule::exact_term_n_pow_no_coeff => {
                        let e: usize = term
                            .into_inner()
                            .next()?
                            .as_str()
                            .parse()
                            .ok()?;
                        while coefficients.len() <= e {
                            coefficients.push(BigUint::zero());
                        }
                        coefficients[e] += BigUint::one();
                    }
                    _ => {}
                }
            }
            if coefficients.is_empty() {
                coefficients.push(BigUint::zero());
            }
            let mut ec = ExactComplexity { coefficients };
            ec.trim();
            Some(ec)
        }
        _ => None,
    }
}

/// The cardinality of a stream's output — how many elements it produces.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "value")]
pub enum Cardinality {
    /// A statically-known exact element count.
    Exact(BigUint),
    /// The count depends on a runtime variable.
    Variable,
    /// The count is bounded by some filtered subset of a larger stream.
    Filtered,
    /// Unknown.
    Unknown,
}

impl Cardinality {
    pub fn max(self, other: Cardinality) -> Cardinality {
        match (&self, &other) {
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            (Cardinality::Variable, _) | (_, Cardinality::Variable) => Cardinality::Variable,
            (Cardinality::Filtered, _) | (_, Cardinality::Filtered) => Cardinality::Filtered,
            (Cardinality::Exact(a), Cardinality::Exact(b)) => {
                if a >= b {
                    self
                } else {
                    other
                }
            }
        }
    }
}

/// Complexity information for a single stream pipeline.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StreamComplexity {
    pub time_class: ComplexityClass,
    pub exact_time: ExactComplexity,
    pub space_class: ComplexityClass,
    pub exact_space: ExactComplexity,
    pub collects_input: bool,
    pub cardinality: Cardinality,
}

/// Complexity information for the whole analyzed program.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProgramComplexity {
    pub streams: Vec<StreamComplexity>,
    pub program_time: ComplexityClass,
    pub program_exact_time: ExactComplexity,
    pub program_space: ComplexityClass,
    pub program_exact_space: ExactComplexity,
    pub program_cardinality: Cardinality,
    /// When true, the analysis assumes user-defined functions are O(1); set when any
    /// `FnDeclaration` node exists in the program. Callers should treat complexity results
    /// conservatively if this flag is true, because the actual cost of those functions is
    /// not analysed.
    #[serde(default)]
    pub assumes_o1_user_fns: bool,
}

fn input_cardinality(inp: &crate::nodes::InputFunctionNode) -> Symbolic {
    match (&inp.variability, &inp.input_count) {
        (InputVariability::Constant, InputCount::Known(n)) => Symbolic::Constant(n.clone()),
        (InputVariability::Constant, InputCount::Unknown) => Symbolic::Unknown,
        (InputVariability::Variable, _) => Symbolic::Variable,
    }
}

/// Returns the [`ComplexityClass`] for a single pipeline step given the cardinality of its input.
///
/// Most stream operations are O(1) per element (map, filter, etc.), so the overall work for the
/// step is proportional to the input cardinality. Aggregation steps (fold, collect) must visit
/// every element once, so they are treated the same way. Only steps with a *known constant*
/// input cardinality short-circuit to O(1) (via `try_evaluate`).
fn step_work_class(input_card: &Symbolic, kind: &StreamFunctionKind) -> ComplexityClass {
    // Constant-cardinality input ⇒ O(1) regardless of operation.
    if input_card.try_evaluate().is_some() {
        return ComplexityClass::O1;
    }

    match kind {
        StreamFunctionKind::Combinations(k) => ComplexityClass::OCombinatorial(*k),
        StreamFunctionKind::Permutations(k) => ComplexityClass::OPermutational(*k),
        StreamFunctionKind::Factorial => ComplexityClass::OFactorial,
        _ => ComplexityClass::ON,
    }
}

/// Returns the [`ExactComplexity`] contribution of a single pipeline step.
///
/// Constant-input steps always contribute `O(1)` (one unit of work). Variable-input steps
/// contribute one unit per element in the input stream (`O(n)`).
fn step_exact_work(input_card: &Symbolic) -> ExactComplexity {
    if input_card.try_evaluate().is_some() {
        ExactComplexity::constant(BigUint::one())
    } else {
        ExactComplexity::n()
    }
}

/// Derive the output cardinality of a step from its input cardinality and the step kind.
fn step_output_cardinality(input: &Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Filter => Symbolic::Filtered(Box::new(input.clone())),
        StreamFunctionKind::Combinations(k) => {
            Symbolic::Combinations(Box::new(input.clone()), *k)
        }
        StreamFunctionKind::Permutations(k) => {
            Symbolic::Permutations(Box::new(input.clone()), *k)
        }
        StreamFunctionKind::Factorial => Symbolic::Factorial(Box::new(input.clone())),
        StreamFunctionKind::FlatMap => {
            // flat_map produces a variable number of outputs per input element.
            Symbolic::Product(Box::new(input.clone()), Box::new(Symbolic::Variable))
        }
        // All other step kinds are cardinality-preserving.
        _ => input.clone(),
    }
}

/// Analyse one complete stream pipeline (a chain of stream operations ending with `.return`)
/// and return its [`StreamComplexity`].
fn analyze_stream(
    pipeline: &[crate::nodes::StreamFunctionNode],
    fn_map: &std::collections::HashMap<
        String,
        (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    >,
    input_card: Symbolic,
) -> StreamComplexity {
    let mut time_class = ComplexityClass::O1;
    let mut exact_time = ExactComplexity::zero();
    let mut space_class = ComplexityClass::O1;
    let mut exact_space = ExactComplexity::zero();
    let mut collects_input = false;
    let mut current_card = input_card;

    for step in pipeline {
        // Accumulate time complexity for this step.
        let step_class = step_work_class(&current_card, &step.kind);
        time_class = time_class.max(step_class);
        exact_time = exact_time + step_exact_work(&current_card);

        // Track space: only collecting steps (fold/collect) require O(n) space.
        match &step.kind {
            StreamFunctionKind::Fold | StreamFunctionKind::Collect => {
                let step_space = step_work_class(&current_card, &step.kind);
                space_class = space_class.max(step_space);
                exact_space = exact_space + step_exact_work(&current_card);
                collects_input = true;
            }
            StreamFunctionKind::UserDefined(fn_name) => {
                if let Some((_, fn_time_class, fn_exact_time, fn_exact_space)) =
                    fn_map.get(fn_name)
                {
                    time_class = time_class.max(fn_time_class.clone());
                    exact_time = exact_time.clone() + fn_exact_time.clone();
                    space_class = space_class.max(ComplexityClass::O1);
                    exact_space = exact_space.clone() + fn_exact_space.clone();
                }
            }
            _ => {}
        }

        // Update the cardinality flowing into the next step.
        current_card = step_output_cardinality(&current_card, &step.kind);
    }

    let cardinality = symbolic_to_cardinality(&current_card);

    StreamComplexity {
        time_class,
        exact_time,
        space_class,
        exact_space,
        collects_input,
        cardinality,
    }
}

/// Convert a [`Symbolic`] cardinality to the serialisable [`Cardinality`] enum.
fn symbolic_to_cardinality(s: &Symbolic) -> Cardinality {
    match s {
        Symbolic::Constant(n) => Cardinality::Exact(n.clone()),
        Symbolic::Variable => Cardinality::Variable,
        Symbolic::Filtered(_) => Cardinality::Filtered,
        Symbolic::Unknown => Cardinality::Unknown,
        // Derived forms — conservatively unknown.
        Symbolic::Product(_, _)
        | Symbolic::Combinations(_, _)
        | Symbolic::Permutations(_, _)
        | Symbolic::Factorial(_) => Cardinality::Unknown,
    }
}

/// Analyse a complete Marigold program (a slice of [`TypedExpression`]s) and return
/// the [`ProgramComplexity`] summary.
pub fn analyze_program(expressions: &[TypedExpression]) -> ProgramComplexity {
    // First pass: collect information about all user-defined functions so that
    // call sites can look up their complexity.
    let mut fn_map: std::collections::HashMap<
        String,
        (Symbolic, ComplexityClass, ExactComplexity, ExactComplexity),
    > = std::collections::HashMap::new();
    let mut has_fn_declaration = false;

    for expr in expressions {
        match expr {
            TypedExpression::FnDeclaration(fn_decl) => {
                has_fn_declaration = true;
                // Analyse the body of the function to obtain its complexity.
                // For now we treat every function body as a single O(1) pipeline
                // (the body is a stream of expressions, not a stream pipeline).
                // A more precise analysis would recurse into the body.
                let fn_card = Symbolic::Variable;
                let fn_time_class = ComplexityClass::O1;
                let fn_exact_time = ExactComplexity::constant(BigUint::one());
                let fn_exact_space = ExactComplexity::constant(BigUint::zero());
                fn_map.insert(
                    fn_decl.name.clone(),
                    (fn_card, fn_time_class, fn_exact_time, fn_exact_space),
                );
            }
            _ => {}
        }
    }

    // Second pass: analyse every top-level stream pipeline.
    let mut streams: Vec<StreamComplexity> = Vec::new();

    for expr in expressions {
        match expr {
            TypedExpression::StreamPipeline(pipeline_node) => {
                let input_card = input_cardinality(&pipeline_node.input);
                let sc = analyze_stream(&pipeline_node.pipeline, &fn_map, input_card);
                streams.push(sc);
            }
            TypedExpression::Assignment(assign) => {
                // An assignment binds a sub-pipeline to a variable. The variable's
                // cardinality is the output cardinality of the sub-pipeline.
                let input_card = input_cardinality(&assign.input);
                let sc = analyze_stream(&assign.pipeline, &fn_map, input_card.clone());
                // Re-run to get the symbolic output cardinality for use by downstream
                // pipelines that reference this variable.
                let mut current_card = input_card;
                for step in &assign.pipeline {
                    current_card = step_output_cardinality(&current_card, &step.kind);
                }
                fn_map.insert(
                    assign.name.clone(),
                    (
                        current_card,
                        sc.time_class.clone(),
                        sc.exact_time.clone(),
                        sc.exact_space.clone(),
                    ),
                );
            }
            TypedExpression::FnDeclaration(_) => {
                has_fn_declaration = true;
            }
            _ => {}
        }
    }

    // Aggregate across all streams.
    let mut program_time = ComplexityClass::O1;
    let mut program_exact_time = ExactComplexity::zero();
    let mut program_space = ComplexityClass::O1;
    let mut program_exact_space = ExactComplexity::zero();
    let mut program_cardinality = Cardinality::Exact(BigUint::zero());

    for sc in &streams {
        program_time = program_time.max(sc.time_class.clone());
        program_exact_time = program_exact_time + sc.exact_time.clone();
        program_space = program_space.max(sc.space_class.clone());
        program_exact_space = program_exact_space + sc.exact_space.clone();
        program_cardinality = program_cardinality.max(sc.cardinality.clone());
    }

    ProgramComplexity {
        streams,
        program_time,
        program_exact_time,
        program_space,
        program_exact_space,
        program_cardinality,
        assumes_o1_user_fns: has_fn_declaration,
    }
}
