use std::collections::BTreeMap;
use std::fmt;

use num_bigint::BigUint;
use num_traits::{One, Zero};
use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

use crate::complexity::{
    analyze_nodes_with, input_cardinality, input_nodes, ProgramComplexity, SizeBindings, SizeName,
    Symbolic,
};
use crate::nodes::{StreamFunctionKind, TypedExpression};
use crate::parser::MarigoldParseError;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ReasonCode {
    UnboundInput,
    UndeclaredStreamVariable,
    OpaquePredicate,
    FallibleItems,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ResolutionKind {
    Exact,
    Symbolic,
    Bounded,
    Unbound,
    Unresolvable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct EvaluatedRange {
    pub lo: Option<BigUint>,
    pub hi: Option<BigUint>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Resolution {
    lo: Symbolic,
    hi: Option<Symbolic>,
    reasons: Vec<ReasonCode>,
}

fn zero() -> Symbolic {
    Symbolic::Constant(BigUint::zero())
}

fn one() -> Symbolic {
    Symbolic::Constant(BigUint::one())
}

fn cap(expression: Symbolic, limit: u64) -> Symbolic {
    Symbolic::Min(
        Box::new(expression),
        Box::new(Symbolic::Constant(BigUint::from(limit))),
    )
}

fn simplify(expression: Symbolic) -> Symbolic {
    match expression.try_evaluate() {
        Some(value) => Symbolic::Constant(value),
        None => expression,
    }
}

fn monotone_only(expression: &Symbolic) -> bool {
    match expression {
        Symbolic::Constant(_) | Symbolic::Param(_) => true,
        Symbolic::Permutations { n, .. }
        | Symbolic::PermutationsWithReplacement { n, .. }
        | Symbolic::Combinations { n, .. } => monotone_only(n),
        Symbolic::Min(a, b) => monotone_only(a) && monotone_only(b),
        Symbolic::Filtered(_) | Symbolic::Unknown | Symbolic::Sum(_) => false,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InvalidResolution {
    NonMonotoneBound,
    LowerAboveUpper,
}

impl fmt::Display for InvalidResolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InvalidResolution::NonMonotoneBound => write!(
                f,
                "a resolution bound may contain only constants, sizes, min and the combinatorial constructors"
            ),
            InvalidResolution::LowerAboveUpper => {
                write!(f, "the lower bound of a resolution exceeds its upper bound")
            }
        }
    }
}

impl std::error::Error for InvalidResolution {}

fn map_size(expression: Symbolic, kind: &StreamFunctionKind) -> Symbolic {
    match kind {
        StreamFunctionKind::Permutations(k) => Symbolic::Permutations {
            n: Box::new(expression),
            k: *k,
        },
        StreamFunctionKind::PermutationsWithReplacement(k) => {
            Symbolic::PermutationsWithReplacement {
                n: Box::new(expression),
                k: *k,
            }
        }
        StreamFunctionKind::Combinations(k) => Symbolic::Combinations {
            n: Box::new(expression),
            k: *k,
        },
        _ => expression,
    }
}

impl Resolution {
    pub(crate) fn new(
        lo: Symbolic,
        hi: Option<Symbolic>,
        mut reasons: Vec<ReasonCode>,
    ) -> Resolution {
        reasons.sort();
        reasons.dedup();
        Resolution {
            lo: simplify(lo),
            hi: hi.map(simplify),
            reasons,
        }
    }

    pub fn checked(
        lo: Symbolic,
        hi: Option<Symbolic>,
        reasons: Vec<ReasonCode>,
    ) -> Result<Resolution, InvalidResolution> {
        if !monotone_only(&lo) || hi.as_ref().is_some_and(|hi| !monotone_only(hi)) {
            return Err(InvalidResolution::NonMonotoneBound);
        }
        if let (Some(low), Some(Some(high))) =
            (lo.try_evaluate(), hi.as_ref().map(|h| h.try_evaluate()))
        {
            if low > high {
                return Err(InvalidResolution::LowerAboveUpper);
            }
        }
        Ok(Resolution::new(lo, hi, reasons))
    }

    pub fn of_source(source: &Symbolic) -> Resolution {
        match source {
            Symbolic::Constant(_) | Symbolic::Param(_) => {
                Resolution::new(source.clone(), Some(source.clone()), Vec::new())
            }
            _ => Resolution::new(zero(), None, vec![ReasonCode::UnboundInput]),
        }
    }

    pub fn undeclared() -> Resolution {
        Resolution::new(zero(), None, vec![ReasonCode::UndeclaredStreamVariable])
    }

    pub fn lo(&self) -> &Symbolic {
        &self.lo
    }

    pub fn hi(&self) -> Option<&Symbolic> {
        self.hi.as_ref()
    }

    pub fn reasons(&self) -> &[ReasonCode] {
        &self.reasons
    }

    pub fn kind(&self) -> ResolutionKind {
        match &self.hi {
            None if self.reasons.contains(&ReasonCode::UnboundInput)
                && !self.reasons.contains(&ReasonCode::UndeclaredStreamVariable) =>
            {
                ResolutionKind::Unbound
            }
            None => ResolutionKind::Unresolvable,
            Some(hi) if *hi == self.lo => {
                if hi.try_evaluate().is_some() {
                    ResolutionKind::Exact
                } else {
                    ResolutionKind::Symbolic
                }
            }
            Some(_) => ResolutionKind::Bounded,
        }
    }

    pub fn after(&self, kind: &StreamFunctionKind) -> Resolution {
        match kind {
            StreamFunctionKind::Map | StreamFunctionKind::OkOrPanic => self.clone(),
            StreamFunctionKind::Filter | StreamFunctionKind::FilterMap => {
                self.filtered(ReasonCode::OpaquePredicate)
            }
            StreamFunctionKind::Ok => self.filtered(ReasonCode::FallibleItems),
            StreamFunctionKind::Fold => Resolution::new(one(), Some(one()), Vec::new()),
            StreamFunctionKind::KeepFirstN(k) => Resolution::new(
                cap(self.lo.clone(), *k),
                Some(match &self.hi {
                    Some(hi) => cap(hi.clone(), *k),
                    None => Symbolic::Constant(BigUint::from(*k)),
                }),
                self.reasons.clone(),
            ),
            StreamFunctionKind::Permutations(_)
            | StreamFunctionKind::PermutationsWithReplacement(_)
            | StreamFunctionKind::Combinations(_) => Resolution::new(
                map_size(self.lo.clone(), kind),
                self.hi.clone().map(|hi| map_size(hi, kind)),
                self.reasons.clone(),
            ),
        }
    }

    fn filtered(&self, reason: ReasonCode) -> Resolution {
        let mut reasons = self.reasons.clone();
        reasons.push(reason);
        Resolution::new(zero(), self.hi.clone(), reasons)
    }

    pub fn evaluate(&self, bindings: &SizeBindings) -> EvaluatedRange {
        EvaluatedRange {
            lo: self.lo.evaluate(bindings),
            hi: self.hi.as_ref().and_then(|hi| hi.evaluate(bindings)),
        }
    }
}

impl Serialize for Resolution {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Resolution", 4)?;
        state.serialize_field("kind", &self.kind())?;
        state.serialize_field("lo", &self.lo.to_string())?;
        state.serialize_field("hi", &self.hi.as_ref().map(|hi| hi.to_string()))?;
        state.serialize_field("reasons", &self.reasons)?;
        state.end()
    }
}

#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct StageResolution {
    pub stage: String,
    pub resolution: Resolution,
}

#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct StreamResolution {
    pub expr_index: usize,
    pub name: Option<String>,
    pub description: String,
    pub stages: Vec<StageResolution>,
}

#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct SymbolicAnalysis {
    pub program: ProgramComplexity,
    pub streams: Vec<StreamResolution>,
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum SizeAnalysisError {
    Parse(MarigoldParseError),
    UnknownInput { ordinal: usize, inputs: usize },
    InputSizeKnown { ordinal: usize },
    InvalidParamName(String),
}

impl fmt::Display for SizeAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SizeAnalysisError::Parse(error) => write!(f, "{error}"),
            SizeAnalysisError::UnknownInput { ordinal, inputs } => write!(
                f,
                "input {ordinal} does not exist: the program has {inputs} input(s), numbered from 0 in source order"
            ),
            SizeAnalysisError::InputSizeKnown { ordinal } => write!(
                f,
                "input {ordinal} has a size known at analysis time, so it cannot be bound to a named size"
            ),
            SizeAnalysisError::InvalidParamName(name) => write!(
                f,
                "{name:?} is not a valid size name: use an ASCII letter or underscore followed by letters, digits or underscores"
            ),
        }
    }
}

impl std::error::Error for SizeAnalysisError {}

impl From<MarigoldParseError> for SizeAnalysisError {
    fn from(error: MarigoldParseError) -> Self {
        SizeAnalysisError::Parse(error)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InputSizes {
    by_ordinal: BTreeMap<usize, SizeName>,
}

impl InputSizes {
    pub fn new() -> InputSizes {
        InputSizes::default()
    }

    pub fn bind(mut self, ordinal: usize, name: &str) -> Result<InputSizes, SizeAnalysisError> {
        let name = SizeName::new(name).map_err(|e| SizeAnalysisError::InvalidParamName(e.0))?;
        self.by_ordinal.insert(ordinal, name);
        Ok(self)
    }
}

pub(crate) fn analyze_with_sizes(
    expressions: &[TypedExpression],
    sizes: &InputSizes,
) -> Result<SymbolicAnalysis, SizeAnalysisError> {
    let inputs = input_nodes(expressions);
    for ordinal in sizes.by_ordinal.keys() {
        match inputs.get(*ordinal) {
            None => {
                return Err(SizeAnalysisError::UnknownInput {
                    ordinal: *ordinal,
                    inputs: inputs.len(),
                })
            }
            Some(input) if !matches!(input_cardinality(input), Symbolic::Unknown) => {
                return Err(SizeAnalysisError::InputSizeKnown { ordinal: *ordinal })
            }
            Some(_) => {}
        }
    }

    let analysis = analyze_nodes_with(expressions, &|ordinal, input| match sizes
        .by_ordinal
        .get(&ordinal)
    {
        Some(name) => Symbolic::Param(name.clone()),
        None => input_cardinality(input),
    });
    let streams = analysis
        .nodes
        .into_iter()
        .zip(analysis.traces)
        .map(
            |((expr_index, name, complexity), stages)| StreamResolution {
                expr_index,
                name,
                description: complexity.description,
                stages,
            },
        )
        .collect();
    Ok(SymbolicAnalysis {
        program: analysis.program,
        streams,
    })
}
