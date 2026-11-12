use num_bigint::BigUint;
use num_traits::{One, Zero};
use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

use crate::complexity::{SizeBindings, Symbolic};
use crate::nodes::StreamFunctionKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReasonCode {
    UnboundInput,
    UndeclaredStreamVariable,
    OpaquePredicate,
    FallibleItems,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ResolutionKind {
    Exact,
    Symbolic,
    Bounded,
    Unresolvable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    match expression {
        Symbolic::Constant(value) => Symbolic::Constant(value.min(BigUint::from(limit))),
        other => Symbolic::Min(
            Box::new(other),
            Box::new(Symbolic::Constant(BigUint::from(limit))),
        ),
    }
}

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
    pub fn new(lo: Symbolic, hi: Option<Symbolic>, mut reasons: Vec<ReasonCode>) -> Resolution {
        reasons.sort();
        reasons.dedup();
        Resolution { lo, hi, reasons }
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
