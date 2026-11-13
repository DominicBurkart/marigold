//! Per-stage stream lengths as a function of the size of an unknown-sized input.
//!
//! [`marigold_analyze_with_sizes`](crate::marigold_analyze_with_sizes) names the size of an input
//! and walks the program symbolically; nothing is ever executed. Each stage of each stream gets a
//! [`Resolution`], an interval `[lo, hi]` of [`Symbolic`] item counts, whose [`ResolutionKind`]
//! says how much is known:
//!
//! - `exact`: `lo == hi` and neither names a size. This is usually a number; a count too large
//!   to compute (see [`Symbolic::evaluate`]) stays written out, such as
//!   `P(1000000, 52429)`, and evaluates to `None` although it is exact.
//! - `symbolic`: `lo == hi` and both mention a named size, so the count is exact once the size is
//!   bound.
//! - `bounded`: `lo` and `hi` differ, typically after a `filter` or a `keep_first_n`.
//! - `unbound`: the length of an input is free. Binding it with [`InputSizes`] narrows the stage
//!   to `symbolic` or `bounded` (or `exact`). An unbound resolution has no upper bound (`hi` is
//!   `None`) and carries [`ReasonCode::UnboundInput`] but not
//!   [`ReasonCode::UndeclaredStreamVariable`].
//! - `unresolvable`: no binding can resolve the stage, as for an undeclared stream variable: `hi`
//!   is `None` and the reasons do not make it `unbound`.
//!
//! ```
//! use marigold_grammar::marigold_analyze_with_sizes;
//! use marigold_grammar::sizes::{InputSizes, ReasonCode, ResolutionKind};
//!
//! let src = "struct D { x: i32 }\nfn keep(d: &D) -> bool { true }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().filter(keep).return";
//! let kinds = |sizes: &InputSizes| -> Vec<ResolutionKind> {
//!     marigold_analyze_with_sizes(src, sizes).unwrap().streams[0]
//!         .stages
//!         .iter()
//!         .map(|stage| stage.resolution.kind())
//!         .collect()
//! };
//!
//! use ResolutionKind::{Bounded, Symbolic, Unbound};
//! assert_eq!(kinds(&InputSizes::new()), [Unbound, Unbound, Unbound]);
//! assert_eq!(kinds(&InputSizes::new().bind(0, "n").unwrap()), [Symbolic, Symbolic, Bounded]);
//!
//! let bound = marigold_analyze_with_sizes(src, &InputSizes::new().bind(0, "n").unwrap()).unwrap();
//! let filtered = &bound.streams[0].stages[2].resolution;
//! assert_eq!(filtered.hi().unwrap().to_string(), "$n");
//! assert_eq!(filtered.reasons(), [ReasonCode::OpaquePredicate]);
//! ```
//!
//! `Cardinality::Bounded` in the default [`ProgramComplexity`] is the same stream seen as one
//! scalar for the cost model (an upper bound), and its `Exact` and `Unknown` are not this
//! module's kinds. Read the stage resolutions here for how a count is known, and `program` for
//! the cost classes.
//!
//! Soundness: `exact` and `symbolic` assume the stream terminates and that `ok_or_panic` does not
//! panic; `fold` emits exactly one item and is assumed to terminate. A resolution is correct for
//! a program whose only unknown is the bound input's length. Known exception: the analyzer
//! reports one item for `permutations_with_replacement(0)` while the runtime yields none (#457).
//!
//! Precision: every resolution is sound, but some are wider than the true count. A
//! `keep_first_n(0)` over a bound input reports `min($n, 0)` rather than the number 0;
//! `filter(..).combinations(0)` reports `[1, C($n, 0)]` although the count is always 1; and
//! `permutations(0)` or `combinations(0)` over an unbound input stays `unbound` with `lo` 1,
//! although the count is exactly 1; and `ok().keep_first_n(0)` over a bound input reports
//! `bounded` `[0, min($n, 0)]` with `fallible_items`, although the count is always 0. Separately,
//! `permutations_with_replacement(0)` is reported as one item where the runtime yields none
//! (#457).
//!
//! With a bound input, `program_cardinality` joins every stream with the rule on
//! [`Cardinality::max`](crate::complexity::Cardinality::max) for values that mention a size: the
//! sum of the size-dependent terms plus the largest number-valued term, for example
//! `$n + P($n, 2)` for a variable consumed by a `map` and a `permutations(2)`.
//!
//! Inputs are numbered from 0 in source order, each stream-variable declaration and each unnamed
//! stream counting once and a `select_all` counting as a single input. This numbering is
//! provisional and is superseded when sizes become part of the language. Two inputs bound to the
//! same name share one size.
//!
//! [`ProgramComplexity`] is both serializable and deserializable; [`Resolution`] and
//! [`SymbolicAnalysis`] are serialize-only.

use std::collections::BTreeMap;
use std::fmt;

use num_bigint::BigUint;
use num_traits::{One, Zero};
use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};

use crate::complexity::{
    analyze_nodes_with, input_cardinality, input_nodes, InvalidSizeName, ProgramComplexity,
    SizeBindings, SizeName, Symbolic,
};
use crate::nodes::{StreamFunctionKind, TypedExpression};
use crate::parser::MarigoldParseError;

/// Why a stage is not exact. Serialized as snake_case strings that consumers may match on.
///
/// ```
/// use marigold_grammar::sizes::ReasonCode;
///
/// assert_eq!(serde_json::to_string(&ReasonCode::OpaquePredicate).unwrap(), "\"opaque_predicate\"");
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ReasonCode {
    /// The stream reads an input whose length is unknown and not bound to a size.
    UnboundInput,
    /// The stream consumes a variable that the program never declares.
    UndeclaredStreamVariable,
    /// A `filter` or `filter_map` keeps an unknown share of its items.
    OpaquePredicate,
    /// An `ok()` drops the items that fail to parse, an unknown share.
    FallibleItems,
}

/// How much is known about the length of a stream at one stage; see the module documentation.
///
/// ```
/// use marigold_grammar::sizes::ResolutionKind;
///
/// assert_eq!(serde_json::to_string(&ResolutionKind::Unbound).unwrap(), "\"unbound\"");
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ResolutionKind {
    /// `lo == hi` and neither names a size; usually a number, or a count written out because it
    /// is too large to compute.
    Exact,
    /// `lo == hi` and both mention a named size.
    Symbolic,
    /// `lo` and `hi` differ.
    Bounded,
    /// `hi` is `None` because an input is unbound; binding it narrows the stage.
    Unbound,
    /// `hi` is `None` and no binding can narrow it, as for an undeclared stream variable.
    Unresolvable,
}

/// A resolution evaluated under bindings, as returned by [`Resolution::evaluate`].
#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub struct EvaluatedRange {
    /// The fewest items the stage can produce, or `None` when `lo` mentions an unbound size or
    /// is too large to compute, whatever the kind.
    pub lo: Option<BigUint>,
    /// The most items the stage can produce, or `None` when `hi` is open-ended, mentions an
    /// unbound size (even if a `min` would cap it) or is too large to compute, whatever the
    /// kind.
    pub hi: Option<BigUint>,
}

/// The interval `[lo, hi]` of item counts a stage can produce, with the reasons it is not exact.
///
/// Every resolution, whether produced by the analysis or by [`Resolution::checked`], satisfies:
///
/// - `lo` and `hi` contain only constants, named sizes, `min` and the combinatorial
///   constructors, all non-decreasing in every size, so widening an input never narrows a stage.
/// - `lo <= hi` under every binding, as shown by the rules listed on [`Resolution::checked`].
/// - `hi` is `None` only with [`ReasonCode::UnboundInput`] or
///   [`ReasonCode::UndeclaredStreamVariable`] among the reasons.
/// - `exact` and `symbolic` stages (`lo == hi`, compared as written) have no reasons and every
///   other stage has at least one.
///
/// Constant bounds are folded to a number; symbolic bounds are left as written.
///
/// ```
/// use marigold_grammar::complexity::{SizeBindings, SizeName, Symbolic};
/// use marigold_grammar::sizes::{InvalidResolution, ReasonCode, Resolution, ResolutionKind};
/// use num_bigint::BigUint;
///
/// let number = |v: u32| Symbolic::Constant(BigUint::from(v));
/// let n = Symbolic::Param(SizeName::new("n").unwrap());
/// let at_most_three = Symbolic::Min(Box::new(n), Box::new(number(3)));
/// let resolution = Resolution::checked(
///     number(0),
///     Some(at_most_three),
///     vec![ReasonCode::OpaquePredicate],
/// )
/// .unwrap();
/// assert_eq!(resolution.kind(), ResolutionKind::Bounded);
///
/// assert_eq!(resolution.evaluate(&SizeBindings::new()).hi, None);
/// let bound = SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(10u32))]);
/// assert_eq!(resolution.evaluate(&bound).hi, Some(BigUint::from(3u32)));
///
/// assert_eq!(
///     Resolution::checked(number(5), Some(number(4)), vec![ReasonCode::OpaquePredicate]),
///     Err(InvalidResolution::LowerAboveUpper)
/// );
/// ```
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

fn floor(expression: &Symbolic) -> Option<BigUint> {
    fn sizes(expression: &Symbolic, bindings: &mut SizeBindings) {
        match expression {
            Symbolic::Param(name) => {
                bindings.insert(name.clone(), BigUint::zero());
            }
            Symbolic::Permutations { n, .. }
            | Symbolic::PermutationsWithReplacement { n, .. }
            | Symbolic::Combinations { n, .. }
            | Symbolic::Filtered(n) => sizes(n, bindings),
            Symbolic::Min(a, b) => {
                sizes(a, bindings);
                sizes(b, bindings);
            }
            Symbolic::Sum(parts) => parts.iter().for_each(|part| sizes(part, bindings)),
            Symbolic::Constant(_) | Symbolic::Unknown => {}
        }
    }
    let mut bindings = SizeBindings::new();
    sizes(expression, &mut bindings);
    expression.evaluate(&bindings)
}

fn names_a_size(expression: &Symbolic) -> bool {
    match expression {
        Symbolic::Param(_) => true,
        Symbolic::Constant(_) | Symbolic::Unknown => false,
        Symbolic::Filtered(n)
        | Symbolic::Permutations { n, .. }
        | Symbolic::PermutationsWithReplacement { n, .. }
        | Symbolic::Combinations { n, .. } => names_a_size(n),
        Symbolic::Min(a, b) => names_a_size(a) || names_a_size(b),
        Symbolic::Sum(parts) => parts.iter().any(names_a_size),
    }
}

fn provably_at_most(lo: &Symbolic, hi: &Symbolic) -> bool {
    if lo == hi {
        return true;
    }
    if let Some(low) = lo.try_evaluate() {
        if low.is_zero() {
            return true;
        }
        if let Some(least) = floor(hi) {
            return low <= least;
        }
    }
    match (lo, hi) {
        (_, Symbolic::Min(a, b)) => provably_at_most(lo, a) && provably_at_most(lo, b),
        (Symbolic::Min(a, b), _) => provably_at_most(a, hi) || provably_at_most(b, hi),
        (Symbolic::Permutations { n: a, k: j }, Symbolic::Permutations { n: b, k })
        | (
            Symbolic::PermutationsWithReplacement { n: a, k: j },
            Symbolic::PermutationsWithReplacement { n: b, k },
        )
        | (Symbolic::Combinations { n: a, k: j }, Symbolic::Combinations { n: b, k }) => {
            j == k && provably_at_most(a, b)
        }
        _ => false,
    }
}

/// Why [`Resolution::checked`] rejected a pair of bounds.
///
/// ```
/// use marigold_grammar::complexity::Symbolic;
/// use marigold_grammar::sizes::{InvalidResolution, Resolution};
///
/// let rejected = Resolution::checked(Symbolic::Unknown, None, vec![]).unwrap_err();
/// assert_eq!(rejected, InvalidResolution::NonMonotoneBound);
/// assert!(rejected.to_string().contains("min"));
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum InvalidResolution {
    /// A bound contains `?`, a filtered term or a sum.
    NonMonotoneBound,
    /// `lo <= hi` could not be shown under every binding.
    LowerAboveUpper,
    /// `hi` is `None` but no reason names an unbound input or an undeclared stream variable.
    OpenBoundWithoutCause,
    /// `lo == hi`, so the count is determined, yet reasons were given.
    ReasonOnDeterminedCount,
    /// `lo` and `hi` differ but no reason says why.
    RangeWithoutReason,
}

impl fmt::Display for InvalidResolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InvalidResolution::NonMonotoneBound => write!(
                f,
                "a resolution bound may contain only constants, sizes, min and the combinatorial constructors"
            ),
            InvalidResolution::LowerAboveUpper => write!(
                f,
                "the lower bound of a resolution is not provably at most its upper bound"
            ),
            InvalidResolution::OpenBoundWithoutCause => write!(
                f,
                "a resolution without an upper bound needs an unbound-input or undeclared-variable reason"
            ),
            InvalidResolution::ReasonOnDeterminedCount => write!(
                f,
                "a resolution whose bounds are equal cannot carry reasons"
            ),
            InvalidResolution::RangeWithoutReason => write!(
                f,
                "a resolution whose bounds differ needs at least one reason"
            ),
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
        let lo = simplify(lo);
        let hi = hi.map(simplify);
        if hi.as_ref() == Some(&lo) {
            reasons.clear();
        }
        reasons.sort();
        reasons.dedup();
        Resolution { lo, hi, reasons }
    }

    /// Builds a resolution from its bounds and reasons, sorting and deduplicating the reasons
    /// and folding constant bounds to numbers. It is rejected unless:
    ///
    /// - neither bound contains `?`, a filtered term or a sum
    ///   ([`InvalidResolution::NonMonotoneBound`]);
    /// - `lo <= hi` is shown under every binding by one of these rules, applied recursively
    ///   ([`InvalidResolution::LowerAboveUpper`]): the bounds are equal as written; `lo` is a
    ///   number no greater than `hi` with every size at 0; `hi` is `min(a, b)` with `lo <= a` and
    ///   `lo <= b`; `lo` is `min(a, b)` with `a <= hi` or `b <= hi`; or both are the same
    ///   combinatorial constructor with the same `k` and the inner `lo` is at most the inner
    ///   `hi`. Other pairs, such as `$m` and `$n`, are rejected even when they happen to be
    ///   ordered;
    /// - a missing `hi` comes with [`ReasonCode::UnboundInput`] or
    ///   [`ReasonCode::UndeclaredStreamVariable`] ([`InvalidResolution::OpenBoundWithoutCause`]);
    /// - equal bounds come with no reasons ([`InvalidResolution::ReasonOnDeterminedCount`]) and
    ///   different bounds with at least one ([`InvalidResolution::RangeWithoutReason`]).
    ///
    /// ```
    /// use marigold_grammar::complexity::{SizeName, Symbolic};
    /// use marigold_grammar::sizes::{InvalidResolution, ReasonCode, Resolution};
    ///
    /// let n = Symbolic::Param(SizeName::new("n").unwrap());
    /// let m = Symbolic::Param(SizeName::new("m").unwrap());
    /// let squared = Symbolic::PermutationsWithReplacement { n: Box::new(n.clone()), k: 2 };
    /// let reason = || vec![ReasonCode::OpaquePredicate];
    ///
    /// let capped = Symbolic::Min(Box::new(n.clone()), Box::new(Symbolic::Constant(3u32.into())));
    /// assert!(Resolution::checked(capped, Some(n.clone()), reason()).is_ok());
    /// assert_eq!(
    ///     Resolution::checked(squared, Some(n.clone()), reason()),
    ///     Err(InvalidResolution::LowerAboveUpper)
    /// );
    /// assert_eq!(
    ///     Resolution::checked(m, Some(n.clone()), reason()),
    ///     Err(InvalidResolution::LowerAboveUpper)
    /// );
    /// assert_eq!(
    ///     Resolution::checked(n.clone(), Some(n.clone()), reason()),
    ///     Err(InvalidResolution::ReasonOnDeterminedCount)
    /// );
    /// assert!(Resolution::checked(n.clone(), Some(n), vec![]).is_ok());
    /// ```
    pub fn checked(
        lo: Symbolic,
        hi: Option<Symbolic>,
        mut reasons: Vec<ReasonCode>,
    ) -> Result<Resolution, InvalidResolution> {
        if !monotone_only(&lo) || hi.as_ref().is_some_and(|hi| !monotone_only(hi)) {
            return Err(InvalidResolution::NonMonotoneBound);
        }
        let lo = simplify(lo);
        let hi = hi.map(simplify);
        match &hi {
            None => {
                if !reasons.iter().any(|reason| {
                    matches!(
                        reason,
                        ReasonCode::UnboundInput | ReasonCode::UndeclaredStreamVariable
                    )
                }) {
                    return Err(InvalidResolution::OpenBoundWithoutCause);
                }
            }
            Some(hi) => {
                if !provably_at_most(&lo, hi) {
                    return Err(InvalidResolution::LowerAboveUpper);
                }
                if *hi == lo && !reasons.is_empty() {
                    return Err(InvalidResolution::ReasonOnDeterminedCount);
                }
                if *hi != lo && reasons.is_empty() {
                    return Err(InvalidResolution::RangeWithoutReason);
                }
            }
        }
        reasons.sort();
        reasons.dedup();
        Ok(Resolution { lo, hi, reasons })
    }

    pub(crate) fn of_source(source: &Symbolic) -> Resolution {
        match source {
            Symbolic::Constant(_) | Symbolic::Param(_) => {
                Resolution::new(source.clone(), Some(source.clone()), Vec::new())
            }
            _ => Resolution::new(zero(), None, vec![ReasonCode::UnboundInput]),
        }
    }

    pub(crate) fn undeclared() -> Resolution {
        Resolution::new(zero(), None, vec![ReasonCode::UndeclaredStreamVariable])
    }

    /// The fewest items the stage can produce.
    ///
    /// ```
    /// use marigold_grammar::marigold_analyze_with_sizes;
    /// use marigold_grammar::sizes::InputSizes;
    ///
    /// let src = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().combinations(2).return";
    /// let analysis = marigold_analyze_with_sizes(src, &InputSizes::new().bind(0, "n").unwrap()).unwrap();
    /// let last = &analysis.streams[0].stages.last().unwrap().resolution;
    /// assert_eq!(last.lo().to_string(), "C($n, 2)");
    /// ```
    pub fn lo(&self) -> &Symbolic {
        &self.lo
    }

    /// The most items the stage can produce, or `None` when no upper bound is known.
    ///
    /// ```
    /// use marigold_grammar::marigold_analyze_with_sizes;
    /// use marigold_grammar::sizes::InputSizes;
    ///
    /// let src = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().return";
    /// let unbound = marigold_analyze_with_sizes(src, &InputSizes::new()).unwrap();
    /// assert_eq!(unbound.streams[0].stages[1].resolution.hi(), None);
    /// let bound = marigold_analyze_with_sizes(src, &InputSizes::new().bind(0, "rows").unwrap()).unwrap();
    /// assert_eq!(bound.streams[0].stages[1].resolution.hi().unwrap().to_string(), "$rows");
    /// ```
    pub fn hi(&self) -> Option<&Symbolic> {
        self.hi.as_ref()
    }

    /// Why the stage is not exact, sorted by code and without duplicates; empty for `exact` and
    /// `symbolic` stages.
    ///
    /// ```
    /// use marigold_grammar::marigold_analyze_with_sizes;
    /// use marigold_grammar::sizes::{InputSizes, ReasonCode};
    ///
    /// let src = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok().return";
    /// let analysis = marigold_analyze_with_sizes(src, &InputSizes::new()).unwrap();
    /// let last = &analysis.streams[0].stages[1].resolution;
    /// assert_eq!(last.reasons(), [ReasonCode::UnboundInput, ReasonCode::FallibleItems]);
    /// ```
    pub fn reasons(&self) -> &[ReasonCode] {
        &self.reasons
    }

    /// How much is known about the count; see [`ResolutionKind`] and the module documentation.
    ///
    /// ```
    /// use marigold_grammar::complexity::Symbolic;
    /// use marigold_grammar::sizes::{ReasonCode, Resolution, ResolutionKind};
    ///
    /// let open = Resolution::checked(Symbolic::Constant(0u32.into()), None, vec![ReasonCode::UnboundInput]).unwrap();
    /// assert_eq!(open.kind(), ResolutionKind::Unbound);
    /// let ghost = Resolution::checked(
    ///     Symbolic::Constant(0u32.into()),
    ///     None,
    ///     vec![ReasonCode::UnboundInput, ReasonCode::UndeclaredStreamVariable],
    /// )
    /// .unwrap();
    /// assert_eq!(ghost.kind(), ResolutionKind::Unresolvable);
    /// ```
    pub fn kind(&self) -> ResolutionKind {
        match &self.hi {
            None if self.reasons.contains(&ReasonCode::UnboundInput)
                && !self.reasons.contains(&ReasonCode::UndeclaredStreamVariable) =>
            {
                ResolutionKind::Unbound
            }
            None => ResolutionKind::Unresolvable,
            Some(hi) if *hi == self.lo => {
                if names_a_size(hi) {
                    ResolutionKind::Symbolic
                } else {
                    ResolutionKind::Exact
                }
            }
            Some(_) => ResolutionKind::Bounded,
        }
    }

    pub(crate) fn after(&self, kind: &StreamFunctionKind) -> Resolution {
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

    /// Evaluates both bounds under `bindings`. A bound that mentions an unbound size is `None`.
    ///
    /// ```
    /// use marigold_grammar::complexity::{SizeBindings, SizeName};
    /// use marigold_grammar::marigold_analyze_with_sizes;
    /// use marigold_grammar::sizes::InputSizes;
    /// use num_bigint::BigUint;
    ///
    /// let src = "struct D { x: i32 }\nfn keep(d: &D) -> bool { true }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().filter(keep).permutations(2).return";
    /// let analysis = marigold_analyze_with_sizes(src, &InputSizes::new().bind(0, "n").unwrap()).unwrap();
    /// let last = &analysis.streams[0].stages.last().unwrap().resolution;
    /// let bindings = SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(4u32))]);
    /// let range = last.evaluate(&bindings);
    /// assert_eq!(range.lo, Some(BigUint::from(0u32)));
    /// assert_eq!(range.hi, Some(BigUint::from(12u32)));
    /// assert_eq!(last.evaluate(&SizeBindings::new()).hi, None);
    /// ```
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

/// The resolution after one stage of a stream: its input, then each stream function in order.
///
/// ```
/// use marigold_grammar::marigold_analyze_with_sizes;
/// use marigold_grammar::sizes::InputSizes;
///
/// let analysis = marigold_analyze_with_sizes("range(0, 4).permutations(2).return", &InputSizes::new()).unwrap();
/// let stages: Vec<(&str, String)> = analysis.streams[0]
///     .stages
///     .iter()
///     .map(|s| (s.stage.as_str(), s.resolution.lo().to_string()))
///     .collect();
/// assert_eq!(stages, [("input", "4".to_string()), ("permutations(2)", "12".to_string())]);
/// ```
#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct StageResolution {
    /// `input` for the source, otherwise the stream function as the default description prints
    /// it, such as `filter(...)` or `permutations(2)`.
    pub stage: String,
    /// The item count after this stage.
    pub resolution: Resolution,
}

/// The stage resolutions of one stream.
///
/// [`SymbolicAnalysis::streams`] has one entry per stream-variable declaration and per output
/// stream, while `program.streams` has only the output streams, so the two lists differ in length
/// whenever a program declares a variable. Pair them with `program_stream`, never by position.
///
/// ```
/// use marigold_grammar::marigold_analyze_with_sizes;
/// use marigold_grammar::sizes::InputSizes;
///
/// let src = "xs = range(0, 3)\nxs.map(f).return\nrange(0, 5).return";
/// let analysis = marigold_analyze_with_sizes(src, &InputSizes::new()).unwrap();
/// assert_eq!(analysis.streams.len(), 3);
/// assert_eq!(analysis.program.streams.len(), 2);
/// let paired: Vec<(Option<&str>, usize, Option<usize>)> = analysis
///     .streams
///     .iter()
///     .map(|s| (s.name.as_deref(), s.expr_index, s.program_stream))
///     .collect();
/// assert_eq!(paired, [(Some("xs"), 0, None), (None, 1, Some(0)), (None, 2, Some(1))]);
/// assert_eq!(analysis.program.streams[1].description, analysis.streams[2].description);
/// ```
#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct StreamResolution {
    /// The position of the stream's expression among the program's top-level expressions,
    /// counting declarations of structs, enums and functions; the same index as
    /// [`NodeComplexity::expr_index`](crate::complexity::NodeComplexity::expr_index).
    pub expr_index: usize,
    /// The index of this stream in `program.streams`, or `None` for a stream-variable
    /// declaration, which `program.streams` does not list.
    pub program_stream: Option<usize>,
    /// The declared variable for `x = ...`, `None` for an output stream.
    pub name: Option<String>,
    /// The same description as the default analysis gives the stream.
    pub description: String,
    /// The input and then one entry per stream function, in order.
    pub stages: Vec<StageResolution>,
}

/// The default analysis of a program plus the resolution of every stage of every stream.
///
/// ```
/// use marigold_grammar::{marigold_analyze, marigold_analyze_with_sizes};
/// use marigold_grammar::sizes::InputSizes;
///
/// let src = "range(0, 3).map(f).return";
/// let analysis = marigold_analyze_with_sizes(src, &InputSizes::new()).unwrap();
/// assert_eq!(
///     serde_json::to_string(&analysis.program).unwrap(),
///     serde_json::to_string(&marigold_analyze(src).unwrap()).unwrap()
/// );
/// assert_eq!(analysis.streams[0].stages.len(), 2);
/// ```
#[derive(Clone, Debug, Serialize)]
#[non_exhaustive]
pub struct SymbolicAnalysis {
    /// The default analysis, with each bound input contributing its named size.
    pub program: ProgramComplexity,
    /// One entry per stream-variable declaration and per output stream, by `expr_index`.
    pub streams: Vec<StreamResolution>,
}

/// Why [`marigold_analyze_with_sizes`](crate::marigold_analyze_with_sizes) or
/// [`InputSizes::bind`] failed.
///
/// ```
/// use marigold_grammar::marigold_analyze_with_sizes;
/// use marigold_grammar::sizes::{InputSizes, SizeAnalysisError};
///
/// let known = marigold_analyze_with_sizes("range(0, 3).return", &InputSizes::new().bind(0, "n").unwrap());
/// assert!(matches!(known, Err(SizeAnalysisError::InputSizeKnown { ordinal: 0 })));
/// assert!(matches!(
///     marigold_analyze_with_sizes("range(0, ", &InputSizes::new()),
///     Err(SizeAnalysisError::Parse(_))
/// ));
/// ```
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum SizeAnalysisError {
    /// The program does not parse; the error is the one the default analysis reports.
    Parse(MarigoldParseError),
    /// A binding names an input the program does not have.
    UnknownInput {
        /// The ordinal that was bound.
        ordinal: usize,
        /// How many inputs the program has.
        inputs: usize,
    },
    /// A binding names an input whose size is already known, such as a constant `range`.
    InputSizeKnown {
        /// The ordinal that was bound.
        ordinal: usize,
    },
    /// The size name is not an ASCII letter or underscore followed by letters, digits or
    /// underscores; [`InvalidSizeName::name`] gives the rejected name.
    InvalidParamName(InvalidSizeName),
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
            SizeAnalysisError::InvalidParamName(error) => write!(f, "{error}"),
        }
    }
}

impl std::error::Error for SizeAnalysisError {}

impl From<MarigoldParseError> for SizeAnalysisError {
    fn from(error: MarigoldParseError) -> Self {
        SizeAnalysisError::Parse(error)
    }
}

/// Names for the sizes of inputs, by ordinal. Inputs are numbered from 0 in source order, a
/// `select_all` is one input, and only inputs whose size is unknown can be bound. Binding the same
/// name to two inputs makes them share one size; binding an ordinal again replaces its name.
///
/// ```
/// use marigold_grammar::marigold_analyze_with_sizes;
/// use marigold_grammar::sizes::{InputSizes, SizeAnalysisError};
///
/// let src = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().return";
/// let sizes = InputSizes::new().bind(0, "rows").unwrap();
/// assert!(marigold_analyze_with_sizes(src, &sizes).is_ok());
/// assert!(matches!(
///     marigold_analyze_with_sizes(src, &InputSizes::new().bind(3, "rows").unwrap()),
///     Err(SizeAnalysisError::UnknownInput { ordinal: 3, inputs: 1 })
/// ));
/// assert!(InputSizes::new().bind(0, "1 rows").is_err());
/// ```
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct InputSizes {
    by_ordinal: BTreeMap<usize, SizeName>,
}

impl InputSizes {
    /// No bindings: every input of unknown size stays unbound.
    ///
    /// ```
    /// use marigold_grammar::sizes::InputSizes;
    ///
    /// assert_eq!(InputSizes::new(), InputSizes::default());
    /// ```
    pub fn new() -> InputSizes {
        InputSizes::default()
    }

    /// Names the size of input `ordinal`, replacing an earlier name for the same ordinal. The
    /// ordinal is checked against the program only when it is analyzed.
    ///
    /// ```
    /// use marigold_grammar::sizes::{InputSizes, SizeAnalysisError};
    ///
    /// let renamed = InputSizes::new().bind(0, "a").unwrap().bind(0, "b").unwrap();
    /// assert_eq!(renamed, InputSizes::new().bind(0, "b").unwrap());
    /// assert!(matches!(
    ///     InputSizes::new().bind(0, "$n"),
    ///     Err(SizeAnalysisError::InvalidParamName(error)) if error.name() == "$n"
    /// ));
    /// ```
    pub fn bind(mut self, ordinal: usize, name: &str) -> Result<InputSizes, SizeAnalysisError> {
        let name = SizeName::new(name).map_err(SizeAnalysisError::InvalidParamName)?;
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
            |((expr_index, name, complexity), (program_stream, stages))| StreamResolution {
                expr_index,
                program_stream,
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

#[cfg(test)]
mod tests;
