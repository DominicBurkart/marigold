//! Structured, position-aware diagnostics for Marigold source.
//!
//! Unlike [`crate::marigold_parse`], which stops at the first error and returns
//! a flat message, [`crate::marigold_check`] returns every diagnostic it can
//! find, each carrying a byte range into the original source. Ranges always lie
//! on `char` boundaries within the input, so they can be converted to any
//! editor position encoding.
//!
//! ```
//! use marigold_grammar::marigold_check;
//! use marigold_grammar::diagnostics::Severity;
//!
//! let src = "range(Color).return";
//! let diags = marigold_check(src);
//! assert_eq!(diags.len(), 1);
//! assert_eq!(diags[0].code, "undefined-enum");
//! assert_eq!(diags[0].severity, Severity::Error);
//! assert!(diags[0].range.end <= src.len());
//! ```

use crate::bound_resolution::ResolutionError;
use crate::span_index::SpanIndex;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
#[non_exhaustive]
pub enum Severity {
    Error,
}

impl Severity {
    /// ```
    /// use marigold_grammar::diagnostics::Severity;
    ///
    /// assert_eq!(Severity::Error.as_str(), "error");
    /// ```
    pub fn as_str(self) -> &'static str {
        match self {
            Severity::Error => "error",
        }
    }
}

/// Largest input, in bytes, that `marigold_check` will parse.
///
/// ```
/// use marigold_grammar::{diagnostics::MAX_CHECK_INPUT_BYTES, marigold_check};
///
/// let oversized = " ".repeat(MAX_CHECK_INPUT_BYTES + 1);
/// assert_eq!(marigold_check(&oversized)[0].code, "input-too-large");
/// ```
pub const MAX_CHECK_INPUT_BYTES: usize = 10 * 1024 * 1024;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Diagnostic {
    pub range: ByteRange,
    pub severity: Severity,
    pub code: &'static str,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<String>,
}

impl Diagnostic {
    /// ```
    /// let diags = marigold_grammar::marigold_check("range(Color).return");
    /// assert!(diags[0].is_error());
    /// ```
    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    pub(crate) fn error(range: ByteRange, code: &'static str, message: String) -> Self {
        Self {
            range,
            severity: Severity::Error,
            code,
            message,
            help: None,
        }
    }

    pub(crate) fn input_too_large(src: &str) -> Self {
        Self::whole(
            src,
            "input-too-large",
            format!(
                "input exceeds the {} MiB limit for diagnostics",
                MAX_CHECK_INPUT_BYTES / (1024 * 1024)
            ),
        )
    }

    pub(crate) fn whole(src: &str, code: &'static str, message: String) -> Self {
        Self::error(
            ByteRange {
                start: 0,
                end: src.len(),
            },
            code,
            message,
        )
    }

    pub(crate) fn from_pest<R: pest::RuleType>(src: &str, e: &pest::error::Error<R>) -> Self {
        let (start, end) = match e.location {
            pest::error::InputLocation::Pos(p) => {
                let start = floor_char_boundary(src, p);
                let end = src[start..]
                    .chars()
                    .next()
                    .map_or(start, |c| start + c.len_utf8());
                (start, end)
            }
            pest::error::InputLocation::Span((s, e)) => {
                let start = floor_char_boundary(src, s);
                (start, floor_char_boundary(src, e).max(start))
            }
        };
        Self::error(
            ByteRange { start, end },
            "syntax-error",
            e.variant.message().into_owned(),
        )
    }

    pub(crate) fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }

    pub(crate) fn from_resolution(src: &str, index: &SpanIndex, e: &ResolutionError) -> Vec<Self> {
        let refs_named = |names: &[&str]| -> Vec<ByteRange> {
            index
                .bound_type_refs
                .iter()
                .filter(|(n, _)| names.contains(&n.as_str()))
                .map(|(_, r)| *r)
                .collect()
        };
        let (code, ranges) = match e {
            ResolutionError::CyclicDependency { cycle } => {
                let names: Vec<&str> = cycle.iter().map(String::as_str).collect();
                ("cyclic-bound", refs_named(&names))
            }
            ResolutionError::UndefinedType(name) => ("undefined-type", refs_named(&[name])),
            ResolutionError::InvalidOperation { type_name, .. } => {
                ("invalid-bound-operation", refs_named(&[type_name]))
            }
            ResolutionError::DivisionByZero => ("bound-division-by-zero", Vec::new()),
            ResolutionError::BoundsViolation { field, .. } => (
                "bounds-violation",
                index
                    .bounded_field_types
                    .iter()
                    .filter(|(f, _)| f == field)
                    .map(|(_, r)| *r)
                    .collect(),
            ),
        };
        if ranges.is_empty() {
            return vec![Self::whole(src, code, e.to_string())];
        }
        ranges
            .into_iter()
            .map(|r| Self::error(r, code, e.to_string()))
            .collect()
    }
}

fn floor_char_boundary(src: &str, i: usize) -> usize {
    let mut i = i.min(src.len());
    while !src.is_char_boundary(i) {
        i -= 1;
    }
    i
}
