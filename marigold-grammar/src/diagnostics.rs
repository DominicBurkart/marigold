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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Diagnostic {
    pub range: ByteRange,
    pub severity: Severity,
    pub code: &'static str,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<String>,
}

impl Diagnostic {
    pub(crate) fn error(range: ByteRange, code: &'static str, message: String) -> Self {
        Self {
            range,
            severity: Severity::Error,
            code,
            message,
            help: None,
        }
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

    pub(crate) fn from_resolution(src: &str, e: &ResolutionError) -> Self {
        let code = match e {
            ResolutionError::CyclicDependency { .. } => "cyclic-bound",
            ResolutionError::UndefinedType(_) => "undefined-type",
            ResolutionError::InvalidOperation { .. } => "invalid-bound-operation",
            ResolutionError::DivisionByZero => "bound-division-by-zero",
            ResolutionError::BoundsViolation { .. } => "bounds-violation",
        };
        Self::whole(src, code, e.to_string())
    }
}

fn floor_char_boundary(src: &str, i: usize) -> usize {
    let mut i = i.min(src.len());
    while !src.is_char_boundary(i) {
        i -= 1;
    }
    i
}
