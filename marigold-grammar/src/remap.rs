//! Remap rustc diagnostics and runtime locations to Marigold source (stub for the red commit).

use crate::diagnostics::ByteRange;
use crate::rustc_json::{Level, RustcDiagnostic, RustcSpan};
use crate::source_map::{GeneratedProgram, SegmentKind};
use serde::Serialize;
use std::path::Path;

/// 1-based line and column (in Unicode scalar values).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[non_exhaustive]
pub struct LineCol {
    /// Line.
    pub line: usize,
    /// Column.
    pub column: usize,
}

/// A location in the Marigold source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MarigoldLocation {
    /// Byte range.
    pub range: ByteRange,
    /// Start.
    pub start: LineCol,
    /// End.
    pub end: LineCol,
}

/// The segment a span belongs to.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SegmentRef {
    /// Kind.
    pub kind: SegmentKind,
    /// Expression index.
    pub expr_index: usize,
    /// Name.
    pub name: Option<String>,
}

/// Why a span could not be mapped.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum UnmappedReason {
    /// No span.
    NoSpan,
    /// Other file.
    ForeignFile,
    /// Outside the program.
    OutsideGeneratedRegion,
    /// Generated glue.
    Glue,
    /// Several expressions.
    MultipleExpressions,
    /// Generated file differs.
    StaleGeneratedFile,
    /// Invalid offsets.
    InvalidSpan,
    /// Source differs from the map.
    StaleSourceMap,
}

/// How precisely a span was mapped.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[non_exhaustive]
pub enum Precision {
    /// Byte-identical text.
    Exact,
    /// Whole segment.
    Segment,
    /// Whole expression.
    Expression,
    /// Not mapped.
    Unmapped {
        /// Why.
        reason: UnmappedReason,
    },
}

/// A span as rustc reported it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct ReportedSpan {
    /// File.
    pub file_name: String,
    /// Start byte.
    pub byte_start: usize,
    /// End byte.
    pub byte_end: usize,
    /// Start line.
    pub line_start: usize,
    /// Start column.
    pub column_start: usize,
    /// End line.
    pub line_end: usize,
    /// End column.
    pub column_end: usize,
}

/// A suggestion.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Suggestion {
    /// Replacement.
    pub replacement: String,
    /// Applicability.
    pub applicability: Option<String>,
    /// Applies to Marigold.
    pub applicable_to_marigold: bool,
}

/// A mapped span.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MappedSpan {
    /// Primary flag.
    pub is_primary: bool,
    /// Label.
    pub label: Option<String>,
    /// Marigold location.
    pub marigold: Option<MarigoldLocation>,
    /// Precision.
    pub precision: Precision,
    /// Segment.
    pub segment: Option<SegmentRef>,
    /// Raw span.
    pub raw: Option<ReportedSpan>,
    /// Macro name.
    pub via_macro: Option<String>,
    /// Suggestion.
    pub suggestion: Option<Suggestion>,
}

/// A mapped diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MappedDiagnostic {
    /// Level.
    pub level: Level,
    /// Code.
    pub code: Option<String>,
    /// Message.
    pub message: String,
    /// Primary span.
    pub primary: Option<MappedSpan>,
    /// Other spans.
    pub secondary: Vec<MappedSpan>,
    /// Children.
    pub children: Vec<MappedDiagnostic>,
    /// Rendered text.
    pub rendered: Option<String>,
}

impl MappedDiagnostic {
    /// First mapped span.
    pub fn anchor(&self) -> Option<&MappedSpan> {
        unimplemented!()
    }

    /// Warning that is probably not the user's.
    pub fn likely_generated_noise(&self) -> bool {
        unimplemented!()
    }
}

/// Unit of a column number.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ColumnUnit {
    /// Unicode scalar values.
    ScalarValues,
    /// Display width.
    DisplayWidth,
    /// UTF-8 bytes.
    Utf8Bytes,
}

/// A location found in text.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct TracedLocation {
    /// Token range.
    pub token: ByteRange,
    /// File.
    pub file: String,
    /// Line.
    pub line: usize,
    /// Column.
    pub column: Option<usize>,
    /// Unit.
    pub unit: ColumnUnit,
    /// Mapping.
    pub mapped: MappedSpan,
}

/// Maps rustc output to Marigold.
pub struct Remapper<'a> {
    _program: &'a GeneratedProgram,
    _source: &'a str,
    _is_generated: &'a dyn Fn(&str) -> bool,
}

impl<'a> Remapper<'a> {
    /// New remapper.
    pub fn new(
        program: &'a GeneratedProgram,
        source: &'a str,
        is_generated: &'a dyn Fn(&str) -> bool,
    ) -> Self {
        Remapper {
            _program: program,
            _source: source,
            _is_generated: is_generated,
        }
    }

    /// Declare a prefix.
    pub fn with_prefix(self, _prefix: &str) -> Self {
        unimplemented!()
    }

    /// Remap a diagnostic.
    pub fn remap(&self, _d: &RustcDiagnostic) -> MappedDiagnostic {
        unimplemented!()
    }

    /// Remap a span.
    pub fn map_span(&self, _s: &RustcSpan) -> MappedSpan {
        unimplemented!()
    }

    /// Locate a runtime position.
    pub fn locate(
        &self,
        _file: &str,
        _line: usize,
        _column: Option<usize>,
        _unit: ColumnUnit,
    ) -> MappedSpan {
        unimplemented!()
    }

    /// Scan text.
    pub fn remap_trace(&self, _text: &str) -> Vec<TracedLocation> {
        unimplemented!()
    }
}

/// Lexical path comparison.
pub fn lexical_path_matches(_root: &Path, _file_name: &str, _generated: &Path) -> bool {
    unimplemented!()
}
