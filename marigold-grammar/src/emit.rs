//! The JSON document printed by `marigold emit --format json`: a Marigold program, the
//! Rust code generated from it and the [`SourceMap`] between them, with line and column
//! positions.
//!
//! [`EmitDocument`] only holds data and implements [`serde::Serialize`]; the command line
//! tool prints it with `serde_json::to_string_pretty`. Other tools (a source viewer, an
//! editor plugin, an agent) read that JSON, so its layout is versioned and documented
//! here.
//!
//! # Schema, version 1
//!
//! ```text
//! Document := { "version": 1,                        // integer, EMIT_SCHEMA_VERSION
//!               "source":  string,                   // the Marigold program, verbatim
//!               "rust":    string,                   // the generated code, == marigold_parse(source)
//!               "segments": [ Segment ] }            // generated order, outer segments first
//! Segment  := { "kind": string,                      // snake_case of SegmentKind, e.g. "stream_variable"
//!               "name": string | null,               // declared name; null for output streams
//!               "expr_index": integer,               // position of the top-level expression in the source
//!               "parent": integer | null,            // index into "segments"; always less than this index
//!               "source": Range,                     // where the expression is, in "source"
//!               "generated": Range }                 // where its code is, in "rust"
//! Range    := { "start": integer, "end": integer,    // 0-based UTF-8 byte offsets, half-open, start < end
//!               "start_line": integer, "start_column": integer,   // 1-based; columns count Unicode scalar values
//!               "end_line": integer,   "end_column": integer }    // position of the (exclusive) end offset
//! ```
//!
//! Top-level `source` and `rust` are the two texts; `segments[i].source` and
//! `segments[i].generated` are ranges into them. The round trip is normative:
//! `document.source[seg.source.start..seg.source.end]` and
//! `document.rust[seg.generated.start..seg.generated.end]`, as byte slices of the
//! UTF-8 strings, are the expression and the code it produced.
//!
//! * **Offsets are bytes, columns are scalar values.** A column counts Unicode scalar
//!   values like `rustc` does, so a multibyte character takes one column but several
//!   offsets. The `\r` of a `\r\n` line ending takes a column; a line ends at `\n`.
//!   Consumers that need UTF-16 or byte columns can convert from the embedded texts
//!   with [`crate::line_index::LineIndex`].
//! * **The offsets are valid for the embedded texts.** `source` is the exact input
//!   (not trimmed, line endings and a byte order mark preserved), and `rust` is the
//!   code exactly as [`crate::marigold_generate`] returned it. Inside `m!()` neither
//!   exists; see [`crate::source_map`].
//! * **`segments` is in generated order**, the order of [`SourceMap::segments`]: ascending
//!   `generated.start`, an enclosing segment before the segments inside it. That is not
//!   source order. `parent` is the index of the innermost earlier segment whose
//!   `generated` range contains this one (`null` for top-level segments), so it is
//!   always smaller than the segment's own index. Every segment of a program built by
//!   [`crate::marigold_generate`] is currently top-level.
//! * A stream variable has two segments with the same `expr_index` and `source`: its
//!   declaration and its runner.
//! * The Rust code of a segment can span several lines, and can start in the middle of a
//!   line (the code after the preamble is indented).
//!
//! # Compatibility
//!
//! * Producers (this module) write exactly the keys above for `"version": 1`. Adding a
//!   key or a `kind` does not change the version; removing or renaming a key, changing a
//!   unit or changing the meaning of a field does.
//! * Consumers must check `"version"`, ignore keys they do not know, treat unknown
//!   `kind` strings as opaque and not rely on the order of keys. They should not assume
//!   `parent` is always `null`.
//!
//! ```
//! use marigold_grammar::{emit::EmitDocument, marigold_generate};
//!
//! let src = "x = range(0, 5)\nx.return";
//! let program = marigold_generate(src).unwrap();
//! let doc = EmitDocument::new(src, &program.rust, &program.map);
//! let json = serde_json::to_value(&doc).unwrap();
//! assert_eq!(json["version"], 1);
//! let first = &json["segments"][0];
//! assert_eq!(first["kind"], "stream_variable");
//! assert_eq!(first["source"]["end_column"], 16);
//! assert_eq!(first["parent"], serde_json::Value::Null);
//! ```

use crate::diagnostics::ByteRange;
use crate::line_index::{ColumnUnit, LineIndex};
use crate::source_map::{SegmentKind, SourceMap};
use serde::Serialize;

/// The `"version"` written to every document. See the [module documentation](self) for
/// what changes it.
///
/// ```
/// assert_eq!(marigold_grammar::emit::EMIT_SCHEMA_VERSION, 1);
/// ```
pub const EMIT_SCHEMA_VERSION: u32 = 1;

/// A byte range with the line and column of both ends. See the
/// [module documentation](self) for the units.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct EmitRange {
    /// First byte, 0-based.
    pub start: usize,
    /// One past the last byte.
    pub end: usize,
    /// Line of `start`, 1-based.
    pub start_line: usize,
    /// Column of `start` in Unicode scalar values, 1-based.
    pub start_column: usize,
    /// Line of `end`, 1-based.
    pub end_line: usize,
    /// Column of `end` in Unicode scalar values, 1-based.
    pub end_column: usize,
}

impl EmitRange {
    /// Offsets past the end of the text are reported as they are, but their line and
    /// column are those of the end of the text (see [`LineIndex::line_col`]).
    fn new(range: ByteRange, index: &LineIndex<'_>) -> EmitRange {
        let start = index.line_col(range.start, ColumnUnit::Chars);
        let end = index.line_col(range.end, ColumnUnit::Chars);
        EmitRange {
            start: range.start,
            end: range.end,
            start_line: start.line,
            start_column: start.col,
            end_line: end.line,
            end_column: end.col,
        }
    }

    fn contains(&self, inner: &EmitRange) -> bool {
        self.start <= inner.start && inner.end <= self.end
    }
}

/// One entry of [`EmitDocument::segments`]: a [`crate::source_map::Segment`] with
/// positions and the index of its parent.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct EmitSegment {
    /// What the expression is; serialized as its snake_case name.
    pub kind: SegmentKind,
    /// The declared name, `null` for output streams.
    pub name: Option<String>,
    /// Position of the top-level expression in the source.
    pub expr_index: usize,
    /// Index into [`EmitDocument::segments`] of the innermost enclosing segment.
    pub parent: Option<usize>,
    /// Where the expression is in [`EmitDocument::source`].
    pub source: EmitRange,
    /// Where its code is in [`EmitDocument::rust`].
    pub generated: EmitRange,
}

/// A Marigold program, its generated Rust and the map between them, ready to serialize.
/// The layout is described in the [module documentation](self).
///
/// Field order is the key order of the serialized document.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct EmitDocument {
    /// Always [`EMIT_SCHEMA_VERSION`].
    pub version: u32,
    /// The Marigold program the map's source ranges refer to, verbatim.
    pub source: String,
    /// The generated Rust the map's generated ranges refer to.
    pub rust: String,
    /// The segments of the map, in the order of [`SourceMap::segments`].
    pub segments: Vec<EmitSegment>,
}

impl EmitDocument {
    /// Combine a program with the code and map [`crate::marigold_generate`] produced
    /// for it.
    ///
    /// Takes `O(n + s)` for `n` bytes of text and `s` segments (plus a scan of the
    /// line for each position when the text is not ASCII, see [`LineIndex`]). Nothing
    /// is checked: if `map` does not belong to `source` and `rust`, ranges outside a
    /// text or inside a character keep their offsets, and their line and column are
    /// clamped like [`LineIndex::line_col`] does. A segment is nested under the
    /// closest earlier segment whose generated range contains its own; segments that
    /// only partly overlap are not nested.
    ///
    /// ```
    /// use marigold_grammar::{emit::EmitDocument, marigold_generate};
    ///
    /// let src = "range(0, 3)\n  .return";
    /// let program = marigold_generate(src).unwrap();
    /// let doc = EmitDocument::new(src, &program.rust, &program.map);
    /// assert_eq!(doc.segments.len(), 1);
    /// let range = &doc.segments[0].source;
    /// assert_eq!((range.start_line, range.start_column), (1, 1));
    /// assert_eq!((range.end_line, range.end_column), (2, 10));
    /// ```
    pub fn new(source: &str, rust: &str, map: &SourceMap) -> EmitDocument {
        let source_index = LineIndex::new(source);
        let rust_index = LineIndex::new(rust);

        let mut segments: Vec<EmitSegment> = Vec::with_capacity(map.segments().len());
        // Indexes of the segments that enclose the segment being added, outermost first.
        let mut open: Vec<usize> = Vec::new();
        for segment in map.segments() {
            let generated = EmitRange::new(segment.generated, &rust_index);
            // Segments are in pre-order (ascending start), so an open segment that does
            // not contain this one can be dropped: any later segment inside it would
            // also be inside this one, which starts earlier and, by not being nested in
            // it, ends later.
            while open
                .last()
                .is_some_and(|&top| !segments[top].generated.contains(&generated))
            {
                open.pop();
            }
            segments.push(EmitSegment {
                kind: segment.kind,
                name: segment.name.clone(),
                expr_index: segment.expr_index,
                parent: open.last().copied(),
                source: EmitRange::new(segment.source, &source_index),
                generated,
            });
            open.push(segments.len() - 1);
        }

        EmitDocument {
            version: EMIT_SCHEMA_VERSION,
            source: source.to_string(),
            rust: rust.to_string(),
            segments,
        }
    }
}
