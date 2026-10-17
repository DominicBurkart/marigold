//! Map generated Rust code back to the Marigold source it came from.
//!
//! [`crate::marigold_generate`] returns the same Rust code as [`crate::marigold_parse`]
//! together with a [`SourceMap`]: one [`Segment`] per piece of generated code that
//! stands for one top-level Marigold expression, in the order the pieces appear in
//! the generated code. That order is not the order of the Marigold source: structs,
//! enums, functions, stream variables, output streams and stream variable runners
//! are emitted in sections.
//!
//! All offsets are byte offsets and all ranges are half-open (`start..end`), like
//! the ranges in [`crate::diagnostics`]. Text that belongs to no expression (the
//! code before the first segment, the separators between segments and the code
//! that wires the streams together) is not covered by any segment.
//!
//! # What the offsets refer to
//!
//! * [`Segment::source`] is valid only against the exact string passed to
//!   [`crate::marigold_generate`]. Inside `m!()` that is not the text in your `.rs` file: the
//!   macro receives rustc's re-printed `TokenStream`, with comments dropped and whitespace
//!   normalized.
//! * [`Segment::generated`] is valid only against [`GeneratedProgram::rust`]. The `m!()` macro
//!   wraps that code in `{\n` ... `\n}\n` and re-lexes it, and every token it produces carries a
//!   call-site span, so rustc diagnostics can never point into the generated code.
//!
//! To map a rustc diagnostic back to Marigold, write `rust` verbatim to a real file, compile
//! that file and look the diagnostic's byte offset up with
//! [`SourceMap::source_for_generated`].
//!
//! ```
//! use marigold_grammar::marigold_generate;
//! use marigold_grammar::source_map::SegmentKind;
//!
//! let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return";
//! let program = marigold_generate(src).unwrap();
//!
//! let function = &program.map.segments()[0];
//! assert_eq!(function.kind, SegmentKind::Function);
//! assert_eq!(&src[function.source.start..function.source.end], "fn double(x: i32) -> i32 { x * 2 }");
//! assert!(program.rust[function.generated.start..function.generated.end].starts_with("const fn double("));
//!
//! // From generated code back to Marigold: find the expression that produced a byte.
//! let offset = program.rust.find("returning_stream_0").unwrap();
//! let origin = program.map.source_for_generated(offset).unwrap();
//! assert_eq!(&src[origin.source.start..origin.source.end], "range(0, 3).map(double).return");
//!
//! // From Marigold to generated code: find what a source range turned into.
//! let hits = program.map.generated_for_source(origin.source);
//! assert_eq!(hits, vec![origin]);
//! ```

use crate::diagnostics::ByteRange;
use serde::Serialize;
use std::cmp::Reverse;

// Work counters for the complexity tests, in the style of `resolver.rs`: the tests assert
// how much work construction and lookups do, which does not depend on how loaded the machine is.
#[cfg(test)]
thread_local! {
    /// Key evaluations in the two sorts plus the steps of the parent-link pass.
    static BUILD_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Parent links followed by `source_for_generated`.
    static WALK_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Segments `generated_for_source` looked at beyond the two binary searches.
    static SCANNED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
fn count(counter: &'static std::thread::LocalKey<std::cell::Cell<usize>>) {
    counter.with(|c| c.set(c.get() + 1));
}

/// What kind of Marigold expression a [`Segment`] stands for.
///
/// A stream variable (`x = range(0, 5)`) is written to the generated code twice, so
/// it has two segments: a [`SegmentKind::StreamVariable`] for the declaration and a
/// [`SegmentKind::StreamVariableRunner`] for the task that drives it.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::source_map::SegmentKind;
///
/// let program = marigold_generate("x = range(0, 5)\nx.return").unwrap();
/// let kinds: Vec<_> = program.map.segments().iter().map(|s| s.kind).collect();
/// assert_eq!(
///     kinds,
///     [SegmentKind::StreamVariable, SegmentKind::ReturningStream, SegmentKind::StreamVariableRunner]
/// );
/// assert_eq!(serde_json::to_string(&SegmentKind::StreamVariableRunner).unwrap(), "\"stream_variable_runner\"");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum SegmentKind {
    /// `struct Name { ... }`
    Struct,
    /// `enum Name { ... }`
    Enum,
    /// `fn name(...) -> T { ... }`
    Function,
    /// The declaration of a stream variable, like `x = range(0, 5)`.
    StreamVariable,
    /// The task that drives a stream variable. Its source is the declaration.
    StreamVariableRunner,
    /// A stream that ends in `.return`, like `range(0, 5).return` or `x.return`.
    ReturningStream,
    /// A stream that ends in a sink, like `x.write_file("out.csv", csv)`.
    NonReturningStream,
}

/// One piece of generated code and the Marigold expression it came from.
///
/// ```
/// use marigold_grammar::diagnostics::ByteRange;
/// use marigold_grammar::source_map::{Segment, SegmentKind};
///
/// let segment = Segment::new(
///     ByteRange { start: 50, end: 90 },
///     ByteRange { start: 0, end: 19 },
///     SegmentKind::ReturningStream,
///     0,
///     None,
/// );
/// assert_eq!(segment.generated.start, 50);
/// assert_eq!(segment.name, None);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Segment {
    /// Where the code was written in the generated Rust. Exactly the bytes emitted for
    /// this expression: the separator that follows it is not included, but nothing is
    /// trimmed either. A struct's code starts with a newline and indentation, so a
    /// highlighter should trim the text it shows.
    pub generated: ByteRange,
    /// The whole Marigold expression, without trailing whitespace.
    pub source: ByteRange,
    /// What the expression is.
    pub kind: SegmentKind,
    /// Position of the expression among all top-level expressions of the program, in
    /// source order. This is the `expr_index` of [`crate::complexity::NodeComplexity`]
    /// and [`crate::symbols::SymbolDeclaration`].
    pub expr_index: usize,
    /// The name the expression declares: the struct, enum, function or stream variable.
    /// `None` for output streams, including a named one like `x.return`.
    pub name: Option<String>,
}

impl Segment {
    /// Build a segment, for example to hand-build a [`SourceMap`] in a test.
    ///
    /// Segments are expected to gain fields in later releases (finer-grained segments),
    /// and the arguments of this constructor may change with them. Outside tests, prefer
    /// the segments [`crate::marigold_generate`] returns over building your own.
    ///
    /// ```
    /// use marigold_grammar::diagnostics::ByteRange;
    /// use marigold_grammar::source_map::{Segment, SegmentKind};
    ///
    /// let segment = Segment::new(
    ///     ByteRange { start: 50, end: 80 },
    ///     ByteRange { start: 0, end: 16 },
    ///     SegmentKind::StreamVariable,
    ///     0,
    ///     Some("x".to_string()),
    /// );
    /// assert_eq!(segment.name.as_deref(), Some("x"));
    /// assert_eq!(segment.kind, SegmentKind::StreamVariable);
    /// ```
    pub fn new(
        generated: ByteRange,
        source: ByteRange,
        kind: SegmentKind,
        expr_index: usize,
        name: Option<String>,
    ) -> Segment {
        Segment {
            generated,
            source,
            kind,
            expr_index,
            name,
        }
    }
}

/// Segments of one generated program, with fast lookups in both directions.
///
/// Serializes as `{"segments": [...]}` in the order of [`SourceMap::segments`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SourceMap {
    /// Pre-order of the generated side: ascending `generated.start`, and for equal
    /// starts the larger `generated.end` first (outer before inner).
    segments: Vec<Segment>,
    /// For each segment, the closest earlier segment whose generated range contains it.
    #[serde(skip)]
    parents: Vec<Option<usize>>,
    /// Indexes into `segments`, ascending by `source.start`.
    #[serde(skip)]
    by_source: Vec<usize>,
    /// `max_source_end[k]` is the largest `source.end` among `by_source[..=k]`.
    #[serde(skip)]
    max_source_end: Vec<usize>,
}

impl SourceMap {
    /// Build a map from segments in any order; they are sorted into generated order
    /// (outer before inner) and the lookup indexes are built in `O(n log n)`.
    ///
    /// Segments of a map built by [`crate::marigold_generate`] never overlap. Hand-built
    /// maps may nest segments (the innermost one wins in
    /// [`source_for_generated`](Self::source_for_generated)). Any input is accepted and
    /// nothing panics, but "innermost" is only defined when the generated ranges nest or
    /// are disjoint: lookups on partially overlapping ranges are best effort. The lookups
    /// are fast as documented only for such maps; see
    /// [`generated_for_source`](Self::generated_for_source) for the source side.
    ///
    /// ```
    /// use marigold_grammar::diagnostics::ByteRange;
    /// use marigold_grammar::source_map::{Segment, SegmentKind, SourceMap};
    ///
    /// let range = |start, end| ByteRange { start, end };
    /// let map = SourceMap::new(vec![
    ///     Segment::new(range(20, 30), range(0, 5), SegmentKind::Struct, 0, None),
    ///     Segment::new(range(0, 10), range(6, 9), SegmentKind::Enum, 1, None),
    /// ]);
    /// assert_eq!(map.segments()[0].kind, SegmentKind::Enum);
    /// assert!(SourceMap::new(Vec::new()).segments().is_empty());
    /// ```
    pub fn new(mut segments: Vec<Segment>) -> SourceMap {
        segments.sort_by_key(|s| {
            #[cfg(test)]
            count(&BUILD_STEPS);
            (s.generated.start, Reverse(s.generated.end))
        });

        let mut parents = Vec::with_capacity(segments.len());
        let mut open: Vec<usize> = Vec::new();
        for (i, segment) in segments.iter().enumerate() {
            while let Some(&top) = open.last() {
                #[cfg(test)]
                count(&BUILD_STEPS);
                if segments[top].generated.end <= segment.generated.start {
                    open.pop();
                } else {
                    break;
                }
            }
            #[cfg(test)]
            count(&BUILD_STEPS);
            parents.push(open.last().copied());
            open.push(i);
        }

        let mut by_source: Vec<usize> = (0..segments.len()).collect();
        by_source.sort_by_key(|&i| {
            #[cfg(test)]
            count(&BUILD_STEPS);
            segments[i].source.start
        });
        let mut largest_end = 0;
        let max_source_end = by_source
            .iter()
            .map(|&i| {
                largest_end = largest_end.max(segments[i].source.end);
                largest_end
            })
            .collect();

        SourceMap {
            segments,
            parents,
            by_source,
            max_source_end,
        }
    }

    /// All segments in generated order: ascending `generated.start`, and an outer
    /// segment before the segments nested in it.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    ///
    /// let program = marigold_generate("range(0, 1).return\nstruct S { a: u8 }").unwrap();
    /// let starts: Vec<_> = program.map.segments().iter().map(|s| s.generated.start).collect();
    /// assert!(starts.windows(2).all(|w| w[0] < w[1]));
    /// assert_eq!(program.map.segments().len(), 2);
    /// ```
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    /// The segment that produced the byte at `offset` of the generated code, with its
    /// [`source`](Segment::source) range in the Marigold program; the innermost one
    /// if segments are nested. `None` for bytes that belong to no segment (the
    /// preamble, separators, the stream wiring) and for offsets past the end.
    ///
    /// Takes `O(log n)` plus the nesting depth. `offset` may fall inside a multibyte
    /// character of the generated code.
    ///
    /// Ranges are half-open, so the offset one past a segment's last byte is not in it.
    /// An editor cursor sitting right after the last byte of an expression (offset 8 in
    /// `x.return`) therefore finds nothing; also query the offset before the cursor.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    ///
    /// let src = "x = range(0, 5)\nx.return";
    /// let program = marigold_generate(src).unwrap();
    ///
    /// let offset = program.rust.find("MultiConsumerStream").unwrap();
    /// let segment = program.map.source_for_generated(offset).unwrap();
    /// assert_eq!(&src[segment.source.start..segment.source.end], "x = range(0, 5)");
    /// assert_eq!(segment.name.as_deref(), Some("x"));
    ///
    /// assert!(program.map.source_for_generated(0).is_none());
    /// assert!(program.map.source_for_generated(usize::MAX).is_none());
    /// ```
    pub fn source_for_generated(&self, offset: usize) -> Option<&Segment> {
        // The last segment starting at or before `offset` is the innermost candidate:
        // every other segment containing `offset` encloses it.
        let after = self
            .segments
            .partition_point(|s| s.generated.start <= offset);
        let mut candidate = after.checked_sub(1);
        while let Some(i) = candidate {
            let segment = &self.segments[i];
            if offset < segment.generated.end {
                return Some(segment);
            }
            #[cfg(test)]
            count(&WALK_STEPS);
            candidate = self.parents[i];
        }
        None
    }

    /// Every segment whose [`source`](Segment::source) overlaps `range`, in generated
    /// order (not source order).
    ///
    /// Ranges are half-open, so touching ranges do not overlap. A zero-width `range`
    /// is a cursor position: it matches the segments containing that byte (a cursor
    /// right after a segment's last byte, like offset 8 in `x.return`, does not match
    /// it, so editors should also query the position before the cursor). An inverted
    /// range (`start > end`) and a range outside the source match nothing.
    ///
    /// Takes `O(log n + k)` for `k` results when source ranges are disjoint, as they
    /// are in a generated program. For hand-built maps whose source ranges nest in long
    /// chains it degrades to a scan of the segments that start before the query ends.
    /// A stream variable's range finds both its declaration
    /// and its runner, but not the streams that merely use the variable.
    ///
    /// ```
    /// use marigold_grammar::diagnostics::ByteRange;
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let src = "x = range(0, 5)\nx.return";
    /// let program = marigold_generate(src).unwrap();
    ///
    /// let hits = program.map.generated_for_source(ByteRange { start: 0, end: 1 });
    /// let kinds: Vec<_> = hits.iter().map(|s| s.kind).collect();
    /// assert_eq!(kinds, [SegmentKind::StreamVariable, SegmentKind::StreamVariableRunner]);
    ///
    /// // The newline between the two expressions belongs to neither.
    /// assert!(program.map.generated_for_source(ByteRange { start: 15, end: 16 }).is_empty());
    /// // A cursor inside `x.return`.
    /// let cursor = ByteRange { start: 18, end: 18 };
    /// assert_eq!(program.map.generated_for_source(cursor)[0].kind, SegmentKind::ReturningStream);
    /// ```
    pub fn generated_for_source(&self, range: ByteRange) -> Vec<&Segment> {
        // A cursor is the one-byte range starting at it.
        let (start, end) = match range.start.cmp(&range.end) {
            std::cmp::Ordering::Greater => return Vec::new(),
            std::cmp::Ordering::Equal => match range.start.checked_add(1) {
                Some(end) => (range.start, end),
                None => return Vec::new(),
            },
            std::cmp::Ordering::Less => (range.start, range.end),
        };

        // Segments starting before `end` ...
        let upper = self
            .by_source
            .partition_point(|&i| self.segments[i].source.start < end);
        // ... and, since `max_source_end` never decreases, nothing before `lower` ends
        // after `start`.
        let lower = self.max_source_end.partition_point(|&m| m <= start);

        let mut hits: Vec<usize> = self
            .by_source
            .get(lower..upper)
            .unwrap_or_default()
            .iter()
            .copied()
            .filter(|&i| {
                #[cfg(test)]
                count(&SCANNED);
                self.segments[i].source.end > start
            })
            .collect();
        hits.sort_unstable();
        hits.into_iter().map(|i| &self.segments[i]).collect()
    }
}

/// Generated Rust code and the map from it back to the Marigold source.
///
/// Serializes as `{"rust": "...", "map": {"segments": [...]}}`.
///
/// ```
/// use marigold_grammar::marigold_generate;
///
/// let program = marigold_generate("range(0, 3).return").unwrap();
/// assert!(program.rust.starts_with("async {"));
/// let json = serde_json::to_value(&program).unwrap();
/// assert_eq!(json["map"]["segments"][0]["kind"], "returning_stream");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct GeneratedProgram {
    /// The generated Rust code, byte for byte what [`crate::marigold_parse`] returns.
    pub rust: String,
    /// Where each expression went in [`rust`](Self::rust).
    pub map: SourceMap,
}

impl GeneratedProgram {
    /// Pair generated code with a map, for example to hand-build a program in a test.
    /// [`crate::marigold_generate`] is the way to get a real one; nothing checks that
    /// `map` describes `rust`.
    ///
    /// ```
    /// use marigold_grammar::diagnostics::ByteRange;
    /// use marigold_grammar::source_map::{GeneratedProgram, Segment, SegmentKind, SourceMap};
    ///
    /// let segment = Segment::new(
    ///     ByteRange { start: 0, end: 5 },
    ///     ByteRange { start: 0, end: 9 },
    ///     SegmentKind::Struct,
    ///     0,
    ///     Some("S".to_string()),
    /// );
    /// let program = GeneratedProgram::new("struct".to_string(), SourceMap::new(vec![segment]));
    /// assert_eq!(program.map.source_for_generated(2).unwrap().name.as_deref(), Some("S"));
    /// ```
    pub fn new(rust: String, map: SourceMap) -> GeneratedProgram {
        GeneratedProgram { rust, map }
    }
}

/// A piece of generated code, before its source range is known.
///
/// Code generation sees expressions, not source text, so it records the generated
/// range and which expression it was; the parser attaches the source range after.
#[derive(Debug)]
pub(crate) struct GeneratedSegment {
    generated: ByteRange,
    kind: SegmentKind,
    expr_index: usize,
    name: Option<String>,
}

impl GeneratedSegment {
    /// Complete the segment with the range of expression `expr_index` in `ranges`.
    pub(crate) fn attach_source(self, ranges: &[ByteRange]) -> Segment {
        Segment {
            generated: self.generated,
            // `ranges` is parallel to the expressions code generation saw: `build_program`
            // pushes exactly one `TypedExpression` per `Rule::expr`, and `Rule::expr` never
            // nests. A wrong index would be a silently wrong range, so it must stay a panic.
            source: ranges[self.expr_index],
            kind: self.kind,
            expr_index: self.expr_index,
            name: self.name,
        }
    }
}

/// Builds generated code while recording where each expression's code lands.
///
/// Offsets are `String::len()`, so they are byte offsets.
#[derive(Debug)]
pub(crate) struct CodeBuilder {
    out: String,
    segments: Vec<GeneratedSegment>,
}

impl CodeBuilder {
    pub(crate) fn new(prefix: &str) -> Self {
        CodeBuilder {
            out: prefix.to_string(),
            segments: Vec::new(),
        }
    }

    /// Append code that belongs to no expression: separators and wiring.
    pub(crate) fn glue(&mut self, code: &str) {
        self.out.push_str(code);
    }

    /// Append the code generated for expression `expr_index` and record where it went.
    pub(crate) fn segment(
        &mut self,
        kind: SegmentKind,
        expr_index: usize,
        name: Option<String>,
        code: &str,
    ) {
        let start = self.out.len();
        self.out.push_str(code);
        self.segments.push(GeneratedSegment {
            generated: ByteRange {
                start,
                end: self.out.len(),
            },
            kind,
            expr_index,
            name,
        });
    }

    pub(crate) fn finish(self) -> (String, Vec<GeneratedSegment>) {
        (self.out, self.segments)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(start: usize, end: usize) -> ByteRange {
        ByteRange { start, end }
    }

    fn seg(gen: (usize, usize), src: (usize, usize), kind: SegmentKind, expr: usize) -> Segment {
        Segment::new(r(gen.0, gen.1), r(src.0, src.1), kind, expr, None)
    }

    /// Flat map whose generated order differs from source order, with two segments sharing one source range.
    fn flat() -> SourceMap {
        SourceMap::new(vec![
            seg((10, 20), (50, 60), SegmentKind::Function, 2),
            seg((22, 30), (0, 10), SegmentKind::StreamVariable, 0),
            seg((32, 40), (20, 30), SegmentKind::ReturningStream, 1),
            seg((42, 50), (0, 10), SegmentKind::StreamVariableRunner, 0),
        ])
    }

    fn ids(v: Vec<&Segment>) -> Vec<(usize, SegmentKind)> {
        v.into_iter().map(|s| (s.expr_index, s.kind)).collect()
    }

    #[test]
    fn new_sorts_segments_into_generated_order_outer_first() {
        let m = SourceMap::new(vec![
            seg((5, 6), (0, 1), SegmentKind::ReturningStream, 3),
            seg((0, 10), (0, 10), SegmentKind::Function, 2),
            seg((0, 4), (2, 3), SegmentKind::Struct, 1),
        ]);
        let order: Vec<_> = m.segments().iter().map(|s| s.expr_index).collect();
        assert_eq!(order, vec![2, 1, 3]);
    }

    #[test]
    fn new_puts_the_outer_segment_first_even_when_the_inner_one_is_listed_first() {
        // Same start, so only the tie-break on the larger end puts the outer one first;
        // a plain stable sort on `generated.start` would keep the input order.
        let m = SourceMap::new(vec![
            seg((0, 4), (2, 3), SegmentKind::Struct, 1),
            seg((0, 10), (0, 10), SegmentKind::Function, 2),
        ]);
        let order: Vec<_> = m.segments().iter().map(|s| s.expr_index).collect();
        assert_eq!(order, vec![2, 1]);
        assert_eq!(m.source_for_generated(2).map(|s| s.expr_index), Some(1));
        assert_eq!(m.source_for_generated(6).map(|s| s.expr_index), Some(2));
    }

    #[test]
    fn source_for_generated_flat_boundaries() {
        let m = flat();
        let at = |o| m.source_for_generated(o).map(|s| s.expr_index);
        assert_eq!(at(0), None);
        assert_eq!(at(9), None);
        assert_eq!(at(10), Some(2));
        assert_eq!(at(19), Some(2));
        assert_eq!(at(20), None);
        assert_eq!(at(21), None);
        assert_eq!(at(22), Some(0));
        assert_eq!(at(42), Some(0));
        assert_eq!(at(49), Some(0));
        assert_eq!(at(50), None);
        assert_eq!(at(usize::MAX), None);
    }

    #[test]
    fn source_for_generated_returns_the_innermost_of_nested_segments() {
        let m = SourceMap::new(vec![
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((10, 20), (5, 10), SegmentKind::StreamVariable, 0),
            seg((15, 18), (6, 7), SegmentKind::StreamVariableRunner, 0),
            seg((30, 40), (20, 30), SegmentKind::Struct, 0),
        ]);
        let kind = |o| m.source_for_generated(o).map(|s| s.kind);
        assert_eq!(kind(5), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(10), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(14), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(15), Some(SegmentKind::StreamVariableRunner));
        assert_eq!(kind(17), Some(SegmentKind::StreamVariableRunner));
        assert_eq!(kind(18), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(20), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(35), Some(SegmentKind::Struct));
        assert_eq!(kind(99), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(100), None);
    }

    #[test]
    fn generated_for_source_returns_overlaps_in_generated_order() {
        let m = flat();
        let q = |a, b| ids(m.generated_for_source(r(a, b)));
        assert_eq!(
            q(0, 10),
            vec![
                (0, SegmentKind::StreamVariable),
                (0, SegmentKind::StreamVariableRunner)
            ]
        );
        assert_eq!(q(0, 100).len(), 4);
        assert_eq!(
            q(0, 100)[0].0,
            2,
            "generated order puts the fn (source 50..60) first"
        );
        assert_eq!(
            q(9, 21),
            vec![
                (0, SegmentKind::StreamVariable),
                (1, SegmentKind::ReturningStream),
                (0, SegmentKind::StreamVariableRunner),
            ]
        );
        assert!(q(10, 20).is_empty(), "gap between source ranges");
        assert!(q(60, 70).is_empty(), "half-open: 60 is outside 50..60");
        assert_eq!(q(59, 60), vec![(2, SegmentKind::Function)]);
        assert!(q(10, 10).is_empty());
        assert_eq!(q(0, 0).len(), 2);
        assert_eq!(q(9, 9).len(), 2);
        assert!(q(7, 3).is_empty());
        assert!(q(usize::MAX - 1, usize::MAX).is_empty());
    }

    #[test]
    fn generated_for_source_handles_nested_source_ranges() {
        let m = SourceMap::new(vec![
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((10, 20), (5, 10), SegmentKind::StreamVariable, 0),
            seg((30, 40), (40, 45), SegmentKind::Struct, 0),
        ]);
        let q = |a, b| m.generated_for_source(r(a, b)).len();
        assert_eq!(q(7, 8), 2);
        assert_eq!(q(20, 30), 1);
        assert_eq!(q(42, 43), 2);
        assert_eq!(q(50, 60), 0);
    }

    #[test]
    fn touching_source_ranges_do_not_overlap_even_inside_a_longer_one() {
        // `(5, 10)` ends exactly where the query starts; the longer `(0, 50)` before it
        // in source order keeps the prefix maximum above the query start, so only the
        // final overlap check can reject it.
        let m = SourceMap::new(vec![
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((10, 20), (5, 10), SegmentKind::StreamVariable, 1),
        ]);
        let q = |a, b| ids(m.generated_for_source(r(a, b)));
        assert_eq!(q(10, 11), vec![(0, SegmentKind::ReturningStream)]);
        assert_eq!(q(10, 10), vec![(0, SegmentKind::ReturningStream)]);
        assert_eq!(q(4, 5).len(), 1);
        assert_eq!(q(9, 10).len(), 2);
    }

    #[test]
    fn adjacent_generated_ranges_are_siblings_not_a_chain() {
        // Otherwise every lookup past the last segment would walk all of them.
        let m = SourceMap::new(
            (0..1000)
                .map(|i| seg((i * 4, i * 4 + 4), (i, i + 1), SegmentKind::Struct, i))
                .collect(),
        );
        assert!(m.parents.iter().all(|p| p.is_none()));
        assert_eq!(m.source_for_generated(4_000), None);
        assert_eq!(m.source_for_generated(3).map(|s| s.expr_index), Some(0));
        assert_eq!(m.source_for_generated(4).map(|s| s.expr_index), Some(1));
    }

    #[test]
    fn equal_generated_ranges_keep_input_order_and_the_later_one_wins() {
        let m = SourceMap::new(vec![
            seg((10, 20), (0, 1), SegmentKind::Struct, 0),
            seg((10, 20), (2, 3), SegmentKind::Enum, 1),
        ]);
        let order: Vec<_> = m.segments().iter().map(|s| s.expr_index).collect();
        assert_eq!(order, vec![0, 1]);
        assert_eq!(m.source_for_generated(15).map(|s| s.expr_index), Some(1));
    }

    /// `n` disjoint segments in a deterministic scrambled input order, so that neither sort
    /// can take its already-sorted shortcut. Source order is the reverse of generated order.
    fn scrambled(n: usize) -> Vec<Segment> {
        let mut order: Vec<usize> = (0..n).collect();
        let mut state = 0x2545_f491_4f6c_dd1d_u64;
        for i in (1..n).rev() {
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1_442_695_040_888_963_407);
            order.swap(i, (state >> 33) as usize % (i + 1));
        }
        order
            .into_iter()
            .map(|i| {
                let (g, s) = (i * 10, (n - 1 - i) * 10);
                seg((g, g + 8), (s, s + 8), SegmentKind::Struct, i)
            })
            .collect()
    }

    fn reset_counters() {
        BUILD_STEPS.with(|c| c.set(0));
        WALK_STEPS.with(|c| c.set(0));
        SCANNED.with(|c| c.set(0));
    }

    fn counter(c: &'static std::thread::LocalKey<std::cell::Cell<usize>>) -> usize {
        c.with(|c| c.get())
    }

    #[test]
    fn construction_work_is_n_log_n() {
        for n in [4_000_usize, 16_000, 64_000] {
            let segments = scrambled(n);
            reset_counters();
            let map = SourceMap::new(segments);
            let steps = counter(&BUILD_STEPS);
            let log_n = usize::BITS as usize - n.leading_zeros() as usize;
            // Two sorts at about 2 n log n key evaluations each, plus about 2 n for the parent
            // pass. Quadratic work at n = 4_000 is already 16_000_000, 15 times the bound.
            assert!(
                steps <= 5 * n * log_n,
                "n={n}: {steps} steps, bound {}",
                5 * n * log_n
            );
            // Real value is about 2.4 n log2 n. A lower bound too, or removing the counters
            // from the sorts would leave a test that can only pass.
            assert!(
                steps >= n * (log_n - 1),
                "n={n}: only {steps} steps counted"
            );
            assert_eq!(map.segments().len(), n);
        }
    }

    #[test]
    fn lookups_do_constant_work_beyond_the_binary_searches_for_disjoint_segments() {
        let n = 50_000;
        let map = SourceMap::new(scrambled(n));
        reset_counters();
        let mut hits = 0;
        for s in map.segments() {
            // first and last byte of a segment, found without following any parent link
            assert!(map.source_for_generated(s.generated.start).is_some());
            assert!(map.source_for_generated(s.generated.end - 1).is_some());
            hits += map.generated_for_source(s.source).len();
        }
        assert_eq!(counter(&WALK_STEPS), 0);
        assert_eq!(hits, n);
        assert_eq!(
            counter(&SCANNED),
            n,
            "scanned exactly the segments returned"
        );

        // a byte in the gap after a segment follows that segment's (absent) parent once
        reset_counters();
        for s in map.segments().iter().take(1_000) {
            assert!(map.source_for_generated(s.generated.end).is_none());
        }
        assert_eq!(counter(&WALK_STEPS), 1_000);
    }

    #[test]
    fn nested_lookups_follow_at_most_the_nesting_depth() {
        // a chain: segment i contains segment i + 1
        let depth = 200;
        let map = SourceMap::new(
            (0..depth)
                .map(|i| {
                    seg(
                        (i, 2 * depth - i),
                        (i, 2 * depth - i),
                        SegmentKind::Struct,
                        i,
                    )
                })
                .collect(),
        );
        reset_counters();
        // just inside the outermost segment, past the end of every deeper one
        assert_eq!(
            map.source_for_generated(2 * depth - 1)
                .map(|s| s.expr_index),
            Some(0)
        );
        assert!(counter(&WALK_STEPS) <= depth);
    }

    #[test]
    fn empty_map_answers_none_and_empty() {
        let m = SourceMap::new(Vec::new());
        assert!(m.segments().is_empty());
        assert!(m.source_for_generated(0).is_none());
        assert!(m.generated_for_source(r(0, 100)).is_empty());
    }

    #[test]
    fn lookups_scale_to_many_segments() {
        let n = 100_000;
        // Scrambled input order, so an uncounted insertion sort in `new` is quadratic here
        // too; source order is the reverse of generated order.
        let segments = scrambled(n);
        // Timed from before construction: building the indexes must be fast too.
        let t = std::time::Instant::now();
        let m = SourceMap::new(segments);
        let mut hits = 0;
        for i in 0..n {
            hits += m.source_for_generated(i * 10 + 3).is_some() as usize;
            hits += m.source_for_generated(i * 10 + 9).is_some() as usize;
            hits += m.generated_for_source(r(i * 10, i * 10 + 8)).len();
        }
        assert_eq!(hits, n + n);
        // O(n log n): milliseconds. O(n^2) would need ~2*10^10 comparisons.
        assert!(t.elapsed().as_secs() < 5, "{:?}", t.elapsed());
    }
}
