//! Map generated Rust code back to the Marigold source it came from.
//!
//! [`crate::marigold_generate`] returns the same Rust code as [`crate::marigold_parse`]
//! together with a [`SourceMap`]: a forest of [`Segment`]s, each tying one region of the
//! generated code to the part of the Marigold source it was generated from.
//!
//! # Statement-level segments and their parts
//!
//! The roots of the forest are the **statement-level** segments, one per piece of generated
//! code that stands for one top-level Marigold expression: a struct, an enum, a function, a
//! stream variable and its runner, and a stream with its output. They appear in the order the
//! pieces appear in the generated code. That order is not the order of the Marigold source:
//! structs, enums, functions, stream variables, output streams and stream variable runners are
//! emitted in sections.
//!
//! Inside a statement, **nested** segments mark its parts, each with its own source slice:
//!
//! | statement | nested segments |
//! |---|---|
//! | stream, stream variable | the input ([`SegmentKind::InputRange`], [`SegmentKind::InputReadFile`], [`SegmentKind::InputSelectAll`] or [`SegmentKind::InputVariable`]), then one segment per stream function (`map(f)`, `filter(g)`, ...), and for `write_file` two [`SegmentKind::OutputWriteFile`] segments around the chain |
//! | `select_all(a, b)` | one [`SegmentKind::SelectAllBranch`] per argument, holding that branch's input and stream functions |
//! | function | [`SegmentKind::FunctionSignature`] and [`SegmentKind::FunctionBody`] |
//! | struct | one [`SegmentKind::StructField`] per field |
//! | enum | one [`SegmentKind::EnumVariant`] per variant, including the `default` variant |
//!
//! The list returned by [`SourceMap::segments`] is a pre-order traversal of that forest
//! (ascending `generated.start`, an enclosing segment before the segments inside it), so a
//! parent always comes before its children. [`SourceMap::top_level`] yields the
//! statement-level segments, [`SourceMap::parent`] and [`SourceMap::children`] walk the
//! nesting, and [`SourceMap::index_of`] turns a lookup result into an index.
//!
//! Nested segments of one statement never overlap each other partially: any two generated
//! ranges are nested or disjoint, and no two segments have the same generated range. Source
//! ranges of nested segments lie inside the source range of their parent. Constructs with no
//! generated code of their own (`return`, the `.` between chain elements, the declared name of
//! a stream variable) have no segment; positions in them resolve to the enclosing segment.
//!
//! # Identifiers
//!
//! [`Segment::ident`] is the Rust identifier the generated code defines for a statement-level
//! segment (`returning_stream_0`, `non_returning_stream_1`, `stream_variable_runners_0`, or
//! the declared name of a struct, enum, function or stream variable), for telemetry and
//! profiling that names generated items. [`SourceMap::segments_for_ident`] finds the segments
//! that define an identifier.
//!
//! # What the offsets mean
//!
//! All offsets are byte offsets and all ranges are half-open (`start..end`), like the ranges
//! in [`crate::diagnostics`].
//!
//! * [`Segment::generated`] is exactly the bytes the code generator emitted for the segment,
//!   without trimming: a struct starts with a newline and indentation, and `read_file`'s code
//!   starts with a newline. A highlighter should trim the text it shows. The `.` that joins
//!   two chain elements, the separators between statements, and the code that wires the
//!   streams together belong to no nested segment (to the enclosing statement, or to none).
//! * [`Segment::source`] of a statement is the whole expression without trailing whitespace.
//!   For a nested segment it is the pest sub-rule's text without trailing ASCII whitespace
//!   (space, tab, `\r`, `\n`), without the `.` that joins a chain: `map(f)`, not `.map(f)`.
//! * `write_file(..)` wraps the stream in generated code, so it has two segments with the
//!   same source: the code before the stream and the code after it. `.return` generates no
//!   code and has none.
//!
//! Segments of a map built by [`crate::marigold_generate`] are valid only against the exact
//! string passed to it and the [`GeneratedProgram::rust`] it returned:
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
//! // From generated code back to Marigold: the innermost segment that produced a byte.
//! let offset = program.rust.find("iter(0..3)").unwrap();
//! let origin = program.map.source_for_generated(offset).unwrap();
//! assert_eq!(origin.kind, SegmentKind::InputRange);
//! assert_eq!(&src[origin.source.start..origin.source.end], "range(0, 3)");
//! // ... and the statement it belongs to.
//! let index = program.map.index_of(origin).unwrap();
//! let statement = &program.map.segments()[program.map.parent(index).unwrap()];
//! assert_eq!(&src[statement.source.start..statement.source.end], "range(0, 3).map(double).return");
//!
//! // From Marigold to generated code: the most specific segments a source range turned into.
//! let hits = program.map.generated_for_source_innermost(origin.source);
//! assert_eq!(hits, vec![origin]);
//! ```

use crate::diagnostics::ByteRange;
use crate::sub_spans::SubSpan;
use serde::Serialize;
use std::cmp::Reverse;
use std::collections::HashMap;

thread_local! {
    static SKIP_LAYOUT: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// While alive, [`Emit`] records no layout on this thread: the generated text is the same,
/// but no [`Piece`] is built. `marigold_parse` runs under it because it needs only the text.
pub(crate) struct SkipLayout {
    previous: bool,
}

impl SkipLayout {
    pub(crate) fn enter(skip: bool) -> SkipLayout {
        SkipLayout {
            previous: SKIP_LAYOUT.with(|c| c.replace(skip)),
        }
    }
}

impl Drop for SkipLayout {
    fn drop(&mut self) {
        SKIP_LAYOUT.with(|c| c.set(self.previous));
    }
}

fn skipping_layout() -> bool {
    SKIP_LAYOUT.with(|c| c.get())
}

#[cfg(test)]
thread_local! {
    /// Key evaluations in the two sorts plus the steps of the parent-link pass.
    static BUILD_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Parent links followed by `source_for_generated`.
    static WALK_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Segments `generated_for_source` looked at beyond the two binary searches.
    static SCANNED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Source-side parts compared with a generated piece while pairing them.
    static PAIR_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Pieces recorded, source-side parts built and parts paired: the work of sub-span segments.
    static SUBSPAN_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Expressions visited by the passes of code generation plus `CodeBuilder` appends.
    static CODEGEN_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
pub(crate) fn reset_subspan_steps() {
    SUBSPAN_STEPS.with(|c| c.set(0));
}

#[cfg(test)]
pub(crate) fn subspan_steps() -> usize {
    SUBSPAN_STEPS.with(|c| c.get())
}

#[cfg(test)]
pub(crate) fn count_subspan_step() {
    count(&SUBSPAN_STEPS);
}

#[cfg(test)]
pub(crate) fn reset_codegen_steps() {
    CODEGEN_STEPS.with(|c| c.set(0));
}

#[cfg(test)]
pub(crate) fn codegen_steps() -> usize {
    CODEGEN_STEPS.with(|c| c.get())
}

#[cfg(test)]
fn count(counter: &'static std::thread::LocalKey<std::cell::Cell<usize>>) {
    counter.with(|c| c.set(c.get() + 1));
}

/// What a [`Segment`] stands for: a statement-level Marigold expression or one part of it.
///
/// The first seven kinds are statement-level (the roots of the segment forest, see the
/// [module documentation](self)); every other kind is nested inside one of them.
///
/// A stream variable (`x = range(0, 5)`) is written to the generated code twice, so
/// it has two segments: a [`SegmentKind::StreamVariable`] for the declaration and a
/// [`SegmentKind::StreamVariableRunner`] for the task that drives it.
///
/// Serialized as one flat `snake_case` string (`"input_range"`,
/// `"stream_permutations_with_replacement"`, ...).
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::source_map::SegmentKind;
///
/// let program = marigold_generate("x = range(0, 5)\nx.return").unwrap();
/// let kinds: Vec<_> = program.map.top_level().map(|s| s.kind).collect();
/// assert_eq!(
///     kinds,
///     [SegmentKind::StreamVariable, SegmentKind::ReturningStream, SegmentKind::StreamVariableRunner]
/// );
/// assert_eq!(serde_json::to_string(&SegmentKind::StreamVariableRunner).unwrap(), "\"stream_variable_runner\"");
/// assert_eq!(serde_json::to_string(&SegmentKind::StreamFilterMap).unwrap(), "\"stream_filter_map\"");
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
    /// `range(0, 5)`, `range(0, =5)` or `range(MyEnum)` as the input of a stream.
    InputRange,
    /// `read_file("in.csv", csv, struct=Row)` as the input of a stream.
    InputReadFile,
    /// `select_all(a, b)` as the input of a stream. Its children are the
    /// [`SegmentKind::SelectAllBranch`]es.
    InputSelectAll,
    /// The stream variable a stream reads from: the `x` of `x.map(f).return` and of
    /// `y = x.map(f)`. Not the declared name of `x = ...`.
    InputVariable,
    /// One argument of `select_all`: an input and its stream functions, like
    /// `range(5, 8).map(f)`. For a bare input (`select_all(range(0, 3))`) it has the same
    /// source as the input inside it.
    SelectAllBranch,
    /// `map(f)`
    StreamMap,
    /// `filter(f)`
    StreamFilter,
    /// `filter_map(f)`
    StreamFilterMap,
    /// `permutations(n)`
    StreamPermutations,
    /// `permutations_with_replacement(n)`
    StreamPermutationsWithReplacement,
    /// `combinations(n)`
    StreamCombinations,
    /// `keep_first_n(n, f)`
    StreamKeepFirstN,
    /// `fold(init, f)`
    StreamFold,
    /// `ok()`. One segment, although the generated code makes two calls.
    StreamOk,
    /// `ok_or_panic()`
    StreamOkOrPanic,
    /// `write_file("out.csv", csv)`. The generated code wraps the whole stream, so the call
    /// has two segments with the same source: the code before the stream and the code after it.
    OutputWriteFile,
    /// The name, parameters and return type of a function: `double(x: i32) -> i32`.
    FunctionSignature,
    /// The braces and body of a function. Its generated code is byte for byte its source.
    FunctionBody,
    /// One field of a struct, like `b : string_10`, without the separating comma.
    StructField,
    /// One variant of an enum, like `A`, `B = "b"` or `default Z`, without the separating
    /// comma. The generated code is just the variant (no serde attributes, no value).
    EnumVariant,
}

/// One piece of generated code and the Marigold source it came from.
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
/// assert_eq!(segment.ident, None);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Segment {
    /// Where the code was written in the generated Rust. Exactly the bytes emitted for
    /// this segment: the separator that follows it is not included, but nothing is
    /// trimmed either. A struct's code starts with a newline and indentation, so a
    /// highlighter should trim the text it shows.
    pub generated: ByteRange,
    /// The Marigold source the code came from. For a statement-level segment, the whole
    /// expression without trailing whitespace. For a nested segment, the part of the
    /// expression (`map(f)`, `range(0, 5)`, `b : string_10`), without trailing ASCII
    /// whitespace and without the `.` that joins stream functions.
    pub source: ByteRange,
    /// What the segment is.
    pub kind: SegmentKind,
    /// Position of the top-level expression the segment belongs to among all top-level
    /// expressions of the program, in source order. This is the `expr_index` of
    /// [`crate::complexity::NodeComplexity`] and [`crate::symbols::SymbolDeclaration`].
    /// A nested segment has the `expr_index` of its statement.
    pub expr_index: usize,
    /// The name the segment declares: the struct, enum, function or stream variable, the
    /// name of a [`SegmentKind::StructField`] or [`SegmentKind::EnumVariant`]. `None` for
    /// everything else, including output streams (even a named one like `x.return`).
    pub name: Option<String>,
    /// The Rust identifier the generated code defines for this statement: the
    /// `returning_stream_{k}`, `non_returning_stream_{k}` or `stream_variable_runners_{k}`
    /// local (`k` counts within that kind, in the order the code is generated), or the
    /// declared name of a struct, enum, function or stream variable. `None` for nested
    /// segments. See [`SourceMap::segments_for_ident`].
    pub ident: Option<String>,
}

impl Segment {
    /// Build a segment, for example to hand-build a [`SourceMap`] in a test. Its
    /// [`ident`](Segment::ident) is `None`; set one with [`Segment::with_ident`].
    ///
    /// Outside tests, prefer the segments [`crate::marigold_generate`] returns over
    /// building your own.
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
            ident: None,
        }
    }

    /// The same segment with [`ident`](Segment::ident) set.
    ///
    /// ```
    /// use marigold_grammar::diagnostics::ByteRange;
    /// use marigold_grammar::source_map::{Segment, SegmentKind};
    ///
    /// let range = |start, end| ByteRange { start, end };
    /// let segment = Segment::new(range(0, 9), range(0, 8), SegmentKind::ReturningStream, 0, None)
    ///     .with_ident(Some("returning_stream_0".to_string()));
    /// assert_eq!(segment.ident.as_deref(), Some("returning_stream_0"));
    /// ```
    pub fn with_ident(mut self, ident: Option<String>) -> Segment {
        self.ident = ident;
        self
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
    /// For each segment, the index one past its last descendant: the segments
    /// `i + 1..subtree_end[i]` are exactly the descendants of segment `i`.
    #[serde(skip)]
    subtree_end: Vec<usize>,
    /// Indexes into `segments` by `ident`, each list ascending.
    #[serde(skip)]
    by_ident: HashMap<String, Vec<usize>>,
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
    /// Maps built by [`crate::marigold_generate`] nest their segments properly: any two
    /// generated ranges are nested or disjoint. Hand-built maps may nest segments too (the
    /// innermost one wins in [`source_for_generated`](Self::source_for_generated)). The
    /// [`parent`](Self::parent) of a segment is the closest earlier segment whose generated
    /// range contains its own (equal ranges nest in input order); a segment that only partly
    /// overlaps an earlier one is not nested in it. Any input is accepted and nothing
    /// panics, but "innermost" is only well defined when the generated ranges nest or are
    /// disjoint: lookups on partially overlapping ranges are best effort. The lookups are fast as documented only for
    /// such maps; see [`generated_for_source`](Self::generated_for_source) for the source
    /// side.
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
        let mut subtree_end = vec![segments.len(); segments.len()];
        let mut open: Vec<usize> = Vec::new();
        for (i, segment) in segments.iter().enumerate() {
            while let Some(&top) = open.last() {
                #[cfg(test)]
                count(&BUILD_STEPS);
                // `top` does not enclose this segment: it ends before it starts, or it
                // ends inside it (a partial overlap is no nesting).
                if segments[top].generated.end <= segment.generated.start
                    || segment.generated.end > segments[top].generated.end
                {
                    subtree_end[top] = i;
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

        let mut by_ident: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, segment) in segments.iter().enumerate() {
            if let Some(ident) = &segment.ident {
                by_ident.entry(ident.clone()).or_default().push(i);
            }
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
            subtree_end,
            by_ident,
            by_source,
            max_source_end,
        }
    }

    /// All segments in generated order, a pre-order traversal of the segment forest:
    /// ascending `generated.start`, and an enclosing segment before the segments nested
    /// in it. Use [`top_level`](Self::top_level) for the statement-level segments only.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    ///
    /// let program = marigold_generate("range(0, 1).return\nstruct S { a: u8 }").unwrap();
    /// let starts: Vec<_> = program.map.segments().iter().map(|s| s.generated.start).collect();
    /// assert!(starts.windows(2).all(|w| w[0] < w[1]));
    /// // the stream and its input, the struct and its field
    /// assert_eq!(program.map.segments().len(), 4);
    /// assert_eq!(program.map.top_level().count(), 2);
    /// ```
    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    /// The statement-level segments (the roots of the forest: those without a parent), in
    /// generated order.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let program = marigold_generate("struct S { a: u8, b: u8 }\nrange(0, 3).map(f).return").unwrap();
    /// let kinds: Vec<_> = program.map.top_level().map(|s| s.kind).collect();
    /// assert_eq!(kinds, [SegmentKind::Struct, SegmentKind::ReturningStream]);
    /// assert_eq!(program.map.segments().len(), 6);
    /// ```
    pub fn top_level(&self) -> impl Iterator<Item = &Segment> + '_ {
        self.segments
            .iter()
            .zip(&self.parents)
            .filter(|(_, parent)| parent.is_none())
            .map(|(segment, _)| segment)
    }

    /// Index (into [`segments`](Self::segments)) of the closest segment whose generated
    /// range contains the generated range of segment `index`. `None` for a statement-level
    /// segment and for an `index` out of range.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let program = marigold_generate("range(0, 3).return").unwrap();
    /// let segments = program.map.segments();
    /// assert_eq!(segments[0].kind, SegmentKind::ReturningStream);
    /// assert_eq!(program.map.parent(0), None);
    /// assert_eq!(segments[1].kind, SegmentKind::InputRange);
    /// assert_eq!(program.map.parent(1), Some(0));
    /// assert_eq!(program.map.parent(2), None); // out of range
    /// ```
    pub fn parent(&self, index: usize) -> Option<usize> {
        self.parents.get(index).copied().flatten()
    }

    /// Indexes of the direct children of segment `index`, in generated order (which is
    /// ascending index order). Empty for a leaf and for an `index` out of range.
    ///
    /// Takes `O(k)` for `k` children.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let program = marigold_generate("range(0, 3).map(f).filter(g).return").unwrap();
    /// let kinds: Vec<_> = program
    ///     .map
    ///     .children(0)
    ///     .into_iter()
    ///     .map(|i| program.map.segments()[i].kind)
    ///     .collect();
    /// assert_eq!(kinds, [SegmentKind::InputRange, SegmentKind::StreamMap, SegmentKind::StreamFilter]);
    /// assert!(program.map.children(1).is_empty());
    /// assert!(program.map.children(99).is_empty());
    /// ```
    pub fn children(&self, index: usize) -> Vec<usize> {
        let Some(&end) = self.subtree_end.get(index) else {
            return Vec::new();
        };
        let mut children = Vec::new();
        let mut next = index + 1;
        while next < end {
            children.push(next);
            next = self.subtree_end[next];
        }
        children
    }

    /// The index of `segment` in [`segments`](Self::segments), found in `O(1)`, or `None`
    /// if the reference does not point into this map's segments. Useful to get the
    /// [`parent`](Self::parent) or [`children`](Self::children) of a lookup result.
    ///
    /// The segment is identified by its address, not by its value: a `clone()` of a
    /// stored segment, or a segment of another map, is not found.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    ///
    /// let program = marigold_generate("range(0, 3).return").unwrap();
    /// let input = &program.map.segments()[1];
    /// assert_eq!(program.map.index_of(input), Some(1));
    /// assert_eq!(program.map.index_of(&input.clone()), None);
    /// ```
    pub fn index_of(&self, segment: &Segment) -> Option<usize> {
        // A `&Segment` points at a whole segment, so if it points into `self.segments`
        // the offset is a multiple of the segment size.
        let offset =
            (segment as *const Segment as usize).checked_sub(self.segments.as_ptr() as usize)?;
        let index = offset / std::mem::size_of::<Segment>();
        (index < self.segments.len()).then_some(index)
    }

    /// The segments that define the Rust identifier `ident` (see [`Segment::ident`]), in
    /// generated order. Several can: a user's stream variable may be called
    /// `returning_stream_0`, and a struct and a stream variable may share a name. Only
    /// definitions are found, not the places that use the identifier. An unknown
    /// identifier, a prefix of one and the empty string find nothing.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let src = "range(0, 1).return\nx = range(0, 2)\nx.return";
    /// let program = marigold_generate(src).unwrap();
    /// let found = program.map.segments_for_ident("returning_stream_1");
    /// assert_eq!(found.len(), 1);
    /// assert_eq!(found[0].kind, SegmentKind::ReturningStream);
    /// assert_eq!(&src[found[0].source.start..found[0].source.end], "x.return");
    /// assert!(program.map.segments_for_ident("returning_stream_").is_empty());
    /// ```
    pub fn segments_for_ident(&self, ident: &str) -> Vec<&Segment> {
        self.by_ident
            .get(ident)
            .map(|indexes| indexes.iter().map(|&i| &self.segments[i]).collect())
            .unwrap_or_default()
    }

    /// The segment that produced the byte at `offset` of the generated code, with its
    /// [`source`](Segment::source) range in the Marigold program; the innermost one
    /// if segments are nested. `None` for bytes that belong to no segment (the
    /// preamble, separators, the stream wiring) and for offsets past the end.
    ///
    /// A byte of generated code that is inside a statement but in no part of it (the
    /// `let ... = Box::pin(` head, the `.` between stream functions) belongs to the
    /// statement.
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
    /// order (not source order). Enclosing segments are included: the statement comes
    /// before the parts of it that overlap `range`. Use
    /// [`generated_for_source_innermost`](Self::generated_for_source_innermost) for the
    /// most specific segments only.
    ///
    /// Ranges are half-open, so touching ranges do not overlap. A zero-width `range`
    /// is a cursor position: it matches the segments containing that byte (a cursor
    /// right after a segment's last byte, like offset 8 in `x.return`, does not match
    /// it, so editors should also query the position before the cursor). An inverted
    /// range (`start > end`) and a range outside the source match nothing.
    ///
    /// Takes `O(log n + k + s)` for `k` results, where `s` counts the segments that start
    /// before the query ends and lie, with it, inside the same enclosing statement but end
    /// before the query starts: a query late in a statement with a very long chain scans
    /// the earlier parts of the chain. For hand-built maps whose source ranges nest in
    /// long chains it degrades to a scan of the segments that start before the query ends.
    /// A stream variable's range finds both its declaration and its runner, but not the
    /// streams that merely use the variable.
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
    /// // A cursor inside `x.return`: the statement, then the variable reference.
    /// let cursor = ByteRange { start: 16, end: 16 };
    /// let kinds: Vec<_> = program.map.generated_for_source(cursor).iter().map(|s| s.kind).collect();
    /// assert_eq!(kinds, [SegmentKind::ReturningStream, SegmentKind::InputVariable]);
    /// ```
    pub fn generated_for_source(&self, range: ByteRange) -> Vec<&Segment> {
        self.overlapping(range)
            .into_iter()
            .map(|i| &self.segments[i])
            .collect()
    }

    /// The most specific segments whose [`source`](Segment::source) overlaps `range`: the
    /// result of [`generated_for_source`](Self::generated_for_source) without every segment
    /// that has another hit nested inside it, in generated order. Pointing at `map(f)` in
    /// `x.map(f).return` gives the `map` segment, not the whole stream as well; pointing at
    /// `return` or at a `.` gives the statement.
    ///
    /// Constructs that generate more than one piece of code give more than one hit: both
    /// halves of a `write_file(..)`, and a stream variable's declaration and runner (the
    /// runner has no parts and shares the declaration's source; filter by
    /// [`kind`](Segment::kind) if you only want one).
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::source_map::SegmentKind;
    ///
    /// let src = "range(0, 5).map(f).return";
    /// let program = marigold_generate(src).unwrap();
    /// let map_fn = src.find("map(f)").unwrap();
    /// let cursor = marigold_grammar::diagnostics::ByteRange { start: map_fn + 1, end: map_fn + 1 };
    ///
    /// let all: Vec<_> = program.map.generated_for_source(cursor).iter().map(|s| s.kind).collect();
    /// assert_eq!(all, [SegmentKind::ReturningStream, SegmentKind::StreamMap]);
    /// let innermost: Vec<_> = program.map.generated_for_source_innermost(cursor).iter().map(|s| s.kind).collect();
    /// assert_eq!(innermost, [SegmentKind::StreamMap]);
    /// ```
    pub fn generated_for_source_innermost(&self, range: ByteRange) -> Vec<&Segment> {
        let hits = self.overlapping(range);
        let mut innermost = Vec::with_capacity(hits.len());
        for (position, &index) in hits.iter().enumerate() {
            let has_hit_below = hits
                .get(position + 1)
                .is_some_and(|&next| next < self.subtree_end[index]);
            if !has_hit_below {
                innermost.push(&self.segments[index]);
            }
        }
        innermost
    }

    /// Indexes of the segments whose source overlaps `range`, ascending.
    fn overlapping(&self, range: ByteRange) -> Vec<usize> {
        let (start, end) = match range.start.cmp(&range.end) {
            std::cmp::Ordering::Greater => return Vec::new(),
            std::cmp::Ordering::Equal => match range.start.checked_add(1) {
                Some(end) => (range.start, end),
                None => return Vec::new(),
            },
            std::cmp::Ordering::Less => (range.start, range.end),
        };

        let upper = self
            .by_source
            .partition_point(|&i| self.segments[i].source.start < end);
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
        hits
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
    /// Where each expression, and each part of it, went in [`rust`](Self::rust).
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

/// The part of a statement a [`Piece`] stands for. Together with the nesting it identifies
/// the partner of a piece in the source-side skeleton (`sub_spans`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Role {
    /// The input of a stream (`range(..)`, `read_file(..)`, `select_all(..)`, `x.get()`).
    Input,
    /// The `j`-th stream function of a chain.
    Fn(usize),
    /// The generated code of `write_file` before the stream.
    OutPrefix,
    /// The generated code of `write_file` after the stream.
    OutPostfix,
    /// The `i`-th argument of `select_all`.
    Branch(usize),
    /// The `i`-th field of a struct.
    Field(usize),
    /// The `i`-th variant of an enum; the `default` variant comes after the plain ones.
    Variant(usize),
    /// A function's name, parameters and return type.
    Signature,
    /// A function's braces and body.
    Body,
}

/// A region of the text of an [`Emit`], relative to the start of the enclosing piece (or of
/// the emit, for a top-level piece).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Piece {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) role: Role,
    pub(crate) children: Vec<Piece>,
}

/// Generated code together with the regions of it that stand for parts of the Marigold
/// source. The nodes build their code with this, and `code()` is `emit().s`, so the
/// recorded layout cannot drift from the emitted text.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Emit {
    pub(crate) s: String,
    pub(crate) pieces: Vec<Piece>,
}

impl Emit {
    /// Code with no recorded parts, for code that was built as a plain string (the
    /// instrumented code of the `otel` feature).
    pub(crate) fn text(s: String) -> Emit {
        Emit {
            s,
            pieces: Vec::new(),
        }
    }

    /// Append text that stands for no part.
    pub(crate) fn push(&mut self, text: &str) {
        self.s.push_str(text);
    }

    /// Run `f`, which appends text to this emit, and record the text it appended as one
    /// piece with the pieces it recorded as children. Text that is empty records nothing.
    pub(crate) fn piece(&mut self, role: Role, f: impl FnOnce(&mut Emit)) {
        if skipping_layout() {
            f(self);
            return;
        }
        #[cfg(test)]
        count(&SUBSPAN_STEPS);
        let start = self.s.len();
        let outer = std::mem::take(&mut self.pieces);
        f(self);
        let end = self.s.len();
        let mut children = std::mem::replace(&mut self.pieces, outer);
        if start == end {
            return;
        }
        for child in &mut children {
            child.start -= start;
            child.end -= start;
        }
        self.pieces.push(Piece {
            start,
            end,
            role,
            children,
        });
    }

    /// Append `text`, which has the (relative) `pieces` recorded for it.
    pub(crate) fn push_with(&mut self, text: &str, pieces: &[Piece]) {
        let base = self.s.len();
        self.s.push_str(text);
        if skipping_layout() {
            return;
        }
        #[cfg(test)]
        count(&SUBSPAN_STEPS);
        self.pieces
            .extend(pieces.iter().cloned().map(|piece| piece.shifted(base)));
    }

    /// Append another emit with its pieces.
    pub(crate) fn splice(&mut self, other: Emit) {
        let base = self.s.len();
        self.s.push_str(&other.s);
        if skipping_layout() {
            return;
        }
        #[cfg(test)]
        count(&SUBSPAN_STEPS);
        self.pieces
            .extend(other.pieces.into_iter().map(|piece| piece.shifted(base)));
    }
}

impl Piece {
    /// The piece moved `by` bytes to the right; children are relative, so they stay.
    fn shifted(mut self, by: usize) -> Piece {
        self.start += by;
        self.end += by;
        self
    }
}

/// A piece of generated code, before its source range is known.
///
/// Code generation sees expressions, not source text, so it records the generated
/// range, which expression it was and the layout of its parts; the parser attaches the
/// source ranges after.
#[derive(Debug)]
pub(crate) struct GeneratedSegment {
    generated: ByteRange,
    kind: SegmentKind,
    expr_index: usize,
    name: Option<String>,
    ident: Option<String>,
    pieces: Vec<Piece>,
}

impl GeneratedSegment {
    /// Complete the segment with the range of expression `expr_index` in `ranges` and append
    /// it to `out`, followed by the segments of its parts.
    ///
    /// A part is paired with the source-side part of the same role in `parts[expr_index]`.
    /// A part with no partner is dropped rather than reported: `marigold_parse`, which
    /// the `m!` macro uses, goes through here, and a mapping bug must never become a compile
    /// error for the user. The tests compare the result with an independent oracle.
    pub(crate) fn attach_source(
        self,
        ranges: &[ByteRange],
        parts: &[Vec<SubSpan>],
        out: &mut Vec<Segment>,
    ) {
        out.push(Segment {
            generated: self.generated,
            source: ranges[self.expr_index],
            kind: self.kind,
            expr_index: self.expr_index,
            name: self.name,
            ident: self.ident,
        });
        if let Some(parts) = parts.get(self.expr_index) {
            attach_parts(
                &self.pieces,
                parts,
                self.generated.start,
                self.expr_index,
                out,
            );
        }
    }
}

/// Pair `pieces` (relative to `base`) with the `parts` of the same role and append a segment
/// for each pair, followed by the pairs of its own parts.
fn attach_parts(
    pieces: &[Piece],
    parts: &[SubSpan],
    base: usize,
    expr_index: usize,
    out: &mut Vec<Segment>,
) {
    // Pieces and parts are nearly always in the same order, so look for the partner just
    // after the previous one first: linear overall, not quadratic in the chain length.
    let mut cursor = 0;
    for piece in pieces {
        let found = (cursor..parts.len()).chain(0..cursor).find(|&k| {
            #[cfg(test)]
            {
                count(&PAIR_STEPS);
                count(&SUBSPAN_STEPS);
            }
            parts[k].role == piece.role
        });
        let Some(k) = found else { continue };
        cursor = k + 1;
        let part = &parts[k];
        let start = base + piece.start;
        out.push(Segment {
            generated: ByteRange {
                start,
                end: base + piece.end,
            },
            source: part.source,
            kind: part.kind,
            expr_index,
            name: part.name.clone(),
            ident: None,
        });
        attach_parts(&piece.children, &part.children, start, expr_index, out);
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
        #[cfg(test)]
        count(&CODEGEN_STEPS);
        self.out.push_str(code);
    }

    /// Append the code generated for expression `expr_index` and record where it went,
    /// together with the layout of its parts.
    pub(crate) fn segment(
        &mut self,
        kind: SegmentKind,
        expr_index: usize,
        name: Option<String>,
        ident: Option<String>,
        code: Emit,
    ) {
        #[cfg(test)]
        count(&CODEGEN_STEPS);
        let start = self.out.len();
        self.out.push_str(&code.s);
        self.segments.push(GeneratedSegment {
            generated: ByteRange {
                start,
                end: self.out.len(),
            },
            kind,
            expr_index,
            name,
            ident,
            pieces: code.pieces,
        });
    }

    pub(crate) fn pass<'a, T>(&self, items: &'a [T]) -> impl Iterator<Item = (usize, &'a T)> {
        items.iter().enumerate().inspect(|_| {
            #[cfg(test)]
            count(&CODEGEN_STEPS);
        })
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
            assert!(
                steps <= 5 * n * log_n,
                "n={n}: {steps} steps, bound {}",
                5 * n * log_n
            );
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

        reset_counters();
        for s in map.segments().iter().take(1_000) {
            assert!(map.source_for_generated(s.generated.end).is_none());
        }
        assert_eq!(counter(&WALK_STEPS), 1_000);
    }

    #[test]
    fn nested_lookups_follow_at_most_the_nesting_depth() {
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
        let segments = scrambled(n);
        reset_counters();
        let m = SourceMap::new(segments);
        let mut hits = 0;
        for i in 0..n {
            hits += m.source_for_generated(i * 10 + 3).is_some() as usize;
            hits += m.source_for_generated(i * 10 + 9).is_some() as usize;
            hits += m.generated_for_source(r(i * 10, i * 10 + 8)).len();
        }
        assert_eq!(hits, n + n);
        let log_n = usize::BITS as usize - n.leading_zeros() as usize;
        let (build, walk, scanned) = (
            counter(&BUILD_STEPS),
            counter(&WALK_STEPS),
            counter(&SCANNED),
        );
        assert!(build <= 5 * n * log_n, "{build} build steps");
        assert!(walk <= 4 * n, "{walk} parent links followed");
        assert!(scanned <= 4 * n, "{scanned} segments scanned");
    }

    fn named(gen: (usize, usize), src: (usize, usize), kind: SegmentKind, ident: &str) -> Segment {
        seg(gen, src, kind, 0).with_ident(Some(ident.to_string()))
    }

    /// stmt(0..100) > [input(10..20), fn(20..30) > [x(22..25)], fn(30..40)], other stmt(110..120)
    fn tree() -> SourceMap {
        SourceMap::new(vec![
            seg((110, 120), (60, 70), SegmentKind::NonReturningStream, 1),
            seg((10, 20), (5, 10), SegmentKind::InputRange, 0),
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((30, 40), (20, 30), SegmentKind::StreamFilter, 0),
            seg((22, 25), (16, 17), SegmentKind::StreamOk, 0),
            seg((20, 30), (10, 20), SegmentKind::StreamMap, 0),
        ])
    }

    #[test]
    fn parent_links_cover_nesting_touching_siblings_and_roots() {
        let m = tree();
        let order: Vec<(usize, usize)> = m
            .segments()
            .iter()
            .map(|s| (s.generated.start, s.generated.end))
            .collect();
        assert_eq!(
            order,
            vec![(0, 100), (10, 20), (20, 30), (22, 25), (30, 40), (110, 120)]
        );
        let parents: Vec<Option<usize>> = (0..6).map(|i| m.parent(i)).collect();
        assert_eq!(
            parents,
            vec![None, Some(0), Some(0), Some(2), Some(0), None]
        );
        assert_eq!(m.children(0), vec![1, 2, 4]);
        assert_eq!(m.children(2), vec![3]);
        assert!(m.children(3).is_empty() && m.children(5).is_empty());
        assert_eq!(m.top_level().count(), 2);
        assert_eq!(m.parent(6), None);
        assert_eq!(m.parent(usize::MAX), None);
        assert!(m.children(99).is_empty());
    }

    #[test]
    fn source_for_generated_walks_out_of_touching_siblings_and_nested_children() {
        let m = tree();
        let at = |o| {
            m.source_for_generated(o)
                .map(|s| (s.generated.start, s.generated.end))
        };
        assert_eq!(at(19), Some((10, 20)));
        assert_eq!(
            at(20),
            Some((20, 30)),
            "touching sibling starts exactly where the previous ends"
        );
        assert_eq!(at(21), Some((20, 30)));
        assert_eq!(at(22), Some((22, 25)));
        assert_eq!(at(24), Some((22, 25)));
        assert_eq!(
            at(25),
            Some((20, 30)),
            "back in the parent after the grandchild"
        );
        assert_eq!(at(30), Some((30, 40)));
        assert_eq!(at(40), Some((0, 100)), "glue inside the root");
        assert_eq!(at(99), Some((0, 100)));
        assert_eq!(at(100), None);
        assert_eq!(at(115), Some((110, 120)));
    }

    #[test]
    fn innermost_drops_every_ancestor_that_has_a_hit_below_it() {
        let m = tree();
        let q = |a, b| -> Vec<(usize, usize)> {
            m.generated_for_source_innermost(r(a, b))
                .iter()
                .map(|s| (s.generated.start, s.generated.end))
                .collect()
        };
        assert_eq!(q(16, 17), vec![(22, 25)]);
        assert_eq!(q(10, 12), vec![(20, 30)]);
        assert_eq!(q(5, 25), vec![(10, 20), (22, 25), (30, 40)]);
        assert_eq!(q(45, 49), vec![(0, 100)]);
        assert!(q(80, 90).is_empty());
        assert!(q(7, 3).is_empty());
        assert!(q(usize::MAX - 1, usize::MAX).is_empty());
        assert_eq!(q(6, 6), vec![(10, 20)]);
        assert_eq!(q(65, 65), vec![(110, 120)]);
        let all: Vec<_> = m
            .generated_for_source(r(16, 17))
            .iter()
            .map(|s| s.generated.start)
            .collect();
        assert_eq!(all, vec![0, 20, 22]);
    }

    #[test]
    fn a_child_with_the_same_source_as_its_parent_is_the_leaf() {
        let m = SourceMap::new(vec![
            seg((0, 50), (0, 30), SegmentKind::ReturningStream, 0),
            seg((10, 40), (11, 22), SegmentKind::SelectAllBranch, 0),
            seg((15, 35), (11, 22), SegmentKind::InputRange, 0),
        ]);
        let kinds = |v: Vec<&Segment>| v.into_iter().map(|s| s.kind).collect::<Vec<_>>();
        assert_eq!(
            kinds(m.generated_for_source(r(12, 13))),
            vec![
                SegmentKind::ReturningStream,
                SegmentKind::SelectAllBranch,
                SegmentKind::InputRange
            ]
        );
        assert_eq!(
            kinds(m.generated_for_source_innermost(r(12, 13))),
            vec![SegmentKind::InputRange]
        );
    }

    #[test]
    fn segments_for_ident_returns_definitions_in_generated_order() {
        let m = SourceMap::new(vec![
            named(
                (50, 60),
                (0, 5),
                SegmentKind::ReturningStream,
                "returning_stream_0",
            ),
            named(
                (0, 10),
                (10, 15),
                SegmentKind::StreamVariable,
                "returning_stream_0",
            ),
            seg((12, 14), (11, 12), SegmentKind::InputRange, 0),
            named((70, 80), (20, 25), SegmentKind::Struct, "S"),
        ]);
        let hits = m.segments_for_ident("returning_stream_0");
        assert_eq!(
            hits.iter().map(|s| s.kind).collect::<Vec<_>>(),
            vec![SegmentKind::StreamVariable, SegmentKind::ReturningStream]
        );
        assert_eq!(m.segments_for_ident("S").len(), 1);
        assert!(m.segments_for_ident("s").is_empty(), "case sensitive");
        assert!(m.segments_for_ident("").is_empty());
        assert!(
            m.segments_for_ident("returning_stream_00").is_empty(),
            "no extension matching"
        );
        assert!(
            m.segments_for_ident("returning_stream_").is_empty(),
            "no prefix matching"
        );
        assert!(m.segments_for_ident("S_").is_empty() && m.segments_for_ident("Struct").is_empty());
    }

    #[test]
    fn a_partial_overlap_is_not_a_nesting() {
        let m = SourceMap::new(vec![
            seg((0, 10), (0, 3), SegmentKind::Struct, 0),
            seg((5, 15), (4, 8), SegmentKind::Enum, 1),
            seg((6, 9), (5, 6), SegmentKind::Function, 2),
            seg((20, 30), (9, 10), SegmentKind::Struct, 3),
        ]);
        let parents: Vec<Option<usize>> = (0..4).map(|i| m.parent(i)).collect();
        assert_eq!(parents, vec![None, None, Some(1), None]);
        assert_eq!(m.children(1), vec![2]);
        assert!(m.children(0).is_empty());
        assert_eq!(m.top_level().count(), 3);
    }

    #[test]
    fn construction_never_panics_on_overlapping_duplicate_or_degenerate_segments() {
        let m = SourceMap::new(vec![
            seg((0, 10), (0, 10), SegmentKind::Struct, 0),
            seg((5, 15), (0, 10), SegmentKind::Enum, 0), // partial overlap: unspecified nesting, no panic
            seg((5, 15), (0, 10), SegmentKind::Function, 0), // duplicate range
            seg((7, 7), (3, 3), SegmentKind::StreamMap, 0), // zero width
            seg((9, 2), (9, 2), SegmentKind::StreamMap, 0), // inverted
            seg(
                (usize::MAX - 1, usize::MAX),
                (usize::MAX - 1, usize::MAX),
                SegmentKind::Struct,
                0,
            ),
        ]);
        for o in [0, 5, 7, 9, 14, 15, usize::MAX - 1, usize::MAX] {
            let _ = m.source_for_generated(o);
        }
        for q in [r(0, 100), r(3, 3), r(9, 2), r(usize::MAX - 1, usize::MAX)] {
            let _ = m.generated_for_source(q);
            let _ = m.generated_for_source_innermost(q);
        }
        for i in 0..8 {
            let _ = (m.parent(i), m.children(i));
        }
    }

    #[test]
    fn very_deep_nesting_is_handled_without_recursion() {
        let depth = 100_000;
        let segs: Vec<Segment> = (0..depth)
            .map(|i| {
                seg(
                    (i, 2 * depth - i),
                    (i, 2 * depth - i),
                    SegmentKind::SelectAllBranch,
                    0,
                )
            })
            .collect();
        let m = SourceMap::new(segs);
        assert_eq!(m.parent(0), None);
        assert_eq!(m.parent(depth - 1), Some(depth - 2));
        assert_eq!(
            m.source_for_generated(depth).unwrap().generated.start,
            depth - 1
        );
        assert_eq!(
            m.generated_for_source_innermost(r(depth - 1, depth)).len(),
            1
        );
        assert_eq!(m.children(depth - 2), vec![depth - 1]);
    }

    #[test]
    fn nested_lookups_scale_to_many_segments() {
        let n = 30_000usize;
        let mut segs = Vec::with_capacity(4 * n);
        for i in 0..n {
            let g = i * 100;
            let s = (n - 1 - i) * 100;
            segs.push(seg(
                (g, g + 90),
                (s, s + 90),
                SegmentKind::ReturningStream,
                i,
            ));
            for c in 0..3 {
                segs.push(seg(
                    (g + 10 + c * 20, g + 25 + c * 20),
                    (s + 10 + c * 20, s + 25 + c * 20),
                    SegmentKind::StreamMap,
                    i,
                ));
            }
        }
        let m = SourceMap::new(segs);
        reset_counters();
        let t = std::time::Instant::now();
        let mut hits = 0usize;
        for i in 0..n {
            hits += m
                .source_for_generated(i * 100 + 12)
                .map(|s| s.generated.end - s.generated.start)
                .unwrap_or(0);
            hits += m
                .generated_for_source_innermost(r((n - 1 - i) * 100 + 12, (n - 1 - i) * 100 + 13))
                .len();
            hits += m
                .generated_for_source(r((n - 1 - i) * 100 + 12, (n - 1 - i) * 100 + 13))
                .len();
        }
        assert_eq!(hits, n * (15 + 1 + 2));
        assert!(counter(&WALK_STEPS) <= n, "{} walks", counter(&WALK_STEPS));
        assert!(
            counter(&SCANNED) <= 16 * n,
            "{} segments scanned",
            counter(&SCANNED)
        );
        assert!(t.elapsed().as_secs() < 60, "{:?}", t.elapsed());
    }

    #[test]
    fn index_of_is_by_address_not_by_value() {
        let m = tree();
        for (i, s) in m.segments().iter().enumerate() {
            assert_eq!(m.index_of(s), Some(i));
        }
        let copy = m.segments()[2].clone();
        assert_eq!(copy, m.segments()[2]);
        assert_eq!(
            m.index_of(&copy),
            None,
            "equal by value, but not stored in the map"
        );
        assert_eq!(SourceMap::new(Vec::new()).index_of(&copy), None);
    }

    /// `range(0, 1)` + `n` stream functions + an output, as Marigold source.
    fn chain_of(n: usize, write_file: bool) -> String {
        let output = if write_file {
            "write_file(\"o.csv\", csv)"
        } else {
            "return"
        };
        format!("range(0, 1){}.{output}", ".map(f)".repeat(n))
    }

    #[test]
    fn pairing_parts_with_pieces_does_linear_work_in_the_chain_length() {
        for write_file in [false, true] {
            for n in [100_usize, 400, 1_600] {
                PAIR_STEPS.with(|c| c.set(0));
                let program = crate::marigold_generate(&chain_of(n, write_file)).unwrap();
                let steps = counter(&PAIR_STEPS);
                let expected_children = n + 1 + 2 * usize::from(write_file);
                assert_eq!(program.map.children(0).len(), expected_children);
                assert!(
                    steps >= expected_children && steps <= 2 * expected_children + 4,
                    "write_file={write_file} n={n}: {steps} steps"
                );
            }
        }
    }
}
