//! Map rustc diagnostics and runtime locations in generated Rust back to Marigold source.
//!
//! [`crate::marigold_generate`] returns Rust code and a [`SourceMap`](crate::source_map::SourceMap)
//! that says which Marigold expression produced which part of it. After that code is
//! written to a real file and compiled, rustc reports problems as byte offsets,
//! lines and columns *in that file* (see [`crate::rustc_json`]). A [`Remapper`] turns such
//! a position into a position in the Marigold source, and says how far to trust it.
//! Everything here is pure computation: no files are read, no process is run.
//!
//! # Precision
//!
//! Marigold expressions are translated, not copied, so most of the generated code has no
//! counterpart character by character. Every mapped span therefore carries a
//! [`Precision`], from best to worst:
//!
//! * [`Precision::Exact`]: the generated text is proven byte-for-byte identical to the
//!   Marigold text. Two things are proven that way (see `verbatim_len`): a segment whose
//!   whole generated text equals its Marigold text (any kind: the finer-grained body of a
//!   function is one), and, for a `fn` ([`SegmentKind::Function`]) alone, the common tail of
//!   its generated and Marigold text, because the generated code is `const ` plus the source.
//!   The range is the exact same text in the Marigold source, and a zero-width insertion
//!   point stays zero-width. A kind that is only *partly* verbatim (a function signature
//!   that is generated as `const fn <signature>`) is [`Precision::Segment`], never a guessed
//!   sub-range: a shared last byte such as `)` proves nothing.
//! * [`Precision::Segment`]: the span lies inside one [`Segment`];
//!   the range is that segment's whole source (never a guessed sub-range).
//! * [`Precision::Expression`]: the span leaves its segment into generated glue or into
//!   other segments of the *same* expression; the range is the widest segment source of
//!   that expression.
//! * [`Precision::Unmapped`]: no Marigold range is given, with an [`UnmappedReason`].
//!   A range is present exactly when the precision is not `Unmapped`.
//!
//! When segments nest (finer-grained segments), the innermost one wins and the ladder
//! gets finer automatically. A span that touches two different expressions is
//! [`UnmappedReason::MultipleExpressions`]; a span in code that no expression owns (the
//! preamble, the stream wiring) is [`UnmappedReason::Glue`].
//!
//! # What the numbers mean
//!
//! Byte offsets are authoritative. rustc's line, column and source text are used only
//! as a checksum: they are recomputed from the byte offsets in the generated Rust you
//! gave the [`Remapper`], and if any of them disagrees the file on disk is not the code
//! the map describes, so the span is [`UnmappedReason::StaleGeneratedFile`] rather than
//! silently pointing at the wrong place. A span whose four line/column numbers are all
//! `0` is treated as "not provided" and is checked by bytes alone.
//! Marigold positions ([`LineCol`]) are 1-based; the column counts Unicode scalar
//! values, like `marigold check`. Mapped ranges always lie inside the source you gave
//! the remapper and on character boundaries; a map that does not fit that source
//! gives [`UnmappedReason::StaleSourceMap`].
//!
//! Compile-time and run-time positions use different column units; see [`TraceColumnUnit`].
//!
//! # Recipes
//!
//! **Compile errors.** Read cargo's stream with [`rustc_json::parse_messages`](crate::rustc_json::parse_messages),
//! keep the records of your package (`package_id`, or `manifest_path` of the record), decide
//! "is this the generated file" with a closure over [`lexical_path_matches`], build one
//! [`Remapper`] per generated program, call [`Remapper::remap`] on each diagnostic and show
//! [`MappedDiagnostic::anchor`] (skipping [`MappedDiagnostic::likely_generated_noise`]):
//!
//! ```
//! use marigold_grammar::marigold_generate;
//! use marigold_grammar::remap::{lexical_path_matches, Precision, Remapper};
//! use marigold_grammar::rustc_json::parse_messages;
//! use std::path::Path;
//!
//! let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).return";
//! let program = marigold_generate(src).unwrap();
//! let at = program.rust.find("{ x }").unwrap() + 2;
//! let stream = format!(
//!     "{{\"reason\":\"compiler-message\",\"package_id\":\"app 0.1.0\",\"message\":{{\"message\":\"m\",\"level\":\"error\",\"children\":[],\"spans\":[{{\"file_name\":\"src/gen.rs\",\"byte_start\":{at},\"byte_end\":{},\"is_primary\":true}}]}}}}\n",
//!     at + 1,
//! );
//! let root = Path::new("/work/app");
//! let generated = Path::new("/work/app/src/gen.rs");
//! let is_generated = |file: &str| lexical_path_matches(root, file, generated);
//! let remapper = Remapper::new(&program, src, &is_generated);
//! let parsed = parse_messages(&stream);
//! let shown: Vec<_> = parsed.messages.iter()
//!     .filter(|m| m.package_id.as_deref() == Some("app 0.1.0"))
//!     .map(|m| remapper.remap(&m.diagnostic))
//!     .filter(|d| !d.likely_generated_noise())
//!     .filter_map(|d| d.anchor().map(|a| a.precision))
//!     .collect();
//! assert_eq!(shown, [Precision::Exact]);
//! ```
//!
//! **Panics.** [`Remapper::remap_trace`] returns the `file:line:column` tokens of a panic
//! message or backtrace with their byte range in the text. Splice the Marigold position in
//! place of each token, last to first so the earlier ranges stay valid:
//!
//! ```
//! use marigold_grammar::marigold_generate;
//! use marigold_grammar::remap::Remapper;
//!
//! let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).return";
//! let program = marigold_generate(src).unwrap();
//! let is_generated = |file: &str| file == "src/gen.rs";
//! let remapper = Remapper::new(&program, src, &is_generated);
//! let at = program.rust.find("{ x }").unwrap() + 2;
//! let line = program.rust[..at].matches('\n').count() + 1;
//! let column = at - program.rust[..at].rfind('\n').map_or(0, |i| i + 1) + 1;
//! let mut text = format!("thread 'main' panicked at src/gen.rs:{line}:{column}:\nboom");
//! for hit in remapper.remap_trace(&text.clone()).iter().rev() {
//!     if let Some(m) = hit.mapped.marigold.as_ref() {
//!         text.replace_range(hit.token.start..hit.token.end, &format!("<marigold>:{}:{}", m.start.line, m.start.column));
//!     }
//! }
//! assert!(text.contains("panicked at <marigold>:1:23:"), "{text}");
//! ```
//!
//! # Limits, honestly
//!
//! * Today's `marigold::m!(..)` macro hands rustc one expansion; an error in it has a single
//!   span that points into the macro call, which is a file that is not the generated one:
//!   such errors are [`UnmappedReason::ForeignFile`]. Mapping needs the generated code to be
//!   a real file (`include!`, or a wrapper declared with [`Remapper::with_prefix`]).
//! * [`Remapper::locate`] and [`Remapper::remap_trace`] work on bare line and column numbers
//!   and cannot check them against rustc's source text, so they have no staleness check:
//!   they believe that the file they were given is the one that ran.
//! * The display-width table used for panic columns (`DisplayWidth`) was measured with
//!   rustc 1.97 for a set of ranges and is not total: a character outside it makes the
//!   column untrusted (at best [`Precision::Segment`]).
//!
//! # What is not translated
//!
//! * `rendered`, the `raw` span and the *message text* talk about generated code (names
//!   like `returning_stream_0`, positions like `src/gen.rs:35:72`); do not show them as
//!   if they were Marigold.
//! * Suggestions (`suggested_replacement`) edit generated text. They are carried
//!   along and flagged with [`Suggestion::applicable_to_marigold`], which is true only
//!   when *every* replacement of the diagnostic (a multipart suggestion has several) is
//!   for an [`Precision::Exact`] span reached directly, the diagnostic offers no
//!   alternatives (several replacements for the same place are a choice, not an edit), and
//!   each edit survives regeneration: rustc calls it `MachineApplicable` when it says
//!   anything, it is not in an enum variant, it is an identifier when the span is in
//!   generated wiring, and it is not a whitespace edit.
//! * Warnings from glue the user never wrote (rustc reports 1-3 `unused_mut` warnings
//!   for every program) are recognisable with [`MappedDiagnostic::likely_generated_noise`]:
//!   a warning is not noise when it points into code the user wrote, at any precision.
//!
//! ```
//! use marigold_grammar::marigold_generate;
//! use marigold_grammar::remap::{Precision, Remapper};
//! use marigold_grammar::rustc_json::parse_message_line;
//!
//! let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
//! let program = marigold_generate(src).unwrap();
//!
//! let start = program.rust.find("{ x }").unwrap() + 2;
//! let json = format!(
//!     r#"{{"$message_type":"diagnostic","message":"mismatched types","level":"error",
//!        "code":{{"code":"E0308"}},"children":[],
//!        "spans":[{{"file_name":"src/gen.rs","byte_start":{start},"byte_end":{end},
//!                  "is_primary":true,"label":"expected `u8`"}}]}}"#,
//!     end = start + 1,
//! );
//! let diagnostic = parse_message_line(&json).unwrap().unwrap().diagnostic;
//!
//! let is_generated = |file: &str| file == "src/gen.rs";
//! let remapper = Remapper::new(&program, src, &is_generated);
//! let mapped = remapper.remap(&diagnostic);
//!
//! let primary = mapped.primary.as_ref().unwrap();
//! assert_eq!(primary.precision, Precision::Exact);
//! let range = primary.marigold.as_ref().unwrap().range;
//! assert_eq!(&src[range.start..range.end], "x");
//! assert_eq!((primary.marigold.as_ref().unwrap().start.line, primary.marigold.as_ref().unwrap().start.column), (1, 23));
//! ```

use crate::diagnostics::ByteRange;
pub use crate::line_index::LineCol;
use crate::line_index::{ColumnUnit, LineIndex};
use crate::rustc_json::{Level, RustcDiagnostic, RustcSpan};
use crate::source_map::{GeneratedProgram, Segment, SegmentKind};
use serde::Serialize;
use std::path::{Component, Path, PathBuf};

/// Expansion hops followed from a span in a library file to the call site in the
/// generated file. A longer chain is [`UnmappedReason::ForeignFile`].
const MAX_EXPANSION_HOPS: usize = 64;

/// Where in the Marigold source a span was mapped to.
///
/// `range` is a half-open byte range into the exact source given to
/// [`Remapper::new`], on character boundaries; `start` and `end` are the matching
/// line/column positions. A zero-width range (`start == end`) is an insertion point.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::Remapper;
///
/// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
///
/// assert!(remapper.locate("gen.rs", 1, None, marigold_grammar::remap::TraceColumnUnit::Bytes).marigold.is_none());
/// let at = program.rust.find("{ x }").unwrap() + 2;
/// let line = program.rust[..at].matches('\n').count() + 1;
/// let loc = remapper.locate("gen.rs", line, None, marigold_grammar::remap::TraceColumnUnit::Bytes).marigold.unwrap();
/// assert_eq!((loc.range.start, loc.range.end), (0, 25));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MarigoldLocation {
    /// Half-open byte range in the Marigold source.
    pub range: ByteRange,
    /// Line and column of `range.start`.
    pub start: LineCol,
    /// Line and column of `range.end`.
    pub end: LineCol,
}

/// The part of the source map a span was attributed to.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{TraceColumnUnit, Remapper};
/// use marigold_grammar::source_map::SegmentKind;
///
/// let src = "fn f(x: i32) -> i32 { x }";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |_: &str| true;
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let line = program.rust[..program.rust.find("const fn").unwrap()].matches('\n').count() + 1;
/// let segment = remapper.locate("x.rs", line, None, TraceColumnUnit::Chars).segment.unwrap();
/// assert_eq!((segment.kind, segment.expr_index, segment.name.as_deref()), (SegmentKind::Function, 0, Some("f")));
/// assert_eq!(program.map.segments()[segment.index].kind, SegmentKind::Function);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SegmentRef {
    /// Index of the segment in [`SourceMap::segments`](crate::source_map::SourceMap::segments)
    /// of the program given to the remapper.
    pub index: usize,
    /// What kind of expression produced the code.
    pub kind: SegmentKind,
    /// Index of the expression among the top-level expressions of the program.
    pub expr_index: usize,
    /// The name the expression declares, if it declares one.
    pub name: Option<String>,
}

/// Why a span has no Marigold range.
///
/// Serializes as `snake_case`.
///
/// ```
/// use marigold_grammar::remap::UnmappedReason;
///
/// assert_eq!(serde_json::to_string(&UnmappedReason::ForeignFile).unwrap(), "\"foreign_file\"");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum UnmappedReason {
    /// The span is in a file that is not the generated one (the standard library, the
    /// hand-written wrapper around the generated code, ...) and no macro expansion leads
    /// back into the generated file.
    ForeignFile,
    /// The span starts or ends outside the generated program: in the wrapper around it
    /// (see [`Remapper::with_prefix`]) or past its end.
    OutsideGeneratedRegion,
    /// The span lies in generated code that belongs to no Marigold expression.
    Glue,
    /// The span covers parts of two or more different Marigold expressions.
    MultipleExpressions,
    /// rustc's line, column or text disagree with the generated Rust the remapper was
    /// given: the file on disk is not that code.
    StaleGeneratedFile,
    /// The offsets are not a valid span of the generated code (inverted, or not on
    /// character boundaries), or a runtime column does not exist on its line.
    InvalidSpan,
    /// The source map does not fit the Marigold source given to the remapper.
    StaleSourceMap,
}

/// How precisely a span was mapped; see the [module documentation](self).
///
/// Serializes as an object tagged with `kind`: `{"kind": "exact"}`,
/// `{"kind": "segment"}`, `{"kind": "expression"}`,
/// `{"kind": "unmapped", "reason": "glue"}`.
///
/// ```
/// use marigold_grammar::remap::{Precision, UnmappedReason};
///
/// assert_eq!(serde_json::to_string(&Precision::Segment).unwrap(), r#"{"kind":"segment"}"#);
/// let glue = Precision::Unmapped { reason: UnmappedReason::Glue };
/// assert_eq!(serde_json::to_string(&glue).unwrap(), r#"{"kind":"unmapped","reason":"glue"}"#);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[non_exhaustive]
pub enum Precision {
    /// The generated text is byte-identical to the Marigold text at `marigold.range`.
    Exact,
    /// The span lies inside one segment; `marigold.range` is that segment's source.
    Segment,
    /// The span leaves its segment into glue or sibling segments of the same
    /// expression; `marigold.range` is the widest source among them.
    Expression,
    /// No Marigold range.
    Unmapped {
        /// Why.
        reason: UnmappedReason,
    },
}

/// A span exactly as rustc reported it, in the generated file.
///
/// It is kept even when mapping went through a macro expansion, so a consumer can
/// always show what rustc said. It describes generated code.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::Remapper;
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |_: &str| true;
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let span: RustcSpan = serde_json::from_str(r#"{"file_name":"a.rs","byte_start":0,"byte_end":3,"line_start":1,"column_start":1}"#).unwrap();
/// let raw = remapper.map_span(&span).raw.unwrap();
/// assert_eq!((raw.file_name.as_str(), raw.byte_start, raw.byte_end, raw.line_start), ("a.rs", 0, 3, 1));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct ReportedSpan {
    /// The file as rustc named it.
    pub file_name: String,
    /// Start byte in that file.
    pub byte_start: usize,
    /// End byte in that file.
    pub byte_end: usize,
    /// 1-based line of the start.
    pub line_start: usize,
    /// 1-based column of the start (scalar values).
    pub column_start: usize,
    /// 1-based line of the end.
    pub line_end: usize,
    /// 1-based column of the end (scalar values).
    pub column_end: usize,
}

/// A rustc suggestion: text that would replace the span in the *generated* file.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::Remapper;
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |_: &str| true;
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let span: RustcSpan = serde_json::from_str(r#"{"byte_start":0,"byte_end":0,"suggested_replacement":"","suggestion_applicability":"MachineApplicable"}"#).unwrap();
/// let suggestion = remapper.map_span(&span).suggestion.unwrap();
/// assert_eq!(suggestion.replacement, "");
/// assert_eq!(suggestion.applicability.as_deref(), Some("MachineApplicable"));
/// assert_eq!((suggestion.part, suggestion.parts), (0, 1));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Suggestion {
    /// The replacement text.
    pub replacement: String,
    /// rustc's applicability (`MachineApplicable`, `MaybeIncorrect`, ...).
    pub applicability: Option<String>,
    /// Position of this replacement among the *places* the diagnostic edits, in rustc's
    /// span order, from 0. Spans with the same or overlapping byte range are one place
    /// (see [`alternative`](Self::alternative)). A single mapped span
    /// ([`Remapper::map_span`]) is part 0 of 1.
    pub part: usize,
    /// How many places the diagnostic edits. More than one is a multipart suggestion (like
    /// "try wrapping the expression in `Some`": insert `Some(` here and `)` there), which
    /// is only meaningful as a whole.
    pub parts: usize,
    /// Which of the replacements for this place this is, from 0, in rustc's order. rustc
    /// lists mutually exclusive choices ("consider importing one of these enums") as several
    /// spans with the same range; they are alternatives, not parts of one edit.
    pub alternative: usize,
    /// How many replacements the diagnostic offers for this place. More than one: choose one.
    pub alternatives: usize,
    /// Whether the suggestion can be applied to the Marigold source. It is decided for the
    /// whole diagnostic and is the same on every replacement: true only when *every* span
    /// of the diagnostic that carries a replacement is [`Precision::Exact`] and was reached
    /// directly (not through a macro call site), and there are no alternatives to choose
    /// from. Applying one half of a multipart suggestion would break the program, so one
    /// inexact part makes all of them false; a set of alternatives is not an edit at all,
    /// so it is never applicable as a set (to apply one alternative, build that edit from
    /// the one replacement). It is also false when the edit would not survive regeneration:
    /// rustc's own applicability is present and not `MachineApplicable`; the span is in an
    /// enum variant; the span is in generated wiring (an input, a stream stage, an output)
    /// and the replacement is not a Marigold identifier; the replaced text is only
    /// whitespace; or the replacement changes only whitespace.
    pub applicable_to_marigold: bool,
}

/// One span of a diagnostic, mapped to Marigold.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{Precision, Remapper};
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
///
/// let at = program.rust.find("returning_stream_0").unwrap();
/// let span: RustcSpan = serde_json::from_str(&format!(
///     r#"{{"file_name":"gen.rs","byte_start":{at},"byte_end":{},"is_primary":true,"label":"here"}}"#, at + 5,
/// )).unwrap();
/// let mapped = remapper.map_span(&span);
/// assert_eq!(mapped.precision, Precision::Segment);
/// assert_eq!(mapped.label.as_deref(), Some("here"));
/// let range = mapped.marigold.unwrap().range;
/// assert_eq!(&src[range.start..range.end], "range(0, 1).return");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MappedSpan {
    /// Copied from rustc.
    pub is_primary: bool,
    /// Copied from rustc; it may mention generated names.
    pub label: Option<String>,
    /// Where it is in the Marigold source; `None` exactly when
    /// [`precision`](Self::precision) is [`Precision::Unmapped`].
    pub marigold: Option<MarigoldLocation>,
    /// How far to trust `marigold`.
    pub precision: Precision,
    /// The segment the span was attributed to; present exactly when mapped.
    pub segment: Option<SegmentRef>,
    /// The span as rustc reported it. `None` for [`Remapper::locate`], which has no rustc span.
    pub raw: Option<ReportedSpan>,
    /// If the span was found by following a macro expansion to its call site, the name of
    /// the macro whose call site that was (like `format!`).
    pub via_macro: Option<String>,
    /// The replacement rustc suggests for the span, if any.
    pub suggestion: Option<Suggestion>,
}

/// A rustc diagnostic with all its spans mapped.
///
/// Layers, from headline to detail: [`message`](Self::message) and
/// [`anchor`](Self::anchor); the precision and [`segment`](MappedSpan::segment) of the
/// spans; [`secondary`](Self::secondary) spans and [`children`](Self::children) (notes,
/// help); and last the raw rustc data ([`rendered`](Self::rendered),
/// [`raw`](MappedSpan::raw)), which describes generated code.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::Remapper;
/// use marigold_grammar::rustc_json::parse_message_line;
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let line = r#"{"$message_type":"diagnostic","message":"aborting due to 1 previous error","level":"error","spans":[],"children":[]}"#;
/// let mapped = remapper.remap(&parse_message_line(line).unwrap().unwrap().diagnostic);
/// assert_eq!(mapped.message, "aborting due to 1 previous error");
/// assert!(mapped.primary.is_none() && mapped.anchor().is_none());
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct MappedDiagnostic {
    /// Severity.
    pub level: Level,
    /// The error or lint code, `None` when rustc gave none or an empty one.
    pub code: Option<String>,
    /// rustc's message, unchanged; it may mention generated names.
    pub message: String,
    /// The first span flagged primary, or the first span if none is flagged.
    pub primary: Option<MappedSpan>,
    /// All other spans in rustc's order (other primary spans keep their flag).
    pub secondary: Vec<MappedSpan>,
    /// Notes and help, mapped recursively.
    pub children: Vec<MappedDiagnostic>,
    /// rustc's pre-rendered text. It describes the generated file; do not show it as Marigold.
    pub rendered: Option<String>,
}

impl MappedDiagnostic {
    /// The span to point the user at: the first span that has a Marigold range, looking at
    /// the primary span, then the secondary spans, then the children depth-first.
    /// `None` when nothing in the diagnostic belongs to Marigold.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    /// use marigold_grammar::rustc_json::parse_message_line;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// let at = program.rust.find("returning_stream_0").unwrap();
    /// let line = format!(
    ///     r#"{{"$message_type":"diagnostic","message":"m","level":"error","spans":[{{"file_name":"other.rs","byte_start":0,"byte_end":1,"is_primary":true}}],
    ///        "children":[{{"message":"n","level":"note","spans":[{{"file_name":"gen.rs","byte_start":{at},"byte_end":{}}}]}}]}}"#, at + 1,
    /// );
    /// let mapped = remapper.remap(&parse_message_line(&line).unwrap().unwrap().diagnostic);
    /// assert!(mapped.primary.as_ref().unwrap().marigold.is_none());
    /// assert!(mapped.anchor().unwrap().marigold.is_some());
    /// ```
    pub fn anchor(&self) -> Option<&MappedSpan> {
        self.primary
            .iter()
            .chain(self.secondary.iter())
            .find(|s| s.marigold.is_some())
            .or_else(|| self.children.iter().find_map(MappedDiagnostic::anchor))
    }

    /// The spans of this diagnostic (not its children) that carry a suggested replacement,
    /// in rustc's order. A multipart suggestion has several; see [`Suggestion::parts`].
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    /// use marigold_grammar::rustc_json::parse_message_line;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// let line = r#"{"$message_type":"diagnostic","message":"m","level":"help","children":[],"spans":[
    ///     {"file_name":"gen.rs","byte_start":0,"byte_end":0,"suggested_replacement":"Some("},
    ///     {"file_name":"gen.rs","byte_start":3,"byte_end":3,"suggested_replacement":")"}]}"#;
    /// let mapped = remapper.remap(&parse_message_line(line).unwrap().unwrap().diagnostic);
    /// let parts: Vec<_> = mapped.suggestion_spans().iter().map(|s| s.suggestion.as_ref().unwrap().replacement.as_str()).collect();
    /// assert_eq!(parts, ["Some(", ")"]);
    /// ```
    pub fn suggestion_spans(&self) -> Vec<&MappedSpan> {
        let mut spans: Vec<&MappedSpan> = self
            .primary
            .iter()
            .chain(self.secondary.iter())
            .filter(|s| s.suggestion.is_some())
            .collect();
        spans.sort_by_key(|s| {
            s.suggestion
                .as_ref()
                .map_or((0, 0), |g| (g.part, g.alternative))
        });
        spans
    }

    /// Whether this is probably a warning about code the user did not write.
    ///
    /// True for a warning whose [`anchor`](Self::anchor) is missing, or is not in code the
    /// user wrote (stream wiring, the preamble, glue): a lint that is not provably about the
    /// user's own text (rustc emits one to three `unused_mut` warnings for every program,
    /// from generated glue). Text is the user's when it is a Marigold `fn` (by the *kind* of
    /// the anchor's segment, at any precision: `unused variable` on a parameter in a
    /// signature that is only [`Precision::Segment`] is shown) or when it is proven
    /// byte-identical to the Marigold text ([`Precision::Exact`], of any kind). Errors,
    /// notes and help are never noise. This is a heuristic for filtering; it does not look
    /// at the lint's name.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    /// use marigold_grammar::rustc_json::parse_message_line;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// let line = |level: &str| format!(
    ///     r#"{{"$message_type":"diagnostic","message":"variable does not need to be mutable","level":"{level}","spans":[{{"file_name":"gen.rs","byte_start":0,"byte_end":1,"is_primary":true}}],"children":[]}}"#
    /// );
    /// let warning = remapper.remap(&parse_message_line(&line("warning")).unwrap().unwrap().diagnostic);
    /// let error = remapper.remap(&parse_message_line(&line("error")).unwrap().unwrap().diagnostic);
    /// assert!(warning.likely_generated_noise());
    /// assert!(!error.likely_generated_noise());
    /// ```
    pub fn likely_generated_noise(&self) -> bool {
        self.level == Level::Warning
            && !self.anchor().is_some_and(|anchor| {
                anchor.precision == Precision::Exact
                    || anchor
                        .segment
                        .as_ref()
                        .is_some_and(|segment| is_user_authored(segment.kind))
            })
    }
}

/// The unit of a column in a run-time location. Three of them are in use and they must not
/// be mixed.
///
/// [`Chars`](Self::Chars) and [`Bytes`](Self::Bytes) are the units of
/// [`crate::line_index::ColumnUnit`] (same names, same JSON spelling: `"chars"`, `"bytes"`);
/// [`DisplayWidth`](Self::DisplayWidth) exists only here, because it needs the width table
/// that [`Remapper::locate`] describes and is only what panic messages print.
///
/// | where the column comes from | unit |
/// |---|---|
/// | rustc's JSON diagnostics (`column_start`, ...) | [`Chars`](Self::Chars) |
/// | the `column!()` macro | [`Chars`](Self::Chars) |
/// | the panic message `panicked at src/gen.rs:4:43`, `Location::caller().column()` | [`DisplayWidth`](Self::DisplayWidth) |
/// | a backtrace frame `at ./src/gen.rs:4:48` (DWARF) | [`Bytes`](Self::Bytes) |
///
/// On a line with text outside ASCII they disagree: for a `panic!` after `"héllo→日本"`
/// the scalar column is 41, the display column 43 and the byte column 48. Rows were
/// measured with rustc 1.97 (`column!()` 63 and `Location::caller().column()` 76 on one
/// line, where the byte column was 67); see [`Remapper::locate`] for how unknown characters
/// are treated.
///
/// ```
/// use marigold_grammar::remap::TraceColumnUnit;
///
/// assert_eq!(serde_json::to_string(&TraceColumnUnit::Bytes).unwrap(), "\"bytes\"");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum TraceColumnUnit {
    /// Unicode scalar values (`char`s); a tab is one.
    Chars,
    /// Terminal display width: most characters 1, East Asian wide characters and emoji 2,
    /// combining marks and zero-width characters 0, a tab 4.
    DisplayWidth,
    /// UTF-8 bytes.
    Bytes,
}

/// A source location found in text such as a panic message or a backtrace.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{TraceColumnUnit, Remapper};
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "src/gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let text = "thread 'main' panicked at src/gen.rs:1:1:\nboom";
/// let hit = &remapper.remap_trace(text)[0];
/// assert_eq!(&text[hit.token.start..hit.token.end], "src/gen.rs:1:1");
/// assert_eq!((hit.line, hit.column, hit.unit), (1, Some(1), TraceColumnUnit::DisplayWidth));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct TracedLocation {
    /// Byte range of the `file:line:column` text in the scanned text, on character boundaries.
    pub token: ByteRange,
    /// The file as written in the text.
    pub file: String,
    /// 1-based line in that file.
    pub line: usize,
    /// 1-based column, if the text had one.
    pub column: Option<usize>,
    /// The unit the column is in, from the context it was found in.
    pub unit: TraceColumnUnit,
    /// The result of [`Remapper::locate`] for this location.
    pub mapped: Located,
}

/// Where a run-time location (a line, optionally a column) in the generated file is in
/// the Marigold source: the result of [`Remapper::locate`].
///
/// It has the fields of a [`MappedSpan`] that make sense without a rustc span: no flags,
/// label, raw span, macro or suggestion.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{Precision, Remapper};
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let located = remapper.locate_line("gen.rs", 1);
/// assert!(matches!(located.precision, Precision::Unmapped { .. }) && located.marigold.is_none());
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Located {
    /// Where it is in the Marigold source; `None` exactly when
    /// [`precision`](Self::precision) is [`Precision::Unmapped`].
    pub marigold: Option<MarigoldLocation>,
    /// How far to trust `marigold`.
    pub precision: Precision,
    /// The segment the location was attributed to; present exactly when mapped.
    pub segment: Option<SegmentRef>,
}

/// One usable segment of the source map, with the data needed to look spans up.
struct Slot {
    /// Index into `GeneratedProgram::map.segments()`.
    segment: usize,
    /// The generated range (non-empty, inside the program).
    start: usize,
    end: usize,
    /// The closest earlier slot whose range contains this one.
    parent: Option<usize>,
    /// The largest `end` among this slot and all earlier ones.
    max_end: usize,
    /// Whether the Marigold range fits the source (in bounds, on character boundaries).
    source_ok: bool,
    /// Length in bytes of the part at the end of the generated range that is byte-identical
    /// to the end of the Marigold range (see `verbatim_len`); zero when nothing is proven.
    verbatim: usize,
}

/// How a span was placed in the source map.
struct Placement {
    precision: Precision,
    /// The slot the placement is attributed to.
    slot: usize,
    /// The Marigold range.
    range: ByteRange,
}

#[cfg(test)]
thread_local! {
    /// Slots looked at while collecting the segments a span overlaps.
    static SCANNED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// Parent links followed while looking for the innermost segment around a position.
    static WALKED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    /// One entry per binary search: the length of the searched slice and how often the
    /// predicate was called.
    static SEARCHES: std::cell::RefCell<Vec<(usize, usize)>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// The first index of `items` at which `pred` is false, for a `pred` that is true on a prefix.
///
/// Every binary search of the remapper goes through here, so that the tests can count the
/// searches and their predicate calls.
#[cfg(test)]
fn search<T>(items: &[T], mut pred: impl FnMut(&T) -> bool) -> usize {
    let mut probes = 0;
    let at = items.partition_point(|item| {
        probes += 1;
        pred(item)
    });
    SEARCHES.with(|log| log.borrow_mut().push((items.len(), probes)));
    at
}

#[cfg(not(test))]
fn search<T>(items: &[T], pred: impl FnMut(&T) -> bool) -> usize {
    items.partition_point(pred)
}

#[cfg(test)]
fn count(counter: &'static std::thread::LocalKey<std::cell::Cell<usize>>) {
    counter.with(|c| c.set(c.get() + 1));
}

/// Maps rustc diagnostics and run-time locations in generated Rust to the Marigold source.
///
/// Build it once per generated program ([`Remapper::new`] indexes the generated text, the
/// Marigold source and the source map in time linear in their sizes); every diagnostic is
/// then mapped in `O(log n + k)` for `n` segments and the `k` segments the span touches,
/// plus the length of the lines it sits on (to count columns). For segments that nest, as
/// the finer-grained segments of a later release will, `k` also counts the segments inside
/// the enclosing one that end before the span starts. All methods take `&self`. The remapper
/// borrows the program, the source and the file test, and is not `Send` or `Sync` because
/// the file test is not required to be.
///
/// The generated code does not have to be the whole file: declare what comes before it
/// with [`with_prefix`](Self::with_prefix); anything after it is simply outside.
///
/// Nothing here panics, whatever the numbers in the rustc data or the source map are:
/// out-of-range, inverted and inconsistent input is reported as an
/// [`Precision::Unmapped`] span with a reason, and a segment that does not fit the
/// program or the source is never used to build a range.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{TraceColumnUnit, Precision, Remapper};
///
/// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file.ends_with("gen.rs");
/// let remapper = Remapper::new(&program, src, &is_generated);
///
/// let at = program.rust.find("{ x }").unwrap() + 2;
/// let line = program.rust[..at].matches('\n').count() + 1;
/// let column = at - program.rust[..at].rfind('\n').map_or(0, |i| i + 1) + 1;
/// let mapped = remapper.locate("./src/gen.rs", line, Some(column), TraceColumnUnit::Chars);
/// assert_eq!(mapped.precision, Precision::Exact);
/// ```
pub struct Remapper<'a> {
    program: &'a GeneratedProgram,
    source: &'a str,
    is_generated: &'a dyn Fn(&str) -> bool,
    /// Bytes of the generated file before the program.
    prefix_len: usize,
    /// Number of `\n` in the prefix.
    prefix_newlines: usize,
    /// The prefix after its last `\n`: the part of the first program line before the program.
    prefix_tail: String,
    /// Usable segments in generated order.
    slots: Vec<Slot>,
    /// The lines of the generated program.
    rust_index: LineIndex<'a>,
    /// The lines of the Marigold source.
    source_index: LineIndex<'a>,
    /// Segments whose generated range does not fit the program.
    unusable: usize,
}

impl<'a> Remapper<'a> {
    /// Index a generated program and the Marigold source it was generated from.
    ///
    /// `source` must be the exact string passed to [`crate::marigold_generate`], and
    /// `program.rust` exactly the text of the generated file (after
    /// [`with_prefix`](Self::with_prefix)): the checks described in the
    /// [module documentation](self) detect a mismatch but cannot repair it.
    ///
    /// The generated code depends on the cargo features of this crate that were compiled in
    /// (`io`, `tokio`, `async-std`; `marigold emit --format json` records them under
    /// `features`). The program and map given here must come from the same features as the
    /// code that was compiled, for example `["io"]` for the command line tool; a map from
    /// other features describes other code, and shows up as stale spans.
    ///
    /// `is_generated` decides whether a file name, exactly as rustc printed it
    /// (`src/gen.rs`, `./src/gen.rs`, an absolute path), is the generated file; see
    /// [`lexical_path_matches`] for a helper. Segments whose generated range does not lie
    /// inside `program.rust` cannot be placed and are ignored.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    ///
    /// let src = "range(0, 3).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "src/gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// # let _ = remapper;
    /// ```
    pub fn new(
        program: &'a GeneratedProgram,
        source: &'a str,
        is_generated: &'a dyn Fn(&str) -> bool,
    ) -> Self {
        let rust = program.rust.as_str();
        let mut slots: Vec<Slot> = Vec::new();
        let mut open: Vec<usize> = Vec::new();
        let mut max_end = 0;
        let mut unusable = 0;
        for (index, segment) in program.map.segments().iter().enumerate() {
            let g = segment.generated;
            if g.start >= g.end || g.end > rust.len() {
                unusable += 1;
                continue;
            }
            while let Some(&top) = open.last() {
                if slots[top].end <= g.start {
                    open.pop();
                } else {
                    break;
                }
            }
            max_end = max_end.max(g.end);
            let source_ok = source_range_fits(source, segment.source);
            let verbatim = if source_ok {
                verbatim_len(segment.kind, rust, g, source, segment.source)
            } else {
                0
            };
            let parent = open.last().copied();
            open.push(slots.len());
            slots.push(Slot {
                segment: index,
                start: g.start,
                end: g.end,
                parent,
                max_end,
                source_ok,
                verbatim,
            });
        }
        Remapper {
            program,
            source,
            is_generated,
            prefix_len: 0,
            prefix_newlines: 0,
            prefix_tail: String::new(),
            slots,
            rust_index: LineIndex::new(rust),
            source_index: LineIndex::new(source),
            unusable,
        }
    }

    /// The generated program this remapper was built for.
    pub fn program(&self) -> &'a GeneratedProgram {
        self.program
    }

    /// The Marigold source this remapper was built for.
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// Bytes of the generated file before the program, as declared with
    /// [`with_prefix`](Self::with_prefix); 0 when none was.
    pub fn prefix_len(&self) -> usize {
        self.prefix_len
    }

    /// How many segments of the source map cannot be used because their generated range is
    /// empty or does not lie inside the program. Their bytes read as glue; a non-zero count
    /// means the source map and the generated text do not belong together.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |_: &str| true;
    /// assert_eq!(Remapper::new(&program, src, &is_generated).unusable_segments(), 0);
    /// ```
    pub fn unusable_segments(&self) -> usize {
        self.unusable
    }

    /// Declare the bytes of the generated file that come before `program.rust`.
    ///
    /// A wrapper like `#[tokio::main]\nasync fn main() {\n    ` followed by the program
    /// shifts every byte offset, every line number and, on the program's first line,
    /// every column. A leading byte order mark counts as bytes but not as a column, like
    /// in rustc. After this call rustc's numbers for the *file* are understood;
    /// spans that touch the prefix, or run past the end of the program, are
    /// [`UnmappedReason::OutsideGeneratedRegion`]. Text after the program needs no
    /// declaration.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{Precision, Remapper};
    /// use marigold_grammar::rustc_json::RustcSpan;
    ///
    /// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
    /// let program = marigold_generate(src).unwrap();
    /// let prefix = "fn main() {\n    ";
    /// let is_generated = |file: &str| file == "src/main.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated).with_prefix(prefix);
    ///
    /// let at = prefix.len() + program.rust.find("{ x }").unwrap() + 2;
    /// let span: RustcSpan = serde_json::from_str(&format!(
    ///     r#"{{"file_name":"src/main.rs","byte_start":{at},"byte_end":{}}}"#, at + 1,
    /// )).unwrap();
    /// assert_eq!(remapper.map_span(&span).precision, Precision::Exact);
    ///
    /// let wrapper: RustcSpan = serde_json::from_str(r#"{"file_name":"src/main.rs","byte_start":0,"byte_end":2}"#).unwrap();
    /// assert!(matches!(remapper.map_span(&wrapper).precision, Precision::Unmapped { .. }));
    /// ```
    #[must_use]
    pub fn with_prefix(mut self, prefix: &str) -> Self {
        self.prefix_len = prefix.len();
        self.prefix_newlines = prefix.bytes().filter(|&b| b == b'\n').count();
        let visible = prefix.strip_prefix('\u{feff}').unwrap_or(prefix);
        self.prefix_tail = visible
            .rsplit_once('\n')
            .map_or(visible, |(_, tail)| tail)
            .to_string();
        self
    }

    /// Map a whole diagnostic: its spans, and recursively its children.
    ///
    /// `primary` is the first span flagged primary, else the first span; `secondary` holds
    /// the others in rustc's order. A diagnostic without spans (like "aborting due to 2
    /// previous errors") maps to one without `primary`.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::Remapper;
    /// use marigold_grammar::rustc_json::parse_message_line;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// let line = r#"{"$message_type":"diagnostic","message":"m","code":{"code":"E0425"},"level":"error",
    ///     "spans":[{"file_name":"a.rs","byte_start":0,"byte_end":1},{"file_name":"b.rs","byte_start":0,"byte_end":1,"is_primary":true}],"children":[]}"#;
    /// let mapped = remapper.remap(&parse_message_line(line).unwrap().unwrap().diagnostic);
    /// assert_eq!(mapped.code.as_deref(), Some("E0425"));
    /// assert_eq!(mapped.primary.as_ref().unwrap().raw.as_ref().unwrap().file_name, "b.rs");
    /// assert_eq!(mapped.secondary[0].raw.as_ref().unwrap().file_name, "a.rs");
    /// ```
    pub fn remap(&self, d: &RustcDiagnostic) -> MappedDiagnostic {
        let primary_index = d
            .spans
            .iter()
            .position(|s| s.is_primary)
            .or(if d.spans.is_empty() { None } else { Some(0) });
        let mut spans: Vec<MappedSpan> = d.spans.iter().map(|s| self.map_span(s)).collect();
        number_suggestions(&mut spans, self.source);
        let primary = primary_index.map(|i| spans.remove(i));
        MappedDiagnostic {
            level: d.level.clone(),
            code: d
                .code
                .as_ref()
                .map(|c| c.code.clone())
                .filter(|c| !c.is_empty()),
            message: d.message.clone(),
            primary,
            secondary: spans,
            children: d.children.iter().map(|c| self.remap(c)).collect(),
            rendered: d.rendered.clone(),
        }
    }

    /// Map one rustc span to the Marigold source.
    ///
    /// The steps, in order:
    ///
    /// 1. **Which file.** If `is_generated` accepts the span's file, the span itself is used
    ///    (even when it carries a macro expansion: text the user wrote inside `println!(..)`
    ///    keeps its own position). Otherwise the macro expansion chain is followed hop by
    ///    hop, at most 64 hops, to the first call site in the generated file;
    ///    [`MappedSpan::via_macro`] names that macro. No chain, or no hit: [`UnmappedReason::ForeignFile`].
    /// 2. **Which bytes.** `byte_end < byte_start` is [`UnmappedReason::InvalidSpan`]; a span
    ///    that starts before the program, or ends after it, is
    ///    [`UnmappedReason::OutsideGeneratedRegion`]; offsets inside a multibyte character
    ///    are `InvalidSpan`.
    /// 3. **Checksum.** If rustc's line and column numbers (when any is non-zero), the text
    ///    of the first and last line and the highlighted columns disagree with the generated
    ///    code, the file is stale: [`UnmappedReason::StaleGeneratedFile`]. Lines between the
    ///    first and the last of a multi-line span are not compared.
    /// 4. **Which segment.** See [`Precision`]. A zero-width span (an insertion) belongs to
    ///    the innermost segment that contains its byte, else to the one that ends right
    ///    before it.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{Precision, Remapper, UnmappedReason};
    /// use marigold_grammar::rustc_json::RustcSpan;
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    ///
    /// let std_span: RustcSpan = serde_json::from_str(r#"{"file_name":"/rustc/abc/library/core/src/option.rs","byte_start":1,"byte_end":2}"#).unwrap();
    /// assert_eq!(
    ///     remapper.map_span(&std_span).precision,
    ///     Precision::Unmapped { reason: UnmappedReason::ForeignFile },
    /// );
    /// let past_the_end: RustcSpan = serde_json::from_str(r#"{"file_name":"gen.rs","byte_start":99999,"byte_end":100000}"#).unwrap();
    /// assert_eq!(
    ///     remapper.map_span(&past_the_end).precision,
    ///     Precision::Unmapped { reason: UnmappedReason::OutsideGeneratedRegion },
    /// );
    /// ```
    pub fn map_span(&self, s: &RustcSpan) -> MappedSpan {
        let raw = ReportedSpan {
            file_name: s.file_name.clone(),
            byte_start: s.byte_start,
            byte_end: s.byte_end,
            line_start: s.line_start,
            column_start: s.column_start,
            line_end: s.line_end,
            column_end: s.column_end,
        };
        let mut via_macro = None;
        let outcome = match self.resolve(s) {
            Some((target, via)) => {
                via_macro = via.map(str::to_string);
                self.place_span(target)
            }
            None => Err(UnmappedReason::ForeignFile),
        };
        let mut mapped = self.assemble(s.is_primary, s.label.clone(), outcome);
        mapped.raw = Some(raw);
        mapped.suggestion = s
            .suggested_replacement
            .as_ref()
            .map(|replacement| Suggestion {
                replacement: replacement.clone(),
                applicability: s.suggestion_applicability.clone(),
                part: 0,
                parts: 1,
                alternative: 0,
                alternatives: 1,
                applicable_to_marigold: false,
            });
        mapped.via_macro = via_macro;
        number_suggestions(std::slice::from_mut(&mut mapped), self.source);
        mapped
    }

    /// Map a run-time location in the generated file: a panic message, a backtrace
    /// frame, `Location::caller()`, tracing metadata.
    ///
    /// `file` goes through the file test as given, `line` is the 1-based line in the
    /// *file* (lines of a declared prefix are subtracted; lines outside the program are
    /// [`UnmappedReason::OutsideGeneratedRegion`]). With `column` `None` or `Some(0)` the
    /// whole line's text is classified as a span: a line that is only the text of a Marigold
    /// `fn` is [`Precision::Exact`], and one that also holds generated indentation before
    /// it is [`Precision::Expression`] (it leaves the segment into glue). With a column, the position is converted to a byte offset in the
    /// unit given and classified as a zero-width position: a column inside a character, or
    /// more than one past the end of the line, is [`UnmappedReason::InvalidSpan`]. On the
    /// program's first line, columns include the declared prefix's last partial line.
    ///
    /// Display widths come from a table sampled against rustc 1.97 (ASCII, Latin, Cyrillic,
    /// arrows, CJK, Hangul, fullwidth forms, emoji, combining marks, zero-width characters);
    /// it is pinned to that version and not total. If a character before the column is
    /// outside it, the column is not trusted and the result is the classification of the
    /// whole line, at best [`Precision::Segment`]. Within a run of zero-width characters the
    /// position after them is used. There is no staleness check: nothing in a bare line and
    /// column says what the file looked like.
    ///
    /// The result is a [`Located`]: the fields of a [`MappedSpan`] that make sense without
    /// a rustc span.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{TraceColumnUnit, Precision, Remapper};
    ///
    /// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "src/gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    ///
    /// let at = program.rust.find("{ x }").unwrap() + 2;
    /// let line_start = program.rust[..at].rfind('\n').map_or(0, |i| i + 1);
    /// let line = program.rust[..at].matches('\n').count() + 1;
    /// let located = remapper.locate("src/gen.rs", line, Some(at - line_start + 1), TraceColumnUnit::Bytes);
    /// assert_eq!(located.precision, Precision::Exact);
    /// let range = located.marigold.unwrap().range;
    /// assert_eq!(range.start, 22);
    /// assert_eq!(range.start, range.end);
    ///
    /// assert!(remapper.locate("src/other.rs", line, None, TraceColumnUnit::Bytes).marigold.is_none());
    /// ```
    pub fn locate(
        &self,
        file: &str,
        line: usize,
        column: Option<usize>,
        unit: TraceColumnUnit,
    ) -> Located {
        self.locate_capped(file, line, column, unit, false)
    }

    /// [`locate`](Self::locate), optionally never better than [`Precision::Segment`]: for a
    /// column whose unit is a guess.
    fn locate_capped(
        &self,
        file: &str,
        line: usize,
        column: Option<usize>,
        unit: TraceColumnUnit,
        cap: bool,
    ) -> Located {
        let outcome = if (self.is_generated)(file) {
            self.place_location(line, column.unwrap_or(0), unit)
                .and_then(|placement| {
                    if cap {
                        self.at_most_segment(placement)
                    } else {
                        Ok(placement)
                    }
                })
        } else {
            Err(UnmappedReason::ForeignFile)
        };
        self.located(outcome)
    }

    /// [`locate`](Self::locate) for a line without a column, like the `file:line` of a
    /// log record or a tracing event: the classification of the whole line's text.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{Precision, Remapper};
    ///
    /// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "src/gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    /// let line = program.rust[..program.rust.find("{ x }").unwrap()].matches('\n').count() + 1;
    /// let located = remapper.locate_line("src/gen.rs", line);
    /// assert_eq!(located.precision, Precision::Expression);
    /// let range = located.marigold.unwrap().range;
    /// assert_eq!(&src[range.start..range.end], "fn f(x: i32) -> i32 { x }");
    /// ```
    pub fn locate_line(&self, file: &str, line: usize) -> Located {
        self.locate(file, line, None, TraceColumnUnit::Chars)
    }

    /// Find every `file:line` and `file:line:column` in `text` whose file is the
    /// generated file, and [`locate`](Self::locate) each one.
    ///
    /// For panic output and backtraces. Hits come in text order and do not overlap.
    /// A file is the text before the location, back to the nearest whitespace, quote,
    /// `(`, `[`, `<` or backtick; a path that contains a space is therefore not
    /// recognised, rather than guessed. The column unit follows the context: the `at`
    /// line under a numbered backtrace frame (`   2: name`) is [`TraceColumnUnit::Bytes`]
    /// (DWARF), and anything else, `panicked at` and an `at file:line:col` in a message the
    /// program formatted from a `Location`, is [`TraceColumnUnit::DisplayWidth`]. The
    /// context is a guess from the text, so a program that prints its own numbered lines
    /// followed by an `at` line can still be misread. An `at` that starts its line without such
    /// a frame above it (a blank line in between) is ambiguous, and so is any line that holds an
    /// escape sequence (coloured output): their locations are mapped no better than
    /// [`Precision::Segment`].
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{TraceColumnUnit, Remapper};
    ///
    /// let src = "range(0, 1).return";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "src/gen.rs" || file == "./src/gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    ///
    /// let stderr = "thread 'main' panicked at src/gen.rs:1:1:\nboom\nstack backtrace:\n   0: f\n             at ./src/gen.rs:2:3\n";
    /// let hits = remapper.remap_trace(stderr);
    /// let units: Vec<_> = hits.iter().map(|h| (h.file.as_str(), h.line, h.column, h.unit)).collect();
    /// assert_eq!(units, [
    ///     ("src/gen.rs", 1, Some(1), TraceColumnUnit::DisplayWidth),
    ///     ("./src/gen.rs", 2, Some(3), TraceColumnUnit::Bytes),
    /// ]);
    /// ```
    pub fn remap_trace(&self, text: &str) -> Vec<TracedLocation> {
        let bytes = text.as_bytes();
        let mut hits = Vec::new();
        let mut token_start = 0;
        let mut position = 0;
        while position < bytes.len() {
            let Some(ch) = text[position..].chars().next() else {
                break;
            };
            if ch.is_whitespace() || matches!(ch, '"' | '\'' | '(' | '[' | '<' | '`') {
                token_start = position + ch.len_utf8();
                position = token_start;
                continue;
            }
            if ch != ':' {
                position += ch.len_utf8();
                continue;
            }
            let colon = position;
            position += 1;
            let Some((line, after_line)) = read_number(text, colon + 1) else {
                continue;
            };
            let mut end = after_line;
            let mut column = None;
            if bytes.get(after_line) == Some(&b':') {
                if let Some((col, after_col)) = read_number(text, after_line + 1) {
                    column = Some(col);
                    end = after_col;
                }
            }
            let file = &text[token_start..colon];
            if !file.is_empty() && (self.is_generated)(file) {
                let before = text[..token_start].trim_end_matches([' ', '\t']);
                let frame = before
                    .strip_suffix("at")
                    .is_some_and(is_frame_header_before);
                let line_start = text[..token_start].rfind('\n').map_or(0, |i| i + 1);
                let line_end = text[end..].find('\n').map_or(text.len(), |i| end + i);
                let coloured = text[line_start..line_end].contains('\u{1b}');
                let ambiguous = coloured
                    || (!frame
                        && before.strip_suffix("at").is_some_and(|rest| {
                            let rest = rest.trim_end_matches([' ', '\t']);
                            rest.is_empty() || rest.ends_with('\n')
                        }));
                let unit = if frame {
                    TraceColumnUnit::Bytes
                } else {
                    TraceColumnUnit::DisplayWidth
                };
                hits.push(TracedLocation {
                    token: ByteRange {
                        start: token_start,
                        end,
                    },
                    file: file.to_string(),
                    line,
                    column,
                    unit,
                    mapped: self.locate_capped(file, line, column, unit, ambiguous),
                });
            }
            token_start = end;
            position = end;
        }
        hits
    }
}

impl Remapper<'_> {
    /// Step 1: the span in the generated file that stands for `s`, and the macro that led to it.
    fn resolve<'s>(&self, s: &'s RustcSpan) -> Option<(&'s RustcSpan, Option<&'s str>)> {
        let mut current = s;
        let mut via = None;
        let mut hops = 0;
        loop {
            if (self.is_generated)(&current.file_name) {
                return Some((current, via));
            }
            let expansion = current.expansion.as_deref()?;
            if hops == MAX_EXPANSION_HOPS {
                return None;
            }
            hops += 1;
            via = Some(expansion.macro_decl_name.as_str());
            current = &expansion.span;
        }
    }

    /// Steps 2 to 4 for a span that is in the generated file.
    fn place_span(&self, s: &RustcSpan) -> Result<Placement, UnmappedReason> {
        if s.byte_end < s.byte_start {
            return Err(UnmappedReason::InvalidSpan);
        }
        let base = self.prefix_len;
        let region_end = base.saturating_add(self.program.rust.len());
        if s.byte_start < base || s.byte_end > region_end {
            return Err(UnmappedReason::OutsideGeneratedRegion);
        }
        let (a, b) = (s.byte_start - base, s.byte_end - base);
        let rust = &self.program.rust;
        if !rust.is_char_boundary(a) || !rust.is_char_boundary(b) {
            return Err(UnmappedReason::InvalidSpan);
        }
        if !self.is_consistent(s, a, b) {
            return Err(UnmappedReason::StaleGeneratedFile);
        }
        self.classify(a, b)
    }

    /// Build a [`MappedSpan`] from a placement; callers add `raw`, `via_macro` and `suggestion`.
    fn assemble(
        &self,
        is_primary: bool,
        label: Option<String>,
        outcome: Result<Placement, UnmappedReason>,
    ) -> MappedSpan {
        let located = self.located(outcome);
        let (precision, marigold, segment) = (located.precision, located.marigold, located.segment);
        MappedSpan {
            is_primary,
            label,
            marigold,
            precision,
            segment,
            raw: None,
            via_macro: None,
            suggestion: None,
        }
    }

    fn located(&self, outcome: Result<Placement, UnmappedReason>) -> Located {
        match outcome {
            Ok(placement) => {
                let slot = &self.slots[placement.slot];
                let segment: &Segment = &self.program.map.segments()[slot.segment];
                Located {
                    marigold: Some(self.source_location(placement.range)),
                    precision: placement.precision,
                    segment: Some(SegmentRef {
                        index: slot.segment,
                        kind: segment.kind,
                        expr_index: segment.expr_index,
                        name: segment.name.clone(),
                    }),
                }
            }
            Err(reason) => Located {
                marigold: None,
                precision: Precision::Unmapped { reason },
                segment: None,
            },
        }
    }

    fn source_location(&self, range: ByteRange) -> MarigoldLocation {
        MarigoldLocation {
            range,
            start: self.source_line_col(range.start),
            end: self.source_line_col(range.end),
        }
    }

    /// Line and column in the Marigold source of an offset on a character boundary.
    fn source_line_col(&self, offset: usize) -> LineCol {
        self.source_index.line_col(offset, ColumnUnit::Chars)
    }

    /// The 0-based line of the generated program that holds `offset`.
    fn rust_line(&self, offset: usize) -> usize {
        self.rust_index.line_col(offset, ColumnUnit::Chars).line - 1
    }

    fn generated_line_number(&self, line: usize) -> usize {
        self.prefix_newlines + 1 + line
    }

    /// 1-based scalar column in the generated *file* of a program offset on `line`.
    fn generated_column(&self, line: usize, offset: usize) -> usize {
        let in_program = self.rust_index.line_col(offset, ColumnUnit::Chars).column - 1;
        let before = if line == 0 {
            self.prefix_tail.chars().count()
        } else {
            0
        };
        before + in_program + 1
    }

    /// The text of program line `line` without its `\n`, and whether the line ends in the
    /// program (a `\n` follows) or may continue into text after the program.
    fn generated_line(&self, line: usize) -> (&str, bool) {
        let rust = self.program.rust.as_str();
        let range = self
            .rust_index
            .line_range(line + 1)
            .expect("callers pass a line of the program");
        (
            &rust[range.start..range.end],
            line + 1 < self.rust_index.line_count(),
        )
    }

    /// Whether rustc's reported line text is the generated file's line `line`.
    fn line_text_matches(&self, line: usize, reported: &str) -> bool {
        let (content, terminated) = self.generated_line(line);
        let content = content.strip_suffix('\r').unwrap_or(content);
        let tail = if line == 0 {
            self.prefix_tail.as_str()
        } else {
            ""
        };
        match reported.strip_prefix(tail) {
            Some(rest) if terminated => rest == content,
            Some(rest) => rest.starts_with(content),
            None => false,
        }
    }

    /// Step 3: do rustc's line, column and text agree with the generated code at `[a, b)`?
    fn is_consistent(&self, s: &RustcSpan, a: usize, b: usize) -> bool {
        let (first, last) = (self.rust_line(a), self.rust_line(b));
        let (column_a, column_b) = (
            self.generated_column(first, a),
            self.generated_column(last, b),
        );
        let reported = [s.line_start, s.line_end, s.column_start, s.column_end];
        if reported.iter().any(|&n| n != 0)
            && reported
                != [
                    self.generated_line_number(first),
                    self.generated_line_number(last),
                    column_a,
                    column_b,
                ]
        {
            return false;
        }
        if s.text.is_empty() {
            return true;
        }
        if s.text.len() != last - first + 1 {
            return false;
        }
        let head = &s.text[0];
        if !self.line_text_matches(first, &head.text) || head.highlight_start != column_a {
            return false;
        }
        if first == last {
            return head.highlight_end == column_b;
        }
        let (content, _) = self.generated_line(first);
        let content = content.strip_suffix('\r').unwrap_or(content);
        let width = content.chars().count()
            + if first == 0 {
                self.prefix_tail.chars().count()
            } else {
                0
            };
        let tail = &s.text[s.text.len() - 1];
        head.highlight_end == width + 1
            && self.line_text_matches(last, &tail.text)
            && tail.highlight_start == 1
            && tail.highlight_end == column_b
    }

    /// The innermost slot around position `pos` (`start <= pos < end`).
    fn innermost_at(&self, pos: usize) -> Option<usize> {
        let mut candidate = search(&self.slots, |s| s.start <= pos).checked_sub(1);
        while let Some(i) = candidate {
            if pos < self.slots[i].end {
                return Some(i);
            }
            #[cfg(test)]
            count(&WALKED);
            candidate = self.slots[i].parent;
        }
        None
    }

    /// The innermost slot that contains all of the non-empty `[a, b)`.
    fn innermost_containing(&self, a: usize, b: usize) -> Option<usize> {
        let mut candidate = search(&self.slots, |s| s.start <= a).checked_sub(1);
        while let Some(i) = candidate {
            if b <= self.slots[i].end {
                return Some(i);
            }
            #[cfg(test)]
            count(&WALKED);
            candidate = self.slots[i].parent;
        }
        None
    }

    fn classify(&self, a: usize, b: usize) -> Result<Placement, UnmappedReason> {
        if a == b {
            return self.classify_point(a);
        }
        let upper = search(&self.slots, |s| s.start < b);
        let lower = search(&self.slots, |s| s.max_end <= a);
        let segments = self.program.map.segments();
        let width = |i: usize| {
            let r = segments[self.slots[i].segment].source;
            r.end.saturating_sub(r.start)
        };
        let mut expression = None;
        let mut widest: Option<usize> = None;
        for i in lower..upper {
            #[cfg(test)]
            count(&SCANNED);
            let slot = &self.slots[i];
            if slot.end <= a {
                continue;
            }
            let segment = &segments[slot.segment];
            match expression {
                None => expression = Some(segment.expr_index),
                Some(e) if e != segment.expr_index => {
                    return Err(UnmappedReason::MultipleExpressions)
                }
                Some(_) => {}
            }
            if widest.is_none_or(|w| width(i) > width(w)) {
                widest = Some(i);
            }
        }
        let Some(widest) = widest else {
            return Err(UnmappedReason::Glue);
        };
        match self.innermost_containing(a, b) {
            Some(container) => self.within(container, a, b),
            None => self.whole(widest, Precision::Expression),
        }
    }

    /// A zero-width position: the segment around it, else the one that ends right before it.
    fn classify_point(&self, a: usize) -> Result<Placement, UnmappedReason> {
        let slot = self
            .innermost_at(a)
            .or_else(|| a.checked_sub(1).and_then(|p| self.innermost_at(p)))
            .ok_or(UnmappedReason::Glue)?;
        self.within(slot, a, a)
    }

    /// `[a, b)` lies inside `slot`: exact when inside its verbatim tail, else the whole segment.
    fn within(&self, slot: usize, a: usize, b: usize) -> Result<Placement, UnmappedReason> {
        let s = &self.slots[slot];
        if !s.source_ok {
            return Err(UnmappedReason::StaleSourceMap);
        }
        let source = self.program.map.segments()[s.segment].source;
        if s.verbatim > 0 && a >= s.end - s.verbatim && b <= s.end {
            return Ok(Placement {
                precision: Precision::Exact,
                slot,
                range: ByteRange {
                    start: source.end - (s.end - a),
                    end: source.end - (s.end - b),
                },
            });
        }
        self.whole(slot, Precision::Segment)
    }

    /// The whole source of `slot`.
    fn whole(&self, slot: usize, precision: Precision) -> Result<Placement, UnmappedReason> {
        let s = &self.slots[slot];
        if !s.source_ok {
            return Err(UnmappedReason::StaleSourceMap);
        }
        Ok(Placement {
            precision,
            slot,
            range: self.program.map.segments()[s.segment].source,
        })
    }

    fn place_location(
        &self,
        line: usize,
        column: usize,
        unit: TraceColumnUnit,
    ) -> Result<Placement, UnmappedReason> {
        let outside = UnmappedReason::OutsideGeneratedRegion;
        let index = line.checked_sub(1 + self.prefix_newlines).ok_or(outside)?;
        let range = self.rust_index.line_range(index + 1).ok_or(outside)?;
        let (start, end) = (range.start, range.end);
        if start >= self.program.rust.len() {
            return Err(outside);
        }
        let whole_line = &self.program.rust[start..end];
        let whole_line_placement = || {
            let visible = whole_line.strip_suffix('\r').unwrap_or(whole_line);
            if visible.is_empty() {
                Err(UnmappedReason::Glue)
            } else {
                self.classify(start, start + visible.len())
            }
        };
        if column == 0 {
            return whole_line_placement();
        }

        let (tail_width, tail_known) = if index == 0 {
            measure(&self.prefix_tail, unit)
        } else {
            (0, true)
        };
        let wanted = column - 1;
        let Some(in_line) = wanted.checked_sub(tail_width) else {
            return if tail_known {
                Err(outside)
            } else {
                self.at_most_segment(whole_line_placement()?)
            };
        };
        let (offset, known) = advance(whole_line, in_line, unit);
        if !(tail_known && known) {
            let line_placement = whole_line_placement()?;
            let guess = offset
                .ok()
                .and_then(|o| self.classify(start + o, start + o).ok());
            return self.at_most_segment(guess.unwrap_or(line_placement));
        }
        let offset = start + offset?;
        self.classify(offset, offset)
    }

    /// Lower an exact placement to the whole segment.
    fn at_most_segment(&self, placement: Placement) -> Result<Placement, UnmappedReason> {
        match placement.precision {
            Precision::Exact => self.whole(placement.slot, Precision::Segment),
            _ => Ok(placement),
        }
    }
}

/// The identifier form of the Marigold grammar (`free_text_identifier` in `marigold.pest`):
/// a letter or `_`, then letters, digits, `_`, `<`, `>` and `-`.
pub(crate) fn is_marigold_identifier(text: &str) -> bool {
    let mut chars = text.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '<' | '>' | '-'))
}

/// Whether applying the suggestion of an exact span to the Marigold source gives the program
/// that applying it to the generated code gives. Exactness says the replaced text is the
/// same in both; this says the edit survives regeneration:
///
/// * rustc must call the suggestion `MachineApplicable` when it says anything about it;
/// * an enum variant is never edited, because Marigold writes its name a second time in the
///   generated name-to-variant conversions;
/// * text the generator wires around the user's expressions (an input, a stream stage, an
///   output) takes only an identifier, the only argument the grammar reads;
/// * whitespace is not edited, and an edit that changes only whitespace is not an edit,
///   because the generator lays code out itself.
fn edit_commutes(span: &MappedSpan, source: &str) -> bool {
    let (Some(suggestion), Some(segment), Some(location)) =
        (&span.suggestion, &span.segment, &span.marigold)
    else {
        return false;
    };
    if suggestion
        .applicability
        .as_deref()
        .is_some_and(|a| a != "MachineApplicable")
    {
        return false;
    }
    if segment.kind == SegmentKind::EnumVariant {
        return false;
    }
    let Some(old) = source.get(location.range.start..location.range.end) else {
        return false;
    };
    if !is_user_authored(segment.kind) && !is_marigold_identifier(&suggestion.replacement) {
        return false;
    }
    let squeeze = |t: &str| t.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    let whitespace_only = !old.is_empty() && old.trim().is_empty();
    !whitespace_only && squeeze(old) != squeeze(&suggestion.replacement)
}

/// Whether `before`, the text in front of an `at` that ends right before a location, is a
/// backtrace frame's second line: the `at` starts its line, and the line above it is a frame
/// header, `<number>:`.
fn is_frame_header_before(before: &str) -> bool {
    let Some(above) = before.trim_end_matches([' ', '\t']).strip_suffix('\n') else {
        return false;
    };
    let header = above.rsplit('\n').next().unwrap_or("").trim_start();
    let digits = header.bytes().take_while(u8::is_ascii_digit).count();
    digits > 0 && header.as_bytes().get(digits) == Some(&b':')
}

/// Numbers the suggestions of the spans of one diagnostic and decides, once for all of them,
/// whether they apply to Marigold. The one place that sets `part`, `parts`, `alternative`,
/// `alternatives` and `applicable_to_marigold`.
///
/// Spans whose reported byte ranges are equal or overlap edit the same place: they are
/// alternatives. The other spans are the parts of one edit.
fn number_suggestions(spans: &mut [MappedSpan], source: &str) {
    let ranges: Vec<(usize, usize)> = spans
        .iter()
        .map(|s| {
            s.raw
                .as_ref()
                .map_or((0, 0), |r| (r.byte_start, r.byte_end))
        })
        .collect();
    let same_place = |i: usize, j: usize| {
        let ((a1, b1), (a2, b2)) = (ranges[i], ranges[j]);
        (a1 == a2 && b1 == b2) || (a1 < b2 && a2 < b1)
    };
    let mut groups: Vec<Vec<usize>> = Vec::new();
    for i in (0..spans.len()).filter(|&i| spans[i].suggestion.is_some()) {
        let mut merged = vec![i];
        let mut kept = Vec::new();
        for group in groups.drain(..) {
            if group.iter().any(|&j| same_place(i, j)) {
                merged.extend(group);
            } else {
                kept.push(group);
            }
        }
        merged.sort_unstable();
        kept.push(merged);
        kept.sort_by_key(|g| g[0]);
        groups = kept;
    }
    let all_direct_and_exact = groups
        .iter()
        .flatten()
        .all(|&i| spans[i].precision == Precision::Exact && spans[i].via_macro.is_none())
        && groups
            .iter()
            .flatten()
            .all(|&i| edit_commutes(&spans[i], source));
    let applicable = all_direct_and_exact && groups.iter().all(|g| g.len() == 1);
    let parts = groups.len();
    for (part, group) in groups.iter().enumerate() {
        for (alternative, &i) in group.iter().enumerate() {
            if let Some(suggestion) = spans[i].suggestion.as_mut() {
                suggestion.part = part;
                suggestion.parts = parts;
                suggestion.alternative = alternative;
                suggestion.alternatives = group.len();
                suggestion.applicable_to_marigold = applicable;
            }
        }
    }
}

fn source_range_fits(source: &str, range: ByteRange) -> bool {
    range.start <= range.end
        && range.end <= source.len()
        && source.is_char_boundary(range.start)
        && source.is_char_boundary(range.end)
}

/// Whether the generated text of a segment of this kind is known to *end* with the Marigold
/// text: the generated `const fn name(..) { .. }` is the user's `fn name(..) { .. }` plus a
/// prefix. This is a property of the kind, never of the bytes: two texts that merely share a
/// last `)` prove nothing, so every other kind needs its whole generated text to equal its
/// Marigold text. A function signature that is generated as `const fn <signature>` is
/// therefore only [`Precision::Segment`] until its kind is added here.
///
/// The `match` has no wildcard on purpose: a new [`SegmentKind`] does not compile until
/// someone has decided what is true of it.
fn suffix_is_verbatim(kind: SegmentKind) -> bool {
    match kind {
        SegmentKind::Function => true,
        SegmentKind::Struct
        | SegmentKind::Enum
        | SegmentKind::StreamVariable
        | SegmentKind::StreamVariableRunner
        | SegmentKind::ReturningStream
        | SegmentKind::NonReturningStream
        | SegmentKind::InputRange
        | SegmentKind::InputReadFile
        | SegmentKind::InputSelectAll
        | SegmentKind::InputVariable
        | SegmentKind::SelectAllBranch
        | SegmentKind::StreamMap
        | SegmentKind::StreamFilter
        | SegmentKind::StreamFilterMap
        | SegmentKind::StreamPermutations
        | SegmentKind::StreamPermutationsWithReplacement
        | SegmentKind::StreamCombinations
        | SegmentKind::StreamKeepFirstN
        | SegmentKind::StreamFold
        | SegmentKind::StreamOk
        | SegmentKind::StreamOkOrPanic
        | SegmentKind::OutputWriteFile
        | SegmentKind::FunctionSignature
        | SegmentKind::FunctionBody
        | SegmentKind::StructField
        | SegmentKind::EnumVariant => false,
    }
}

/// Segment kinds whose text the user wrote: a warning anchored in one of them is the user's
/// own, at any precision (see [`MappedDiagnostic::likely_generated_noise`]): a lint on a
/// `fn`, a `struct` or an `enum` (`non_snake_case`, `dead_code`, a field that is never read)
/// is about code the user can change. Text that is proven identical to the Marigold text
/// counts too, whatever its kind, and does not need to be listed. Stream kinds are the
/// generator's wiring around the user's expressions: lints there (`unused_mut` and the like)
/// are noise.
///
/// The `match` has no wildcard on purpose: a new [`SegmentKind`] (a function body, a struct
/// field, an enum variant, ...) does not compile until someone has decided which side of
/// the line it is on.
fn is_user_authored(kind: SegmentKind) -> bool {
    match kind {
        SegmentKind::Function
        | SegmentKind::Struct
        | SegmentKind::Enum
        | SegmentKind::FunctionSignature
        | SegmentKind::FunctionBody
        | SegmentKind::StructField
        | SegmentKind::EnumVariant => true,
        SegmentKind::StreamVariable
        | SegmentKind::StreamVariableRunner
        | SegmentKind::ReturningStream
        | SegmentKind::NonReturningStream
        | SegmentKind::InputRange
        | SegmentKind::InputReadFile
        | SegmentKind::InputSelectAll
        | SegmentKind::InputVariable
        | SegmentKind::SelectAllBranch
        | SegmentKind::StreamMap
        | SegmentKind::StreamFilter
        | SegmentKind::StreamFilterMap
        | SegmentKind::StreamPermutations
        | SegmentKind::StreamPermutationsWithReplacement
        | SegmentKind::StreamCombinations
        | SegmentKind::StreamKeepFirstN
        | SegmentKind::StreamFold
        | SegmentKind::StreamOk
        | SegmentKind::StreamOkOrPanic
        | SegmentKind::OutputWriteFile => false,
    }
}

/// Length in bytes of the part, at the end of a segment's generated text, that is proven to be
/// byte-identical to the end of its Marigold text, on a character boundary on both sides.
///
/// Two rules prove it. If the whole generated text equals the whole Marigold text, all of it
/// is verbatim (any kind: that is data, not an assumption). If the kind is
/// [`suffix_is_verbatim`], the longest common suffix is.
fn verbatim_len(
    kind: SegmentKind,
    rust: &str,
    generated: ByteRange,
    source: &str,
    range: ByteRange,
) -> usize {
    if !rust.is_char_boundary(generated.end) {
        return 0;
    }
    let g = &rust.as_bytes()[generated.start..generated.end];
    let s = &source.as_bytes()[range.start..range.end];
    if g != s && !suffix_is_verbatim(kind) {
        return 0;
    }
    let mut len = g
        .iter()
        .rev()
        .zip(s.iter().rev())
        .take_while(|(x, y)| x == y)
        .count();
    while len > 0
        && !(rust.is_char_boundary(generated.end - len) && source.is_char_boundary(range.end - len))
    {
        len -= 1;
    }
    len
}

/// Digits at `from` as a number and the offset after them; `None` if there are none or the
/// number does not fit.
fn read_number(text: &str, from: usize) -> Option<(usize, usize)> {
    let digits = text.as_bytes().get(from..)?;
    let len = digits.iter().take_while(|b| b.is_ascii_digit()).count();
    if len == 0 {
        return None;
    }
    text[from..from + len].parse().ok().map(|n| (n, from + len))
}

/// Display width of `c` as `Location::caller().column()` counts it, for the ranges that were
/// sampled against rustc 1.97 (see the tests for the characters measured at the edges of each
/// range); `None` for every other non-ASCII character. The table is not total and not a
/// proof for every code point of a range: the sampling is what makes a range trusted.
fn display_width(c: char) -> Option<usize> {
    let cp = c as u32;
    Some(match cp {
        0x09 => 4,
        0x00..=0x7F => 1,
        0xAD => 0,
        0xA0..=0xFF => 1,
        0x100..=0x24F => 1,
        0x300..=0x36F => 0,
        0x400..=0x482 | 0x48A..=0x4FF => 1,
        0x627 => 1,
        0xE31 => 0,
        0x1100..=0x115F => 2,
        0x200B..=0x200D => 0,
        0x20D0..=0x20F0 => 0,
        0x2190..=0x21FF => 1,
        0x2605 => 1,
        0x3000 => 2,
        0x3041..=0x3096 | 0x30A1..=0x30FA => 2,
        0x3400..=0x4DBF | 0x4E00..=0x9FFF => 2,
        0xAC00..=0xD7A3 => 2,
        0xFE00..=0xFE0F => 0,
        0xFF01..=0xFF60 => 2,
        0x1F600..=0x1F64F => 2,
        0x20000..=0x2FFFD | 0x30000..=0x3FFFD => 2,
        _ => return None,
    })
}

/// The display width of `c`, counting a character outside the verified table as one column.
/// For callers that need a number whatever the trust; the remapper itself tracks the trust.
#[allow(dead_code)]
pub(crate) fn display_width_or_1(c: char) -> usize {
    display_width(c).unwrap_or(1)
}

/// The width of `c` in `unit`, and whether it is trusted.
fn char_width(c: char, unit: TraceColumnUnit) -> (usize, bool) {
    match unit {
        TraceColumnUnit::Chars => (1, true),
        TraceColumnUnit::Bytes => (c.len_utf8(), true),
        TraceColumnUnit::DisplayWidth => match display_width(c) {
            Some(w) => (w, true),
            None => (1, false),
        },
    }
}

/// The width of `text` in `unit`, and whether every character's width is trusted.
fn measure(text: &str, unit: TraceColumnUnit) -> (usize, bool) {
    text.chars().fold((0, true), |(total, known), c| {
        let (w, ok) = char_width(c, unit);
        (total + w, known && ok)
    })
}

/// The byte offset in `line` that is `wanted` units from its start, and whether the widths
/// of the characters before it are trusted. Zero-width characters at the position are skipped.
/// A position inside a character, or past the end of the line, is an invalid span.
fn advance(
    line: &str,
    wanted: usize,
    unit: TraceColumnUnit,
) -> (Result<usize, UnmappedReason>, bool) {
    let mut width = 0;
    let mut offset = 0;
    let mut known = true;
    loop {
        let next = line[offset..].chars().next();
        if width == wanted {
            let mut skipped = offset;
            let mut rest = next;
            while let Some(c) = rest {
                if char_width(c, unit) != (0, true) {
                    break;
                }
                skipped += c.len_utf8();
                rest = line[skipped..].chars().next();
            }
            return (Ok(skipped), known);
        }
        let Some(c) = next else {
            return (Err(UnmappedReason::InvalidSpan), known);
        };
        let (w, ok) = char_width(c, unit);
        width += w;
        known &= ok;
        offset += c.len_utf8();
        if width > wanted {
            return (Err(UnmappedReason::InvalidSpan), known);
        }
    }
}

/// Whether the generated file named `file_name` by rustc is `generated`, comparing paths
/// lexically.
///
/// rustc names files relative to a directory that depends on how it was started; `root` is
/// that directory (for cargo, the workspace root, or the package directory of a standalone
/// package). Relative names, and a relative `generated`, are joined onto `root`. Then `.` and
/// `..` components and repeated separators are resolved *textually*: the file system is never
/// consulted, so symbolic links are not followed and nothing is canonicalized. A caller that
/// needs that can canonicalize both paths itself and pass its own file test to
/// [`Remapper::new`].
///
/// ```
/// use marigold_grammar::remap::lexical_path_matches;
/// use std::path::Path;
///
/// let root = Path::new("/work/project");
/// let generated = Path::new("/work/project/src/gen.rs");
/// assert!(lexical_path_matches(root, "src/gen.rs", generated));
/// assert!(lexical_path_matches(root, "./src/../src/gen.rs", generated));
/// assert!(lexical_path_matches(root, "/work/project/src/gen.rs", generated));
/// assert!(!lexical_path_matches(root, "../src/gen.rs", generated));
/// assert!(!lexical_path_matches(root, "", generated));
/// ```
pub fn lexical_path_matches(root: &Path, file_name: &str, generated: &Path) -> bool {
    if file_name.is_empty() {
        return false;
    }
    normalize(&root.join(file_name)) == normalize(&root.join(generated))
}

/// Resolve `.`, `..` and repeated separators without touching the file system.
fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    let mut depth = 0usize;
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                if depth > 0 {
                    out.pop();
                    depth -= 1;
                } else if !out.has_root() {
                    out.push("..");
                }
            }
            Component::Normal(part) => {
                out.push(part);
                depth += 1;
            }
            other => out.push(other.as_os_str()),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_map::SourceMap;
    use std::path::Path;

    fn all_kinds() -> [(SegmentKind, bool, bool); 27] {
        [
            (SegmentKind::Struct, true, false),
            (SegmentKind::Enum, true, false),
            (SegmentKind::Function, true, true),
            (SegmentKind::StreamVariable, false, false),
            (SegmentKind::StreamVariableRunner, false, false),
            (SegmentKind::ReturningStream, false, false),
            (SegmentKind::NonReturningStream, false, false),
            (SegmentKind::InputRange, false, false),
            (SegmentKind::InputReadFile, false, false),
            (SegmentKind::InputSelectAll, false, false),
            (SegmentKind::InputVariable, false, false),
            (SegmentKind::SelectAllBranch, false, false),
            (SegmentKind::StreamMap, false, false),
            (SegmentKind::StreamFilter, false, false),
            (SegmentKind::StreamFilterMap, false, false),
            (SegmentKind::StreamPermutations, false, false),
            (SegmentKind::StreamPermutationsWithReplacement, false, false),
            (SegmentKind::StreamCombinations, false, false),
            (SegmentKind::StreamKeepFirstN, false, false),
            (SegmentKind::StreamFold, false, false),
            (SegmentKind::StreamOk, false, false),
            (SegmentKind::StreamOkOrPanic, false, false),
            (SegmentKind::OutputWriteFile, false, false),
            (SegmentKind::FunctionSignature, true, false),
            (SegmentKind::FunctionBody, true, false),
            (SegmentKind::StructField, true, false),
            (SegmentKind::EnumVariant, true, false),
        ]
    }

    #[test]
    fn segment_kind_helpers_decide_every_kind() {
        let kinds = all_kinds();
        let distinct: std::collections::HashSet<_> = kinds.iter().map(|k| k.0).collect();
        assert_eq!(distinct.len(), kinds.len());
        let rust = "const fn f(x: i32) -> i32 { x }";
        let source = "fn f(x: i32) -> i32 { x }";
        let generated = ByteRange {
            start: 0,
            end: rust.len(),
        };
        let range = ByteRange {
            start: 0,
            end: source.len(),
        };
        for (kind, user_authored, suffix_verbatim) in kinds {
            assert_eq!(is_user_authored(kind), user_authored, "{kind:?}");
            assert_eq!(suffix_is_verbatim(kind), suffix_verbatim, "{kind:?}");
            let want = if suffix_verbatim { source.len() } else { 0 };
            assert_eq!(
                verbatim_len(kind, rust, generated, source, range),
                want,
                "{kind:?}"
            );
            assert_eq!(
                verbatim_len(kind, source, range, source, range),
                source.len(),
                "{kind:?}"
            );
        }
    }

    #[test]
    fn the_identifier_check_agrees_with_the_grammar() {
        for id in [
            "f",
            "_f",
            "mk",
            "Ok",
            "a1",
            "a_b",
            "a-b",
            "a<b>",
            "string_10",
            "1a",
            "",
            "-a",
            "<a",
            "a b",
            "a::b",
            "&f",
            "f()",
            "crate::mk",
            "é",
            "a.b",
        ] {
            let parses = crate::marigold_generate(&format!("range(0, 3).map({id}).return")).is_ok();
            assert_eq!(is_marigold_identifier(id), parses, "{id:?}");
        }
    }

    fn counters() -> (usize, usize) {
        (SCANNED.with(|c| c.get()), WALKED.with(|c| c.get()))
    }

    fn reset() {
        SCANNED.with(|c| c.set(0));
        WALKED.with(|c| c.set(0));
    }

    fn span(start: usize, end: usize) -> RustcSpan {
        RustcSpan {
            file_name: "gen.rs".to_string(),
            byte_start: start,
            byte_end: end,
            ..Default::default()
        }
    }

    fn range(start: usize, end: usize) -> ByteRange {
        ByteRange { start, end }
    }

    /// `n` single-line functions, one expression each, separated by glue.
    fn flat(n: usize) -> (String, GeneratedProgram) {
        let mut src = String::new();
        let mut rust = String::from("async {\n");
        let mut segments = Vec::new();
        for i in 0..n {
            let f = format!("fn f{i}() -> i32 {{ {i} }}");
            let (s0, g0) = (src.len(), rust.len());
            src.push_str(&f);
            rust.push_str("const ");
            rust.push_str(&f);
            segments.push(Segment::new(
                range(g0, rust.len()),
                range(s0, src.len()),
                SegmentKind::Function,
                i,
                None,
            ));
            src.push('\n');
            rust.push_str("\n\n");
        }
        rust.push_str("}\n");
        (src, GeneratedProgram::new(rust, SourceMap::new(segments)))
    }

    #[test]
    fn display_width_table_is_what_rustc_reported_and_nothing_else() {
        let verified: &[(char, usize)] = &[
            ('a', 1),
            ('\u{e9}', 1),
            ('\u{2192}', 1),
            ('\u{65e5}', 2),
            ('\u{1F600}', 2),
            ('\u{0301}', 0),
            ('\u{200b}', 0),
            ('\t', 4),
            ('\u{ff01}', 2),
            ('\u{ac00}', 2),
            ('\u{0627}', 1),
            ('\u{3042}', 2),
            ('\u{00a0}', 1),
            ('\u{200d}', 0),
            ('\u{fe0f}', 0),
            ('\u{2605}', 1),
            ('\u{20000}', 2),
            ('\u{00ad}', 0),
            ('\u{0007}', 1),
            ('\u{0e31}', 0),
            ('\u{0436}', 1),
            ('\u{1100}', 2),
            ('\u{3000}', 2),
            ('\u{20dd}', 0),
        ];
        for &(c, w) in verified {
            assert_eq!(display_width(c), Some(w), "U+{:04X}", c as u32);
        }
        for c in [
            '\u{0f00}',
            '\u{0080}',
            '\u{1f321}',
            '\u{e000}',
            '\u{10ffff}',
        ] {
            assert_eq!(display_width(c), None, "U+{:04X}", c as u32);
        }
        assert_eq!(char_width('日', TraceColumnUnit::Chars), (1, true));
        assert_eq!(char_width('日', TraceColumnUnit::Bytes), (3, true));
        assert_eq!(
            char_width('\u{0f00}', TraceColumnUnit::DisplayWidth),
            (1, false)
        );
    }

    #[test]
    fn advance_converts_columns_and_rejects_positions_inside_characters() {
        let line = "a日\u{0301}b";
        assert_eq!(advance(line, 0, TraceColumnUnit::Chars), (Ok(0), true));
        assert_eq!(advance(line, 2, TraceColumnUnit::Chars), (Ok(4), true));
        assert_eq!(advance(line, 4, TraceColumnUnit::Chars), (Ok(7), true));
        assert_eq!(
            advance(line, 5, TraceColumnUnit::Chars),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        assert_eq!(advance(line, 4, TraceColumnUnit::Bytes), (Ok(4), true));
        assert_eq!(
            advance(line, 2, TraceColumnUnit::Bytes),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        assert_eq!(
            advance(line, 3, TraceColumnUnit::DisplayWidth),
            (Ok(6), true)
        );
        assert_eq!(
            advance(line, 4, TraceColumnUnit::DisplayWidth),
            (Ok(7), true)
        );
        assert_eq!(
            advance(line, 2, TraceColumnUnit::DisplayWidth),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        assert!(!advance("\u{0f00}x", 1, TraceColumnUnit::DisplayWidth).1);
        assert_eq!(advance("ab", 2, TraceColumnUnit::Chars), (Ok(2), true));
        assert_eq!(
            advance("ab", 3, TraceColumnUnit::Chars),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        assert_eq!(
            advance("ab", usize::MAX, TraceColumnUnit::Bytes),
            (Err(UnmappedReason::InvalidSpan), true)
        );
    }

    #[test]
    fn read_number_rejects_what_does_not_fit_or_is_not_there() {
        assert_eq!(read_number("x:12:3", 2), Some((12, 4)));
        assert_eq!(read_number("x:", 2), None);
        assert_eq!(read_number("x:a", 2), None);
        assert_eq!(read_number("1", 5), None);
        assert_eq!(read_number("99999999999999999999999", 0), None);
    }

    #[test]
    fn verbatim_region_is_the_common_suffix_on_character_boundaries() {
        let rust = "pre const fn f() { 1 }";
        let source = "fn f() { 1 }";
        let g = range(4, rust.len());
        assert_eq!(
            verbatim_len(
                SegmentKind::Function,
                rust,
                g,
                source,
                range(0, source.len())
            ),
            source.len()
        );
        assert_eq!(
            verbatim_len(
                SegmentKind::Function,
                "abc",
                range(0, 3),
                "xyz",
                range(0, 3)
            ),
            0
        );
        assert_eq!(
            verbatim_len(
                SegmentKind::Function,
                "\u{e9}",
                range(0, 2),
                "\u{a9}",
                range(0, 2)
            ),
            0
        );
        assert_eq!(
            verbatim_len(
                SegmentKind::Function,
                "a\u{e9}",
                range(0, 3),
                "\u{a9}",
                range(0, 2)
            ),
            0
        );
    }

    #[test]
    fn lexical_normalisation_resolves_dots_in_order_and_never_climbs_above_the_root() {
        let n = |p: &str| normalize(Path::new(p));
        assert_eq!(n("/a/./b/../c"), Path::new("/a/c"));
        assert_eq!(n("/a/b/../../c"), Path::new("/c"));
        assert_eq!(n("/../a"), Path::new("/a"));
        assert_eq!(n("a/../../b"), Path::new("../b"));
        assert_eq!(n("a//b"), Path::new("a/b"));
        assert_eq!(n("/a/./../b"), n("/a/.././b"));
        assert_eq!(n("/a/./../b"), Path::new("/b"));
    }

    #[test]
    fn lookups_do_the_same_work_whatever_the_number_of_segments() {
        for n in [10, 2_000, 40_000] {
            let (src, program) = flat(n);
            let is_generated = |f: &str| f == "gen.rs";
            let remapper = Remapper::new(&program, &src, &is_generated);
            let segments = program.map.segments();
            reset();
            let lookups = 500;
            for k in 0..lookups {
                let segment = &segments[(k * 7919) % n];
                let at = segment.generated.end - 3;
                for (a, b) in [(at, at + 1), (at, at), (at, segment.generated.end + 1)] {
                    let mapped = remapper.map_span(&span(a, b));
                    assert!(mapped.marigold.is_some(), "n={n} {a}..{b}: {mapped:?}");
                }
            }
            let (scanned, walked) = counters();
            assert_eq!((scanned, walked), (2 * lookups, lookups), "n={n}");
        }
    }

    #[test]
    fn a_span_over_the_whole_program_stops_at_the_second_expression() {
        for n in [3, 5_000] {
            let (src, program) = flat(n);
            let is_generated = |f: &str| f == "gen.rs";
            let remapper = Remapper::new(&program, &src, &is_generated);
            reset();
            let mapped = remapper.map_span(&span(0, program.rust.len()));
            assert_eq!(
                mapped.precision,
                Precision::Unmapped {
                    reason: UnmappedReason::MultipleExpressions
                }
            );
            assert_eq!(counters().0, 2, "n={n}");
        }
    }

    #[test]
    fn nesting_costs_the_depth_and_not_the_number_of_segments() {
        let depth = 40;
        let mut segments = Vec::new();
        let mut rust = String::new();
        let mut src = String::new();
        for i in 0..3_000 {
            let (g0, s0) = (rust.len(), src.len());
            rust.push_str("xxxx\n");
            src.push_str("yyyy\n");
            segments.push(Segment::new(
                range(g0, g0 + 4),
                range(s0, s0 + 4),
                SegmentKind::StreamVariable,
                i,
                None,
            ));
        }
        let (g0, s0) = (rust.len(), src.len());
        rust.push_str(&"z".repeat(2 * depth));
        src.push_str(&"w".repeat(2 * depth));
        for d in 0..depth {
            segments.push(Segment::new(
                range(g0 + d, g0 + 2 * depth - d),
                range(s0 + d, s0 + 2 * depth - d),
                SegmentKind::StreamVariable,
                3_000,
                None,
            ));
        }
        let program = GeneratedProgram::new(rust, SourceMap::new(segments));
        let is_generated = |f: &str| f == "gen.rs";
        let remapper = Remapper::new(&program, &src, &is_generated);

        reset();
        let middle = g0 + depth;
        let mapped = remapper.map_span(&span(middle, middle));
        let location = mapped.marigold.unwrap();
        assert_eq!(
            (location.range.start, location.range.end),
            (s0 + depth - 1, s0 + depth + 1)
        );
        assert_eq!(counters().1, 0);

        reset();
        let (a, b) = (g0 + depth - 1, g0 + depth + 5);
        let mapped = remapper.map_span(&span(a, b));
        assert_eq!(
            mapped.marigold.unwrap().range,
            range(s0 + depth - 5, s0 + depth + 5)
        );
        assert_eq!(counters().1, 4);

        reset();
        let last = g0 + 2 * depth - 1;
        let mapped = remapper.map_span(&span(last, last));
        let location = mapped.marigold.unwrap();
        assert_eq!(location.range, range(s0, s0 + 2 * depth));
        assert_eq!(counters().1, depth - 1);
    }

    fn clear_searches() {
        SEARCHES.with(|log| log.borrow_mut().clear());
    }

    fn searches() -> Vec<(usize, usize)> {
        SEARCHES.with(|log| log.borrow().clone())
    }

    /// The most predicate calls a binary search over `len` items needs.
    fn most_probes(len: usize) -> usize {
        (usize::BITS - len.leading_zeros()) as usize + 1
    }

    #[test]
    fn every_search_is_a_binary_search_and_a_lookup_makes_the_same_searches_at_any_size() {
        for n in [10, 2_000, 40_000] {
            let (src, program) = flat(n);
            let is_generated = |f: &str| f == "gen.rs";
            let remapper = Remapper::new(&program, &src, &is_generated);
            let segment = &program.map.segments()[n / 2];
            let at = segment.generated.end - 3;
            for ((a, b), expected) in [
                ((at, at + 1), 3),
                ((at, at), 1),
                ((at, segment.generated.end + 1), 3),
            ] {
                clear_searches();
                let mapped = remapper.map_span(&span(a, b));
                assert!(mapped.marigold.is_some(), "n={n} {a}..{b}: {mapped:?}");
                let log = searches();
                assert_eq!(log.len(), expected, "n={n} {a}..{b}: {log:?}");
                for &(len, probes) in &log {
                    assert!(
                        probes <= most_probes(len),
                        "n={n} {a}..{b}: {probes} probes over {len} items"
                    );
                    assert!(
                        probes >= 1,
                        "n={n} {a}..{b}: a search over {len} items probed nothing"
                    );
                }
                assert!(log.iter().all(|&(len, _)| len >= n), "n={n}: {log:?}");
            }
        }
    }

    #[test]
    fn display_width_never_contradicts_rustc_and_knows_where_each_range_ends() {
        #[rustfmt::skip]
        const MEASURED: &[(u32, usize)] = &[
            (0x41, 1), (0x7f, 1), (0x9f, 1), (0xa0, 1), (0xac, 1), (0xad, 0), (0xff, 1), (0x100, 1),
            (0x24f, 1), (0x250, 1), (0x2ff, 1), (0x300, 0), (0x36f, 0), (0x370, 1), (0x482, 1),
            (0x483, 0), (0x489, 0), (0x48a, 1), (0x4ff, 1), (0x500, 1), (0x626, 1), (0x627, 1),
            (0x628, 1), (0xe30, 1), (0xe31, 0), (0xe32, 1), (0x1100, 2), (0x115f, 2), (0x1160, 0),
            (0x200a, 1), (0x200b, 0), (0x200d, 0), (0x200e, 0), (0x20cf, 1), (0x20d0, 0), (0x20dd, 0),
            (0x20f0, 0), (0x20f1, 1), (0x20f5, 1), (0x20ff, 1), (0x2100, 1), (0x218f, 1), (0x2190, 1),
            (0x21ff, 1), (0x2200, 1), (0x2605, 1), (0x2fff, 2), (0x3000, 2), (0x3001, 2), (0x3040, 1),
            (0x3041, 2), (0x3096, 2), (0x3097, 1), (0x30a0, 2), (0x30a1, 2), (0x30fa, 2), (0x30fb, 2),
            (0x3399, 2), (0x33ff, 2), (0x3400, 2), (0x4dbf, 2), (0x4dc0, 2), (0x4dff, 2), (0x4e00, 2),
            (0x9fff, 2), (0xa000, 2), (0xabff, 1), (0xac00, 2), (0xd7a3, 2), (0xd7a4, 1), (0xfdff, 1),
            (0xfe00, 0), (0xfe0f, 0), (0xfe10, 2), (0xfeff, 0), (0xff00, 1), (0xff01, 2), (0xff60, 2),
            (0xff61, 1), (0xffdc, 1), (0x1f321, 1), (0x1f5ff, 2), (0x1f600, 2), (0x1f64f, 2),
            (0x1f650, 1), (0x1f680, 2), (0x1ffff, 1), (0x20000, 2), (0x2fffd, 2), (0x2fffe, 1),
            (0x30000, 2), (0x3fffd, 2),
        ];
        const ENDS: &[u32] = &[
            0x41, 0x7f, 0xa0, 0xad, 0xff, 0x100, 0x24f, 0x300, 0x36f, 0x482, 0x48a, 0x4ff, 0x627,
            0xe31, 0x1100, 0x115f, 0x200b, 0x200d, 0x20d0, 0x20dd, 0x20f0, 0x2190, 0x21ff, 0x2605,
            0x3000, 0x3041, 0x3096, 0x30a1, 0x30fa, 0x3400, 0x4dbf, 0x4e00, 0x9fff, 0xac00, 0xd7a3,
            0xfe00, 0xfe0f, 0xff01, 0xff60, 0x1f600, 0x1f64f, 0x20000, 0x2fffd, 0x30000, 0x3fffd,
        ];
        for &(cp, width) in MEASURED {
            let c = char::from_u32(cp).unwrap();
            match display_width(c) {
                Some(w) => assert_eq!(w, width, "U+{cp:04X}"),
                None => assert!(!ENDS.contains(&cp), "U+{cp:04X} is an edge of a range"),
            }
            assert_eq!(display_width_or_1(c), display_width(c).unwrap_or(1));
        }
        for &cp in ENDS {
            assert!(
                display_width(char::from_u32(cp).unwrap()).is_some(),
                "U+{cp:04X}"
            );
            assert!(
                MEASURED.iter().any(|&(m, _)| m == cp),
                "U+{cp:04X} unmeasured"
            );
        }
        for cp in [
            0x250, 0x370, 0x483, 0x1160, 0x20f1, 0x20f5, 0x20ff, 0x2100, 0x3001, 0xa000, 0x1f650,
        ] {
            assert_eq!(
                display_width(char::from_u32(cp).unwrap()),
                None,
                "U+{cp:04X}"
            );
        }
    }

    #[test]
    fn a_segment_is_verbatim_when_its_bytes_are_the_source_or_its_kind_ends_with_it() {
        let rust = "pre const fn f() { 1 }";
        let source = "fn f() { 1 }";
        let tail = range(4, rust.len());
        assert_eq!(
            verbatim_len(
                SegmentKind::Function,
                rust,
                tail,
                source,
                range(0, source.len())
            ),
            source.len()
        );
        for kind in [
            SegmentKind::StreamVariable,
            SegmentKind::Struct,
            SegmentKind::Enum,
        ] {
            assert_eq!(
                verbatim_len(kind, rust, tail, source, range(0, source.len())),
                0,
                "{kind:?}"
            );
            assert_eq!(
                verbatim_len(
                    kind,
                    source,
                    range(0, source.len()),
                    source,
                    range(0, source.len())
                ),
                source.len(),
                "{kind:?}"
            );
        }
        assert_eq!(
            verbatim_len(
                SegmentKind::Struct,
                "a\u{e9}",
                range(1, 3),
                "\u{e9}",
                range(0, 2)
            ),
            2
        );
        assert!(suffix_is_verbatim(SegmentKind::Function));
        assert!(!suffix_is_verbatim(SegmentKind::Struct));
        assert!(is_user_authored(SegmentKind::Function));
        assert!(!is_user_authored(SegmentKind::StreamVariable));
    }

    #[test]
    fn the_kind_decisions_have_no_wildcard_arm() {
        let source = include_str!("remap.rs");
        for name in ["fn suffix_is_verbatim(kind", "fn is_user_authored(kind"] {
            let start = source.find(name).unwrap();
            let end = start + source[start..].find("\n}\n").unwrap();
            let body = &source[start..end];
            assert!(body.contains("match kind {"), "{name}");
            assert!(
                !body.contains("_ =>") && !body.contains("_ |") && !body.contains("| _"),
                "{name} has a wildcard arm"
            );
        }
    }

    #[test]
    fn two_hundred_thousand_diagnostics_make_the_same_searches_each() {
        let (src, program) = flat(2_000);
        let is_generated = |f: &str| f == "gen.rs";
        let remapper = Remapper::new(&program, &src, &is_generated);
        let segment = &program.map.segments()[1_000];
        let at = segment.generated.end - 3;
        let diagnostic: RustcDiagnostic = RustcDiagnostic {
            spans: vec![span(at, at + 1), span(at, at)],
            ..Default::default()
        };
        clear_searches();
        let first = remapper.remap(&diagnostic);
        assert!(first.primary.as_ref().unwrap().marigold.is_some());
        let per_diagnostic = searches().len();
        assert_eq!(per_diagnostic, 3 + 1);
        clear_searches();
        for _ in 0..200_000 {
            let mapped = remapper.remap(&diagnostic);
            assert_eq!(mapped.primary.as_ref().unwrap().precision, Precision::Exact);
        }
        let log = searches();
        assert_eq!(log.len(), 200_000 * per_diagnostic);
        assert!(log.iter().all(|&(len, probes)| probes <= most_probes(len)));
    }
}
