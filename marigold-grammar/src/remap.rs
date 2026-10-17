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
//!   Marigold text. Today that is the body of a Marigold `fn`, which is copied through
//!   (the generated `const ` before `fn` is not part of it). The range is the exact
//!   same text in the Marigold source, and a zero-width insertion point stays zero-width.
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
//! Compile-time and run-time positions use different column units; see [`ColumnUnit`].
//!
//! # What is not translated
//!
//! * `rendered`, the `raw` span and the *message text* talk about generated code (names
//!   like `returning_stream_0`, positions like `src/gen.rs:35:72`); do not show them as
//!   if they were Marigold.
//! * Suggestions (`suggested_replacement`) edit generated text. They are carried
//!   along and flagged with [`Suggestion::applicable_to_marigold`], which is true only
//!   for a replacement of an [`Precision::Exact`] span.
//! * Warnings from glue the user never wrote (rustc reports 1-3 `unused_mut` warnings
//!   for every program) are recognisable with [`MappedDiagnostic::likely_generated_noise`].
//!
//! ```
//! use marigold_grammar::marigold_generate;
//! use marigold_grammar::remap::{Precision, Remapper};
//! use marigold_grammar::rustc_json::parse_message_line;
//!
//! let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
//! let program = marigold_generate(src).unwrap();
//!
//! // Pretend rustc rejected the `x` in the function body of the file the code was written to.
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
use crate::rustc_json::{Level, RustcDiagnostic, RustcSpan};
use crate::source_map::{GeneratedProgram, Segment, SegmentKind};
use serde::Serialize;
use std::path::{Component, Path, PathBuf};

/// Expansion hops followed from a span in a library file to the call site in the
/// generated file. A longer chain is [`UnmappedReason::ForeignFile`].
const MAX_EXPANSION_HOPS: usize = 64;

/// A 1-based line and column in the Marigold source.
///
/// The column counts Unicode scalar values (`\r` and tabs count as one), the same
/// convention as `marigold check`.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::Remapper;
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |_: &str| true;
/// let remapper = Remapper::new(&program, src, &is_generated);
///
/// let at = program.rust.find("{ x }").unwrap() + 2;
/// let span: RustcSpan = serde_json::from_str(&format!(r#"{{"byte_start":{at},"byte_end":{}}}"#, at + 1)).unwrap();
/// let start = remapper.map_span(&span).marigold.unwrap().start;
/// assert_eq!((start.line, start.column), (1, 23));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[non_exhaustive]
pub struct LineCol {
    /// 1-based line; only `\n` ends a line.
    pub line: usize,
    /// 1-based column in Unicode scalar values.
    pub column: usize,
}

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
/// // Line 1 of the generated file is the preamble: glue, nothing to map to.
/// assert!(remapper.locate("gen.rs", 1, None, marigold_grammar::remap::ColumnUnit::Utf8Bytes).marigold.is_none());
/// let at = program.rust.find("{ x }").unwrap() + 2;
/// let line = program.rust[..at].matches('\n').count() + 1;
/// let loc = remapper.locate("gen.rs", line, None, marigold_grammar::remap::ColumnUnit::Utf8Bytes).marigold.unwrap();
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
/// use marigold_grammar::remap::{ColumnUnit, Remapper};
/// use marigold_grammar::source_map::SegmentKind;
///
/// let src = "fn f(x: i32) -> i32 { x }";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |_: &str| true;
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let line = program.rust[..program.rust.find("const fn").unwrap()].matches('\n').count() + 1;
/// let segment = remapper.locate("x.rs", line, None, ColumnUnit::ScalarValues).segment.unwrap();
/// assert_eq!((segment.kind, segment.expr_index, segment.name.as_deref()), (SegmentKind::Function, 0, Some("f")));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SegmentRef {
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
    /// The diagnostic has no span. The library reports this as `primary: None`; the
    /// variant exists for consumers that need a value for "no span".
    NoSpan,
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
/// // A removal suggestion has an empty replacement; it is kept.
/// let span: RustcSpan = serde_json::from_str(r#"{"byte_start":0,"byte_end":0,"suggested_replacement":"","suggestion_applicability":"MachineApplicable"}"#).unwrap();
/// let suggestion = remapper.map_span(&span).suggestion.unwrap();
/// assert_eq!(suggestion.replacement, "");
/// assert_eq!(suggestion.applicability.as_deref(), Some("MachineApplicable"));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Suggestion {
    /// The replacement text.
    pub replacement: String,
    /// rustc's applicability (`MachineApplicable`, `MaybeIncorrect`, ...).
    pub applicability: Option<String>,
    /// Whether the replacement can be applied to the Marigold source at
    /// [`MappedSpan::marigold`]: only when that span is [`Precision::Exact`] and rustc
    /// pointed at it directly (not through a macro call site). A suggestion that edits
    /// generated text, like "a tuple variant with a similar name exists: `Ok`", is
    /// never applicable.
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
    /// // The primary span is in another file; the note's span is in the generated code.
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

    /// Whether this is probably a warning about code the user did not write.
    ///
    /// True for a warning whose [`anchor`](Self::anchor) is missing or not
    /// [`Precision::Exact`]: a lint that is not provably about the user's own text (rustc
    /// emits one to three `unused_mut` warnings for every program, from generated glue).
    /// Errors are never noise. This is a heuristic for filtering; it does not look at the
    /// lint's name.
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
            && self
                .anchor()
                .is_none_or(|s| s.precision != Precision::Exact)
    }
}

/// The unit of a column number. Three of them are in use and they must not be mixed.
///
/// | where the column comes from | unit |
/// |---|---|
/// | rustc's JSON diagnostics (`column_start`, ...) | [`ScalarValues`](Self::ScalarValues) |
/// | the panic message `panicked at src/gen.rs:4:43`, `Location::caller()`, `column!()` | [`DisplayWidth`](Self::DisplayWidth) |
/// | a backtrace frame `at ./src/gen.rs:4:48` (DWARF) | [`Utf8Bytes`](Self::Utf8Bytes) |
///
/// On a line with text outside ASCII they disagree: for a `panic!` after `"héllo→日本"`
/// the scalar column is 41, the display column 43 and the byte column 48. Rows were
/// measured with rustc 1.97; see [`Remapper::locate`] for how unknown characters are treated.
///
/// ```
/// use marigold_grammar::remap::ColumnUnit;
///
/// assert_eq!(serde_json::to_string(&ColumnUnit::Utf8Bytes).unwrap(), "\"utf8_bytes\"");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum ColumnUnit {
    /// Unicode scalar values (`char`s); a tab is one.
    ScalarValues,
    /// Terminal display width: most characters 1, East Asian wide characters and emoji 2,
    /// combining marks and zero-width characters 0, a tab 4.
    DisplayWidth,
    /// UTF-8 bytes.
    Utf8Bytes,
}

/// A source location found in text such as a panic message or a backtrace.
///
/// ```
/// use marigold_grammar::marigold_generate;
/// use marigold_grammar::remap::{ColumnUnit, Remapper};
///
/// let src = "range(0, 1).return";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file == "src/gen.rs";
/// let remapper = Remapper::new(&program, src, &is_generated);
/// let text = "thread 'main' panicked at src/gen.rs:1:1:\nboom";
/// let hit = &remapper.remap_trace(text)[0];
/// assert_eq!(&text[hit.token.start..hit.token.end], "src/gen.rs:1:1");
/// assert_eq!((hit.line, hit.column, hit.unit), (1, Some(1), ColumnUnit::DisplayWidth));
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
    pub unit: ColumnUnit,
    /// The result of [`Remapper::locate`] for this location.
    pub mapped: MappedSpan,
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
    /// to the end of the Marigold range. Zero unless this is a `Function`.
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
}

#[cfg(test)]
fn count(counter: &'static std::thread::LocalKey<std::cell::Cell<usize>>) {
    counter.with(|c| c.set(c.get() + 1));
}

/// Maps rustc diagnostics and run-time locations in generated Rust to the Marigold source.
///
/// Build it once per generated program ([`Remapper::new`] indexes the generated text, the
/// Marigold source and the source map in `O(n log n)`); every diagnostic is then mapped in
/// `O(log n + k)` for `k` segments the span touches (plus the length of the lines it
/// sits on). All methods take `&self`. The remapper borrows the program, the source and
/// the file test, and is not `Send` or `Sync` because the file test is not required to be.
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
/// use marigold_grammar::remap::{ColumnUnit, Precision, Remapper};
///
/// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
/// let program = marigold_generate(src).unwrap();
/// let is_generated = |file: &str| file.ends_with("gen.rs");
/// let remapper = Remapper::new(&program, src, &is_generated);
///
/// // The `x` in the function body, found from a run-time location (line, scalar column).
/// let at = program.rust.find("{ x }").unwrap() + 2;
/// let line = program.rust[..at].matches('\n').count() + 1;
/// let column = at - program.rust[..at].rfind('\n').map_or(0, |i| i + 1) + 1;
/// let mapped = remapper.locate("./src/gen.rs", line, Some(column), ColumnUnit::ScalarValues);
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
    /// Byte offsets where the lines of the generated program start.
    rust_lines: Vec<usize>,
    /// Byte offsets where the lines of the Marigold source start.
    source_lines: Vec<usize>,
}

impl<'a> Remapper<'a> {
    /// Index a generated program and the Marigold source it was generated from.
    ///
    /// `source` must be the exact string passed to [`crate::marigold_generate`], and
    /// `program.rust` exactly the text of the generated file (after
    /// [`with_prefix`](Self::with_prefix)): the checks described in the
    /// [module documentation](self) detect a mismatch but cannot repair it.
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
        for (index, segment) in program.map.segments().iter().enumerate() {
            let g = segment.generated;
            // A segment that does not fit the program cannot be placed.
            if g.start >= g.end || g.end > rust.len() {
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
            let verbatim = if source_ok && segment.kind == SegmentKind::Function {
                verbatim_len(rust, g, source, segment.source)
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
            rust_lines: line_starts(rust),
            source_lines: line_starts(source),
        }
    }

    /// Declare the bytes of the generated file that come before `program.rust`.
    ///
    /// A wrapper like `#[tokio::main]\nasync fn main() {\n    ` followed by the program
    /// shifts every byte offset, every line number and, on the program's first line,
    /// every column. After this call rustc's numbers for the *file* are understood;
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
    /// // Offsets are those of the whole file.
    /// let at = prefix.len() + program.rust.find("{ x }").unwrap() + 2;
    /// let span: RustcSpan = serde_json::from_str(&format!(
    ///     r#"{{"file_name":"src/main.rs","byte_start":{at},"byte_end":{}}}"#, at + 1,
    /// )).unwrap();
    /// assert_eq!(remapper.map_span(&span).precision, Precision::Exact);
    ///
    /// // A span inside the prefix is outside the generated region.
    /// let wrapper: RustcSpan = serde_json::from_str(r#"{"file_name":"src/main.rs","byte_start":0,"byte_end":2}"#).unwrap();
    /// assert!(matches!(remapper.map_span(&wrapper).precision, Precision::Unmapped { .. }));
    /// ```
    #[must_use]
    pub fn with_prefix(mut self, prefix: &str) -> Self {
        self.prefix_len = prefix.len();
        self.prefix_newlines = prefix.bytes().filter(|&b| b == b'\n').count();
        self.prefix_tail = prefix
            .rsplit_once('\n')
            .map_or(prefix, |(_, tail)| tail)
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
        MappedDiagnostic {
            level: d.level.clone(),
            code: d
                .code
                .as_ref()
                .map(|c| c.code.clone())
                .filter(|c| !c.is_empty()),
            message: d.message.clone(),
            primary: primary_index.map(|i| self.map_span(&d.spans[i])),
            secondary: d
                .spans
                .iter()
                .enumerate()
                .filter(|&(i, _)| Some(i) != primary_index)
                .map(|(_, s)| self.map_span(s))
                .collect(),
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
                applicable_to_marigold: mapped.precision == Precision::Exact && via_macro.is_none(),
            });
        mapped.via_macro = via_macro;
        mapped
    }

    /// Map a run-time location in the generated file: a panic message, a backtrace
    /// frame, `Location::caller()`, tracing metadata.
    ///
    /// `file` goes through the file test as given, `line` is the 1-based line in the
    /// *file* (lines of a declared prefix are subtracted; lines outside the program are
    /// [`UnmappedReason::OutsideGeneratedRegion`]). With `column` `None` or `Some(0)` the
    /// whole line's text is classified as a span, so a line wholly inside a Marigold `fn` is
    /// [`Precision::Exact`]. With a column, the position is converted to a byte offset in the
    /// unit given and classified as a zero-width position: a column inside a character, or
    /// more than one past the end of the line, is [`UnmappedReason::InvalidSpan`]. On the
    /// program's first line, columns include the declared prefix's last partial line.
    ///
    /// Display widths come from a table verified against rustc 1.97 (ASCII, Latin, Cyrillic,
    /// arrows, CJK, Hangul, fullwidth forms, emoji, combining marks, zero-width characters).
    /// If a character before the column is outside that table, the column is not trusted and
    /// the result is the classification of the whole line, at best [`Precision::Segment`].
    /// Within a run of zero-width characters the position after them is used.
    ///
    /// The result has `raw: None`.
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{ColumnUnit, Precision, Remapper};
    ///
    /// let src = "fn f(x: i32) -> i32 { x }\nrange(0, 1).map(f).write_file(\"o\", csv)";
    /// let program = marigold_generate(src).unwrap();
    /// let is_generated = |file: &str| file == "src/gen.rs";
    /// let remapper = Remapper::new(&program, src, &is_generated);
    ///
    /// let at = program.rust.find("{ x }").unwrap() + 2;
    /// let line_start = program.rust[..at].rfind('\n').map_or(0, |i| i + 1);
    /// let line = program.rust[..at].matches('\n').count() + 1;
    /// let located = remapper.locate("src/gen.rs", line, Some(at - line_start + 1), ColumnUnit::Utf8Bytes);
    /// assert_eq!(located.precision, Precision::Exact);
    /// let range = located.marigold.unwrap().range;
    /// assert_eq!(range.start, 22); // the position of `x` in the Marigold function
    /// assert_eq!(range.start, range.end);
    ///
    /// assert!(remapper.locate("src/other.rs", line, None, ColumnUnit::Utf8Bytes).marigold.is_none());
    /// ```
    pub fn locate(
        &self,
        file: &str,
        line: usize,
        column: Option<usize>,
        unit: ColumnUnit,
    ) -> MappedSpan {
        let outcome = if (self.is_generated)(file) {
            self.place_location(line, column.unwrap_or(0), unit)
        } else {
            Err(UnmappedReason::ForeignFile)
        };
        self.assemble(true, None, outcome)
    }

    /// Find every `file:line` and `file:line:column` in `text` whose file is the
    /// generated file, and [`locate`](Self::locate) each one.
    ///
    /// For panic output and backtraces. Hits come in text order and do not overlap.
    /// A file is the text before the location, back to the nearest whitespace, quote,
    /// `(`, `[`, `<` or backtick; a path that contains a space is therefore not
    /// recognised, rather than guessed. The column unit follows the context: after
    /// `panicked at` it is [`ColumnUnit::DisplayWidth`] (the panic message prints
    /// `Location`), after a backtrace frame's `at` it is [`ColumnUnit::Utf8Bytes`], and
    /// anywhere else [`ColumnUnit::DisplayWidth`].
    ///
    /// ```
    /// use marigold_grammar::marigold_generate;
    /// use marigold_grammar::remap::{ColumnUnit, Remapper};
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
    ///     ("src/gen.rs", 1, Some(1), ColumnUnit::DisplayWidth),
    ///     ("./src/gen.rs", 2, Some(3), ColumnUnit::Utf8Bytes),
    /// ]);
    /// ```
    pub fn remap_trace(&self, text: &str) -> Vec<TracedLocation> {
        let bytes = text.as_bytes();
        let mut hits = Vec::new();
        // Start of the current path-like token, and end of the last hit.
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
                let after_at = before
                    .strip_suffix("at")
                    .is_some_and(|rest| rest.is_empty() || rest.ends_with(char::is_whitespace));
                let unit = if before.ends_with("panicked at") || !after_at {
                    ColumnUnit::DisplayWidth
                } else {
                    ColumnUnit::Utf8Bytes
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
                    mapped: self.locate(file, line, column, unit),
                });
            }
            // The location is used up: text right after it starts a new token.
            token_start = end;
            position = end;
        }
        hits
    }
}

// ---------------------------------------------------------------------------------------------
// internals
// ---------------------------------------------------------------------------------------------

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
        let (precision, marigold, segment) = match outcome {
            Ok(placement) => {
                let slot = &self.slots[placement.slot];
                let segment: &Segment = &self.program.map.segments()[slot.segment];
                (
                    placement.precision,
                    Some(self.source_location(placement.range)),
                    Some(SegmentRef {
                        kind: segment.kind,
                        expr_index: segment.expr_index,
                        name: segment.name.clone(),
                    }),
                )
            }
            Err(reason) => (Precision::Unmapped { reason }, None, None),
        };
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

    fn source_location(&self, range: ByteRange) -> MarigoldLocation {
        MarigoldLocation {
            range,
            start: self.source_line_col(range.start),
            end: self.source_line_col(range.end),
        }
    }

    /// Line and column in the Marigold source of an offset on a character boundary.
    fn source_line_col(&self, offset: usize) -> LineCol {
        let line = line_index(&self.source_lines, offset);
        LineCol {
            line: line + 1,
            column: self.source[self.source_lines[line]..offset].chars().count() + 1,
        }
    }

    // --- checksum (step 3) ---

    fn generated_line_number(&self, line: usize) -> usize {
        self.prefix_newlines + 1 + line
    }

    /// 1-based scalar column in the generated *file* of a program offset on `line`.
    fn generated_column(&self, line: usize, offset: usize) -> usize {
        let in_program = self.program.rust[self.rust_lines[line]..offset]
            .chars()
            .count();
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
        let start = self.rust_lines[line];
        match self.rust_lines.get(line + 1) {
            Some(&next) => (&rust[start..next - 1], true),
            None => (&rust[start..], false),
        }
    }

    /// Whether rustc's reported line text is the generated file's line `line`.
    fn line_text_matches(&self, line: usize, reported: &str) -> bool {
        let (content, terminated) = self.generated_line(line);
        // rustc strips the `\r` of a `\r\n` from the text it reports.
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
        let (first, last) = (
            line_index(&self.rust_lines, a),
            line_index(&self.rust_lines, b),
        );
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
        // rustc highlights a multi-line span's first line to its end.
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

    // --- classification (step 4) ---

    /// The innermost slot around position `pos` (`start <= pos < end`).
    fn innermost_at(&self, pos: usize) -> Option<usize> {
        let mut candidate = self
            .slots
            .partition_point(|s| s.start <= pos)
            .checked_sub(1);
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
        let mut candidate = self.slots.partition_point(|s| s.start <= a).checked_sub(1);
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
        // Collect the slots overlapping [a, b). Slots starting before `b` form a prefix of
        // the sorted slots; since `max_end` never decreases, none before `lower` ends after `a`.
        let upper = self.slots.partition_point(|s| s.start < b);
        let lower = self.slots.partition_point(|s| s.max_end <= a);
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
            // The tail of the generated text is the tail of the source: anchor at the end.
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

    // --- run-time locations ---

    fn place_location(
        &self,
        line: usize,
        column: usize,
        unit: ColumnUnit,
    ) -> Result<Placement, UnmappedReason> {
        let outside = UnmappedReason::OutsideGeneratedRegion;
        let index = line.checked_sub(1 + self.prefix_newlines).ok_or(outside)?;
        // A line that starts at the end of the program (after its final `\n`) holds only
        // whatever follows the program in the file.
        let start = *self.rust_lines.get(index).ok_or(outside)?;
        if start >= self.program.rust.len() {
            return Err(outside);
        }
        let (content, _) = self.generated_line(index);
        let end = start + content.len();
        // The text of the line that column counting sees: all of it, including a `\r`.
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

        // Columns on the first program line also count the declared prefix's partial line.
        let (tail_width, tail_known) = if index == 0 {
            measure(&self.prefix_tail, unit)
        } else {
            (0, true)
        };
        let wanted = column - 1;
        let Some(in_line) = wanted.checked_sub(tail_width) else {
            // Inside the declared prefix, unless its width is not trusted.
            return if tail_known {
                Err(outside)
            } else {
                self.at_most_segment(whole_line_placement()?)
            };
        };
        let (offset, known) = advance(whole_line, in_line, unit);
        if !(tail_known && known) {
            // A width we cannot vouch for: the line must at least be one expression's, and the
            // guessed position is never better than the whole segment.
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

// ---------------------------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------------------------

/// Byte offsets where the lines of `text` start; always begins with 0.
fn line_starts(text: &str) -> Vec<usize> {
    let mut starts = vec![0];
    starts.extend(
        text.bytes()
            .enumerate()
            .filter(|&(_, b)| b == b'\n')
            .map(|(i, _)| i + 1),
    );
    starts
}

/// The 0-based line containing `offset`.
fn line_index(starts: &[usize], offset: usize) -> usize {
    starts.partition_point(|&s| s <= offset) - 1
}

fn source_range_fits(source: &str, range: ByteRange) -> bool {
    range.start <= range.end
        && range.end <= source.len()
        && source.is_char_boundary(range.start)
        && source.is_char_boundary(range.end)
}

/// Length of the longest common suffix, in bytes, of the generated and the Marigold text of a
/// segment, shortened until it starts on a character boundary on both sides.
fn verbatim_len(rust: &str, generated: ByteRange, source: &str, range: ByteRange) -> usize {
    if !rust.is_char_boundary(generated.end) {
        return 0;
    }
    let g = &rust.as_bytes()[generated.start..generated.end];
    let s = &source.as_bytes()[range.start..range.end];
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

/// Display width of `c` as `Location::caller().column()` counts it, for the characters that
/// were verified against rustc 1.97; `None` for every other non-ASCII character.
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
        0x20D0..=0x20FF => 0,
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

/// The width of `c` in `unit`, and whether it is trusted.
fn char_width(c: char, unit: ColumnUnit) -> (usize, bool) {
    match unit {
        ColumnUnit::ScalarValues => (1, true),
        ColumnUnit::Utf8Bytes => (c.len_utf8(), true),
        ColumnUnit::DisplayWidth => match display_width(c) {
            Some(w) => (w, true),
            None => (1, false),
        },
    }
}

/// The width of `text` in `unit`, and whether every character's width is trusted.
fn measure(text: &str, unit: ColumnUnit) -> (usize, bool) {
    text.chars().fold((0, true), |(total, known), c| {
        let (w, ok) = char_width(c, unit);
        (total + w, known && ok)
    })
}

/// The byte offset in `line` that is `wanted` units from its start, and whether the widths
/// of the characters before it are trusted. Zero-width characters at the position are skipped.
/// A position inside a character, or past the end of the line, is an invalid span.
fn advance(line: &str, wanted: usize, unit: ColumnUnit) -> (Result<usize, UnmappedReason>, bool) {
    let mut width = 0;
    let mut offset = 0;
    let mut known = true;
    loop {
        let next = line[offset..].chars().next();
        if width == wanted {
            // Zero-width characters share the column of what follows them.
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
        // (character, width) = (scalar delta measured with `Location::caller().column()`) + 1
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
        // Not in the verified set: the width is unknown, never a guess.
        for c in [
            '\u{0f00}',
            '\u{0080}',
            '\u{1f321}',
            '\u{e000}',
            '\u{10ffff}',
        ] {
            assert_eq!(display_width(c), None, "U+{:04X}", c as u32);
        }
        // units that do not depend on a table
        assert_eq!(char_width('日', ColumnUnit::ScalarValues), (1, true));
        assert_eq!(char_width('日', ColumnUnit::Utf8Bytes), (3, true));
        assert_eq!(char_width('\u{0f00}', ColumnUnit::DisplayWidth), (1, false));
    }

    #[test]
    fn advance_converts_columns_and_rejects_positions_inside_characters() {
        let line = "a日\u{0301}b";
        // scalar: a=0 日=1 U+0301=4 b=6
        assert_eq!(advance(line, 0, ColumnUnit::ScalarValues), (Ok(0), true));
        assert_eq!(advance(line, 2, ColumnUnit::ScalarValues), (Ok(4), true));
        assert_eq!(advance(line, 4, ColumnUnit::ScalarValues), (Ok(7), true));
        assert_eq!(
            advance(line, 5, ColumnUnit::ScalarValues),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        // bytes: 日 occupies 1..4
        assert_eq!(advance(line, 4, ColumnUnit::Utf8Bytes), (Ok(4), true));
        assert_eq!(
            advance(line, 2, ColumnUnit::Utf8Bytes),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        // display: a=1, 日=2, U+0301=0, b=1; width 3 sits after 日 and after the combining mark
        assert_eq!(advance(line, 3, ColumnUnit::DisplayWidth), (Ok(6), true));
        assert_eq!(advance(line, 4, ColumnUnit::DisplayWidth), (Ok(7), true));
        // the middle of a wide character
        assert_eq!(
            advance(line, 2, ColumnUnit::DisplayWidth),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        // a width that is not in the table is reported as unknown
        assert!(!advance("\u{0f00}x", 1, ColumnUnit::DisplayWidth).1);
        // the end of the line is a valid position, one past it is not
        assert_eq!(advance("ab", 2, ColumnUnit::ScalarValues), (Ok(2), true));
        assert_eq!(
            advance("ab", 3, ColumnUnit::ScalarValues),
            (Err(UnmappedReason::InvalidSpan), true)
        );
        assert_eq!(
            advance("ab", usize::MAX, ColumnUnit::Utf8Bytes),
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
            verbatim_len(rust, g, source, range(0, source.len())),
            source.len()
        );
        // nothing in common
        assert_eq!(verbatim_len("abc", range(0, 3), "xyz", range(0, 3)), 0);
        // the last byte of two different characters is not a region
        assert_eq!(
            verbatim_len("\u{e9}", range(0, 2), "\u{a9}", range(0, 2)),
            0
        );
        // the region does not start inside a character on either side
        assert_eq!(
            verbatim_len("a\u{e9}", range(0, 3), "\u{a9}", range(0, 2)),
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
        // `./..` and `../.` are the same step
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
                // a one-byte span, a point, and a span that leaves the segment into glue
                for (a, b) in [(at, at + 1), (at, at), (at, segment.generated.end + 1)] {
                    let mapped = remapper.map_span(&span(a, b));
                    assert!(mapped.marigold.is_some(), "n={n} {a}..{b}: {mapped:?}");
                }
            }
            let (scanned, walked) = counters();
            // Exactly the one segment each non-empty span overlaps (a point scans nothing), and
            // one parent link for the span that leaves its segment (no container is found).
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
        // 3 000 flat segments, then one expression nested 40 deep.
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

        // a point in the middle: the innermost segment is the last one starting at or before it
        reset();
        let middle = g0 + depth;
        let mapped = remapper.map_span(&span(middle, middle));
        let location = mapped.marigold.unwrap();
        assert_eq!(
            (location.range.start, location.range.end),
            (s0 + depth - 1, s0 + depth + 1)
        );
        assert_eq!(counters().1, 0);

        // a span that the innermost candidate (the segment starting last) does not contain:
        // the lookup walks up the parent links, one per segment too small, and no further
        reset();
        let (a, b) = (g0 + depth - 1, g0 + depth + 5);
        let mapped = remapper.map_span(&span(a, b));
        assert_eq!(
            mapped.marigold.unwrap().range,
            range(s0 + depth - 5, s0 + depth + 5)
        );
        assert_eq!(counters().1, 4);
    }
}
