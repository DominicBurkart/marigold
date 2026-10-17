//! Read the JSON diagnostics that rustc and cargo print.
//!
//! `rustc --error-format=json` prints one JSON object per line to stderr, and
//! `cargo build --message-format=json` prints one object per line to stdout, with each
//! rustc diagnostic wrapped in `{"reason": "compiler-message", ...}`. This module reads
//! both forms into plain Rust values; [`crate::remap`] maps them back to Marigold.
//!
//! The types model only what remapping needs and are deliberately forgiving: unknown
//! fields are ignored, every field has a default, and an unknown severity is kept as
//! [`Level::Other`]. Numbers must be non-negative integers. `code.explanation`, which is
//! about a kilobyte per error, and `expansion.def_site_span` are not read. This module
//! does no IO: callers hand it text.
//!
//! # Units
//!
//! rustc reports `byte_start` and `byte_end` as byte offsets into the file on disk
//! (end exclusive, a `\r` counts), `line_*` as 1-based lines (only `\n` ends a line) and
//! `column_*` and `highlight_*` as 1-based counts of Unicode scalar values. This module
//! keeps those numbers exactly as rustc printed them.
//!
//! ```
//! use marigold_grammar::rustc_json::{parse_messages, Level};
//!
//! // What `cargo build --message-format=json` printed: an artifact record, a
//! // diagnostic wrapped by cargo, and cargo's human-readable summary line.
//! let output = r#"{"reason":"compiler-artifact","package_id":"p 0.1.0"}
//! {"reason":"compiler-message","package_id":"p 0.1.0","manifest_path":"/w/Cargo.toml","message":{"message":"cannot find value `x` in this scope","code":{"code":"E0425","explanation":"..."},"level":"error","spans":[{"file_name":"src/gen.rs","byte_start":10,"byte_end":11,"line_start":2,"line_end":2,"column_start":5,"column_end":6,"is_primary":true,"label":"not found in this scope","text":[],"expansion":null}],"children":[],"rendered":null}}
//! error: could not compile `p` due to 1 previous error"#;
//!
//! let parsed = parse_messages(output);
//! assert_eq!(parsed.skipped, 1); // the artifact record
//! assert_eq!(parsed.errors.len(), 1); // the last line is not JSON
//! assert_eq!(parsed.errors[0].line, 3);
//!
//! let message = &parsed.messages[0];
//! assert_eq!(message.manifest_path.as_deref(), Some("/w/Cargo.toml"));
//! let diagnostic = &message.diagnostic;
//! assert_eq!(diagnostic.level, Level::Error);
//! assert_eq!(diagnostic.code.as_ref().unwrap().code, "E0425");
//! assert_eq!(diagnostic.spans[0].byte_start, 10);
//! assert_eq!(diagnostic.spans[0].label.as_deref(), Some("not found in this scope"));
//! ```

use serde::de::{IgnoredAny, MapAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::borrow::Cow;
use std::fmt;

/// The severity of a diagnostic, as rustc spells it in the JSON `level` field.
///
/// Reads any string, so a severity added by a later rustc is kept as
/// [`Level::Other`] instead of failing the diagnostic. Serializes back to the same
/// string. The default is [`Level::Error`].
///
/// ```
/// use marigold_grammar::rustc_json::Level;
///
/// let level: Level = serde_json::from_str("\"failure-note\"").unwrap();
/// assert_eq!(level, Level::FailureNote);
/// assert_eq!(level.as_str(), "failure-note");
///
/// let future: Level = serde_json::from_str("\"future-level\"").unwrap();
/// assert_eq!(future, Level::Other("future-level".to_string()));
/// assert_eq!(serde_json::to_string(&future).unwrap(), "\"future-level\"");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
#[non_exhaustive]
pub enum Level {
    /// `"error"`.
    #[default]
    Error,
    /// `"warning"`.
    Warning,
    /// `"note"`.
    Note,
    /// `"help"`.
    Help,
    /// `"failure-note"`: the closing records, like "For more information about this error".
    FailureNote,
    /// `"error: internal compiler error"`.
    Ice,
    /// Any other string.
    Other(String),
}

impl Level {
    /// The string rustc uses for this level.
    ///
    /// ```
    /// use marigold_grammar::rustc_json::Level;
    ///
    /// assert_eq!(Level::Warning.as_str(), "warning");
    /// assert_eq!(Level::Ice.as_str(), "error: internal compiler error");
    /// assert_eq!(Level::Other("lint".to_string()).as_str(), "lint");
    /// ```
    pub fn as_str(&self) -> &str {
        match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
            Level::FailureNote => "failure-note",
            Level::Ice => "error: internal compiler error",
            Level::Other(other) => other,
        }
    }

    fn from_wire(s: String) -> Level {
        match s.as_str() {
            "error" => Level::Error,
            "warning" => Level::Warning,
            "note" => Level::Note,
            "help" => Level::Help,
            "failure-note" => Level::FailureNote,
            "error: internal compiler error" => Level::Ice,
            _ => Level::Other(s),
        }
    }
}

impl Serialize for Level {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for Level {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        String::deserialize(deserializer).map(Level::from_wire)
    }
}

/// One diagnostic as rustc prints it: a message, a severity, spans and nested
/// sub-diagnostics (notes and help).
///
/// Build one with [`parse_message_line`] or [`parse_messages`], or with
/// `serde_json::from_str` on the bare object. Every field is optional on the wire.
///
/// ```
/// use marigold_grammar::rustc_json::{Level, RustcDiagnostic};
///
/// let d: RustcDiagnostic = serde_json::from_str(
///     r#"{"message":"unused variable","level":"warning","spans":[],"children":[{"message":"help text","level":"help"}]}"#,
/// ).unwrap();
/// assert_eq!(d.level, Level::Warning);
/// assert_eq!(d.children[0].level, Level::Help);
/// assert!(d.code.is_none() && d.rendered.is_none());
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcDiagnostic {
    /// The headline, like ``cannot find value `x` in this scope``.
    pub message: String,
    /// The error or lint code (`E0425`, `unused_mut`), if any.
    pub code: Option<RustcCode>,
    /// The severity.
    pub level: Level,
    /// Where the diagnostic points; empty for records like "aborting due to 2 previous errors".
    pub spans: Vec<RustcSpan>,
    /// Notes, help and suggestions attached to this diagnostic, with their own spans.
    pub children: Vec<RustcDiagnostic>,
    /// The text rustc would print. It describes the *generated* Rust file, so it is not
    /// meant to be shown to a Marigold user.
    pub rendered: Option<String>,
}

/// The code of a diagnostic.
///
/// ```
/// use marigold_grammar::rustc_json::RustcCode;
///
/// // `explanation` is present in rustc's output and deliberately not read.
/// let code: RustcCode = serde_json::from_str(r#"{"code":"E0308","explanation":"long text"}"#).unwrap();
/// assert_eq!(code.code, "E0308");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcCode {
    /// `E0308`, `unused_mut`, ...
    pub code: String,
}

/// A region of a file that a diagnostic points at, in rustc's units (see the
/// [module documentation](self)).
///
/// All fields default, so a minimal span only needs the file and the byte range. rustc
/// also reports zero-width spans (`byte_start == byte_end`) for insertions.
///
/// ```
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let span: RustcSpan = serde_json::from_str(
///     r#"{"file_name":"src/gen.rs","byte_start":5,"byte_end":9,"is_primary":true,"label":"here"}"#,
/// ).unwrap();
/// assert_eq!((span.byte_start, span.byte_end), (5, 9));
/// assert!(span.is_primary && span.text.is_empty() && span.expansion.is_none());
///
/// // Spans are plain data: build one by default-constructing it.
/// let mut built = RustcSpan::default();
/// built.file_name = "src/gen.rs".to_string();
/// assert_eq!(built.byte_end, 0);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcSpan {
    /// The path exactly as rustc was given it (`src/gen.rs`, `./src/gen.rs`, an absolute
    /// path, or `/rustc/<hash>/library/...` for the standard library).
    pub file_name: String,
    /// Offset of the first byte, in the file on disk.
    pub byte_start: usize,
    /// Offset one past the last byte.
    pub byte_end: usize,
    /// 1-based line of `byte_start`.
    pub line_start: usize,
    /// 1-based line of `byte_end`.
    pub line_end: usize,
    /// 1-based column of `byte_start`, in Unicode scalar values.
    pub column_start: usize,
    /// 1-based column of `byte_end`, in Unicode scalar values.
    pub column_end: usize,
    /// Whether this is the span the diagnostic is about.
    pub is_primary: bool,
    /// The text rustc prints under the span.
    pub label: Option<String>,
    /// The source lines the span covers, as rustc printed them.
    pub text: Vec<RustcSpanLine>,
    /// For suggestions, the text that would replace the span.
    pub suggested_replacement: Option<String>,
    /// For suggestions, rustc's `MachineApplicable`, `MaybeIncorrect`, ...
    pub suggestion_applicability: Option<String>,
    /// If the span comes out of a macro, where the macro was called.
    pub expansion: Option<Box<RustcExpansion>>,
}

/// One source line shown for a span.
///
/// ```
/// use marigold_grammar::rustc_json::RustcSpanLine;
///
/// let line: RustcSpanLine = serde_json::from_str(
///     r#"{"text":"let a = 1;","highlight_start":5,"highlight_end":6}"#,
/// ).unwrap();
/// // `highlight_*` are 1-based columns in scalar values, end exclusive.
/// assert_eq!(&line.text[line.highlight_start - 1..line.highlight_end - 1], "a");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcSpanLine {
    /// The line without its line break (rustc strips `\r` as well).
    pub text: String,
    /// 1-based column where the span starts on this line.
    pub highlight_start: usize,
    /// 1-based column one past where the span ends on this line.
    pub highlight_end: usize,
}

/// How a span came out of a macro: the span of the macro call, and the macro's name.
///
/// Expansions nest: `span.expansion` is the next hop, ending at the call written in
/// the user's own code.
///
/// ```
/// use marigold_grammar::rustc_json::RustcSpan;
///
/// let span: RustcSpan = serde_json::from_str(
///     r#"{"file_name":"/rustc/x/library/alloc/src/macros.rs","byte_start":1,"byte_end":2,
///        "expansion":{"span":{"file_name":"src/gen.rs","byte_start":50,"byte_end":60},
///                     "macro_decl_name":"format!","def_site_span":null}}"#,
/// ).unwrap();
/// let hop = span.expansion.unwrap();
/// assert_eq!(hop.macro_decl_name, "format!");
/// assert_eq!(hop.span.file_name, "src/gen.rs");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcExpansion {
    /// The call site of the macro.
    pub span: RustcSpan,
    /// The macro's name as rustc prints it (`format!`, `$crate::format_args_nl!`).
    pub macro_decl_name: String,
}

/// A diagnostic together with the fields cargo adds when it forwards rustc's output.
///
/// `package_id` and `manifest_path` are `None` for rustc's own (bare) output. cargo
/// reports `file_name`s relative to a directory that depends on the package; the
/// `manifest_path` is what a caller needs to resolve them.
///
/// ```
/// use marigold_grammar::rustc_json::parse_message_line;
///
/// let bare = r#"{"$message_type":"diagnostic","message":"m","level":"warning","spans":[],"children":[]}"#;
/// let message = parse_message_line(bare).unwrap().unwrap();
/// assert_eq!(message.diagnostic.message, "m");
/// assert!(message.package_id.is_none() && message.manifest_path.is_none());
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct CompilerMessage {
    /// The diagnostic.
    pub diagnostic: RustcDiagnostic,
    /// cargo's `package_id` of the crate being compiled.
    pub package_id: Option<String>,
    /// The absolute path of that crate's `Cargo.toml`.
    pub manifest_path: Option<String>,
}

/// A line of output that [`parse_messages`] could not read.
///
/// ```
/// use marigold_grammar::rustc_json::parse_messages;
///
/// let parsed = parse_messages("error: could not compile `p`");
/// assert_eq!(parsed.errors[0].line, 1);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct LineError {
    /// 1-based line number in the text that was read.
    pub line: usize,
    /// Why the line was rejected (a `serde_json` message; it does not repeat the line).
    pub message: String,
}

/// Everything [`parse_messages`] found in a stream of JSON lines.
///
/// ```
/// use marigold_grammar::rustc_json::ParsedMessages;
///
/// let none = ParsedMessages::default();
/// assert!(none.messages.is_empty() && none.errors.is_empty() && none.skipped == 0);
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[non_exhaustive]
pub struct ParsedMessages {
    /// The diagnostics, in output order.
    pub messages: Vec<CompilerMessage>,
    /// Lines that were valid JSON but not diagnostics (cargo's `compiler-artifact`,
    /// `build-finished`, rustc's `artifact` and `future_incompat` records).
    pub skipped: usize,
    /// Lines that were not valid JSON, or whose diagnostic was malformed.
    pub errors: Vec<LineError>,
}

/// What a line looks like before its diagnostic is read.
struct Probe {
    reason: Option<String>,
    message_type: Option<String>,
    has_spans: bool,
    has_level: bool,
}

impl<'de> Deserialize<'de> for Probe {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct ProbeVisitor;

        impl<'de> Visitor<'de> for ProbeVisitor {
            type Value = Probe;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a JSON object")
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Probe, A::Error> {
                let mut probe = Probe {
                    reason: None,
                    message_type: None,
                    has_spans: false,
                    has_level: false,
                };
                while let Some(key) = map.next_key::<Cow<'de, str>>()? {
                    match &*key {
                        "reason" => probe.reason = map.next_value()?,
                        "$message_type" => probe.message_type = map.next_value()?,
                        "spans" => {
                            probe.has_spans = true;
                            map.next_value::<IgnoredAny>()?;
                        }
                        "level" => {
                            probe.has_level = true;
                            map.next_value::<IgnoredAny>()?;
                        }
                        _ => {
                            map.next_value::<IgnoredAny>()?;
                        }
                    }
                }
                Ok(probe)
            }
        }

        deserializer.deserialize_map(ProbeVisitor)
    }
}

/// The part of a cargo `compiler-message` record that is read.
#[derive(Deserialize)]
struct CargoRecord {
    package_id: Option<String>,
    manifest_path: Option<String>,
    message: RustcDiagnostic,
}

/// Read one line of rustc or cargo JSON output.
///
/// Accepts the three shapes a diagnostic comes in:
///
/// * cargo's `{"reason": "compiler-message", "message": {...}}`
///   (`cargo build --message-format=json`),
/// * rustc's bare `{"$message_type": "diagnostic", ...}` (`--error-format=json`),
/// * the bare object of an older rustc, which has no `$message_type` but has `spans`
///   and `level`.
///
/// Returns `Ok(None)` for any other JSON object (`compiler-artifact`,
/// `build-finished`, `artifact`, `future_incompat`, ...) and for a blank line, and
/// `Err` with a message for text that is not JSON, JSON that is not an object, or a
/// diagnostic with a malformed field. The text may be a pretty-printed document that
/// spans several lines, as long as it is a single JSON value. JSON nested deeper than
/// `serde_json`'s limit of 128 levels is an `Err`, never a stack overflow.
///
/// ```
/// use marigold_grammar::rustc_json::parse_message_line;
///
/// let line = r#"{"$message_type":"diagnostic","message":"boom","level":"error","spans":[],"children":[]}"#;
/// let message = parse_message_line(line).unwrap().unwrap();
/// assert_eq!(message.diagnostic.message, "boom");
///
/// assert_eq!(parse_message_line(r#"{"reason":"build-finished","success":false}"#), Ok(None));
/// assert_eq!(parse_message_line("   "), Ok(None));
/// assert!(parse_message_line("error: could not compile `p`").is_err());
/// ```
pub fn parse_message_line(line: &str) -> Result<Option<CompilerMessage>, String> {
    if line.trim().is_empty() {
        return Ok(None);
    }
    let error = |e: serde_json::Error| e.to_string();
    let probe: Probe = serde_json::from_str(line).map_err(error)?;
    let bare = |diagnostic| CompilerMessage {
        diagnostic,
        package_id: None,
        manifest_path: None,
    };
    match probe.reason.as_deref() {
        Some("compiler-message") => {
            let record: CargoRecord = serde_json::from_str(line).map_err(error)?;
            Ok(Some(CompilerMessage {
                diagnostic: record.message,
                package_id: record.package_id,
                manifest_path: record.manifest_path,
            }))
        }
        Some(_) => Ok(None),
        None => {
            let is_diagnostic = match probe.message_type.as_deref() {
                Some(kind) => kind == "diagnostic",
                None => probe.has_spans && probe.has_level,
            };
            if !is_diagnostic {
                return Ok(None);
            }
            serde_json::from_str(line)
                .map(|d| Some(bare(d)))
                .map_err(error)
        }
    }
}

/// Read a whole stream of rustc or cargo JSON lines.
///
/// Never fails as a whole: each line is read with [`parse_message_line`]; diagnostics
/// are collected in order, other JSON records are counted in
/// [`skipped`](ParsedMessages::skipped), and anything else (cargo's human-readable
/// `error: could not compile ...` line, for example) is recorded as a [`LineError`]
/// with its 1-based line number. Blank lines are ignored.
///
/// ```
/// use marigold_grammar::rustc_json::parse_messages;
///
/// let text = concat!(
///     r#"{"$message_type":"diagnostic","message":"a","level":"warning","spans":[],"children":[]}"#, "\n",
///     "not json\n",
///     "\n",
///     r#"{"$message_type":"artifact","artifact":"x.rmeta","emit":"metadata"}"#, "\n",
///     r#"{"$message_type":"diagnostic","message":"b","level":"error","spans":[],"children":[]}"#, "\n",
/// );
/// let parsed = parse_messages(text);
/// let messages: Vec<_> = parsed.messages.iter().map(|m| m.diagnostic.message.as_str()).collect();
/// assert_eq!(messages, ["a", "b"]);
/// assert_eq!(parsed.skipped, 1);
/// assert_eq!(parsed.errors.len(), 1);
/// assert_eq!(parsed.errors[0].line, 2);
/// ```
pub fn parse_messages(text: &str) -> ParsedMessages {
    let mut parsed = ParsedMessages::default();
    for (index, line) in text.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        match parse_message_line(line) {
            Ok(Some(message)) => parsed.messages.push(message),
            Ok(None) => parsed.skipped += 1,
            Err(message) => parsed.errors.push(LineError {
                line: index + 1,
                message,
            }),
        }
    }
    parsed
}
