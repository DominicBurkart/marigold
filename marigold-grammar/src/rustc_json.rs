//! Read rustc's and cargo's JSON diagnostics (stub for the red commit).

use serde::{Deserialize, Serialize, Serializer};

/// Severity of a rustc diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
#[non_exhaustive]
pub enum Level {
    /// `error`
    #[default]
    Error,
    /// `warning`
    Warning,
    /// `note`
    Note,
    /// `help`
    Help,
    /// `failure-note`
    FailureNote,
    /// `error: internal compiler error`
    Ice,
    /// Any other string.
    Other(String),
}

impl Level {
    /// The string rustc used.
    pub fn as_str(&self) -> &str {
        unimplemented!()
    }
}

impl Serialize for Level {
    fn serialize<S: Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        unimplemented!()
    }
}

impl<'de> Deserialize<'de> for Level {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        unimplemented!()
    }
}

/// One rustc diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcDiagnostic {
    /// Message.
    pub message: String,
    /// Error code.
    pub code: Option<RustcCode>,
    /// Severity.
    pub level: Level,
    /// Spans.
    pub spans: Vec<RustcSpan>,
    /// Sub-diagnostics.
    pub children: Vec<RustcDiagnostic>,
    /// Pre-rendered text.
    pub rendered: Option<String>,
}

/// A diagnostic code.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcCode {
    /// The code.
    pub code: String,
}

/// A rustc span.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcSpan {
    /// File.
    pub file_name: String,
    /// Start byte.
    pub byte_start: usize,
    /// End byte.
    pub byte_end: usize,
    /// Start line.
    pub line_start: usize,
    /// End line.
    pub line_end: usize,
    /// Start column.
    pub column_start: usize,
    /// End column.
    pub column_end: usize,
    /// Primary flag.
    pub is_primary: bool,
    /// Label.
    pub label: Option<String>,
    /// Source lines.
    pub text: Vec<RustcSpanLine>,
    /// Replacement.
    pub suggested_replacement: Option<String>,
    /// Applicability.
    pub suggestion_applicability: Option<String>,
    /// Macro expansion.
    pub expansion: Option<Box<RustcExpansion>>,
}

/// One source line of a span.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcSpanLine {
    /// Text.
    pub text: String,
    /// Highlight start.
    pub highlight_start: usize,
    /// Highlight end.
    pub highlight_end: usize,
}

/// A macro expansion.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
#[serde(default)]
#[non_exhaustive]
pub struct RustcExpansion {
    /// Call site.
    pub span: RustcSpan,
    /// Macro name.
    pub macro_decl_name: String,
}

/// A diagnostic with cargo's wrapper.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct CompilerMessage {
    /// The diagnostic.
    pub diagnostic: RustcDiagnostic,
    /// Cargo package id.
    pub package_id: Option<String>,
    /// Cargo manifest path.
    pub manifest_path: Option<String>,
}

/// A line that could not be read.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct LineError {
    /// 1-based line.
    pub line: usize,
    /// Reason.
    pub message: String,
}

/// Result of reading a stream.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[non_exhaustive]
pub struct ParsedMessages {
    /// Diagnostics.
    pub messages: Vec<CompilerMessage>,
    /// Skipped JSON lines.
    pub skipped: usize,
    /// Unreadable lines.
    pub errors: Vec<LineError>,
}

/// Read one message.
pub fn parse_message_line(_line: &str) -> Result<Option<CompilerMessage>, String> {
    unimplemented!()
}

/// Read a stream.
pub fn parse_messages(_text: &str) -> ParsedMessages {
    unimplemented!()
}
