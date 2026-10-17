//! Stub (red commit): the JSON document of `marigold emit --format json`.

use crate::source_map::{SegmentKind, SourceMap};
use serde::Serialize;

/// Version of the document layout.
pub const EMIT_SCHEMA_VERSION: u32 = 1;

/// A byte range with line and column positions.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EmitRange {
    /// Start byte offset.
    pub start: usize,
    /// End byte offset.
    pub end: usize,
    /// Start line.
    pub start_line: usize,
    /// Start column.
    pub start_column: usize,
    /// End line.
    pub end_line: usize,
    /// End column.
    pub end_column: usize,
}

/// One segment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EmitSegment {
    /// Kind.
    pub kind: SegmentKind,
    /// Name.
    pub name: Option<String>,
    /// Expression index.
    pub expr_index: usize,
    /// Parent segment index.
    pub parent: Option<usize>,
    /// Range in the source.
    pub source: EmitRange,
    /// Range in the Rust.
    pub generated: EmitRange,
}

/// The document.
#[derive(Debug, Clone, Serialize)]
pub struct EmitDocument {
    /// Segments.
    pub segments: Vec<EmitSegment>,
}

impl EmitDocument {
    /// Build the document.
    pub fn new(_source: &str, _rust: &str, _map: &SourceMap) -> EmitDocument {
        unimplemented!("T3 red stub")
    }
}
