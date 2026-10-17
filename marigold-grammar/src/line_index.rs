//! Stub (red commit): converts byte offsets to 1-based line/column positions.

/// Unit in which a column is counted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ColumnUnit {
    /// UTF-8 bytes.
    Bytes,
    /// Unicode scalar values.
    Chars,
    /// UTF-16 code units.
    Utf16,
}

/// A 1-based line and column.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    /// 1-based line.
    pub line: usize,
    /// 1-based column.
    pub col: usize,
}

/// Index of the line starts of a text.
#[derive(Debug, Clone)]
pub struct LineIndex<'a> {
    text: &'a str,
}

impl<'a> LineIndex<'a> {
    /// Index `text`.
    pub fn new(text: &'a str) -> Self {
        LineIndex { text }
    }

    /// Number of lines.
    pub fn line_count(&self) -> usize {
        let _ = self.text;
        unimplemented!("T3 red stub")
    }

    /// Position of a byte offset.
    pub fn line_col(&self, _offset: usize, _unit: ColumnUnit) -> LineCol {
        unimplemented!("T3 red stub")
    }

    /// Byte offset of a position.
    pub fn offset(&self, _line: usize, _col: usize, _unit: ColumnUnit) -> Option<usize> {
        unimplemented!("T3 red stub")
    }
}
