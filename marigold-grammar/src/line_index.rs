//! Convert byte offsets into text to 1-based line and column positions, and back.
//!
//! Every range in this crate ([`crate::diagnostics::ByteRange`], the ranges of a
//! [`crate::source_map::SourceMap`]) is a pair of byte offsets. People and tools want
//! lines and columns, and they disagree on what a column counts, so [`LineIndex`]
//! takes the unit as an argument ([`ColumnUnit`]):
//!
//! | unit | a column counts | used by |
//! |---|---|---|
//! | [`ColumnUnit::Chars`] | Unicode scalar values | `rustc` (diagnostics, `--error-format=json`), `marigold check`, `marigold emit` |
//! | [`ColumnUnit::Utf16`] | UTF-16 code units | JavaScript strings, the Language Server Protocol |
//! | [`ColumnUnit::Bytes`] | UTF-8 bytes | `gcc`, `clang`, most byte-oriented tools |
//!
//! Lines and columns are **1-based**, like the diagnostics of `rustc` and `marigold check`.
//!
//! # Lines
//!
//! Only `\n` ends a line. A `\r` is an ordinary character that takes one column, so in a
//! `\r\n` file the `\r` is the last column of its line and the `\n` byte is addressable as
//! the position one past it. A lone `\r` does not start a new line. Tabs take one column.
//! Grapheme clusters are not combined: an accent written as a separate combining
//! character is a column of its own.
//!
//! # Offsets
//!
//! An index is only meaningful for the exact string it was built from. Offsets taken
//! from another copy of the text (trimmed, re-lexed by a proc macro, with converted line
//! endings) give positions in that other text.
//!
//! [`LineIndex::line_col`] never panics: an offset past the end of the text is clamped
//! to the end and an offset inside a multibyte character is moved back to the start of
//! that character. [`LineIndex::offset`] is its exact inverse: for every char boundary
//! `b` and every unit, `offset(line_col(b).line, line_col(b).col)` is `Some(b)`.
//!
//! # Cost
//!
//! [`LineIndex::new`] is `O(n)`. A query takes `O(log lines)` for ASCII text and for
//! [`ColumnUnit::Bytes`]; for other units on a text that contains non-ASCII characters
//! it also scans the line that holds the offset, so a very long non-ASCII line makes
//! many queries quadratic. Programs that consist of one huge line of non-ASCII text are
//! not expected: only string literals and comments can contain non-ASCII characters.
//!
//! ```
//! use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
//!
//! let src = "a\u{1F600}b\r\nx";                  // 'a', emoji (4 bytes, 2 UTF-16 units), 'b', CRLF, 'x'
//! let index = LineIndex::new(src);
//! let b = src.find('b').unwrap();
//! assert_eq!(index.line_col(b, ColumnUnit::Bytes), LineCol { line: 1, col: 6 });
//! assert_eq!(index.line_col(b, ColumnUnit::Chars), LineCol { line: 1, col: 3 });   // rustc's unit
//! assert_eq!(index.line_col(b, ColumnUnit::Utf16), LineCol { line: 1, col: 4 });
//! assert_eq!(index.line_col(src.len() - 1, ColumnUnit::Chars), LineCol { line: 2, col: 1 });
//! assert_eq!(index.offset(1, 3, ColumnUnit::Chars), Some(b));
//! assert_eq!(index.offset(1, 3, ColumnUnit::Utf16), None);   // inside the emoji
//! assert_eq!(index.offset(3, 1, ColumnUnit::Chars), None);   // no such line
//! ```

/// What a column counts. See the [module documentation](self) for who uses which unit.
///
/// ```
/// use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
///
/// let index = LineIndex::new("日本語 x"); // three characters of three bytes each
/// let x = "日本語 ".len();
/// assert_eq!(index.line_col(x, ColumnUnit::Bytes).col, 11);
/// assert_eq!(index.line_col(x, ColumnUnit::Chars).col, 5);
/// assert_eq!(index.line_col(x, ColumnUnit::Utf16).col, 5);
/// assert_eq!(index.line_col(x, ColumnUnit::Chars), LineCol { line: 1, col: 5 });
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ColumnUnit {
    /// UTF-8 bytes.
    Bytes,
    /// Unicode scalar values (`char`s). This is the unit `rustc` uses.
    Chars,
    /// UTF-16 code units: a character outside the Basic Multilingual Plane counts twice.
    Utf16,
}

/// A position in a text. Both fields are 1-based.
///
/// Positions are ordered by line, then by column, which is document order for positions
/// counted in the same unit.
///
/// This is a plain value type that other crates construct (for example in tests), so it
/// is deliberately not `#[non_exhaustive]`, unlike most public types of this crate.
///
/// ```
/// use marigold_grammar::line_index::LineCol;
///
/// assert!(LineCol { line: 1, col: 80 } < LineCol { line: 2, col: 1 });
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    /// Line number, starting at 1.
    pub line: usize,
    /// Column number in the requested [`ColumnUnit`], starting at 1.
    pub col: usize,
}

/// Line starts of a text, for converting between byte offsets and line/column positions.
///
/// Borrows the text. See the [module documentation](self) for the exact semantics.
///
/// ```
/// use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
///
/// let index = LineIndex::new("range(0, 3)\n  .return");
/// assert_eq!(index.line_count(), 2);
/// assert_eq!(index.line_col(15, ColumnUnit::Chars), LineCol { line: 2, col: 4 });
/// assert_eq!(index.offset(2, 4, ColumnUnit::Chars), Some(15));
/// ```
#[derive(Debug, Clone)]
pub struct LineIndex<'a> {
    text: &'a str,
    /// Byte offset of the first byte of each line; the first entry is always 0.
    line_starts: Vec<usize>,
    /// All of `text` is ASCII, so every unit equals the byte distance.
    ascii: bool,
}

impl<'a> LineIndex<'a> {
    /// Index `text` in `O(text.len())`.
    ///
    /// ```
    /// use marigold_grammar::line_index::LineIndex;
    ///
    /// assert_eq!(LineIndex::new("").line_count(), 1);
    /// assert_eq!(LineIndex::new("a\nb\n").line_count(), 3);
    /// ```
    pub fn new(text: &'a str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        LineIndex {
            text,
            line_starts,
            ascii: text.is_ascii(),
        }
    }

    /// Number of lines: the number of `\n` plus one. The (possibly empty) text after
    /// the last `\n` counts as a line, so `"a\n"` has two lines and `""` has one.
    ///
    /// ```
    /// use marigold_grammar::line_index::LineIndex;
    ///
    /// assert_eq!(LineIndex::new("a\r\nb").line_count(), 2);
    /// assert_eq!(LineIndex::new("a\rb").line_count(), 1);
    /// ```
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// The 1-based line and column of the byte at `offset`.
    ///
    /// Total: an `offset` past the end of the text is clamped to the end (the position
    /// just after the last character; after a final `\n` that is column 1 of the last
    /// line), and an `offset` inside a multibyte character is moved back to the start
    /// of that character. The `\n` byte of a line has the column one past the last
    /// character of its line.
    ///
    /// ```
    /// use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
    ///
    /// let index = LineIndex::new("ab\r\ncd");
    /// let at = |offset| index.line_col(offset, ColumnUnit::Chars);
    /// assert_eq!(at(0), LineCol { line: 1, col: 1 });
    /// assert_eq!(at(2), LineCol { line: 1, col: 3 }); // the '\r'
    /// assert_eq!(at(3), LineCol { line: 1, col: 4 }); // the '\n'
    /// assert_eq!(at(4), LineCol { line: 2, col: 1 });
    /// assert_eq!(at(1_000), LineCol { line: 2, col: 3 }); // clamped to the end
    ///
    /// let index = LineIndex::new("é!");
    /// assert_eq!(index.line_col(1, ColumnUnit::Chars), LineCol { line: 1, col: 1 }); // inside 'é'
    /// ```
    pub fn line_col(&self, offset: usize, unit: ColumnUnit) -> LineCol {
        let mut offset = offset.min(self.text.len());
        while !self.text.is_char_boundary(offset) {
            offset -= 1;
        }
        let line = self.line_starts.partition_point(|&start| start <= offset);
        let start = self.line_starts[line - 1];
        let col = 1 + self.width(&self.text[start..offset], unit);
        LineCol { line, col }
    }

    /// The byte offset of a 1-based position: the exact inverse of
    /// [`line_col`](Self::line_col).
    ///
    /// `None` if `line` or `col` is 0, the line does not exist, the column is beyond
    /// the end of the line (the end itself, the `\n` byte or the end of the text, is
    /// valid), or the column falls inside a character: between the two halves of a
    /// surrogate pair for [`ColumnUnit::Utf16`], inside a multibyte character for
    /// [`ColumnUnit::Bytes`]. It never rounds to a neighbouring position.
    ///
    /// ```
    /// use marigold_grammar::line_index::{ColumnUnit, LineIndex};
    ///
    /// let index = LineIndex::new("é\nxy");
    /// assert_eq!(index.offset(1, 2, ColumnUnit::Chars), Some(2)); // the '\n'
    /// assert_eq!(index.offset(1, 2, ColumnUnit::Bytes), None); // inside 'é'
    /// assert_eq!(index.offset(1, 3, ColumnUnit::Bytes), Some(2));
    /// assert_eq!(index.offset(1, 3, ColumnUnit::Chars), None); // past the end of the line
    /// assert_eq!(index.offset(2, 3, ColumnUnit::Chars), Some(5)); // end of the text
    /// assert_eq!(index.offset(0, 1, ColumnUnit::Chars), None);
    /// assert_eq!(index.offset(2, usize::MAX, ColumnUnit::Chars), None);
    /// ```
    pub fn offset(&self, line: usize, col: usize, unit: ColumnUnit) -> Option<usize> {
        if line == 0 || col == 0 || line > self.line_count() {
            return None;
        }
        let start = self.line_starts[line - 1];
        let end = match self.line_starts.get(line) {
            Some(next_start) => next_start - 1, // the '\n' that ends this line
            None => self.text.len(),
        };
        let want = col - 1;
        if unit == ColumnUnit::Bytes || self.ascii {
            // `want` is a byte distance. Without the `then` closure the sum would be
            // evaluated for `usize::MAX` columns too and overflow.
            return (want <= end - start)
                .then(|| start + want)
                .filter(|&at| self.text.is_char_boundary(at));
        }
        let mut seen = 0;
        for (at, c) in self.text[start..end].char_indices() {
            if seen >= want {
                return (seen == want).then_some(start + at);
            }
            seen += Self::char_width(c, unit);
        }
        (seen == want).then_some(end)
    }

    /// Columns taken by `text` (a part of one line) in `unit`.
    fn width(&self, text: &str, unit: ColumnUnit) -> usize {
        match unit {
            ColumnUnit::Bytes => text.len(),
            _ if self.ascii => text.len(),
            unit => text.chars().map(|c| Self::char_width(c, unit)).sum(),
        }
    }

    fn char_width(c: char, unit: ColumnUnit) -> usize {
        match unit {
            ColumnUnit::Bytes => c.len_utf8(),
            ColumnUnit::Chars => 1,
            ColumnUnit::Utf16 => c.len_utf16(),
        }
    }
}
