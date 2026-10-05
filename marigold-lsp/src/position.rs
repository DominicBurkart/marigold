//! Conversion between byte offsets and LSP positions (UTF-16 code units).
//!
//! ```
//! use lsp_types::Position;
//! use marigold_lsp::position::LineIndex;
//!
//! let idx = LineIndex::new("range(0, 1).return\nrange(Colour).return");
//! assert_eq!(idx.position(25), Position::new(1, 6));
//! assert_eq!(idx.offset(Position::new(1, 6)), 25);
//! ```

use lsp_types::Position;

pub struct LineIndex<'a> {
    text: &'a str,
    line_starts: Vec<usize>,
}

impl<'a> LineIndex<'a> {
    /// Indexes the line starts of `text`.
    ///
    /// ```
    /// use marigold_lsp::position::LineIndex;
    /// let idx = LineIndex::new("a\nb");
    /// assert_eq!(idx.position(2).line, 1);
    /// ```
    pub fn new(text: &'a str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        Self { text, line_starts }
    }

    /// Converts a byte offset to a UTF-16 position. Never panics: offsets are
    /// clamped to the text length and floored to a char boundary, and an
    /// offset on the `\n` of a `\r\n` maps to the position of the `\r`.
    ///
    /// ```
    /// use lsp_types::Position;
    /// use marigold_lsp::position::LineIndex;
    /// let idx = LineIndex::new("a😀b");
    /// assert_eq!(idx.position(3), Position::new(0, 1));
    /// assert_eq!(idx.position(99), Position::new(0, 4));
    ///
    /// let crlf = LineIndex::new("ab\r\ncd");
    /// assert_eq!(crlf.position(2), Position::new(0, 2));
    /// assert_eq!(crlf.position(3), Position::new(0, 2));
    /// assert_eq!(crlf.position(4), Position::new(1, 0));
    /// ```
    pub fn position(&self, offset: usize) -> Position {
        let (line, character) = self.line_and_prefix(offset);
        let units = self.text[self.line_starts[line]..character]
            .chars()
            .map(char::len_utf16)
            .sum::<usize>();
        Position::new(line as u32, units as u32)
    }

    /// Converts a byte offset to a zero-based `(line, column)` pair counted in
    /// Unicode scalar values, with the same clamping rules as [`Self::position`].
    ///
    /// ```
    /// use marigold_lsp::position::LineIndex;
    /// let idx = LineIndex::new("a😀b\nc");
    /// assert_eq!(idx.char_position(5), (0, 2));
    /// assert_eq!(idx.char_position(6), (0, 3));
    /// assert_eq!(idx.char_position(8), (1, 1));
    /// ```
    pub fn char_position(&self, offset: usize) -> (u32, u32) {
        let (line, offset) = self.line_and_prefix(offset);
        let column = self.text[self.line_starts[line]..offset].chars().count();
        (line as u32, column as u32)
    }

    /// Converts a zero-based `(line, column)` pair counted in Unicode scalar
    /// values to a byte offset. Returns `None` when the line does not exist or
    /// the column is past the end of its line (the end itself is valid).
    ///
    /// ```
    /// use marigold_lsp::position::LineIndex;
    /// let idx = LineIndex::new("a😀b\r\nc");
    /// assert_eq!(idx.offset_of_char(0, 2), Some(5));
    /// assert_eq!(idx.offset_of_char(0, 3), Some(6));
    /// assert_eq!(idx.offset_of_char(0, 4), None);
    /// assert_eq!(idx.offset_of_char(1, 1), Some(9));
    /// assert_eq!(idx.offset_of_char(2, 0), None);
    /// ```
    pub fn offset_of_char(&self, line: u32, column: u32) -> Option<usize> {
        let start = *self.line_starts.get(line as usize)?;
        let end = self.line_content_end(line as usize);
        let mut chars = self.text[start..end].char_indices();
        match chars.nth(column as usize) {
            Some((i, _)) => Some(start + i),
            None if column as usize == self.text[start..end].chars().count() => Some(end),
            None => None,
        }
    }

    /// The number of lines, counting a trailing empty line after a final newline.
    ///
    /// ```
    /// use marigold_lsp::position::LineIndex;
    /// assert_eq!(LineIndex::new("a\nb\n").line_count(), 3);
    /// ```
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    fn line_content_end(&self, line: usize) -> usize {
        self.line_starts
            .get(line + 1)
            .map_or(self.text.len(), |&next| {
                if next >= 2 && self.text.as_bytes()[next - 2] == b'\r' {
                    next - 2
                } else {
                    next - 1
                }
            })
    }

    fn line_and_prefix(&self, offset: usize) -> (usize, usize) {
        let mut offset = offset.min(self.text.len());
        while !self.text.is_char_boundary(offset) {
            offset -= 1;
        }
        if self.text.as_bytes().get(offset) == Some(&b'\n')
            && offset > 0
            && self.text.as_bytes()[offset - 1] == b'\r'
        {
            offset -= 1;
        }
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        (line, offset)
    }

    /// Converts an LSP position to a byte offset, clamping out-of-range
    /// lines and characters and never splitting a char or passing a `\r\n`.
    ///
    /// ```
    /// use lsp_types::Position;
    /// use marigold_lsp::position::LineIndex;
    /// let idx = LineIndex::new("a\r\nb");
    /// assert_eq!(idx.offset(Position::new(0, 99)), 1);
    /// assert_eq!(idx.offset(Position::new(1, 1)), 4);
    /// ```
    pub fn offset(&self, position: Position) -> usize {
        let Some(&start) = self.line_starts.get(position.line as usize) else {
            return self.text.len();
        };
        let end =
            self.line_starts
                .get(position.line as usize + 1)
                .map_or(self.text.len(), |&next| {
                    if next >= 2 && self.text.as_bytes()[next - 2] == b'\r' {
                        next - 2
                    } else {
                        next - 1
                    }
                });
        let mut units = 0;
        for (i, c) in self.text[start..end].char_indices() {
            if units >= position.character as usize {
                return start + i;
            }
            units += c.len_utf16();
        }
        end
    }
}
