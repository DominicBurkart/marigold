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
        let start = self.line_starts[line];
        let character = self.text[start..offset]
            .chars()
            .map(char::len_utf16)
            .sum::<usize>();
        Position::new(line as u32, character as u32)
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
