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
    pub fn new(text: &'a str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        Self { text, line_starts }
    }

    pub fn position(&self, offset: usize) -> Position {
        let offset = offset.min(self.text.len());
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        let start = self.line_starts[line];
        let character = self.text[start..offset]
            .chars()
            .map(char::len_utf16)
            .sum::<usize>();
        Position::new(line as u32, character as u32)
    }

    pub fn offset(&self, position: Position) -> usize {
        let Some(&start) = self.line_starts.get(position.line as usize) else {
            return self.text.len();
        };
        let end = self
            .line_starts
            .get(position.line as usize + 1)
            .map_or(self.text.len(), |&next| next - 1);
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
