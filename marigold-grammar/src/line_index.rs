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
//! `b` and every unit, `offset(line_col(b).line, line_col(b).column)` is `Some(b)`.
//!
//! # Cost
//!
//! [`LineIndex::new`] is `O(n)` time and `O(lines + n / 256)` extra memory. A query takes
//! `O(log lines)` for [`ColumnUnit::Bytes`] and for lines that are pure ASCII. On a line
//! with non-ASCII characters the index remembers the character counts at about every
//! 256th byte, so a query takes a binary search plus a scan of at most a few hundred
//! bytes, however long the line is.
//!
//! ```
//! use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
//!
//! let src = "a\u{1F600}b\r\nx";                  // 'a', emoji (4 bytes, 2 UTF-16 units), 'b', CRLF, 'x'
//! let index = LineIndex::new(src);
//! let b = src.find('b').unwrap();
//! assert_eq!(index.line_col(b, ColumnUnit::Bytes), LineCol { line: 1, column: 6 });
//! assert_eq!(index.line_col(b, ColumnUnit::Chars), LineCol { line: 1, column: 3 });   // rustc's unit
//! assert_eq!(index.line_col(b, ColumnUnit::Utf16), LineCol { line: 1, column: 4 });
//! assert_eq!(index.line_col(src.len() - 1, ColumnUnit::Chars), LineCol { line: 2, column: 1 });
//! assert_eq!(index.offset(1, 3, ColumnUnit::Chars), Some(b));
//! assert_eq!(index.offset(1, 3, ColumnUnit::Utf16), None);   // inside the emoji
//! assert_eq!(index.offset(3, 1, ColumnUnit::Chars), None);   // no such line
//! ```

use crate::diagnostics::ByteRange;
use std::collections::BTreeMap;

/// What a column counts. See the [module documentation](self) for who uses which unit.
///
/// ```
/// use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
///
/// let index = LineIndex::new("日本語 x"); // three characters of three bytes each
/// let x = "日本語 ".len();
/// assert_eq!(index.line_col(x, ColumnUnit::Bytes).column, 11);
/// assert_eq!(index.line_col(x, ColumnUnit::Chars).column, 5);
/// assert_eq!(index.line_col(x, ColumnUnit::Utf16).column, 5);
/// assert_eq!(index.line_col(x, ColumnUnit::Chars), LineCol { line: 1, column: 5 });
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
#[serde(rename_all = "snake_case")]
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
/// assert!(LineCol { line: 1, column: 80 } < LineCol { line: 2, column: 1 });
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize)]
pub struct LineCol {
    /// Line number, starting at 1.
    pub line: usize,
    /// Column number in the requested [`ColumnUnit`], starting at 1.
    pub column: usize,
}

/// Distance in bytes between the character counts remembered for a long non-ASCII line.
const STRIDE: usize = 256;

#[cfg(test)]
thread_local! {
    static SCANNED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Character counts at a char boundary of a long non-ASCII line.
#[derive(Debug, Clone, Copy)]
struct Checkpoint {
    /// Byte offset from the start of the line.
    byte: usize,
    /// Unicode scalar values before it, from the start of the line.
    chars: usize,
    /// UTF-16 code units before it, from the start of the line.
    utf16: usize,
}

impl Checkpoint {
    fn units(&self, unit: ColumnUnit) -> usize {
        match unit {
            ColumnUnit::Bytes => self.byte,
            ColumnUnit::Chars => self.chars,
            ColumnUnit::Utf16 => self.utf16,
        }
    }
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
/// assert_eq!(index.line_col(15, ColumnUnit::Chars), LineCol { line: 2, column: 4 });
/// assert_eq!(index.offset(2, 4, ColumnUnit::Chars), Some(15));
/// ```
#[derive(Debug, Clone)]
pub struct LineIndex<'a> {
    text: &'a str,
    /// Byte offset of the first byte of each line; the first entry is always 0.
    line_starts: Vec<usize>,
    /// The lines that contain a non-ASCII character, by 0-based line number, with
    /// checkpoints about every [`STRIDE`] bytes (none for a line shorter than that).
    /// A line without an entry is pure ASCII: every unit is the byte distance.
    non_ascii: BTreeMap<usize, Vec<Checkpoint>>,
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
        let line_starts: Vec<usize> = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        let mut non_ascii = BTreeMap::new();
        for (index, &start) in line_starts.iter().enumerate() {
            let end = Self::end_of(text, &line_starts, index);
            let line = &text[start..end];
            if line.is_ascii() {
                continue;
            }
            let mut checkpoints = Vec::new();
            let (mut chars, mut utf16, mut next) = (0, 0, STRIDE);
            for (byte, c) in line.char_indices() {
                if byte >= next {
                    checkpoints.push(Checkpoint { byte, chars, utf16 });
                    next = byte + STRIDE;
                }
                chars += 1;
                utf16 += c.len_utf16();
            }
            non_ascii.insert(index, checkpoints);
        }
        LineIndex {
            text,
            line_starts,
            non_ascii,
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

    /// The bytes of a 1-based `line`, without the `\n` that ends it (a `\r` before it
    /// belongs to the line). `None` if `line` is 0 or past [`line_count`](Self::line_count).
    ///
    /// ```
    /// use marigold_grammar::line_index::LineIndex;
    ///
    /// let src = "ab\r\ncd";
    /// let index = LineIndex::new(src);
    /// let second = index.line_range(2).unwrap();
    /// assert_eq!(&src[second.start..second.end], "cd");
    /// let first = index.line_range(1).unwrap();
    /// assert_eq!(&src[first.start..first.end], "ab\r");
    /// assert!(index.line_range(3).is_none());
    /// ```
    pub fn line_range(&self, line: usize) -> Option<ByteRange> {
        let index = line.checked_sub(1).filter(|&i| i < self.line_count())?;
        Some(ByteRange {
            start: self.line_starts[index],
            end: Self::end_of(self.text, &self.line_starts, index),
        })
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
    /// assert_eq!(at(0), LineCol { line: 1, column: 1 });
    /// assert_eq!(at(2), LineCol { line: 1, column: 3 }); // the '\r'
    /// assert_eq!(at(3), LineCol { line: 1, column: 4 }); // the '\n'
    /// assert_eq!(at(4), LineCol { line: 2, column: 1 });
    /// assert_eq!(at(1_000), LineCol { line: 2, column: 3 }); // clamped to the end
    ///
    /// let index = LineIndex::new("é!");
    /// assert_eq!(index.line_col(1, ColumnUnit::Chars), LineCol { line: 1, column: 1 }); // inside 'é'
    /// ```
    pub fn line_col(&self, offset: usize, unit: ColumnUnit) -> LineCol {
        let mut offset = offset.min(self.text.len());
        while !self.text.is_char_boundary(offset) {
            offset -= 1;
        }
        let line = self.line_starts.partition_point(|&start| start <= offset);
        let start = self.line_starts[line - 1];
        let relative = offset - start;
        let before_offset = match self.checkpoints(line - 1, unit) {
            None => relative,
            Some(checkpoints) => {
                let before = checkpoints.partition_point(|c| c.byte <= relative);
                let (byte, units) = match before.checked_sub(1).map(|i| checkpoints[i]) {
                    Some(c) => (c.byte, c.units(unit)),
                    None => (0, 0),
                };
                units + Self::width(&self.text[start + byte..offset], unit)
            }
        };
        LineCol {
            line,
            column: 1 + before_offset,
        }
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
        if col == 0 {
            return None;
        }
        let range = self.line_range(line)?;
        let want = col - 1;
        let Some(checkpoints) = self.checkpoints(line - 1, unit) else {
            return (want <= range.end - range.start)
                .then(|| range.start + want)
                .filter(|&at| self.text.is_char_boundary(at));
        };
        let before = checkpoints.partition_point(|c| c.units(unit) <= want);
        let (mut at, mut seen) = match before.checked_sub(1).map(|i| checkpoints[i]) {
            Some(c) => (range.start + c.byte, c.units(unit)),
            None => (range.start, 0),
        };
        for c in self.text[at..range.end].chars() {
            #[cfg(test)]
            SCANNED.with(|s| s.set(s.get() + 1));
            if seen >= want {
                break;
            }
            seen += Self::char_width(c, unit);
            at += c.len_utf8();
        }
        (seen == want).then_some(at)
    }

    /// Where line `index` (0-based) ends, before its `\n`.
    fn end_of(text: &str, line_starts: &[usize], index: usize) -> usize {
        match line_starts.get(index + 1) {
            Some(next_start) => next_start - 1,
            None => text.len(),
        }
    }

    /// The checkpoints of line `index` if its columns in `unit` are not the byte distance.
    fn checkpoints(&self, index: usize, unit: ColumnUnit) -> Option<&[Checkpoint]> {
        if unit == ColumnUnit::Bytes {
            return None;
        }
        self.non_ascii.get(&index).map(Vec::as_slice)
    }

    /// Columns taken by `text` (a part of one line) in `unit`.
    fn width(text: &str, unit: ColumnUnit) -> usize {
        text.chars()
            .map(|c| {
                #[cfg(test)]
                SCANNED.with(|s| s.set(s.get() + 1));
                Self::char_width(c, unit)
            })
            .sum()
    }

    fn char_width(c: char, unit: ColumnUnit) -> usize {
        match unit {
            ColumnUnit::Bytes => c.len_utf8(),
            ColumnUnit::Chars => 1,
            ColumnUnit::Utf16 => c.len_utf16(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn scanned(work: impl FnOnce()) -> usize {
        SCANNED.with(|s| s.set(0));
        work();
        SCANNED.with(|s| s.get())
    }

    /// A query looks at the characters between two checkpoints, never at the whole line.
    #[test]
    fn queries_on_a_long_non_ascii_line_scan_one_stride() {
        let line = format!("é{}日{}", "a".repeat(100_000), "b".repeat(100_000));
        let index = LineIndex::new(&line);
        let mut worst = 0;
        for offset in (0..line.len()).step_by(997) {
            for unit in [ColumnUnit::Chars, ColumnUnit::Utf16] {
                let mut position = None;
                worst = worst.max(scanned(|| position = Some(index.line_col(offset, unit))));
                let position = position.unwrap();
                let start = (0..=offset).rev().find(|&i| line.is_char_boundary(i));
                worst = worst.max(scanned(|| {
                    assert_eq!(index.offset(position.line, position.column, unit), start)
                }));
            }
        }
        assert!(worst <= STRIDE + 4, "a query scanned {worst} characters");
    }

    /// The upper bound above is vacuous if the counter stops counting: both conversions must
    /// report some work, and no more than a stride.
    #[test]
    fn the_work_counter_is_wired_to_both_conversions() {
        let line = format!("é{}", "a".repeat(5000));
        let index = LineIndex::new(&line);
        let n = scanned(|| {
            index.line_col(3000, ColumnUnit::Chars);
        });
        assert!((1..=STRIDE + 4).contains(&n), "line_col scanned {n}");
        let n = scanned(|| {
            index.offset(1, 3000, ColumnUnit::Chars);
        });
        assert!((1..=STRIDE + 4).contains(&n), "offset scanned {n}");
    }

    #[test]
    fn building_keeps_one_checkpoint_per_stride() {
        let line = "é".repeat(10 * STRIDE);
        let index = LineIndex::new(&line);
        let checkpoints = &index.non_ascii[&0];
        assert_eq!(checkpoints.len(), 19);
        assert_eq!(checkpoints[0].byte, STRIDE);
        assert_eq!(checkpoints[0].chars, STRIDE / 2);
        let index = LineIndex::new("plain\né\nplain");
        assert_eq!(index.non_ascii.keys().copied().collect::<Vec<_>>(), [1]);
        assert!(index.non_ascii[&1].is_empty());
    }

    fn lc(line: usize, column: usize) -> LineCol {
        LineCol { line, column }
    }

    #[test]
    fn many_short_lines_need_no_character_scanning() {
        let text = "x\n".repeat(200_000);
        let index = LineIndex::new(&text);
        let mut worst = 0;
        for i in 0..200_000usize {
            worst = worst.max(scanned(|| {
                assert_eq!(index.line_col(i * 2, ColumnUnit::Chars), lc(i + 1, 1));
            }));
        }
        assert!(worst <= STRIDE + 4, "a query scanned {worst} characters");
    }

    #[test]
    fn column_queries_on_a_long_ascii_line_scan_no_characters() {
        let text = "a".repeat(2_000_000);
        let index = LineIndex::new(&text);
        let mut worst = 0;
        for i in 0..200_000usize {
            worst = worst.max(scanned(|| {
                assert_eq!(index.line_col(i * 10, ColumnUnit::Chars), lc(1, i * 10 + 1));
                assert_eq!(index.offset(1, i * 10 + 1, ColumnUnit::Utf16), Some(i * 10));
            }));
        }
        assert!(worst <= STRIDE + 4, "a query scanned {worst} characters");
    }

    #[test]
    fn a_long_line_with_one_multibyte_character_scans_one_stride_per_query() {
        let text = format!("é{}", "a".repeat(909_000));
        let index = LineIndex::new(&text);
        let mut worst = 0;
        for i in 0..20_000usize {
            let b = 2 + (i * 45) % 909_000;
            let want = lc(1, b);
            worst = worst.max(scanned(|| {
                assert_eq!(index.line_col(b, ColumnUnit::Chars), want);
                assert_eq!(index.line_col(b, ColumnUnit::Utf16), want);
                assert_eq!(index.offset(1, b, ColumnUnit::Chars), Some(b));
            }));
        }
        assert!(
            worst <= 3 * (STRIDE + 4),
            "a query scanned {worst} characters"
        );
        assert!(worst >= 1, "the counter saw no work");
    }

    #[test]
    fn twenty_thousand_expressions_on_one_line_build_with_bounded_scanning() {
        let n = 20_000;
        let src: String = (0..n)
            .map(|i| format!("v{i} = range(0, 1) "))
            .collect::<String>()
            + "v0.return";
        let g = crate::marigold_generate(&src).unwrap();
        let mut doc = None;
        let work = scanned(|| {
            doc = Some(crate::emit::EmitDocument::new(&src, &g.rust, &g.map));
        });
        let doc = doc.unwrap();
        assert_eq!(doc.segments.len(), 2 * n + 1);
        let last = doc.segments.iter().find(|s| s.expr_index == n).unwrap();
        assert_eq!(
            (last.source.start_line, last.source.start_column),
            (1, src.len() - "v0.return".len() + 1)
        );
        assert!(
            work <= 4 * (STRIDE + 4) * doc.segments.len(),
            "building the document scanned {work} characters"
        );
    }
}
