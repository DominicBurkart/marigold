//! Differential test: `marigold_grammar::line_index::LineIndex` (T3) against this crate's own
//! `position::LineIndex` (LSP, 0-based, UTF-16). The two implementations stay separate on purpose
//! (different base, different `Position` type, different CRLF rule); this file pins exactly how
//! they agree and where they are documented to differ, so a future unification is a conscious act.
//!
//! The one documented difference: the grammar index treats `\r` and `\n` as ordinary columns,
//! while the LSP index snaps an offset that sits on the `\n` of a `\r\n` back to the `\r`.

use marigold_grammar::line_index::{ColumnUnit, LineIndex as Grammar};
use marigold_lsp::position::LineIndex as Lsp;
use proptest::prelude::*;

fn mixed_text() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        prop_oneof![
            4 => Just("😀".to_string()),
            4 => Just("\r\n".to_string()),
            2 => Just("\n".to_string()),
            1 => Just("\r".to_string()),
            2 => Just("é".to_string()),
            1 => Just("𝄞".to_string()),
            4 => "[a-z ]{1,3}",
            1 => "\\PC{1,3}",
        ],
        0..40,
    )
    .prop_map(|v| v.concat())
}

fn on_lf_of_crlf(src: &str, offset: usize) -> bool {
    let mut o = offset.min(src.len());
    while !src.is_char_boundary(o) {
        o -= 1;
    }
    o > 0 && src.as_bytes().get(o) == Some(&b'\n') && src.as_bytes()[o - 1] == b'\r'
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]

    #[test]
    fn char_and_utf16_positions_agree_except_between_cr_and_lf(src in mixed_text()) {
        let (g, l) = (Grammar::new(&src), Lsp::new(&src));
        prop_assert_eq!(g.line_count(), l.line_count());
        for off in 0..=src.len() + 2 {
            if on_lf_of_crlf(&src, off) {
                continue;
            }
            let chars = g.line_col(off, ColumnUnit::Chars);
            let (line0, col0) = l.char_position(off);
            prop_assert_eq!((chars.line - 1, chars.column - 1), (line0 as usize, col0 as usize), "chars at {} in {:?}", off, src);
            let utf16 = g.line_col(off, ColumnUnit::Utf16);
            let p = l.position(off);
            prop_assert_eq!((utf16.line - 1, utf16.column - 1), (p.line as usize, p.character as usize), "utf16 at {} in {:?}", off, src);
        }
    }

    #[test]
    fn inverse_agrees_on_line_interiors(src in mixed_text()) {
        let (g, l) = (Grammar::new(&src), Lsp::new(&src));
        for off in 0..=src.len() {
            if !src.is_char_boundary(off) || on_lf_of_crlf(&src, off) {
                continue;
            }
            let at = g.line_col(off, ColumnUnit::Chars);
            // the LSP index excludes the '\r' of a CRLF from its addressable columns: skip them
            if src.as_bytes().get(off) == Some(&b'\r') && src.as_bytes().get(off + 1) == Some(&b'\n') {
                continue;
            }
            prop_assert_eq!(g.offset(at.line, at.column, ColumnUnit::Chars), l.offset_of_char(at.line as u32 - 1, at.column as u32 - 1));
        }
    }
}

#[test]
fn the_documented_difference_exists_and_is_exactly_this() {
    let src = "ab\r\ncd";
    let (g, l) = (Grammar::new(src), Lsp::new(src));
    assert_eq!(g.line_col(3, ColumnUnit::Chars).column, 4);
    assert_eq!(l.char_position(3), (0, 2));
}
