use lsp_types::Position;
use marigold_lsp::position::LineIndex;
use proptest::prelude::*;

#[test]
fn ascii_offsets_map_to_line_and_column() {
    let idx = LineIndex::new("ab\ncd\n");
    assert_eq!(idx.position(0), Position::new(0, 0));
    assert_eq!(idx.position(2), Position::new(0, 2));
    assert_eq!(idx.position(3), Position::new(1, 0));
    assert_eq!(idx.position(5), Position::new(1, 2));
    assert_eq!(idx.position(6), Position::new(2, 0));
}

#[test]
fn columns_count_utf16_code_units() {
    let src = "é𝄞x";
    let idx = LineIndex::new(src);
    assert_eq!(idx.position(src.find('𝄞').unwrap()), Position::new(0, 1));
    assert_eq!(idx.position(src.find('x').unwrap()), Position::new(0, 3));
}

#[test]
fn crlf_line_endings_start_new_lines() {
    let idx = LineIndex::new("a\r\nb");
    assert_eq!(idx.position(3), Position::new(1, 0));
}

#[test]
fn out_of_range_positions_clamp_to_the_end() {
    let src = "ab\ncd";
    let idx = LineIndex::new(src);
    assert_eq!(idx.offset(Position::new(9, 0)), src.len());
    assert_eq!(idx.offset(Position::new(0, 99)), 2);
}

proptest! {
    #[test]
    fn offsets_round_trip_through_positions(src in "(\\PC|\n|\r\n){0,60}", pick in any::<prop::sample::Index>()) {
        let idx = LineIndex::new(&src);
        let boundaries: Vec<usize> = (0..=src.len()).filter(|i| src.is_char_boundary(*i)).collect();
        let b = boundaries[pick.index(boundaries.len())];
        prop_assert_eq!(idx.offset(idx.position(b)), b);
    }

    #[test]
    fn offset_is_always_a_char_boundary(src in "(\\PC|\n){0,60}", line in 0u32..10, character in 0u32..80) {
        let idx = LineIndex::new(&src);
        let o = idx.offset(Position::new(line, character));
        prop_assert!(o <= src.len());
        prop_assert!(src.is_char_boundary(o));
    }
}
