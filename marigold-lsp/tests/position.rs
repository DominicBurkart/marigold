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

#[test]
fn position_never_panics_for_any_byte_offset() {
    for src in [
        "a😀b\r\nc é\r\n𝄞",
        "é",
        "😀",
        "\r\n",
        "\r",
        "\n\n",
        "",
        "x\r",
    ] {
        let idx = LineIndex::new(src);
        for offset in 0..=src.len() + 5 {
            let _ = idx.position(offset);
        }
    }
}

#[test]
fn position_inside_surrogate_pair_rounds_down() {
    let src = "a😀b";
    let idx = LineIndex::new(src);
    for mid in 2..=4 {
        assert_eq!(idx.position(mid), Position::new(0, 1));
    }
    assert_eq!(idx.position(5), Position::new(0, 3));
    let e = LineIndex::new("é");
    assert_eq!(e.position(1), Position::new(0, 0));
}

#[test]
fn empty_document_position_zero() {
    let idx = LineIndex::new("");
    assert_eq!(idx.position(0), Position::new(0, 0));
    assert_eq!(idx.position(7), Position::new(0, 0));
    assert_eq!(idx.offset(Position::new(0, 0)), 0);
    assert_eq!(idx.offset(Position::new(3, 3)), 0);
}

#[test]
fn trailing_newline_position_at_len_is_next_line_start() {
    for src in ["ab\n", "ab\r\n"] {
        let idx = LineIndex::new(src);
        assert_eq!(idx.position(src.len()), Position::new(1, 0));
    }
}

#[test]
fn offset_on_crlf_line_end_never_passes_the_cr() {
    let src = "ab\r\ncd";
    let idx = LineIndex::new(src);
    assert_eq!(idx.position(2), Position::new(0, 2));
    assert_eq!(idx.position(3), Position::new(0, 2));
    assert_eq!(idx.position(4), Position::new(1, 0));
    assert_eq!(idx.offset(Position::new(0, 99)), 2);
}

fn mixed_text() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        prop_oneof![
            4 => Just("😀".to_string()),
            4 => Just("\r\n".to_string()),
            2 => Just("\n".to_string()),
            2 => Just("é".to_string()),
            1 => Just("𝄞".to_string()),
            4 => "[a-z ]{1,3}",
            1 => "\\PC{1,3}",
        ],
        0..40,
    )
    .prop_map(|v| v.concat())
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2000))]

    #[test]
    fn offsets_round_trip_through_positions(src in mixed_text(), pick in any::<prop::sample::Index>()) {
        let idx = LineIndex::new(&src);
        let boundaries: Vec<usize> = (0..=src.len())
            .filter(|i| src.is_char_boundary(*i))
            .filter(|i| !(src.as_bytes().get(*i) == Some(&b'\n') && *i > 0 && src.as_bytes()[*i - 1] == b'\r'))
            .collect();
        let b = boundaries[pick.index(boundaries.len())];
        prop_assert_eq!(idx.offset(idx.position(b)), b);
    }

    #[test]
    fn position_never_panics_and_is_monotonic(src in mixed_text(), offset in 0usize..400) {
        let idx = LineIndex::new(&src);
        let p = idx.position(offset);
        let q = idx.position(offset + 1);
        prop_assert!((p.line, p.character) <= (q.line, q.character));
    }

    #[test]
    fn offset_is_always_a_char_boundary(src in mixed_text(), line in 0u32..10, character in 0u32..80) {
        let idx = LineIndex::new(&src);
        let o = idx.offset(Position::new(line, character));
        prop_assert!(o <= src.len());
        prop_assert!(src.is_char_boundary(o));
    }
}
