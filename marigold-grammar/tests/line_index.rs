//! Integration tests for `marigold_grammar::line_index` (issue #400, task T3).
//!
//! Oracles used here:
//! * `naive`      -- a deliberately different algorithm (one pass over the chars, no index);
//! * `old_line_col` -- verbatim copy of `marigold/src/main.rs::line_col` as it was on main@2343ee8;
//! * `rustc`      -- the real compiler's `--error-format=json` line/column/byte fields.
//!
//! Binding semantics (also in the module docs):
//! * lines and columns are 1-based; only `\n` ends a line (`\r` is an ordinary column);
//! * a column counts `Bytes`, `Chars` (Unicode scalar values, rustc's unit) or `Utf16` code units
//!   from the start of the line;
//! * `line_col` is total: offsets past the end clamp to the end, offsets inside a multibyte
//!   char floor to the char start, and the byte after a `\r` is NOT special (no CRLF snapping);
//! * `offset` is the exact inverse on char boundaries; it answers `None` instead of guessing.

use marigold_grammar::line_index::{ColumnUnit, LineCol, LineIndex};
use proptest::prelude::*;

const UNITS: [ColumnUnit; 3] = [ColumnUnit::Bytes, ColumnUnit::Chars, ColumnUnit::Utf16];

fn lc(line: usize, col: usize) -> LineCol {
    LineCol { line, column: col }
}

fn width(c: char, unit: ColumnUnit) -> usize {
    match unit {
        ColumnUnit::Bytes => c.len_utf8(),
        ColumnUnit::Chars => 1,
        ColumnUnit::Utf16 => c.len_utf16(),
        _ => unreachable!("new unit: extend the oracle"),
    }
}

/// Independent oracle: walk the chars once, no index, no slicing.
fn naive(text: &str, offset: usize, unit: ColumnUnit) -> LineCol {
    let (mut line, mut col) = (1, 1);
    for (i, c) in text.char_indices() {
        // `i + len_utf8 > offset` means the char contains or follows `offset`: stop (floor).
        if i + c.len_utf8() > offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += width(c, unit);
        }
    }
    lc(line, col)
}

/// Verbatim `main.rs::line_col` (0-based nothing: returns 1-based (line, char column)).
fn old_line_col(src: &str, offset: usize) -> (usize, usize) {
    let mut end = offset.min(src.len());
    while !src.is_char_boundary(end) {
        end -= 1;
    }
    let before = &src[..end];
    let line = before.matches('\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let col = before[line_start..].chars().count() + 1;
    (line, col)
}

fn boundaries(text: &str) -> Vec<usize> {
    (0..=text.len())
        .filter(|i| text.is_char_boundary(*i))
        .collect()
}

// ---------------------------------------------------------------------------------------------
// hand-computed goldens
// ---------------------------------------------------------------------------------------------

#[test]
fn ascii_goldens() {
    let idx = LineIndex::new("ab\ncd\n");
    for (off, want) in [
        (0, lc(1, 1)),
        (1, lc(1, 2)),
        (2, lc(1, 3)), // the '\n' itself belongs to line 1, one past the last column
        (3, lc(2, 1)),
        (5, lc(2, 3)),
        (6, lc(3, 1)), // EOF after a trailing newline is the start of an empty last line
    ] {
        for unit in UNITS {
            assert_eq!(idx.line_col(off, unit), want, "offset {off} {unit:?}");
        }
    }
    assert_eq!(idx.line_count(), 3);
}

#[test]
fn units_differ_exactly_where_expected() {
    // 'é' = 2 bytes / 1 char / 1 utf16; '𝄞' = 4 bytes / 1 char / 2 utf16; '日' = 3 / 1 / 1
    let src = "é𝄞日x\nÿy";
    let idx = LineIndex::new(src);
    let x = src.find('x').unwrap();
    assert_eq!(x, 9);
    assert_eq!(idx.line_col(x, ColumnUnit::Bytes), lc(1, 10));
    assert_eq!(idx.line_col(x, ColumnUnit::Chars), lc(1, 4));
    assert_eq!(idx.line_col(x, ColumnUnit::Utf16), lc(1, 5));
    let y = src.find('y').unwrap();
    assert_eq!(idx.line_col(y, ColumnUnit::Bytes), lc(2, 3));
    assert_eq!(idx.line_col(y, ColumnUnit::Chars), lc(2, 2));
    assert_eq!(idx.line_col(y, ColumnUnit::Utf16), lc(2, 2));
}

#[test]
fn crlf_is_not_special_cased() {
    // Binding: only '\n' ends a line; '\r' is a column; the '\n' of a CRLF is addressable.
    let src = "ab\r\ncd";
    let idx = LineIndex::new(src);
    assert_eq!(idx.line_col(2, ColumnUnit::Chars), lc(1, 3)); // the '\r'
    assert_eq!(idx.line_col(3, ColumnUnit::Chars), lc(1, 4)); // the '\n' (LSP's LineIndex would say col 3)
    assert_eq!(idx.line_col(4, ColumnUnit::Chars), lc(2, 1));
    assert_eq!(idx.offset(1, 4, ColumnUnit::Chars), Some(3));
    assert_eq!(idx.offset(1, 5, ColumnUnit::Chars), None);
    assert_eq!(idx.line_count(), 2);
}

#[test]
fn lone_cr_does_not_start_a_line() {
    let idx = LineIndex::new("a\rb\rc");
    assert_eq!(idx.line_count(), 1);
    assert_eq!(idx.line_col(4, ColumnUnit::Chars), lc(1, 5));
}

#[test]
fn empty_text() {
    let idx = LineIndex::new("");
    assert_eq!(idx.line_count(), 1);
    for unit in UNITS {
        assert_eq!(idx.line_col(0, unit), lc(1, 1));
        assert_eq!(idx.line_col(99, unit), lc(1, 1));
        assert_eq!(idx.line_col(usize::MAX, unit), lc(1, 1));
        assert_eq!(idx.offset(1, 1, unit), Some(0));
        assert_eq!(idx.offset(1, 2, unit), None);
        assert_eq!(idx.offset(2, 1, unit), None);
    }
}

#[test]
fn only_newlines() {
    let idx = LineIndex::new("\n\n\n");
    assert_eq!(idx.line_count(), 4);
    assert_eq!(idx.line_col(0, ColumnUnit::Chars), lc(1, 1));
    assert_eq!(idx.line_col(1, ColumnUnit::Chars), lc(2, 1));
    assert_eq!(idx.line_col(3, ColumnUnit::Chars), lc(4, 1));
    assert_eq!(idx.offset(4, 1, ColumnUnit::Chars), Some(3));
}

#[test]
fn offsets_inside_a_char_floor_to_the_char_start() {
    let src = "a😀b";
    let idx = LineIndex::new(src);
    for mid in 2..=4 {
        assert_eq!(idx.line_col(mid, ColumnUnit::Chars), lc(1, 2), "{mid}");
        assert_eq!(idx.line_col(mid, ColumnUnit::Bytes), lc(1, 2), "{mid}");
        assert_eq!(idx.line_col(mid, ColumnUnit::Utf16), lc(1, 2), "{mid}");
    }
    assert_eq!(idx.line_col(5, ColumnUnit::Chars), lc(1, 3));
    assert_eq!(idx.line_col(5, ColumnUnit::Utf16), lc(1, 4));
}

#[test]
fn offsets_past_the_end_clamp_to_the_end() {
    let idx = LineIndex::new("a\nbc");
    for unit in UNITS {
        assert_eq!(idx.line_col(4, unit), lc(2, 3));
        assert_eq!(idx.line_col(5, unit), lc(2, 3));
        assert_eq!(idx.line_col(usize::MAX, unit), lc(2, 3));
    }
}

#[test]
fn offset_rejects_out_of_range_and_misaligned_positions() {
    let src = "a😀b\nxy";
    let idx = LineIndex::new(src);
    for unit in UNITS {
        assert_eq!(idx.offset(0, 1, unit), None, "line 0 {unit:?}");
        assert_eq!(idx.offset(1, 0, unit), None, "col 0 {unit:?}");
        assert_eq!(idx.offset(3, 1, unit), None, "line past the end {unit:?}");
        assert_eq!(idx.offset(usize::MAX, 1, unit), None);
        assert_eq!(idx.offset(1, usize::MAX, unit), None);
        assert_eq!(idx.offset(usize::MAX, usize::MAX, unit), None);
        assert_eq!(idx.offset(2, 4, unit), None, "col past end+1 {unit:?}");
        assert_eq!(idx.offset(2, 3, unit), Some(src.len()), "end of last line");
    }
    // line 1 is "a😀b": 1 + 4 + 1 bytes, 3 chars, 4 utf16 units
    assert_eq!(idx.offset(1, 3, ColumnUnit::Chars), Some(5)); // 'b'
    assert_eq!(idx.offset(1, 4, ColumnUnit::Chars), Some(6)); // the '\n'
    assert_eq!(idx.offset(1, 5, ColumnUnit::Chars), None);
    // column 3 in utf16 / 3..5 in bytes fall inside the emoji: no such position
    assert_eq!(idx.offset(1, 3, ColumnUnit::Utf16), None);
    assert_eq!(idx.offset(1, 4, ColumnUnit::Utf16), Some(5));
    for col in 3..=5 {
        assert_eq!(
            idx.offset(1, col, ColumnUnit::Bytes),
            None,
            "byte col {col}"
        );
    }
    assert_eq!(idx.offset(1, 6, ColumnUnit::Bytes), Some(5));
}

#[test]
fn migrated_cases_from_main_rs_line_col_tests() {
    // The four `line_col_tests` that live in marigold/src/main.rs on main@2343ee8; they move here
    // when `line_col` is deleted, with identical expected values.
    assert_eq!(
        LineIndex::new("abc").line_col(0, ColumnUnit::Chars),
        lc(1, 1)
    );
    assert_eq!(
        LineIndex::new("ab\ncd").line_col(4, ColumnUnit::Chars),
        lc(2, 2)
    );
    let src = "\"ünï\" x";
    assert_eq!(
        LineIndex::new(src).line_col(src.find('x').unwrap(), ColumnUnit::Chars),
        lc(1, 7)
    );
    assert_eq!(
        LineIndex::new("a\nb").line_col(99, ColumnUnit::Chars),
        lc(2, 2)
    );
    assert_eq!(LineIndex::new("ü").line_col(1, ColumnUnit::Chars), lc(1, 1));
}

#[test]
fn linecol_orders_by_line_then_column() {
    assert!(lc(1, 99) < lc(2, 1));
    assert!(lc(2, 1) < lc(2, 2));
}

// ---------------------------------------------------------------------------------------------
// properties
// ---------------------------------------------------------------------------------------------

fn mixed_text() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        prop_oneof![
            4 => Just("😀".to_string()),
            4 => Just("\r\n".to_string()),
            3 => Just("\n".to_string()),
            1 => Just("\r".to_string()),
            2 => Just("é".to_string()),
            2 => Just("日本".to_string()),
            1 => Just("𝄞".to_string()),
            1 => Just("\u{feff}".to_string()),
            1 => Just("\u{2028}".to_string()),
            6 => "[a-z ]{1,4}",
            1 => "\\PC{1,3}",
        ],
        0..40,
    )
    .prop_map(|v| v.concat())
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(400))]

    #[test]
    fn line_col_matches_the_naive_oracle_for_every_offset(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for off in 0..=src.len() + 3 {
            for unit in UNITS {
                prop_assert_eq!(idx.line_col(off, unit), naive(&src, off, unit), "off {} {:?} in {:?}", off, unit, src);
            }
        }
        for unit in UNITS {
            prop_assert_eq!(idx.line_col(usize::MAX, unit), naive(&src, src.len(), unit));
        }
    }

    #[test]
    fn chars_agree_with_the_old_main_rs_line_col(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for off in 0..=src.len() + 3 {
            let got = idx.line_col(off, ColumnUnit::Chars);
            prop_assert_eq!((got.line, got.column), old_line_col(&src, off), "off {} in {:?}", off, src);
        }
    }

    #[test]
    fn offset_inverts_line_col_on_every_char_boundary(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for b in boundaries(&src) {
            for unit in UNITS {
                let at = idx.line_col(b, unit);
                prop_assert_eq!(idx.offset(at.line, at.column, unit), Some(b), "{:?} {:?} {:?}", b, unit, src);
            }
        }
    }

    #[test]
    fn offset_is_defined_exactly_on_positions_line_col_produces(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for unit in UNITS {
            let reachable: std::collections::BTreeSet<_> =
                boundaries(&src).into_iter().map(|b| idx.line_col(b, unit)).collect();
            for line in 0..=idx.line_count() + 1 {
                for col in 0..=src.len() + 3 {
                    let got = idx.offset(line, col, unit);
                    prop_assert_eq!(got.is_some(), reachable.contains(&lc(line, col)), "({}, {}) {:?} {:?}", line, col, unit, src);
                    if let Some(o) = got {
                        prop_assert!(src.is_char_boundary(o) && o <= src.len());
                        prop_assert_eq!(idx.line_col(o, unit), lc(line, col));
                    }
                }
            }
        }
    }

    #[test]
    fn line_col_is_monotonic_in_the_offset(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for unit in UNITS {
            let mut prev = lc(1, 1);
            for off in 0..=src.len() + 1 {
                let cur = idx.line_col(off, unit);
                prop_assert!(prev <= cur, "{:?} then {:?} at {} in {:?}", prev, cur, off, src);
                prev = cur;
            }
        }
    }

    #[test]
    fn units_are_consistently_related(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for off in 0..=src.len() {
            let b = idx.line_col(off, ColumnUnit::Bytes);
            let c = idx.line_col(off, ColumnUnit::Chars);
            let u = idx.line_col(off, ColumnUnit::Utf16);
            prop_assert_eq!(b.line, c.line);
            prop_assert_eq!(u.line, c.line);
            prop_assert!(c.column <= u.column && u.column <= b.column, "{:?} {:?} {:?}", c, u, b);
        }
    }

    #[test]
    fn line_count_is_newlines_plus_one(src in mixed_text()) {
        prop_assert_eq!(LineIndex::new(&src).line_count(), src.matches('\n').count() + 1);
    }

    #[test]
    fn nothing_panics_for_arbitrary_arguments(
        src in mixed_text(),
        off in prop_oneof![0usize..300, Just(usize::MAX), Just(usize::MAX - 1)],
        line in prop_oneof![0usize..20, Just(usize::MAX)],
        col in prop_oneof![0usize..300, Just(usize::MAX)],
    ) {
        let idx = LineIndex::new(&src);
        for unit in UNITS {
            let _ = idx.line_col(off, unit);
            let _ = idx.offset(line, col, unit);
        }
    }
}

// ---------------------------------------------------------------------------------------------
// scale
// ---------------------------------------------------------------------------------------------

#[test]
fn many_lines_and_long_ascii_lines_are_cheap() {
    use std::time::{Duration, Instant};
    let budget = Duration::from_secs(5);
    // 200k lines: building is linear, a query is a binary search.
    let many_lines = "x\n".repeat(200_000);
    let idx = LineIndex::new(&many_lines);
    let t = Instant::now();
    for i in 0..200_000usize {
        assert_eq!(idx.line_col(i * 2, ColumnUnit::Chars), lc(i + 1, 1));
        assert!(
            i % 1000 != 0 || t.elapsed() < budget,
            "line lookups are not O(log n): {:?}",
            t.elapsed()
        );
    }

    // One 2 MB ASCII line, 200k column queries: must not rescan the line for each query
    // (a rescan is ~2e5 * 1e6 byte visits). The clock is checked inside the loop so a
    // quadratic implementation fails in seconds instead of minutes.
    let long_line = "a".repeat(2_000_000);
    let idx = LineIndex::new(&long_line);
    let t = Instant::now();
    for i in 0..200_000usize {
        assert_eq!(idx.line_col(i * 10, ColumnUnit::Chars), lc(1, i * 10 + 1));
        assert_eq!(idx.offset(1, i * 10 + 1, ColumnUnit::Utf16), Some(i * 10));
        assert!(
            i % 1000 != 0 || t.elapsed() < budget,
            "ASCII column lookups are not O(1): {:?}",
            t.elapsed()
        );
    }
}

// ---------------------------------------------------------------------------------------------
// external oracle: rustc
// ---------------------------------------------------------------------------------------------

/// rustc's JSON spans are the ground truth for "1-based Unicode scalar column + 0-based byte
/// offset of the on-disk file". Compile a file full of multibyte text, CRLFs and tabs, and
/// require `LineIndex` to reproduce every span's start and end. Skips (loudly) when `rustc` is
/// not on PATH; CI always has it.
#[test]
fn chars_columns_and_byte_offsets_agree_with_rustc() {
    let src = "fn main() {\r\n    let s = \"日本😀\"; let a: u32 = \"x\"; // é\r\n\tlet 😀b = 1;\r\n    let c: u8 = \"é\"; let d: u8 = 1u16;\r\n}\r\n";
    let dir =
        std::env::temp_dir().join(format!("marigold-line-index-rustc-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let file = dir.join("t.rs");
    std::fs::write(&file, src).unwrap();
    let out = match std::process::Command::new("rustc")
        .args([
            "--edition",
            "2021",
            "--error-format=json",
            "--emit=metadata",
            "-o",
        ])
        .arg(dir.join("t.rmeta"))
        .arg(&file)
        .output()
    {
        Ok(out) => out,
        Err(e) => {
            eprintln!("SKIPPED: rustc not runnable: {e}");
            return;
        }
    };
    let idx = LineIndex::new(src);
    let mut checked = 0;
    for line in String::from_utf8(out.stderr).unwrap().lines() {
        let Ok(diag) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        for span in diag["spans"].as_array().into_iter().flatten() {
            let n = |k: &str| span[k].as_u64().unwrap() as usize;
            let (bs, be) = (n("byte_start"), n("byte_end"));
            assert_eq!(
                idx.line_col(bs, ColumnUnit::Chars),
                lc(n("line_start"), n("column_start")),
                "start of {span}"
            );
            assert_eq!(
                idx.line_col(be, ColumnUnit::Chars),
                lc(n("line_end"), n("column_end")),
                "end of {span}"
            );
            assert_eq!(
                idx.offset(n("line_start"), n("column_start"), ColumnUnit::Chars),
                Some(bs)
            );
            assert_eq!(
                idx.offset(n("line_end"), n("column_end"), ColumnUnit::Chars),
                Some(be)
            );
            checked += 1;
        }
    }
    assert!(checked >= 6, "expected several rustc spans, got {checked}");
    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------------------------------
// line terminators, combining marks, serialization, line ranges
// ---------------------------------------------------------------------------------------------

#[test]
fn only_lf_ends_a_line_whatever_unicode_says() {
    // NEL, VT, FF, FS/GS/RS, LS, PS are all ordinary one-column characters.
    for c in [
        '\u{85}', '\u{b}', '\u{c}', '\u{1c}', '\u{1d}', '\u{1e}', '\u{2028}', '\u{2029}',
    ] {
        let src = format!("a{c}b\nc");
        let idx = LineIndex::new(&src);
        assert_eq!(idx.line_count(), 2, "{c:?}");
        let b = src.find('b').unwrap();
        for unit in [ColumnUnit::Chars, ColumnUnit::Utf16] {
            assert_eq!(idx.line_col(b, unit), lc(1, 3), "{c:?} {unit:?}");
        }
        assert_eq!(
            idx.offset(2, 1, ColumnUnit::Chars),
            Some(src.find('c').unwrap())
        );
    }
}

#[test]
fn a_combining_mark_is_a_column_of_its_own() {
    // No grapheme clustering: "e" + U+0301 are two scalar values.
    let src = "e\u{301}x";
    let idx = LineIndex::new(src);
    let x = src.find('x').unwrap();
    assert_eq!(idx.line_col(x, ColumnUnit::Chars), lc(1, 3));
    assert_eq!(idx.line_col(x, ColumnUnit::Utf16), lc(1, 3));
    assert_eq!(idx.line_col(x, ColumnUnit::Bytes), lc(1, 4));
    assert_eq!(idx.offset(1, 2, ColumnUnit::Chars), Some(1));
}

#[test]
fn positions_serialize_with_the_json_key_names_of_the_emit_document() {
    assert_eq!(
        serde_json::to_string(&lc(3, 7)).unwrap(),
        r#"{"line":3,"column":7}"#
    );
    let units: Vec<_> = UNITS
        .iter()
        .map(|u| serde_json::to_string(u).unwrap())
        .collect();
    assert_eq!(units, [r#""bytes""#, r#""chars""#, r#""utf16""#]);
}

#[test]
fn line_range_covers_a_line_without_its_newline() {
    let idx = LineIndex::new("ab\r\ncd\n\nlast");
    let range = |line| idx.line_range(line).map(|r| (r.start, r.end));
    assert_eq!(range(1), Some((0, 3))); // the '\r' belongs to the line
    assert_eq!(range(2), Some((4, 6)));
    assert_eq!(range(3), Some((7, 7))); // empty line
    assert_eq!(range(4), Some((8, 12))); // last line has no '\n'
    assert_eq!(range(0), None);
    assert_eq!(range(5), None);
    assert_eq!(range(usize::MAX), None);
    let idx = LineIndex::new("");
    assert_eq!(idx.line_range(1).map(|r| (r.start, r.end)), Some((0, 0)));
    let idx = LineIndex::new("a\n");
    assert_eq!(idx.line_range(2).map(|r| (r.start, r.end)), Some((2, 2)));
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(300))]

    #[test]
    fn line_range_agrees_with_offset_and_line_col(src in mixed_text()) {
        let idx = LineIndex::new(&src);
        for line in 1..=idx.line_count() {
            let r = idx.line_range(line).unwrap();
            prop_assert_eq!(idx.offset(line, 1, ColumnUnit::Chars), Some(r.start));
            prop_assert_eq!(idx.line_col(r.start, ColumnUnit::Chars), lc(line, 1));
            prop_assert_eq!(idx.line_col(r.end, ColumnUnit::Chars).line, line);
            prop_assert!(!src[r.start..r.end].contains('\n'));
            prop_assert!(r.end == src.len() || src.as_bytes()[r.end] == b'\n');
        }
    }
}

// ---------------------------------------------------------------------------------------------
// long non-ASCII lines (checkpoints)
// ---------------------------------------------------------------------------------------------

/// Lines much longer than the checkpoint stride, full of multibyte characters, so every
/// checkpoint and every stride boundary is exercised (the shorter `mixed_text` never reaches one).
fn long_line_text() -> impl Strategy<Value = String> {
    proptest::collection::vec(
        prop_oneof![
            30 => Just("a".to_string()),
            2 => Just("é".to_string()),
            2 => Just("日".to_string()),
            1 => Just("😀".to_string()),
            1 => Just("\n".to_string()),
        ],
        0..1500,
    )
    .prop_map(|v| v.concat())
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(60))]

    #[test]
    fn long_lines_match_the_naive_oracle_everywhere(src in long_line_text()) {
        let idx = LineIndex::new(&src);
        for b in boundaries(&src) {
            for unit in UNITS {
                let got = idx.line_col(b, unit);
                prop_assert_eq!(got, naive(&src, b, unit), "{:?} at {}", unit, b);
                prop_assert_eq!(idx.offset(got.line, got.column, unit), Some(b));
            }
        }
    }
}

#[test]
fn long_ascii_prefix_then_multibyte_text_on_the_same_line() {
    // Whether a line is pure ASCII must be decided over the whole line, not its first bytes.
    let texts = [
        // the first non-ASCII character is far past the first stride
        format!("{}é{}", "a".repeat(1000), "b".repeat(1000)),
        format!("{}😀", "a".repeat(600)),
        // an ASCII line, then a line whose first 300 bytes are ASCII, then ASCII again
        format!(
            "{}\n{}日{}\n{}",
            "x".repeat(400),
            "y".repeat(700),
            "z".repeat(700),
            "w".repeat(400)
        ),
        // non-ASCII only at the very end of a long line, before the newline
        format!("{}ü\n{}", "q".repeat(5000), "r".repeat(300)),
    ];
    for text in &texts {
        let idx = LineIndex::new(text);
        for b in boundaries(text) {
            for unit in UNITS {
                let got = idx.line_col(b, unit);
                assert_eq!(got, naive(text, b, unit), "{unit:?} at {b}");
                assert_eq!(
                    idx.offset(got.line, got.column, unit),
                    Some(b),
                    "{unit:?} at {b}"
                );
            }
        }
    }
}

#[test]
fn a_long_line_with_one_multibyte_character_is_cheap() {
    use std::time::{Duration, Instant};
    // 909 KB on one line and a single `é`: with a whole-text ASCII flag every query rescans the line
    // (17 s for the emit document of such a file). The clock is checked inside the loop.
    let line = format!("é{}", "a".repeat(909_000));
    let idx = LineIndex::new(&line);
    let budget = Duration::from_secs(10);
    let t = Instant::now();
    for i in 0..20_000usize {
        let b = 2 + (i * 45) % 909_000;
        let want = lc(1, b); // one `é` of two bytes: chars = bytes - 1
        assert_eq!(idx.line_col(b, ColumnUnit::Chars), want);
        assert_eq!(idx.line_col(b, ColumnUnit::Utf16), want);
        assert_eq!(idx.offset(1, b, ColumnUnit::Chars), Some(b));
        assert!(
            i % 500 != 0 || t.elapsed() < budget,
            "non-ASCII line lookups are not bounded: {:?}",
            t.elapsed()
        );
    }
}
