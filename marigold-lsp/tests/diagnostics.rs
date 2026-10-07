use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position};
use marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES as MAX_INPUT_BYTES;
use marigold_lsp::lsp_diagnostics;
use proptest::prelude::*;

fn utf16_lines(text: &str) -> Vec<usize> {
    let mut widths = vec![0usize];
    let mut chars = text.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\n' => widths.push(0),
            '\r' if chars.peek() == Some(&'\n') => {
                chars.next();
                widths.push(0);
            }
            c => *widths.last_mut().unwrap() += c.len_utf16(),
        }
    }
    widths
}

fn assert_within(text: &str, diags: &[Diagnostic]) -> Result<(), TestCaseError> {
    let widths = utf16_lines(text);
    let check = |p: Position| -> Result<(), TestCaseError> {
        prop_assert!((p.line as usize) < widths.len(), "line {p:?} of {text:?}");
        prop_assert!(
            (p.character as usize) <= widths[p.line as usize],
            "character {p:?} of {text:?}"
        );
        Ok(())
    };
    for d in diags {
        prop_assert!(
            (d.range.start.line, d.range.start.character)
                <= (d.range.end.line, d.range.end.character)
        );
        check(d.range.start)?;
        check(d.range.end)?;
    }
    Ok(())
}

const VALID: &[&str] = &[
    "range(0, 10).return",
    "range(0, 10).map(add_one).return",
    "enum Colour { Red, Green }\nrange(0, 3).return",
    "range(0, 1).return\r\nrange(0, 2).return\r\n",
];

fn mutated() -> impl Strategy<Value = String> {
    (
        proptest::sample::select(VALID),
        any::<prop::sample::Index>(),
        "(😀|é|𝄞|\r\n|\n|[a-z(). ]){0,4}",
        0usize..3,
    )
        .prop_map(|(base, idx, insert, kind)| {
            let boundaries: Vec<usize> = (0..=base.len())
                .filter(|i| base.is_char_boundary(*i))
                .collect();
            let at = boundaries[idx.index(boundaries.len())];
            let mut s = base.to_string();
            match kind {
                0 => s.insert_str(at, &insert),
                1 => s.truncate(at),
                _ => {
                    let end = boundaries.iter().copied().find(|b| *b > at).unwrap_or(at);
                    s.replace_range(at..end, &insert);
                }
            }
            s
        })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2000))]

    #[test]
    fn lsp_diagnostics_ranges_within_document(text in prop_oneof![any::<String>(), "(\\PC|😀|\r\n|\n){0,60}".prop_map(String::from), mutated()]) {
        assert_within(&text, &lsp_diagnostics(&text))?;
    }
}

#[test]
fn error_after_emoji_and_crlf_uses_utf16_columns() {
    let diags = lsp_diagnostics("range(0, 1).return\r\nrange(Colour).return\r\n");
    let [d] = diags.as_slice() else {
        panic!("{diags:?}")
    };
    assert_eq!(d.range.start, Position::new(1, 6));
    assert_eq!(d.range.end, Position::new(1, 12));
}

#[test]
fn error_after_emoji_on_same_line_counts_surrogate_pairs() {
    let diags = lsp_diagnostics("range(0, 1).return 😀😀");
    let [d] = diags.as_slice() else {
        panic!("{diags:?}")
    };
    assert_eq!(d.range.start, Position::new(0, 19));
    assert_eq!(d.range.end, Position::new(0, 21));
}

#[test]
fn error_after_crlf_lines_uses_utf16_columns() {
    let diags =
        lsp_diagnostics("range(0, 1).return\r\nrange(0, 1).return\r\n😀😀range(0, 1).return");
    let [d] = diags.as_slice() else {
        panic!("{diags:?}")
    };
    assert_eq!(d.range.start, Position::new(2, 0));
    assert_eq!(d.range.end, Position::new(2, 2));
}

#[test]
fn error_severity_maps_to_lsp_error() {
    let diags = lsp_diagnostics("range(Colour).return");
    assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
}

#[test]
fn help_text_is_appended_to_message() {
    let diags = lsp_diagnostics("range(Colour).return");
    assert!(diags[0].message.contains("no enums are declared"));
    if diags[0].message.contains("\nhelp: ") {
        assert!(!diags[0].message.ends_with("help: "));
    }
}

#[test]
fn input_at_cap_is_parsed_and_above_cap_is_rejected() {
    let at_cap = " ".repeat(MAX_INPUT_BYTES);
    assert!(lsp_diagnostics(&at_cap)
        .iter()
        .all(|d| d.code != Some(NumberOrString::String("input-too-large".into()))));

    let over = " ".repeat(MAX_INPUT_BYTES + 1);
    let diags = lsp_diagnostics(&over);
    let [d] = diags.as_slice() else {
        panic!("{} diagnostics", diags.len())
    };
    assert_eq!(
        d.code,
        Some(NumberOrString::String("input-too-large".into()))
    );
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(d.range.start, Position::new(0, 0));
}
