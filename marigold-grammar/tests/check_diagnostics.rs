use marigold_grammar::diagnostics::{Diagnostic, Severity};
use marigold_grammar::{marigold_check, marigold_parse};
use proptest::prelude::*;

fn only(diags: Vec<Diagnostic>) -> Diagnostic {
    assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
    diags.into_iter().next().unwrap()
}

#[test]
fn valid_program_has_no_diagnostics() {
    assert!(marigold_check("range(0, 10).return").is_empty());
}

#[test]
fn empty_program_has_no_diagnostics() {
    assert!(marigold_check("").is_empty());
}

#[test]
fn syntax_error_points_at_the_offending_line() {
    let src = "range(0, 1).return\nrange(0, 1).retur\n";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "syntax-error");
    assert_eq!(d.severity, Severity::Error);
    let second_line = src.find('\n').unwrap() + 1;
    assert!(
        d.range.start >= second_line,
        "range {:?} should be on line 2",
        d.range
    );
    assert!(d.range.end <= src.len());
    assert!(!d.message.is_empty());
}

#[test]
fn syntax_error_range_is_not_empty_when_inside_input() {
    let src = "range(0, 1).bogus";
    let d = only(marigold_check(src));
    assert!(d.range.start < src.len());
    assert!(d.range.end > d.range.start);
}

#[test]
fn syntax_error_at_end_of_input_is_in_bounds() {
    let src = "range(0, 1)";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "syntax-error");
    assert!(d.range.start <= d.range.end && d.range.end <= src.len());
}

fn slice<'a>(src: &'a str, d: &Diagnostic) -> &'a str {
    &src[d.range.start..d.range.end]
}

#[test]
fn undefined_enum_in_range() {
    let src = "range(Color).return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "undefined-enum");
    assert!(d.message.contains("Color"), "{}", d.message);
    assert_eq!(slice(src, &d), "Color");
    assert!(
        d.help.as_deref().unwrap().contains("no enums"),
        "{:?}",
        d.help
    );
}

#[test]
fn undefined_enum_help_lists_declared_enums() {
    let src = "enum Size { Small, Large }\nrange(Szie).return";
    let d = only(marigold_check(src));
    assert_eq!(slice(src, &d), "Szie");
    assert!(d.help.as_deref().unwrap().contains("Size"), "{:?}", d.help);
}

#[test]
fn every_undefined_enum_is_reported() {
    let src = "range(Color).return\nrange(Shape).return";
    let diags = marigold_check(src);
    let slices: Vec<_> = diags.iter().map(|d| slice(src, d)).collect();
    assert_eq!(slices, vec!["Color", "Shape"]);
}

#[test]
fn undefined_enum_in_stream_variable() {
    let src = "x = range(Color)\nx.return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "undefined-enum");
    assert_eq!(slice(src, &d), "Color");
}

#[test]
fn bounded_min_greater_than_max() {
    let src = "struct Test { field: int[10, 5] }\nrange(0, 1).return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "bounds-violation");
    assert!(d.message.contains("min") && d.message.contains("max"));
    assert_eq!(slice(src, &d), "int[10, 5]");
}

#[test]
fn bounded_uint_negative_min() {
    let src = "struct Test { field: uint[-1, 10] }\nrange(0, 1).return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "bounds-violation");
    assert!(d.message.contains("negative"), "{}", d.message);
    assert_eq!(slice(src, &d), "uint[-1, 10]");
}

#[test]
fn bounded_undefined_type() {
    let src = "struct Test { field: int[0, NonExistent.len()] }\nrange(0, 1).return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "undefined-type");
    assert!(d.message.contains("NonExistent"));
    assert_eq!(slice(src, &d), "NonExistent");
}

#[test]
fn bounded_errors_are_all_reported() {
    let src = "struct Test { a: int[10, 5], b: int[0, Missing.len()] }\nrange(0, 1).return";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    let mut found: Vec<_> = diags.iter().map(|d| (d.code, slice(src, d))).collect();
    found.sort();
    assert_eq!(
        found,
        vec![
            ("bounds-violation", "int[10, 5]"),
            ("undefined-type", "Missing")
        ]
    );
}

#[test]
fn cyclic_bounds_point_at_references() {
    let src = "struct T { a: int[0, b.max()], b: int[0, a.max()] }\nrange(0, 1).return";
    let diags = marigold_check(src);
    assert!(!diags.is_empty());
    for d in &diags {
        assert_eq!(d.code, "cyclic-bound");
        assert!(["a", "b"].contains(&slice(src, d)), "{d:?}");
    }
}

#[test]
fn unlocatable_errors_fall_back_to_whole_document() {
    let src = "struct T { a: int[0, 10 / 0] }\nrange(0, 1).return";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "bound-division-by-zero");
    assert_eq!((d.range.start, d.range.end), (0, src.len()));
}

#[test]
fn diagnostics_are_not_duplicated() {
    let src = "struct T { a: int[0, Missing.len()], b: int[0, Missing.max()] }\nrange(0, 1).return";
    let diags = marigold_check(src);
    let slices: Vec<_> = diags.iter().map(|d| (d.range.start, d.range.end)).collect();
    let mut deduped = slices.clone();
    deduped.dedup();
    assert_eq!(slices, deduped, "{diags:?}");
    assert_eq!(diags.len(), 2, "{diags:?}");
}

#[test]
fn diagnostics_serialize_to_stable_json() {
    let d = only(marigold_check("range(Color).return"));
    let v = serde_json::to_value(&d).unwrap();
    assert_eq!(v["code"], "undefined-enum");
    assert_eq!(v["severity"], "error");
    assert!(v["range"]["start"].is_u64());
    assert!(v["range"]["end"].is_u64());
    assert!(v["message"].is_string());
}

const VALID_PROGRAMS: &[&str] = &[
    "range(0, 10).return",
    "x = range(0, 5).map(f)\nx.filter(g).return",
    "enum Color { Red, Green }\nrange(Color).return",
    "struct Test { value: int[0, 100] }\nrange(0, 1).return",
    "range(0, 4).permutations(2).return",
    "struct T { a: int[0, b.max()], b: uint[1, 10] }\nrange(0, 1).return",
    "enum Color { Red, Green }\nstruct P { c: int[0, Color.len()] }\nx = range(Color)\nx.return",
    "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
    "range(0, 1).write_file(\"ünïcødé.csv\", csv)",
];

fn has_error(diags: &[Diagnostic]) -> bool {
    diags.iter().any(|d| d.severity == Severity::Error)
}

fn assert_ranges_valid(src: &str, diags: &[Diagnostic]) {
    for d in diags {
        assert!(d.range.start <= d.range.end, "{d:?}");
        assert!(d.range.end <= src.len(), "{d:?} len={}", src.len());
        assert!(src.is_char_boundary(d.range.start), "{d:?}");
        assert!(src.is_char_boundary(d.range.end), "{d:?}");
    }
}

proptest! {
    #[test]
    fn ranges_are_in_bounds_for_arbitrary_input(src in "\\PC{0,80}") {
        let diags = marigold_check(&src);
        assert_ranges_valid(&src, &diags);
    }

    #[test]
    fn check_agrees_with_parse(src in "\\PC{0,80}") {
        prop_assert_eq!(has_error(&marigold_check(&src)), marigold_parse(&src).is_err());
    }

    #[test]
    fn mutated_programs_have_valid_ranges_and_agree_with_parse(
        idx in 0..VALID_PROGRAMS.len(),
        at in any::<prop::sample::Index>(),
        insert in prop::option::of("[\\PC]"),
    ) {
        let base = VALID_PROGRAMS[idx];
        let boundaries: Vec<usize> = (0..=base.len()).filter(|i| base.is_char_boundary(*i)).collect();
        let pos = boundaries[at.index(boundaries.len())];
        let mut src = base.to_string();
        match insert {
            Some(s) => src.insert_str(pos, &s),
            None => {
                if pos < src.len() {
                    src.remove(pos);
                }
            }
        }
        let diags = marigold_check(&src);
        assert_ranges_valid(&src, &diags);
        prop_assert_eq!(has_error(&diags), marigold_parse(&src).is_err());
    }
}

#[test]
fn valid_fixtures_have_no_diagnostics() {
    for src in VALID_PROGRAMS {
        assert!(
            marigold_parse(src).is_ok(),
            "fixture should be valid: {src}"
        );
        assert!(marigold_check(src).is_empty(), "{src}");
    }
}

#[test]
fn severity_has_stable_labels_and_error_predicate() {
    assert_eq!(Severity::Error.as_str(), "error");
    let d = only(marigold_check("range(Color).return"));
    assert!(d.is_error());
}

#[test]
fn oversized_input_is_rejected_with_a_single_bounded_diagnostic() {
    let src = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES + 1);
    let d = only(marigold_check(&src));
    assert_eq!(d.code, "input-too-large");
    assert!(d.is_error());
    assert_eq!((d.range.start, d.range.end), (0, src.len()));
    assert!(d.message.contains("10"), "{}", d.message);
}

#[test]
fn input_at_the_size_limit_is_still_checked() {
    let src = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES);
    assert!(marigold_check(&src).is_empty());
}

fn line_ranges(src: &str) -> Vec<(usize, usize)> {
    let mut out = Vec::new();
    let mut start = 0;
    for line in src.split_inclusive('\n') {
        out.push((start, start + line.len()));
        start += line.len();
    }
    out
}

fn assert_ordered_disjoint(diags: &[Diagnostic]) {
    for pair in diags.windows(2) {
        assert!(pair[0].range.end <= pair[1].range.start, "{pair:?}");
    }
}

#[test]
fn three_independent_syntax_errors_yield_three_ordered_ranges() {
    let src = "range(0, 1).retur\nrange(0, 1).return\nrange(0, 2).bogus\nrange(0, 3).retur\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert!(diags.iter().all(|d| d.code == "syntax-error"));
    assert_ordered_disjoint(&diags);
    let lines = line_ranges(src);
    for (d, line) in diags.iter().zip([0usize, 2, 3]) {
        let (s, e) = lines[line];
        assert!(d.range.start >= s && d.range.end <= e, "{d:?} line {line}");
    }
}

#[test]
fn unicode_errors_across_lines_are_recovered_on_char_boundaries() {
    let src = "range(0, 1).write_file(\"\u{fc}n\u{ef}.csv\", csv)\nrange(0, 1).ret\u{fc}r\nrange(0, 1).return\nrange(0, 1).\u{e9}\u{e9}\u{e9}\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    assert_ordered_disjoint(&diags);
    assert_ranges_valid(src, &diags);
    let lines = line_ranges(src);
    assert!(diags[0].range.start >= lines[1].0 && diags[0].range.end <= lines[1].1);
    assert!(diags[1].range.start >= lines[3].0 && diags[1].range.end <= lines[3].1);
}

#[test]
fn multi_line_expression_is_not_split() {
    let src = "range(0, 1)\n  .map(f)\n  .return\nrange(0, 1).bogus\nrange(0, 2).bogus\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    let lines = line_ranges(src);
    assert!(diags[0].range.start >= lines[3].0);
    assert!(diags[1].range.start >= lines[4].0);
}

#[test]
fn braces_and_strings_in_fn_bodies_do_not_split_chunks() {
    let src = "fn f(x: i32) -> i32 {\nlet s = \"}\n\";\nlet c = '\"';\nx\n}\nrange(0, 1).bogus\nrange(0, 2).bogus\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    let fn_end = src.find("range").unwrap();
    assert!(diags.iter().all(|d| d.range.start >= fn_end), "{diags:?}");
}

#[test]
fn recovery_does_not_flag_valid_neighbours() {
    let src = "x = range(0, 5).map(f)\nrange(0, 1).retur\nx.filter(g).return\n";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "syntax-error");
    let lines = line_ranges(src);
    assert!(d.range.start >= lines[1].0 && d.range.end <= lines[1].1);
}

#[test]
fn error_at_end_of_a_chunk_stays_inside_that_chunk() {
    let src = "range(0, 1)\nrange(0, 2)\nrange(0, 3).return\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    assert_ordered_disjoint(&diags);
    let lines = line_ranges(src);
    assert!(diags[0].range.end < lines[0].1);
    assert!(diags[1].range.end < lines[1].1);
}

proptest! {
    #[test]
    fn multi_error_programs_have_ordered_disjoint_ranges(
        picks in prop::collection::vec(0..VALID_PROGRAMS.len(), 2..5),
        cuts in prop::collection::vec((any::<prop::sample::Index>(), prop::option::of("[\\PC]")), 1..4),
    ) {
        let mut parts: Vec<String> = picks.iter().map(|i| VALID_PROGRAMS[*i].to_string()).collect();
        for (at, insert) in cuts {
            let n = at.index(parts.len());
            let part = &mut parts[n];
            let boundaries: Vec<usize> = (0..=part.len()).filter(|i| part.is_char_boundary(*i)).collect();
            let pos = boundaries[(n * 7 + part.len()) % boundaries.len()];
            match insert {
                Some(s) => part.insert_str(pos, &s),
                None => {
                    if pos < part.len() {
                        part.remove(pos);
                    }
                }
            }
        }
        let src = parts.join("\n");
        let diags = marigold_check(&src);
        assert_ranges_valid(&src, &diags);
        prop_assert_eq!(has_error(&diags), marigold_parse(&src).is_err());
        if diags.iter().all(|d| d.code == "syntax-error") {
            for pair in diags.windows(2) {
                prop_assert!(pair[0].range.end <= pair[1].range.start, "{:?}", diags);
            }
        }
    }
}
