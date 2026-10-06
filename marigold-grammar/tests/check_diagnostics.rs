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
fn valid_fixtures_have_no_error_diagnostics() {
    for src in VALID_PROGRAMS {
        assert!(
            marigold_parse(src).is_ok(),
            "fixture should be valid: {src}"
        );
        assert!(!has_error(&marigold_check(src)), "{src}");
    }
}

#[test]
fn fully_defined_fixtures_have_no_diagnostics_at_all() {
    for src in VALID_PROGRAMS.iter().filter(|s| !s.contains("(f)")) {
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

fn warnings(src: &str) -> Vec<Diagnostic> {
    let diags = marigold_check(src);
    assert!(!has_error(&diags), "{diags:?}");
    diags
}

fn warning(src: &str) -> Diagnostic {
    only(warnings(src))
}

#[test]
fn undefined_fn_in_map_is_information_with_range() {
    let src = "range(0, 5).map(doubel).return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-fn");
    assert_eq!(d.severity, Severity::Information);
    assert!(!d.is_error());
    assert!(
        d.message.contains("Rust item in scope inside m!()"),
        "{}",
        d.message
    );
    assert!(d
        .help
        .as_deref()
        .unwrap()
        .contains("Rust item in scope inside m!()"));
    assert_eq!(slice(src, &d), "doubel");
    assert!(d.message.contains("doubel"), "{}", d.message);
    assert!(
        d.help.as_deref().unwrap().contains("no functions"),
        "{:?}",
        d.help
    );
    assert!(marigold_parse(src).is_ok());
}

#[test]
fn undefined_fn_suggests_close_match_and_lists_declared() {
    let src = "fn double(x: i32) -> i32 { x * 2 }\nfn triple(x: i32) -> i32 { x * 3 }\nrange(0, 5).map(doubel).return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-fn");
    let help = d.help.unwrap();
    assert!(help.contains("did you mean 'double'?"), "{help}");
    assert!(help.contains("double, triple"), "{help}");
}

#[test]
fn undefined_fn_without_close_match_has_no_suggestion() {
    let src = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 5).map(completely_different).return";
    let help = warning(src).help.unwrap();
    assert!(!help.contains("did you mean"), "{help}");
    assert!(help.contains("double"), "{help}");
}

#[test]
fn every_fn_position_is_checked() {
    let src = "range(0, 5).filter(a).filter_map(b).fold(0, c).return\nrange(0, 5).keep_first_n(2, d).return";
    let diags = warnings(src);
    let names: Vec<_> = diags.iter().map(|d| slice(src, d)).collect();
    assert_eq!(names, vec!["a", "b", "c", "d"]);
    assert!(diags.iter().all(|d| d.code == "undefined-fn"));
}

#[test]
fn declared_fn_is_not_flagged_even_when_declared_later() {
    let src = "range(0, 3).map(double).return\nfn double(x: i32) -> i32 { x * 2 }";
    assert!(marigold_check(src).is_empty());
}

#[test]
fn undefined_stream_variable_in_stream_start() {
    let src = "x = range(0, 3)\nxs.return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-stream-variable");
    assert_eq!(d.severity, Severity::Information);
    assert_eq!(slice(src, &d), "xs");
    assert!(
        d.message.contains("Rust binding with a get() method"),
        "{}",
        d.message
    );
    let help = d.help.unwrap();
    assert!(help.contains("did you mean 'x'?"), "{help}");
    assert!(help.contains("defined stream variables: x"), "{help}");
    assert!(help.contains("Rust binding"), "{help}");
}

#[test]
fn undefined_name_used_as_a_stream_start_is_never_a_warning() {
    for src in [
        "x.return",
        "x.map(f).return",
        "other = range(0, 3)
x.return",
        "x.filter(f).return",
    ] {
        let diags = marigold_check(src);
        assert!(!has_error(&diags), "{src}: {diags:?}");
        let x = diags
            .iter()
            .find(|d| d.code == "undefined-stream-variable")
            .unwrap_or_else(|| panic!("{src}: {diags:?}"));
        assert_eq!(x.severity, Severity::Information, "{src}");
        assert!(
            diags.iter().all(|d| d.severity != Severity::Warning),
            "{src}: {diags:?}"
        );
        assert!(marigold_parse(src).is_ok(), "{src}");
    }
}

#[test]
fn undefined_stream_variable_in_variable_source() {
    let src = "a = range(0, 3)\nb = aa.filter(p)\nb.return";
    let diags = warnings(src);
    let codes: Vec<_> = diags.iter().map(|d| (d.code, slice(src, d))).collect();
    assert_eq!(
        codes,
        vec![("undefined-stream-variable", "aa"), ("undefined-fn", "p")]
    );
    assert!(diags.iter().all(|d| d.severity == Severity::Information));
}

#[test]
fn stream_variable_with_no_declarations_says_so() {
    let src = "nothing.return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-stream-variable");
    assert_eq!(d.severity, Severity::Information);
    assert!(d.help.unwrap().contains("no stream variables"));
}

#[test]
fn forward_reference_from_stream_to_later_variable_is_fine() {
    let src = "x.return\nx = range(0, 3)";
    assert!(marigold_check(src).is_empty());
    assert!(marigold_parse(src).is_ok());
    let code = marigold_parse(src).unwrap();
    assert!(code.find("let mut x").unwrap() < code.find("x.get()").unwrap());
}

#[test]
fn stream_use_before_and_after_declaration_agree() {
    let before = "x.return\nx = range(0, 3)";
    let after = "x = range(0, 3)\nx.return";
    assert!(marigold_check(before).is_empty());
    assert!(marigold_check(after).is_empty());
}

#[test]
fn variable_reading_later_variable_matches_what_codegen_cannot_compile() {
    let src = "b = a.filter(p)\na = range(0, 3)\nb.return";
    assert!(marigold_parse(src).is_ok());
    let code = marigold_parse(src).unwrap();
    assert!(code.find("let mut b").unwrap() < code.find("let mut a").unwrap());
    let d = &warnings(src)[0];
    assert_eq!(d.code, "undefined-stream-variable");
    assert_eq!(d.severity, Severity::Warning);
    assert!(d.message.contains("above"), "{}", d.message);
}

#[test]
fn forward_reference_from_variable_to_later_variable_warns() {
    let src = "b = a.filter(p)\na = range(0, 3)\nb.return";
    let diags = warnings(src);
    let found: Vec<_> = diags.iter().map(|d| (d.code, slice(src, d))).collect();
    assert_eq!(
        found,
        vec![("undefined-stream-variable", "a"), ("undefined-fn", "p")]
    );
    assert_eq!(diags[0].severity, Severity::Warning);
    assert_eq!(diags[1].severity, Severity::Information);
    assert!(
        diags[0].message.contains("declared"),
        "{}",
        diags[0].message
    );
}

#[test]
fn variable_referencing_itself_warns() {
    let src = "a = a.map(f)\na.return";
    let d = &warnings(src)[0];
    assert_eq!(d.code, "undefined-stream-variable");
    assert_eq!(d.severity, Severity::Warning);
    assert_eq!(slice(src, d), "a");
}

#[test]
fn undefined_struct_in_read_file() {
    let src = "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Rwo).return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-struct");
    assert_eq!(d.severity, Severity::Information);
    assert!(
        d.message.contains("Rust item in scope inside m!()"),
        "{}",
        d.message
    );
    assert!(d
        .help
        .as_deref()
        .unwrap()
        .contains("Rust item in scope inside m!()"));
    assert_eq!(slice(src, &d), "Rwo");
    let help = d.help.unwrap();
    assert!(help.contains("did you mean 'Row'?"), "{help}");
    assert!(help.contains("declared structs: Row"), "{help}");
}

#[test]
fn undefined_struct_with_no_structs_declared() {
    let src = "read_file(\"d.csv\", csv, struct=Row).return";
    let d = warning(src);
    assert_eq!(d.code, "undefined-struct");
    assert!(d.help.unwrap().contains("no structs"));
}

#[test]
fn declared_struct_is_not_flagged() {
    let src = "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Row).return";
    assert!(marigold_check(src).is_empty());
}

#[test]
fn enum_name_does_not_satisfy_a_struct_reference() {
    let src = "enum Row { A }\nread_file(\"d.csv\", csv, struct=Row).return";
    assert_eq!(warning(src).code, "undefined-struct");
}

#[test]
fn warnings_accompany_errors_in_source_order() {
    let src = "range(0, 5).map(f).return\nrange(Color).return";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    assert_eq!(diags[0].severity, Severity::Information);
    assert_eq!(diags[1].severity, Severity::Error);
    assert_ordered_disjoint(&diags);
}

#[test]
fn information_serializes_as_information() {
    let d = warning("range(0, 5).map(f).return");
    let v = serde_json::to_value(&d).unwrap();
    assert_eq!(v["severity"], "information");
    assert_eq!(v["code"], "undefined-fn");
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

fn corpus() -> Vec<std::path::PathBuf> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in [
        root.join("tests/programs"),
        root.join("../examples"),
        root.join(".."),
    ] {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let ext = path.extension().and_then(|e| e.to_str());
            if path.is_file() && matches!(ext, Some("marigold") | Some("mg")) {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

#[test]
fn corpus_is_not_empty() {
    assert!(corpus().len() >= 30, "{:?}", corpus());
}

#[test]
fn legitimate_programs_have_no_errors_or_warnings() {
    for path in corpus() {
        let src = std::fs::read_to_string(&path).unwrap();
        for d in marigold_check(&src) {
            assert!(
                d.severity == Severity::Information,
                "{} emitted {:?}[{}] at {:?}: {}",
                path.display(),
                d.severity,
                d.code,
                &src[d.range.start..d.range.end],
                d.message
            );
        }
    }
}

#[test]
fn use_before_declare_is_the_only_warning_source() {
    let sources = [
        "range(0, 5).map(f).filter(g).filter_map(h).fold(0, i).return\nrange(0, 5).keep_first_n(2, j).return",
        "read_file(\"d.csv\", csv, struct=Nope).return",
        "range(Nope).return",
        "enum E { A }\nrange(E).return",
        "b = a.map(f)\nb.return\nc.return",
        "x = x.map(f)\nx.return",
        "b = a.map(f)\na = range(0, 3)\nb.return",
    ];
    let mut warnings_seen = Vec::new();
    for src in sources {
        for d in marigold_check(src) {
            if d.severity == Severity::Warning {
                assert!(
                    d.message.contains("before it is declared"),
                    "{src}: {}",
                    d.message
                );
                warnings_seen.push((d.code, slice(src, &d).to_string()));
            } else if !d.is_error() {
                assert_eq!(d.severity, Severity::Information, "{src}: {d:?}");
            }
        }
    }
    assert_eq!(
        warnings_seen,
        vec![
            ("undefined-stream-variable", "x".to_string()),
            ("undefined-stream-variable", "a".to_string())
        ]
    );
}

#[test]
fn valid_fully_declared_program_has_zero_diagnostics() {
    let src = "enum Color { Red, Green }\nstruct Row { a: int[0, 5] }\nfn double(x: i32) -> i32 { x * 2 }\nfn big(x: &i32) -> bool { *x > 2 }\nxs = range(0, 5).map(double)\nxs.filter(big).return\nrange(Color).return\nread_file(\"d.csv\", csv, struct=Row).return";
    assert_eq!(marigold_check(src), Vec::<Diagnostic>::new());
}

#[test]
fn range_over_declared_enum_has_no_resolver_diagnostic() {
    assert!(marigold_check("enum Color { Red }\nrange(Color).return").is_empty());
}

#[test]
fn keyword_on_its_own_line_does_not_split_a_declaration() {
    let src = "fn\nf(x: i32) -> i32 { x }\nrange(0, 3).map(f).retur";
    let d = only(marigold_check(src));
    assert_eq!(d.code, "syntax-error");
    assert!(d.range.start >= src.rfind("range").unwrap(), "{d:?}");
}

#[test]
fn struct_and_enum_keywords_on_their_own_line_do_not_split() {
    let src = "struct\nRow { a: int[0, 5] }\nenum\nColor { Red }\nrange(0, 1).retur";
    let d = only(marigold_check(src));
    assert!(d.range.start >= src.rfind("range").unwrap(), "{d:?}");
}

#[test]
fn block_comments_in_fn_bodies_do_not_confuse_recovery() {
    let src = "fn f(x: i32) -> i32 {\n/* } \" ( */\nx\n}\nrange(0, 1).bogus\nrange(0, 2).bogus\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    let fn_end = src.find("range").unwrap();
    assert!(diags.iter().all(|d| d.range.start >= fn_end), "{diags:?}");
}

#[test]
fn escaped_quote_char_literal_in_fn_body_does_not_confuse_recovery() {
    let src = "fn f(c: char) -> bool {\nc == '\\''\n}\nrange(0, 1).bogus\nrange(0, 2).bogus\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    let fn_end = src.find("range").unwrap();
    assert!(diags.iter().all(|d| d.range.start >= fn_end), "{diags:?}");
}

#[test]
fn error_ranges_never_cover_trailing_whitespace() {
    let src = "range(0, 1).retur   \n\n\nrange(0, 2).retur \t \n\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 2, "{diags:?}");
    for d in &diags {
        let text = &src[d.range.start..d.range.end];
        assert_eq!(text, text.trim_end(), "{d:?}");
    }
}

#[test]
fn unbalanced_open_paren_swallows_later_errors_into_one_diagnostic() {
    let src = "range(0, 1.retur\nrange(0, 2).retur\nrange(0, 3).retur\n";
    let diags = marigold_check(src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(diags[0].code, "syntax-error");
}

fn timed<T>(f: impl FnOnce() -> T) -> (T, std::time::Duration) {
    let start = std::time::Instant::now();
    let out = f();
    (out, start.elapsed())
}

const BACKSTOP: std::time::Duration = std::time::Duration::from_secs(60);

fn best_of_three(src: &str) -> std::time::Duration {
    (0..3)
        .map(|_| {
            let (out, elapsed) = timed(|| marigold_check(src));
            std::hint::black_box(out);
            elapsed
        })
        .min()
        .unwrap()
}

fn assert_scales_linearly(build: impl Fn(usize) -> String) {
    let n = 5_000;
    let (small, large) = (build(n), build(2 * n));
    let t_small = best_of_three(&small);
    let t_large = best_of_three(&large);
    let allowed = t_small.mul_f64(3.5) + std::time::Duration::from_millis(100);
    assert!(
        t_large < allowed,
        "t({n})={t_small:?} t({})={t_large:?} allowed={allowed:?}",
        2 * n
    );
    assert!(t_large < BACKSTOP, "took {t_large:?}");
}

fn many_fns(n: usize) -> String {
    let mut src = String::new();
    for i in 0..n {
        src.push_str(&format!("fn f{i}(x: i32) -> i32 {{ x }}\n"));
    }
    for i in 0..n {
        src.push_str(&format!("range(0,3).map(g{i}).return\n"));
    }
    src
}

fn chained_variables(n: usize) -> String {
    let mut src = String::new();
    for i in 0..n {
        src.push_str(&format!("v{i} = range(0,3)\nv{} = v{i}.map(f{i})\n", i + 1));
    }
    src
}

#[test]
fn many_undefined_fns_against_many_declared_fns_scale_linearly_and_are_capped() {
    assert_scales_linearly(many_fns);
    let (diags, elapsed) = timed(|| marigold_check(&many_fns(60_000)));
    assert!(elapsed < BACKSTOP, "took {elapsed:?}");
    assert!(!has_error(&diags));
    assert!(diags.len() <= 51, "{}", diags.len());
    let summary = diags
        .iter()
        .filter(|d| d.code == "resolver-diagnostics-truncated");
    assert_eq!(summary.count(), 1);
    for d in &diags {
        assert!(
            d.help.as_deref().map_or(0, str::len) < 2_000,
            "{}",
            d.help.as_deref().unwrap().len()
        );
    }
}

#[test]
fn many_chained_stream_variables_scale_linearly() {
    assert_scales_linearly(chained_variables);
    let (diags, elapsed) = timed(|| marigold_check(&chained_variables(50_000)));
    assert!(elapsed < BACKSTOP, "took {elapsed:?}");
    assert!(!has_error(&diags));
    assert!(diags.len() <= 51, "{}", diags.len());
}

#[test]
fn resolver_diagnostics_are_capped_with_one_summary_that_prefers_warnings() {
    let mut src = String::new();
    for i in 0..80 {
        src.push_str(&format!("range(0,3).map(g{i}).return\n"));
    }
    src.push_str("x = x\nx.return\n");
    let diags = marigold_check(&src);
    assert_eq!(diags.len(), 51, "{}", diags.len());
    assert!(diags.iter().any(|d| d.severity == Severity::Warning));
    let summary: Vec<_> = diags
        .iter()
        .filter(|d| d.code == "resolver-diagnostics-truncated")
        .collect();
    assert_eq!(summary.len(), 1);
    assert_eq!(summary[0].severity, Severity::Information);
    assert!(summary[0].message.contains("31"), "{}", summary[0].message);
}

#[test]
fn help_lists_at_most_a_bounded_number_of_defined_names() {
    let mut src = String::new();
    for i in 0..60 {
        src.push_str(&format!("fn name{i}(x: i32) -> i32 {{ x }}\n"));
    }
    src.push_str("range(0,3).map(missing).return\n");
    let help = only(marigold_check(&src)).help.unwrap();
    assert!(help.contains("name0"), "{help}");
    assert!(!help.contains("name59"), "{help}");
    assert!(help.contains("more"), "{help}");
}

#[test]
fn syntax_error_hides_resolver_diagnostics_for_the_whole_file() {
    let src = "range(0, 5).map(f).return\nrange(0, 5).retur";
    let diags = marigold_check(src);
    assert!(diags.iter().all(|d| d.is_error()), "{diags:?}");
    assert!(diags.iter().all(|d| d.code != "undefined-fn"), "{diags:?}");
}
