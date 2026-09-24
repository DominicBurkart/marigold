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

#[test]
fn undefined_enum_in_range() {
    let d = only(marigold_check("range(Color).return"));
    assert_eq!(d.code, "undefined-enum");
    assert!(d.message.contains("Color"), "{}", d.message);
}

#[test]
fn bounded_min_greater_than_max() {
    let d = only(marigold_check(
        "struct Test { field: int[10, 5] }\nrange(0, 1).return",
    ));
    assert_eq!(d.code, "bounds-violation");
    assert!(d.message.contains("min") && d.message.contains("max"));
}

#[test]
fn bounded_uint_negative_min() {
    let d = only(marigold_check(
        "struct Test { field: uint[-1, 10] }\nrange(0, 1).return",
    ));
    assert_eq!(d.code, "bounds-violation");
    assert!(d.message.contains("negative"), "{}", d.message);
}

#[test]
fn bounded_undefined_type() {
    let d = only(marigold_check(
        "struct Test { field: int[0, NonExistent.len()] }\nrange(0, 1).return",
    ));
    assert_eq!(d.code, "undefined-type");
    assert!(d.message.contains("NonExistent"));
}

#[test]
fn bounded_errors_are_all_reported() {
    let diags = marigold_check(
        "struct Test { a: int[10, 5], b: int[0, Missing.len()] }\nrange(0, 1).return",
    );
    assert_eq!(diags.len(), 2, "{diags:?}");
    let codes: Vec<_> = diags.iter().map(|d| d.code).collect();
    assert!(codes.contains(&"bounds-violation"));
    assert!(codes.contains(&"undefined-type"));
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
    "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
    "range(0, 1).write_file(\"ünïcødé.csv\", csv)",
];

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
        prop_assert_eq!(marigold_check(&src).is_empty(), marigold_parse(&src).is_ok());
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
        prop_assert_eq!(diags.is_empty(), marigold_parse(&src).is_ok());
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
