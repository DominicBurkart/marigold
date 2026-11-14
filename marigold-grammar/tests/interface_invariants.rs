use marigold_grammar::{
    marigold_analyze, marigold_check, marigold_node_ids, marigold_parse,
    marigold_stream_complexities, marigold_symbols,
};
use proptest::prelude::*;
use std::collections::HashSet;

const PROGRAMS: &[&str] = &[
    "range(0, 10).return",
    "x = range(0, 5)\nx.return",
    "fn double(i: i32) -> i32 { i * 2 }\nrange(0, 3).map(double).return",
    "range(0, 4).permutations(2).combinations(2).return",
    "range(0, 9).keep_first_n(3, sorter).return",
    "enum Color { Red, Green }\nrange(Color).return",
    "select_all(range(0, 5), range(10, 15)).return",
    "range(0, 5).fold(0, add).return",
];

fn mutate(base: &str, at: usize, insert: &str, delete: bool) -> String {
    let boundaries: Vec<usize> = (0..=base.len())
        .filter(|i| base.is_char_boundary(*i))
        .collect();
    let pos = boundaries[at % boundaries.len()];
    let mut src = base.to_string();
    if delete {
        if pos < src.len() {
            src.remove(pos);
        }
    } else {
        src.insert_str(pos, insert);
    }
    src
}

#[test]
fn parse_accepts_the_empty_program() {
    assert_eq!(marigold_parse("").is_ok(), marigold_parse("   \n").is_ok());
    assert!(marigold_parse("").is_ok());
}

#[test]
fn parse_returns_err_with_a_message_for_a_syntax_error() {
    let err = marigold_parse("range(0,").unwrap_err();
    assert!(!err.to_string().is_empty());
}

#[test]
fn parse_is_total_on_adversarial_inputs() {
    let inputs = [
        String::new(),
        "\0".to_string(),
        "\u{feff}range(0, 1).return".to_string(),
        "日本語 → ünïcödé".to_string(),
        "range(0, 1).return\r\n\r\n".to_string(),
        format!("{}range(0,1){}.return", "(".repeat(5000), ")".repeat(5000)),
        format!("range(0,1){}.return", ".map(f)".repeat(5000)),
        format!("range(0,1).filter({}).return", "f(".repeat(5000)),
        format!(
            "fn f(x: i32) -> i32 {}x{}",
            "{".repeat(5000),
            "}".repeat(5000)
        ),
        "\"".repeat(1001),
        "/*".repeat(1000),
    ];
    for input in &inputs {
        let parsed = marigold_parse(input);
        let diags = marigold_check(input);
        assert_eq!(
            parsed.is_err(),
            diags.iter().any(|d| d.is_error()),
            "{:?}",
            &input[..input.len().min(40)]
        );
    }
}

proptest! {
    #[test]
    fn parse_never_panics_on_arbitrary_text(src in "\\PC{0,300}") {
        let _ = marigold_parse(&src);
    }

    #[test]
    fn parse_never_panics_on_arbitrary_ascii_with_newlines(src in "[ -~\n\t]{0,300}") {
        let _ = marigold_parse(&src);
    }

    #[test]
    fn parse_never_panics_on_mutated_programs(
        idx in 0..PROGRAMS.len(),
        at in any::<usize>(),
        insert in "\\PC{0,3}",
        delete in any::<bool>(),
    ) {
        let src = mutate(PROGRAMS[idx], at, &insert, delete);
        let _ = marigold_parse(&src);
    }
}

#[test]
fn analyze_and_stream_complexities_accept_every_program_parse_accepts() {
    for src in PROGRAMS {
        assert!(marigold_parse(src).is_ok(), "{src}");
        assert!(marigold_analyze(src).is_ok(), "{src}");
        assert!(marigold_stream_complexities(src).is_ok(), "{src}");
    }
}

#[test]
fn analyze_and_stream_complexities_reject_syntax_errors() {
    for src in ["range(0,", "range(0, 10).retur", "range(0, 10).bogus"] {
        assert!(marigold_parse(src).is_err(), "{src}");
        assert!(marigold_analyze(src).is_err(), "{src}");
        assert!(marigold_stream_complexities(src).is_err(), "{src}");
    }
}

#[test]
fn parse_is_stricter_than_analyze_on_a_redeclared_stream_variable() {
    let src = "x = range(0, 5)\nx.return\nx = range(0, 7)\nx.return";
    assert!(marigold_parse(src).is_err());
    assert!(marigold_check(src).iter().any(|d| d.is_error()));
    assert!(marigold_analyze(src).is_ok());
    assert!(marigold_stream_complexities(src).is_ok());
}

proptest! {
    #[test]
    fn parse_ok_implies_analyze_and_complexities_ok_on_arbitrary_text(src in "\\PC{0,120}") {
        let parsed = marigold_parse(&src).is_ok();
        let analyzed = marigold_analyze(&src).is_ok();
        let complexities = marigold_stream_complexities(&src).is_ok();
        prop_assert!(!parsed || analyzed);
        prop_assert!(!parsed || complexities);
    }

    #[test]
    fn parse_ok_implies_analyze_ok_on_mutated_programs(
        idx in 0..PROGRAMS.len(),
        at in any::<usize>(),
        insert in "\\PC{0,3}",
        delete in any::<bool>(),
    ) {
        let src = mutate(PROGRAMS[idx], at, &insert, delete);
        if marigold_parse(&src).is_ok() {
            prop_assert!(marigold_analyze(&src).is_ok());
            prop_assert!(marigold_stream_complexities(&src).is_ok());
        }
        if marigold_analyze(&src).is_err() {
            prop_assert!(marigold_parse(&src).is_err());
        }
    }
}

fn assert_symbol_ranges_are_valid(src: &str) {
    let index = marigold_symbols(src);
    let spans = index
        .declarations
        .iter()
        .map(|d| (d.range, d.name.as_str()))
        .chain(index.references.iter().map(|r| (r.range, r.name.as_str())));
    for (range, name) in spans {
        assert!(range.start <= range.end, "{range:?}");
        assert!(range.end <= src.len(), "{range:?}");
        assert!(src.is_char_boundary(range.start), "{range:?}");
        assert!(src.is_char_boundary(range.end), "{range:?}");
        assert_eq!(&src[range.start..range.end], name);
    }
}

#[test]
fn symbols_of_the_empty_and_adversarial_inputs_are_valid() {
    for src in [
        "",
        "\0",
        "x = ",
        "日本語 = range(0, 1)",
        "x = range(0,\nx.return",
    ] {
        assert_symbol_ranges_are_valid(src);
    }
    for src in PROGRAMS {
        assert_symbol_ranges_are_valid(src);
    }
}

proptest! {
    #[test]
    fn symbols_ranges_are_valid_for_arbitrary_text(src in "\\PC{0,200}") {
        assert_symbol_ranges_are_valid(&src);
    }

    #[test]
    fn symbols_ranges_are_valid_for_mutated_programs(
        idx in 0..PROGRAMS.len(),
        at in any::<usize>(),
        insert in "\\PC{0,3}",
        delete in any::<bool>(),
    ) {
        assert_symbol_ranges_are_valid(&mutate(PROGRAMS[idx], at, &insert, delete));
    }
}

fn assert_node_ids_are_valid(src: &str) {
    let nodes = marigold_node_ids(src, "p");
    let again = marigold_node_ids(src, "p");
    assert_eq!(
        nodes.iter().map(|n| n.id.clone()).collect::<Vec<_>>(),
        again.iter().map(|n| n.id.clone()).collect::<Vec<_>>()
    );
    let mut seen = HashSet::new();
    for n in &nodes {
        assert!(n.range.start <= n.range.end, "{:?}", n.range);
        assert!(n.range.end <= src.len(), "{:?}", n.range);
        assert!(src.is_char_boundary(n.range.start), "{:?}", n.range);
        assert!(src.is_char_boundary(n.range.end), "{:?}", n.range);
        assert!(seen.insert(n.id.to_string()), "duplicate id {}", n.id);
    }
}

#[test]
fn node_ids_are_namespaced_by_program() {
    for src in PROGRAMS {
        let a: HashSet<String> = marigold_node_ids(src, "alpha")
            .iter()
            .map(|n| n.id.to_string())
            .collect();
        let b: HashSet<String> = marigold_node_ids(src, "beta")
            .iter()
            .map(|n| n.id.to_string())
            .collect();
        assert!(!a.is_empty(), "{src}");
        assert_eq!(a.len(), marigold_node_ids(src, "alpha").len(), "{src}");
        assert!(a.is_disjoint(&b), "{src}");
    }
}

#[test]
fn node_ids_of_the_empty_program_are_empty() {
    assert!(marigold_node_ids("", "p").is_empty());
}

#[test]
fn node_ids_of_repeated_identical_expressions_are_distinct() {
    let src = "range(0, 3).return\nrange(0, 3).return\nrange(0, 3).return";
    let nodes = marigold_node_ids(src, "p");
    let ids: HashSet<String> = nodes.iter().map(|n| n.id.to_string()).collect();
    assert_eq!(ids.len(), nodes.len());
    assert!(nodes.len() >= 6);
}

#[test]
fn node_ids_are_valid_for_broken_and_adversarial_input() {
    for src in [
        "",
        "\0",
        "range(0,",
        "x = range(0, 1)\nrange(0, 1).retur\nx.return",
    ] {
        assert_node_ids_are_valid(src);
    }
    for src in PROGRAMS {
        assert_node_ids_are_valid(src);
    }
}

proptest! {
    #[test]
    fn node_ids_are_valid_for_arbitrary_text(src in "\\PC{0,200}") {
        assert_node_ids_are_valid(&src);
    }

    #[test]
    fn node_ids_are_valid_for_mutated_programs(
        idx in 0..PROGRAMS.len(),
        at in any::<usize>(),
        insert in "\\PC{0,3}",
        delete in any::<bool>(),
    ) {
        assert_node_ids_are_valid(&mutate(PROGRAMS[idx], at, &insert, delete));
    }
}
