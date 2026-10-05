use marigold_grammar::{marigold_analyze, marigold_stream_complexities};

fn text<'a>(src: &'a str, n: &marigold_grammar::complexity::NodeComplexity) -> &'a str {
    &src[n.range.start..n.range.end]
}

#[test]
fn golden_named_and_unnamed() {
    let src = "x = range(0, 5).map(f)\ny = x.filter(g)\nx.return\nrange(0, 3).return";
    let nodes = marigold_stream_complexities(src).unwrap();
    let got: Vec<_> = nodes
        .iter()
        .map(|n| (n.name.clone(), n.expr_index, text(src, n).to_string()))
        .collect();
    assert_eq!(
        got,
        vec![
            (
                Some("x".to_string()),
                0,
                "x = range(0, 5).map(f)".to_string()
            ),
            (Some("y".to_string()), 1, "y = x.filter(g)".to_string()),
            (None, 2, "x.return".to_string()),
            (None, 3, "range(0, 3).return".to_string()),
        ]
    );
}

#[test]
fn skips_declarations_that_are_not_streams() {
    let src = "fn f(x: i32) -> i32 { x }\nenum E { A, B }\nrange(E).map(f).return";
    let nodes = marigold_stream_complexities(src).unwrap();
    assert_eq!(nodes.len(), 1);
    assert_eq!(nodes[0].expr_index, 2);
    assert_eq!(text(src, &nodes[0]), "range(E).map(f).return");
}

#[test]
fn multiline_expression_range_covers_whole_expression() {
    let src = "x = range(0, 5)\n  .map(f)\n  .filter(g)\nx\n  .return";
    let nodes = marigold_stream_complexities(src).unwrap();
    assert_eq!(
        text(src, &nodes[0]),
        "x = range(0, 5)\n  .map(f)\n  .filter(g)"
    );
    assert_eq!(text(src, &nodes[1]), "x\n  .return");
}

#[test]
fn output_streams_agree_with_analyze() {
    let programs = [
        "range(0, 10).return",
        "x = range(0, 5).map(f)\ny = x.filter(g)\ny.return\nx.permutations(2).return",
        "z = range(0, 4).combinations(2)\nz.fold(0, add).return",
        "enum E { A, B }\nrange(E).return",
        "q = range(0, 4)\nr = q.keep_first_n(2, cmp)\nr.return",
    ];
    for src in programs {
        let analyzed = marigold_analyze(src).unwrap();
        let outputs: Vec<_> = marigold_stream_complexities(src)
            .unwrap()
            .into_iter()
            .filter(|n| n.name.is_none())
            .map(|n| serde_json::to_value(&n.complexity).unwrap())
            .collect();
        let expected: Vec<_> = analyzed
            .streams
            .iter()
            .map(|s| serde_json::to_value(s).unwrap())
            .collect();
        assert_eq!(outputs, expected, "{src}");
    }
}

#[test]
fn variable_complexity_reflects_its_chain() {
    let src = "x = range(0, 5).permutations(2)\ny = x.filter(g)\ny.return";
    let nodes = marigold_stream_complexities(src).unwrap();
    let x = &nodes[0].complexity;
    let y = &nodes[1].complexity;
    assert!(x.collects_input);
    assert!(y.collects_input);
    assert_eq!(x.description, "x = input.permutations(2)");
    assert_eq!(y.description, "y = x.filter(...)");
    assert!(y.exact_time.to_string().len() >= x.exact_time.to_string().len());
}

#[test]
fn unicode_offsets_are_byte_offsets() {
    let src = "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)\nq = range(0, 2)\nq.return";
    let nodes = marigold_stream_complexities(src).unwrap();
    assert_eq!(nodes.len(), 3);
    assert_eq!(
        text(src, &nodes[0]),
        "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)"
    );
    assert_eq!(text(src, &nodes[1]), "q = range(0, 2)");
    assert_eq!(nodes[1].range.start, src.find("q =").unwrap());
    assert_eq!(text(src, &nodes[2]), "q.return");
}

#[test]
fn range_excludes_trailing_whitespace() {
    let src = "x = range(0, 2)   \n\n  x.return  \n";
    let nodes = marigold_stream_complexities(src).unwrap();
    assert_eq!(text(src, &nodes[0]), "x = range(0, 2)");
    assert_eq!(text(src, &nodes[1]), "x.return");
}

#[test]
fn syntax_errors_are_err() {
    assert!(marigold_stream_complexities("range(0, 10).retur").is_err());
    assert!(marigold_stream_complexities("x = range(0, 5)\nrange(0,").is_err());
}

#[test]
fn undefined_enum_is_err() {
    assert!(marigold_stream_complexities("range(Nope).return").is_err());
}

#[test]
fn empty_program_has_no_nodes() {
    assert!(marigold_stream_complexities("").unwrap().is_empty());
}
