use lsp_types::{HoverContents, Position};

fn hover_text(text: &str, position: Position) -> String {
    let hover = marigold_lsp::hover::hover(text, position).expect("hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markup")
    };
    markup.value
}

#[test]
fn golden_map_chain_prefers_exact_space() {
    let text = "x = range(0, 5).map(f)\nx.return";
    assert_eq!(
        hover_text(text, Position::new(0, 0)),
        "```marigold\nx = range(0, 5).map(f)\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 5 \u{b7} time: not estimated (calls user functions) \u{b7} space: O(1) \u{b7} collects input: no"
    );
}

#[test]
fn golden_huge_permutations_cardinality_is_readable() {
    let text = "range(0, 1000000).permutations(3).return";
    assert_eq!(
        hover_text(text, Position::new(0, 2)),
        "```marigold\nrange(0, 1000000).permutations(3).return\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: ~1.0e18 \u{b7} time: not estimated (cardinality too large) \u{b7} space: O(1) \u{b7} collects input: yes"
    );
}

#[test]
fn golden_collects_input_case() {
    let text = "x = range(0, 3).permutations(2)\nx.return";
    assert_eq!(
        hover_text(text, Position::new(0, 0)),
        "```marigold\nx = range(0, 3).permutations(2)\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 6 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: yes"
    );
}

#[test]
fn format_cardinality_unit_cases() {
    use marigold_grammar::complexity::Cardinality;
    use marigold_lsp::hover::format_cardinality;
    let exact = |s: &str| Cardinality::Exact(s.parse().unwrap());
    assert_eq!(format_cardinality(&exact("5")), "5");
    assert_eq!(
        format_cardinality(&exact("999999999999999")),
        "999999999999999"
    );
    assert_eq!(format_cardinality(&exact("1000000000000000")), "~1.0e15");
    assert_eq!(format_cardinality(&exact("999997000002000000")), "~1.0e18");
    assert_eq!(
        format_cardinality(&exact("123456789012345678901234567890")),
        "~1.2e29"
    );
    assert_eq!(format_cardinality(&Cardinality::Unknown), "?");
}

#[test]
fn golden_bare_range_space_uses_exact_space_not_space_class() {
    let text = "x = range(0, 5)\nx.return";
    assert_eq!(
        hover_text(text, Position::new(0, 0)),
        "```marigold\nx = range(0, 5)\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 5 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: no"
    );
}

#[test]
fn large_cardinality_never_claims_o1_time() {
    let text = "range(0, 1000000).permutations(3).return";
    let shown = hover_text(text, Position::new(0, 2));
    assert!(!shown.contains("time: O(1)"), "{shown}");
    assert!(shown.contains("time: not estimated"), "{shown}");
}

#[test]
fn just_over_threshold_is_not_estimated_and_at_threshold_is_estimated() {
    let over = hover_text("x = range(0, 1000001)\nx.return", Position::new(0, 0));
    assert!(over.contains("time: not estimated"), "{over}");
    let at = hover_text("x = range(0, 1000000)\nx.return", Position::new(0, 0));
    assert!(at.contains("time: O(1) per whole stream"), "{at}");
}

#[test]
fn unknown_cardinality_is_not_estimated() {
    let text = "x = range(0, 5).filter(keep)\nx.return";
    let shown = hover_text(text, Position::new(0, 0));
    assert!(!shown.contains("time: O(1)"), "{shown}");
}
