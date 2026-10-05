mod common;

use common::{pos_of, uri, Client};
use lsp_types::request::HoverRequest;
use lsp_types::{
    Hover, HoverContents, HoverParams, MarkupKind, Position, TextDocumentIdentifier,
    TextDocumentPositionParams,
};
use proptest::prelude::*;

const PROGRAM: &str = "enum Color { Red, Green }\nstruct Row { a: int[0, 5] }\nfn double(x: i32) -> i32 { x * 2 }\nx = range(0, 5).map(double)\nx.return\nrange(Color).return\nread_file(\"d.csv\", csv, struct=Row).return\nrange(0, 3).permutations(2).return\n";

fn hover(client: &mut Client, u: &lsp_types::Uri, position: Position) -> serde_json::Value {
    client.request::<HoverRequest>(HoverParams {
        text_document_position_params: TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(u.clone()),
            position,
        ),
        work_done_progress_params: Default::default(),
    })
}

fn markdown(value: serde_json::Value) -> String {
    let hover: Hover = serde_json::from_value(value).unwrap();
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markup")
    };
    assert_eq!(markup.kind, MarkupKind::Markdown);
    markup.value
}

#[test]
fn advertises_hover() {
    let (client, init) = Client::start();
    assert!(matches!(
        init.capabilities.hover_provider,
        Some(lsp_types::HoverProviderCapability::Simple(true))
    ));
    client.shutdown();
}

#[test]
fn hover_on_stream_variable_declaration_shows_expression_and_complexity() {
    let (mut client, _) = Client::start();
    let u = uri("hover_decl");
    client.open_at(&u, PROGRAM);
    let text = markdown(hover(&mut client, &u, pos_of(PROGRAM, "x = range")));
    assert_eq!(
        text,
        "```marigold\nx = range(0, 5).map(double)\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 5 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: no"
    );
    client.shutdown();
}

#[test]
fn hover_on_stream_variable_reference_shows_declaration_expression() {
    let (mut client, _) = Client::start();
    let u = uri("hover_ref");
    client.open_at(&u, PROGRAM);
    let text = markdown(hover(&mut client, &u, pos_of(PROGRAM, "x.return")));
    assert!(
        text.starts_with("```marigold\nx = range(0, 5).map(double)\n```\n"),
        "{text}"
    );
    assert!(text.ends_with("collects input: no"), "{text}");
    client.shutdown();
}

#[test]
fn hover_on_output_stream_expression() {
    let (mut client, _) = Client::start();
    let u = uri("hover_expr");
    client.open_at(&u, PROGRAM);
    let text = markdown(hover(&mut client, &u, pos_of(PROGRAM, "permutations")));
    assert_eq!(
        text,
        "```marigold\nrange(0, 3).permutations(2).return\n```\nanalyzer estimate for the whole chain \u{b7} cardinality: 6 \u{b7} time: O(1) per whole stream \u{b7} space: O(1) \u{b7} collects input: yes"
    );
    client.shutdown();
}

#[test]
fn hover_on_function_struct_enum_declaration_and_reference() {
    let (mut client, _) = Client::start();
    let u = uri("hover_decls");
    client.open_at(&u, PROGRAM);
    let f = "**function** `double`\n```marigold\nfn double(x: i32) -> i32 { x * 2 }\n```";
    assert_eq!(
        markdown(hover(&mut client, &u, pos_of(PROGRAM, "double("))),
        f
    );
    assert_eq!(
        markdown(hover(&mut client, &u, pos_of(PROGRAM, "double)"))),
        f
    );
    let s = "**struct** `Row`\n```marigold\nstruct Row { a: int[0, 5] }\n```";
    assert_eq!(
        markdown(hover(&mut client, &u, pos_of(PROGRAM, "Row {"))),
        s
    );
    assert_eq!(markdown(hover(&mut client, &u, pos_of(PROGRAM, "Row)"))), s);
    let e = "**enum** `Color`\n```marigold\nenum Color { Red, Green }\n```";
    assert_eq!(
        markdown(hover(&mut client, &u, pos_of(PROGRAM, "Color)"))),
        e
    );
    client.shutdown();
}

#[test]
fn hover_on_undeclared_reference_says_so() {
    let (mut client, _) = Client::start();
    let u = uri("hover_undeclared");
    let text = "range(0, 3).map(missing).return\n";
    client.open_at(&u, text);
    assert_eq!(
        markdown(hover(&mut client, &u, pos_of(text, "missing"))),
        "**function** `missing` (not declared)"
    );
    client.shutdown();
}

#[test]
fn hover_is_null_on_whitespace_blank_lines_and_out_of_range() {
    let (mut client, _) = Client::start();
    let u = uri("hover_null");
    let text = "x = range(0, 5)\n\nx.return\n";
    client.open_at(&u, text);
    assert!(hover(&mut client, &u, Position::new(0, 1)).is_null());
    assert!(hover(&mut client, &u, Position::new(1, 0)).is_null());
    assert!(hover(&mut client, &u, Position::new(3, 0)).is_null());
    assert!(hover(&mut client, &u, Position::new(99, 99)).is_null());
    assert!(hover(&mut client, &uri("unopened"), Position::new(0, 0)).is_null());
    client.shutdown();
}

#[test]
fn hover_range_covers_hovered_symbol() {
    let (mut client, _) = Client::start();
    let u = uri("hover_range");
    client.open_at(&u, PROGRAM);
    let at = pos_of(PROGRAM, "double)");
    let parsed: Hover = serde_json::from_value(hover(&mut client, &u, at)).unwrap();
    let range = parsed.range.unwrap();
    assert_eq!(range.start, at);
    assert_eq!(range.end, Position::new(at.line, at.character + 6));
    client.shutdown();
}

#[test]
fn hover_on_unparsable_document_degrades_without_complexity() {
    let (mut client, _) = Client::start();
    let u = uri("hover_broken");
    let text = "x = range(0, 5)\nrange(0, 1).retur\nx.return\n";
    client.open_at(&u, text);
    let value = hover(&mut client, &u, pos_of(text, "x = range"));
    let md = markdown(value);
    assert_eq!(
        md,
        "**stream variable** `x`\n```marigold\nx = range(0, 5)\n```"
    );
    client.shutdown();
}

const SAMPLES: &[&str] = &[
    PROGRAM,
    "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}\u{1F600}.csv\", csv)\nq = range(0, 2)\nq.return\r\nx = \u{1F600}\n",
    "struct Row { a: int[0, 5] }\nread_file(\"\u{1F600}.csv\", csv, struct=Row).return",
    "x = range(0,\n",
    "",
    "\n\n\r\n",
];

#[test]
fn hover_never_panics_at_any_position_of_samples() {
    for text in SAMPLES {
        let lines = text.lines().count() as u32 + 2;
        for line in 0..lines {
            for character in 0..80 {
                let _ = marigold_lsp::hover::hover(text, Position::new(line, character));
            }
        }
    }
}

#[test]
fn hover_never_panics_at_any_byte_offset_of_samples() {
    for text in SAMPLES {
        let lines = marigold_lsp::position::LineIndex::new(text);
        for offset in 0..=text.len() + 2 {
            let _ = marigold_lsp::hover::hover(text, lines.position(offset));
        }
    }
}

proptest! {
    #[test]
    fn hover_never_panics_on_arbitrary_input(
        text in "\\PC{0,200}",
        line in any::<u32>(),
        character in any::<u32>(),
    ) {
        let _ = marigold_lsp::hover::hover(&text, Position::new(line, character));
    }
}
