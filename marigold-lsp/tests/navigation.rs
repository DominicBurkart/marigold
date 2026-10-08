mod common;

use common::{pos_at, pos_of, uri, Client};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, References};
use lsp_types::{
    ClientCapabilities, DocumentSymbolClientCapabilities, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, InitializeParams, Location, OneOf, Position,
    Range, ReferenceContext, ReferenceParams, SymbolKind, TextDocumentClientCapabilities,
    TextDocumentIdentifier, TextDocumentPositionParams,
};

const PROGRAM: &str = "enum Color { Red, Green }\nstruct Row { a: int[0, 5] }\nfn double(x: i32) -> i32 { x * 2 }\nx = range(0, 3).map(double)\nx.return\nx.return\nrange(Color).return\nread_file(\"d.csv\", csv, struct=Row).return\n";

fn at(uri: &lsp_types::Uri, position: Position) -> TextDocumentPositionParams {
    TextDocumentPositionParams::new(TextDocumentIdentifier::new(uri.clone()), position)
}

fn definition(client: &mut Client, u: &lsp_types::Uri, position: Position) -> serde_json::Value {
    client.request::<GotoDefinition>(GotoDefinitionParams {
        text_document_position_params: at(u, position),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    })
}

fn references(
    client: &mut Client,
    u: &lsp_types::Uri,
    position: Position,
    include_declaration: bool,
) -> serde_json::Value {
    client.request::<References>(ReferenceParams {
        text_document_position: at(u, position),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: ReferenceContext {
            include_declaration,
        },
    })
}

fn range_of(text: &str, needle: &str, nth: usize) -> Range {
    let start = text.match_indices(needle).nth(nth).unwrap().0;
    Range::new(pos_at(text, start), pos_at(text, start + needle.len()))
}

fn x_decl() -> Range {
    let start = PROGRAM.find("x = range").unwrap();
    Range::new(pos_at(PROGRAM, start), pos_at(PROGRAM, start + 1))
}

fn location(u: &lsp_types::Uri, range: Range) -> serde_json::Value {
    serde_json::to_value(Location::new(u.clone(), range)).unwrap()
}

fn hierarchical_params() -> InitializeParams {
    InitializeParams {
        capabilities: ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                document_symbol: Some(DocumentSymbolClientCapabilities {
                    hierarchical_document_symbol_support: Some(true),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            ..Default::default()
        },
        ..Default::default()
    }
}

fn document_symbols(client: &mut Client, u: &lsp_types::Uri) -> serde_json::Value {
    client.request::<DocumentSymbolRequest>(DocumentSymbolParams {
        text_document: TextDocumentIdentifier::new(u.clone()),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    })
}

#[test]
fn advertises_navigation_capabilities() {
    let (client, init) = Client::start();
    let caps = init.capabilities;
    assert_eq!(caps.definition_provider, Some(OneOf::Left(true)));
    assert_eq!(caps.references_provider, Some(OneOf::Left(true)));
    assert_eq!(caps.document_symbol_provider, Some(OneOf::Left(true)));
    client.shutdown();
}

#[test]
fn definition_of_stream_variable_reference() {
    let (mut client, _) = Client::start();
    let u = uri("def_var");
    client.open_at(&u, PROGRAM);
    let reference = pos_at(PROGRAM, PROGRAM.find("x.return").unwrap());
    let result = definition(&mut client, &u, reference);
    assert_eq!(result, location(&u, x_decl()));
    client.shutdown();
}

#[test]
fn definition_of_function_enum_and_struct() {
    let (mut client, _) = Client::start();
    let u = uri("def_kinds");
    client.open_at(&u, PROGRAM);
    let f = definition(&mut client, &u, pos_of(PROGRAM, "double)"));
    assert_eq!(f, location(&u, range_of(PROGRAM, "double", 0)));
    let e = definition(&mut client, &u, pos_of(PROGRAM, "Color).return"));
    assert_eq!(e, location(&u, range_of(PROGRAM, "Color", 0)));
    let s = definition(&mut client, &u, pos_of(PROGRAM, "Row)"));
    assert_eq!(s, location(&u, range_of(PROGRAM, "Row", 0)));
    client.shutdown();
}

#[test]
fn definition_on_declaration_returns_itself() {
    let (mut client, _) = Client::start();
    let u = uri("def_self");
    client.open_at(&u, PROGRAM);
    let result = definition(&mut client, &u, pos_of(PROGRAM, "double"));
    assert_eq!(result, location(&u, range_of(PROGRAM, "double", 0)));
    client.shutdown();
}

#[test]
fn definition_is_null_for_whitespace_undeclared_and_unopened() {
    let (mut client, _) = Client::start();
    let u = uri("def_null");
    client.open_at(&u, "range(0, 3).map(missing).return\n");
    let text = "range(0, 3).map(missing).return\n";
    assert!(definition(&mut client, &u, pos_of(text, "missing")).is_null());
    assert!(definition(&mut client, &u, Position::new(0, 5)).is_null());
    assert!(definition(&mut client, &u, Position::new(99, 99)).is_null());
    assert!(definition(&mut client, &uri("never_opened"), Position::new(0, 0)).is_null());
    client.shutdown();
}

#[test]
fn references_honour_include_declaration() {
    let (mut client, _) = Client::start();
    let u = uri("refs");
    client.open_at(&u, PROGRAM);
    let at_x = pos_of(PROGRAM, "x = range");
    let with = references(&mut client, &u, at_x, true);
    let without = references(&mut client, &u, at_x, false);
    assert_eq!(
        with,
        serde_json::json!([
            location(&u, x_decl()),
            location(&u, range_of(PROGRAM, "x.return", 0).start.into_range(1)),
            location(&u, range_of(PROGRAM, "x.return", 1).start.into_range(1)),
        ])
    );
    assert_eq!(without.as_array().unwrap().len(), 2);
    assert!(!without
        .as_array()
        .unwrap()
        .contains(&location(&u, x_decl())));
    client.shutdown();
}

trait IntoRange {
    fn into_range(self, width: u32) -> Range;
}

impl IntoRange for Position {
    fn into_range(self, width: u32) -> Range {
        Range::new(self, Position::new(self.line, self.character + width))
    }
}

#[test]
fn references_from_a_reference_position_match_declaration_query() {
    let (mut client, _) = Client::start();
    let u = uri("refs_from_ref");
    client.open_at(&u, PROGRAM);
    let from_decl = references(&mut client, &u, pos_of(PROGRAM, "double)"), true);
    let from_fn_decl = references(&mut client, &u, pos_of(PROGRAM, "double("), true);
    assert_eq!(from_decl, from_fn_decl);
    assert_eq!(from_decl.as_array().unwrap().len(), 2);
    client.shutdown();
}

#[test]
fn references_null_for_unopened_and_nothing() {
    let (mut client, _) = Client::start();
    let u = uri("refs_null");
    client.open_at(&u, PROGRAM);
    assert!(references(&mut client, &uri("nope"), Position::new(0, 0), true).is_null());
    assert!(references(&mut client, &u, Position::new(0, 4), true).is_null());
    client.shutdown();
}

#[test]
fn positions_after_multibyte_text_map_to_utf16_columns() {
    let (mut client, _) = Client::start();
    let u = uri("utf16");
    let text =
        "struct Row { a: int[0, 5] }\nread_file(\"\u{1F600}\u{e9}.csv\", csv, struct=Row).return\n";
    client.open_at(&u, text);
    let at_ref = pos_of(text, "Row).return");
    assert_eq!(at_ref.line, 1);
    let result = definition(&mut client, &u, at_ref);
    assert_eq!(result, location(&u, range_of(text, "Row", 0)));
    let refs = references(&mut client, &u, at_ref, false);
    let expected = Range::new(at_ref, Position::new(at_ref.line, at_ref.character + 3));
    assert_eq!(refs, serde_json::json!([location(&u, expected)]));
    client.shutdown();
}

#[test]
fn document_symbols_hierarchical() {
    let (mut client, init) = Client::start_with(hierarchical_params());
    assert_eq!(
        init.capabilities.document_symbol_provider,
        Some(OneOf::Left(true))
    );
    let u = uri("symbols");
    client.open_at(&u, PROGRAM);
    let value = document_symbols(&mut client, &u);
    let DocumentSymbolResponse::Nested(symbols) = serde_json::from_value(value).unwrap() else {
        panic!("expected nested symbols")
    };
    let summary: Vec<_> = symbols.iter().map(|s| (s.name.as_str(), s.kind)).collect();
    assert_eq!(
        summary,
        [
            ("Color", SymbolKind::ENUM),
            ("Row", SymbolKind::STRUCT),
            ("double", SymbolKind::FUNCTION),
            ("x", SymbolKind::VARIABLE),
        ]
    );
    let color = &symbols[0];
    assert_eq!(color.selection_range, range_of(PROGRAM, "Color", 0));
    assert_eq!(
        color.range,
        range_of(PROGRAM, "enum Color { Red, Green }", 0)
    );
    let double = &symbols[2];
    assert_eq!(
        double.range,
        range_of(PROGRAM, "fn double(x: i32) -> i32 { x * 2 }", 0)
    );
    assert_eq!(double.selection_range, range_of(PROGRAM, "double", 0));
    let x = &symbols[3];
    assert_eq!(x.range, range_of(PROGRAM, "x = range(0, 3).map(double)", 0));
    assert_eq!(x.selection_range, x_decl());
    for s in &symbols {
        assert!(s.range.start <= s.selection_range.start && s.selection_range.end <= s.range.end);
    }
    client.shutdown();
}

#[test]
fn document_symbols_flat_without_hierarchical_support() {
    let (mut client, _) = Client::start();
    let u = uri("symbols_flat");
    client.open_at(&u, PROGRAM);
    let value = document_symbols(&mut client, &u);
    let DocumentSymbolResponse::Flat(symbols) = serde_json::from_value(value).unwrap() else {
        panic!("expected flat symbols")
    };
    assert_eq!(symbols.len(), 4);
    assert_eq!(symbols[3].name, "x");
    assert_eq!(symbols[3].kind, SymbolKind::VARIABLE);
    assert_eq!(symbols[3].location.uri, u);
    client.shutdown();
}

#[test]
fn document_symbols_null_for_unopened() {
    let (mut client, _) = Client::start();
    assert!(document_symbols(&mut client, &uri("unopened")).is_null());
    client.shutdown();
}

#[test]
fn document_symbols_survive_broken_documents() {
    let (mut client, _) = Client::start_with(hierarchical_params());
    let u = uri("symbols_broken");
    client.open_at(
        &u,
        "x = range(0, 5)\nrange(0, 1).retur\nfn f(a: i32) -> i32 { a }\n",
    );
    let value = document_symbols(&mut client, &u);
    let names: Vec<_> = value
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["name"].as_str().unwrap().to_string())
        .collect();
    assert!(names.contains(&"x".to_string()), "{names:?}");
    client.shutdown();
}

#[test]
fn unknown_methods_still_method_not_found() {
    let (mut client, _) = Client::start();
    let response = client.call_raw("textDocument/notAThing", serde_json::json!({}));
    assert_eq!(
        response.response_result.unwrap_err().code,
        lsp_server::ErrorCode::MethodNotFound as i32
    );
    client.shutdown();
}

#[test]
fn malformed_params_get_invalid_params() {
    let (mut client, _) = Client::start();
    let response = client.call_raw("textDocument/definition", serde_json::json!({"bogus": 1}));
    assert_eq!(
        response.response_result.unwrap_err().code,
        lsp_server::ErrorCode::InvalidParams as i32
    );
    client.shutdown();
}

mod block_ranges {
    use lsp_types::Position;

    #[test]
    fn char_literal_double_quote_does_not_toggle_string_state() {
        let text = "fn f(x: i32) -> i32 {\n    let q = '\"';\n    x\n}\ny = range(0, 1)\ny.return";
        let outline = marigold_lsp::nav::outline(text);
        assert_eq!(outline[0].name, "f");
        assert_eq!(outline[0].range.end, Position::new(3, 1));
        assert_eq!(outline[1].name, "y");
    }

    #[test]
    fn raw_string_with_braces_and_quotes_is_skipped() {
        let text = "fn f() -> String {\n    r#\"} {\"#.to_string()\n}\ny = range(0, 1)\ny.return";
        let outline = marigold_lsp::nav::outline(text);
        assert_eq!(outline[0].range.end, Position::new(2, 1));
    }

    #[test]
    fn escaped_quote_char_and_lifetimes_are_handled() {
        let text = "fn f(s: i32) -> char {\n    let l: &'static str = \"x\";\n    let c = '\\'';\n    let d = '{';\n    '}'\n}\ny = range(0, 1)\ny.return";
        let outline = marigold_lsp::nav::outline(text);
        assert_eq!(outline[0].range.end, Position::new(5, 1));
    }
}
