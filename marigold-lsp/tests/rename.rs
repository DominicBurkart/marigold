mod common;

use common::{pos_at, pos_of, uri, Client};
use lsp_types::request::{PrepareRenameRequest, Rename};
use lsp_types::{
    OneOf, Position, RenameParams, TextDocumentIdentifier, TextDocumentPositionParams, TextEdit,
    WorkspaceEdit,
};
use proptest::prelude::*;

const PROGRAM: &str = "enum Color { Red, Green }\nstruct Row { a: int[0, 5] }\nfn double(x: i32) -> i32 { x * 2 }\nx = range(0, 3).map(double)\nx.return\ny = x.map(double)\ny.return\nrange(Color).return\nread_file(\"d.csv\", csv, struct=Row).return\n";

fn rename(
    client: &mut Client,
    u: &lsp_types::Uri,
    position: Position,
    new_name: &str,
) -> lsp_server::Response {
    client.call::<Rename>(RenameParams {
        text_document_position: TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(u.clone()),
            position,
        ),
        new_name: new_name.to_string(),
        work_done_progress_params: Default::default(),
    })
}

fn edits_of(response: lsp_server::Response, u: &lsp_types::Uri) -> Vec<TextEdit> {
    let edit: WorkspaceEdit = serde_json::from_value(response.response_result.unwrap()).unwrap();
    edit.changes.unwrap().remove(u).unwrap_or_default()
}

fn error_code(response: lsp_server::Response) -> i32 {
    response.response_result.unwrap_err().code
}

#[test]
fn advertises_rename_with_prepare() {
    let (client, init) = Client::start();
    let Some(OneOf::Right(options)) = init.capabilities.rename_provider else {
        panic!("rename provider must carry options")
    };
    assert_eq!(options.prepare_provider, Some(true));
    assert_eq!(
        init.capabilities.workspace_symbol_provider,
        Some(OneOf::Left(true))
    );
    client.shutdown();
}

#[test]
fn prepare_rename_returns_name_range_and_placeholder() {
    let (mut client, _) = Client::start();
    let u = uri("prep");
    client.open_at(&u, PROGRAM);
    let at = pos_of(PROGRAM, "double)");
    let value = client.request::<PrepareRenameRequest>(TextDocumentPositionParams::new(
        TextDocumentIdentifier::new(u.clone()),
        at,
    ));
    assert_eq!(
        value,
        serde_json::json!({
            "range": {"start": at, "end": {"line": at.line, "character": at.character + 6}},
            "placeholder": "double"
        })
    );
    let none = client.request::<PrepareRenameRequest>(TextDocumentPositionParams::new(
        TextDocumentIdentifier::new(u.clone()),
        Position::new(0, 2),
    ));
    assert!(none.is_null());
    client.shutdown();
}

#[test]
fn rename_function_updates_declaration_and_all_references() {
    let (mut client, _) = Client::start();
    let u = uri("rename_fn");
    client.open_at(&u, PROGRAM);
    let response = rename(&mut client, &u, pos_of(PROGRAM, "double)"), "twice");
    let edits = edits_of(response, &u);
    assert_eq!(edits.len(), 3);
    assert!(edits.iter().all(|e| e.new_text == "twice"));
    let mut starts: Vec<_> = edits.iter().map(|e| e.range.start).collect();
    starts.sort_by_key(|p| (p.line, p.character));
    assert_eq!(starts[0], pos_of(PROGRAM, "double("));
    client.shutdown();
}

#[test]
fn rename_stream_variable_from_declaration_covers_references() {
    let (mut client, _) = Client::start();
    let u = uri("rename_var");
    client.open_at(&u, PROGRAM);
    let response = rename(&mut client, &u, pos_of(PROGRAM, "x = range"), "xs");
    let edits = edits_of(response, &u);
    assert_eq!(edits.len(), 3);
    client.shutdown();
}

#[test]
fn rename_struct_and_enum() {
    let (mut client, _) = Client::start();
    let u = uri("rename_types");
    client.open_at(&u, PROGRAM);
    let s = edits_of(
        rename(&mut client, &u, pos_of(PROGRAM, "Row {"), "Record"),
        &u,
    );
    assert_eq!(s.len(), 2);
    let e = edits_of(
        rename(&mut client, &u, pos_of(PROGRAM, "Color)"), "Hue"),
        &u,
    );
    assert_eq!(e.len(), 2);
    client.shutdown();
}

#[test]
fn rename_rejects_invalid_identifiers_with_invalid_params() {
    let (mut client, _) = Client::start();
    let u = uri("rename_invalid");
    client.open_at(&u, PROGRAM);
    for bad in [
        "",
        "1abc",
        "has space",
        "a.b",
        "caf\u{e9}",
        "x;y",
        "-a",
        "a-b",
    ] {
        let response = rename(&mut client, &u, pos_of(PROGRAM, "double)"), bad);
        assert_eq!(
            error_code(response),
            lsp_server::ErrorCode::InvalidParams as i32,
            "{bad:?}"
        );
    }
    client.shutdown();
}

#[test]
fn rename_rejects_clash_with_existing_declaration_of_same_kind() {
    let (mut client, _) = Client::start();
    let u = uri("rename_clash");
    let text =
        "fn a(x: i32) -> i32 { x }\nfn b(x: i32) -> i32 { x }\nrange(0, 3).map(a).map(b).return\n";
    client.open_at(&u, text);
    let response = rename(&mut client, &u, pos_of(text, "a)"), "b");
    assert_eq!(
        error_code(response),
        lsp_server::ErrorCode::InvalidParams as i32
    );
    let ok = rename(&mut client, &u, pos_of(text, "a)"), "c");
    assert_eq!(edits_of(ok, &u).len(), 2);
    client.shutdown();
}

#[test]
fn rename_to_same_name_is_a_no_op_and_other_kind_is_not_a_clash() {
    let (mut client, _) = Client::start();
    let u = uri("rename_same");
    let text = "enum E { A }\nfn f(x: i32) -> i32 { x }\nrange(E).map(f).return\n";
    client.open_at(&u, text);
    let same = edits_of(rename(&mut client, &u, pos_of(text, "f)"), "f"), &u);
    assert!(same.is_empty());
    let cross = rename(&mut client, &u, pos_of(text, "f)"), "E");
    assert_eq!(edits_of(cross, &u).len(), 2);
    client.shutdown();
}

#[test]
fn rename_null_for_unopened_nothing_and_error_for_undeclared() {
    let (mut client, _) = Client::start();
    let u = uri("rename_edge");
    let text = "range(0, 3).map(missing).return\n";
    client.open_at(&u, text);
    assert!(rename(&mut client, &uri("nope"), Position::new(0, 0), "a")
        .response_result
        .unwrap()
        .is_null());
    assert!(rename(&mut client, &u, Position::new(0, 2), "a")
        .response_result
        .unwrap()
        .is_null());
    let undeclared = rename(&mut client, &u, pos_of(text, "missing"), "found");
    assert_eq!(
        error_code(undeclared),
        lsp_server::ErrorCode::RequestFailed as i32
    );
    client.shutdown();
}

#[test]
fn rename_after_multibyte_text_edits_correct_range() {
    let (mut client, _) = Client::start();
    let u = uri("rename_utf16");
    let text =
        "struct Row { a: int[0, 5] }\nread_file(\"\u{1F600}\u{e9}.csv\", csv, struct=Row).return\n";
    client.open_at(&u, text);
    let at = pos_of(text, "Row).return");
    let edits = edits_of(rename(&mut client, &u, at, "Record"), &u);
    assert_eq!(edits.len(), 2);
    assert!(edits.iter().any(|e| e.range.start == at));
    client.shutdown();
}

#[test]
fn rename_with_hostile_positions_never_panics() {
    let (mut client, _) = Client::start();
    let u = uri("rename_hostile");
    client.open_at(&u, PROGRAM);
    for p in [
        Position::new(u32::MAX, u32::MAX),
        Position::new(0, u32::MAX),
        Position::new(3, 9999),
    ] {
        let _ = rename(&mut client, &u, p, "z");
    }
    client.shutdown();
}

fn apply(text: &str, edits: &[TextEdit]) -> String {
    let lines = marigold_lsp::position::LineIndex::new(text);
    let mut spans: Vec<(usize, usize, &str)> = edits
        .iter()
        .map(|e| {
            (
                lines.offset(e.range.start),
                lines.offset(e.range.end),
                e.new_text.as_str(),
            )
        })
        .collect();
    spans.sort_by_key(|s| std::cmp::Reverse(s.0));
    let mut out = text.to_string();
    for (start, end, new) in spans {
        out.replace_range(start..end, new);
    }
    out
}

fn errors(text: &str) -> usize {
    marigold_grammar::marigold_check(text)
        .iter()
        .filter(|d| d.severity == marigold_grammar::diagnostics::Severity::Error)
        .count()
}

const SAMPLES: &[&str] = &[
    "x = range(0, 5)\nx.return",
    "x = range(0, 5).map(f)\nx.filter(g).return",
    "enum Color { Red, Green }\nrange(Color).return",
    "struct Row { a: int[0, 5] }\nread_file(\"d.csv\", csv, struct=Row).return",
    "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
    "y = range(0, 3).filter_map(fm)\nz = y.fold(0, add)\nz.keep_first_n(3, cmp).return",
    "x = range(0, 5)\nx = x.map(f)\nx.return",
    "range(0, 1).write_file(\"\u{fc}n\u{ef}c\u{f8}d\u{e9}.csv\", csv)\nq = range(0, 2)\nq.return",
];

proptest! {
    #[test]
    fn rename_never_introduces_errors(
        sample in 0..SAMPLES.len(),
        which in 0usize..16,
        name in "[A-Za-z_][A-Za-z0-9_]{0,10}",
    ) {
        let text = SAMPLES[sample];
        let symbols = marigold_grammar::marigold_symbols(text);
        let mut places: Vec<usize> = symbols
            .declarations
            .iter()
            .map(|d| d.range.start)
            .chain(symbols.references.iter().map(|r| r.range.start))
            .collect();
        places.sort();
        let offset = places[which % places.len()];
        let position = pos_at(text, offset);
        let before = errors(text);
        if let Ok(edits) = marigold_lsp::nav::rename_edits(text, position, &name) {
            let renamed = apply(text, &edits);
            prop_assert!(errors(&renamed) <= before, "{renamed:?}");
        }
    }
}
