#![cfg(feature = "telemetry")]

mod common;

use common::{pos_of, uri, Client};
use lsp_types::request::{HoverRequest, InlayHintRequest};
use lsp_types::{
    Hover, HoverContents, HoverParams, InlayHint, InlayHintLabel, InlayHintParams, Position, Range,
    TextDocumentIdentifier, TextDocumentPositionParams,
};
use marigold_grammar::marigold_node_ids;
use marigold_lsp::telemetry::{NodeStats, StaticSource, TelemetryError, TelemetrySource};
use std::time::Duration;

const SRC: &str = "x = range(0, 5).map(f)\nx.return";
const CAPABILITIES: &str = include_str!("golden/capabilities.json");

fn stats(src: &str, pick: usize, hash: Option<String>) -> NodeStats {
    let nodes = marigold_node_ids(src, "telemetry-doc");
    NodeStats {
        node_id: nodes[pick].id.clone(),
        observed_inputs: 40_000,
        observed_runs: 3,
        errors: 2,
        last_seen: Some(1),
        window: Duration::from_secs(86_400),
        content_hash: hash,
    }
}

fn source(stats: Vec<NodeStats>) -> Option<Box<dyn TelemetrySource + Send>> {
    Some(Box::new(StaticSource::new(stats)))
}

fn hover_text(client: &mut Client, u: &lsp_types::Uri, at: Position) -> Option<String> {
    let value = client.request::<HoverRequest>(HoverParams {
        text_document_position_params: TextDocumentPositionParams::new(
            TextDocumentIdentifier::new(u.clone()),
            at,
        ),
        work_done_progress_params: Default::default(),
    });
    let hover: Option<Hover> = serde_json::from_value(value).unwrap();
    hover.map(|h| match h.contents {
        HoverContents::Markup(m) => m.value,
        _ => panic!("expected markup"),
    })
}

fn hints(client: &mut Client, u: &lsp_types::Uri, range: Range) -> Vec<InlayHint> {
    let value = client.request::<InlayHintRequest>(InlayHintParams {
        work_done_progress_params: Default::default(),
        text_document: TextDocumentIdentifier::new(u.clone()),
        range,
    });
    serde_json::from_value::<Option<Vec<InlayHint>>>(value)
        .unwrap()
        .unwrap_or_default()
}

fn label(hint: &InlayHint) -> String {
    match &hint.label {
        InlayHintLabel::String(s) => s.clone(),
        InlayHintLabel::LabelParts(parts) => parts.iter().map(|p| p.value.clone()).collect(),
    }
}

fn whole() -> Range {
    Range::new(Position::new(0, 0), Position::new(100, 0))
}

#[test]
fn advertises_inlay_hints_only_with_a_source() {
    let (client, with) = Client::start_telemetry(source(vec![]));
    assert!(with.capabilities.inlay_hint_provider.is_some());
    client.shutdown();
    let (client, without) = Client::start_telemetry(None);
    assert!(without.capabilities.inlay_hint_provider.is_none());
    assert_eq!(serde_json::to_string(&without).unwrap(), CAPABILITIES);
    client.shutdown();
}

#[test]
fn hover_gains_an_observed_line() {
    let u = uri("telemetry-doc");
    let (mut client, _) = Client::start_telemetry(source(vec![stats(SRC, 2, None)]));
    client.open_at(&u, SRC);
    let text = hover_text(&mut client, &u, pos_of(SRC, "map")).unwrap();
    assert!(text.contains("cardinality:"), "{text}");
    assert!(
        text.ends_with("\nobserved: 40000 inputs \u{b7} 3 runs \u{b7} 2 errors (last 24h)"),
        "{text}"
    );
    client.shutdown();
}

#[test]
fn hover_without_stats_is_unchanged() {
    let u = uri("telemetry-doc");
    let (mut client, _) = Client::start_telemetry(source(vec![]));
    client.open_at(&u, SRC);
    let with_source = hover_text(&mut client, &u, pos_of(SRC, "map")).unwrap();
    client.shutdown();
    let (mut plain, _) = Client::start();
    plain.open_at(&u, SRC);
    let baseline = hover_text(&mut plain, &u, pos_of(SRC, "map")).unwrap();
    plain.shutdown();
    assert_eq!(with_source, baseline);
}

#[test]
fn stale_annotations_are_flagged() {
    let u = uri("telemetry-doc");
    let (mut client, _) =
        Client::start_telemetry(source(vec![stats(SRC, 2, Some("0000000000000000".into()))]));
    client.open_at(&u, SRC);
    let text = hover_text(&mut client, &u, pos_of(SRC, "map")).unwrap();
    assert!(text.ends_with("(last 24h) (stale)"), "{text}");
    let shown = hints(&mut client, &u, whole());
    assert!(label(&shown[0]).ends_with("(stale)"));
    client.shutdown();
}

#[test]
fn source_failure_degrades_without_breaking_hover_or_diagnostics() {
    let u = uri("telemetry-doc");
    let failing: Option<Box<dyn TelemetrySource + Send>> = Some(Box::new(StaticSource::failing(
        TelemetryError::Unavailable("down".into()),
    )));
    let (mut client, _) = Client::start_telemetry(failing);
    client.open_at(&u, SRC);
    let text = hover_text(&mut client, &u, pos_of(SRC, "map")).unwrap();
    assert!(text.contains("cardinality:") && !text.contains("observed:"));
    assert!(hints(&mut client, &u, whole()).is_empty());
    client.shutdown();
}

#[test]
fn inlay_hints_are_short_and_respect_the_requested_range() {
    let u = uri("telemetry-doc");
    let all = vec![stats(SRC, 2, None), stats(SRC, 4, None)];
    let (mut client, _) = Client::start_telemetry(source(all));
    client.open_at(&u, SRC);
    let shown = hints(&mut client, &u, whole());
    assert_eq!(shown.len(), 2);
    assert_eq!(label(&shown[0]), "40000 in \u{b7} 3 runs \u{b7} 2 err");
    assert_eq!(shown[0].position, Position::new(0, 22));
    assert_eq!(shown[1].position, Position::new(1, 8));
    let first_line = Range::new(Position::new(0, 0), Position::new(0, 22));
    assert_eq!(hints(&mut client, &u, first_line).len(), 1);
    client.shutdown();
}

#[test]
fn unknown_document_has_no_hints() {
    let (mut client, _) = Client::start_telemetry(source(vec![]));
    assert!(hints(&mut client, &uri("never-opened"), whole()).is_empty());
    client.shutdown();
}

#[test]
fn feature_on_without_a_source_rejects_inlay_hints_like_today() {
    let (mut client, _) = Client::start_telemetry(None);
    let response = client.call_raw(
        "textDocument/inlayHint",
        serde_json::json!({"textDocument": {"uri": "file:///tmp/a.marigold"}, "range": whole()}),
    );
    assert!(response.response_result.is_err());
    client.shutdown();
}
