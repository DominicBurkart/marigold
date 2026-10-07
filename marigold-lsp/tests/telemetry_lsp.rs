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
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{channel, Receiver};
use std::sync::{Arc, Mutex};
use std::time::Duration;

struct Counting {
    calls: Arc<AtomicUsize>,
    gate: Option<Mutex<Receiver<()>>>,
}

impl TelemetrySource for Counting {
    fn node_stats(
        &self,
        _program: &str,
        ids: &[marigold_grammar::NodeId],
    ) -> Result<Vec<NodeStats>, TelemetryError> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        if let Some(gate) = &self.gate {
            let _ = gate.lock().unwrap().recv();
        }
        Ok(ids
            .iter()
            .map(|id| NodeStats {
                node_id: id.clone(),
                observed_inputs: 40_000,
                observed_runs: 3,
                errors: 2,
                last_seen: Some(1),
                window: Duration::from_secs(86_400),
                content_hash: None,
            })
            .collect())
    }
}

fn counting(
    gated: bool,
) -> (
    Arc<AtomicUsize>,
    std::sync::mpsc::Sender<()>,
    Option<Box<dyn TelemetrySource + Send>>,
) {
    let calls = Arc::new(AtomicUsize::new(0));
    let (tx, rx) = channel();
    let source = Counting {
        calls: Arc::clone(&calls),
        gate: gated.then(|| Mutex::new(rx)),
    };
    (calls, tx, Some(Box::new(source)))
}

fn change(client: &Client, u: &lsp_types::Uri, version: i32, text: &str) {
    client.notify::<lsp_types::notification::DidChangeTextDocument>(
        lsp_types::DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier::new(u.clone(), version),
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: text.to_string(),
            }],
        },
    );
}

fn settle(client: &mut Client, u: &lsp_types::Uri, at: Position) -> String {
    hover_text(client, u, at).unwrap();
    client.wait_for_refresh();
    hover_text(client, u, at).unwrap()
}

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
    let (mut client, _) = Client::start_telemetry_refreshing(source(vec![stats(SRC, 2, None)]));
    client.open_at(&u, SRC);
    let text = settle(&mut client, &u, pos_of(SRC, "map"));
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
    let (mut client, _) = Client::start_telemetry_refreshing(source(vec![stats(
        SRC,
        2,
        Some("0000000000000000".into()),
    )]));
    client.open_at(&u, SRC);
    let text = settle(&mut client, &u, pos_of(SRC, "map"));
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
    let (mut client, _) = Client::start_telemetry_refreshing(source(all));
    client.open_at(&u, SRC);
    assert!(hints(&mut client, &u, whole()).is_empty());
    client.wait_for_refresh();
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

#[test]
fn hover_and_hints_do_not_wait_for_a_blocked_source() {
    let u = uri("telemetry-doc");
    let (calls, gate, source) = counting(true);
    let (mut client, _) = Client::start_telemetry_refreshing(source);
    client.open_at(&u, SRC);
    let at = pos_of(SRC, "map");
    let before = hover_text(&mut client, &u, at).unwrap();
    assert!(before.contains("cardinality:") && !before.contains("observed:"));
    assert!(hints(&mut client, &u, whole()).is_empty());
    assert!(hover_text(&mut client, &u, at)
        .unwrap()
        .find("observed:")
        .is_none());
    gate.send(()).unwrap();
    client.wait_for_refresh();
    assert!(hover_text(&mut client, &u, at)
        .unwrap()
        .contains("observed: 40000 inputs"));
    assert_eq!(hints(&mut client, &u, whole()).len(), 5);
    assert_eq!(calls.load(Ordering::SeqCst), 1);
    client.shutdown();
}

#[test]
fn text_edits_that_keep_node_ids_reuse_the_cached_stats() {
    let u = uri("telemetry-doc");
    let other = uri("telemetry-other");
    let (calls, _gate, source) = counting(false);
    let (mut client, _) = Client::start_telemetry_refreshing(source);
    client.open_at(&u, SRC);
    let at = pos_of(SRC, "map");
    settle(&mut client, &u, at);
    let edited = "x = range(0, 5).map(g)\nx.return";
    assert_eq!(
        marigold_node_ids(SRC, "telemetry-doc")
            .iter()
            .map(|n| &n.id)
            .collect::<Vec<_>>(),
        marigold_node_ids(edited, "telemetry-doc")
            .iter()
            .map(|n| &n.id)
            .collect::<Vec<_>>()
    );
    change(&client, &u, 2, edited);
    assert!(hover_text(&mut client, &u, at)
        .unwrap()
        .contains("observed:"));
    assert_eq!(hints(&mut client, &u, whole()).len(), 5);
    client.open_at(&other, SRC);
    settle(&mut client, &other, at);
    assert_eq!(calls.load(Ordering::SeqCst), 2);
    client.shutdown();
}

#[test]
fn edits_that_change_node_ids_query_again() {
    let u = uri("telemetry-doc");
    let (calls, _gate, source) = counting(false);
    let (mut client, _) = Client::start_telemetry_refreshing(source);
    client.open_at(&u, SRC);
    let at = pos_of(SRC, "map");
    settle(&mut client, &u, at);
    let edited = "x = range(0, 5).map(f).filter(h)\nx.return";
    assert_ne!(
        marigold_node_ids(SRC, "telemetry-doc").len(),
        marigold_node_ids(edited, "telemetry-doc").len()
    );
    change(&client, &u, 2, edited);
    let served = hover_text(&mut client, &u, at).unwrap();
    assert!(served.contains("observed:"), "{served}");
    client.wait_for_refresh();
    assert_eq!(calls.load(Ordering::SeqCst), 2);
    client.shutdown();
}
