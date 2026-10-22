#![cfg(feature = "otel")]

use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::marigold_node_ids;
use opentelemetry_sdk::metrics::data::{AggregatedMetrics, MetricData};
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};
use std::collections::BTreeMap;
use std::time::Duration;

const PROGRAM: &str = "otel_named_collecting";

#[tokio::test]
async fn named_collecting_step_completes_and_reports_node_counts() {
    let exporter = InMemoryMetricExporter::default();
    let provider = SdkMeterProvider::builder()
        .with_reader(PeriodicReader::builder(exporter.clone()).build())
        .build();
    marigold::telemetry::opentelemetry::global::set_meter_provider(provider.clone());

    let src = "digits = range(0, 4)\ndigits.permutations(2).return";
    let mut out = tokio::time::timeout(Duration::from_secs(20), async {
        m!(
            digits = range(0, 4)
            digits.permutations(2).return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await
    .expect("HANG: instrumented named collecting program did not complete");
    out.sort();
    assert_eq!(out.len(), 12);
    assert_eq!(out[0], [0, 1]);
    assert_eq!(out[11], [3, 2]);

    provider.force_flush().unwrap();
    let finished = exporter.get_finished_metrics().unwrap();
    let mut counts: BTreeMap<(String, String), u64> = BTreeMap::new();
    for scope in finished.last().unwrap().scope_metrics() {
        for metric in scope.metrics() {
            if let AggregatedMetrics::U64(MetricData::Sum(sum)) = metric.data() {
                for dp in sum.data_points() {
                    let id = dp
                        .attributes()
                        .find(|kv| kv.key.as_str() == "marigold.node_id")
                        .unwrap()
                        .value
                        .to_string();
                    counts.insert((metric.name().to_string(), id), dp.value());
                }
            }
        }
    }
    let ids: Vec<String> = marigold_node_ids(src, PROGRAM)
        .into_iter()
        .filter(|n| n.kind != marigold_grammar::node_ids::NodeKind::VariableDeclaration)
        .map(|n| n.id.to_string())
        .collect();
    assert_eq!(ids.len(), 4);
    for id in &ids {
        assert!(counts.keys().any(|(_, k)| k == id), "{id} missing");
    }
    let get = |name: &str, id: &str| counts.get(&(name.to_string(), id.to_string())).copied();
    let perm_id = ids
        .iter()
        .find(|id| get("marigold.node.items_out", id) == Some(12))
        .expect("permutations node reports 12 items out");
    assert_eq!(get("marigold.node.items_in", perm_id), Some(4));
}
