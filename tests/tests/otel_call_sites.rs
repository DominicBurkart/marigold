#![cfg(feature = "otel")]

use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::marigold_node_ids;
use opentelemetry_sdk::metrics::data::{AggregatedMetrics, MetricData};
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};
use std::collections::BTreeMap;

fn double(v: i32) -> i32 {
    v * 2
}

fn items_out(exporter: &InMemoryMetricExporter) -> BTreeMap<String, u64> {
    let finished = exporter.get_finished_metrics().unwrap();
    let mut out = BTreeMap::new();
    for scope in finished.last().unwrap().scope_metrics() {
        for metric in scope.metrics() {
            if metric.name() != "marigold.node.items_out" {
                continue;
            }
            if let AggregatedMetrics::U64(MetricData::Sum(sum)) = metric.data() {
                for dp in sum.data_points() {
                    let id = dp
                        .attributes()
                        .find(|kv| kv.key.as_str() == "marigold.node_id")
                        .unwrap()
                        .value
                        .to_string();
                    out.insert(id, dp.value());
                }
            }
        }
    }
    out
}

#[tokio::test]
async fn two_invocations_with_the_same_names_report_separately() {
    let exporter = InMemoryMetricExporter::default();
    let provider = SdkMeterProvider::builder()
        .with_reader(PeriodicReader::builder(exporter.clone()).build())
        .build();
    marigold::telemetry::opentelemetry::global::set_meter_provider(provider.clone());

    let a = m!(
        digits = range(0, 10)
        digits.map(double).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    let b = m!(
        digits = range(0, 3)
        digits.map(double).return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    let c = m!(range(0, 4).return).await.collect::<Vec<_>>().await;
    let d = m!(range(0, 4).return).await.collect::<Vec<_>>().await;
    assert_eq!((a.len(), b.len(), c.len(), d.len()), (10, 3, 4, 4));

    provider.force_flush().unwrap();
    let counts = items_out(&exporter);

    let digits_decl = "digits = range(0, 10)\ndigits.map(double).return";
    let plain: Vec<_> = marigold_node_ids(digits_decl, "otel_call_sites")
        .into_iter()
        .filter(|n| n.kind != marigold_grammar::node_ids::NodeKind::VariableDeclaration)
        .map(|n| n.id.to_string())
        .collect();
    let totals: Vec<u64> = counts.values().copied().collect();
    assert_eq!(counts.len(), 4 + 4 + 2 + 2, "{counts:?}");
    assert_eq!(totals.iter().filter(|v| **v == 10).count(), 4);
    assert_eq!(totals.iter().filter(|v| **v == 3).count(), 4);
    assert_eq!(totals.iter().filter(|v| **v == 4).count(), 4);
    assert!(plain.iter().all(|id| counts.contains_key(id)));

    let raw = "unique_stream = range(0, 6)\n\nunique_stream\n    .map(double)\n    .return";
    let out = m!(
        unique_stream = range(0, 6)

        unique_stream
            .map(double)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(out.len(), 6);

    provider.force_flush().unwrap();
    let counts = items_out(&exporter);
    assert_eq!(counts.len(), 4 + 4 + 2 + 2 + 4, "{counts:?}");
    for node in marigold_node_ids(raw, "otel_call_sites") {
        if node.kind != marigold_grammar::node_ids::NodeKind::VariableDeclaration {
            assert!(counts.contains_key(node.id.as_str()), "{}", node.id);
        }
    }
}
