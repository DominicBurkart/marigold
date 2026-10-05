#![cfg(feature = "otel")]

use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::marigold_node_ids;
use opentelemetry_sdk::metrics::data::{AggregatedMetrics, MetricData};
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};
use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};
use std::collections::BTreeMap;

fn double(v: i32) -> i32 {
    v * 2
}

fn is_even(v: i32) -> bool {
    v % 2 == 0
}

fn is_odd(v: i32) -> bool {
    v % 2 == 1
}

const PROGRAM: &str = "otel_instrumentation";

type Counts = BTreeMap<(String, String), u64>;

fn counters(exporter: &InMemoryMetricExporter) -> Counts {
    let finished = exporter.get_finished_metrics().unwrap();
    let mut out = Counts::new();
    for scope in finished.last().unwrap().scope_metrics() {
        for metric in scope.metrics() {
            if let AggregatedMetrics::U64(MetricData::Sum(sum)) = metric.data() {
                for dp in sum.data_points() {
                    let attrs: BTreeMap<String, String> = dp
                        .attributes()
                        .map(|kv| (kv.key.to_string(), kv.value.to_string()))
                        .collect();
                    assert_eq!(
                        attrs.keys().map(String::as_str).collect::<Vec<_>>(),
                        ["marigold.node_id", "marigold.program"]
                    );
                    assert_eq!(attrs["marigold.program"], PROGRAM);
                    out.insert(
                        (metric.name().to_string(), attrs["marigold.node_id"].clone()),
                        dp.value(),
                    );
                }
            }
        }
    }
    out
}

#[tokio::test]
async fn instrumented_programs_match_plain_rust_and_report_per_node_counts() {
    let metric_exporter = InMemoryMetricExporter::default();
    let meter_provider = SdkMeterProvider::builder()
        .with_reader(PeriodicReader::builder(metric_exporter.clone()).build())
        .build();
    let span_exporter = InMemorySpanExporter::default();
    let tracer_provider = SdkTracerProvider::builder()
        .with_batch_exporter(span_exporter.clone())
        .build();
    marigold::telemetry::opentelemetry::global::set_meter_provider(meter_provider.clone());
    marigold::telemetry::opentelemetry::global::set_tracer_provider(tracer_provider.clone());

    let chain_src = "range(0, 10).map(double).filter(is_even).return";
    let chain = m!(range(0, 10).map(double).filter(is_even).return)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(
        chain,
        (0..10)
            .map(double)
            .filter(|v| is_even(*v))
            .collect::<Vec<_>>()
    );

    let perms_src = "range(0, 4).permutations(2).return";
    let perms = m!(range(0, 4).permutations(2).return)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(perms.len(), 12);
    assert_eq!(perms[0], [0, 1]);
    assert_eq!(perms[11], [3, 2]);

    let fold_src = "range(0, 100).fold(0, add).return";
    fn add(acc: i32, v: i32) -> i32 {
        acc + v
    }
    let folded = m!(range(0, 100).fold(0, add).return)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(folded, vec![4950]);

    let variable_src = "digits = range(0, 10)\ndigits.filter(is_odd).return\ndigits.return";
    let (odds, all) = {
        let both = m!(
            digits = range(0, 10)
            digits.filter(is_odd).return
            digits.return
        )
        .await
        .collect::<Vec<_>>()
        .await;
        let mut odds: Vec<i32> = both.iter().copied().filter(|v| is_odd(*v)).collect();
        odds.sort_unstable();
        let mut all = both;
        all.sort_unstable();
        (odds, all)
    };
    assert_eq!(all.len(), 15);
    assert_eq!(odds, vec![1, 1, 3, 3, 5, 5, 7, 7, 9, 9]);

    let select_src = "select_all(range(0, 10), range(10, 20)).return";
    let mut selected = m!(select_all(range(0, 10), range(10, 20)).return)
        .await
        .collect::<Vec<i32>>()
        .await;
    selected.sort_unstable();
    assert_eq!(selected, (0..20).collect::<Vec<_>>());

    meter_provider.force_flush().unwrap();
    tracer_provider.force_flush().unwrap();
    let counts = counters(&metric_exporter);
    let get = |name: &str, id: &str| counts.get(&(name.to_string(), id.to_string())).copied();

    let ids = |src: &str| {
        marigold_node_ids(src, PROGRAM)
            .into_iter()
            .map(|n| n.id.to_string())
            .collect::<Vec<_>>()
    };

    let chain_ids = ids(chain_src);
    assert_eq!(chain_ids.len(), 4);
    assert_eq!(get("marigold.node.items_out", &chain_ids[0]), Some(10));
    assert_eq!(get("marigold.node.items_in", &chain_ids[1]), Some(10));
    assert_eq!(get("marigold.node.items_out", &chain_ids[1]), Some(10));
    assert_eq!(get("marigold.node.items_in", &chain_ids[2]), Some(10));
    assert_eq!(get("marigold.node.items_out", &chain_ids[2]), Some(10));
    assert_eq!(get("marigold.node.items_in", &chain_ids[3]), Some(10));
    assert_eq!(get("marigold.node.items_out", &chain_ids[3]), Some(10));
    assert_eq!(get("marigold.node.runs", &chain_ids[3]), Some(1));

    let perm_ids = ids(perms_src);
    assert_eq!(get("marigold.node.items_in", &perm_ids[1]), Some(4));
    assert_eq!(get("marigold.node.items_out", &perm_ids[1]), Some(12));

    let fold_ids = ids(fold_src);
    assert_eq!(get("marigold.node.items_in", &fold_ids[1]), Some(100));
    assert_eq!(get("marigold.node.items_out", &fold_ids[1]), Some(1));

    let var_nodes = marigold_node_ids(variable_src, PROGRAM);
    assert_eq!(var_nodes.len(), 7);
    for node in var_nodes
        .iter()
        .filter(|n| n.kind != marigold_grammar::node_ids::NodeKind::VariableDeclaration)
    {
        let id = node.id.as_str();
        assert!(counts.keys().any(|(_, k)| k == id), "{id} missing");
    }

    let select_ids = ids(select_src);
    assert_eq!(get("marigold.node.items_out", &select_ids[0]), Some(20));

    let spans = span_exporter.get_finished_spans().unwrap();
    assert!(!spans.is_empty());
    for span in &spans {
        let file = span
            .attributes
            .iter()
            .find(|kv| kv.key.as_str() == "marigold.file")
            .expect("span carries marigold.file")
            .value
            .to_string();
        assert!(file.ends_with("otel_instrumentation.rs"), "{file}");
    }
}
