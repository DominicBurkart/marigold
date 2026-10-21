#![cfg(feature = "otel")]

use futures::executor::block_on;
use futures::StreamExt;
use marigold_impl::telemetry::{instrument, instrument_in, NodeMeta};
use opentelemetry::{global, Value};
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};
use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};

const NODE: NodeMeta = NodeMeta::new(
    "mg1-1234567890abcdef",
    "span_demo",
    "src/pipeline.marigold",
    40,
    62,
);

#[test]
fn file_and_range_are_span_attributes_and_never_metric_attributes() {
    let metric_exporter = InMemoryMetricExporter::default();
    let meter_provider = SdkMeterProvider::builder()
        .with_reader(PeriodicReader::builder(metric_exporter.clone()).build())
        .build();
    let span_exporter = InMemorySpanExporter::default();
    let tracer_provider = SdkTracerProvider::builder()
        .with_batch_exporter(span_exporter.clone())
        .build();
    global::set_meter_provider(meter_provider.clone());
    global::set_tracer_provider(tracer_provider.clone());

    let out: Vec<u32> =
        block_on(instrument(instrument_in(futures::stream::iter(0..20u32), NODE), NODE).collect());
    assert_eq!(out.len(), 20);

    meter_provider.force_flush().unwrap();
    tracer_provider.force_flush().unwrap();

    let spans = span_exporter.get_finished_spans().unwrap();
    assert_eq!(spans.len(), 1);
    let attrs: std::collections::BTreeMap<String, Value> = spans[0]
        .attributes
        .iter()
        .map(|kv| (kv.key.to_string(), kv.value.clone()))
        .collect();
    assert_eq!(attrs["marigold.node_id"].to_string(), NODE.node_id);
    assert_eq!(attrs["marigold.file"].to_string(), "src/pipeline.marigold");
    assert_eq!(attrs["marigold.range.start"], Value::I64(40));
    assert_eq!(attrs["marigold.range.end"], Value::I64(62));

    let finished = metric_exporter.get_finished_metrics().unwrap();
    let rendered = format!("{finished:?}");
    assert!(rendered.contains("marigold.node_id"));
    assert!(rendered.contains("marigold.node.items_out"));
    assert!(!rendered.contains("marigold.file"));
    assert!(!rendered.contains("marigold.range"));
    assert!(!rendered.contains("pipeline.marigold"));
}
