#![cfg(feature = "otel")]

use futures::executor::block_on;
use futures::{Stream, StreamExt};
use marigold_impl::telemetry::{
    instrument_in_results_with, instrument_in_with, instrument_results_with, instrument_with,
    resolve_program, NodeMeta, ABORT_ERRORS, DURATION, ITEMS_IN, ITEMS_OUT, ITEM_ERRORS, RUNS,
};
use opentelemetry::metrics::{Meter, MeterProvider};
use opentelemetry_sdk::metrics::data::{AggregatedMetrics, MetricData};
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};
use std::collections::BTreeMap;
use std::panic::{catch_unwind, AssertUnwindSafe};

const A: NodeMeta = NodeMeta::new("mg1-aaaaaaaaaaaaaaaa", "demo", "demo.marigold", 0, 10);
const B: NodeMeta = NodeMeta::new("mg1-bbbbbbbbbbbbbbbb", "demo", "demo.marigold", 11, 25);

struct Harness {
    provider: SdkMeterProvider,
    exporter: InMemoryMetricExporter,
}

#[derive(Debug)]
struct Point {
    name: String,
    attributes: BTreeMap<String, String>,
    value: f64,
}

impl Harness {
    fn new() -> Self {
        let exporter = InMemoryMetricExporter::default();
        let provider = SdkMeterProvider::builder()
            .with_reader(PeriodicReader::builder(exporter.clone()).build())
            .build();
        Self { provider, exporter }
    }

    fn meter(&self) -> Meter {
        self.provider.meter("test")
    }

    fn points(&self) -> Vec<Point> {
        self.provider.force_flush().unwrap();
        let finished = self.exporter.get_finished_metrics().unwrap();
        let Some(last) = finished.last() else {
            return Vec::new();
        };
        let mut points = Vec::new();
        for scope in last.scope_metrics() {
            for metric in scope.metrics() {
                let name = metric.name().to_string();
                let mut push = |attrs: Vec<(String, String)>, value: f64| {
                    points.push(Point {
                        name: name.clone(),
                        attributes: attrs.into_iter().collect(),
                        value,
                    })
                };
                let attrs = |it: &mut dyn Iterator<Item = &opentelemetry::KeyValue>| {
                    it.map(|kv| (kv.key.to_string(), kv.value.to_string()))
                        .collect::<Vec<_>>()
                };
                match metric.data() {
                    AggregatedMetrics::U64(MetricData::Sum(sum)) => {
                        for dp in sum.data_points() {
                            push(attrs(&mut dp.attributes()), dp.value() as f64);
                        }
                    }
                    AggregatedMetrics::F64(MetricData::Histogram(h)) => {
                        for dp in h.data_points() {
                            push(attrs(&mut dp.attributes()), dp.count() as f64);
                        }
                    }
                    other => panic!("unexpected metric data: {other:?}"),
                }
            }
        }
        points
    }

    fn value(&self, name: &str, node_id: &str) -> Option<f64> {
        self.points()
            .into_iter()
            .find(|p| {
                p.name == name
                    && p.attributes.get("marigold.node_id").map(String::as_str) == Some(node_id)
            })
            .map(|p| p.value)
    }
}

#[test]
fn counts_items_in_and_out_per_node() {
    let h = Harness::new();
    let m = h.meter();
    let source = instrument_with(futures::stream::iter(0..1000u32), A, &m);
    let filtered = instrument_in_with(source, B, &m).filter(|v| futures::future::ready(v % 2 == 0));
    let out = block_on(instrument_with(filtered, B, &m).collect::<Vec<_>>());
    assert_eq!(out.len(), 500);

    assert_eq!(h.value(ITEMS_OUT, A.node_id), Some(1000.0));
    assert_eq!(h.value(ITEMS_IN, A.node_id), None);
    assert_eq!(h.value(ITEMS_IN, B.node_id), Some(1000.0));
    assert_eq!(h.value(ITEMS_OUT, B.node_id), Some(500.0));
    assert_eq!(h.value(RUNS, A.node_id), Some(1.0));
    assert_eq!(h.value(RUNS, B.node_id), Some(1.0));
    assert_eq!(h.value(ITEM_ERRORS, A.node_id), None);
    assert_eq!(h.value(ABORT_ERRORS, A.node_id), None);
}

#[test]
fn metrics_carry_only_program_and_node_id_attributes() {
    let h = Harness::new();
    let m = h.meter();
    let items: Vec<Result<u32, String>> = (0..300)
        .map(|i| {
            if i % 7 == 0 {
                Err("bad".to_string())
            } else {
                Ok(i)
            }
        })
        .collect();
    let s = instrument_results_with(futures::stream::iter(items), A, &m);
    let s = instrument_in_results_with(s, B, &m);
    let _ = block_on(instrument_with(s, B, &m).collect::<Vec<_>>());

    let points = h.points();
    assert!(points.len() >= 6, "{points:?}");
    for p in &points {
        let keys: Vec<&str> = p.attributes.keys().map(String::as_str).collect();
        assert_eq!(keys, ["marigold.node_id", "marigold.program"], "{}", p.name);
        assert_eq!(p.attributes["marigold.program"], resolve_program("demo"));
    }
    let names: std::collections::BTreeSet<_> = points.iter().map(|p| p.name.as_str()).collect();
    assert!(names.contains(DURATION));
    assert!(names.contains(ITEM_ERRORS));
}

#[test]
fn program_attribute_is_the_codegen_name_without_override() {
    let h = Harness::new();
    let m = h.meter();
    let _ = block_on(instrument_with(futures::stream::iter(0..3u8), A, &m).collect::<Vec<_>>());
    let points = h.points();
    assert!(!points.is_empty());
    if std::env::var_os("OTEL_SERVICE_NAME").is_none() {
        assert!(points
            .iter()
            .all(|p| p.attributes["marigold.program"] == "demo"));
    }
}

#[test]
fn counts_result_errors_at_input_and_output_roles() {
    let h = Harness::new();
    let m = h.meter();
    let items: Vec<Result<u32, String>> = (0..100)
        .map(|i| {
            if i % 4 == 0 {
                Err(format!("e{i}"))
            } else {
                Ok(i)
            }
        })
        .collect();
    let s = instrument_results_with(futures::stream::iter(items), A, &m);
    let s = instrument_in_results_with(s, B, &m)
        .filter(|r| futures::future::ready(r.is_ok()))
        .map(|r| r.unwrap());
    let out = block_on(instrument_with(s, B, &m).collect::<Vec<_>>());
    assert_eq!(out.len(), 75);

    assert_eq!(h.value(ITEMS_OUT, A.node_id), Some(100.0));
    assert_eq!(h.value(ITEM_ERRORS, A.node_id), Some(25.0));
    assert_eq!(h.value(ITEMS_IN, B.node_id), Some(100.0));
    assert_eq!(h.value(ITEMS_OUT, B.node_id), Some(75.0));
    assert!(h.value(ITEM_ERRORS, B.node_id).is_some());
}

#[test]
fn counts_aborts_when_the_stream_panics() {
    let h = Harness::new();
    let m = h.meter();
    let s = futures::stream::iter(0..10u32).map(|v| {
        if v == 4 {
            panic!("boom");
        }
        v
    });
    let s = instrument_with(s, A, &m);
    let result = catch_unwind(AssertUnwindSafe(|| {
        block_on(s.collect::<Vec<_>>());
    }));
    assert!(result.is_err());

    assert_eq!(h.value(ABORT_ERRORS, A.node_id), Some(1.0));
    assert_eq!(h.value(ITEMS_OUT, A.node_id), Some(4.0));
    assert_eq!(h.value(RUNS, A.node_id), Some(1.0));
}

#[test]
fn stream_dropped_early_is_not_an_abort_and_keeps_its_count() {
    let h = Harness::new();
    let m = h.meter();
    let mut s = Box::pin(instrument_with(futures::stream::iter(0..100u32), A, &m));
    for _ in 0..10 {
        block_on(s.next());
    }
    drop(s);
    assert_eq!(h.value(ITEMS_OUT, A.node_id), Some(10.0));
    assert_eq!(h.value(ABORT_ERRORS, A.node_id), None);
}

#[test]
fn latency_histogram_is_sampled() {
    let h = Harness::new();
    let m = h.meter();
    let n = marigold_impl::telemetry::LATENCY_SAMPLE_EVERY * 4;
    let _ = block_on(instrument_with(futures::stream::iter(0..n), A, &m).collect::<Vec<_>>());
    assert_eq!(h.value(DURATION, A.node_id), Some(4.0));
}

#[test]
fn results_and_order_are_unchanged() {
    let h = Harness::new();
    let m = h.meter();
    let plain: Vec<u64> = block_on(
        futures::stream::iter(0..2000u64)
            .map(|v| async move { v * 3 })
            .buffered(4)
            .filter(|v| futures::future::ready(v % 5 != 0))
            .collect(),
    );
    let wrapped = futures::stream::iter(0..2000u64);
    let wrapped = instrument_with(wrapped, A, &m);
    let wrapped = instrument_in_with(wrapped, B, &m)
        .map(|v| async move { v * 3 })
        .buffered(4)
        .filter(|v| futures::future::ready(v % 5 != 0));
    let wrapped: Vec<u64> = block_on(instrument_with(wrapped, B, &m).collect());
    assert_eq!(plain, wrapped);
}

#[test]
fn size_hint_and_unpin_are_preserved() {
    let h = Harness::new();
    let m = h.meter();
    let s = instrument_with(futures::stream::iter(0..7u8), A, &m);
    assert_eq!(s.size_hint(), (7, Some(7)));
    fn assert_unpin<T: Unpin>(_: &T) {}
    assert_unpin(&s);
    fn assert_send<T: Send>(_: &T) {}
    assert_send(&s);
}

#[test]
fn global_noop_provider_is_inert() {
    let out: Vec<u8> = block_on(
        marigold_impl::telemetry::instrument(
            marigold_impl::telemetry::instrument_in(futures::stream::iter(0..50u8), B),
            A,
        )
        .collect(),
    );
    assert_eq!(out, (0..50u8).collect::<Vec<_>>());
}
