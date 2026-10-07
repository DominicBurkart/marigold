use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use futures::StreamExt;
use marigold_impl::telemetry::{instrument, instrument_in, instrument_with, NodeMeta};
use opentelemetry::metrics::MeterProvider;
use opentelemetry_sdk::metrics::{InMemoryMetricExporter, PeriodicReader, SdkMeterProvider};

const N: u64 = 100_000;

const INPUT: NodeMeta = NodeMeta::new("mg1-0000000000000001", "bench", "bench.marigold", 0, 12);
const MAP: NodeMeta = NodeMeta::new("mg1-0000000000000002", "bench", "bench.marigold", 13, 24);
const FILTER: NodeMeta = NodeMeta::new("mg1-0000000000000003", "bench", "bench.marigold", 25, 40);
const OUTPUT: NodeMeta = NodeMeta::new("mg1-0000000000000004", "bench", "bench.marigold", 41, 48);

async fn plain() -> u64 {
    futures::stream::iter(0..N)
        .map(|v| v.wrapping_mul(3))
        .filter(|v| futures::future::ready(v % 2 == 0))
        .fold(0u64, |acc, v| futures::future::ready(acc.wrapping_add(v)))
        .await
}

async fn instrumented_global() -> u64 {
    let s = instrument(futures::stream::iter(0..N), INPUT);
    let s = instrument(instrument_in(s, MAP).map(|v| v.wrapping_mul(3)), MAP);
    let s = instrument(
        instrument_in(s, FILTER).filter(|v| futures::future::ready(v % 2 == 0)),
        FILTER,
    );
    instrument(instrument_in(s, OUTPUT), OUTPUT)
        .fold(0u64, |acc, v| futures::future::ready(acc.wrapping_add(v)))
        .await
}

async fn instrumented_sdk(meter: &opentelemetry::metrics::Meter) -> u64 {
    let s = instrument_with(futures::stream::iter(0..N), INPUT, meter);
    let s = instrument_with(
        marigold_impl::telemetry::instrument_in_with(s, MAP, meter).map(|v| v.wrapping_mul(3)),
        MAP,
        meter,
    );
    let s = instrument_with(
        marigold_impl::telemetry::instrument_in_with(s, FILTER, meter)
            .filter(|v| futures::future::ready(v % 2 == 0)),
        FILTER,
        meter,
    );
    instrument_with(
        marigold_impl::telemetry::instrument_in_with(s, OUTPUT, meter),
        OUTPUT,
        meter,
    )
    .fold(0u64, |acc, v| futures::future::ready(acc.wrapping_add(v)))
    .await
}

fn bench_overhead(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let provider = SdkMeterProvider::builder()
        .with_reader(PeriodicReader::builder(InMemoryMetricExporter::default()).build())
        .build();
    let meter = provider.meter("bench");

    assert_eq!(rt.block_on(plain()), rt.block_on(instrumented_global()));
    assert_eq!(rt.block_on(plain()), rt.block_on(instrumented_sdk(&meter)));

    let mut group = c.benchmark_group("otel_overhead_map_filter_fold_100k");
    group.throughput(Throughput::Elements(N));
    group.bench_function("off", |b| b.iter(|| black_box(rt.block_on(plain()))));
    group.bench_function("on_noop_provider", |b| {
        b.iter(|| black_box(rt.block_on(instrumented_global())))
    });
    group.bench_function("on_sdk_provider", |b| {
        b.iter(|| black_box(rt.block_on(instrumented_sdk(&meter))))
    });
    group.finish();
}

criterion_group!(benches, bench_overhead);
criterion_main!(benches);
