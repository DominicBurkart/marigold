use criterion::{criterion_group, criterion_main, Criterion};

fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Runtime::new().expect("could not build tokio runtime")
}

fn bench_gcd(c: &mut Criterion) {
    let rt = runtime();
    c.bench_function("gcd", |b| {
        b.to_async(&rt).iter(algorithms::gcd::run);
    });
}

fn bench_tsp_nearest_neighbour(c: &mut Criterion) {
    let rt = runtime();
    c.bench_function("tsp_nearest_neighbour", |b| {
        b.to_async(&rt).iter(algorithms::tsp_nearest_neighbour::run);
    });
}

fn bench_tsp_christofides(c: &mut Criterion) {
    let rt = runtime();
    c.bench_function("tsp_christofides", |b| {
        b.to_async(&rt).iter(algorithms::tsp_christofides::run);
    });
}

fn bench_tsp_lkh(c: &mut Criterion) {
    let rt = runtime();
    let mut group = c.benchmark_group("tsp_lkh");
    group.sample_size(10);
    group.bench_function("run", |b| {
        b.to_async(&rt).iter(algorithms::tsp_lkh::run);
    });
    group.finish();
}

criterion_group!(
    benches,
    bench_gcd,
    bench_tsp_nearest_neighbour,
    bench_tsp_christofides,
    bench_tsp_lkh
);
criterion_main!(benches);
