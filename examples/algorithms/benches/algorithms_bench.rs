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

fn bench_tsp_exact(c: &mut Criterion) {
    let rt = runtime();
    let mut group = c.benchmark_group("tsp_exact");
    group.sample_size(10);
    group.bench_function("tsp_brute_force", |b| {
        b.to_async(&rt).iter(algorithms::tsp_brute_force::run);
    });
    group.bench_function("tsp_held_karp", |b| {
        b.to_async(&rt).iter(algorithms::tsp_held_karp::run);
    });
    group.finish();
}

fn bench_tsp_constructive(c: &mut Criterion) {
    let rt = runtime();
    let mut group = c.benchmark_group("tsp_constructive");
    group.sample_size(10);
    group.bench_function("tsp_nearest_neighbour", |b| {
        b.to_async(&rt).iter(algorithms::tsp_nearest_neighbour::run);
    });
    group.bench_function("tsp_christofides", |b| {
        b.to_async(&rt).iter(algorithms::tsp_christofides::run);
    });
    group.bench_function("tsp_lkh", |b| {
        b.to_async(&rt).iter(algorithms::tsp_lkh::run);
    });
    group.finish();
}

criterion_group!(benches, bench_gcd, bench_tsp_exact, bench_tsp_constructive);
criterion_main!(benches);
