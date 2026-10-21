use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_tsp_nearest_neighbour(c: &mut Criterion) {
    c.bench_function("tsp_nearest_neighbour", |b| {
        b.iter(|| block_on(algorithms::tsp_nearest_neighbour::run()))
    });
}

fn bench_tsp_christofides(c: &mut Criterion) {
    c.bench_function("tsp_christofides", |b| {
        b.iter(|| block_on(algorithms::tsp_christofides::run()))
    });
}

fn bench_tsp_lkh(c: &mut Criterion) {
    let mut group = c.benchmark_group("tsp_lkh");
    group.sample_size(10);
    group.bench_function("run", |b| b.iter(|| block_on(algorithms::tsp_lkh::run())));
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
