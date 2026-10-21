use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_dijkstra(c: &mut Criterion) {
    c.bench_function("dijkstra", |b| {
        b.iter(|| block_on(algorithms::dijkstra::run()))
    });
}

fn bench_kruskal(c: &mut Criterion) {
    c.bench_function("kruskal", |b| {
        b.iter(|| block_on(algorithms::kruskal::run()))
    });
}

criterion_group!(benches, bench_gcd, bench_dijkstra, bench_kruskal);
criterion_main!(benches);
