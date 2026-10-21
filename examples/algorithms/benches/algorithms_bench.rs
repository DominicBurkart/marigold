use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("gcd", |b| {
        b.to_async(&rt).iter(algorithms::gcd::run);
    });
}

fn bench_dijkstra(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("dijkstra", |b| {
        b.to_async(&rt).iter(algorithms::dijkstra::run);
    });
}

fn bench_kruskal(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("kruskal", |b| {
        b.to_async(&rt).iter(algorithms::kruskal::run);
    });
}

criterion_group!(benches, bench_gcd, bench_dijkstra, bench_kruskal);
criterion_main!(benches);
