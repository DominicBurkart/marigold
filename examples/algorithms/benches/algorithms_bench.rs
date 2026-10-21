use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_knapsack(c: &mut Criterion) {
    c.bench_function("knapsack", |b| {
        b.iter(|| block_on(algorithms::knapsack::run()))
    });
}

fn bench_floyd_warshall(c: &mut Criterion) {
    c.bench_function("floyd_warshall", |b| {
        b.iter(|| block_on(algorithms::floyd_warshall::run()))
    });
}

criterion_group!(benches, bench_gcd, bench_knapsack, bench_floyd_warshall);
criterion_main!(benches);
