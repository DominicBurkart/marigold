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

fn bench_knapsack(c: &mut Criterion) {
    let rt = runtime();
    c.bench_function("knapsack", |b| {
        b.to_async(&rt).iter(algorithms::knapsack::run);
    });
}

fn bench_floyd_warshall(c: &mut Criterion) {
    let rt = runtime();
    c.bench_function("floyd_warshall", |b| {
        b.to_async(&rt).iter(algorithms::floyd_warshall::run);
    });
}

criterion_group!(benches, bench_gcd, bench_knapsack, bench_floyd_warshall);
criterion_main!(benches);
