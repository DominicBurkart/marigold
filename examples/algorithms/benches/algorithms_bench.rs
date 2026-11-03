use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("gcd", |b| {
        b.to_async(&rt).iter(algorithms::gcd::run);
    });
}

fn bench_knapsack(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("knapsack", |b| {
        b.to_async(&rt).iter(algorithms::knapsack::run);
    });
}

fn bench_floyd_warshall(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("floyd_warshall", |b| {
        b.to_async(&rt).iter(algorithms::floyd_warshall::run);
    });
}

fn bench_tsp_exact(c: &mut Criterion) {
    let rt = algorithms::runtime();
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

fn bench_binary_search(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("binary_search", |b| {
        b.to_async(&rt).iter(algorithms::binary_search::run);
    });
}

fn bench_quicksort(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("quicksort", |b| {
        b.to_async(&rt).iter(algorithms::quicksort::run);
    });
}

criterion_group!(
    benches,
    bench_gcd,
    bench_binary_search,
    bench_quicksort,
    bench_knapsack,
    bench_floyd_warshall,
    bench_tsp_exact
);
criterion_main!(benches);
