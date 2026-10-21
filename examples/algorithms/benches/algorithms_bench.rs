use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_tsp_exact(c: &mut Criterion) {
    let mut group = c.benchmark_group("tsp_exact");
    group.sample_size(10);
    group.bench_function("tsp_brute_force", |b| {
        b.iter(|| block_on(algorithms::tsp_brute_force::run()))
    });
    group.bench_function("tsp_held_karp", |b| {
        b.iter(|| block_on(algorithms::tsp_held_karp::run()))
    });
    group.finish();
}

criterion_group!(benches, bench_gcd, bench_tsp_exact);
criterion_main!(benches);
