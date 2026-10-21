use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_binary_search(c: &mut Criterion) {
    c.bench_function("binary_search", |b| {
        b.iter(|| block_on(algorithms::binary_search::run()))
    });
}

fn bench_quicksort(c: &mut Criterion) {
    c.bench_function("quicksort", |b| {
        b.iter(|| block_on(algorithms::quicksort::run()))
    });
}

criterion_group!(benches, bench_gcd, bench_binary_search, bench_quicksort);
criterion_main!(benches);
