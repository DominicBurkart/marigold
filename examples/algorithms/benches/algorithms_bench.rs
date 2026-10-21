use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("gcd", |b| {
        b.to_async(&rt).iter(algorithms::gcd::run);
    });
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

criterion_group!(benches, bench_gcd, bench_binary_search, bench_quicksort);
criterion_main!(benches);
