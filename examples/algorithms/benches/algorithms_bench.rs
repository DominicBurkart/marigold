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

criterion_group!(benches, bench_gcd);
criterion_main!(benches);
