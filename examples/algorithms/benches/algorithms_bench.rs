use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    let rt = algorithms::runtime();
    c.bench_function("gcd", |b| {
        b.to_async(&rt).iter(algorithms::gcd::run);
    });
}

fn bench_tsp_exact(c: &mut Criterion) {
    let rt = runtime();
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

fn bench_tsp_metaheuristics(c: &mut Criterion) {
    let rt = runtime();
    let mut group = c.benchmark_group("tsp_metaheuristics");
    group.sample_size(10);
    group.bench_function("tsp_simulated_annealing", |b| {
        b.to_async(&rt)
            .iter(algorithms::tsp_simulated_annealing::run);
    });
    group.bench_function("tsp_genetic", |b| {
        b.to_async(&rt).iter(algorithms::tsp_genetic::run);
    });
    group.bench_function("tsp_ant_colony", |b| {
        b.to_async(&rt).iter(algorithms::tsp_ant_colony::run);
    });
    group.finish();
}

criterion_group!(
    benches,
    bench_gcd,
    bench_tsp_exact,
    bench_tsp_metaheuristics
);
criterion_main!(benches);
