use algorithms::block_on;
use criterion::{criterion_group, criterion_main, Criterion};

fn bench_gcd(c: &mut Criterion) {
    c.bench_function("gcd", |b| b.iter(|| block_on(algorithms::gcd::run())));
}

fn bench_tsp_metaheuristics(c: &mut Criterion) {
    let mut group = c.benchmark_group("tsp_metaheuristics");
    group.sample_size(10);
    group.bench_function("tsp_simulated_annealing", |b| {
        b.iter(|| block_on(algorithms::tsp_simulated_annealing::run()))
    });
    group.bench_function("tsp_genetic", |b| {
        b.iter(|| block_on(algorithms::tsp_genetic::run()))
    });
    group.bench_function("tsp_ant_colony", |b| {
        b.iter(|| block_on(algorithms::tsp_ant_colony::run()))
    });
    group.finish();
}

criterion_group!(benches, bench_gcd, bench_tsp_metaheuristics);
criterion_main!(benches);
