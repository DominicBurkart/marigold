use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use futures::StreamExt;
use marigold_impl::combinations::Combinable;
use marigold_impl::keep_first_n::KeepFirstN;
use marigold_impl::permutations::Permutable;
use std::cmp::Ordering;
use std::sync::Arc;
use std::time::Duration;

fn compare_by_sum_nested(a: &[Vec<u16>], b: &[Vec<u16>]) -> Ordering {
    let sa: u32 = a.iter().flatten().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().flatten().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

fn bench_pipeline_stages(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let pwr_items: Arc<Vec<Vec<u16>>> = Arc::new({
        let mut v = Vec::with_capacity(512);
        for i in 0u16..8 {
            for j in 0u16..8 {
                for k in 0u16..8 {
                    v.push(vec![i, j, k]);
                }
            }
        }
        v
    });

    let mut group = c.benchmark_group("pipeline_stages");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(30));

    group.bench_function("range_8_only", |b| {
        b.iter(|| rt.block_on(async { futures::stream::iter(0u16..8).collect::<Vec<_>>().await }))
    });

    group.bench_function("permutations_with_replacement_8_k3", |b| {
        b.iter(|| {
            rt.block_on(async {
                futures::stream::iter(0u16..8)
                    .permutations_with_replacement(3)
                    .await
                    .collect::<Vec<_>>()
                    .await
            })
        })
    });

    group.measurement_time(Duration::from_secs(60));

    group.bench_function("combinations_512_k3", |b| {
        b.iter_batched(
            || pwr_items.to_vec(),
            |v| {
                rt.block_on(async move {
                    futures::stream::iter(v)
                        .combinations(3)
                        .await
                        .collect::<Vec<_>>()
                        .await
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.measurement_time(Duration::from_secs(120));

    group.bench_function("full_pipeline_range8_pwr3_comb3_kfn20", |b| {
        b.iter(|| {
            rt.block_on(async {
                futures::stream::iter(0u16..8)
                    .permutations_with_replacement(3)
                    .await
                    .combinations(3)
                    .await
                    .keep_first_n(20, compare_by_sum_nested)
                    .await
                    .collect::<Vec<_>>()
                    .await
            })
        })
    });

    group.finish();
}

criterion_group!(benches, bench_pipeline_stages);
criterion_main!(benches);
