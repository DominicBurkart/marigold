use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use futures::StreamExt;
use marigold_impl::keep_first_n::KeepFirstN;
use std::cmp::Ordering;
use std::sync::Arc;
use std::time::Duration;

fn compare_by_sum(a: &[u16; 3], b: &[u16; 3]) -> Ordering {
    let sa: u32 = a.iter().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

fn bench_keep_first_n(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();

    let items: Arc<Vec<[u16; 3]>> = Arc::new({
        let mut v = Vec::with_capacity(22_238_720);
        for i in 0u16..512 {
            for j in (i + 1)..512 {
                for k in (j + 1)..512 {
                    v.push([i, j, k]);
                }
            }
        }
        v
    });

    let mut group = c.benchmark_group("keep_first_n");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(60));

    group.bench_function("c_512_3_stream_iter", |b| {
        b.iter_batched(
            || items.to_vec(),
            |v| {
                rt.block_on(async move {
                    futures::stream::iter(v)
                        .keep_first_n(20, compare_by_sum)
                        .await
                        .collect::<Vec<_>>()
                        .await
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.finish();
}

criterion_group!(benches, bench_keep_first_n);
criterion_main!(benches);
