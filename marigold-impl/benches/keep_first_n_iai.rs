use futures::StreamExt;
use iai_callgrind::{library_benchmark, library_benchmark_group, main};
use marigold_impl::keep_first_n::KeepFirstN;
use std::cmp::Ordering;
use std::hint::black_box;

fn compare_by_sum(a: &[u16; 3], b: &[u16; 3]) -> Ordering {
    let sa: u32 = a.iter().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

fn setup_items() -> Vec<[u16; 3]> {
    let mut v = Vec::new();
    for i in 0u16..8 {
        for j in (i + 1)..8 {
            for k in (j + 1)..8 {
                v.push([i, j, k]);
            }
        }
    }
    v
}

#[library_benchmark]
#[bench::c_8_3_stream_iter(setup = setup_items)]
fn bench_keep_first_n(items: Vec<[u16; 3]>) -> Vec<[u16; 3]> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    black_box(rt.block_on(async move {
        futures::stream::iter(items)
            .keep_first_n(5, compare_by_sum)
            .await
            .collect::<Vec<_>>()
            .await
    }))
}

library_benchmark_group!(
    name = keep_first_n_group;
    benchmarks = bench_keep_first_n
);

main!(library_benchmark_groups = keep_first_n_group);
