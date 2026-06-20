use futures::StreamExt;
use iai_callgrind::{library_benchmark, library_benchmark_group, main};
use marigold_impl::combinations::Combinable;
use marigold_impl::keep_first_n::KeepFirstN;
use marigold_impl::permutations::Permutable;
use std::cmp::Ordering;
use std::hint::black_box;

#[allow(clippy::ptr_arg)]
fn compare_by_sum_nested(a: &Vec<Vec<u16>>, b: &Vec<Vec<u16>>) -> Ordering {
    let sa: u32 = a.iter().flatten().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().flatten().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

#[library_benchmark]
fn bench_pwr_small() -> Vec<Vec<u16>> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    black_box(rt.block_on(async {
        futures::stream::iter(0u16..4)
            .permutations_with_replacement(3)
            .await
            .collect::<Vec<_>>()
            .await
    }))
}

#[library_benchmark]
fn bench_full_pipeline_small() -> Vec<Vec<Vec<u16>>> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();
    black_box(rt.block_on(async {
        futures::stream::iter(0u16..4)
            .permutations_with_replacement(3)
            .await
            .combinations(2)
            .await
            .keep_first_n(5, compare_by_sum_nested)
            .await
            .collect::<Vec<_>>()
            .await
    }))
}

library_benchmark_group!(
    name = pipeline_group;
    benchmarks = bench_pwr_small, bench_full_pipeline_small
);

main!(library_benchmark_groups = pipeline_group);
