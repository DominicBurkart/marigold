use color_palette_picker::compare_contrast;
use criterion::{criterion_group, criterion_main, Criterion};
use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::cmp::Ordering;

fn mod_fifty(i: u8) -> bool {
    i % 50 == 0
}

fn compare_by_sum(a: &[[u8; 3]; 3], b: &[[u8; 3]; 3]) -> Ordering {
    let sa: u32 = a.iter().flatten().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().flatten().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

pub fn bench_color_picker(c: &mut Criterion) {
    c.bench_function("bench_color_picker", |b| {
        b.to_async(
            tokio::runtime::Runtime::new().expect("could not setup up tokio runtime in bench"),
        )
        .iter(|| async {
            m!(
                range(0, 255)
                    .filter(mod_fifty)
                    .permutations_with_replacement(3)
                    .combinations(3)
                    .keep_first_n(2, compare_contrast)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
        })
    });
}

pub fn bench_range_8_pipeline(c: &mut Criterion) {
    let mut group = c.benchmark_group("range_8_pipeline");
    group.sample_size(10);
    group.measurement_time(std::time::Duration::from_secs(120));
    group.bench_function("pwr3_comb3_kfn20_compare_by_sum", |b| {
        b.to_async(
            tokio::runtime::Runtime::new().expect("could not setup up tokio runtime in bench"),
        )
        .iter(|| async {
            m!(
                range(0, 8)
                    .permutations_with_replacement(3)
                    .combinations(3)
                    .keep_first_n(20, compare_by_sum)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
        })
    });
    group.finish();
}

criterion_group!(benches, bench_color_picker, bench_range_8_pipeline);
criterion_main!(benches);
