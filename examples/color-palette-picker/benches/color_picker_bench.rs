use color_palette_picker::compare_contrast;
use criterion::{criterion_group, criterion_main, Criterion};
use marigold::m;

fn mod_fifty(i: &u8) -> bool {
    i % 50 == 0
}

pub fn bench_color_picker(c: &mut Criterion) {
    c.bench_function("bench_color_picker", |b| {
        b.to_async(
            tokio::runtime::Runtime::new().expect("could not setup up tokio runtime in bench"),
        )
        .iter(|| {
            m!(
                range(0, 255)
                .filter(mod_fifty)
                .permutations_with_replacement(3)
                .combinations(3)
                .keep_first_n(2, compare_contrast)
                .to_vec()
                .return
            )
        })
    });
}

criterion_group!(benches, bench_color_picker);
criterion_main!(benches);
