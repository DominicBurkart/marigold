//! Wall-clock scaling tests for issue #135.
//!
//! These tests run the same marigold program at three or more input sizes,
//! measure wall-clock elapsed time with `std::time::Instant`, and assert
//! that the observed growth ratio is consistent with the complexity class
//! predicted by the analyzer. Because wall-clock measurements are noisy
//! (GC pauses, CPU frequency scaling, shared CI runners), each assertion
//! uses a generous tolerance — the goal is to catch asymptotic regressions,
//! not micro-benchmarks.
//!
//! All tests in this module are marked `#[ignore]` so they do not run
//! during `cargo test` / `cargo nextest` on every CI push. Run them
//! explicitly:
//!
//! ```bash
//! cargo test --release -p tests -- --ignored scaling_tests
//! ```
//!
//! Always use `--release`; debug builds distort the growth ratios.

use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::time::{Duration, Instant};

/// Run `body` and return the wall-clock elapsed time.
async fn time_async<F, Fut, T>(body: F) -> (T, Duration)
where
    F: FnOnce() -> Fut,
    Fut: std::future::Future<Output = T>,
{
    let start = Instant::now();
    let out = body().await;
    (out, start.elapsed())
}

/// Assert that `observed_ratio` is within `tolerance` times `predicted_ratio`.
///
/// `tolerance >= 1.0`; a tolerance of `3.0` accepts an observed ratio in
/// `[predicted / 3, predicted * 3]`. Wall-clock tests are noisy so this is
/// intentionally loose.
fn assert_growth_ratio(observed_ratio: f64, predicted_ratio: f64, tolerance: f64, label: &str) {
    assert!(
        tolerance >= 1.0,
        "tolerance must be >= 1.0, got {tolerance}"
    );
    let lower = predicted_ratio / tolerance;
    let upper = predicted_ratio * tolerance;
    assert!(
        observed_ratio >= lower && observed_ratio <= upper,
        "{label}: observed growth ratio {observed_ratio:.3} outside predicted \
         [{lower:.3}, {upper:.3}] (predicted {predicted_ratio:.3}, tolerance {tolerance}x)"
    );
}

/// O(n) growth: `range(0, N).map(double).return` should roughly double in
/// time when N doubles. Uses three input sizes (1_000, 10_000, 100_000) to
/// fit the issue's "≥ 3 input sizes" criterion.
#[ignore = "wall-clock scaling test; run with --ignored"]
#[tokio::test(flavor = "multi_thread")]
async fn map_scales_linearly() {
    // Three input sizes spanning two orders of magnitude.
    let (_, t_small) = time_async(|| async {
        m!(
            fn double(v: i32) -> i32 { v * 2 }
            range(0, 1000).map(double).return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;

    let (_, t_medium) = time_async(|| async {
        m!(
            fn double(v: i32) -> i32 { v * 2 }
            range(0, 10000).map(double).return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;

    let (_, t_large) = time_async(|| async {
        m!(
            fn double(v: i32) -> i32 { v * 2 }
            range(0, 100000).map(double).return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;

    // Predicted ratios: 10x input → 10x time for O(n).
    let ratio_med_small = t_medium.as_secs_f64() / t_small.as_secs_f64().max(1e-9);
    let ratio_large_med = t_large.as_secs_f64() / t_medium.as_secs_f64().max(1e-9);
    eprintln!(
        "map_scales_linearly: t_small={t_small:?} t_medium={t_medium:?} t_large={t_large:?} \
         ratio(med/small)={ratio_med_small:.3} ratio(large/med)={ratio_large_med:.3}"
    );

    // 3x tolerance absorbs startup noise at the smallest size and CI jitter.
    assert_growth_ratio(ratio_med_small, 10.0, 3.0, "map 1k→10k");
    assert_growth_ratio(ratio_large_med, 10.0, 3.0, "map 10k→100k");
}

/// O(n²) growth: `permutations(2)` over three input sizes. Doubling N
/// should roughly quadruple time.
#[ignore = "wall-clock scaling test; run with --ignored"]
#[tokio::test(flavor = "multi_thread")]
async fn permutations2_scales_quadratically() {
    // Sizes chosen so the smallest is large enough to dominate startup
    // cost and the largest completes in a reasonable time on CI.
    let (_, t_small) = time_async(|| async {
        m!(range(0, 200).permutations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    let (_, t_medium) = time_async(|| async {
        m!(range(0, 400).permutations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    let (_, t_large) = time_async(|| async {
        m!(range(0, 800).permutations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    // Predicted: doubling input → 4x time for O(n²).
    let ratio_med_small = t_medium.as_secs_f64() / t_small.as_secs_f64().max(1e-9);
    let ratio_large_med = t_large.as_secs_f64() / t_medium.as_secs_f64().max(1e-9);
    eprintln!(
        "permutations2_scales_quadratically: t_small={t_small:?} t_medium={t_medium:?} \
         t_large={t_large:?} ratio(med/small)={ratio_med_small:.3} \
         ratio(large/med)={ratio_large_med:.3}"
    );

    // 3x tolerance around the predicted 4.0 ratio.
    assert_growth_ratio(ratio_med_small, 4.0, 3.0, "permutations(2) 200→400");
    assert_growth_ratio(ratio_large_med, 4.0, 3.0, "permutations(2) 400→800");
}

/// O(n²) growth: `combinations(2)` over three input sizes. Same predicted
/// ratio as `permutations(2)` (the constant factor is ~½ but the growth
/// rate is identical).
#[ignore = "wall-clock scaling test; run with --ignored"]
#[tokio::test(flavor = "multi_thread")]
async fn combinations2_scales_quadratically() {
    let (_, t_small) = time_async(|| async {
        m!(range(0, 200).combinations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    let (_, t_medium) = time_async(|| async {
        m!(range(0, 400).combinations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    let (_, t_large) = time_async(|| async {
        m!(range(0, 800).combinations(2).return)
            .await
            .collect::<Vec<_>>()
            .await
    })
    .await;

    let ratio_med_small = t_medium.as_secs_f64() / t_small.as_secs_f64().max(1e-9);
    let ratio_large_med = t_large.as_secs_f64() / t_medium.as_secs_f64().max(1e-9);
    eprintln!(
        "combinations2_scales_quadratically: t_small={t_small:?} t_medium={t_medium:?} \
         t_large={t_large:?} ratio(med/small)={ratio_med_small:.3} \
         ratio(large/med)={ratio_large_med:.3}"
    );

    assert_growth_ratio(ratio_med_small, 4.0, 3.0, "combinations(2) 200→400");
    assert_growth_ratio(ratio_large_med, 4.0, 3.0, "combinations(2) 400→800");
}
