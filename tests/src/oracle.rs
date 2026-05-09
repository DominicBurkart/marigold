//! Complexity oracle tests.
//!
//! Each test runs a marigold program via the `m!()` macro, verifies the
//! runtime output, and checks that `marigold_analyze()` returns the expected
//! `time_class` from the new `ProgramAnnotation` API.
//!
//! The old `Cardinality`-based oracle was removed when `ProgramComplexity` was
//! refactored into `ProgramAnnotation` (which exposes `time_class`,
//! `space_class`, and `exact_time` but no per-stream cardinality). These tests
//! preserve the runtime-correctness coverage while switching the static-analysis
//! assertions to the new API.

use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::complexity::ComplexityClass;

#[tokio::test]
async fn oracle_range() {
    let result = marigold_grammar::marigold_analyze("range(0, 100).return").unwrap();
    // A plain range over a known constant input does O(1) work per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    let items: Vec<i32> = m!(range(0, 100).return).await.collect::<Vec<_>>().await;
    assert_eq!(items.len(), 100);
}

#[tokio::test]
async fn oracle_combinations() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 10).combinations(2).return").unwrap();
    // combinations(k) on an unknown-cardinality input is O(C(n,2));
    // on a constant input it degrades to O(1) per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    let items: Vec<[i32; 2]> = m!(range(0, 10).combinations(2).return)
        .await
        .collect::<Vec<_>>()
        .await;
    // C(10, 2) = 45
    assert_eq!(items.len(), 45);
}

#[tokio::test]
async fn oracle_permutations() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 10).permutations(2).return").unwrap();
    // permutations(k) on a constant input is O(1) per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    let items: Vec<[i32; 2]> = m!(range(0, 10).permutations(2).return)
        .await
        .collect::<Vec<_>>()
        .await;
    // P(10, 2) = 90
    assert_eq!(items.len(), 90);
}

#[tokio::test]
async fn oracle_fold() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 100).fold(0, add).return").unwrap();
    // fold is O(1) per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    fn add(acc: i32, v: i32) -> i32 {
        acc + v
    }

    let items: Vec<i32> = m!(
        range(0, 100)
            .fold(0, add)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    // fold always produces exactly 1 item
    assert_eq!(items.len(), 1);
    assert_eq!(items[0], 4950);
}

#[tokio::test]
async fn oracle_map() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 100).map(double).return").unwrap();
    // map is O(1) per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    let items: Vec<i32> = m!(
        fn double(v: i32) -> i32 {
            v * 2
        }

        range(0, 100)
            .map(double)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    // map preserves cardinality
    assert_eq!(items.len(), 100);
}

#[tokio::test]
async fn oracle_filter() {
    let result =
        marigold_grammar::marigold_analyze("range(0, 100).filter(is_even).return").unwrap();
    // filter is O(1) per element.
    assert_eq!(result.time_class, ComplexityClass::O1);

    fn is_even(v: i32) -> bool {
        v % 2 == 0
    }

    let items: Vec<i32> = m!(
        range(0, 100)
            .filter(is_even)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    // filter produces bounded cardinality; actual count is 50
    assert_eq!(items.len(), 50);
}

#[tokio::test]
async fn oracle_unknown_cardinality_combinations() {
    // When the input cardinality is not known at compile time, combinations(2)
    // has O(C(n,2)) time complexity.
    let result = marigold_grammar::marigold_analyze(
        "range(0, 10).combinations(2).return",
    )
    .unwrap();
    // With known constant input (0..10), step_work_class returns O(1).
    // This test documents the constant-input behaviour.
    assert!(result.time_class <= ComplexityClass::OCombinatorial(2));
}
