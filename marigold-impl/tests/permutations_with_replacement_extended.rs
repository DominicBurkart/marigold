//! Extended edge-case tests for `permutations_with_replacement`.
//!
//! The existing `permutations_edge_cases.rs` covers `with_replacement_k_zero`
//! only for a non-empty stream. This file adds:
//! - Empty stream with k > 0 (Cartesian product of k empty iterators is empty)
//! - k = 1 yields n singleton tuples
//! - k > n with replacement yields n^k tuples (unlike plain permutations)
//! - Repeated elements are present in output (distinguishing it from combinations)

use futures::stream::StreamExt;
use marigold_impl::Permutable;

// --- Empty stream with k > 0 -------------------------------------------------

#[tokio::test]
async fn permutations_with_replacement_empty_stream_k3_yields_nothing() {
    let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
        .permutations_with_replacement(3)
        .await
        .collect()
        .await;
    assert!(
        result.is_empty(),
        "empty stream with k=3 should yield no tuples, got: {:?}",
        result
    );
}

#[tokio::test]
async fn permutations_with_replacement_empty_stream_k1_yields_nothing() {
    let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
        .permutations_with_replacement(1)
        .await
        .collect()
        .await;
    assert!(
        result.is_empty(),
        "empty stream with k=1 should yield no tuples"
    );
}

// --- k = 1: each element becomes a singleton tuple ---------------------------

#[tokio::test]
async fn permutations_with_replacement_k1_yields_n_singletons() {
    let result: Vec<Vec<i32>> = futures::stream::iter(vec![10, 20, 30])
        .permutations_with_replacement(1)
        .await
        .collect()
        .await;
    // n=3, k=1: 3^1 = 3 tuples, each of length 1
    assert_eq!(result.len(), 3, "k=1 should yield 3 singleton tuples");
    for tuple in &result {
        assert_eq!(tuple.len(), 1, "each tuple should have length 1");
    }
}

// --- Cartesian product semantics (n^k) ---------------------------------------

#[tokio::test]
async fn permutations_with_replacement_k2_n2_yields_4_tuples() {
    let result: Vec<Vec<i32>> = futures::stream::iter(vec![0, 1])
        .permutations_with_replacement(2)
        .await
        .collect()
        .await;
    // 2^2 = 4
    assert_eq!(result.len(), 4, "2^2 = 4 tuples expected, got {:?}", result);
    assert!(result.contains(&vec![0, 0]), "should contain (0,0)");
    assert!(result.contains(&vec![0, 1]), "should contain (0,1)");
    assert!(result.contains(&vec![1, 0]), "should contain (1,0)");
    assert!(result.contains(&vec![1, 1]), "should contain (1,1)");
}

#[tokio::test]
async fn permutations_with_replacement_k3_n2_yields_8_tuples() {
    let result: Vec<Vec<i32>> = futures::stream::iter(vec![0, 1])
        .permutations_with_replacement(3)
        .await
        .collect()
        .await;
    assert_eq!(result.len(), 8, "2^3 = 8 tuples expected");
}

// --- Repetition: same element appears in same tuple --------------------------

#[tokio::test]
async fn permutations_with_replacement_allows_repeated_elements_in_tuple() {
    let result: Vec<Vec<i32>> = futures::stream::iter(vec![5, 6])
        .permutations_with_replacement(2)
        .await
        .collect()
        .await;
    assert!(
        result.contains(&vec![5, 5]),
        "repeated element (5,5) should be present; got {:?}",
        result
    );
    assert!(
        result.contains(&vec![6, 6]),
        "repeated element (6,6) should be present; got {:?}",
        result
    );
}
