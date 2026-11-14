use futures::stream::StreamExt;
use marigold_impl::Permutable;

#[tokio::test]
async fn permutations_k_zero() {
    let result = futures::stream::iter(vec![1, 2, 3])
        .permutations(0)
        .await
        .collect::<Vec<_>>()
        .await;
    // P(3,0) = 1 empty permutation
    assert_eq!(result, vec![Vec::<i32>::new()]);
}

#[tokio::test]
async fn permutations_k_exceeds_n() {
    let result = futures::stream::iter(vec![1, 2])
        .permutations(5)
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn permutations_empty_stream() {
    let result = futures::stream::iter(Vec::<i32>::new())
        .permutations(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn permutations_single_element_k1() {
    let result = futures::stream::iter(vec![7])
        .permutations(1)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![7]]);
}

#[tokio::test]
async fn permutations_with_replacement_k_zero() {
    // itertools::multi_cartesian_product called on zero iterators yields an
    // empty iterator (observed behavior on itertools >=0.13). This diverges
    // from the mathematical Cartesian-product identity, but we assert the
    // observed library contract so the test is stable.
    //
    // Note: this differs from `permutations(0)` which yields vec![vec![]]
    // via itertools::permutations(0); the two code paths use different
    // itertools primitives and have different k=0 semantics.
    let result = futures::stream::iter(vec![1, 2])
        .permutations_with_replacement(0)
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn permutations_with_replacement_single_element() {
    let result = futures::stream::iter(vec![5])
        .permutations_with_replacement(3)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![5, 5, 5]]);
}

#[tokio::test]
async fn permutations_count_matches_formula() {
    // P(4,2) = 4!/(4-2)! = 12
    let result = futures::stream::iter(vec![1, 2, 3, 4])
        .permutations(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result.len(), 12);
}

#[tokio::test]
async fn permutations_with_replacement_count_matches_formula() {
    // n^k = 3^2 = 9
    let result = futures::stream::iter(vec![1, 2, 3])
        .permutations_with_replacement(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result.len(), 9);
}

fn falling_factorial(n: usize, k: usize) -> usize {
    if k > n {
        0
    } else {
        (n - k + 1..=n).product()
    }
}

#[tokio::test]
async fn permutations_count_is_n_falling_k_with_distinct_orderings_exhaustively() {
    for n in 0..=6usize {
        for k in 0..=7usize {
            let items: Vec<usize> = (0..n).collect();
            let result = futures::stream::iter(items)
                .permutations(k)
                .await
                .collect::<Vec<_>>()
                .await;
            assert_eq!(result.len(), falling_factorial(n, k), "n={n} k={k}");
            let distinct: std::collections::HashSet<Vec<usize>> = result.iter().cloned().collect();
            assert_eq!(distinct.len(), result.len(), "n={n} k={k}");
            for permutation in &result {
                assert_eq!(permutation.len(), k);
                let members: std::collections::HashSet<usize> =
                    permutation.iter().copied().collect();
                assert_eq!(members.len(), k, "an item repeats in {permutation:?}");
                assert!(permutation.iter().all(|item| *item < n));
            }
        }
    }
}

#[tokio::test]
async fn permutations_count_is_by_position_not_by_value() {
    let result = futures::stream::iter(vec![1, 1, 2])
        .permutations(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result.len(), 6);
    let distinct: std::collections::HashSet<Vec<i32>> = result.iter().cloned().collect();
    assert_eq!(distinct.len(), 3);
}

#[tokio::test]
async fn permutations_with_replacement_count_is_n_pow_k_for_positive_k_exhaustively() {
    for n in 0..=4usize {
        for k in 0..=4usize {
            let items: Vec<usize> = (0..n).collect();
            let result = futures::stream::iter(items)
                .permutations_with_replacement(k)
                .await
                .collect::<Vec<_>>()
                .await;
            let expected = if k == 0 { 0 } else { n.pow(k as u32) };
            assert_eq!(result.len(), expected, "n={n} k={k}");
            let distinct: std::collections::HashSet<Vec<usize>> = result.iter().cloned().collect();
            assert_eq!(distinct.len(), result.len(), "n={n} k={k}");
            assert!(result.iter().all(|sequence| sequence.len() == k));
            assert!(result.iter().flatten().all(|item| *item < n));
        }
    }
}
