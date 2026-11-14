use futures::stream::StreamExt;
use marigold_impl::Combinable;

#[tokio::test]
async fn combinations_k_zero() {
    // C(3,0) = 1 empty combination.
    // itertools::Itertools::combinations(0) is documented to yield exactly one empty
    // combination for any n >= 0 (the empty combination). This relies on itertools'
    // specified contract, not an implementation detail, so it is stable.
    let result = futures::stream::iter(vec![1, 2, 3])
        .combinations(0)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![Vec::<i32>::new()]);
}

#[tokio::test]
async fn combinations_k_equals_n() {
    // C(3,3) = 1
    let result = futures::stream::iter(vec![1, 2, 3])
        .combinations(3)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![1, 2, 3]]);
}

#[tokio::test]
async fn combinations_k_exceeds_n() {
    // C(2,5) = 0
    let result = futures::stream::iter(vec![1, 2])
        .combinations(5)
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn combinations_empty_stream() {
    let result = futures::stream::iter(Vec::<i32>::new())
        .combinations(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn combinations_single_element() {
    let result = futures::stream::iter(vec![42])
        .combinations(1)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![42]]);
}

fn binomial(n: usize, k: usize) -> usize {
    if k > n {
        return 0;
    }
    (0..k).fold(1, |acc, i| acc * (n - i) / (i + 1))
}

#[tokio::test]
async fn combinations_count_is_binomial_with_no_two_equal_as_sets_exhaustively() {
    for n in 0..=7usize {
        for k in 0..=8usize {
            let items: Vec<usize> = (0..n).collect();
            let result = futures::stream::iter(items)
                .combinations(k)
                .await
                .collect::<Vec<_>>()
                .await;
            assert_eq!(result.len(), binomial(n, k), "n={n} k={k}");
            let as_sets: std::collections::HashSet<std::collections::BTreeSet<usize>> = result
                .iter()
                .map(|combination| combination.iter().copied().collect())
                .collect();
            assert_eq!(as_sets.len(), result.len(), "n={n} k={k}");
            for combination in &result {
                assert_eq!(combination.len(), k);
                assert!(
                    combination.windows(2).all(|w| w[0] < w[1]),
                    "{combination:?}"
                );
            }
        }
    }
}

#[tokio::test]
async fn combinations_count_is_by_position_not_by_value() {
    let result = futures::stream::iter(vec![1, 1, 2])
        .combinations(2)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![1, 1], vec![1, 2], vec![1, 2]]);
}
