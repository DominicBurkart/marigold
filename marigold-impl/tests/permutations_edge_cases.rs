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
    // Cartesian product of 0 iterators = 1 empty tuple
    let result = futures::stream::iter(vec![1, 2])
        .permutations_with_replacement(0)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![Vec::<i32>::new()]);
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
