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
    // itertools::multi_cartesian_product of 0 iterators yields exactly one
    // result: the empty product [[]].  This is the standard mathematical
    // convention (there is exactly one way to choose 0 elements from any set)
    // and is the behaviour of itertools 0.10.x multi_cartesian_product.
    //
    // Note: this differs from permutations_with_replacement(k) on an *empty*
    // source (which yields nothing because there are no elements to choose from).
    let result = futures::stream::iter(vec![1, 2])
        .permutations_with_replacement(0)
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![vec![] as Vec<i32>]);
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
