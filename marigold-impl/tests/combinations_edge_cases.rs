use futures::stream::StreamExt;
use marigold_impl::Combinable;

#[tokio::test]
async fn combinations_k_zero() {
    // C(3,0) = 1 empty combination
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
