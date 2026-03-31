use futures::stream::StreamExt;
use marigold_impl::KeepFirstN;

#[tokio::test]
async fn keep_first_n_zero() {
    let result = futures::stream::iter(1..10)
        .keep_first_n(0, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn keep_first_n_exceeds_stream_length() {
    let result = futures::stream::iter(vec![3, 1, 2])
        .keep_first_n(100, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    // Should return all elements sorted in descending order: the comparator a.cmp(b) ranks
    // greater values first, so keep_first_n returns elements greatest-first.
    assert_eq!(result, vec![3, 2, 1]);
}

#[tokio::test]
async fn keep_first_n_single_element() {
    let result = futures::stream::iter(vec![42])
        .keep_first_n(1, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![42]);
}

#[tokio::test]
async fn keep_first_n_all_equal() {
    let result = futures::stream::iter(vec![5, 5, 5, 5, 5])
        .keep_first_n(3, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result.len(), 3);
    assert!(result.iter().all(|&x| x == 5));
}

#[tokio::test]
async fn keep_first_n_reversed_comparator_keeps_smallest() {
    // Keep 3 smallest by reversing the comparator
    let result = futures::stream::iter(vec![5, 3, 8, 1, 9, 2])
        .keep_first_n(3, |a: &i32, b: &i32| b.cmp(a)) // reversed: keep smallest
        .await
        .collect::<Vec<_>>()
        .await;
    assert_eq!(result, vec![1, 2, 3]);
}

#[tokio::test]
async fn keep_first_n_empty_stream() {
    let result = futures::stream::iter(Vec::<i32>::new())
        .keep_first_n(5, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    assert!(result.is_empty());
}

#[tokio::test]
async fn keep_first_n_exactly_n() {
    let result = futures::stream::iter(vec![3, 1, 2])
        .keep_first_n(3, |a: &i32, b: &i32| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;
    // Returns all elements sorted greatest-first (comparator a.cmp(b) ranks greater values first).
    assert_eq!(result, vec![3, 2, 1]);
}
