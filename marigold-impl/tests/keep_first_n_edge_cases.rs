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
    // keep_first_n with the default (greatest-first) comparator sorts descending.
    // When n >= stream length all elements are returned, ordered greatest first.
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
    // keep_first_n with the default (greatest-first) comparator sorts descending.
    // When n == stream length all elements are returned, ordered greatest first.
    assert_eq!(result, vec![3, 2, 1]);
}

fn reference(items: &[(u8, usize)], n: usize) -> Vec<(u8, usize)> {
    let mut sorted = items.to_vec();
    sorted.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));
    sorted.truncate(n);
    sorted
}

fn sequences(alphabet: u8, max_len: usize) -> Vec<Vec<u8>> {
    let mut all = vec![vec![]];
    let mut frontier = vec![vec![]];
    for _ in 0..max_len {
        let mut next = Vec::new();
        for seq in &frontier {
            for symbol in 0..alphabet {
                let mut longer: Vec<u8> = seq.clone();
                longer.push(symbol);
                next.push(longer);
            }
        }
        all.extend(next.iter().cloned());
        frontier = next;
    }
    all
}

#[tokio::test]
async fn keep_first_n_is_bounded_descending_and_keeps_the_largest_keys_exhaustively() {
    for keys in sequences(3, 6) {
        let items: Vec<(u8, usize)> = keys.iter().copied().zip(0..).collect();
        for n in 0..=7 {
            let result = futures::stream::iter(items.clone())
                .keep_first_n(n, |a: &(u8, usize), b: &(u8, usize)| a.0.cmp(&b.0))
                .await
                .collect::<Vec<_>>()
                .await;
            let expected = reference(&items, n);
            assert!(result.len() <= n, "{keys:?} n={n}");
            assert_eq!(result.len(), expected.len(), "{keys:?} n={n}");
            assert!(
                result.windows(2).all(|w| w[0].0 >= w[1].0),
                "{keys:?} n={n} {result:?}"
            );
            let result_keys: Vec<u8> = result.iter().map(|item| item.0).collect();
            let expected_keys: Vec<u8> = expected.iter().map(|item| item.0).collect();
            assert_eq!(result_keys, expected_keys, "{keys:?} n={n}");
            let mut positions: Vec<usize> = result.iter().map(|item| item.1).collect();
            positions.sort_unstable();
            positions.dedup();
            assert_eq!(positions.len(), result.len(), "{keys:?} n={n}");
            assert!(result.iter().all(|item| items.contains(item)));
        }
    }
}

#[tokio::test]
async fn keep_first_n_keeps_the_earliest_of_many_ties_across_chunks() {
    let items: Vec<(u8, usize)> = (0..2000).map(|i| ((i % 3) as u8, i)).collect();
    let result = futures::stream::iter(items.clone())
        .keep_first_n(5, |a: &(u8, usize), b: &(u8, usize)| a.0.cmp(&b.0))
        .await
        .collect::<Vec<_>>()
        .await;
    let mut kept = result.clone();
    kept.sort_by_key(|item| item.1);
    assert_eq!(kept, vec![(2, 2), (2, 5), (2, 8), (2, 11), (2, 14)]);
    assert!(result.iter().all(|item| item.0 == 2));
}

#[tokio::test]
async fn keep_first_n_cutoff_tie_keeps_the_earlier_item_and_drops_the_later() {
    let items = vec![(1u8, 0usize), (2, 1), (2, 2), (2, 3), (1, 4)];
    let result = futures::stream::iter(items)
        .keep_first_n(2, |a: &(u8, usize), b: &(u8, usize)| a.0.cmp(&b.0))
        .await
        .collect::<Vec<_>>()
        .await;
    let mut indices: Vec<usize> = result.iter().map(|item| item.1).collect();
    indices.sort_unstable();
    assert_eq!(indices, vec![1, 2]);
}
