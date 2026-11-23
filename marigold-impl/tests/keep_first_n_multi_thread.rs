#![cfg(feature = "tokio")]

use futures::stream::StreamExt;
use marigold_impl::KeepFirstN;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn cross_chunk_correctness_on_a_multi_thread_runtime() {
    let items: Vec<u64> = (0..20_000).map(|i| (i * 7919 + 13) % 20_011).collect();
    let mut distinct = items.clone();
    distinct.sort_unstable();
    distinct.dedup();
    assert_eq!(distinct.len(), items.len());

    let mut expected = items.clone();
    expected.sort_by(|a, b| b.cmp(a));
    expected.truncate(64);

    let result = futures::stream::iter(items)
        .keep_first_n(64, |a, b| a.cmp(b))
        .await
        .collect::<Vec<_>>()
        .await;

    assert_eq!(result, expected);
}
