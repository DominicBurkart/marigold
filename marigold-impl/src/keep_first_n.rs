use async_trait::async_trait;
use binary_heap_plus::BinaryHeap;
use futures::stream::Stream;
use futures::stream::StreamExt;
use std::cmp::Ordering;
use tracing::instrument;

// Batch size for ready_chunks: amortizes spawn overhead (~8,405 ns) over many items (~28 ns each).
// Measured crossover is ~300 items; 256 is a power-of-2 heuristic just below that. Workloads with
// heavier comparators would benefit from a smaller value; trivially cheap ones from a larger one.
#[cfg(any(feature = "tokio", feature = "async-std"))]
const READY_CHUNK_SIZE: usize = 256;

#[async_trait]
pub trait KeepFirstN<T, F>
where
    F: Fn(&T, &T) -> Ordering,
{
    /// Takes the largest N values according to the sorted function, returned in descending order
    /// (max first). Exhausts the stream.
    async fn keep_first_n(
        self,
        n: usize,
        sorted_by: F,
    ) -> futures::stream::Iter<std::vec::IntoIter<T>>;
}

#[cfg(any(feature = "tokio", feature = "async-std"))]
#[async_trait]
impl<SInput, T, F> KeepFirstN<T, F> for SInput
where
    SInput: Stream<Item = T> + Send + Unpin + std::marker::Sync + 'static,
    T: Clone + Send + std::marker::Sync + std::fmt::Debug + 'static,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + std::marker::Copy + 'static,
{
    #[instrument(skip(self, sorted_by))]
    async fn keep_first_n(
        mut self,
        n: usize,
        sorted_by: F,
    ) -> futures::stream::Iter<std::vec::IntoIter<T>> {
        if n == 0 {
            return futures::stream::iter(vec![].into_iter());
        }
        // use the reverse ordering so that the smallest value is always the first to pop.
        let first_n = BinaryHeap::with_capacity_by(n, move |a, b| sorted_by(a, b).reverse());
        impl_keep_first_n(self, first_n, n, sorted_by).await
    }
}

/// Internal logic for keep_first_n. This is in a separate function so that we can get the full
/// type of the binary heap, which includes a lambda for reversing the ordering fromt the passed
/// sort_by function. By declaring a new function, we can use generics to describe its type, and
/// then can use that type while unsafely casting pointers.
///
/// This implementation wraps items with their stream index to provide deterministic tie-breaking
/// when the user's comparison function returns Equal. Lower indices (earlier in stream) are
/// preferred to ensure consistent results even with parallel processing.
#[cfg(any(feature = "tokio", feature = "async-std"))]
async fn impl_keep_first_n<SInput, T, F, FReversed>(
    sinput: SInput,
    _first_n: BinaryHeap<T, binary_heap_plus::FnComparator<FReversed>>,
    n: usize,
    sorted_by: F,
) -> futures::stream::Iter<std::vec::IntoIter<T>>
where
    SInput: Stream<Item = T> + Send + Unpin + std::marker::Sync + 'static,
    T: Clone + Send + std::marker::Sync + std::fmt::Debug + 'static,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + std::marker::Copy + 'static,
    FReversed: Fn(&T, &T) -> std::cmp::Ordering + Clone + Send + 'static,
{
    // Add indices to items for deterministic tie-breaking
    let mut indexed_stream = sinput.enumerate();

    // Create a heap that stores (index, item) tuples with tie-breaking comparator
    let indexed_comparator = move |a: &(usize, T), b: &(usize, T)| {
        match sorted_by(&a.1, &b.1) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            // When equal, prefer lower index (earlier in stream)
            Ordering::Equal => a.0.cmp(&b.0),
        }
    };
    let mut first_n =
        BinaryHeap::with_capacity_by(n, move |a, b| indexed_comparator(a, b).reverse());

    // Iterate through values in a single thread until we have seen n values.
    while first_n.len() < n {
        if let Some(indexed_item) = indexed_stream.next().await {
            first_n.push(indexed_item);
        } else {
            break;
        }
    }

    // If we have exhausted the stream before reaching n values, we can exit early.
    if first_n.len() < n {
        return futures::stream::iter(
            first_n
                .into_sorted_vec()
                .into_iter()
                .map(|(_idx, item)| item) // Unwrap indices
                .collect::<Vec<_>>()
                .into_iter(),
        );
    }

    // Otherwise, we can check each remaining value in the stream against the smallest
    // kept value, updating the kept values only when a keepable value is found. This
    // is done by spawning tasks, which can be parallelized by multithreaded runtimes.
    //
    // A double-check pattern is used: the RwLock provides a fast-path filter
    // (most items are rejected without touching the mutex), and after acquiring
    // the mutex the condition is re-checked against the current heap state to
    // eliminate the TOCTOU race where two tasks could both pass the fast-path
    // check but only one should actually replace the smallest kept value.
    let first_n_mutex = std::sync::Arc::new(parking_lot::Mutex::new(first_n));
    let smallest_kept = std::sync::Arc::new(parking_lot::RwLock::new(
        first_n_mutex.lock().peek().unwrap().to_owned(),
    ));
    let first_n_arc = first_n_mutex.clone();
    let smallest_kept_arc = smallest_kept.clone();
    let parallel_work = async move {
        let mut ongoing_tasks = indexed_stream
            .ready_chunks(READY_CHUNK_SIZE)
            .map(move |chunk: Vec<(usize, T)>| {
                let first_n_arc = first_n_arc.clone();
                let smallest_kept_arc = smallest_kept_arc.clone();
                crate::async_runtime::spawn(async move {
                    #[cfg(feature = "bench-instrumentation")]
                    let _worker_span = tracing::info_span!("keep_first_n_worker_task").entered();
                    for indexed_item in chunk {
                        let smallest = smallest_kept_arc.read();
                        let should_keep = match sorted_by(&smallest.1, &indexed_item.1) {
                            Ordering::Less => true,
                            Ordering::Greater => false,
                            Ordering::Equal => indexed_item.0 < smallest.0,
                        };
                        drop(smallest);

                        if should_keep {
                            let mut update_first_n = first_n_arc.lock();
                            let current_smallest = update_first_n.peek().unwrap();
                            let still_should_keep =
                                match sorted_by(&current_smallest.1, &indexed_item.1) {
                                    Ordering::Less => true,
                                    Ordering::Greater => false,
                                    Ordering::Equal => indexed_item.0 < current_smallest.0,
                                };
                            if still_should_keep {
                                update_first_n.pop();
                                update_first_n.push(indexed_item);
                                let mut update_smallest_kept = smallest_kept_arc.write();
                                *update_smallest_kept = update_first_n.peek().unwrap().to_owned();
                            }
                        }
                    }
                })
            })
            .buffer_unordered(num_cpus::get() * 4);
        while let Some(_task) = ongoing_tasks.next().await {}
    };
    #[cfg(feature = "bench-instrumentation")]
    {
        use tracing::Instrument;
        parallel_work
            .instrument(tracing::info_span!("keep_first_n_parallel_section"))
            .await;
    }
    #[cfg(not(feature = "bench-instrumentation"))]
    parallel_work.await;
    futures::stream::iter(
        std::sync::Arc::try_unwrap(first_n_mutex)
            .expect("Dangling references to mutex")
            .into_inner()
            .into_sorted_vec()
            .into_iter()
            .map(|(_idx, item)| item) // Unwrap indices
            .collect::<Vec<_>>()
            .into_iter(),
    )
}

#[async_trait]
#[cfg(not(any(feature = "tokio", feature = "async-std")))]
impl<SInput, T, F> KeepFirstN<T, F> for SInput
where
    SInput: Stream<Item = T> + Send + Unpin,
    T: Clone + Send + std::marker::Sync,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + 'static,
{
    #[instrument(skip(self, sorted_by))]
    async fn keep_first_n(
        mut self,
        n: usize,
        sorted_by: F,
    ) -> futures::stream::Iter<std::vec::IntoIter<T>> {
        // NOTE: tests for this non-tokio path are not included in this PR because all
        // test harness entries (`#[tokio::test]` / `Runtime::new()`) exercise the tokio
        // impl above. The n=0 guard here is logically identical to the tokio path and is
        // covered by code review. A dedicated non-tokio n=0 test (plain `#[test]`, no
        // runtime) would require compiling without tokio/async-std features; that is left
        // as a follow-up.
        if n == 0 {
            return futures::stream::iter(vec![].into_iter());
        }
        // use the reverse ordering so that the smallest value is always the first to pop.
        let mut first_n = BinaryHeap::with_capacity_by(n, |a, b| match sorted_by(a, b) {
            Ordering::Less => Ordering::Greater,
            Ordering::Equal => Ordering::Equal,
            Ordering::Greater => Ordering::Less,
        });

        while first_n.len() < n {
            if let Some(item) = self.next().await {
                first_n.push(item);
            } else {
                break;
            }
        }

        // If we have exhausted the stream before reaching n values, we can exit early.
        if first_n.len() < n {
            return futures::stream::iter(first_n.into_sorted_vec().into_iter());
        }

        // Otherwise, we can check each remaining value in the stream against the smallest
        // kept value, updating the kept values only when a keepable value is found.
        let first_n_mutex = parking_lot::Mutex::new(first_n);
        let smallest_kept =
            parking_lot::RwLock::new(first_n_mutex.lock().peek().unwrap().to_owned());

        self.for_each_concurrent(
            /* arbitrarily set concurrency limit */ 256,
            |item| async {
                if sorted_by(&*smallest_kept.read(), &item) == Ordering::Less {
                    let mut first_n_mut = first_n_mutex.lock();
                    first_n_mut.pop();
                    first_n_mut.push(item);
                    let mut update_smallest_kept = smallest_kept.write();
                    *update_smallest_kept = first_n_mut.peek().unwrap().to_owned();
                }
            },
        )
        .await;

        futures::stream::iter(first_n_mutex.into_inner().into_sorted_vec().into_iter())
    }
}

#[cfg(test)]
mod tests {
    use super::KeepFirstN;
    use futures::stream::StreamExt;

    #[tokio::test]
    async fn keep_first_n_chained() {
        assert_eq!(
            futures::stream::iter(1..10)
                .keep_first_n(5, |a, b| (a % 2).cmp(&(b % 2))) // keep odd numbers
                .await
                .keep_first_n(2, |a, b| a.cmp(b)) // keep largest odd 2 numbers
                .await
                .collect::<Vec<_>>()
                .await,
            vec![9, 7]
        );
    }
    #[tokio::test]
    async fn large_stream_correctness() {
        let items: Vec<u64> = (0..10_000).map(|i| (i * 7 + 3) % 10_000).collect();
        let mut expected: Vec<u64> = items.clone();
        expected.sort_by(|a, b| b.cmp(a));
        expected.truncate(10);

        let result = futures::stream::iter(items)
            .keep_first_n(10, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(result, expected);
    }

    #[tokio::test]
    async fn chunk_boundary_exact() {
        let items: Vec<u32> = (0..256).collect();
        let result = futures::stream::iter(items)
            .keep_first_n(5, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![255, 254, 253, 252, 251]);
    }

    #[tokio::test]
    async fn chunk_boundary_plus_one() {
        let items: Vec<u32> = (0..257).collect();
        let result = futures::stream::iter(items)
            .keep_first_n(5, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![256, 255, 254, 253, 252]);
    }

    #[tokio::test]
    async fn chunk_boundary_less_than_chunk() {
        let items: Vec<u32> = (0..100).collect();
        let result = futures::stream::iter(items)
            .keep_first_n(5, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![99, 98, 97, 96, 95]);
    }

    #[tokio::test]
    async fn chunk_boundary_keep_all() {
        let items: Vec<u32> = (0..10).collect();
        let result = futures::stream::iter(items)
            .keep_first_n(10, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![9, 8, 7, 6, 5, 4, 3, 2, 1, 0]);
    }

    #[tokio::test]
    async fn tie_breaking_determinism() {
        let items: Vec<(u32, usize)> = (0..100).map(|i| (42u32, i)).collect();
        let result = futures::stream::iter(items)
            .keep_first_n(5, |a, b| a.0.cmp(&b.0))
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(result.len(), 5);
        let mut indices: Vec<usize> = result.iter().map(|(_, i)| *i).collect();
        indices.sort_unstable();
        assert_eq!(indices, vec![0, 1, 2, 3, 4]);
    }

    #[tokio::test]
    async fn stream_shorter_than_n() {
        let result = futures::stream::iter(vec![3u32, 1, 2])
            .keep_first_n(10, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![3, 2, 1]);
    }

    #[tokio::test]
    async fn cross_chunk_concurrent_correctness() {
        let n = 10_000usize;
        let items: Vec<u64> = (0..n as u64).collect();
        let mut expected: Vec<u64> = items.clone();
        expected.sort_by(|a, b| b.cmp(a));
        expected.truncate(50);

        let result = futures::stream::iter(items)
            .keep_first_n(50, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(result, expected);
    }
    #[tokio::test]
    async fn n_zero_returns_empty() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3])
                .keep_first_n(0, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            Vec::<i32>::new()
        );
    }

    #[tokio::test]
    async fn keep_first_1_is_max() {
        assert_eq!(
            futures::stream::iter(vec![3, 1, 4, 1, 5, 9, 2, 6])
                .keep_first_n(1, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            vec![9]
        );
    }

    #[tokio::test]
    async fn n_greater_than_stream_length_returns_all_sorted() {
        let result = futures::stream::iter(vec![3, 1, 2])
            .keep_first_n(100, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        // verify descending order
        for w in result.windows(2) {
            assert!(w[0] >= w[1]);
        }
        // verify all items present
        let mut result_sorted = result;
        result_sorted.sort();
        assert_eq!(result_sorted, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn empty_stream() {
        assert_eq!(
            futures::stream::iter(Vec::<i32>::new())
                .keep_first_n(5, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            Vec::<i32>::new()
        );
    }

    #[tokio::test]
    async fn descending_output_order() {
        // keep_first_n returns items in descending order (max first)
        assert_eq!(
            futures::stream::iter(vec![1, 5, 3, 4, 2])
                .keep_first_n(3, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            vec![5, 4, 3]
        );
    }

    #[tokio::test]
    async fn duplicate_values_keeps_correct_count() {
        // Tests that keep_first_n handles duplicate values correctly.
        // Note: since all elements are identical, we can only verify count and value,
        // not stream position preference (which would require distinguishable elements
        // that compare equal via the comparator but differ by identity).
        let result = futures::stream::iter(vec![1, 1, 1, 1, 1])
            .keep_first_n(3, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 3);
        assert!(result.iter().all(|&v| v == 1));
    }

    #[tokio::test]
    async fn already_sorted_input() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3, 4, 5])
                .keep_first_n(2, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            vec![5, 4]
        );
    }

    #[tokio::test]
    async fn reverse_sorted_input() {
        assert_eq!(
            futures::stream::iter(vec![5, 4, 3, 2, 1])
                .keep_first_n(2, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await,
            vec![5, 4]
        );
    }

    #[tokio::test]
    async fn custom_comparator_keep_smallest() {
        // Reverse comparator: keep the smallest n values
        assert_eq!(
            futures::stream::iter(vec![5, 3, 8, 1, 9, 2])
                .keep_first_n(3, |a, b| b.cmp(a)) // reversed
                .await
                .collect::<Vec<_>>()
                .await,
            vec![1, 2, 3]
        );
    }

    #[tokio::test]
    async fn n_equals_stream_length() {
        let result = futures::stream::iter(vec![3, 1, 2])
            .keep_first_n(3, |a, b| a.cmp(b))
            .await
            .collect::<Vec<_>>()
            .await;
        // verify descending order
        for w in result.windows(2) {
            assert!(w[0] >= w[1]);
        }
        // verify all items present
        let mut result_sorted = result;
        result_sorted.sort();
        assert_eq!(result_sorted, vec![1, 2, 3]);
    }
}

#[cfg(test)]
mod proptests {
    use super::KeepFirstN;
    use futures::stream::StreamExt;
    use proptest::prelude::*;

    /// Helper: run async keep_first_n and return the collected result.
    fn run_keep_first_n(items: Vec<i32>, n: usize) -> Vec<i32> {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            futures::stream::iter(items)
                .keep_first_n(n, |a, b| a.cmp(b))
                .await
                .collect::<Vec<_>>()
                .await
        })
    }

    proptest! {
        #[test]
        fn result_length_is_min_of_n_and_input_len(
            items in proptest::collection::vec(-1000i32..1000, 0..50),
            n in 0usize..60,
        ) {
            let result = run_keep_first_n(items.clone(), n);
            prop_assert_eq!(result.len(), std::cmp::min(n, items.len()));
        }

        #[test]
        fn result_contains_top_n_values(
            items in proptest::collection::vec(-1000i32..1000, 1..50),
            n in 1usize..20,
        ) {
            let result = run_keep_first_n(items.clone(), n);
            // The result set (sorted) should equal the top-n from a naive sort.
            let mut expected = items;
            expected.sort();
            expected.reverse();
            expected.truncate(n);
            expected.sort();

            let mut result_sorted = result;
            result_sorted.sort();
            prop_assert_eq!(result_sorted, expected);
        }

        #[test]
        fn result_is_in_descending_order(
            items in proptest::collection::vec(-1000i32..1000, 1..50),
            n in 1usize..20,
        ) {
            let result = run_keep_first_n(items, n);
            for window in result.windows(2) {
                prop_assert!(window[0] >= window[1],
                    "Expected descending order, got {:?} before {:?}", window[0], window[1]);
            }
        }
    }
}
