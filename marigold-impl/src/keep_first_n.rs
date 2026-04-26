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
        // A min-heap (reverse of sorted_by) keeps the smallest kept item at the top,
        // so eviction is O(log n) instead of scanning the whole set.
        let first_n = BinaryHeap::with_capacity_by(n, move |a, b| sorted_by(a, b).reverse());
        impl_keep_first_n(self, first_n, n, sorted_by).await
    }
}

/// Internal logic for keep_first_n. Separated into its own function so generics can fully
/// express the BinaryHeap type (including the reversed-comparator lambda), which is required
/// for safe pointer operations in the parallel section.
///
/// Items are wrapped with their stream index for deterministic tie-breaking: when the
/// user comparator returns Equal, the earlier item (lower index) wins, ensuring consistent
/// results across parallel executions.
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
    let mut indexed_stream = sinput.enumerate();

    // Store (index, item) so tie-breaking is deterministic even with parallel processing.
    let indexed_comparator = move |a: &(usize, T), b: &(usize, T)| {
        match sorted_by(&a.1, &b.1) {
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
            // Earlier stream position wins on a tie.
            Ordering::Equal => a.0.cmp(&b.0),
        }
    };
    let mut first_n =
        BinaryHeap::with_capacity_by(n, move |a, b| indexed_comparator(a, b).reverse());

    // Fill the heap sequentially until we have n items or exhaust the stream.
    while first_n.len() < n {
        if let Some(indexed_item) = indexed_stream.next().await {
            first_n.push(indexed_item);
        } else {
            break;
        }
    }

    if first_n.len() < n {
        return futures::stream::iter(
            first_n
                .into_sorted_vec()
                .into_iter()
                .map(|(_idx, item)| item)
                .collect::<Vec<_>>()
                .into_iter(),
        );
    }

    // For the remainder of the stream, spawn parallel tasks to check each item against
    // the current smallest kept value.
    //
    // A double-check pattern guards the heap: the RwLock gives a fast-path read
    // (most items are rejected without touching the mutex), and after acquiring the
    // mutex the condition is re-checked to eliminate the TOCTOU race where two tasks
    // both pass the fast path but only one should actually replace the smallest value.
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
            .map(|(_idx, item)| item)
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
        // A min-heap (reverse of sorted_by) keeps the smallest kept item at the top,
        // so eviction is O(log n) instead of scanning the whole set.
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

        if first_n.len() < n {
            return futures::stream::iter(first_n.into_sorted_vec().into_iter());
        }

        // Without a spawning runtime, drive concurrency via for_each_concurrent.
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
    async fn keep_first_n() {
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
}
