use async_trait::async_trait;
use binary_heap_plus::BinaryHeap;
use futures::stream::Stream;
use futures::stream::StreamExt;
use parallel_stream::ParallelStream;
use std::cmp::Ordering;
use std::ops::Deref;
use std::sync::Arc;

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

#[cfg(feature = "parallel_stream")]
#[async_trait]
impl<SInput, T, F> KeepFirstN<T, F> for SInput
where
    SInput: Stream<Item = T> + Send + Unpin + std::marker::Sync + 'static,
    T: Clone + Send + std::marker::Sync + std::fmt::Debug + 'static,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + std::marker::Copy + 'static,
{
    async fn keep_first_n(
        mut self,
        n: usize,
        sorted_by: F,
    ) -> futures::stream::Iter<std::vec::IntoIter<T>> {
        // use the reverse ordering so that the smallest value is always the first to pop.
        let first_n = BinaryHeap::with_capacity_by(n, |a, b| sorted_by(a, b).reverse());
        impl_keep_first_n(self, first_n, n, sorted_by).await
    }
}

/// Internal logic for keep_first_n. This is in a separate function so that we can get the full
/// type of the binary heap, which includes a lambda for reversing the ordering fromt the passed
/// sort_by function. By declaring a new function, we can use generics to describe its type, and
/// then can use that type while unsafely casting pointers.
#[cfg(feature = "parallel_stream")]
async fn impl_keep_first_n<SInput, T, F, FReversed>(
    mut sinput: SInput,
    mut first_n: BinaryHeap<T, binary_heap_plus::FnComparator<FReversed>>,
    n: usize,
    sorted_by: F,
) -> futures::stream::Iter<std::vec::IntoIter<T>>
where
    SInput: Stream<Item = T> + Send + Unpin + std::marker::Sync + 'static,
    T: Clone + Send + std::marker::Sync + std::fmt::Debug + 'static,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + std::marker::Copy + 'static,
    FReversed: Fn(&T, &T) -> std::cmp::Ordering + Clone + Send,
{
    // Iterate through values in a single thread until we have seen n values.
    while first_n.len() < n {
        if let Some(item) = sinput.next().await {
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
    // kept value, updating the kept values only when a keepable value is found. This
    // is done in parallel.
    let first_n_mutex = Arc::new(parking_lot::Mutex::new(first_n));
    let smallest_kept = Arc::new(parking_lot::RwLock::new(
        first_n_mutex.lock().peek().unwrap().to_owned(),
    ));
    {
        let smallest_kept_ptr = &smallest_kept as *const Arc<parking_lot::RwLock<T>> as usize;
        let first_n_ptr = &first_n_mutex
            as *const Arc<
                parking_lot::Mutex<BinaryHeap<T, binary_heap_plus::FnComparator<FReversed>>>,
            > as usize;
        parallel_stream::from_stream(sinput)
            .for_each(move |item| async move {
                let smallest_kept_arc =
                    unsafe { &*(smallest_kept_ptr as *const Arc<parking_lot::RwLock<T>>) };
                if sorted_by(smallest_kept_arc.read().deref(), &item) == Ordering::Less {
                    let first_n_arc = unsafe {
                        &*(first_n_ptr
                            as *const Arc<
                                parking_lot::Mutex<
                                    BinaryHeap<T, binary_heap_plus::FnComparator<FReversed>>,
                                >,
                            >)
                    };
                    let mut update_first_n = first_n_arc.lock();
                    update_first_n.pop();
                    update_first_n.push(item);
                    let mut update_smallest_kept = smallest_kept_arc.write();
                    *update_smallest_kept = update_first_n.peek().unwrap().to_owned();
                }
            })
            .await;
    }
    futures::stream::iter(
        Arc::try_unwrap(first_n_mutex)
            .unwrap()
            .into_inner()
            .into_sorted_vec()
            .into_iter(),
    )
}

#[async_trait]
#[cfg(not(feature = "parallel_stream"))]
impl<SInput, T, F> KeepFirstN<T, F> for SInput
where
    SInput: Stream<Item = T> + Send + Unpin,
    T: Clone + Send + std::marker::Sync,
    F: Fn(&T, &T) -> Ordering + std::marker::Send + std::marker::Sync + 'static,
{
    async fn keep_first_n(
        mut self,
        n: usize,
        sorted_by: F,
    ) -> futures::stream::Iter<std::vec::IntoIter<T>> {
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
}
