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
        let mut first_n = BinaryHeap::with_capacity_by(n, move |a, b| match sorted_by(a, b) {
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
        let first_n_mutex = Arc::new(parking_lot::Mutex::new(first_n));
        let smallest_kept = Arc::new(parking_lot::RwLock::new(
            first_n_mutex.lock().peek().unwrap().to_owned(),
        ));
        {
            let smallest_kept_ptr = Arc::as_ptr(&smallest_kept) as usize;
            let first_n_ptr = Arc::as_ptr(&first_n_mutex) as usize;
            parallel_stream::from_stream(self)
                .for_each(move |item| async move {
                    let smallest_kept_arc = unsafe {
                        Arc::from_raw(smallest_kept_ptr as *const parking_lot::RwLock<T>)
                    };
                    if sorted_by(smallest_kept_arc.read().deref(), &item) == Ordering::Less {
                        let first_n_arc = unsafe {
                            Arc::from_raw(
                                first_n_ptr
                                    as *const parking_lot::Mutex<
                                        BinaryHeap<T, binary_heap_plus::FnComparator<F>>,
                                    >,
                            )
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
