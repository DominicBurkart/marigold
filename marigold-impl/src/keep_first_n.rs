use async_trait::async_trait;
use binary_heap_plus::BinaryHeap;
use futures::stream::Stream;
use futures::stream::StreamExt;
use std::cmp::Ordering;

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

        self.for_each(|item| async {
            if sorted_by(&*smallest_kept.read(), &item) == Ordering::Less {
                let mut first_n_mut = first_n_mutex.lock();
                first_n_mut.pop();
                first_n_mut.push(item);
                let mut update_smallest_kept = smallest_kept.write();
                *update_smallest_kept = first_n_mut.peek().unwrap().to_owned();
            }
        })
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
