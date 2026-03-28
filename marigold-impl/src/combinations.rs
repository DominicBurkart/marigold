use async_trait::async_trait;
use futures::stream::Stream;
use futures::stream::StreamExt;
use itertools::Combinations;
use tracing::instrument;

#[async_trait]
pub trait Combinable<T> {
    async fn combinations(
        self,
        k: usize,
    ) -> futures::stream::Iter<Combinations<std::vec::IntoIter<T>>>;
}

/// This is a glue trait to allow streams to use Combinable in itertools.
/// The current implementation eagerly consumes the parent stream.
#[async_trait]
impl<T, SInput> Combinable<T> for SInput
where
    SInput: Stream<Item = T> + Send,
    T: Clone + Send + std::fmt::Debug,
{
    #[instrument(skip(self))]
    async fn combinations(
        self,
        k: usize,
    ) -> futures::stream::Iter<Combinations<std::vec::IntoIter<T>>> {
        use itertools::Itertools;

        let combinations_iterable = self.collect::<Vec<_>>().await.into_iter().combinations(k);
        futures::stream::iter(combinations_iterable)
    }
}

#[cfg(test)]
mod tests {
    use super::Combinable;
    use futures::stream::StreamExt;

    #[tokio::test]
    async fn combinations() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3])
                .combinations(2)
                .await
                .collect::<Vec<_>>()
                .await,
            vec![vec![1, 2], vec![1, 3], vec![2, 3],]
        );
    }

    #[tokio::test]
    async fn combinations_empty_stream() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty(), "combinations of empty stream should be empty");
    }

    #[tokio::test]
    async fn combinations_k_zero() {
        // C(n, 0) = 1 (the empty combination)
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<Vec<i32>>>()
            .await;
        assert_eq!(result, vec![Vec::<i32>::new()]);
    }

    #[tokio::test]
    async fn combinations_k_equals_n() {
        // C(n, n) = 1
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![1, 2, 3]]);
    }

    #[tokio::test]
    async fn combinations_k_greater_than_n() {
        // C(n, k) where k > n = 0
        let result = futures::stream::iter(vec![1, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty(), "C(2, 5) should produce no combinations");
    }

    #[tokio::test]
    async fn combinations_single_element_k_one() {
        let result = futures::stream::iter(vec![42])
            .combinations(1)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![42]]);
    }

    #[tokio::test]
    async fn combinations_preserves_count() {
        // C(5, 2) = 10
        let result = futures::stream::iter(vec![1, 2, 3, 4, 5])
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 10);
    }
}
