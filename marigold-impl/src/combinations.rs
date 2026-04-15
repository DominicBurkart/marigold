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

/// Implements [`Combinable`] for any stream, eagerly consuming it to produce combinations.
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
    async fn combinations_basic() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3])
                .combinations(2)
                .await
                .collect::<Vec<_>>()
                .await,
            vec![vec![1, 2], vec![1, 3], vec![2, 3]]
        );
    }

    #[tokio::test]
    async fn combinations_k_zero() {
        // k=0 yields exactly one combination: the empty set
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![Vec::<i32>::new()]);
    }

    #[tokio::test]
    async fn combinations_k_equals_length() {
        // k = len yields exactly one combination containing all elements
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3])
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![1, 2, 3]]);
    }

    #[tokio::test]
    async fn combinations_k_exceeds_length() {
        // k > len yields nothing
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn combinations_empty_stream() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn combinations_count() {
        // C(5,2) = 10
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3, 4, 5])
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 10);
    }
}
