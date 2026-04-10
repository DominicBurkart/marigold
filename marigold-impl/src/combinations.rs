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
    async fn combinations_k2_from_3() {
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
    async fn combinations_k_equals_n_yields_one_result() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![1, 2, 3]]);
    }

    #[tokio::test]
    async fn combinations_k_greater_than_n_yields_nothing() {
        let result = futures::stream::iter(vec![1, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn combinations_k0_yields_one_empty_vec() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![]]);
    }
}
