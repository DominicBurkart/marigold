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

    /// k=0 yields exactly one combination: the empty set.
    #[tokio::test]
    async fn k_zero_yields_one_empty_combination() {
        let result = futures::stream::iter(vec![1i32, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![] as Vec<i32>]);
    }

    /// k=1 is equivalent to wrapping each element in a singleton vec.
    #[tokio::test]
    async fn k_one_yields_singletons() {
        let result = futures::stream::iter(vec![10i32, 20, 30])
            .combinations(1)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![10], vec![20], vec![30]]);
    }

    /// k=n (take all items) yields exactly one combination containing all elements.
    #[tokio::test]
    async fn k_equals_n_yields_one_full_combination() {
        let result = futures::stream::iter(vec![1i32, 2, 3])
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![1, 2, 3]]);
    }

    /// k > n yields no combinations (not enough elements to form any group).
    #[tokio::test]
    async fn k_greater_than_n_yields_nothing() {
        let result = futures::stream::iter(vec![1i32, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(
            result.is_empty(),
            "expected no combinations when k > n, got {result:?}"
        );
    }

    /// An empty source stream with any k > 0 yields no combinations.
    #[tokio::test]
    async fn empty_source_yields_nothing() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// combinations(k) produces exactly C(n, k) results (count invariant).
    #[tokio::test]
    async fn count_matches_binomial_coefficient() {
        // C(6, 3) = 20
        let result = futures::stream::iter(0i32..6)
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 20);
    }
}
