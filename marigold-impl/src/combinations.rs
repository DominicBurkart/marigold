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

    /// combinations(k) where k == len yields exactly one combination: the full set.
    #[tokio::test]
    async fn k_equals_len_yields_one_combination() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![1, 2, 3]]);
    }

    /// combinations(k) where k > len yields no combinations.
    #[tokio::test]
    async fn k_greater_than_len_yields_empty() {
        let result = futures::stream::iter(vec![1, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// combinations(0) yields exactly one empty combination (the empty set).
    #[tokio::test]
    async fn k_zero_yields_one_empty_combination() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![] as Vec<i32>]);
    }

    /// combinations on an empty stream yields no combinations for any k > 0.
    #[tokio::test]
    async fn empty_stream_yields_no_combinations() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// The total count of combinations(n, k) == n! / (k! * (n-k)!).
    #[tokio::test]
    async fn combination_count_matches_formula() {
        // C(5, 2) = 10
        let result = futures::stream::iter(0..5i32)
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 10);
    }
}
