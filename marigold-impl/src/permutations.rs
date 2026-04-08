use async_trait::async_trait;
use futures::stream::Stream;
use futures::stream::StreamExt;
use itertools::Permutations;
use tracing::instrument;

#[async_trait]
pub trait Permutable<T: Clone> {
    async fn permutations(
        self,
        k: usize,
    ) -> futures::stream::Iter<Permutations<std::vec::IntoIter<T>>>;

    async fn permutations_with_replacement(
        self,
        k: usize,
    ) -> futures::stream::Iter<itertools::structs::MultiProduct<std::vec::IntoIter<T>>>;
}

/// This is a glue trait to allow streams to use Permutable in itertools.
/// The current implementation eagerly consumes the parent stream.
#[async_trait]
impl<T, SInput> Permutable<T> for SInput
where
    SInput: Stream<Item = T> + Send,
    T: Clone + Send + std::fmt::Debug,
{
    #[instrument(skip(self))]
    async fn permutations(
        self,
        k: usize,
    ) -> futures::stream::Iter<Permutations<std::vec::IntoIter<T>>> {
        use itertools::Itertools;

        let permutations_iterable = self.collect::<Vec<_>>().await.into_iter().permutations(k);
        futures::stream::iter(permutations_iterable)
    }

    #[instrument(skip(self))]
    async fn permutations_with_replacement(
        self,
        k: usize,
    ) -> futures::stream::Iter<itertools::structs::MultiProduct<std::vec::IntoIter<T>>> {
        use itertools::Itertools;

        let items = self.collect::<Vec<_>>().await;
        let iterators: Vec<std::vec::IntoIter<T>> =
            (0..k).map(|_| items.clone().into_iter()).collect();
        let product = iterators.into_iter().multi_cartesian_product();
        futures::stream::iter(product)
    }
}

#[cfg(test)]
mod tests {
    use super::Permutable;
    use futures::stream::StreamExt;

    #[tokio::test]
    async fn permutations() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3])
                .permutations(2)
                .await
                .collect::<Vec<_>>()
                .await,
            vec![
                vec![1, 2],
                vec![1, 3],
                vec![2, 1],
                vec![2, 3],
                vec![3, 1],
                vec![3, 2]
            ]
        );
    }

    #[tokio::test]
    async fn permutations_with_replacement() {
        assert_eq!(
            futures::stream::iter(vec![0, 1, 2])
                .permutations_with_replacement(2)
                .await
                .collect::<Vec<_>>()
                .await,
            vec![
                vec![0, 0],
                vec![0, 1],
                vec![0, 2],
                vec![1, 0],
                vec![1, 1],
                vec![1, 2],
                vec![2, 0],
                vec![2, 1],
                vec![2, 2],
            ]
        );
    }

    /// permutations(k) where k > len yields no permutations.
    #[tokio::test]
    async fn k_greater_than_len_yields_empty() {
        let result = futures::stream::iter(vec![1, 2])
            .permutations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// permutations(0) yields exactly one empty permutation.
    #[tokio::test]
    async fn k_zero_yields_one_empty_permutation() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .permutations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![] as Vec<i32>]);
    }

    /// permutations on an empty stream with k > 0 yields no permutations.
    #[tokio::test]
    async fn empty_stream_yields_no_permutations() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .permutations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// The count of permutations(n, k) == n! / (n-k)!  (P(n,k)).
    #[tokio::test]
    async fn permutation_count_matches_formula() {
        // P(4, 2) = 4 * 3 = 12
        let result = futures::stream::iter(0..4i32)
            .permutations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 12);
    }

    /// permutations_with_replacement(n, k) count == n^k.
    #[tokio::test]
    async fn permutations_with_replacement_count_matches_formula() {
        // 3^3 = 27
        let result = futures::stream::iter(0..3i32)
            .permutations_with_replacement(3)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 27);
    }

    /// permutations_with_replacement on empty stream with k > 0 yields no results.
    #[tokio::test]
    async fn permutations_with_replacement_empty_stream() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .permutations_with_replacement(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }
}
