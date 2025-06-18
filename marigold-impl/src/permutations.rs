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
}
