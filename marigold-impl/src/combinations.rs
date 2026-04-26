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

    /// C(3,2) produces exactly the three expected pairs.
    #[tokio::test]
    async fn combinations_c3_2() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3])
                .combinations(2)
                .await
                .collect::<Vec<_>>()
                .await,
            vec![vec![1, 2], vec![1, 3], vec![2, 3],]
        );
    }

    /// Empty input stream yields no combinations for any k.
    #[tokio::test]
    async fn combinations_empty_input() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(
            result.is_empty(),
            "expected no combinations from empty input"
        );
    }

    /// A single-element stream with k=1 yields exactly that one element as a
    /// length-1 combination.
    #[tokio::test]
    async fn combinations_single_item() {
        let result = futures::stream::iter(vec![42])
            .combinations(1)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![42]]);
    }

    /// No combination appears more than once in the output.
    #[tokio::test]
    async fn combinations_no_duplicates() {
        let result = futures::stream::iter(vec![1, 2, 3, 4])
            .combinations(2)
            .await
            .collect::<Vec<_>>()
            .await;

        // C(4,2) = 6 combinations.
        assert_eq!(result.len(), 6);

        // Every combination is unique.
        let mut seen = std::collections::HashSet::new();
        for combo in &result {
            let inserted = seen.insert(combo.clone());
            assert!(inserted, "duplicate combination found: {:?}", combo);
        }
    }

    /// Choosing k larger than the stream length produces no combinations.
    #[tokio::test]
    async fn combinations_k_larger_than_input() {
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2])
            .combinations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(
            result.is_empty(),
            "expected no combinations when k > input length"
        );
    }

    /// k=0 on a non-empty input yields exactly one empty combination (itertools
    /// semantics, symmetric with `permutations(0)` on a non-empty input).
    #[tokio::test]
    async fn combinations_k0_yields_one_empty_combination() {
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3])
            .combinations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(
            result,
            vec![vec![] as Vec<i32>],
            "itertools yields one empty combination for k=0 on a non-empty input"
        );
    }
}
