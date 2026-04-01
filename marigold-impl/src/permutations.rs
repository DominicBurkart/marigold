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

    /// P(3, 2): 6 ordered pairs drawn from [1,2,3].
    #[tokio::test]
    async fn permutations_p3_2() {
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

    /// P(3, 3) = 6 full-length permutations of [1,2,3].
    #[tokio::test]
    async fn permutations_p3_full() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .permutations(3)
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(result.len(), 6, "P(3) should produce exactly 6 permutations");

        // Every output is a valid permutation of the input (same multiset of values).
        let mut input_sorted = vec![1, 2, 3];
        input_sorted.sort();
        for perm in &result {
            let mut p = perm.clone();
            p.sort();
            assert_eq!(p, input_sorted, "permutation {:?} contains unexpected values", perm);
        }

        // All permutations are distinct.
        let mut seen = std::collections::HashSet::new();
        for perm in &result {
            let inserted = seen.insert(perm.clone());
            assert!(inserted, "duplicate permutation: {:?}", perm);
        }
    }

    /// Empty input stream: itertools yields no permutations (not even an empty one).
    /// Invariant: permutations of an empty set with k=0 yields one empty permutation;
    /// with k>0 yields nothing.
    #[tokio::test]
    async fn permutations_empty_input_nonzero_k() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(
            result.is_empty(),
            "expected no permutations from empty input with k=2"
        );
    }

    #[tokio::test]
    async fn permutations_empty_input_k0() {
        // k=0 on any non-empty input produces exactly one empty permutation (itertools
        // semantics); on an empty input it still produces one empty permutation.
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(
            result,
            vec![vec![] as Vec<i32>],
            "itertools yields one empty permutation for k=0"
        );
    }

    /// A single-element stream with k=1 yields exactly one permutation containing
    /// that element.
    #[tokio::test]
    async fn permutations_single_item() {
        let result = futures::stream::iter(vec![7])
            .permutations(1)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![vec![7]]);
    }

    /// permutations_with_replacement on [0,1,2] with k=2 produces all 9 ordered
    /// pairs (including self-pairs).
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

    /// permutations_with_replacement on an empty stream yields no results.
    #[tokio::test]
    async fn permutations_with_replacement_empty_input() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations_with_replacement(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(
            result.is_empty(),
            "expected no results from permutations_with_replacement on empty input"
        );
    }
}
