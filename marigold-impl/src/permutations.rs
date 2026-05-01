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
    async fn permutations_basic() {
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
    async fn permutations_with_replacement_basic() {
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

    #[tokio::test]
    async fn permutations_k_zero() {
        // k=0 yields exactly one permutation: the empty sequence
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3])
            .permutations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![Vec::<i32>::new()]);
    }

    #[tokio::test]
    async fn permutations_k_exceeds_length() {
        // k > len yields nothing
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2])
            .permutations(5)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn permutations_empty_stream() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn permutations_count() {
        // P(4,2) = 4!/(4-2)! = 12
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3, 4])
            .permutations(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result.len(), 12);
    }

    #[tokio::test]
    async fn permutations_with_replacement_empty_stream() {
        // empty stream yields nothing regardless of k
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations_with_replacement(2)
            .await
            .collect::<Vec<_>>()
            .await;
        assert!(result.is_empty());
    }

    /// P(3, 3) = 6 full-length permutations of [1,2,3].
    #[tokio::test]
    async fn permutations_p3_full() {
        let result = futures::stream::iter(vec![1, 2, 3])
            .permutations(3)
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(
            result.len(),
            6,
            "P(3) should produce exactly 6 permutations"
        );

        // Every output is a valid permutation of the input (same multiset of values).
        let mut input_sorted = vec![1, 2, 3];
        input_sorted.sort_unstable();
        for perm in &result {
            let mut p = perm.clone();
            p.sort_unstable();
            assert_eq!(
                p, input_sorted,
                "permutation {:?} contains unexpected values",
                perm
            );
        }

        // All permutations are distinct.
        let mut seen = std::collections::HashSet::new();
        for perm in &result {
            let inserted = seen.insert(perm.clone());
            assert!(inserted, "duplicate permutation: {:?}", perm);
        }
    }

    /// k=0 on a non-empty input: itertools yields exactly one empty permutation.
    /// This matches itertools' documented semantics: `permutations(0)` yields
    /// `[[]]` for any input (empty or not), because there is exactly one way
    /// to choose 0 elements.  The test uses a non-empty input to make the k=0
    /// invariant explicit — symmetrical with `combinations_k_zero` in
    /// combinations.rs.
    #[tokio::test]
    async fn permutations_k0_yields_one_empty_permutation() {
        let result: Vec<Vec<i32>> = futures::stream::iter(vec![1, 2, 3])
            .permutations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(
            result,
            vec![vec![] as Vec<i32>],
            "itertools yields one empty permutation for k=0 regardless of input length"
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

    /// Empty input with k=0: itertools yields exactly one empty permutation,
    /// `[[]]`, because there is exactly one way to choose 0 elements from any
    /// collection (including an empty one).  This completes the axis coverage:
    /// `permutations_empty_stream` covers empty-input + k>0,
    /// `permutations_k0_yields_one_empty_permutation` covers non-empty-input + k=0,
    /// and this test covers the empty-input + k=0 corner.
    #[tokio::test]
    async fn permutations_empty_input_k0_yields_one_empty_permutation() {
        let result: Vec<Vec<i32>> = futures::stream::iter(Vec::<i32>::new())
            .permutations(0)
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(
            result,
            vec![vec![] as Vec<i32>],
            "itertools yields [[]] for permutations(0) even on an empty input"
        );
    }
}
