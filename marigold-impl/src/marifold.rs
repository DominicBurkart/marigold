use async_trait::async_trait;
use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::Future;
use tracing::instrument;

#[async_trait]
pub trait Marifold<SInput, State, F, Fut> {
    async fn marifold(
        self,
        init: State,
        f: F,
    ) -> futures::stream::Once<futures::stream::Fold<SInput, Fut, State, F>>;
}

/// This is an adapter trait that allows fold from StreamExt to return a Stream
/// with a single value (the state after the parent stream has been exhausted).
#[async_trait]
impl<State, SInput, T, F, Fut> Marifold<SInput, State, F, Fut> for SInput
where
    SInput: Stream<Item = T> + Send + Sized,
    T: Clone + Send + std::fmt::Debug,
    F: FnMut(State, T) -> Fut + std::marker::Send + 'static,
    State: std::fmt::Debug + std::marker::Send + 'static,
    Fut: Future<Output = State>,
{
    #[instrument(skip(self, f))]
    async fn marifold(
        self,
        init: State,
        f: F,
    ) -> futures::stream::Once<futures::stream::Fold<SInput, Fut, State, F>> {
        futures::stream::once(self.fold(init, f))
    }
}

#[cfg(test)]
mod tests {
    use super::Marifold;
    use futures::stream::StreamExt;

    #[tokio::test]
    async fn fold() {
        assert_eq!(
            futures::stream::iter(0..5)
                .marifold(0, |acc, x| async move { acc + x })
                .await
                .collect::<Vec<u8>>()
                .await,
            vec![10]
        );
    }

    /// An empty stream produces exactly one item: the initial accumulator value.
    #[tokio::test]
    async fn empty_stream_returns_init() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .marifold(99i32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<i32>>()
            .await;
        assert_eq!(result, vec![99]);
    }

    /// A single-element stream folds correctly with the initial value.
    #[tokio::test]
    async fn single_element_fold() {
        let result = futures::stream::iter(vec![7i32])
            .marifold(3i32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<i32>>()
            .await;
        assert_eq!(result, vec![10]);
    }

    /// The output is always a stream of exactly one element, regardless of input length.
    #[tokio::test]
    async fn output_is_always_single_element() {
        for n in [0usize, 1, 10, 100] {
            let count = futures::stream::iter(0..n)
                .marifold(0usize, |acc, _| async move { acc + 1 })
                .await
                .collect::<Vec<_>>()
                .await
                .len();
            assert_eq!(count, 1, "expected exactly 1 output item for n={n}");
        }
    }

    /// Non-commutative fold (subtraction) validates that ordering is preserved.
    #[tokio::test]
    async fn non_commutative_fold_preserves_order() {
        // 0 - 1 - 2 - 3 = -6
        let result = futures::stream::iter(1i32..=3)
            .marifold(0i32, |acc, x| async move { acc - x })
            .await
            .collect::<Vec<i32>>()
            .await;
        assert_eq!(result, vec![-6]);
    }

    /// String concatenation confirms the fold works with non-numeric state.
    #[tokio::test]
    async fn string_accumulation() {
        let result = futures::stream::iter(vec!["b", "c", "d"])
            .marifold("a".to_string(), |mut acc, x| async move {
                acc.push_str(x);
                acc
            })
            .await
            .collect::<Vec<String>>()
            .await;
        assert_eq!(result, vec!["abcd"]);
    }
}
