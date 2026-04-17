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

    /// Basic fold: sum 0..5 = 10.
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

    /// Folding an empty stream returns the initial accumulator as the sole item.
    #[tokio::test]
    async fn fold_empty_stream_returns_init() {
        let result: Vec<i32> = futures::stream::iter(Vec::<i32>::new())
            .marifold(42, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![42]);
    }

    /// Folding a single-element stream applies the function exactly once.
    #[tokio::test]
    async fn fold_single_element() {
        let result: Vec<i32> = futures::stream::iter(vec![7i32])
            .marifold(0, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![7]);
    }

    /// marifold always produces exactly one output item regardless of input length.
    #[tokio::test]
    async fn fold_always_produces_one_item() {
        for len in [0usize, 1, 10, 100] {
            let count: usize = futures::stream::iter(vec![1u32; len])
                .marifold(0u32, |acc, x| async move { acc + x })
                .await
                .collect::<Vec<_>>()
                .await
                .len();
            assert_eq!(count, 1, "expected 1 item for input length {len}");
        }
    }
}
