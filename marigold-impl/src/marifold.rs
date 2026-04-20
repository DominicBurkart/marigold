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
///
/// The name `marifold` is a portmanteau of "marigold" and "fold": it wraps
/// [`futures::StreamExt::fold`] so that the result is itself a single-item
/// stream rather than a bare future, keeping it composable with other stream
/// operations in a Marigold pipeline.
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

    #[tokio::test]
    async fn fold_empty_stream() {
        // An empty stream should yield exactly one item: the initial state.
        let result: Vec<u32> = futures::stream::iter(std::iter::empty::<u32>())
            .marifold(42u32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn fold_non_zero_init() {
        // Verify that the initial state is respected.
        let result: Vec<i32> = futures::stream::iter(1..=3)
            .marifold(100i32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        // 100 + 1 + 2 + 3 = 106
        assert_eq!(result, vec![106]);
    }

    #[tokio::test]
    async fn fold_single_item() {
        let result: Vec<i32> = futures::stream::iter(std::iter::once(7i32))
            .marifold(0i32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![7]);
    }
}
