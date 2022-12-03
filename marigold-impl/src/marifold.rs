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
}
