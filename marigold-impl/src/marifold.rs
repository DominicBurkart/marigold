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

    #[tokio::test]
    async fn fold_empty_stream() {
        let result: Vec<u64> = futures::stream::iter(Vec::<u64>::new())
            .marifold(0_u64, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![0]);
    }

    #[tokio::test]
    async fn fold_always_produces_single_item() {
        let result: Vec<u32> = futures::stream::iter(vec![1_u32, 2, 3, 4, 5])
            .marifold(0_u32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result.len(), 1);
    }

    #[tokio::test]
    async fn fold_nonzero_initial_state() {
        let result: Vec<u32> = futures::stream::iter(vec![1_u32, 2, 3])
            .marifold(10_u32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![16]);
    }

    #[tokio::test]
    async fn fold_single_item_stream() {
        let result: Vec<u32> = futures::stream::iter(vec![42_u32])
            .marifold(0_u32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![42]);
    }
}
