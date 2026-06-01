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
    async fn empty_stream_returns_init() {
        let result = futures::stream::iter(std::iter::empty::<u32>())
            .marifold(42u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn single_element_stream() {
        let result = futures::stream::iter(vec![7u32])
            .marifold(0u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(result, vec![7]);
    }

    #[tokio::test]
    async fn string_accumulator() {
        let result = futures::stream::iter(vec!["a", "b", "c"])
            .marifold(String::new(), |mut acc, x| async move {
                acc.push_str(x);
                acc
            })
            .await
            .collect::<Vec<String>>()
            .await;
        assert_eq!(result, vec!["abc".to_string()]);
    }

    #[tokio::test]
    async fn result_is_single_item_stream() {
        let stream = futures::stream::iter(0..3u32)
            .marifold(0u32, |acc, x| async move { acc + x })
            .await;
        let items = stream.collect::<Vec<u32>>().await;
        assert_eq!(items.len(), 1, "marifold must produce exactly one item");
        assert_eq!(items[0], 3);
    }
}
