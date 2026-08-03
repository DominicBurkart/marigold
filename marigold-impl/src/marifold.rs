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
        let result: Vec<i32> = futures::stream::iter(std::iter::empty::<i32>())
            .marifold(42i32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn fold_single_element() {
        let result: Vec<i32> = futures::stream::iter(vec![7i32])
            .marifold(0i32, |acc, x| async move { acc + x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![7]);
    }

    #[tokio::test]
    async fn fold_string_concatenation() {
        let words = vec!["hello", " ", "world"];
        let result: Vec<String> = futures::stream::iter(words)
            .marifold(String::new(), |mut acc, x| async move {
                acc.push_str(x);
                acc
            })
            .await
            .collect()
            .await;
        assert_eq!(result, vec!["hello world"]);
    }

    #[tokio::test]
    async fn fold_produces_single_value_stream() {
        let stream = futures::stream::iter(0..100i64)
            .marifold(0i64, |acc, x| async move { acc + x })
            .await;
        let results: Vec<i64> = stream.collect().await;
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], 4950);
    }

    #[tokio::test]
    async fn fold_with_multiplication() {
        let result: Vec<u64> = futures::stream::iter(1u64..=5u64)
            .marifold(1u64, |acc, x| async move { acc * x })
            .await
            .collect()
            .await;
        assert_eq!(result, vec![120]);
    }
}
