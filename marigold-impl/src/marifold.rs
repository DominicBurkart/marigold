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
        // An empty stream should yield a stream containing just the initial state.
        let result: Vec<u32> = futures::stream::iter(std::iter::empty::<u32>())
            .marifold(42u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(result, vec![42]);
    }

    #[tokio::test]
    async fn fold_single_element() {
        let result: Vec<u32> = futures::stream::iter(std::iter::once(5u32))
            .marifold(0u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(result, vec![5]);
    }

    #[tokio::test]
    async fn fold_string_accumulation() {
        let words = vec!["hello", " ", "world"];
        let result: Vec<String> = futures::stream::iter(words)
            .marifold(String::new(), |mut acc, word| async move {
                acc.push_str(word);
                acc
            })
            .await
            .collect::<Vec<String>>()
            .await;
        assert_eq!(result, vec!["hello world"]);
    }

    #[tokio::test]
    async fn fold_result_is_exactly_one_element() {
        // marifold always produces a stream with exactly one element (the final state).
        let result: Vec<u32> = futures::stream::iter(0..10u32)
            .marifold(0u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(
            result.len(),
            1,
            "marifold should produce exactly one element, got {}: {result:?}",
            result.len()
        );
        assert_eq!(result[0], 45); // 0+1+2+...+9 = 45
    }

    #[tokio::test]
    async fn fold_with_multiplication() {
        let result: Vec<u64> = futures::stream::iter(1..=5u64)
            .marifold(1u64, |acc, x| async move { acc * x })
            .await
            .collect::<Vec<u64>>()
            .await;
        assert_eq!(result, vec![120]); // 5! = 120
    }
}
