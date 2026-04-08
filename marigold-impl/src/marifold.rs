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

    /// Folding over an empty stream returns the initial state wrapped in a single-item stream.
    #[tokio::test]
    async fn empty_stream_returns_init() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .marifold(42i32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec![42]);
    }

    /// The output is always exactly one item regardless of how many items the input has.
    #[tokio::test]
    async fn always_produces_exactly_one_item() {
        for n in [0usize, 1, 10, 1000] {
            let count = futures::stream::iter(0..n)
                .marifold(0usize, |acc, _| async move { acc + 1 })
                .await
                .collect::<Vec<_>>()
                .await
                .len();
            assert_eq!(count, 1, "expected 1 output item for input of length {n}");
        }
    }

    /// State can be a non-numeric type such as a Vec accumulator (string building).
    #[tokio::test]
    async fn non_numeric_string_accumulator() {
        let result = futures::stream::iter(vec!["a", "b", "c"])
            .marifold(String::new(), |mut acc, s| async move {
                acc.push_str(s);
                acc
            })
            .await
            .collect::<Vec<_>>()
            .await;
        assert_eq!(result, vec!["abc"]);
    }
}
