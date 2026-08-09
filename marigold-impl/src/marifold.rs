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

    /// Basic fold: sum 0+1+2+3+4 = 10.
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

    /// Folding an empty stream should yield the init value unchanged.
    #[tokio::test]
    async fn fold_empty_stream() {
        let result = futures::stream::iter(Vec::<u32>::new())
            .marifold(42_u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(
            result,
            vec![42_u32],
            "empty stream should return the init value"
        );
    }

    /// Folding a single-element stream applies the function exactly once.
    #[tokio::test]
    async fn fold_single_item() {
        let result = futures::stream::iter(vec![5_u32])
            .marifold(10_u32, |acc, x| async move { acc + x })
            .await
            .collect::<Vec<u32>>()
            .await;
        assert_eq!(
            result,
            vec![15_u32],
            "single item: 10 (init) + 5 = 15"
        );
    }

    /// Fold can accumulate into a String (non-numeric accumulator).
    #[tokio::test]
    async fn fold_string_concatenation() {
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
}
