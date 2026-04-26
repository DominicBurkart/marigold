use async_trait::async_trait;
use futures::stream::Stream;
use futures::StreamExt;
use tracing::instrument;

#[async_trait]
pub trait CollectAndAppliable<T, U, F>
where
    F: Fn(Vec<T>) -> U,
{
    /// Collects all values from the stream into a vector and gives the vector to passed function.
    async fn collect_and_apply(self, f: F) -> U;
}

#[async_trait]
impl<SInput, T, U, F> CollectAndAppliable<T, U, F> for SInput
where
    SInput: Stream<Item = T> + Send + Unpin + std::fmt::Debug,
    T: Clone + Send + std::fmt::Debug,
    F: std::marker::Send + Fn(Vec<T>) -> U + 'static,
{
    #[instrument(skip(self, f))]
    async fn collect_and_apply(mut self, f: F) -> U {
        let values = self.collect::<Vec<_>>().await;
        f(values)
    }
}

#[cfg(test)]
mod tests {
    use super::CollectAndAppliable;

    #[tokio::test]
    async fn collect_and_apply() {
        assert_eq!(
            futures::stream::iter(1..=3).collect_and_apply(|x| x).await,
            vec![1, 2, 3]
        );
    }

    /// Verify that the closure receives the full collected Vec and can
    /// transform it arbitrarily (here: reverse the order).
    #[tokio::test]
    async fn collect_and_apply_transform() {
        let result = futures::stream::iter(1..=4)
            .collect_and_apply(|mut v| {
                v.reverse();
                v
            })
            .await;
        assert_eq!(result, vec![4, 3, 2, 1]);
    }

    /// Verify that the closure can reduce the Vec to a scalar value.
    #[tokio::test]
    async fn collect_and_apply_sum() {
        let result: i32 = futures::stream::iter(1..=5)
            .collect_and_apply(|v| v.into_iter().sum())
            .await;
        assert_eq!(result, 15);
    }
}
