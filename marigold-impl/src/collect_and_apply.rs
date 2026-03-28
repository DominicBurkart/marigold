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

    #[tokio::test]
    async fn permutations_with_replacement() {
        use futures::StreamExt;
        use genawaiter;

        let collected = futures::stream::iter(1..=3)
            .collect_and_apply(|values| async {
                genawaiter::sync::Gen::new(|co| async move {
                    for val0 in values.iter() {
                        for val1 in values.iter() {
                            co.yield_(vec![*val0, *val1]).await;
                        }
                    }
                })
            })
            .await
            .await
            .collect::<Vec<_>>()
            .await;

        assert_eq!(
            collected,
            vec![
                vec![1, 1],
                vec![1, 2],
                vec![1, 3],
                vec![2, 1],
                vec![2, 2],
                vec![2, 3],
                vec![3, 1],
                vec![3, 2],
                vec![3, 3]
            ]
        );
    }

    #[tokio::test]
    async fn collect_and_apply_empty_stream() {
        let result = futures::stream::iter(Vec::<i32>::new())
            .collect_and_apply(|v| v.len())
            .await;
        assert_eq!(result, 0, "applying to empty stream should yield empty vec");
    }

    #[tokio::test]
    async fn collect_and_apply_transforms_to_different_type() {
        let result = futures::stream::iter(vec![1, 2, 3, 4, 5])
            .collect_and_apply(|v| v.iter().sum::<i32>())
            .await;
        assert_eq!(result, 15);
    }

    #[tokio::test]
    async fn collect_and_apply_single_element() {
        let result = futures::stream::iter(vec![99])
            .collect_and_apply(|v| v)
            .await;
        assert_eq!(result, vec![99]);
    }

    #[tokio::test]
    async fn collect_and_apply_preserves_order() {
        let result = futures::stream::iter(vec![5, 3, 1, 4, 2])
            .collect_and_apply(|v| v)
            .await;
        assert_eq!(result, vec![5, 3, 1, 4, 2], "collected values should preserve stream order");
    }
}
