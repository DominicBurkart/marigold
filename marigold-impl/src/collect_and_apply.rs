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

    /// Identity function: collected items are passed through unchanged.
    #[tokio::test]
    async fn collect_and_apply_identity() {
        assert_eq!(
            futures::stream::iter(1..=3).collect_and_apply(|x| x).await,
            vec![1, 2, 3]
        );
    }

    /// Empty input: the function receives an empty Vec and its return value is
    /// emitted as the single output of `collect_and_apply`.
    #[tokio::test]
    async fn collect_and_apply_empty_input() {
        let result: Vec<i32> = futures::stream::iter(Vec::<i32>::new())
            .collect_and_apply(|v| v)
            .await;
        assert!(result.is_empty(), "expected empty vec from empty input");
    }

    /// The function result is returned directly — here we transform the collected
    /// Vec into a scalar (sum), verifying the return type is whatever the closure
    /// produces.
    #[tokio::test]
    async fn collect_and_apply_returns_function_result() {
        let sum: i32 = futures::stream::iter(vec![1, 2, 3, 4])
            .collect_and_apply(|v| v.iter().sum())
            .await;
        assert_eq!(sum, 10);
    }

    /// Transformation: the closure can transform the collected items into a
    /// different shape.  Here we double every element and verify the result.
    #[tokio::test]
    async fn collect_and_apply_transform() {
        let doubled: Vec<i32> = futures::stream::iter(vec![1, 2, 3])
            .collect_and_apply(|v| v.into_iter().map(|x| x * 2).collect())
            .await;
        assert_eq!(doubled, vec![2, 4, 6]);
    }

    /// Empty input with a non-trivial function: the function is still called
    /// (with an empty vec) and its result is the output.
    #[tokio::test]
    async fn collect_and_apply_empty_input_with_transform() {
        let result: i32 = futures::stream::iter(Vec::<i32>::new())
            .collect_and_apply(|v| v.iter().sum())
            .await;
        assert_eq!(result, 0, "sum of empty slice should be 0");
    }

    /// The closure receives the items in stream order.
    #[tokio::test]
    async fn collect_and_apply_preserves_order() {
        let result: Vec<i32> = futures::stream::iter(vec![3, 1, 2])
            .collect_and_apply(|v| v)
            .await;
        assert_eq!(result, vec![3, 1, 2]);
    }

    /// collect_and_apply with a function that produces a stream output
    /// (using genawaiter, as exercised by the permutations_with_replacement
    /// integration test).
    #[tokio::test]
    async fn collect_and_apply_returns_async_generator() {
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
}
