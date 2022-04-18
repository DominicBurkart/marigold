use async_trait::async_trait;
use futures::Stream;
use futures::StreamExt;
use tracing::instrument;

#[async_trait]
pub trait Vectable<T> {
    async fn to_vec(self) -> Vec<T>;
}

/// Glue trait to turn streams into vectors.
#[async_trait]
impl<T, SInput> Vectable<T> for SInput
where
    SInput: Stream<Item = T> + Send,
    T: Clone + Send + std::fmt::Debug,
{
    #[instrument(skip(self))]
    async fn to_vec(self) -> Vec<T> {
        self.collect::<Vec<_>>().await
    }
}

#[cfg(test)]
mod tests {
    use super::Vectable;

    #[tokio::test]
    async fn to_vec() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3]).to_vec().await,
            vec![1, 2, 3]
        );
    }
}
