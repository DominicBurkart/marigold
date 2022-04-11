use async_trait::async_trait;
use futures::Stream;
use futures::StreamExt;

#[async_trait]
pub trait Vectable<T> {
    async fn to_vec(self) -> Vec<T>;
}

/// Glue trait to turn streams into vectors.
#[async_trait]
impl<T, SInput> Vectable<T> for SInput
where
    SInput: Stream<Item = T> + Send,
    T: Clone + Send,
{
    async fn to_vec(self) -> Vec<T> {
        self.collect::<Vec<_>>().await
    }
}

#[cfg(test)]
mod tests {
    use super::Vectable;

    #[tokio::test]
    async fn combinations() {
        assert_eq!(
            futures::stream::iter(vec![1, 2, 3]).to_vec().await,
            vec![1, 2, 3]
        );
    }
}
