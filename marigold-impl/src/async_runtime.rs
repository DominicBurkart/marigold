#[cfg(feature = "tokio")]
pub fn spawn<T>(future: T) -> tokio::task::JoinHandle<T::Output>
where
    T: std::future::Future + Send + 'static,
    T::Output: Send + 'static,
{
    tokio::spawn(future)
}

#[cfg(all(feature = "async-std", not(feature = "tokio")))]
pub fn spawn<T>(future: T) -> async_std::task::JoinHandle<T::Output>
where
    T: std::future::Future + Send + 'static,
    T::Output: Send + 'static,
{
    async_std::task::spawn(future)
}

#[cfg(all(test, feature = "tokio"))]
mod tests {
    #[tokio::test]
    async fn tokio_spawn_executes_future() {
        let handle = super::spawn(async { 42_u32 });
        let result = handle.await.unwrap();
        assert_eq!(result, 42);
    }

    #[tokio::test]
    async fn tokio_spawn_handles_computation() {
        let handle = super::spawn(async { (1_u32..=10).sum::<u32>() });
        assert_eq!(handle.await.unwrap(), 55);
    }
}
