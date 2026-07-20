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

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn spawn_runs_future_to_completion() {
        let handle = spawn(async { 42u32 });
        assert_eq!(handle.await.unwrap(), 42);
    }

    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn spawn_unit_future() {
        let (tx, rx) = tokio::sync::oneshot::channel::<u8>();
        spawn(async move {
            tx.send(1).ok();
        });
        assert_eq!(rx.await.unwrap(), 1);
    }
}
