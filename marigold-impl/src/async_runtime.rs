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
    #[cfg(feature = "tokio")]
    #[tokio::test]
    async fn spawn_tokio_task_runs_to_completion() {
        let handle = super::spawn(async { 42u32 });
        assert_eq!(handle.await.unwrap(), 42);
    }
}
