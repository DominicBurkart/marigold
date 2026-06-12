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
mod tokio_tests {
    use super::*;

    #[tokio::test]
    async fn spawn_yields_correct_output() {
        let handle = spawn(async { 42_u32 });
        assert_eq!(handle.await.unwrap(), 42);
    }
}

#[cfg(all(test, feature = "async-std", not(feature = "tokio")))]
mod async_std_tests {
    use super::*;

    #[test]
    fn spawn_yields_correct_output() {
        let result = async_std::task::block_on(async {
            let handle = spawn(async { 42_u32 });
            handle.await
        });
        assert_eq!(result, 42);
    }
}
