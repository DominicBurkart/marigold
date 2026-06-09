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
    mod tokio_tests {
        use crate::async_runtime::spawn;

        #[tokio::test]
        async fn spawn_returns_value() {
            let handle = spawn(async { 42_u32 });
            assert_eq!(handle.await.unwrap(), 42);
        }

        #[tokio::test]
        async fn spawn_concurrent_tasks() {
            let handles: Vec<_> = (0_u32..5).map(|i| spawn(async move { i * i })).collect();
            let mut results = Vec::new();
            for h in handles {
                results.push(h.await.unwrap());
            }
            results.sort();
            assert_eq!(results, vec![0, 1, 4, 9, 16]);
        }

        #[tokio::test]
        async fn spawn_string_result() {
            let handle = spawn(async { "hello".to_string() });
            assert_eq!(handle.await.unwrap(), "hello");
        }
    }
}
