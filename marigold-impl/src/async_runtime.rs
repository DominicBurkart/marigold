#[cfg(all(feature = "tokio", not(feature = "async-std")))]
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

// When both features are enabled, tokio takes precedence
#[cfg(all(feature = "tokio", feature = "async-std"))]
pub fn spawn<T>(future: T) -> tokio::task::JoinHandle<T::Output>
where
    T: std::future::Future + Send + 'static,
    T::Output: Send + 'static,
{
    tokio::spawn(future)
}
