use std::future::Future;

macro_rules! program {
    ($name:literal => $item:ty) => {
        #[allow(unused_mut, clippy::let_and_return)]
        pub async fn run() -> Vec<$item> {
            use marigold::marigold_impl::StreamExt;
            include!(concat!(env!("OUT_DIR"), "/", $name, ".rs"))
                .await
                .collect::<Vec<_>>()
                .await
        }
    };
}

#[cfg(all(feature = "async-std", not(feature = "tokio")))]
pub fn block_on<F: Future>(future: F) -> F::Output {
    async_std::task::block_on(future)
}

#[cfg(feature = "tokio")]
pub fn block_on<F: Future>(future: F) -> F::Output {
    tokio::runtime::Runtime::new()
        .expect("could not build tokio runtime")
        .block_on(future)
}

#[cfg(not(any(feature = "tokio", feature = "async-std")))]
pub fn block_on<F: Future>(future: F) -> F::Output {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("could not build tokio runtime")
        .block_on(future)
}

pub mod binary_search;
pub mod gcd;
pub mod quicksort;
