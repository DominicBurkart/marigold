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

#[cfg(feature = "tokio")]
pub fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Runtime::new().expect("could not build tokio runtime")
}

#[cfg(not(feature = "tokio"))]
pub fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("could not build tokio runtime")
}

#[cfg(all(feature = "async-std", not(feature = "tokio")))]
pub fn block_on<F: Future>(future: F) -> F::Output {
    async_std::task::block_on(future)
}

#[cfg(not(all(feature = "async-std", not(feature = "tokio"))))]
pub fn block_on<F: Future>(future: F) -> F::Output {
    runtime().block_on(future)
}

pub mod gcd;
#[cfg(test)]
mod tsp;
pub mod tsp_brute_force;
pub mod tsp_christofides;
pub mod tsp_held_karp;
pub mod tsp_lkh;
pub mod tsp_nearest_neighbour;
