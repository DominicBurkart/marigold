#![forbid(unsafe_code)]

pub use futures;
mod async_runtime;
pub mod collect_and_apply;
pub mod combinations;
pub mod keep_first_n;
pub mod marifold;
pub mod multi_consumer_stream;
pub mod permutations;
pub mod run_stream;

pub use collect_and_apply::CollectAndAppliable;
pub use combinations::Combinable;
pub use keep_first_n::KeepFirstN;
pub use marifold::Marifold;
pub use permutations::Permutable;

pub use arrayvec;
pub use futures::StreamExt;
pub use gen_nested_iter_yield;
pub use genawaiter;
pub use once_cell;

#[cfg(any(feature = "tokio", feature = "async-std"))]
pub use num_cpus;

#[cfg(feature = "io")]
pub use async_compression;
#[cfg(feature = "io")]
pub use csv_async;
#[cfg(feature = "io")]
pub use flate2;
#[cfg(feature = "io")]
pub use serde;
#[cfg(feature = "io")]
pub use tokio;
#[cfg(feature = "io")]
pub use tokio::io::AsyncWriteExt;
#[cfg(feature = "io")]
pub use tokio_util;
#[cfg(feature = "io")]
pub use tokio_util::compat::TokioAsyncReadCompatExt;
#[cfg(feature = "io")]
pub use tokio_util::compat::TokioAsyncWriteCompatExt;
#[cfg(feature = "io")]
pub mod writer;
