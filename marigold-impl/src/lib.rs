#![forbid(unsafe_code)]

pub use futures;
pub use std::cmp::Ordering;
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

pub use futures::StreamExt;

// The items below are re-exported solely for use by marigold-macros codegen, which emits
// fully-qualified paths like `::marigold::marigold_impl::arrayvec::ArrayString`. They are
// NOT part of the public API. Use the upstream crates directly in your own code.
#[doc(hidden)]
pub use arrayvec;
#[doc(hidden)]
pub use gen_nested_iter_yield;
#[doc(hidden)]
pub use genawaiter;
#[doc(hidden)]
pub use once_cell;

#[cfg(any(feature = "tokio", feature = "async-std"))]
#[doc(hidden)]
pub use num_cpus;

#[cfg(feature = "io")]
#[doc(hidden)]
pub use async_compression;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use csv_async;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use flate2;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use serde;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use tokio;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use tokio::io::AsyncWriteExt;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use tokio_util;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use tokio_util::compat::TokioAsyncReadCompatExt;
#[cfg(feature = "io")]
#[doc(hidden)]
pub use tokio_util::compat::TokioAsyncWriteCompatExt;
#[cfg(feature = "io")]
pub mod writer;
