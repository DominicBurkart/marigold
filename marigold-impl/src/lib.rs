#![forbid(unsafe_code)]

pub use futures;
mod async_runtime;
pub mod collect_and_apply;
pub mod combinations;
pub mod keep_first_n;
pub mod permutations;

pub use collect_and_apply::CollectAndAppliable;
pub use combinations::Combinable;
pub use keep_first_n::KeepFirstN;
pub use permutations::Permutable;

pub use futures::StreamExt;
pub use gen_nested_iter_yield;
pub use genawaiter;
pub use pin_utils;

#[cfg(feature = "io")]
pub use async_std;
#[cfg(feature = "io")]
pub use csv_async;
