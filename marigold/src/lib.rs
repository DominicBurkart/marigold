//! Marigold is an imperative, domain-specific language for data pipelining
//! and analysis over async streams. It compiles to asynchronous Rust and can
//! be embedded in Rust programs via the [`m!`] macro.
//!
//! # Example
//!
//! ```rust
//! # #[tokio::main]
//! # async fn main() {
//! use marigold::m;
//! # use marigold::marigold_impl::StreamExt;
//!
//! let odd_digits = m!(
//!  fn is_odd(i: i32) -> bool {
//!    i.wrapping_rem(2) == 1
//!  }
//!
//!  range(0, 10)
//!    .filter(is_odd)
//!    .return
//! ).await.collect::<Vec<_>>().await;
//!
//! assert_eq!(odd_digits, vec![1, 3, 5, 7, 9]);
//! # }
//! ```
#![forbid(unsafe_code)]

// Re-exported so tests and macro output can reference this crate by name.
pub use crate as marigold;
pub use marigold_impl;

pub use marigold_macros::marigold as m;
