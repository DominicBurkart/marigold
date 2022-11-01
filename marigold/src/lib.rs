//! Marigold is a domain specific language for data pipelining and analysis. It compiles to
//! asynchronous Rust and can be integrated into Rust programs using a macro.
//!
//! ## Example Usage
//!
//! ```rust
//! # #[tokio::main]
//! # async fn main() {
//! use marigold::m;
//! # use marigold::marigold_impl::StreamExt;
//!
//! let odd_digits = m!(
//!  fn is_odd(i: &i32) -> bool {
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

pub use crate as marigold; // used so that the tests can reference re-exported values
pub use marigold_impl;

pub use marigold_macros::marigold as m;
