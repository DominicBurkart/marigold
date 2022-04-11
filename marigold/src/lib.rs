//! Marigold is a domain specific language for data pipelining and analysis. It compiles to
//! asynchronous Rust and can be integrated into Rust programs using a macro.
//!
//! ## Example Usage
//!
//! ```rust
//! ##[tokio::main]
//! # async fn main() {
//! use marigold::m;
//!
//! let is_odd = |i: &i32| i % 2 == 1;
//!
//! let odd_digits = m!(
//!  range(0, 10)
//!    .filter(is_odd)
//!    .to_vec()
//!    .return
//! ).await;
//!
//! assert_eq!(odd_digits, vec![1, 3, 5, 7, 9]);
//! # }
//! ```

#[doc(no_inline)]
pub use crate as marigold; // used so that the tests can reference re-exported values
pub use marigold_impl;

pub use marigold_macros::marigold as m;

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn chained_permutations_and_combinations() {
        let r = m!(
            range(0, 2)
                .permutations(2)
                .combinations(2)
                .to_vec()
                .return
        )
        .await;
        assert_eq!(r, vec![vec![vec![0, 1], vec![1, 0]]]);
    }

    #[tokio::test]
    async fn sort_by_external_function() {
        let sorter = |a: &Vec<i32>, b: &Vec<i32>| a[0].partial_cmp(&b[0]).unwrap();
        let r = m!(
            range(0, 2)
                .permutations(2)
                .keep_first_n(1, sorter)
                .to_vec()
                .return
        )
        .await;
        assert_eq!(r, vec![vec![1, 0]]);
    }

    #[tokio::test]
    async fn test_filter() {
        let is_odd_number = |i: &i32| i % 2 == 1;

        assert_eq!(
            m!(
                range(0, 10)
                .filter(is_odd_number)
                .to_vec()
                .return
            )
            .await,
            vec![1, 3, 5, 7, 9]
        );
    }
}
