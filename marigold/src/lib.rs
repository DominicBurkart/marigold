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
}
