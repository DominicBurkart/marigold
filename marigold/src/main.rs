use anyhow::Result;

fn main() -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use marigold_macros::marigold;

    #[tokio::test]
    async fn chained_permutations_and_combinations() {
        let r = marigold!(
            range(0, 2)
                .permutations(2)
                .combinations(2)
                .return
        )
        .await
        .collect::<Vec<_>>();
        assert_eq!(r, vec![vec![vec![0, 1], vec![1, 0]]]);
    }

    #[tokio::test]
    async fn sort_by_external_function() {
        let sorter = |a: &Vec<Vec<i32>>, b: &Vec<Vec<i32>>| a[0].partial_cmp(&b[0]).unwrap();
        let r = marigold!(
            range(0, 5)
                .permutations(3)
                .combinations(2)
                .keep_first_n(1, sorter)
                .return
        )
        .await
        .collect::<Vec<_>>();
        assert_eq!(r, vec![vec![vec![0, 1, 2], vec![0, 1, 3]]]);
    }
}
