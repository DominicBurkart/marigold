use anyhow::Result;

fn main() -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use marigold_macros::marigold;

    #[tokio::test]
    async fn it_works() {
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
}
