use marigold::m;

mod lib;
use lib::compare_contrast;

#[tokio::main]
async fn main() {
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(2)
            .keep_first_n(20, compare_contrast)
            .to_vec()
            .return
        )
        .await
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn limited_palette() {
        let mod_fifty = |i: &u8| i % 50 == 0; // decrease space to search
        assert_eq!(
            m!(
                range(0, 255)
                .filter(mod_fifty)
                .permutations_with_replacement(3)
                .combinations(2)
                .keep_first_n(2, compare_contrast)
                .to_vec()
                .return
            )
            .await,
            vec![
                vec![vec![0, 0, 0], vec![0, 0, 100]],
                vec![vec![0, 0, 0], vec![0, 0, 50]]
            ]
        );
    }
}
