use futures::executor::block_on;
use futures::stream::StreamExt;
use marigold_impl::{Combinable, Permutable};
use proptest::prelude::*;
use std::collections::HashSet;

fn falling(n: usize, k: usize) -> usize {
    if k > n {
        0
    } else {
        (n - k + 1..=n).product()
    }
}

fn binomial(n: usize, k: usize) -> usize {
    if k > n {
        return 0;
    }
    (0..k).fold(1, |acc, i| acc * (n - i) / (i + 1))
}

proptest! {
    #[test]
    fn permutations_of_distinct_items_are_counted_and_distinct(n in 0usize..7, k in 0usize..9) {
        let result: Vec<Vec<usize>> =
            block_on(async { futures::stream::iter(0..n).permutations(k).await.collect().await });
        prop_assert_eq!(result.len(), falling(n, k));
        let distinct: HashSet<_> = result.iter().cloned().collect();
        prop_assert_eq!(distinct.len(), result.len());
        prop_assert!(result.iter().all(|p| p.len() == k));
    }

    #[test]
    fn combinations_of_distinct_items_are_counted_and_distinct(n in 0usize..9, k in 0usize..11) {
        let result: Vec<Vec<usize>> =
            block_on(async { futures::stream::iter(0..n).combinations(k).await.collect().await });
        prop_assert_eq!(result.len(), binomial(n, k));
        let distinct: HashSet<Vec<usize>> = result
            .iter()
            .map(|c| {
                let mut c = c.clone();
                c.sort_unstable();
                c
            })
            .collect();
        prop_assert_eq!(distinct.len(), result.len());
        prop_assert!(result.iter().all(|c| c.len() == k));
    }
}
