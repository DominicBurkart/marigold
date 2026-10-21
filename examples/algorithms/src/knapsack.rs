program!("knapsack" => i64);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const WEIGHTS: [usize; 14] = [12, 7, 11, 8, 9, 6, 14, 5, 10, 13, 4, 15, 3, 2];
    const VALUES: [i64; 14] = [24, 13, 23, 15, 16, 11, 28, 8, 19, 26, 7, 30, 5, 40];
    const CAPACITY: usize = 50;

    fn brute_force(weights: &[usize], values: &[i64], capacity: usize) -> i64 {
        (0u32..1 << weights.len())
            .filter_map(|mask| {
                let (mut w, mut v) = (0, 0);
                for i in 0..weights.len() {
                    if mask >> i & 1 == 1 {
                        w += weights[i];
                        v += values[i];
                    }
                }
                (w <= capacity).then_some(v)
            })
            .max()
            .unwrap()
    }

    #[test]
    fn knapsack_matches_subset_enumeration() {
        let expected = brute_force(&WEIGHTS, &VALUES, CAPACITY);
        assert_eq!(block_on(run()), vec![expected]);
    }

    #[test]
    fn last_item_matters() {
        let full = brute_force(&WEIGHTS, &VALUES, CAPACITY);
        assert!(brute_force(&WEIGHTS[..13], &VALUES[..13], CAPACITY) < full);
    }

    #[test]
    fn oracle_is_nontrivial() {
        let best = brute_force(&WEIGHTS, &VALUES, CAPACITY);
        assert!(best > 0);
        assert!(best < VALUES.iter().sum::<i64>());
    }
}
