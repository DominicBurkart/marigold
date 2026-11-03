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

    fn program_array(name: &str) -> Vec<i64> {
        let source = include_str!("../programs/knapsack.marigold");
        let start = source.find(&format!("let {name}:")).unwrap();
        let line = source[start..].lines().next().unwrap();
        line[line.find("= [").unwrap()..]
            .split(|c: char| !c.is_ascii_digit())
            .filter(|t| !t.is_empty())
            .map(|t| t.parse().unwrap())
            .collect()
    }

    #[test]
    fn program_items_equal_the_oracle_items() {
        let weights: Vec<i64> = WEIGHTS.iter().map(|&w| w as i64).collect();
        assert_eq!(program_array("weights"), weights);
        assert_eq!(program_array("values"), VALUES.to_vec());
    }

    #[test]
    fn knapsack_matches_subset_enumeration() {
        let expected = brute_force(&WEIGHTS, &VALUES, CAPACITY);
        assert_eq!(block_on(run()), vec![expected]);
    }

    #[test]
    fn pinned_optimum_binds_item_count_capacity_and_take_branch() {
        assert_eq!(brute_force(&WEIGHTS, &VALUES, CAPACITY), 136);
        assert_eq!(block_on(run()), vec![136]);
    }

    #[test]
    fn nearby_instances_have_other_optima() {
        assert_eq!(brute_force(&WEIGHTS[..13], &VALUES[..13], CAPACITY), 101);
        assert_eq!(brute_force(&WEIGHTS, &VALUES, CAPACITY - 1), 134);
    }
}
