program!("quicksort" => [i64; 16]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const INPUT: [i64; 16] = [33, 16, 36, 1, 15, 4, 12, 37, 8, 6, 1, 31, 0, 0, 22, 22];
    const STEPS: usize = 16;

    fn sort_with_cap(input: [i64; 16], cap: usize) -> ([i64; 16], usize) {
        let mut data = input;
        let mut stack = vec![(0usize, 15usize)];
        let mut partitions = 0;
        for _ in 0..cap {
            let Some((lo, hi)) = stack.pop() else { break };
            partitions += 1;
            let pivot = data[hi];
            let mut store = lo;
            for j in lo..hi {
                if data[j] < pivot {
                    data.swap(store, j);
                    store += 1;
                }
            }
            data.swap(store, hi);
            if store >= lo + 2 {
                stack.push((lo, store - 1));
            }
            if hi >= store + 2 {
                stack.push((store + 1, hi));
            }
        }
        (data, partitions)
    }

    fn sorted(input: [i64; 16]) -> [i64; 16] {
        let mut expected = input;
        expected.sort();
        expected
    }

    #[test]
    fn matches_std_sort() {
        assert_eq!(block_on(run()), vec![sorted(INPUT)]);
    }

    #[test]
    fn output_keeps_duplicates() {
        let out = block_on(run());
        assert!(out[0].windows(2).all(|w| w[0] <= w[1]));
        assert_eq!(out[0].iter().filter(|&&x| x == 1).count(), 2);
        assert_eq!(out[0].iter().filter(|&&x| x == 22).count(), 2);
    }

    #[test]
    fn step_cap_in_program_matches_tested_cap() {
        let source = include_str!("../programs/quicksort.marigold");
        assert!(source.contains(&format!("range(0, {STEPS})")));
    }

    #[test]
    fn sorted_distinct_input_is_the_worst_case_and_fits_the_cap() {
        let worst: [i64; 16] = std::array::from_fn(|i| i as i64);
        let (out, partitions) = sort_with_cap(worst, STEPS);
        assert_eq!(out, worst);
        assert_eq!(partitions, 15);
        assert!(partitions <= STEPS);
        let mut reversed = worst;
        reversed.reverse();
        assert_eq!(sort_with_cap(reversed, STEPS).0, worst);
        assert!(sort_with_cap(reversed, 1000).1 <= STEPS);
        assert!(sort_with_cap(INPUT, 1000).1 <= STEPS);
    }

    #[test]
    fn one_step_short_of_the_needed_partitions_fails() {
        let (_, needed) = sort_with_cap(INPUT, 1000);
        assert!(needed > 1);
        assert_eq!(sort_with_cap(INPUT, needed).0, sorted(INPUT));
        assert_ne!(sort_with_cap(INPUT, needed - 1).0, sorted(INPUT));
    }
}
