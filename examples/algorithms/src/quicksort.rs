program!("quicksort" => [[i64; 16]; 6]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const STEPS: usize = 15;
    const LENGTHS: [usize; 6] = [16, 16, 16, 16, 1, 2];
    const NAMES: [&str; 6] = [
        "input_sorted",
        "input_reversed",
        "input_duplicates",
        "input_equal",
        "input_single",
        "input_pair",
    ];
    const INPUTS: [[i64; 16]; 6] = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
        [15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
        [33, 16, 36, 1, 15, 4, 12, 37, 8, 6, 1, 31, 0, 0, 22, 22],
        [5; 16],
        [7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [9, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ];
    const PARTITIONS: [usize; 6] = [15, 15, 14, 15, 0, 1];
    const SHORT_CAP_FAILS: [bool; 6] = [false, true, true, false, false, true];

    fn program_array(name: &str) -> Vec<i64> {
        let source = include_str!("../programs/quicksort.marigold");
        let body = &source[source.find(&format!("fn {name}(")).unwrap()..];
        let literal = &body[body.find("{\n").unwrap()..];
        let start = literal.find('[').unwrap() + 1;
        let end = literal.find(']').unwrap();
        literal[start..end]
            .split(',')
            .map(|t| t.trim().parse().unwrap())
            .collect()
    }

    fn program_steps() -> usize {
        let source = include_str!("../programs/quicksort.marigold");
        let line = source.trim_end().lines().last().unwrap();
        let start = line.find("range(0, ").unwrap() + "range(0, ".len();
        let end = start + line[start..].find(')').unwrap();
        line[start..end].parse().unwrap()
    }

    fn sort_with_cap(input: [i64; 16], len: usize, cap: usize) -> ([i64; 16], usize) {
        let mut data = input;
        let mut stack = if len >= 2 { vec![(0, len - 1)] } else { vec![] };
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

    fn expected(input: [i64; 16], len: usize) -> [i64; 16] {
        let mut out = input;
        out[..len].sort();
        out
    }

    #[test]
    fn program_data_equals_the_oracle_data() {
        for (name, input) in NAMES.iter().zip(INPUTS) {
            assert_eq!(program_array(name), input.to_vec(), "{name}");
        }
        let lengths: Vec<i64> = LENGTHS.iter().map(|&n| n as i64).collect();
        assert_eq!(program_array("lengths"), lengths);
        assert_eq!(program_steps(), STEPS);
    }

    #[test]
    fn every_instance_matches_std_sort_and_keeps_padding() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        for k in 0..6 {
            assert_eq!(out[0][k], expected(INPUTS[k], LENGTHS[k]), "{}", NAMES[k]);
        }
    }

    #[test]
    fn pinned_outputs_for_the_unsorted_instances() {
        let out = block_on(run());
        assert_eq!(out[0][1], std::array::from_fn(|i| i as i64));
        assert_eq!(
            out[0][2],
            [0, 0, 1, 1, 4, 6, 8, 12, 15, 16, 22, 22, 31, 33, 36, 37]
        );
        assert_eq!(out[0][5][..2], [4, 9]);
        assert_eq!(out[0][4][0], 7);
    }

    #[test]
    fn partition_counts_fit_the_step_cap_and_the_worst_case_is_tight() {
        for k in 0..6 {
            let (out, partitions) = sort_with_cap(INPUTS[k], LENGTHS[k], 1000);
            assert_eq!(out, expected(INPUTS[k], LENGTHS[k]), "{}", NAMES[k]);
            assert_eq!(partitions, PARTITIONS[k], "{}", NAMES[k]);
            assert!(partitions <= STEPS);
        }
        assert_eq!(PARTITIONS.iter().max(), Some(&STEPS));
    }

    #[test]
    fn one_step_short_fails_where_data_moves() {
        for k in 0..6 {
            let short = PARTITIONS[k].saturating_sub(1);
            let (out, _) = sort_with_cap(INPUTS[k], LENGTHS[k], short);
            assert_eq!(
                out != expected(INPUTS[k], LENGTHS[k]),
                SHORT_CAP_FAILS[k],
                "{}",
                NAMES[k]
            );
        }
        assert!(SHORT_CAP_FAILS[1]);
    }
}
