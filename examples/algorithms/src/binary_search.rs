program!("binary_search" => [i64; 8]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const SORTED: [i64; 16] = [1, 3, 3, 7, 9, 12, 12, 12, 20, 25, 31, 40, 40, 55, 70, 99];
    const TARGETS: [i64; 8] = [-5, 1, 7, 8, 40, 2, 100, 3];

    #[test]
    fn lower_bounds_match_partition_point() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        for (k, &t) in TARGETS.iter().enumerate() {
            assert_eq!(
                out[0][k],
                SORTED.partition_point(|&x| x < t) as i64,
                "target {t}"
            );
        }
    }

    #[test]
    fn hits_and_misses_agree_with_std_binary_search() {
        let out = block_on(run());
        for (k, &t) in TARGETS.iter().enumerate() {
            let idx = out[0][k] as usize;
            let found = idx < SORTED.len() && SORTED[idx] == t;
            assert_eq!(found, SORTED.binary_search(&t).is_ok(), "target {t}");
        }
        assert_eq!(out[0], [0, 0, 3, 4, 11, 1, 16, 1]);
    }

    const ROUNDS: usize = 5;

    fn lower_bound_with_rounds(t: i64, rounds: usize) -> usize {
        let (mut lo, mut hi) = (0usize, SORTED.len());
        for _ in 0..rounds {
            if lo < hi {
                let mid = (lo + hi) / 2;
                if SORTED[mid] < t {
                    lo = mid + 1;
                } else {
                    hi = mid;
                }
            }
        }
        lo
    }

    #[test]
    fn round_cap_in_program_matches_tested_cap() {
        let source = include_str!("../programs/binary_search.marigold");
        assert!(source.contains(&format!("range(0, {ROUNDS})")));
    }

    #[test]
    fn round_cap_is_sufficient_and_minimal() {
        for t in -5..=105 {
            assert_eq!(
                lower_bound_with_rounds(t, ROUNDS),
                SORTED.partition_point(|&x| x < t),
                "target {t}"
            );
        }
        assert!((-5..=105)
            .any(|t| lower_bound_with_rounds(t, ROUNDS - 1) != SORTED.partition_point(|&x| x < t)));
        assert!(
            TARGETS
                .iter()
                .any(|&t| lower_bound_with_rounds(t, ROUNDS - 1)
                    != SORTED.partition_point(|&x| x < t))
        );
    }
}
