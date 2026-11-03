program!("binary_search" => [i64; 8]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const SORTED: [i64; 16] = [1, 3, 3, 7, 9, 12, 12, 12, 20, 25, 31, 40, 40, 55, 70, 99];
    const TARGETS: [i64; 8] = [0, 1, 3, 7, 8, 12, 99, 100];
    const ROUNDS: usize = 5;
    const LOWER_BOUNDS: [i64; 8] = [0, 0, 1, 3, 4, 5, 15, 16];
    const HITS: [bool; 8] = [false, true, true, true, false, true, true, false];

    fn program_array(name: &str) -> Vec<i64> {
        let source = include_str!("../programs/binary_search.marigold");
        let body = &source[source.find(&format!("fn {name}(")).unwrap()..];
        let open = body.find("{\n").unwrap();
        let literal = &body[open..];
        let start = literal.find('[').unwrap() + 1;
        let end = literal.find(']').unwrap();
        literal[start..end]
            .split(',')
            .map(|t| t.trim().parse().unwrap())
            .collect()
    }

    fn program_rounds() -> usize {
        let source = include_str!("../programs/binary_search.marigold");
        let line = source.trim_end().lines().last().unwrap();
        let start = line.find("range(0, ").unwrap() + "range(0, ".len();
        let end = start + line[start..].find(')').unwrap();
        line[start..end].parse().unwrap()
    }

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
    fn program_data_equals_the_oracle_data() {
        assert_eq!(program_array("sorted"), SORTED.to_vec());
        assert_eq!(program_array("targets"), TARGETS.to_vec());
        assert_eq!(program_rounds(), ROUNDS);
    }

    #[test]
    fn lower_bounds_match_partition_point_and_the_pinned_indices() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        for (k, &t) in TARGETS.iter().enumerate() {
            assert_eq!(
                out[0][k],
                SORTED.partition_point(|&x| x < t) as i64,
                "target {t}"
            );
        }
        assert_eq!(out[0], LOWER_BOUNDS);
    }

    #[test]
    fn hits_and_misses_agree_with_std_binary_search() {
        let out = block_on(run());
        for (k, &t) in TARGETS.iter().enumerate() {
            let idx = out[0][k] as usize;
            let found = idx < SORTED.len() && SORTED[idx] == t;
            assert_eq!(found, SORTED.binary_search(&t).is_ok(), "target {t}");
            assert_eq!(found, HITS[k], "target {t}");
        }
    }

    #[test]
    fn targets_cover_each_boundary_case() {
        assert!(TARGETS.iter().any(|&t| t < SORTED[0]));
        assert!(TARGETS.contains(&SORTED[0]));
        assert!(TARGETS.contains(&SORTED[15]));
        assert!(TARGETS.iter().any(|&t| t > SORTED[15]));
        assert!(TARGETS
            .iter()
            .any(|&t| SORTED.iter().filter(|&&x| x == t).count() > 1));
        assert!(TARGETS
            .iter()
            .any(|&t| !SORTED.contains(&t) && t > SORTED[0] && t < SORTED[15]));
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
        let short =
            |t: i64| lower_bound_with_rounds(t, ROUNDS - 1) != SORTED.partition_point(|&x| x < t);
        assert!((-5..=105).any(short));
        assert!(TARGETS.iter().any(|&t| short(t)));
    }
}
