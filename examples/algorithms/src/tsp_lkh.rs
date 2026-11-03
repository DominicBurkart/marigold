program!("tsp_lkh" => [i64; 11]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{optimum, tour_length, N};

    const PROGRAM: &str = include_str!("../programs/tsp_lkh.marigold");
    const SEED_LINE: &str = "let mut seed = 624u64;";
    const ROUNDS_STAGE: &str = "range(0, 30).fold(seeded_start, improve_once)";
    const PINNED_START: [usize; N] = [0, 3, 4, 1, 7, 6, 8, 2, 9, 5];
    const PINNED_START_LENGTH: i32 = 138;
    const PINNED_TOUR: [i64; N] = [0, 1, 6, 4, 8, 3, 5, 9, 7, 2];
    const PINNED_LENGTH: i32 = 84;
    const PINNED_IMPROVING_STEPS: usize = 5;

    fn seeded_start() -> Vec<usize> {
        let mut t: Vec<usize> = (0..N).collect();
        let mut seed = 624u64;
        for i in (1..N).rev() {
            seed = (seed * 1103515245 + 12345) % 2147483648;
            let j = 1 + ((seed >> 16) as usize) % i;
            t.swap(i, j);
        }
        t
    }

    fn reference_best_move(t: &[usize]) -> Vec<usize> {
        let mut best = t.to_vec();
        let mut best_len = tour_length(t);
        let mut consider = |c: Vec<usize>| {
            let l = tour_length(&c);
            if l < best_len {
                best_len = l;
                best = c;
            }
        };
        for i in 1..N - 1 {
            for j in i + 1..N {
                let mut c = t.to_vec();
                c[i..=j].reverse();
                consider(c);
            }
        }
        for seg in 1..=3 {
            for start in 1..=N - seg {
                for gap in 1..=N - seg {
                    for rev in [false, true] {
                        let mut rest = t.to_vec();
                        let mut moved: Vec<usize> = rest.drain(start..start + seg).collect();
                        if rev {
                            moved.reverse();
                        }
                        for (k, city) in moved.into_iter().enumerate() {
                            rest.insert(gap + k, city);
                        }
                        consider(rest);
                    }
                }
            }
        }
        best
    }

    fn solve() -> (Vec<usize>, i64) {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        (out[0][..N].iter().map(|&c| c as usize).collect(), out[0][N])
    }

    #[test]
    fn returns_every_city_exactly_once() {
        let (tour, _) = solve();
        let mut sorted = tour.clone();
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
    }

    #[test]
    fn seeded_start_matches_independent_python_reference() {
        assert_eq!(seeded_start(), PINNED_START);
        assert_eq!(tour_length(&seeded_start()), PINNED_START_LENGTH);
    }

    #[test]
    fn matches_independent_python_reference() {
        let (tour, reported) = solve();
        let expected: Vec<usize> = PINNED_TOUR.iter().map(|&c| c as usize).collect();
        assert_eq!(tour, expected);
        assert_eq!(tour_length(&tour), PINNED_LENGTH);
        assert_eq!(reported, i64::from(PINNED_LENGTH));
    }

    #[test]
    fn length_is_at_least_the_optimum_and_below_the_seeded_start() {
        let (tour, _) = solve();
        let len = tour_length(&tour);
        assert!(len >= optimum());
        assert!(len < tour_length(&seeded_start()));
    }

    #[test]
    fn matches_reference_best_improvement_search_and_converges_before_the_round_cap() {
        let mut t = seeded_start();
        let mut improving = 0;
        for _ in 0..30 {
            let next = reference_best_move(&t);
            if next != t {
                improving += 1;
            }
            t = next;
        }
        assert_eq!(improving, PINNED_IMPROVING_STEPS);
        assert_eq!(solve().0, t);
        assert_eq!(reference_best_move(&t), t);
    }

    #[test]
    fn result_is_a_two_opt_and_or_opt_local_optimum() {
        let (tour, _) = solve();
        let len = tour_length(&tour);
        for i in 1..N - 1 {
            for j in i + 1..N {
                let mut c = tour.clone();
                c[i..=j].reverse();
                assert!(tour_length(&c) >= len);
            }
        }
        for seg in 1..=3 {
            for start in 1..=N - seg {
                let mut rest = tour.clone();
                let moved: Vec<usize> = rest.drain(start..start + seg).collect();
                for gap in 1..=rest.len() {
                    for rev in [false, true] {
                        let mut m = moved.clone();
                        if rev {
                            m.reverse();
                        }
                        let mut c = rest.clone();
                        for (k, city) in m.into_iter().enumerate() {
                            c.insert(gap + k, city);
                        }
                        assert!(tour_length(&c) >= len);
                    }
                }
            }
        }
    }

    #[test]
    fn program_embeds_the_seed_round_cap_and_move_neighbourhood() {
        assert!(PROGRAM.contains(SEED_LINE));
        assert!(PROGRAM.contains(ROUNDS_STAGE));
        assert!(PROGRAM.contains("(seed * 1103515245 + 12345) % 2147483648"));
        assert!(PROGRAM.contains("let j = 1 + ((seed >> 16) as usize) % i;"));
        assert!(PROGRAM.contains("while len <= 3 {"));
        assert!(PROGRAM.contains("while i < 9 {"));
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
