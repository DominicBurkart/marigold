program!("tsp_simulated_annealing" => [i64; 10]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{optimum, tour_length, N};

    const PROGRAM: &str = include_str!("../programs/tsp_simulated_annealing.marigold");
    const SEED_LINE: &str = "s[11] = 12345;";
    const ITERATIONS_STAGE: &str = "range(0, 2000)";
    const PINNED_TOUR: [i64; N] = [0, 1, 8, 4, 6, 5, 3, 7, 9, 2];
    const PINNED_LENGTH: i32 = 84;

    fn solve() -> Vec<usize> {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        out[0].iter().map(|&c| c as usize).collect()
    }

    #[test]
    fn returns_every_city_exactly_once() {
        let mut sorted = solve();
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
    }

    #[test]
    fn matches_independent_python_reference_for_the_fixed_seed() {
        let tour = solve();
        let expected: Vec<usize> = PINNED_TOUR.iter().map(|&c| c as usize).collect();
        assert_eq!(tour, expected);
        assert_eq!(tour_length(&tour), PINNED_LENGTH);
    }

    #[test]
    fn length_is_at_least_the_optimum_and_below_the_identity_tour() {
        let len = tour_length(&solve());
        let identity: Vec<usize> = (0..N).collect();
        assert!(len >= optimum());
        assert!(len < tour_length(&identity));
    }

    #[test]
    fn program_embeds_the_seed_and_parameters_the_reference_uses() {
        assert!(PROGRAM.contains(SEED_LINE));
        assert!(PROGRAM.contains(ITERATIONS_STAGE));
        assert!(PROGRAM.contains("(r * 1103515245 + 12345) % 2147483648"));
        assert!(PROGRAM.contains("let mut temp = 480 * (2000 - round as i64) / 2000;"));
        assert!(PROGRAM.contains("let k = (delta * 256 + temp / 2) / temp;"));
        assert!(PROGRAM.contains("k <= 96 && draw < table[k as usize]"));
        assert!(PROGRAM.contains("let u = r >> 16;"));
    }

    #[test]
    fn exp_table_in_the_program_matches_f64_exp_within_half_a_unit() {
        let opening = "let table: [i64; 97] = [";
        let start = PROGRAM.find(opening).expect("table declaration") + opening.len();
        let body = &PROGRAM[start..];
        let table: Vec<i64> = body[..body.find("];").expect("table end")]
            .split(',')
            .map(str::trim)
            .filter(|v| !v.is_empty())
            .map(|v| v.parse().unwrap())
            .collect();
        assert_eq!(table.len(), 97);
        for (k, &entry) in table.iter().enumerate() {
            let exact = 32768.0 * (-(k as f64) / 16.0).exp();
            assert!(
                (entry as f64 - exact).abs() <= 0.5,
                "entry {k} is {entry}, exp gives {exact}"
            );
        }
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
