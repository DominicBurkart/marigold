program!("tsp_ant_colony" => [i64; 10]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{optimum, tour_length, N};

    const PROGRAM: &str = include_str!("../programs/tsp_ant_colony.marigold");
    const SEED_LINE: &str = "s[111] = 24680;";
    const ITERATIONS_STAGE: &str = "range(0, 200)";
    const PINNED_TOUR: [i64; N] = [3, 8, 4, 6, 1, 0, 2, 7, 9, 5];
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
        assert!(PROGRAM.contains("while a < 8"));
        assert!(PROGRAM.contains("s[e] * 90 / 100"));
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
