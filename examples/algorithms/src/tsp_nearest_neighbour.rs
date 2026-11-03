program!("tsp_nearest_neighbour" => [i64; 11]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{dist, optimum, tour_length, N};

    const PROGRAM: &str = include_str!("../programs/tsp_nearest_neighbour.marigold");
    const PINNED_TOUR: [i64; N] = [0, 1, 8, 4, 6, 5, 3, 7, 2, 9];
    const PINNED_LENGTH: i32 = 90;

    fn solve() -> ([i64; N], i64) {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        let mut tour = [0i64; N];
        tour.copy_from_slice(&out[0][..N]);
        (tour, out[0][N])
    }

    fn reference_nearest_neighbour_from_city_zero() -> Vec<usize> {
        let mut tour = vec![0usize];
        while tour.len() < N {
            let last = *tour.last().unwrap();
            let next = (0..N)
                .filter(|c| !tour.contains(c))
                .min_by_key(|&c| (dist(last, c), c))
                .unwrap();
            tour.push(next);
        }
        tour
    }

    #[test]
    fn starts_at_city_zero_and_visits_every_city_once() {
        let (tour, _) = solve();
        assert_eq!(tour[0], 0);
        let mut sorted: Vec<usize> = tour.iter().map(|&c| c as usize).collect();
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
    }

    #[test]
    fn matches_independent_python_reference() {
        let (tour, reported) = solve();
        assert_eq!(tour, PINNED_TOUR);
        let as_usize: Vec<usize> = tour.iter().map(|&c| c as usize).collect();
        assert_eq!(tour_length(&as_usize), PINNED_LENGTH);
        assert_eq!(reported, i64::from(PINNED_LENGTH));
    }

    #[test]
    fn matches_reference_nearest_neighbour_with_lowest_index_tie_break() {
        let (tour, _) = solve();
        let as_usize: Vec<usize> = tour.iter().map(|&c| c as usize).collect();
        assert_eq!(as_usize, reference_nearest_neighbour_from_city_zero());
    }

    #[test]
    fn the_first_step_is_a_tie_that_the_lowest_index_rule_resolves() {
        assert_eq!(dist(0, 1), dist(0, 2));
        assert!((3..N).all(|c| dist(0, c) > dist(0, 1)));
        let (tour, _) = solve();
        assert_eq!(tour[1], 1);
    }

    #[test]
    fn length_is_at_least_the_optimum() {
        let (_, reported) = solve();
        assert!(reported >= i64::from(optimum()));
    }

    #[test]
    fn program_embeds_the_start_city_step_count_and_tie_break() {
        assert!(PROGRAM.contains("[0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]"));
        assert!(PROGRAM.contains("range(0, 9).fold(start, extend_tour)"));
        assert!(PROGRAM.contains("if d < best_d {"));
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
