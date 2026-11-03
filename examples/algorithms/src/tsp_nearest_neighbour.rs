program!("tsp_nearest_neighbour" => [i64; 11]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{self, N};

    fn dist(a: usize, b: usize) -> i64 {
        i64::from(tsp::dist(a, b))
    }

    fn optimum() -> i64 {
        i64::from(tsp::optimum())
    }

    fn tour_length(t: &[usize]) -> i64 {
        i64::from(tsp::tour_length(t))
    }

    fn reference_nearest_neighbour() -> Vec<usize> {
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
    fn matches_reference_nearest_neighbour() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        let tour: Vec<usize> = out[0][..N].iter().map(|&c| c as usize).collect();
        assert_eq!(tour, reference_nearest_neighbour());
        let mut sorted = tour.clone();
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
        assert_eq!(out[0][N], tour_length(&tour));
    }

    #[test]
    fn respects_rosenkrantz_log_bound() {
        let out = block_on(run());
        let bound = (optimum() as f64) * (((N as f64).log2().ceil() + 1.0) / 2.0);
        assert!(out[0][N] >= optimum());
        assert!((out[0][N] as f64) <= bound);
    }
}
