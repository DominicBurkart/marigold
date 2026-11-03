program!("tsp_genetic" => [i64; 10]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{optimum, tour_length, N};

    #[test]
    fn returns_valid_tour_that_beats_identity_and_reaches_the_optimum_for_this_seed() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        let tour: Vec<usize> = out[0].iter().map(|&c| c as usize).collect();
        let mut sorted = tour.clone();
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
        let identity: Vec<usize> = (0..N).collect();
        let len = tour_length(&tour);
        assert!(len >= optimum());
        assert_eq!(len, optimum());
        assert!(len < tour_length(&identity));
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
