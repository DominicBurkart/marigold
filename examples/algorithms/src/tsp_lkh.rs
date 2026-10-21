program!("tsp_lkh" => [i64; 11]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{is_valid_tour, optimum, tour_length, N};

    fn seeded_start() -> Vec<usize> {
        let mut t: Vec<usize> = (0..N).collect();
        let mut seed = 12345u64;
        for i in (1..N).rev() {
            seed = (seed * 1103515245 + 12345) % 2147483648;
            let j = 1 + ((seed >> 16) as usize) % i;
            t.swap(i, j);
        }
        t
    }

    fn result() -> (Vec<usize>, i64) {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        (out[0][..N].iter().map(|&c| c as usize).collect(), out[0][N])
    }

    #[test]
    fn tour_is_valid_and_reported_length_is_correct() {
        let (tour, len) = result();
        assert!(is_valid_tour(&tour));
        assert_eq!(len, tour_length(&tour));
        assert!(len >= optimum());
    }

    #[test]
    fn improves_on_the_seeded_start() {
        let (_, len) = result();
        assert!(len < tour_length(&seeded_start()));
    }

    #[test]
    fn is_two_opt_and_or_opt_local_optimum() {
        let (tour, len) = result();
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
}
