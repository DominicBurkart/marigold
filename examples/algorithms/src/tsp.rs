pub const N: usize = 10;

pub const CITIES: [(i64, i64); N] = [
    (0, 0),
    (4, 7),
    (9, 2),
    (13, 8),
    (6, 12),
    (15, 15),
    (2, 14),
    (11, 5),
    (8, 9),
    (17, 3),
];

pub fn dist(a: usize, b: usize) -> i64 {
    (CITIES[a].0 - CITIES[b].0).abs() + (CITIES[a].1 - CITIES[b].1).abs()
}

pub fn tour_length(tour: &[usize]) -> i64 {
    (0..tour.len())
        .map(|i| dist(tour[i], tour[(i + 1) % tour.len()]))
        .sum()
}

pub fn is_valid_tour(tour: &[usize]) -> bool {
    let mut seen = [false; N];
    tour.len() == N
        && tour.iter().all(|&c| {
            let fresh = c < N && !seen[c];
            if c < N {
                seen[c] = true;
            }
            fresh
        })
}

pub fn optimum() -> i64 {
    fn go(tour: &mut Vec<usize>, used: &mut [bool; N], best: &mut i64) {
        if tour.len() == N {
            *best = (*best).min(tour_length(tour));
            return;
        }
        for c in 1..N {
            if !used[c] {
                used[c] = true;
                tour.push(c);
                go(tour, used, best);
                tour.pop();
                used[c] = false;
            }
        }
    }
    let mut best = i64::MAX;
    let mut used = [false; N];
    used[0] = true;
    go(&mut vec![0], &mut used, &mut best);
    best
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn distances_are_a_metric() {
        for a in 0..N {
            assert_eq!(dist(a, a), 0);
            for b in 0..N {
                assert_eq!(dist(a, b), dist(b, a));
                for c in 0..N {
                    assert!(dist(a, c) <= dist(a, b) + dist(b, c));
                }
            }
        }
    }

    #[test]
    fn identity_tour_is_valid_and_not_better_than_optimum() {
        let tour: Vec<usize> = (0..N).collect();
        assert!(is_valid_tour(&tour));
        assert!(tour_length(&tour) >= optimum());
    }

    #[test]
    fn rejects_duplicates_and_wrong_length() {
        assert!(!is_valid_tour(&[0, 0, 2, 3, 4, 5, 6, 7, 8, 9]));
        assert!(!is_valid_tour(&[0, 1, 2]));
        assert!(!is_valid_tour(&[0, 1, 2, 3, 4, 5, 6, 7, 8, 10]));
    }
}
