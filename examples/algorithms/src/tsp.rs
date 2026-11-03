pub const N: usize = 10;

pub const CITIES: [(i32, i32); N] = [
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

pub fn dist(a: usize, b: usize) -> i32 {
    (CITIES[a].0 - CITIES[b].0).abs() + (CITIES[a].1 - CITIES[b].1).abs()
}

pub fn tour_length(tour: &[usize]) -> i32 {
    (0..tour.len())
        .map(|i| dist(tour[i], tour[(i + 1) % tour.len()]))
        .sum()
}

pub fn optimum() -> i32 {
    fn go(tour: &mut Vec<usize>, used: &mut [bool; N], best: &mut i32) {
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
    let mut best = i32::MAX;
    let mut used = [false; N];
    used[0] = true;
    go(&mut vec![0], &mut used, &mut best);
    best
}

#[cfg(test)]
mod tests {
    use super::*;

    fn literal(program: &str, name: &str) -> Vec<i32> {
        let prefix = format!("let {name}: [i32; 10] = [");
        let line = program
            .lines()
            .find(|l| l.trim_start().starts_with(&prefix))
            .unwrap_or_else(|| panic!("no `{prefix}` line"));
        let inner = line.split_once('[').unwrap().1;
        let inner = inner.split_once('[').unwrap().1;
        inner
            .split_once(']')
            .unwrap()
            .0
            .split(',')
            .map(|v| v.trim().parse().unwrap())
            .collect()
    }

    #[test]
    fn optimum_is_pinned() {
        assert_eq!(optimum(), 84);
    }

    #[test]
    fn programs_embed_the_shared_cities() {
        let programs = [
            include_str!("../programs/tsp_brute_force.marigold"),
            include_str!("../programs/tsp_held_karp.marigold"),
        ];
        let xs: Vec<i32> = CITIES.iter().map(|c| c.0).collect();
        let ys: Vec<i32> = CITIES.iter().map(|c| c.1).collect();
        for program in programs {
            assert_eq!(literal(program, "xs"), xs);
            assert_eq!(literal(program, "ys"), ys);
        }
    }

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
}
