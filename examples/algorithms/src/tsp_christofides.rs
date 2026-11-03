program!("tsp_christofides" => [i64; 13]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{dist, optimum, tour_length, N};

    const PROGRAM: &str = include_str!("../programs/tsp_christofides.marigold");
    const PINNED_TOUR: [i64; N] = [0, 2, 7, 9, 3, 5, 8, 6, 4, 1];
    const PINNED_LENGTH: i32 = 90;
    const PINNED_MST_WEIGHT: i64 = 61;
    const PINNED_MATCHING_WEIGHT: i64 = 39;
    const PINNED_ODD_MASK: i64 = 0b11_1110_1101;
    const PINNED_MATCHING_COUNT: usize = 105;

    fn solve() -> [i64; 13] {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        out[0]
    }

    fn tour_of(out: &[i64; 13]) -> Vec<usize> {
        out[..N].iter().map(|&c| c as usize).collect()
    }

    fn kruskal_weight() -> i32 {
        let mut edges: Vec<(i32, usize, usize)> = (0..N)
            .flat_map(|a| ((a + 1)..N).map(move |b| (dist(a, b), a, b)))
            .collect();
        edges.sort();
        let mut parent: Vec<usize> = (0..N).collect();
        fn find(p: &mut Vec<usize>, x: usize) -> usize {
            if p[x] != x {
                let r = find(p, p[x]);
                p[x] = r;
            }
            p[x]
        }
        let mut total = 0;
        for (w, a, b) in edges {
            let (ra, rb) = (find(&mut parent, a), find(&mut parent, b));
            if ra != rb {
                parent[ra] = rb;
                total += w;
            }
        }
        total
    }

    fn all_perfect_matchings(odd: &[usize]) -> Vec<Vec<(usize, usize)>> {
        match odd.split_first() {
            None => vec![Vec::new()],
            Some((&a, rest)) => {
                let mut all = Vec::new();
                for k in 0..rest.len() {
                    let mut others = rest.to_vec();
                    let b = others.remove(k);
                    for mut m in all_perfect_matchings(&others) {
                        m.push((a, b));
                        all.push(m);
                    }
                }
                all
            }
        }
    }

    #[test]
    fn tour_visits_every_city_once() {
        let mut sorted = tour_of(&solve());
        sorted.sort();
        assert_eq!(sorted, (0..N).collect::<Vec<_>>());
    }

    #[test]
    fn matches_independent_python_reference() {
        let out = solve();
        assert_eq!(out[..N], PINNED_TOUR);
        assert_eq!(tour_length(&tour_of(&out)), PINNED_LENGTH);
        assert_eq!(out[10], PINNED_MST_WEIGHT);
        assert_eq!(out[11], PINNED_MATCHING_WEIGHT);
        assert_eq!(out[12], PINNED_ODD_MASK);
    }

    #[test]
    fn mst_weight_matches_kruskal() {
        assert_eq!(solve()[10], i64::from(kruskal_weight()));
    }

    #[test]
    fn matching_weight_is_the_minimum_over_every_perfect_matching_of_the_odd_vertices() {
        let out = solve();
        let odd: Vec<usize> = (0..N).filter(|&v| (out[12] >> v) & 1 == 1).collect();
        assert_eq!(odd.len() % 2, 0);
        let matchings = all_perfect_matchings(&odd);
        assert_eq!(matchings.len(), PINNED_MATCHING_COUNT);
        let weights: Vec<i32> = matchings
            .iter()
            .map(|m| m.iter().map(|&(a, b)| dist(a, b)).sum())
            .collect();
        assert_eq!(out[11], i64::from(*weights.iter().min().unwrap()));
        assert!(weights.iter().any(|&w| i64::from(w) > out[11]));
    }

    #[test]
    fn tour_is_within_three_halves_of_the_optimum_and_not_below_it() {
        let len = tour_length(&tour_of(&solve()));
        assert!(len >= optimum());
        assert!(2 * len <= 3 * optimum());
    }

    #[test]
    fn tour_is_no_longer_than_the_mst_plus_the_matching() {
        let out = solve();
        assert!(i64::from(tour_length(&tour_of(&out))) <= out[10] + out[11]);
    }

    #[test]
    fn program_embeds_the_prim_step_count_and_tie_breaks() {
        assert!(PROGRAM.contains(
            "range(0, 9).fold(mst_start, prim_step).map(add_matching).map(euler_circuit).map(shortcut).map(finish)"
        ));
        assert!(PROGRAM.contains("if d < best_d {"));
        assert!(PROGRAM.contains("dist(i, j) + match_cost(rest & !(1i64 << j)) == target"));
    }

    #[test]
    fn is_deterministic() {
        assert_eq!(block_on(run()), block_on(run()));
    }
}
