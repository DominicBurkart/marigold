program!("tsp_christofides" => [i64; 11]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp::{dist, is_valid_tour, optimum, tour_length, N};

    fn kruskal_weight() -> i64 {
        let mut edges: Vec<(i64, usize, usize)> = (0..N)
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

    #[test]
    fn mst_weight_matches_kruskal() {
        assert_eq!(block_on(run())[0][N], kruskal_weight());
    }

    #[test]
    fn tour_is_valid_and_within_three_halves_of_optimum() {
        let out = block_on(run());
        assert_eq!(out.len(), 1);
        let tour: Vec<usize> = out[0][..N].iter().map(|&c| c as usize).collect();
        assert!(is_valid_tour(&tour));
        assert!(tour_length(&tour) >= optimum());
        assert!(2 * tour_length(&tour) <= 3 * optimum());
    }
}
