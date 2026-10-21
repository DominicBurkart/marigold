program!("kruskal" => [i64; 24]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const V: usize = 8;
    const EDGES: [(usize, usize, i64); 13] = [
        (0, 1, 7),
        (0, 3, 5),
        (1, 2, 8),
        (1, 3, 9),
        (1, 4, 6),
        (2, 4, 10),
        (3, 4, 15),
        (3, 5, 11),
        (4, 5, 12),
        (4, 6, 13),
        (5, 6, 14),
        (6, 7, 3),
        (5, 7, 16),
    ];

    fn prim() -> (i64, Vec<(usize, usize)>) {
        let mut in_tree = [false; V];
        in_tree[0] = true;
        let mut total = 0;
        let mut chosen = Vec::new();
        for _ in 1..V {
            let (u, v, w) = EDGES
                .iter()
                .copied()
                .filter(|&(u, v, _)| in_tree[u] != in_tree[v])
                .min_by_key(|&(_, _, w)| w)
                .unwrap();
            in_tree[u] = true;
            in_tree[v] = true;
            total += w;
            chosen.push((u, v));
        }
        chosen.sort();
        (total, chosen)
    }

    fn decode(state: &[i64; 24]) -> (i64, Vec<(usize, usize)>) {
        let count = state[9] as usize;
        let mut chosen: Vec<_> = (0..count)
            .map(|i| (state[10 + 2 * i] as usize, state[11 + 2 * i] as usize))
            .collect();
        chosen.sort();
        (state[8], chosen)
    }

    #[test]
    fn mst_matches_prim() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        assert_eq!(decode(&result[0]), prim());
    }

    #[test]
    fn spans_all_vertices_with_v_minus_one_edges() {
        let result = block_on(run());
        assert_eq!(result[0][9], (V - 1) as i64);
        let root = |mut x: usize| {
            while result[0][x] as usize != x {
                x = result[0][x] as usize;
            }
            x
        };
        assert!((0..V).all(|v| root(v) == root(0)));
    }

    #[test]
    fn known_total_weight() {
        let result = block_on(run());
        assert_eq!(result[0][8], 7 + 5 + 8 + 6 + 11 + 13 + 3);
    }
}
