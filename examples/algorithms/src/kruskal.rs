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
        (2, 4, 8),
        (3, 4, 15),
        (3, 5, 11),
        (4, 5, 12),
        (4, 6, 13),
        (5, 6, 14),
        (6, 7, 3),
        (5, 7, 16),
    ];

    fn prim_weight() -> i64 {
        let mut in_tree = [false; V];
        in_tree[0] = true;
        let mut total = 0;
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
        }
        total
    }

    fn weight_of(edge: (usize, usize)) -> i64 {
        EDGES
            .iter()
            .find(|&&(u, v, _)| (u, v) == edge)
            .unwrap_or_else(|| panic!("{edge:?} is not an edge"))
            .2
    }

    fn is_spanning_tree(chosen: &[(usize, usize)]) -> bool {
        let mut parent: Vec<usize> = (0..V).collect();
        fn find(parent: &[usize], mut x: usize) -> usize {
            while parent[x] != x {
                x = parent[x];
            }
            x
        }
        chosen.len() == V - 1
            && chosen.iter().all(|&(u, v)| {
                let (a, b) = (find(&parent, u), find(&parent, v));
                parent[a] = b;
                a != b
            })
    }

    fn decode(state: &[i64; 24]) -> (i64, Vec<(usize, usize)>) {
        let count = state[9] as usize;
        let mut chosen: Vec<_> = (0..count)
            .map(|i| (state[10 + 2 * i] as usize, state[11 + 2 * i] as usize))
            .collect();
        chosen.sort();
        (state[8], chosen)
    }

    fn program_edges() -> Vec<i64> {
        let source = include_str!("../programs/kruskal.marigold");
        let start = source.find("const EDGES").unwrap();
        let body = &source[start..start + source[start..].find("];\n    let").unwrap()];
        let after_type = &body[body.find("= [").unwrap()..];
        after_type
            .split(|c: char| !c.is_ascii_digit())
            .filter(|t| !t.is_empty())
            .map(|t| t.parse().unwrap())
            .collect()
    }

    fn program_keep_count() -> usize {
        let source = include_str!("../programs/kruskal.marigold");
        let start = source.find("keep_first_n(").unwrap() + "keep_first_n(".len();
        source[start..start + source[start..].find(',').unwrap()]
            .parse()
            .unwrap()
    }

    #[test]
    fn program_edge_list_equals_the_oracle_edge_list() {
        let expected: Vec<i64> = EDGES
            .iter()
            .flat_map(|&(u, v, w)| [u as i64, v as i64, w])
            .collect();
        assert_eq!(program_edges(), expected);
        assert!(EDGES.iter().all(|&(u, v, _)| u < v));
        assert_eq!(program_keep_count(), EDGES.len());
    }

    #[test]
    fn result_is_a_minimum_spanning_tree_by_prim() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        let (total, chosen) = decode(&result[0]);
        assert!(is_spanning_tree(&chosen));
        assert_eq!(total, chosen.iter().map(|&e| weight_of(e)).sum::<i64>());
        assert_eq!(total, prim_weight());
    }

    #[test]
    fn pinned_edge_set_and_total_weight() {
        let result = block_on(run());
        assert_eq!(
            decode(&result[0]),
            (
                53,
                vec![(0, 1), (0, 3), (1, 2), (1, 4), (3, 5), (4, 6), (6, 7)]
            )
        );
    }

    #[test]
    fn equal_weight_tie_keeps_the_earlier_candidate() {
        let (_, chosen) = decode(&block_on(run())[0]);
        assert_eq!(weight_of((1, 2)), weight_of((2, 4)));
        assert!(chosen.contains(&(1, 2)));
        assert!(!chosen.contains(&(2, 4)));
    }

    #[test]
    fn cycle_closing_edges_are_rejected() {
        let (_, chosen) = decode(&block_on(run())[0]);
        for rejected in [(1, 3), (2, 4), (3, 4), (4, 5), (5, 6), (5, 7)] {
            assert!(EDGES.iter().any(|&(u, v, _)| (u, v) == rejected));
            assert!(!chosen.contains(&rejected));
        }
    }

    #[test]
    fn union_find_state_connects_every_vertex() {
        let state = block_on(run())[0];
        let root = |mut x: usize| {
            while state[x] as usize != x {
                x = state[x] as usize;
            }
            x
        };
        assert!((0..V).all(|v| root(v) == root(0)));
    }
}
