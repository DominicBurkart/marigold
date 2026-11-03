program!("floyd_warshall" => [i64; 64]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const V: usize = 8;
    const INF: i64 = 1_000_000;
    const EDGES: [(usize, usize, i64); 9] = [
        (0, 1, 4),
        (1, 2, 3),
        (2, 3, 5),
        (3, 0, 2),
        (3, 4, 6),
        (4, 5, 2),
        (5, 6, 3),
        (6, 7, 4),
        (7, 4, 1),
    ];

    fn bellman_ford(source: usize) -> [i64; V] {
        let mut dist = [INF; V];
        dist[source] = 0;
        for _ in 0..V - 1 {
            for &(u, v, w) in &EDGES {
                if dist[u] + w < dist[v] {
                    dist[v] = dist[u] + w;
                }
            }
        }
        dist
    }

    fn program_edges() -> Vec<i64> {
        let source = include_str!("../programs/floyd_warshall.marigold");
        let start = source.find("let edges").unwrap();
        let body = &source[start..start + source[start..].find("\n    ];").unwrap() + 7];
        let after_type = &body[body.find("= [").unwrap()..];
        after_type
            .split(|c: char| !c.is_ascii_digit())
            .filter(|t| !t.is_empty())
            .map(|t| t.parse().unwrap())
            .collect()
    }

    #[test]
    fn program_edge_list_equals_the_oracle_edge_list() {
        let expected: Vec<i64> = EDGES
            .iter()
            .flat_map(|&(u, v, w)| [u as i64, v as i64, w])
            .collect();
        assert_eq!(program_edges(), expected);
    }

    #[test]
    fn floyd_warshall_matches_bellman_ford_from_every_source() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        for source in 0..V {
            assert_eq!(
                result[0][source * V..(source + 1) * V],
                bellman_ford(source)
            );
        }
    }

    #[test]
    fn pinned_matrix_binds_every_relaxation_and_the_infinity_sentinel() {
        const X: i64 = INF;
        #[rustfmt::skip]
        let expected: [i64; 64] = [
            0, 4, 7, 12, 18, 20, 23, 27,
            10, 0, 3, 8, 14, 16, 19, 23,
            7, 11, 0, 5, 11, 13, 16, 20,
            2, 6, 9, 0, 6, 8, 11, 15,
            X, X, X, X, 0, 2, 5, 9,
            X, X, X, X, 8, 0, 3, 7,
            X, X, X, X, 5, 7, 0, 4,
            X, X, X, X, 1, 3, 6, 0,
        ];
        assert_eq!(block_on(run()), vec![expected]);
    }
}
