program!("floyd_warshall" => [i64; 64]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const V: usize = 8;
    const INF: i64 = 1_000_000;
    const EDGES: [(usize, usize, i64); 14] = [
        (0, 1, 4),
        (1, 2, 3),
        (2, 3, 5),
        (3, 0, 2),
        (0, 2, 9),
        (3, 4, 6),
        (4, 5, 2),
        (5, 6, 3),
        (6, 7, 4),
        (7, 4, 1),
        (5, 7, 8),
        (1, 3, 10),
        (6, 4, 9),
        (2, 0, 12),
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
