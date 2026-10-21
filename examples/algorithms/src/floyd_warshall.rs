program!("floyd_warshall" => [i64; 64]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const V: usize = 8;
    const INF: i64 = 1_000_000;
    const EDGES: [(usize, usize, i64); 14] = [
        (0, 1, 4),
        (0, 2, 9),
        (1, 2, 3),
        (1, 3, 8),
        (2, 4, 2),
        (3, 5, 5),
        (4, 3, 1),
        (4, 6, 7),
        (5, 7, 3),
        (6, 5, 2),
        (6, 7, 9),
        (7, 0, 6),
        (3, 6, 11),
        (2, 1, 1),
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
    fn graph_has_nontrivial_shortest_paths() {
        let from_zero = bellman_ford(0);
        assert!(from_zero.iter().all(|&d| d < INF));
        assert!(from_zero[7] > EDGES.iter().map(|e| e.2).max().unwrap());
    }
}
