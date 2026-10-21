program!("dijkstra" => [i64; 9]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const INF: i64 = 1_000_000_000;
    const EDGES: [(usize, usize, i64); 12] = [
        (0, 1, 4),
        (0, 2, 1),
        (2, 1, 2),
        (1, 3, 5),
        (2, 3, 8),
        (3, 4, 3),
        (2, 5, 10),
        (4, 5, 2),
        (5, 6, 1),
        (4, 6, 7),
        (6, 7, 4),
        (3, 7, 20),
    ];

    fn bellman_ford(n: usize, source: usize) -> Vec<i64> {
        let mut dist = vec![INF; n];
        dist[source] = 0;
        for _ in 0..n {
            for &(u, v, w) in &EDGES {
                if dist[u] < INF && dist[u] + w < dist[v] {
                    dist[v] = dist[u] + w;
                }
                if dist[v] < INF && dist[v] + w < dist[u] {
                    dist[u] = dist[v] + w;
                }
            }
        }
        dist
    }

    #[test]
    fn distances_match_bellman_ford() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_vec(), bellman_ford(9, 0));
    }

    #[test]
    fn known_distances_and_unreachable_vertex() {
        let result = block_on(run());
        assert_eq!(result[0], [0, 3, 1, 8, 11, 11, 12, 16, INF]);
    }

    #[test]
    fn detour_beats_direct_edge() {
        let result = block_on(run());
        assert!(result[0][7] < 20);
        assert!(result[0][1] < 4);
    }
}
