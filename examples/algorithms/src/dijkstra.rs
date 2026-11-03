program!("dijkstra" => [i64; 9]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const V: usize = 9;
    const INF: i64 = 1_000_000_000;
    const EDGES: [(usize, usize, i64); 13] = [
        (0, 1, 4),
        (0, 2, 1),
        (2, 1, 2),
        (1, 3, 5),
        (2, 3, 7),
        (3, 4, 3),
        (2, 5, 20),
        (5, 4, 2),
        (5, 6, 1),
        (4, 6, 7),
        (4, 7, 4),
        (3, 7, 20),
        (2, 6, 7),
    ];

    fn bellman_ford(source: usize) -> Vec<i64> {
        let mut dist = vec![INF; V];
        dist[source] = 0;
        for _ in 0..V {
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

    fn program_edges() -> Vec<i64> {
        let source = include_str!("../programs/dijkstra.marigold");
        let start = source.find("const EDGES").unwrap();
        let body = &source[start..start + source[start..].find("];\n    let").unwrap()];
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
    fn distances_match_bellman_ford() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_vec(), bellman_ford(0));
    }

    #[test]
    fn pinned_distances_bind_every_component() {
        assert_eq!(block_on(run()), vec![[0, 3, 1, 8, 11, 9, 8, 15, INF]]);
    }

    #[test]
    fn isolated_vertex_keeps_the_sentinel() {
        let result = block_on(run());
        assert_eq!(result[0][8], INF);
        assert!(EDGES.iter().all(|&(u, v, _)| u != 8 && v != 8));
    }

    #[test]
    fn indirect_paths_beat_direct_edges() {
        let d = block_on(run())[0];
        for &(u, v, w) in &[(0, 1, 4), (2, 5, 20), (4, 6, 7), (3, 7, 20)] {
            assert!(EDGES.contains(&(u, v, w)));
            assert!((d[u] - d[v]).abs() < w);
        }
    }

    #[test]
    fn tied_shortest_paths_and_tied_frontier_vertices() {
        let d = block_on(run())[0];
        assert_eq!(d[1] + 5, d[2] + 7);
        assert_eq!(d[3], 8);
        assert_eq!(d[3], d[6]);
        assert_eq!(d[3] + 3, d[5] + 2);
    }

    #[test]
    fn reverse_direction_relaxation_is_needed() {
        let d = block_on(run())[0];
        assert!(EDGES.contains(&(5, 6, 1)));
        assert_eq!(d[5], d[6] + 1);
        assert!(d[5] < d[2] + 20);
    }
}
