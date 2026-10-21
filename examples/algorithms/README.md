# Algorithm examples

Each program in `programs/` is a standalone `.marigold` file. `build.rs` turns
it into Rust with the same parser the `m!` macro uses, so the file that
`marigold run` executes is the file that the tests, the complexity analysis and
the benches exercise.

Every example has four checks:

| Check | Where |
| --- | --- |
| program runs and returns the right answer | `src/<name>.rs` tests (`cargo test -p algorithms`) |
| complexity estimate pinned and compared with the canonical class | `marigold-grammar/tests/algorithm_examples_complexity.rs` |
| benchmark | `benches/algorithms_bench.rs`, run by `.github/workflows/bench.yaml` |
| program parses and analyzes | the two above |

## Writing a program

- `map`, `filter` and `fold` take function names, not closures. Functions are
  emitted as `const fn`, so bodies cannot allocate or use iterators; use fixed
  size arrays and `while` loops.
- There are no comments at top level of a `.marigold` file. References live in
  the table below.
- Fixed-size inputs are declared inside functions. The analyzer sees only the
  stream shape, so loops that live inside a function body are invisible to it.

## Programs

| Program | Issue | Reference | Canonical complexity | Analyzer estimate |
| --- | --- | --- | --- | --- |
| `gcd` | [#410](https://github.com/DominicBurkart/marigold/issues/410) | [Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm) | O(log(min(a, b))) | O(1), exact O(2): a fold over the constant `range(0, 48)` (48 bounds the Euclid steps for any `u32` pair by Lame's theorem) and a map. The logarithm lives in the step bound, not the stream shape. |
| `tsp_nearest_neighbour` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | Rosenkrantz, Stearns, Lewis 1977, [Nearest neighbour algorithm](https://en.wikipedia.org/wiki/Nearest_neighbour_algorithm) | O(n^2) time, tour within (ceil(log2 n)+1)/2 of optimal | O(1), exact O(2): `range(0, 9).fold(extend_tour).map(close_tour)`. Each fold step appends the nearest unvisited city (one O(n) scan hidden in the fn body, issue [#416](https://github.com/DominicBurkart/marigold/issues/416)); the city count is a literal, so the n^2 collapses to a constant (issue [#415](https://github.com/DominicBurkart/marigold/issues/415)). |
| `tsp_christofides` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | Christofides 1976, [Christofides algorithm](https://en.wikipedia.org/wiki/Christofides_algorithm) | O(n^3) time with a polynomial matching, tour within 1.5 of optimal | O(1), exact O(5): `range(0, 9).fold(prim_step)` then `.map(add_matching).map(euler_circuit).map(shortcut).map(finish)`. Prim is the fold (one vertex per step); each later stage is one map. Same collapse and hidden per-item cost as above (#415, #416). |
| `tsp_lkh` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | Helsgaun 2017, [An Extension of the Lin-Kernighan-Helsgaun TSP Solver for Constrained Traveling Salesman and Vehicle Routing Problems](http://webhotel4.ruc.dk/~keld/research/LKH-3/) | Heuristic local search, no polynomial worst-case bound | O(1), exact O(2): `range(0, 30).fold(improve_once).map(with_length)`. Each fold step applies the single best 2-opt or Or-opt move. Same collapse (#415, #416). |

## TSP constructive and local-search programs

All three share the N=10 Manhattan-distance instance in `src/tsp.rs`; the same
coordinates are repeated inside each `.marigold` file because programs are
standalone. Tests compare against independent Rust references (a reference
nearest-neighbour, Kruskal MST, brute-force `tsp::optimum()`).

- `tsp_nearest_neighbour`: exact implementation, ties broken by lowest city
  index. The test also checks the Rosenkrantz et al. bound against the optimum.
- `tsp_christofides`: the minimum-weight perfect matching of the odd-degree
  vertices is found by brute-force recursion over bitmasks. That is exponential
  in the number of odd vertices and is acceptable only because N=10 leaves at
  most 945 matchings; a real implementation would use Edmonds' blossom
  algorithm. The Eulerian circuit is Hierholzer's algorithm on the multigraph
  and the shortcut keeps first visits. The test asserts a valid tour with
  length at most 1.5 times optimal, and the MST weight (emitted as the last
  element of the output) equals Kruskal's.
- `tsp_lkh`: **this is NOT LKH-3.** It is a simplified local search standing in
  for the Lin-Kernighan family: from a tour made by a seeded LCG shuffle, each
  fold step evaluates every 2-opt reversal and every Or-opt move (segments of
  1 to 3 cities, either orientation) and applies the best improving one, for at
  most 30 steps. LKH-3's alpha-nearness candidate sets, 5-opt sequential moves,
  and constraint handling are not implemented, and neither is variable-depth
  Lin-Kernighan search. The test asserts a valid tour that beats the seeded
  start and is a local optimum under 2-opt and Or-opt, checked independently.
  It does not claim optimality.

Analyzer divergence for all three: the analyzer sees only the stream shape.
Every pipeline starts from a constant `range`, so the estimate is O(1) with
exact O(k) where k is the number of stream stages, and the cardinality is
exactly one. The canonical polynomial costs live in `while` loops inside the
fold and map bodies, which the analyzer cannot see (#416), and in the city
count, which is a literal rather than a symbolic input size (#415). The
analyzer does not expose the intermediate cardinalities (9 fold steps for the
first two programs, 30 for `tsp_lkh`), so only the final cardinality is pinned.
