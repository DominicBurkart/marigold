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
| `dijkstra` | [#407](https://github.com/DominicBurkart/marigold/issues/407) | [Dijkstra 1959](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) | O(V^2) with an array scan (O(E log V) with a heap) | O(1), exact O(2): `range(0, 9).fold(start, settle).map(distances)`. The 9 vertices are a constant, so the whole stream collapses to O(1) (symbolic input size is [#415](https://github.com/DominicBurkart/marigold/issues/415)); the O(V) scan and O(E) relaxation inside `settle` are invisible to the analyzer (per-item fn cost is [#416](https://github.com/DominicBurkart/marigold/issues/416)). One fold step settles exactly one vertex (select the closest unsettled vertex, relax its edges). Vertex 8 is isolated and stays at the 1000000000 sentinel. Tests compare against Bellman-Ford. |
| `kruskal` | [#408](https://github.com/DominicBurkart/marigold/issues/408) | [Kruskal 1956](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm) | O(E log E) (sort) plus near-linear union-find | O(1), exact O(4), `collects_input`: `range(0, 8).combinations(2)` enumerates all 28 vertex pairs, `map(weigh)` attaches the weight (a 1000000000 sentinel for the 15 non-edges), `keep_first_n(28, by_weight)` sorts all 28 candidates ascending (the comparator reverses the order because `keep_first_n` returns the largest first, so sentinel pairs sort last), and `fold(start, unite)` runs union-find over the sorted stream with one fold step per candidate (sentinel candidates are skipped). Every size is a constant, so the analyzer collapses the stream to O(1) ([#415](https://github.com/DominicBurkart/marigold/issues/415)) and cannot see the sort cost or the union-find cost inside `unite` ([#416](https://github.com/DominicBurkart/marigold/issues/416)). Intermediate cardinalities are pinned: 28 after `combinations(2)`, 28 after `map`, 28 after `keep_first_n`. Simplification: `filter`/`filter_map` before `keep_first_n` would drop the non-edges (and the analyzer then reports O(C(n,2)) for that variant), but under the `tokio`/`async-std` features the generated `filter` code (`.buffered(..).filter_map(\|v\| v)`) does not compile and `filter_map` yields a stream that is not `Unpin`, which `keep_first_n` requires; the sentinel avoids both. Edge weights are distinct, so the MST is unique; tests compare edges and total weight against Prim. Union-find omits path compression and union by rank (8 vertices). |
