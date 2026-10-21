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
| `knapsack` | [#409](https://github.com/DominicBurkart/marigold/issues/409) | [Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem) | O(n * W) time, O(W) space | O(1), exact O(2), cardinality 1: a fold over the constant `range(0, 14)` and a map. Spine: one fold step per item, each step is one descending-capacity DP row update (`c` from `W` down to the item weight) on a `[i64; 51]` row, so the 14 items and capacity 50 are constants. The `n * W` product lives in the step function's loops and the fixed row, invisible to the analyzer (see [#416](https://github.com/DominicBurkart/marigold/issues/416) per-item fn cost, [#415](https://github.com/DominicBurkart/marigold/issues/415) symbolic input size). Oracle: subset enumeration of all 2^14 subsets in the test. |
| `floyd_warshall` | [#411](https://github.com/DominicBurkart/marigold/issues/411) | [Floyd 1962](https://doi.org/10.1145/367766.368168), [Warshall 1962](https://doi.org/10.1145/321105.321107), [Wikipedia](https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm) | O(V^3) time, O(V^2) space | O(1), exact O(1), cardinality 1: a single fold over the constant `range(0, 8)`. Spine: one fold step per intermediate vertex `k`, each step relaxing all `V^2` pairs of the flat `[i64; 64]` distance matrix. The `V^3` lives in the step function's two nested loops and the fixed array, invisible to the analyzer (#415, #416). Oracle: Bellman-Ford from every source in the test. |
