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
| `tsp_brute_force` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms) | O(n!) (here (n-1)! tours, 9! = 362880) | O(1), exact O(3), collects input, cardinality 1. `range(1, 10).permutations(9)` has 362880 items (pinned on the prefix stream), but the input is a constant so the analyzer collapses the cost to O(1); the factorial is invisible until input size can be symbolic ([#415](https://github.com/DominicBurkart/marigold/issues/415)) and the per-item `tour_length` loop is not charged ([#416](https://github.com/DominicBurkart/marigold/issues/416)). City 0 is fixed as the start, so `permutations(9)` of the other nine cities enumerates every tour once per direction. The cities are the shared fixture in `src/tsp.rs`, copied into `dist`; a test asserts the program equals `tsp::optimum()`. |
| `tsp_held_karp` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | Held and Karp 1962, [Held-Karp algorithm](https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm) | O(2^n n^2) | O(1), exact O(2), does not collect input, cardinality 1: a fold over the constant `range(1, 512)` followed by a map. The prefix stream has 511 items (the non-empty subsets of the nine non-start cities). The exponential and the n^2 live inside `fill_mask` ([#416](https://github.com/DominicBurkart/marigold/issues/416)) and in the constant input ([#415](https://github.com/DominicBurkart/marigold/issues/415)). Simplification: the DP table (512 masks x 9 end cities, a fixed `[i32; 4608]`) is the fold state because `const fn` has no heap structures; each fold step fills exactly one mask row (pull-style, so subsets are visited in increasing order and every predecessor row is complete), and `close_tour` is a separate map. |
