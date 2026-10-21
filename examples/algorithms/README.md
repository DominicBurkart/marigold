# Algorithm examples

Each program in `programs/` is a standalone `.marigold` file. `build.rs` turns
it into Rust with the same parser the `m!` macro uses, so the same file is what
the run tests, the complexity analysis and the benches exercise. Programs end
in `.return` so tests can read their output; `marigold run` wraps a program in
a `main` that expects no output, so use `marigold analyze` or `marigold check`
on these files rather than `marigold run`.

`build.rs` always emits the default (single-future) codegen. Forwarding the
`tokio`/`async-std` features to `marigold-grammar` would change what `m!`
generates for every crate in the workspace, so the examples run on the
requested runtime but do not exercise the parallel codegen paths.

Every example has three checks:

| Check | Where |
| --- | --- |
| program runs and returns the right answer, compared with an independent oracle | `src/<name>.rs` tests (`cargo test -p algorithms`) |
| analyzer estimate pinned; where it differs from the canonical class, a test asserts the difference so an analyzer fix shows up as a failing test | `tests/complexity.rs` |
| benchmark | `benches/algorithms_bench.rs`, run by `.github/workflows/bench.yaml` |

## Writing a program

- `map`, `filter` and `fold` take function names, not closures. Functions are
  emitted as `const fn`, so bodies cannot allocate or use iterators; use fixed
  size arrays and `while` loops. Array literals need an explicit element type
  when a method such as `.abs()` is called on their items.
- There are no comments at top level of a `.marigold` file. References live in
  the table below.
- Fixed-size inputs are declared inside functions. The analyzer sees only the
  stream shape, so loops that live inside a function body are invisible to it.

## Programs

| Program | Issue | Reference | Canonical complexity | Analyzer estimate |
| --- | --- | --- | --- | --- |
| `gcd` | [#410](https://github.com/DominicBurkart/marigold/issues/410) | [Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm) | O(log(min(a, b))) | O(1), exact O(2): a fold over the constant `range(0, 48)` and a map. |
| `tsp_brute_force` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms) | O(n!) (here (n-1)! tours, 9! = 362880) | O(1), exact O(3), collects input, cardinality 1. `range(1, 10).permutations(9)` has 362880 items (pinned on the prefix stream), but the input is a constant so the analyzer collapses the cost to O(1); the factorial is invisible until input size can be symbolic ([#415](https://github.com/DominicBurkart/marigold/issues/415)) and the per-item `tour_length` loop is not charged ([#416](https://github.com/DominicBurkart/marigold/issues/416)). City 0 is fixed as the start, so `permutations(9)` of the other nine cities enumerates every tour once per direction. The cities are the shared fixture in `src/tsp.rs`, copied into `dist`; a test asserts the program equals `tsp::optimum()`. |
| `tsp_held_karp` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | Held and Karp 1962, [Held-Karp algorithm](https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm) | O(2^n n^2) | O(1), exact O(2), does not collect input, cardinality 1: a fold over the constant `range(1, 512)` followed by a map. The prefix stream has 511 items (the non-empty subsets of the nine non-start cities). The exponential and the n^2 live inside `fill_mask` ([#416](https://github.com/DominicBurkart/marigold/issues/416)) and in the constant input ([#415](https://github.com/DominicBurkart/marigold/issues/415)). Simplification: the DP table (512 masks x 9 end cities, a fixed `[i32; 4608]`) is the fold state because `const fn` has no heap structures; each fold step fills exactly one mask row (pull-style, so subsets are visited in increasing order and every predecessor row is complete), and `close_tour` is a separate map. |

### GCD

Marigold has no data-dependent termination: a fold runs for the length of its
source. This Euclid is therefore a fixed-iteration program, O(1) by
construction, and the analyzer's O(1) is correct for the program as written.
It is not a demonstration of O(log(min(a, b))).

The 48 rounds bound the Euclid steps for every `u32` pair: the worst case is
consecutive Fibonacci numbers (Lamé's theorem), and the largest such pair that
fits in `u32` is (F47, F46), which takes 45 steps. The program runs 64 pairs
per round, including that worst case, a zero operand and LCG-derived pairs, so
the bound and the arithmetic are exercised together. Tests compare every pair
with a reference Euclid and assert that the worst case needs fewer rounds than
the cap.
