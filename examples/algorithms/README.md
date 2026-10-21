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

- The program runs and returns the right answer, compared with an independent
  oracle: `src/<name>.rs` tests (`cargo test -p algorithms`).
- The analyzer estimate is pinned in `tests/complexity.rs`. Where it differs
  from the canonical class, a test asserts the difference, so an analyzer fix
  shows up as a failing test.
- A benchmark in `benches/algorithms_bench.rs`, run by
  `.github/workflows/bench.yaml`.

## Writing a program

- `map`, `filter` and `fold` take function names, not closures. Functions are
  emitted as `const fn`, so bodies cannot allocate or use iterators; use fixed
  size arrays and `while` loops. Array literals need an explicit element type
  when a method such as `.abs()` is called on their items.
- There are no comments at top level of a `.marigold` file. References live
  in this file.
- Fixed-size inputs are declared inside functions. The analyzer sees only the
  stream shape, so loops that live inside a function body are invisible to it.
- Keep this file within 80 columns (markdownlint enforces it, tables
  included): one `###` section per program, as below.

## Programs

### gcd

- Issue: [#410](https://github.com/DominicBurkart/marigold/issues/410)
- Reference: [Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm)
- Canonical complexity: O(log(min(a, b)))
- Analyzer estimate: O(1), exact O(2): a fold over the constant
  `range(0, 48)` and a map.

Marigold has no data-dependent termination: a fold runs for the length of its
source. This Euclid is therefore a fixed-iteration program, O(1) by
construction, and the analyzer's O(1) is correct for the program as written.
It is not a demonstration of O(log(min(a, b))); see
[#429](https://github.com/DominicBurkart/marigold/issues/429).

The 48 rounds bound the Euclid steps for every `u32` pair: the worst case is
consecutive Fibonacci numbers (Lamé's theorem), and the largest such pair that
fits in `u32` is (F47, F46), which takes 45 steps. The program runs 64 pairs
per round, including that worst case, a zero operand and LCG-derived pairs, so
the bound and the arithmetic are exercised together. Tests compare every pair
with a reference Euclid and assert that the worst case needs fewer rounds than
the cap.

### knapsack

- Issue: [#409](https://github.com/DominicBurkart/marigold/issues/409)
- Reference: [Knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem)
- Canonical complexity: O(n * W) time, O(W) space
- Analyzer estimate: O(1), exact O(2), cardinality 1: a fold over the
  constant `range(0, 14)` and a map.

The spine is a fold with one item per step. Each step is one
descending-capacity DP row update (`c` from `W` down to the item weight) on a
`[i64; 51]` row, so the 14 items and capacity 50 are constants. The `n * W`
product lives in the step function's loop and the fixed row, which the
analyzer cannot see (per-item function cost:
[#416](https://github.com/DominicBurkart/marigold/issues/416); symbolic input
size: [#415](https://github.com/DominicBurkart/marigold/issues/415)). The
oracle enumerates all 2^14 subsets in the test. The tests assert that the
analyzer class differs from the canonical class, and pin the range
cardinality (14) and the folded cardinality (1).

### floyd_warshall

- Issue: [#411](https://github.com/DominicBurkart/marigold/issues/411)
- Reference: Floyd 1962 (doi 10.1145/367766.368168), Warshall 1962
  (doi 10.1145/321105.321107),
  [Wikipedia](https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm)
- Canonical complexity: O(V^3) time, O(V^2) space
- Analyzer estimate: O(1), exact O(1), cardinality 1: a single fold over the
  constant `range(0, 8)`.

The spine is a fold with one step per intermediate vertex `k`, each step
relaxing all `V^2` pairs of the flat `[i64; 64]` distance matrix. The `V^3`
lives in the step function's two nested loops and the fixed array, invisible
to the analyzer (#415, #416). The oracle is Bellman-Ford from every source in
the test. Weights are nonnegative; there is no negative-cycle case. The tests
pin the range cardinality (8) and assert divergence from the canonical class.
