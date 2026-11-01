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

### tsp_brute_force

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: [Travelling salesman problem, exact algorithms][tsp-exact]
- Canonical complexity: O(n!) (here (n-1)! tours, 9! = 362880)
- Analyzer estimate: O(1), exact O(3), collects input, cardinality 1.

`range(1, 10).permutations(9)` has 362880 items, pinned on the prefix stream
in `tests/complexity.rs`. The input is a constant, so the analyzer collapses
the cost to O(1) and the factorial is invisible until input size can be
symbolic ([#415](https://github.com/DominicBurkart/marigold/issues/415)). The
per-item `tour_length` loop is not charged either
([#416](https://github.com/DominicBurkart/marigold/issues/416)).

The comparison with O(n!) assumes k = n - 1. Because k is fixed at 9 in the
program, a symbolic-input analyzer would report `OPermutational(9)`, not
O(n!); the divergence test rejects both `OFactorial` and `OPermutational(9)`.

City 0 is fixed as the start, so `permutations(9)` of the other nine cities
enumerates every tour once per direction. The cities are the shared fixture in
`src/tsp.rs`, copied into `dist`; a test asserts the program equals
`tsp::optimum()` (84) and another asserts both programs embed exactly the
fixture coordinates.

### tsp_held_karp

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: Held and Karp 1962 and Bellman 1962, below, and
  [Held-Karp algorithm][held-karp]
- Canonical complexity: O(2^n n^2)
- Analyzer estimate: O(1), exact O(2), does not collect input, cardinality 1.

The stream is a fold over the constant `range(1, 512)` followed by a map. The
prefix stream has 511 items, the non-empty subsets of the nine non-start
cities. That stream length is the 2^(n-1) factor of the canonical bound; the
analyzer hides it as a constant
([#415](https://github.com/DominicBurkart/marigold/issues/415)). Only the n^2
factor lives inside `fill_mask`, where the analyzer cannot see it
([#416](https://github.com/DominicBurkart/marigold/issues/416)).

The analyzer has no exponential complexity class
([#435](https://github.com/DominicBurkart/marigold/issues/435)), so the
canonical class cannot be expressed.
`held_karp_canonical_class_is_not_representable` pins the O(1) time class and
the 511-mask prefix instead; `tsp_held_karp` pins exact O(2).

Simplification: the DP table (512 masks x 9 end cities, a fixed
`[i32; 4608]`) is the fold state because `const fn` has no heap structures.
Each fold step fills exactly one mask row, pull-style, so subsets are visited
in increasing order and every predecessor row is complete. `close_tour` is a
separate map.

Cost of that state: the fold passes the roughly 18 KB table by value on every
step. In a release build the whole program takes about 549 microseconds per
run, against about 399 microseconds for a variant whose `fill_mask` only
copies the table, so roughly 70 to 80 percent of the runtime is state copying
rather than DP work (the program executes 9,216 relaxations). The benchmark
therefore measures the fold-state copy more than the algorithm. A smaller
state is a follow-up.

References for Held-Karp, as listed in the Wikipedia article (which gives no
DOI or URL for either):

- Richard Bellman, "Dynamic programming treatment of the travelling salesman
  problem", Journal of Assoc. Computing Mach. 9, 1962.
- Michael Held and Richard M. Karp, "A dynamic programming approach to
  sequencing problems", Journal for the Society for Industrial and Applied
  Mathematics 1:10, 1962.

[tsp-exact]: https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms
[held-karp]: https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
