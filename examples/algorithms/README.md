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
- Reference:
  [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms)
- Canonical complexity: O(n!) (here (n-1)! tours, 9! = 362880)
- Analyzer estimate: O(1), exact O(3), collects input, cardinality 1.
  `range(1, 10).permutations(9)` has 362880 items (pinned on the prefix stream),
  but the input is a constant so the analyzer collapses the cost to O(1).

The factorial is invisible until input size can be symbolic
([#415](https://github.com/DominicBurkart/marigold/issues/415)) and the per-item
`tour_length` loop is not charged
([#416](https://github.com/DominicBurkart/marigold/issues/416)). City 0 is fixed
as the start, so `permutations(9)` of the other nine cities enumerates every
tour once per direction. The cities are the shared fixture in `src/tsp.rs`,
copied into `dist`; a test asserts the program equals `tsp::optimum()`.

### tsp_held_karp

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: Held and Karp 1962,
  [Held-Karp algorithm](https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm)
- Canonical complexity: O(2^n n^2)
- Analyzer estimate: O(1), exact O(2), does not collect input, cardinality 1: a
  fold over the constant `range(1, 512)` followed by a map. The prefix stream
  has 511 items (the non-empty subsets of the nine non-start cities).

The exponential and the n^2 live inside `fill_mask`
([#416](https://github.com/DominicBurkart/marigold/issues/416)) and in the
constant input ([#415](https://github.com/DominicBurkart/marigold/issues/415)).
Simplification: the DP table (512 masks x 9 end cities, a fixed `[i32; 4608]`)
is the fold state because `const fn` has no heap structures; each fold step
fills exactly one mask row (pull-style, so subsets are visited in increasing
order and every predecessor row is complete), and `close_tour` is a separate
map.

### tsp_simulated_annealing

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference:
  [Kirkpatrick, Gelatt, Vecchi 1983](https://en.wikipedia.org/wiki/Simulated_annealing)
- Canonical complexity: O(I x n) for I iterations on n cities
- Analyzer estimate: O(1), exact O(2), space O(1), cardinality exactly 1, no
  input collection. The fold over a constant `range` collapses to one item and
  the per-step cost is a function body the analyzer cannot see
  ([#415](https://github.com/DominicBurkart/marigold/issues/415),
  [#416](https://github.com/DominicBurkart/marigold/issues/416)).

2000 iterations of 2-opt reversal, integer linear cooling, and a linear integer
approximation of Metropolis acceptance (accept a worse move when
`delta * 100 < u * T`, `u` in 0..100). One fold step is one move; the best tour
seen is kept. Tests pin the O(1) estimate and assert it differs from the
canonical class. The 10-city Manhattan instance from `src/tsp.rs` is repeated in
the program because programs are standalone. An in-program LCG
(`r * 1103515245 + 12345 mod 2^31`, high bits) with a fixed seed makes runs
deterministic.

### tsp_genetic

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: [Holland 1975](https://en.wikipedia.org/wiki/Genetic_algorithm),
  order crossover per Davis 1985
- Canonical complexity: O(G x P x n) for G generations and population P
- Analyzer estimate: O(1), exact O(2), space O(1), cardinality exactly 1, no
  input collection. The fold over a constant `range` collapses to one item and
  the per-step cost is a function body the analyzer cannot see
  ([#415](https://github.com/DominicBurkart/marigold/issues/415),
  [#416](https://github.com/DominicBurkart/marigold/issues/416)).

150 generations, population 16, tournament selection, order crossover, swap
mutation, elitism. One fold step is one generation. Tests pin the O(1) estimate
and assert it differs from the canonical class. The 10-city Manhattan instance
from `src/tsp.rs` is repeated in the program because programs are standalone. An
in-program LCG (`r * 1103515245 + 12345 mod 2^31`, high bits) with a fixed seed
makes runs deterministic.

### tsp_ant_colony

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference:
  [Dorigo, Maniezzo, Colorni 1996, Ant System](https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms)
- Canonical complexity: O(I x m x n^2) for I iterations and m ants
- Analyzer estimate: O(1), exact O(2), space O(1), cardinality exactly 1, no
  input collection. The fold over a constant `range` collapses to one item and
  the per-step cost is a function body the analyzer cannot see
  ([#415](https://github.com/DominicBurkart/marigold/issues/415),
  [#416](https://github.com/DominicBurkart/marigold/issues/416)).

200 iterations, 8 ants, integer fixed-point pheromone (scale 1000), weight
`tau * (10000 / d^2)`, roulette selection, evaporation 0.9. One fold step is one
colony iteration. Tests pin the O(1) estimate and assert it differs from the
canonical class. The 10-city Manhattan instance from `src/tsp.rs` is repeated in
the program because programs are standalone. An in-program LCG
(`r * 1103515245 + 12345 mod 2^31`, high bits) with a fixed seed makes runs
deterministic.

#### Metaheuristic notes

- The fold state is one fixed array (tour, length, RNG and best tour; 16 tours
  and RNG; pheromone matrix, best tour and RNG). Loops over cities, ants and
  individuals are `while` loops inside the step function because generated
  functions are `const fn`.
- Genetic and ant colony tours may start at any city; tours are cyclic, so
  lengths are comparable.
- With these seeds all three reach the optimum (84). This is a property of the
  seeds, not a guarantee of the heuristics; tests also assert a valid
  permutation and a length strictly below the identity tour (128).
- Mutation checks run: the annealing and ant colony tests fail when the
  iteration count is cut to 5 and 1, the genetic test fails when generations are
  cut to 1 and when the seed changes. Changing the annealing or ant colony seed
  still reaches 84, so those results are robust to the seed.
