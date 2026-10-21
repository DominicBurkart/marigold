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

## TSP metaheuristics

Issue [#406](https://github.com/DominicBurkart/marigold/issues/406). All three
solve the 10-city Manhattan instance in the shared TSP fixture (`src/tsp.rs`,
test-only); the coordinates are repeated in each `.marigold` file because
programs are standalone. Each uses an in-program LCG (`r * 1103515245 + 12345
mod 2^31`, high bits) with a fixed seed, so a run is deterministic.

Analyzer result for all three: O(1), exact O(2), space O(1), cardinality exactly
1, no input collection. The fold over a constant `range` collapses to one item,
and the per-step cost is a function body the analyzer cannot see
([#415](https://github.com/DominicBurkart/marigold/issues/415),
[#416](https://github.com/DominicBurkart/marigold/issues/416)). Tests pin the
O(1) estimate and assert it differs from the canonical class.

### tsp_simulated_annealing

[Kirkpatrick, Gelatt, Vecchi
1983](https://en.wikipedia.org/wiki/Simulated_annealing). Canonical O(I * n) for
I iterations on n cities. 2000 iterations of 2-opt reversal, integer linear
cooling, and a linear integer approximation of Metropolis acceptance (accept a
worse move when `delta * 100 < u * T`, `u` in 0..100). One fold step is one
move; the best tour seen is kept.

### tsp_genetic

[Holland 1975](https://en.wikipedia.org/wiki/Genetic_algorithm), order crossover
per Davis 1985. Canonical O(G * P * n) for G generations and population P. 150
generations, population 16, tournament selection, order crossover, swap
mutation, elitism. One fold step is one generation.

### tsp_ant_colony

[Dorigo, Maniezzo, Colorni 1996, Ant
System](https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms).
Canonical O(I * m * n^2) for I iterations and m ants. 200 iterations, 8 ants,
integer fixed-point pheromone (scale 1000), weight `tau * (10000 / d^2)`,
roulette selection, evaporation 0.9. One fold step is one colony iteration.

### Notes

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
