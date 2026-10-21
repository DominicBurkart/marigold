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
| `tsp_simulated_annealing` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | [Kirkpatrick, Gelatt, Vecchi 1983](https://en.wikipedia.org/wiki/Simulated_annealing) | O(I * n) for I iterations on n cities (full tour re-evaluation per 2-opt move) | O(1), exact O(2), cardinality exactly 1 (fold collapses the constant `range` to one item), no input collection. The canonical cost is hidden in the constant iteration count and in the per-step function body (see issues [#415](https://github.com/DominicBurkart/marigold/issues/415) and [#416](https://github.com/DominicBurkart/marigold/issues/416)). Config: 2000 iterations, integer linear cooling, linear acceptance approximation of exp(-delta/T); one fold step is one annealing move. |
| `tsp_genetic` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | [Holland 1975; order crossover, Davis 1985](https://en.wikipedia.org/wiki/Genetic_algorithm) | O(G * P * n) for G generations, population P | O(1), exact O(2), cardinality exactly 1 (fold collapses the constant `range` to one item), no input collection. The canonical cost is hidden in the constant iteration count and in the per-step function body (see issues [#415](https://github.com/DominicBurkart/marigold/issues/415) and [#416](https://github.com/DominicBurkart/marigold/issues/416)). Config: 150 generations, population 16, tournament selection, OX, swap mutation, elitism; one fold step is one generation. |
| `tsp_ant_colony` | [#406](https://github.com/DominicBurkart/marigold/issues/406) | [Dorigo, Maniezzo, Colorni 1996, Ant System](https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms) | O(I * m * n^2) for I iterations, m ants | O(1), exact O(2), cardinality exactly 1 (fold collapses the constant `range` to one item), no input collection. The canonical cost is hidden in the constant iteration count and in the per-step function body (see issues [#415](https://github.com/DominicBurkart/marigold/issues/415) and [#416](https://github.com/DominicBurkart/marigold/issues/416)). Config: 200 iterations, 8 ants, fixed-point pheromone (scale 1000), weight tau * (10000 / d^2); one fold step is one colony iteration. |

## TSP metaheuristics

All three share the 10-city Manhattan instance in `src/tsp.rs`, with the same coordinates embedded in each `.marigold` file because programs must be standalone. Each uses a deterministic LCG (`r * 1103515245 + 12345 mod 2^31`, high bits used) inside the program, so every run returns the same tour.

- State is a fixed array carried through `fold` (tour, current length, RNG state and best tour for annealing; 16 tours plus RNG for the genetic algorithm; pheromone matrix, best tour and RNG for ant colony). Each fold step is exactly one algorithmic iteration (one annealing move, one generation, one colony iteration), which is the stream spine. Inside a step, loops over cities, ants or individuals are `while` loops because generated functions are `const fn`; the analyzer cannot see them.
- Annealing and ant colony use integer arithmetic only. The Metropolis rule is approximated by accepting a worse move when `delta * 100 < u * T` with `u` uniform in 0..100.
- Genetic and ant colony tours may start at any city (tours are cyclic); the tests compare cyclic tour length with `tsp::optimum()`.
- With these seeds all three reach the exact optimum (84), deterministically, and the tests assert equality with the brute-force optimum as well as validity and strict improvement over the identity tour. This is a property of the seeds, not a guarantee of the heuristics.
