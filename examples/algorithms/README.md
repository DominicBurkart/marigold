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

### dijkstra

- Issue: [#407](https://github.com/DominicBurkart/marigold/issues/407)
- Reference: [Dijkstra 1959](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
- Canonical complexity: O(V^2) with an array scan (O(E log V) with a heap)
- Analyzer estimate: O(1), exact O(2): `range(0, 9).fold(start, settle)`
  followed by `map(distances)`.

The 9 vertices are a constant, so the whole stream collapses to O(1) (symbolic
input size is [#415](https://github.com/DominicBurkart/marigold/issues/415)).
The O(V) scan and the O(E) relaxation inside `settle` are invisible to the
analyzer (per-item fn cost is
[#416](https://github.com/DominicBurkart/marigold/issues/416)).

One fold step settles exactly one vertex: select the closest unsettled vertex,
then relax its edges. The relaxation loops live inside `settle`, so the stream
shape is only the fold. Vertex 8 is isolated and stays at the 1000000000
sentinel. Edges are undirected and one is listed in descending vertex order,
so relaxation from both endpoints is exercised. Tests compare the result
with Bellman-Ford.

### kruskal

- Issue: [#408](https://github.com/DominicBurkart/marigold/issues/408)
- Reference: [Kruskal 1956](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm)
- Canonical complexity: O(E log E) for the sort, plus near-linear union-find
- Analyzer estimate: O(C(n,2)), exact 2*O(C(n,2)) + 3, `collects_input`.

The spine is `range(0, 8).combinations(2)` (all 28 vertex pairs),
`map(weigh)` (attach a weight, 0 for the 15 non-edges), `filter(is_edge)`
(keep the 13 edges), `keep_first_n(13, by_weight)` (sort ascending; the
comparator reverses the order because `keep_first_n` returns the largest
first) and `fold(start, unite)` (union-find, one fold step per edge).

Divergence from the canonical class: the analyzer reports the quadratic pair
enumeration, because the filter output size is unknown, and cannot see the
sort cost or the union-find cost inside `unite`
([#416](https://github.com/DominicBurkart/marigold/issues/416)). V = 8 is
constant, so the symbolic form is lost
([#415](https://github.com/DominicBurkart/marigold/issues/415)).

The candidate counts match the canonical values: C(8, 2) = 28 pairs after
`combinations(2)` and `map`, and `min(<=C(8, 2), 13)` after the filter and
`keep_first_n`. Edge weights are distinct, so the MST is unique; tests
compare the edges and the total weight with Prim. Union-find omits path
compression and union by rank (8 vertices).
