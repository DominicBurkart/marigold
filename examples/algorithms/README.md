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

The spine is a fold with one item per step. Each step is one DP row update
(`c` from `W` down to the item weight) on a `[i64; 51]` row, so the 14 items
and capacity 50 are constants. The step reads the previous row and writes a
new one, so the direction of the capacity loop does not change the result.
The `n * W` product lives in the step function's loop and the fixed row, which the
analyzer cannot see (per-item function cost:
[#416](https://github.com/DominicBurkart/marigold/issues/416); symbolic input
size: [#415](https://github.com/DominicBurkart/marigold/issues/415)). The
analyzer therefore cannot express O(n * W): the tests pin the 14-item range
and the single folded state, derived from the program file, and assert that
the reported classes differ from the canonical ones. Those tests use
`assert_ne!`, so they show a difference, not the exact class.

The run test compares the program with an enumeration of all 2^14 subsets and
with the pinned optimum 136. The optimum is not unique (three subsets reach
it), so some single-item edits leave the result unchanged. Nearby instances
have other optima: 101 without the last item, 134 at capacity 49, 1000 if the
step reads the in-place row with an ascending capacity loop (the unbounded
variant), 127 if every candidate is taken, 0 if none is. Reading the in-place
row with the descending loop is the textbook 0/1 update and gives 136, as
does reading the previous row with either loop direction. A test compares the
item arrays in the program text with the oracle's constants.

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
to the analyzer (#415, #416). The tests pin the 8-vertex range from the
program file and assert divergence from the canonical classes.

The graph is two directed rings of four vertices joined by one edge (nine
edges), so every vertex is a needed intermediate and 16 pairs are
unreachable. The run test compares the program with Bellman-Ford from every
source, and with a pinned 64-entry matrix that includes the `1000000`
unreachable sentinel. A test compares the edge list in the program text with
the oracle's. Weights are nonnegative; there is no negative-cycle case.

### binary_search

- Issue: [#412](https://github.com/DominicBurkart/marigold/issues/412)
- Reference: [Binary search algorithm][binary-search]
- Canonical complexity: O(log n) per query
- Analyzer estimate: O(1), exact O(2), cardinality 1: a fold over the
  constant `range(0, 5)` and a map.

The spine is a fold with one halving round per step, over the sorted
16-element array declared in `sorted()`. Each round halves the `[lo, hi)`
interval of all 8 queries in `targets()` at once and the final map returns the
8 lower-bound indices (the first index whose element is not less than the
target). The program is a fixed-iteration fold sized for the worst case: the
interval starts with 17 candidates (0 through 16), so 5 rounds are needed and
5 is the cap. It is O(1) by construction, not a demonstration of O(log n). The
analyzer cannot see data-dependent early exit
([#429](https://github.com/DominicBurkart/marigold/issues/429)), the per-round
work lives in the fn body
([#416](https://github.com/DominicBurkart/marigold/issues/416)) and sizes are
constants ([#415](https://github.com/DominicBurkart/marigold/issues/415)).
`tests/complexity.rs` pins the 5-round prefix (derived from the program file),
the single folded state and exact O(2), and asserts the class is not `OLogN`.

The queries are `0` (below the minimum), `1` (first element), `3` (duplicate
run, returns its first index 1), `7` (unique hit), `8` (miss between
elements), `12` (duplicate run of three, index 5), `99` (last element) and
`100` (above the maximum, index 16 = length). Tests compare the result with
`partition_point`, with `binary_search().is_ok()` for found versus absent, and
with the pinned vector `[0, 0, 1, 3, 4, 5, 15, 16]`. The pinned numbers were
checked with an independent Python script. A test reads `sorted`, `targets`
and the round count out of the program text and compares them with the
oracle's constants. The cap tests simulate the same loop: 5 rounds are
correct for every target from -5 to 105 and 4 rounds are wrong for the query
`3`.

Mutations run against the program, each with `cargo test -p algorithms
--no-fail-fast`:

- Killed by the behavior tests: 4 rounds (also by the round-count pin and
  the complexity pin); mid rounded up; `lo = mid`; `hi = mid - 1`;
  `hi = mid + 1`; `<=` in the comparison (upper bound instead of lower
  bound); guard `lo <= hi`; guard removed; initial `hi` 15; changing the
  sorted element `7` to `6` or `99` to `98` (both move a query's lower bound,
  and the drift test also fails).
- Killed only by the drift test: changes that leave every lower bound
  unchanged, namely `40, 40` to `40, 41`, `70` to `71`, target `8` to `9`
  (still a miss at index 4) and target `100` to `101` (still index 16). No
  behavior test can see these; a sorted element or target change is caught by
  a behavior test only when it moves some query's lower bound or hit flag.
- Survivors, equivalent: `lo + (hi - lo) / 2` for the midpoint (same value);
  returning `hi` instead of `lo` (after 5 rounds every interval has
  `lo == hi`, so the mutation is equivalent under the cap).
- 6 rounds is output-equivalent (a spare round does nothing once `lo == hi`).
  It fails two pins, not a behavior test: the round-count drift test
  `program_data_equals_the_oracle_data` and
  `binary_search_canonical_class_is_not_representable` (the 5-round prefix
  cardinality).

### quicksort

- Issue: [#413](https://github.com/DominicBurkart/marigold/issues/413)
- Reference: Hoare, "Quicksort", The Computer Journal 5:1, 1962, pp. 10-16
  (doi 10.1093/comjnl/5.1.10), [Quicksort][quicksort]
- Canonical complexity: O(n log n) average, O(n^2) worst
- Analyzer estimate: O(1), exact O(2), cardinality 1: a fold over the
  constant `range(0, 15)` and a map.

The spine is a fold with one partition per step. The state is a flat
`[i64; 198]` holding six independent instances of 33 slots each: a stack
pointer, 16 data slots and a 16-slot work-list of `(lo, hi)` pairs. A step
pops one range per instance, performs one Lomuto partition (last element as
pivot, not Hoare's two-pointer scheme) and pushes the sub-ranges of size 2 or
more. Every step fixes a pivot, so at most n - 1 = 15 partitions occur and 15
is the cap. This is a fixed-iteration fold sized for the O(n^2) worst case, so
the analyzer reports O(1) by construction. Data-dependent termination is
[#429](https://github.com/DominicBurkart/marigold/issues/429), the partition
work in the fn body is
[#416](https://github.com/DominicBurkart/marigold/issues/416) and constant
sizes are [#415](https://github.com/DominicBurkart/marigold/issues/415).
`tests/complexity.rs` pins the 15-step prefix from the program file, the
single folded state and exact O(2), and asserts the class is not `ONLogN`.

The six instances are fixed arrays declared in the program (the program stays
standalone): already sorted, reversed, with duplicates, all equal, a single
element (length 1) and a pair (length 2, padding left untouched). Partitions
needed, from an independent Python script: 15, 15, 14, 15, 0 and 1. Sorted and
reversed distinct input are the worst case for a last-element pivot, so the
cap is tight. Sorted and all-equal input never move data, so they check the
step bookkeeping but not the cap; the reversed, duplicate and pair instances
do move data and fail one step short. Tests compare each instance with
`slice::sort` on its first `length` elements and compare the program text with
the oracle's arrays and lengths.

Mutations run against the program, each with `cargo test -p algorithms
--no-fail-fast`:

- Killed by the behavior tests: 14 steps; `>` instead of `<` (descending);
  left or right bound `+ 3`; left or right sub-range including the pivot;
  pivot taken from the first element; final pivot swap dropped; initial range
  `n - 2`; pair not pushed (`>= 3`); the last instance never stepped; a
  changed data value in the equal or duplicates arrays; a changed length on
  the reversed, duplicates, single or pair instance.
- Killed only by the drift test: a changed length on the sorted or all-equal
  instance (16 to 15), because those instances never move data.
- Survivors, equivalent: `<=` instead of `<` (still a valid partition, equal
  keys go left); `while j <= hi` (the extra iteration compares the pivot with
  itself, and `a[hi] < a[hi]` is false); the push order of the two
  sub-ranges (the ranges are disjoint, so order does not change the result);
  pushing size-1 sub-ranges on the left, on the right or initially. The last
  one does need more pops than the cap: left singletons make the sorted
  instance need 16 pops, and right singletons make the reversed, duplicates
  and all-equal instances need 16 each, against a cap of 15. It is still
  equivalent. Every pop fixes one distinct position as pivot, so there are at
  most n = 16 pops; if there are 16, every position was a pivot, so the last
  pop is a size-1 range that pushes nothing and moves nothing. Under a cap of
  n - 1 = 15 the result equals the original's.
- 16 steps is output-equivalent (a spare step). It fails two pins, not a
  behavior test: the step-count drift test `program_data_equals_the_oracle_data`
  and `quicksort_canonical_class_is_not_representable` (the 15-step prefix
  cardinality).

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

[tsp-exact]: https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms
[held-karp]: https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
[binary-search]: https://en.wikipedia.org/wiki/Binary_search_algorithm
[quicksort]: https://en.wikipedia.org/wiki/Quicksort
