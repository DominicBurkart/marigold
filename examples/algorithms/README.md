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
- The `program!` macro allows `clippy::clone_on_copy` because the default
  `filter` codegen calls `.clone()` on the item, which is a `Copy` array for
  programs such as `kruskal`.

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
- Reference: Dijkstra 1959, "A note on two problems in connexion with
  graphs", Numerische Mathematik 1:269-271 (doi 10.1007/BF01386390),
  [Wikipedia](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
- Canonical complexity: O(V^2) with an array scan (O((V+E) log V) with a heap)
- Analyzer estimate: O(1), exact O(2), space O(2), cardinality 1:
  `range(0, 9).fold(start, settle).map(distances)`.

The stream carries the round structure: nine rounds, one fold step per round,
and a map that extracts the distances. Each step settles one vertex. The
selection of the closest unsettled vertex, the relaxation of its edges and
the settled flags are `while` loops inside `settle`, over a `[i64; 18]` state
(nine distances, nine flags) and a 13-edge array declared in the function.
There is no heap and no sort. Each round scans the nine vertices and loops
over all 13 edges, so the program does `O(V^2 + V x E)` work, more than the
canonical O(V^2) of an array scan over an adjacency matrix. The analyzer sees
none of it (per-item fn cost,
[#416](https://github.com/DominicBurkart/marigold/issues/416)). The sizes are
constants, so the stream collapses to O(1)
([#415](https://github.com/DominicBurkart/marigold/issues/415)). A fold runs
for the length of its source, so the nine rounds are a worst-case cap rather
than a data-dependent stop
([#429](https://github.com/DominicBurkart/marigold/issues/429)).
The source is vertex 0; the program has no other source.

The graph has nine vertices and 13 undirected edges with nonnegative weights.
The pinned distances from vertex 0 are `[0, 3, 1, 8, 11, 9, 8, 15, 1000000000]`,
checked against an independent heap-based Dijkstra and a Floyd-Warshall
script, and against Bellman-Ford in the Rust test. The instance binds each
component:

- Vertex 8 has no edges, so it keeps the `1000000000` sentinel.
- Edges 0-1, 2-5, 4-6 and 3-7 are each longer than the distance gap they
  span, so the direct edge loses to a cheaper detour.
- Vertex 3 has two shortest paths (3+5 and 1+7), and vertices 3 and 6 are
  tied at distance 8 when the next vertex is selected.
- Edge 5-6 is listed as `[5, 6, 1]` and vertex 6 is settled before 5, so the
  relaxation from the second endpoint is needed.

The edge list in the program text is compared with the oracle's edge list.
The guard reads digits only, so it cannot see a sign flip (weights are
nonnegative by contract).

Nine rounds are not all needed: seven are the least that give the pinned
answer (checked: six give `28` for vertex 7). Round eight settles vertex 7,
which has no unsettled neighbour, and round nine selects the isolated vertex 8
and returns at the unreachable guard without settling it.

Mutations run against the program (each changes the program file, then
`cargo test -p algorithms`):

- Fail the run tests: skipping the relaxation from either endpoint, selecting
  without the settled flag, selecting the farthest vertex, ignoring the edge
  weight in the relaxation, source distance 1, six rounds, and changing the
  weight of edge 2-6 or 4-7 (the last two also fail the edge-list guard).
- Fail only the complexity test, because the run result is unchanged: eight
  or seven rounds (the pinned cardinality of the range is nine).
- Survive: dropping the settled-flag check in the relaxation (equivalent for
  nonnegative weights). Setting the edge `[5, 4, 2]` to `[5, 4, -12]`
  separates every variant: vertex 6 is `8` in the original and `4` with the
  check dropped in the first-endpoint relaxation, and vertex 5 is `9` in the
  original and `-15` with it dropped in the second-endpoint relaxation (or in
  both). Setting edge 1-3 to `-9` separates only the second-endpoint
  variant: vertex 1 is `3` in the original and `-15` in the mutant, while the
  first-endpoint variant matches the original.
- Also survive: selecting ties by the later index (the settlement order
  changes from 3, 6 to 6, 3 but the distances do not), and `>` for `>=` in
  the unreachable guard (the mutant settles vertex 8 in round nine; it has no
  edges, so nothing else changes).

### kruskal

- Issue: [#408](https://github.com/DominicBurkart/marigold/issues/408)
- Reference: Kruskal 1956, "On the shortest spanning subtree of a graph and
  the traveling salesman problem", Proceedings of the AMS 7(1):48-50
  (doi 10.1090/S0002-9939-1956-0078686-7),
  [Wikipedia](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm)
- Canonical complexity: O(E log E) for the sort, plus near-linear union-find
- Analyzer estimate: O(C(n,2)), exact 3 + 2*C(n,2), space O(5),
  `collects_input`, cardinality 1.

The stream carries the candidate set, the sort and the sweep:
`range(0, 8).combinations(2)` (28 vertex pairs), `map(weigh)` (attach a
weight, 0 for the 15 non-edges), `filter(is_edge)` (keep the 13 edges),
`keep_first_n(13, by_weight)` (sort ascending; the comparator reverses the
order because `keep_first_n` returns the largest first) and
`fold(start, unite)` (one fold step per sorted edge). The sort is the
runtime's `keep_first_n`, not a loop in the program. Hidden in function
bodies: the edge lookup in `weigh` (a linear search of a 13-edge array), the
union-find `root` loop, and the union itself. The fold carries the
union-find parents, the total weight, the edge count and the chosen edges in
a `[i64; 24]`. The union-find has no path compression and no union by rank.

The work the analyzer does not see adds up as follows. `weigh` searches the
edge array once per pair, `O(V^2 x E)` over the 28 pairs. The runtime sort is
`O(E log E)`. Each of the E fold steps runs two `root` loops that cost O(V),
because there is no compression or rank, so `O(E x V)` in total, against the
canonical near-linear union-find. The program is therefore
`O(V^2 x E + E log E + E x V)`, dominated by `weigh`, not the canonical
O(E log E).

The analyzer reports O(C(n,2)) because it bounds the filtered stream by the
28 pairs; it cannot see the sort cost or the union-find cost inside `unite`
([#416](https://github.com/DominicBurkart/marigold/issues/416)), and V = 8 is
constant, so the symbolic form is lost
([#415](https://github.com/DominicBurkart/marigold/issues/415)). The canonical
O(E log E) is therefore not reported (a test asserts the difference). The
per-stage cardinalities are pinned from the program file: 28 pairs after
`combinations(2)`, 28 after `map`, `<=C(8, 2)` after `filter`, and
`min(<=C(8, 2), 13)` after `keep_first_n`.

The graph has eight vertices and 13 edges and is connected, so the fold
yields a spanning tree; a disconnected graph would give a forest but is not
exercised. Weights 8 on edges 1-2 and 2-4 are equal and close a cycle with
edge 1-4, so there are two minimum spanning trees of weight 53 (enumerated by
script; the other uses 2-4 in place of 1-2). The comparator breaks equal weights
by pair id, lower first, so the program picks 1-2. `keep_first_n` orders
equal items differently by build (checked: without the pair-id tie-break the
pinned set held in the default build and failed under `tokio` and
`async-std`, where 2-4 came out first); this is tracked in
[#452](https://github.com/DominicBurkart/marigold/issues/452). The Kruskal
program works around it by breaking equal weights by pair id inside its
comparator, so its result is identical on all four feature sets. The
pinned set is `0-1 0-3 1-2 1-4 3-5 4-6 6-7`; the rejected cycle edges are
`1-3 2-4 3-4 4-5 5-6 5-7`. The Rust test checks that the result is a spanning
tree whose weight equals Prim's, and the edge list and the `keep_first_n`
count in the program text are compared with the oracle's. As for Dijkstra,
the edge-list guard reads digits only and cannot see a sign flip.

Mutations run against the program:

- Fail the run tests (default features): skipping the cycle check, a
  descending sort, a `root` that never climbs, `>=` in `is_edge`, recording
  the wrong endpoint, wrong initial parents, not accumulating the weight, not
  counting edges, dropping the `filter` (this also fails the `kruskal`
  analyzer pin and the cardinality pin), an edge written with `v < u`,
  changing the weight of edge 1-4, and reversing the pair-id tie-break (the
  last fails exactly the cycle-edge, tie and pinned-set tests).
- Dropping the pair-id tie-break passes in the default build and fails the
  same three tests under `--features tokio` and `--features async-std`.
- Fail only a static test, because the run result is unchanged: changing the
  weight of edge 2-4 to 9 (edge-list guard only), `range(0, 9)` (the extra
  pairs have weight 0 and are filtered; the cardinality pin fails), and
  `keep_first_n` of 12 or 14 (edge-list guard and cardinality pin).
- Survive: swapping the union direction (only which vertex is the root
  changes) and replacing the `Equal` arm of `by_weight` with a `panic!`
  (default, `tokio` and `async-std`). The 13 sort keys, `weight * 64 + pair
  id`, are distinct, so the comparator is never called with equal keys and
  the arm is unreachable.

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
- All three reach the optimum (84) for the committed seeds. Manual seed
  sweeps (the LCG seed edited in the `.marigold` file, outside the test
  suite) tried seeds 1, 777, 4242, 99991 and 31337 for each program, plus
  one more for each (annealing 54321, genetic 11111, ant colony 13579).
  Annealing and ant colony reached 84 for all six; genetic failed for 99991
  and 11111. So the guarantee is "reaches the optimum for these seeds and
  many others", not a proof, and genetic is the most seed-sensitive. Seeds
  are compile-time constants in the generated `const fn`s, so the tests
  cannot sweep seeds at run time; this is a stated limitation. Tests assert
  a valid permutation, a length strictly below the identity tour (128), and
  equality with the optimum.
- Mutation checks run: the annealing and ant colony tests fail when the
  iteration count is cut to 5 and 1, the genetic test fails when generations are
  cut to 1 and when the seed changes. Changing the annealing or ant colony seed
  still reaches 84, so the tests do not pin their seed-dependent trajectory.

[tsp-exact]: https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms
[held-karp]: https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
[binary-search]: https://en.wikipedia.org/wiki/Binary_search_algorithm
[quicksort]: https://en.wikipedia.org/wiki/Quicksort
