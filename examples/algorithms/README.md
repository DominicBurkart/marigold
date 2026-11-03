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
- Reference: Kirkpatrick, Gelatt and Vecchi, "Optimization by Simulated
  Annealing", Science 220(4598):671-680, 1983,
  [doi:10.1126/science.220.4598.671][sa-doi], and the Wikipedia article
  [Simulated annealing][sa-wiki]
- Canonical complexity: O(I n) for I iterations on n cities (each move
  re-evaluates the whole tour)
- Analyzer estimate: O(1), exact O(2), exact space O(2), cardinality 1, does
  not collect input

### tsp_genetic

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: John Holland, "Adaptation in Natural and Artificial Systems", MIT
  Press, 1992 (the edition the Wikipedia article lists;
  [doi:10.7551/mitpress/1090.001.0001][ga-doi]), and the Wikipedia article
  [Genetic algorithm][ga-wiki]. Order crossover (OX1) is implemented as
  described in the Wikipedia article [Crossover (evolutionary
  algorithm)][ox-wiki], which attributes it to Davis and cites Lawrence Davis,
  "Handbook of Genetic Algorithms", Van Nostrand Reinhold, 1991.
- Canonical complexity: O(G P n) for G generations and population P (each
  child re-evaluates tours of length n)
- Analyzer estimate: O(1), exact O(2), exact space O(2), cardinality 1, does
  not collect input

### tsp_ant_colony

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: Dorigo, Maniezzo and Colorni, "Ant system: optimization by a
  colony of cooperating agents", IEEE Transactions on Systems, Man, and
  Cybernetics, Part B 26(1):29-41, 1996,
  [doi:10.1109/3477.484436][aco-doi], and the Wikipedia article
  [Ant colony optimization algorithms][aco-wiki]
- Canonical complexity: O(I m n^2) for I iterations and m ants (each ant step
  scans the n cities)
- Analyzer estimate: O(1), exact O(2), exact space O(2), cardinality 1, does
  not collect input

The three programs share one shape: a fold over a constant `range` (2000
iterations, 150 generations, 200 iterations) whose step function is exactly one
algorithmic iteration, followed by a map that extracts the best tour. The whole
algorithm state travels through the fold in one fixed array: tour, length, RNG
and best tour for annealing; 16 tours and the RNG for the genetic algorithm;
the 10 x 10 pheromone matrix, best tour and RNG for ant colony. The loops over
cities, individuals and ants are `while` loops inside the step functions,
because generated functions are `const fn`. The stream therefore carries only
the iteration structure. The iteration counts are constants, so the analyzer
reports O(1) by construction, and it cannot see the loops in the step
functions ([#429](https://github.com/DominicBurkart/marigold/issues/429),
[#416](https://github.com/DominicBurkart/marigold/issues/416),
[#415](https://github.com/DominicBurkart/marigold/issues/415),
[#435](https://github.com/DominicBurkart/marigold/issues/435)). The canonical
bounds above are the textbook statements, not measurements of these programs.

Each `<name>_canonical_class_is_not_representable` test in
`tests/complexity.rs` pins the prefix cardinality read from the program file
(`analyze_prefix(name, None)` for the range, `Some("fold")` for the fold)
and asserts that the estimate differs from the canonical class. The final
`assert_ne!` in each of these tests documents the divergence; it cannot fail
while the sibling test pins the O(1) class.

Parameters, all fixed in the program file and asserted by
`program_embeds_the_seed_and_parameters_the_reference_uses` in each module:

- Every program uses the LCG `(r * 1103515245 + 12345) % 2147483648` and reads
  the high bits (`r >> 16`, or `r >> 8` for the ant colony roulette).
- Annealing: seed 12345, 2000 iterations, 2-opt reversal of positions in
  1..=9 (position 0 holds city 0 for the whole run), and the Metropolis rule:
  a worse move (`delta > 0`) is accepted with probability `exp(-delta / T)`.
  The temperature is stored in sixteenths, `temp = 16 T = 480 * (2000 - round)
  / 2000` (so T falls linearly from 30 towards 0), with a floor of 1 so the
  division below is defined. The program does no floating point. It reads
  `exp` from a table of 97 integers, `table[k] = round(32768 * exp(-k / 16))`,
  indexed by `k = (delta * 256 + temp / 2) / temp`, which is `delta / T` in
  sixteenths rounded to nearest. A worse move is accepted when `k <= 96` and
  the draw `u = r >> 16` (uniform on 0..32767) is below `table[k]`. Beyond
  `k = 96`, that is `delta / T` above 6.03, the probability is treated as 0;
  the true value there is at most 0.25 percent.
- Genetic: seed 987654321, 150 generations, population 16, binary tournament,
  OX1 with two random cuts, a swap mutation with probability 20 percent, and
  the best tour carried over unchanged.
- Ant colony: seed 24680, 200 iterations, 8 ants, initial pheromone 1000,
  weight `tau * (10000 / d^2)` (Ant System with alpha = 1 and beta = 2, the
  heuristic term being the integer `10000 / d^2`), roulette selection,
  evaporation to 90 percent with a floor of 10, and a deposit of
  `100000 / length` on each edge of each ant's tour.

The programs embed the 10 city coordinates as `[i32; 10]` arrays, and
`programs_embed_the_shared_cities` in `src/tsp.rs` parses them from the
program files and compares them with `tsp::CITIES`.

What the run tests assert, for each program: the output is one tour that
contains every city exactly once (`returns_every_city_exactly_once`); its
length is at least `tsp::optimum()` (84) and below the identity tour, whose
length is 128 (`length_is_at_least_the_optimum_and_below_the_identity_tour`);
and the tour and its length equal the values from an independent Python
re-implementation of the same LCG and algorithm
(`matches_independent_python_reference_for_the_fixed_seed`). The pinned values
are:

- Annealing: tour `0 1 8 4 6 5 3 7 9 2` (starts at city 0), length 84.
- Genetic: tour `1 8 4 6 5 3 7 9 2 0`, length 84.
- Ant colony: tour `3 8 4 6 1 0 2 7 9 5`, length 84.

The annealing table is checked by
`exp_table_in_the_program_matches_f64_exp_within_half_a_unit`: it parses the 97
entries out of the program text with `include_str!` and asserts each is within
0.5 of `32768 * exp(-k / 16)` computed with f64 `exp`, which is the bound for
round-to-nearest. Changing any single entry by one fails it (checked for
entries 0, 1 and 5).

Tours are cyclic, so the genetic and ant colony tours start at an arbitrary
city. All three lengths equal the optimum for the pinned seeds. That is the
only guarantee: it is not a property of the algorithms.

An earlier version of the annealing program used a truncated linear
acceptance rule (accept with probability about 1 - delta/T, never when
delta >= T); it reached 84 on all 20 seeds below. The Metropolis rule, which is
what the cited paper describes, reaches 84 on 14 of 20 (see below).
Faithfulness to the cited algorithm was preferred over the better score.

Seeds tried. The Python reference was run with 20 seeds per program (1, 2, 3,
5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 777, 4242, 11111, 13579, 31337, 54321
and 99991). The program files were also run with 8 edited seeds (1, 777, 4242,
31337, 99991, 54321, 11111, 13579), and every tour equalled the Python tour for
the same seed. Ant colony reached length 84 for all 20 seeds. The genetic
algorithm reached 84 for 18 of 20; seed 99991 gave 90 and seed 11111 gave 86.
Annealing, with the Metropolis rule and this schedule, reached 84 for 14 of the
20 seeds; the other six gave 86 (seeds 5 and 21), 88 (seeds 13579, 34 and 144)
and 90 (seed 89). The annealing program was additionally run with 13 edited
seeds (the 8 above plus 5, 21, 34, 89 and 144), and every tour equalled the
Python tour. These sweeps are manual and are not part of the test suite: seeds
are compile-time constants of the generated `const fn`s, so the tests cannot
vary them at run time. Only the pinned seed is guaranteed to give the optimum.

Mutation checks, run on the program files with `cargo test -p algorithms`
(default features), one edit at a time. Killed means at least one test failed.

- Annealing (Metropolis rule), killed by the pinned tour test: index scaling
  256 to 128 and to 512, index rounding changed to a floor (dropping
  `temp / 2`), comparison direction reversed, draw shift 16 to 15, temperature
  start 480 to 240, seed 12345 to 54321, iterations 2000 to 5, the reversal
  loop `lo < hi` changed to `lo + 1 < hi`, and the LCG multiplier incremented.
  The seed mutant 54321 changes the pinned tour (to `0 2 9 7 3 5 6 4 8 1`,
  length 84); the parameter test fails too.
- Annealing, killed by the table test: changing table entry 0, 1 or 5 by one.
  These do not change the pinned tour.
- Annealing, caught only by the parameter test (the tour is unchanged, so the
  pinned tour test passes): cutoff `k <= 96` changed to `95` or `94`,
  comparison `<` changed to `<=`, temperature start 480 to 960, schedule
  `2000 - round` changed to `1999 - round`, and iterations 2000 to 1999. The
  complexity prefix test also catches the iteration edit. The Python
  reference gives the same tour for each of these (the pinned run evaluates
  1034 worse candidates, 86 of them with `k > 94`). This is
  observed, not structural.
- Annealing, survivors (no test fails): accepting `delta < 0` instead of
  `delta <= 0` is structurally equivalent, because `delta = 0` gives `k = 0`,
  `table[0] = 32768` and every draw is below it. Raising the temperature floor
  from 1 to 2 is equivalent: with floor 1 the clamp fires in rounds 1996 to
  1999 (where `temp` computes to 0), with floor 2 it fires in rounds 1992 to
  1999 (rounds 1992 to 1995 compute 1). In all of those rounds `temp <= 2`, so
  any worse move (`delta >= 1`) has `k >= 128 > 96` and is rejected either
  way. Changing the best-tour update from `<` to `<=` leaves the tour unchanged
  in the Python reference for this seed (observed, not structural).
- Genetic, killed by the pinned tour test: generations 150 to 1, mutation rate
  20 to 0 and to 21, elitism copying individual 0, tournament `<=` changed to
  `<`, the OX fill start position, the OX donor index, seed 987654321 to
  987654322, and the LCG multiplier. Generations 150 to 149 is caught only by
  the parameter test and the complexity prefix test.
- Ant colony, killed by the pinned tour test: iterations 200 to 1, evaporation
  90 to 100 percent, heuristic weight removed, no deposit, best update `<`
  changed to `<=`, roulette shift 8 to 7, and seed 24680 to 24681. Iterations
  200 to 199 is caught only by the parameter test and the complexity prefix
  test.
- Every city coordinate edit (the second x of each program from 4 to 5) is
  killed by `programs_embed_the_shared_cities` and by the pinned tour test.
- Inserting a second `fold` stage after the `map` stops the build in the
  genetic program (error E0308: the generated step call no longer type
  checks). A type-correct extra stage, such as an identity `map` appended to the
  annealing program, builds and is killed by the `tsp_simulated_annealing`
  complexity test (exact time changes from O(2) to O(3)).
- Equivalent survivors, for the ant colony: the pheromone floor. Removing it,
  or raising it from 10 to 11, changes no test result. For the pinned seed
  this is structural: all 1600 clamps are on the diagonal cells `s[u * 10 + u]`,
  which `next_city` never reads (the current city is always visited), and the
  smallest off-diagonal pheromone read at the start of any iteration is 16,
  above both floors. For other seeds the floor does bind off-diagonal (13 of
  the 21 seeds tried, with a smallest read value of 10), yet the Python
  reference produced identical tours with the floor at 10, at 11 and removed
  for all 21 seeds (the 20 sweep seeds and 24680); that is observed, not
  structural.
- The complexity tests do not see mutations inside step functions, which the
  analyzer does not inspect. They do catch range edits, as listed above.

### tsp_nearest_neighbour

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: the Wikipedia article [Nearest neighbour algorithm][nn-wiki]. The
  Wikipedia article [Travelling salesman problem][tsp-wiki] says that
  Rosenkrantz et al. showed the factor is Theta(log |V|) for instances that
  satisfy the triangle inequality, citing Rosenkrantz, Stearns and Lewis,
  "Approximate algorithms for the traveling salesperson problem", 15th Annual
  Symposium on Switching and Automata Theory, 1974
  ([doi:10.1109/SWAT.1974.4][nn-swat]; the page numbers 33-42 come from
  Crossref, not from Wikipedia), and lists their "An Analysis of Several
  Heuristics for the Traveling Salesman Problem", SIAM Journal on Computing,
  1977 ([doi:10.1137/0206041][nn-siam]). The TSP article gives issue 5 for
  that paper; Crossref gives volume 6, issue 3, pages 563-581, September 1977,
  and this README follows Crossref. The nearest-neighbour article itself does
  not mention that bound.
- Canonical complexity: Theta(N^2) time (the Wikipedia infobox)
- Analyzer estimate: O(1), exact O(2), exact space O(2), cardinality 1, does
  not collect input

The program is `range(0, 9).fold(start, extend_tour).map(close_tour)`. The
state travels through the fold in one `[i64; 13]` array (current city,
visited bitmask, length so far, tour). Each fold step appends exactly one
city: the nearest unvisited one. The scan over the 10 cities is a `while` loop
inside `extend_tour`, because generated functions are `const fn`. The nine
steps are a constant, so the analyzer reports O(1) by construction, not the
canonical N^2 ([#429](https://github.com/DominicBurkart/marigold/issues/429),
[#416](https://github.com/DominicBurkart/marigold/issues/416),
[#415](https://github.com/DominicBurkart/marigold/issues/415),
[#435](https://github.com/DominicBurkart/marigold/issues/435)).

Determinism. The start city is city 0, fixed by the initial state
`[0, 1, 0, ...]` (current city 0, visited mask 1). Ties are broken by the
lowest city index, because the scan runs in index order and replaces the best
candidate only on a strictly smaller distance (`if d < best_d`). The pinned
instance has exactly one tie, at the first step: cities 1 and 2 are both at
distance 11 from city 0, and the program picks city 1
(`the_first_step_is_a_tie_that_the_lowest_index_rule_resolves`,
`matches_reference_nearest_neighbour_with_lowest_index_tie_break`).

Result: tour `0 1 8 4 6 5 3 7 2 9`, length 90, against the optimum 84 (ratio
1.071). Both values come from an independent Python re-implementation
(`matches_independent_python_reference`), and the Rust test also compares the
tour with a second Rust implementation. The output array holds the tour in
elements 0..10 and the closed length in element 10. No test asserts the
Theta(log |V|) factor; the instance is a metric (`distances_are_a_metric` in
`src/tsp.rs`), and the length is only asserted to be at least the optimum.

### tsp_christofides

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: Nicos Christofides, "Worst-case analysis of a new heuristic for
  the travelling salesman problem", Report 388, Graduate School of Industrial
  Administration, CMU, 1976, as cited by the Wikipedia article
  [Christofides algorithm][chr-wiki]
  ([the report][chr-report], the link the article gives; the Internet
  Archive copy of 21 July 2019 was fetched and is an 11-page PDF with this
  title, dated February 1976, numbered 388)
- Canonical complexity: O(n^3) with a polynomial-time minimum-weight perfect
  matching; the tour is within a factor of 3/2 of optimal on metric instances
  (the Wikipedia article)
- Analyzer estimate: O(1), exact O(5), exact space O(5), cardinality 1, does
  not collect input

What is canonical. The program does the steps the Wikipedia article lists, in
order: a minimum spanning tree (Prim, one vertex per fold step), the set of
odd-degree vertices, a minimum-weight perfect matching on those vertices,
the union of tree and matching as an Eulerian multigraph, an Eulerian circuit
(Hierholzer's algorithm, lowest-index neighbour first), and shortcutting the
repeated vertices. The program is
`range(0, 9).fold(mst_start, prim_step)` followed by
`.map(add_matching).map(euler_circuit).map(shortcut).map(finish)`. The fold
state is one `[i64; 146]` array (the 10 x 10 multigraph, the visited mask,
weights and the circuit). The fold steps are algorithmic steps; each of the
four maps is a whole phase of the algorithm, not a step.

What is simplified. The matching is brute force: `match_cost` recurses over
bitmasks of odd vertices, pairing the lowest vertex with every other one. It
is exponential in the number of odd vertices. That is acceptable only at
N = 10 (at most 945 matchings for 10 odd vertices, here 105 for 8), and a
polynomial implementation would use Edmonds' blossom algorithm. Among
equal-weight matchings it keeps the pair with the lowest partner index.

Results on the pinned instance, from an independent Python re-implementation
that follows the same tie-breaks (`matches_independent_python_reference`):

- MST weight 61. Kruskal's algorithm gives 61 too
  (`mst_weight_matches_kruskal`, a Rust Kruskal) and so does the Python Prim.
- Odd-degree vertices `{0, 2, 3, 5, 6, 7, 8, 9}`, reported as the bitmask 1005
  in output element 12.
- Matching weight 39. The test enumerates all 105 perfect matchings of those
  eight vertices and checks that 39 is their minimum and that some matching is
  heavier (`matching_weight_is_the_minimum_over_every_perfect_matching_of_the_odd_vertices`);
  the Python enumeration also gives 39.
- Tour `0 2 7 9 3 5 8 6 4 1`, length 90. The 3/2 guarantee on this instance is
  90 <= 126 = 1.5 * 84, with ratio 1.071
  (`tour_is_within_three_halves_of_the_optimum_and_not_below_it`). The tour is
  also no longer than MST + matching = 100.

The output array holds the tour (elements 0..10), the MST weight, the
matching weight and the odd mask.

### tsp_lkh

**This is not LKH-3.** It is a simplified Lin-Kernighan-style local search:
the file name keeps the issue's label.

- Issue: [#406](https://github.com/DominicBurkart/marigold/issues/406)
- Reference: Shen Lin and Brian W. Kernighan, "An Effective Heuristic
  Algorithm for the Traveling-Salesman Problem", Operations Research
  21(2):498-516, 1973 ([doi:10.1287/opre.21.2.498][lk-doi]), and the Wikipedia
  article [Lin-Kernighan heuristic][lk-wiki]; Keld Helsgaun, "An effective
  implementation of the Lin-Kernighan traveling salesman heuristic", European
  Journal of Operational Research 126(1):106-130, 2000
  ([doi:10.1016/S0377-2217(99)00284-2][lkh-doi]), as the Wikipedia article
  cites it; and Keld Helsgaun, "An Extension of the Lin-Kernighan-Helsgaun TSP
  Solver for Constrained Traveling Salesman and Vehicle Routing Problems",
  Technical Report, Roskilde University, 2017, as named on the
  [LKH-3 page][lkh3-page]
- Canonical complexity: heuristic local search with no polynomial worst-case
  bound; per pass, a 2-opt neighbourhood scan is O(n^2)
- Analyzer estimate: O(1), exact O(2), exact space O(2), cardinality 1, does
  not collect input

What it implements: the program is
`range(0, 30).fold(seeded_start, improve_once).map(with_length)`. The start
tour is a Fisher-Yates shuffle of the cities 1..9 driven by an LCG with seed
982 (city 0 stays in position 0). Each fold step is one best-improvement pass:
it evaluates every 2-opt reversal of positions `i..=j` (`1 <= i < j <= 9`)
and every Or-opt move (a segment of 1, 2 or 3 cities moved to any other gap,
in either orientation), scans them in a fixed order, and applies the one with
the strictly shortest resulting tour, or leaves the tour unchanged at a local
optimum. Thirty rounds is a cap; the pinned run makes 5 improving steps and is
at a fixed point afterwards.

What it lacks, compared with the Lin-Kernighan heuristic and with LKH-3. There
is no variable-depth alternating-trail search (the depth-limited step of
Lin-Kernighan), so the moves are only the fixed 2-opt and Or-opt
neighbourhoods. Compared with the LKH pages: no 5-opt sequential basic move
(LKH-1) or k-opt sub-moves (LKH-2), no alpha-nearness candidate sets, no
trials or restarts, and none of the constraint handling that LKH-3 adds for
constrained TSP and vehicle routing problems. The quality shown here says
nothing about LKH-3. The feature list comes from the LKH and LKH-3 pages as
archived by the Internet Archive; the host itself did not answer from the
machine used for this work.

Result: start tour `0 7 5 6 8 2 3 9 1 4` (length 124) improves to
`0 2 7 9 5 3 8 4 6 1`, length 84, which equals the optimum for this seed. Both
tours and the five improving steps come from an independent Python
re-implementation (`seeded_start_matches_independent_python_reference`,
`matches_independent_python_reference`); the Rust tests also run a second
Rust best-improvement search and compare the final tour and the number of
improving steps
(`matches_reference_best_improvement_search_and_converges_before_the_round_cap`)
and check exhaustively that no 2-opt or Or-opt move improves the result
(`result_is_a_two_opt_and_or_opt_local_optimum`).

Seed choice, stated plainly. The seed was chosen by a Python sweep over the
seeds 1 to 1499, to make the mutation tests below sensitive. The history:

- Seed 12345 (first version). Left two mutants alive: an Or-opt gap range one
  position short, and Or-opt without the reversed orientation.
- Seed 224 (commit 4461ed8). Killed those two but left alive the 2-opt `<=`
  mutant and the 2-opt range `j` one short.
- Seed 624 (commit 76d0b06). Killed all five move-class mutants named below, but
  it also left alive removing the start shuffle's swap (the identity tour
  reaches the same final tour in 4 steps) and the 2-opt segment limit of 2
  cities. An earlier version of this README listed the swap removal as killed;
  that was true at seed 224 and false at seed 624.
- Seed 982 (this version). Pinned.

The five move-class mutants are: removing 2-opt, removing Or-opt, removing the
reversed Or-opt orientation, shortening the Or-opt gap range by one, and
shortening the 2-opt range `j` by one. Seeds 624 and 982 are the only two of
the 1499 swept seeds for which all five change the final tour. Seed 982 was
preferred over 624 because, on the program file, it also kills the swap-line
removal mutants and the segment limit of 2 cities, which seed 624 leaves alive.
With seed 982 two
mutants still survive behaviourally (see the mutation list): limiting Or-opt
segments to 1 city, and `range(0, 5)`.

The sweep was re-run to check the figures. Over the seeds 1 to 1499 the local
search ends at length 84 for 1383 seeds and at 90 for 116; over the seeds 1 to
399 it ends at 84 for 373 and at 90 for 26. The pinned result (length 84, the
optimum) is therefore not evidence about the algorithm's typical quality: the
seed was chosen for mutation sensitivity, and a length of 84 is what most
seeds give on this one small instance, which says little about larger
instances or about LKH-3.

### Constructive and local-search TSP programs: shared notes

Spine. Each program is a fold over a constant `range` whose step is one
algorithmic step (a city for nearest neighbour, a vertex for Prim, an
improvement pass for the local search), followed by one or more maps. The
state travels through the fold in one fixed array and the loops over cities
are `while` loops inside the step functions. None of these programs uses
`keep_first_n`. The stream therefore carries only the iteration structure.
The step counts are constants, so the analyzer reports O(1) by construction
and cannot see the loops in the step functions.

Analyzer pins. `tests/complexity.rs` pins, for each program: time class O(1),
the exact time (O(2), O(5), O(2)), the space class, the exact space (the same
values), cardinality 1, and that the input is not collected. Each
`<name>_canonical_class_is_not_representable` test reads the program file,
pins the cardinality of the range (9, 9 and 30) with `analyze_prefix(name,
None)` and of the fold (1) with `Some("fold")`, and asserts that the class
differs from the canonical O(n^2), O(n^3) and O(n^2) polynomial classes (the
final `assert_ne!` cannot fail while the sibling test pins O(1)).

Seeds and constants. Only the local search uses a random number generator. The
generator, seed and every constant are in the program text, and the results do
not depend on the runtime feature (`tokio`, `async-std` or none).
`program_embeds_*` in each module asserts the seed, the stage text and the
tie-break lines that the Python references rely on. The coordinates are
`[i32; 10]` arrays, and `programs_embed_the_shared_cities` in `src/tsp.rs`
parses them from all three program files and compares them with
`tsp::CITIES`.

Benches. `benches/algorithms_bench.rs` runs the three programs in the group
`tsp_constructive`, as `tsp_constructive/tsp_nearest_neighbour`,
`tsp_constructive/tsp_christofides` and `tsp_constructive/tsp_lkh`.

Mutation checks, run on the program files with `cargo test -p algorithms
--no-fail-fast` (default features), one edit at a time. Killed means at least
one test failed.

- Nearest neighbour, all killed by the pinned tour test
  (`matches_independent_python_reference`): tie-break `<` changed to `<=`,
  farthest city instead of nearest, `range(0, 9)` changed to `range(0, 8)` and
  to `range(0, 10)`, the initial visited mask 1 changed to 0, the closing edge
  dropped from the reported length, and city 1's x changed from 4 to 5. The
  tie-break and farthest-city mutants are also killed by the Rust reference and
  by the tie test, the coordinate mutant by `programs_embed_the_shared_cities`,
  and the range mutants by the complexity prefix test.
- Christofides, all killed: greedy first-pair matching (by the pinned values
  and the all-matchings test), maximum-weight matching, the odd test inverted,
  the matching weight accumulation removed, the shortcut keeping repeats
  (invalid tour), Prim picking the farthest edge, `range(0, 9)` changed to
  `range(0, 8)`, the `add_matching`, `euler_circuit` or `shortcut` stage
  removed, the reported MST weight forced to 0, and city 9's y changed. The
  Prim tie-break `<` changed to `<=` is killed by the pinned values and the
  test `program_embeds_the_prim_step_count_and_tie_breaks`. The Euler circuit
  taking the last neighbour instead of the lowest is killed only by the pinned
  Python tour: the tour stays valid and within 3/2, so the pin is what catches
  it.
- Local search at seed 982, killed by the pinned Python tour and by the Rust
  best-improvement reference (`matches_independent_python_reference` and
  `matches_reference_best_improvement_search_and_converges_before_the_round_cap`):
  Or-opt removed, 2-opt removed, Or-opt reversal removed, Or-opt gap range one
  short, 2-opt range `j` one short, Or-opt segments limited to 2 cities,
  seed 982 changed to 983, `range(0, 30)` changed to `range(0, 4)`, the 2-opt
  acceptance `<` changed to `<=`, city 1's y changed from 7 to 6 (also by
  `programs_embed_the_shared_cities`), and four edits to the start shuffle:
  the whole swap (three lines) removed, so the search starts from the identity
  tour and ends at `0 1 6 4 8 3 5 9 7 2` instead of the pinned tour; the line
  `t[j] = tmp;` replaced by `t[j] = t[j];`; the line `let tmp = t[i];`
  replaced by `let tmp = t[j];`; and the line `t[i] = t[j];` replaced by
  `t[i] = t[i];`. The last three produce a non-permutation start, so they are
  also killed by `returns_every_city_exactly_once`.
- The `<=` mutant works through a plateau that is structural: at a local
  optimum the whole-tour reversal of positions 1 to 9 has exactly the same
  length, so with `<=` it is accepted every round and the tour flips
  orientation each round. Whether the output changes depends on the parity of
  the rounds left after the fifth improving step; at seed 982 there are 25
  (odd) and the output changes. At seed 224, with an even number left, the
  mutant survived. This parity effect is observed, not structural.
- Local search, not killed behaviourally (only text and analyzer checks fail),
  observed rather than structural: `range(0, 30)` changed to `range(0, 5)`
  produces the same tour because five rounds reach the fixed point; it is
  caught by the complexity prefix test (cardinality 30) and by the stage text
  in `program_embeds_the_seed_round_cap_and_move_neighbourhood`. Limiting
  Or-opt segments to 1 city (`while len <= 3` changed to `<= 1`) also gives the
  same tour at seed 982, and only the text guard fails. A Python
  re-implementation agrees on both. The swap lines also have a text guard in
  `program_embeds_the_seed_round_cap_and_move_neighbourhood`, which is a
  second line of defence, not the one that kills the swap mutants.
- Compile error, not a test kill: removing the `with_length` map leaves the
  output type mismatched (`E0308`).
- The complexity tests do not see mutations inside step functions, which the
  analyzer does not inspect. They do catch range edits.

[tsp-exact]: https://en.wikipedia.org/wiki/Travelling_salesman_problem#Exact_algorithms
[held-karp]: https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
[binary-search]: https://en.wikipedia.org/wiki/Binary_search_algorithm
[quicksort]: https://en.wikipedia.org/wiki/Quicksort
[sa-doi]: https://doi.org/10.1126/science.220.4598.671
[sa-wiki]: https://en.wikipedia.org/wiki/Simulated_annealing
[ga-doi]: https://doi.org/10.7551/mitpress/1090.001.0001
[ga-wiki]: https://en.wikipedia.org/wiki/Genetic_algorithm
[ox-wiki]: https://en.wikipedia.org/wiki/Crossover_(evolutionary_algorithm)
[aco-doi]: https://doi.org/10.1109/3477.484436
[aco-wiki]: https://en.wikipedia.org/wiki/Ant_colony_optimization_algorithms
[nn-wiki]: https://en.wikipedia.org/wiki/Nearest_neighbour_algorithm
[tsp-wiki]: https://en.wikipedia.org/wiki/Travelling_salesman_problem
[nn-swat]: https://doi.org/10.1109/SWAT.1974.4
[nn-siam]: https://doi.org/10.1137/0206041
[chr-wiki]: https://en.wikipedia.org/wiki/Christofides_algorithm
[chr-report]: https://apps.dtic.mil/dtic/tr/fulltext/u2/a025602.pdf
[lk-wiki]: https://en.wikipedia.org/wiki/Lin%E2%80%93Kernighan_heuristic
[lk-doi]: https://doi.org/10.1287/opre.21.2.498
[lkh-doi]: https://doi.org/10.1016/S0377-2217(99)00284-2
[lkh3-page]: http://webhotel4.ruc.dk/~keld/research/LKH-3/
