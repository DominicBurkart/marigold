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
| `binary_search` | [#412](https://github.com/DominicBurkart/marigold/issues/412) | [Binary search algorithm](https://en.wikipedia.org/wiki/Binary_search_algorithm) | O(log n) per query | O(1), exact O(2): a fold over the constant `range(0, 5)` and a map. This is a fixed-iteration program, O(1) by construction, not a demonstration of O(log n): the 5 rounds are sized for the worst case on 16 elements, and the analyzer cannot see data-dependent early exit ([#429](https://github.com/DominicBurkart/marigold/issues/429)); symbolic input size is [#415](https://github.com/DominicBurkart/marigold/issues/415) and per-item fn cost is [#416](https://github.com/DominicBurkart/marigold/issues/416). See the Binary search section. |
| `quicksort` | [#413](https://github.com/DominicBurkart/marigold/issues/413) | Hoare, "Quicksort", 1962; [Quicksort](https://en.wikipedia.org/wiki/Quicksort) | O(n log n) average, O(n^2) worst | O(1), exact O(2): a fold over the constant `range(0, 16)` and a map. Fixed-iteration, O(1) by construction: the 16 steps are sized for the O(n^2) worst case on 16 elements, and the partition work lives in the fn body, invisible to the analyzer ([#415](https://github.com/DominicBurkart/marigold/issues/415), [#416](https://github.com/DominicBurkart/marigold/issues/416), [#429](https://github.com/DominicBurkart/marigold/issues/429)). See the Quicksort section. |

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

### Binary search

Each fold round halves the lo/hi interval of 8 queries at once (misses below
and above the array, hits, duplicate runs); the program returns the 8
lower-bound indices. The interval starts at 17 candidates (0..=16), so 5
rounds are needed in the worst case and the cap is exactly that. Tests compare
every result with `partition_point` and `binary_search`, and assert that a
cap one round short gives a wrong answer for at least one query.

### Quicksort

Each fold step pops one partition from an explicit work-list held in the
state array and performs one Lomuto partition (last element as pivot), pushing
sub-ranges of size >= 2. Lomuto is used instead of Hoare's two-pointer scheme
for simplicity. Every step fixes a pivot, so at most n-1 = 15 partitions occur;
the worst case is already-sorted distinct input (O(n^2) comparisons), and the
program input has duplicates and needs 14 of the 15 possible partitions. Tests
compare with `slice::sort`, assert sorted distinct input needs 15 partitions
(under the cap of 16) and that a cap one step short fails on the program's
input.
