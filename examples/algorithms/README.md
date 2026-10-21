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
| `binary_search` | [#412](https://github.com/DominicBurkart/marigold/issues/412) | [Binary search algorithm](https://en.wikipedia.org/wiki/Binary_search_algorithm) | O(log n) per query | O(1), exact O(2): a fold over the constant `range(0, 5)` and a map. 5 = ceil(log2 16) + 1 halving rounds bound the lower-bound search over the 16-element sorted array, so the logarithm sits in the step bound, not the stream shape (symbolic input size is [#415](https://github.com/DominicBurkart/marigold/issues/415); the per-round cost of the fn body is [#416](https://github.com/DominicBurkart/marigold/issues/416)). Each fold round halves the interval of all 8 queries at once (misses, hits, duplicates and both boundaries); the program returns the 8 lower-bound indices and tests compare them with `partition_point` and `binary_search`. |
| `quicksort` | [#413](https://github.com/DominicBurkart/marigold/issues/413) | Hoare, "Quicksort", 1962; [Quicksort](https://en.wikipedia.org/wiki/Quicksort) | O(n log n) average, O(n^2) worst | O(1), exact O(2): a fold over the constant `range(0, 16)` and a map. Each fold step pops one partition from an explicit work-list held in the state array and performs one Lomuto partition, pushing the sub-ranges of size >= 2 (at most n steps, because every step fixes a pivot). Lomuto is used instead of Hoare's two-pointer scheme for simplicity; the last element is the pivot, so already sorted input is the O(n^2) case. The n log n vs n^2 behaviour lives in the fn body and is invisible to the analyzer ([#415](https://github.com/DominicBurkart/marigold/issues/415), [#416](https://github.com/DominicBurkart/marigold/issues/416)). Input has duplicates and tests compare against `slice::sort`. |
