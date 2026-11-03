# color-palette-picker

Two ways to pick a maximally contrasting three-colour palette.

`src/main.rs` runs the original brute force with Rust closures and the prisma
f64 CIECAM02 `compare_contrast` over `range(0, 255)` filtered to multiples
of 51. The two `.marigold` programs
(`examples/color-palette-picker.marigold` and
`examples/color-palette-picker-geometric.marigold`) are scored by the same
integer LMS contrast function: the minimum pairwise contrast across normal
vision, deutan, protan, tritan and achromat.

## Brute force

`examples/color-palette-picker.marigold`:
`range(0, 256).filter(mod_fifty_one).permutations_with_replacement(3)`
`.combinations(3).keep_first_n(5, compare_contrast)`.
Canonical work: 6 channel values, 216 colours, C(216,3) = 1,656,360 palettes
scored. An independent triple loop over those 216 colours (in the tests) gives
the grid optimum 499,574,432,835, reached by (0,0,0), (153,204,0) and
(255,255,255).

## Geometric (colour wheel)

`examples/color-palette-picker-geometric.marigold` ends in
`.write_file("/dev/stdout", csv)` and runs with `marigold run`. The same
program is embedded in `src/lib.rs` ending in `.return`, run by
`color_palette_picker::geometric_palettes()`, by `main`, by the tests in
`src/main.rs` and by the `bench_geometric_picker` bench. A third copy,
`marigold-grammar/tests/programs/geometric_palette.marigold`, is analysed by
the grammar tests. `m!` cannot include a file, so the copies are kept and
guarded: tests require the grammar copy to equal the example file, and the
`lib.rs` copy to equal it except for the output stage.

The wheel has 18 hues (20 degree steps) and 6 rings per hue: the white centre
pole, a pastel (two thirds toward white), the pure hue, two thirds and one
third brightness, and the black rim pole. The poles are the same colour at
every hue. A candidate is an equilateral polygon: vertices at hues `r`,
`r + 6`, `r + 12` (120 degrees apart), each with its own ring, so brightness
varies between vertices. The search is

```text
range(0, 6).permutations_with_replacement(4).keep_first_n(5, compare_polygons).map(to_palette)
```

Each item `[r, ring0, ring1, ring2]` is one polygon: 6 rotations x 6^3 rings
= 6^4 = 1,296 candidates (polygons with pole vertices repeat colours)
instead of 1,656,360.

Limits relative to issue #414, stated plainly:

- The five vision models are not separate wheels that are compared or
  summed. They are combined as a per-pair minimum of five contrast metrics on
  one wheel.
- Only triangles are searched, and only equilateral hue spacing.
- `range(0, 6)` is reused for the rotation and for the three rings, so the
  rotation count and ring count are coupled (both 6).

### Correctness oracle

Tests in `src/main.rs` and `src/lib.rs` check against plain-loop oracles on the same wheel:

- the five best geometric scores equal the five best scores of an exhaustive
  loop over all 1,296 polygons (exact equality), and the program enumerates
  exactly the oracle's polygons;
- the best geometric score, 499,851,629,344, is at least the brute-force
  grid optimum 499,574,432,835 (100.06% of it) and equals the optimum over
  all C(74,3) triples of the 74 distinct wheel colours. Both relations are
  measured on this wheel, not theorems: the wheel channel values are not the
  51-step grid, and no guarantee is claimed for other wheels or metrics;
- the program scores three fixed palettes through its own functions:
  (0,0,0), (255,255,255), (153,204,0) as 499,574,432,835; a deutan-bound
  palette as 405,408,682,152 (427,676,629,344 if deutan were dropped); a
  tritan-bound palette as 105,671,398,184 (116,983,101,218 if tritan were
  dropped). The values were computed with an independent script and fail a
  test if an LMS or luminance weight, or the deutan or tritan model, is
  changed.

## Complexity estimation

`marigold-grammar/tests/e2e_geometric_palette.rs` pins the analyzer output for
both programs. The stage prefixes are cut from the program file itself.

| quantity                       | canonical | analyzer             |
| ------------------------------ | --------- | -------------------- |
| geometric `range(0, 6)`        | 6         | `Exact(6)`           |
| geometric after pwr(4)         | 1,296     | `Exact(1296)`        |
| geometric output               | 5         | `Exact(5)`           |
| geometric time                 | 1,296     | `O(1)`, exact `O(3)` |
| geometric space                | 5 kept    | `O(1)`, exact `O(3)` |
| `range(0, 74).combinations(3)` | 64,824    | `Exact(64824)`       |
| brute-force time               | 1,656,360 | `O(C(n,3))`          |
| brute-force space              | constant  | `O(n^3)`             |

Brute-force exact time is `O(2C(n,3) + n^3)` and exact space is
`O(n^3 + n)`. The analyzer's upper bound on brute-force palettes enumerated
is C(256^3, 3) against the canonical 1,656,360.

Divergences from the canonical count:

- `filter` discards the constant cardinality of its input
  (`Symbolic::Filtered`), so the brute-force example is classed as
  combinatorial although its input is the constant 6 values; the geometric
  program avoids `filter` and stays exact.
- All inputs are constants, so every class collapses to `O(1)`; the exact
  complexity counts stream steps, not candidate evaluations. The 1,296 versus
  1,656,360 versus 64,824 difference is visible only in the cardinalities, not
  in the time class. See #415 (symbolic input size).
- The cost of `compare_polygons` / `compare_contrast` per item is not
  modelled. See #416 (per-item function cost).
- The analyzer ranks the geometric program below the brute-force example in
  time and space, agreeing with the canonical order, but for the
  filter-related reason above rather than the work saved.
- The final `Exact(5)` of the program file hides the range bound and the
  permutation kind, so the tests analyse each stage prefix cut from the file
  (6, then 1,296, then 5) and a change to `range(0, 6)` or
  `permutations_with_replacement(4)` fails a cardinality test.
