# color-palette-picker

Two ways to pick a maximally contrasting three-colour palette, both written as
Marigold stream programs and scored by the same integer LMS contrast function
(minimum pairwise contrast across normal vision, deutan, protan, tritan and
achromat).

## Brute force

`examples/color-palette-picker.marigold`:
`range(0, 256).filter(mod_fifty_one).permutations_with_replacement(3)`
`.combinations(3).keep_first_n(5, compare_contrast)`.
Canonical work: 6 channel values, 216 colours, C(216,3) = 1,656,360 palettes
scored.

## Geometric (colour wheel)

`examples/color-palette-picker-geometric.marigold`, run by
`color_palette_picker::geometric_palettes()`, by `main`, by the tests in
`src/main.rs` and by the `bench_geometric_picker` bench.

The wheel has 12 hues (30 degree steps) and 4 brightness rings per hue (pastel
toward the white centre, pure, two-thirds, one-third toward black): 48 wheel
points. A candidate is an equilateral polygon: vertices at hues `r`, `r + 4`,
`r + 8` (120 degrees apart), each with its own ring, so brightness varies
between vertices (irregular in brightness, regular in hue). `r` ranges over
the 4 distinct rotations. The search is

```text
range(0, 4).permutations_with_replacement(4).keep_first_n(5, compare_polygons).map(to_palette).return
```

Each item `[r, ring0, ring1, ring2]` is one polygon: 4 x 4^3 = 4^4 = 256
candidates instead of 1,656,360.

Simplifications: the vision types are not separate wheels but separate
contrast metrics over the same wheel, minimised per pair exactly as in the
brute force; polygons are triangles only; the white-centre and black-rim poles
are not wheel points; only equilateral hue spacing is searched.

### Correctness oracle

Tests in `src/main.rs` check against plain-loop oracles on the same 48-point
discretization:

- the five best geometric scores equal the five best scores of an exhaustive
  loop over all 256 polygons (exact equality);
- every returned palette is a wheel polygon with hues `r`, `r + 4`, `r + 8`;
- the best geometric score is at most the unrestricted optimum over all
  C(48,3) = 17,296 triples (the geometric space is a subset) and at least 80%
  of it. The 80% bound is a measured bound (the measured ratio is 84.1%), not
  a theorem: no guarantee is claimed for other wheels or metrics. Replacing
  the search with an arbitrary palette fails this test and the exact-score
  test. The prisma (f64 CIECAM02) ordering also agrees that the geometric
  winner does not beat the unrestricted optimum;
- the best geometric score is pinned to the literal 384,514,391,494, and every
  LMS and luminance weight in the program text must equal the weights of the
  oracle (which the existing tests validate against prisma ordering).

## Complexity estimation

`marigold-grammar/tests/e2e_geometric_palette.rs` pins the analyzer output for
both programs.

| quantity               | canonical | analyzer             |
| ---------------------- | --------- | -------------------- |
| geometric after pwr(4) | 256       | `Exact(256)`         |
| geometric output       | 5         | `Exact(5)`           |
| geometric time         | 256 evals | `O(1)`, exact `O(3)` |
| geometric space        | 5 kept    | `O(1)`, exact `O(3)` |
| wheel combinations(3)  | 17,296    | `Exact(17296)`       |
| brute-force time       | 1,656,360 | `O(C(n,3))`          |
| brute-force space      | constant  | `O(n^3)`             |

Brute-force exact time is `O(2C(n,3) + n^3)` and exact space is
`O(n^3 + n)`. The analyzer's upper bound on brute-force palettes enumerated
is C(256^3, 3) against the canonical 1,656,360.

Divergences from the canonical count:

- `filter` discards the constant cardinality of its input
  (`Symbolic::Filtered`), so the brute-force example is classed as
  combinatorial although its input is the constant 6 values; the geometric
  program avoids `filter` and stays exact.
- All inputs are constants, so every class collapses to `O(1)`; the exact
  complexity counts stream steps, not candidate evaluations. The 256 versus
  1,656,360 versus 17,296 difference is visible only in the cardinalities, not
  in the time class. See #415 (symbolic input size).
- The cost of `compare_polygons` / `compare_contrast` per item is not
  modelled. See #416 (per-item function cost).
- The analyzer ranks the geometric program below the brute-force example in
  time and space, agreeing with the canonical order, but for the
  filter-related reason above rather than the work saved.
- The pinned program file's own estimate is `Exact(5)` whatever the range
  bound, so changing `range(0, 4)` in the program is caught by the mirror
  guard and the example-crate tests, not by the analyzer.
