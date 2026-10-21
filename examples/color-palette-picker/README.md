# color-palette-picker

Two ways to pick a maximally contrasting three-colour palette, both written as Marigold stream programs and scored by the same integer LMS contrast function (minimum pairwise contrast across normal vision, deutan, protan, tritan and achromat).

## Brute force

`examples/color-palette-picker.marigold`: `range(0, 256).filter(mod_fifty_one).permutations_with_replacement(3).combinations(3).keep_first_n(5, compare_contrast)`. Canonical work: 6 channel values, 216 colours, C(216,3) = 1,656,360 palettes scored.

## Geometric (colour wheel)

`examples/color-palette-picker-geometric.marigold`, run by `color_palette_picker::geometric_palettes()`, by `main`, by the tests in `src/main.rs` and by the `bench_geometric_picker` bench.

The wheel has 12 hues (30 degree steps) and 4 brightness rings per hue (pastel toward the white centre, pure, two-thirds, one-third toward black), 48 wheel points. A candidate is an equilateral polygon: vertices at hues `r`, `r + 4`, `r + 8` (120 degrees apart), each with its own ring, so brightness varies between vertices (irregular in brightness, regular in hue). `r` ranges over the 4 distinct rotations. The search is

```
range(0, 4).permutations_with_replacement(4).keep_first_n(5, compare_polygons).map(to_palette).return
```

each item `[r, ring0, ring1, ring2]` being one polygon: 4 x 4^3 = 4^4 = 256 candidates instead of 1,656,360.

Simplifications: the vision types are not separate wheels but separate contrast metrics over the same wheel, minimised per pair exactly as in the brute force; polygons are triangles only; the white-centre and black-rim poles are not wheel points; only equilateral hue spacing is searched.

### Correctness oracle

Tests in `src/main.rs` check against plain-loop oracles on the same 48-point discretization:

- the five best geometric scores equal the five best scores of an exhaustive loop over all 256 polygons (exact equality);
- every returned palette is a wheel polygon with hues `r`, `r + 4`, `r + 8`;
- the best geometric score is at most the unrestricted optimum over all C(48,3) = 17,296 triples (the geometric space is a subset) and at least 80% of it. The 80% bound is a measured bound (the measured ratio is 84.1%), not a theorem: no guarantee is claimed for other wheels or metrics. Replacing the search with an arbitrary palette fails this test and the exact-score test, and the prisma (f64 CIECAM02) ordering agrees that the geometric winner does not beat the unrestricted optimum.

## Complexity estimation

`marigold-grammar/tests/e2e_geometric_palette.rs` pins the analyzer output for both programs.

| quantity | canonical | analyzer |
| --- | --- | --- |
| geometric candidates after `permutations_with_replacement(4)` | 256 | `Exact(256)` |
| geometric output | 5 | `Exact(5)` |
| geometric time / exact time | 256 polygon evaluations, each a constant 3-vertex, 5-vision-type pair scan | `O(1)` / `O(3)` (one unit per stream step) |
| geometric space | 5 retained candidates | `O(1)` / `O(3)` |
| unrestricted wheel brute force `range(0, 48).combinations(3)` | 17,296 | `Exact(17296)`, time `O(1)` |
| brute-force example time | 1,656,360 palettes, constant | `O(C(n,3))`, exact `O(2C(n,3) + n^3)` |
| brute-force example space | constant | `O(n^3)`, exact `O(n^3 + n)` |
| brute-force palettes enumerated | 1,656,360 | upper bound C(256^3, 3) |

Divergences from the canonical count:

- `filter` discards the constant cardinality of its input (`Symbolic::Filtered`), so the brute-force example is classed as combinatorial although its input is the constant 6 values; the geometric program avoids `filter` and stays exact.
- All inputs are constants, so every class collapses to `O(1)`; the exact complexity counts stream steps, not candidate evaluations. The 256 versus 1,656,360 versus 17,296 difference is visible only in the cardinalities, not in the time class. See #415 (symbolic input size).
- The cost of `compare_polygons` / `compare_contrast` per item is not modelled. See #416 (per-item function cost).
- The analyzer ranks the geometric program below the brute-force example in time and space, agreeing with the canonical order, but for the filter-related reason above rather than the work saved.
