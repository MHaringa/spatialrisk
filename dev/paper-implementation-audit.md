# Paper-implementation audit

## A. Exact matches

- `concentration_hotspot(method = "continuous")` follows the documented
  `prepare_spatialrisk()` -> `select_candidates()` -> `optimize_hotspot()`
  workflow. Input coordinates are interpreted as EPSG:4326 and projected to
  the selected metric CRS before raster or Euclidean calculations.
- `initialise_terra_hotspot_state()` aggregates weights with `terra::rasterize`
  and stores the one-time `terra::cellFromXY()` point-to-cell mapping. The
  raster margin is at least the requested radius, so observed and pair-derived
  centres are contained by the screening raster.
- `mw_create()` implements the Lemma 1 mask on raster-cell centres:
  `distance <= radius + full cell diagonal`. The resulting focal values are
  conservative upper bounds for non-negative weights, not reported hotspot
  values.
- The automatic feasible lower bound uses five focal seed cells and a 10 by 10
  trial grid in each cell. All 500 evaluations are scored against the complete
  active portfolio in projected Euclidean geometry. The initial bound selects
  cells once per greedy iteration; later exact evaluations update only the
  incumbent.
- Candidate-generation points are retrieved from the stored raster mapping,
  but Rcpp scoring uses the complete active portfolio. A dedicated regression
  test covers a contributing point outside the generation subset.
- The continuous Rcpp sweep includes observed candidates and visits the angular
  endpoints corresponding to valid distinct pairs at distance at most twice
  the radius. Coincident records contribute jointly without generating a pair;
  tangent pairs, repeated derived centres, and equal maxima are covered by
  tests.
- Direct `optimize_hotspot(prepare_spatialrisk(...))` uses the full observed and
  pair-intersection candidate set and cannot invoke grid fallback. The screened
  route records each realised refinement in `attr(result,
  "refinement_methods")`.
- The manuscript validation and Berlin benchmark scripts use the current API,
  documented seeds and sizes, projected point scoring, and
  `max_refinement_points = 2500`. Stored benchmark runs report pair refinement,
  not fallback, for every continuous run.

## B. Manuscript changes

- Described the lower-bound step as 500 centre evaluations rather than 500
  necessarily unique centres, because adjacent seed cells can share trial
  coordinates on their edges.
- Rephrased the role of observed locations as an explicit component of the
  classical finite candidate set, without adding a separate claim about every
  degenerate configuration.
- Added the implemented relative `1e-10` objective-comparison guard alongside
  the documented squared-distance and angular-event tolerances.
- Made the actuarial contribution explicit as an auditable deterministic
  continuous-centre workflow with conditional single-hotspot optimality, and
  removed repeated caveats about novelty, cross-paper runtime, and universal
  speed rankings.
- Added an explicit note that cells satisfying `U(C) == L` are retained, and
  refreshed the validation timing medians after rerunning all 100 cases.

## C. Code changes

- Replaced the temporary `terra::distance()` moving-window construction with
  the direct Euclidean cell-centre formula. This makes rectangular-cell
  behaviour exactly match Lemma 1; the package's normal square-cell masks are
  unchanged.
- Named the internal lower-bound constants (five seed cells and ten trials per
  axis) and clarified that focal values are screening bounds.
- Centralised distance, angular-event, and objective-comparison tolerances in
  the Rcpp source and documented the angular-sweep invariant.
- Added focused tests for rectangular masks, random screened/full equivalence,
  unsafe user-threshold diagnostics, tangent pairs, coincident points,
  repeated pair-derived centres, and equal maxima. The final adversarial pass
  additionally covers one-point portfolios, pairs below/at/above twice the
  radius, zero and highly unequal weights, exact disk-boundary inclusion,
  large coordinate magnitudes, strict threshold equality, raster boundaries,
  and all documented cell sizes.
- Clarified in the public documentation that hotspot input coordinates are
  expected in EPSG:4326 and are projected internally.
- Final verification again produced 100/100 validation objective recovery,
  zero maximum relative gap, and no validation grid fallbacks. The stored
  Berlin results contain all 45 size/seed/method runs; all 15 continuous runs
  report pair-intersection refinement, and a fresh 50,000-record seed-1 run
  reproduced its stored objective and contributing-record count.

## D. Remaining conditional assumptions

The single-disk optimality guarantee remains conditional on point-represented
risks, non-negative weights, a fixed radius, Euclidean distance in the chosen
projected metric CRS, a feasible automatic lower bound, the conservative focal
mask, complete surviving observed/pair geometry, scoring against the complete
active portfolio, and pair refinement without grid fallback. It does not cover
an unsafe user threshold, negative weights, approximate grid refinement, or
joint optimisation of multiple hotspots. Numerical equality is interpreted
using the documented floating-point tolerances.
