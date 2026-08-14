# Find fixed-radius concentration hotspots

Finds fixed-radius concentration hotspots in weighted point-level data.
This is a computational building block for weighted circle-placement
problems: given point locations and a fixed radius, find a centre whose
surrounding circle contains a large aggregated value. In insurance
applications, the weights may represent insured values or another
exposure measure. This function is a wrapper around the decomposed
workflow
[`prepare_spatialrisk`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
[`select_candidates`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
and
[`optimize_hotspot`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md).

## Usage

``` r
concentration_hotspot(
  data,
  value,
  n_hotspots = 1,
  radius = 200,
  cell_size = 100,
  grid_spacing = 1,
  max_refinement_points = 1000,
  lon = "lon",
  lat = "lat",
  crs_metric = 3035,
  progress = TRUE,
  method = c("continuous", "observed", "grid"),
  top_n = lifecycle::deprecated(),
  grid_precision = lifecycle::deprecated()
)
```

## Arguments

- data:

  A data.frame containing point-level exposures. Must include columns
  for longitude, latitude, and the value of interest.

- value:

  A string giving the name of the numeric column in `data` to aggregate
  within each radius.

- n_hotspots:

  Positive integer greater or equal to 1. Number of sequential
  non-overlapping hotspots to return. Default is `1`.

- radius:

  Numeric. Radius of the circle in meters. This is typically the
  application-specific radius of interest. Default is `200`.

- cell_size:

  Numeric. Size of the initial screening cells in meters. This is used
  by `method = "continuous"` and `method = "grid"`. Smaller values give
  a finer initial search but increase computation time.
  `method = "observed"` searches observed point locations and does not
  use this value as a search-grid resolution. Default is `100`.

- grid_spacing:

  Numeric. Spacing between candidate grid centres in the units of
  `crs_metric`; for the default metric CRS these units are meters. This
  is used by `method = "grid"` and by `method = "continuous"` only when
  the local subset is larger than `max_refinement_points` and the method
  falls back to grid refinement. It is not used by
  `method = "observed"`. Smaller values evaluate more candidate centres
  and increase computation time. Default is `1`.

- max_refinement_points:

  Positive integer. Maximum number of local points used for
  pair-intersection refinement. If the local subset contains more
  points, `method = "continuous"` automatically falls back to the grid
  refinement used by `method = "grid"`. Default is `1000`.

- lon:

  A string giving the longitude column in `data`. Default is `"lon"`.

- lat:

  A string giving the latitude column in `data`. Default is `"lat"`.

- crs_metric:

  Numeric. EPSG code for a projected CRS with meter units, used for
  distances, buffers, raster cells, and pair-intersection calculations.
  The default `3035` is ETRS89 / LAEA Europe and is a suitable default
  for Europe-wide applications. For other regions, choose a metric CRS
  appropriate to the study area, for example a local UTM zone, `5070`
  for the conterminous United States, or `3577` for Australia. For Asian
  portfolios there is no single universal choice; use a national
  projected CRS or the relevant UTM zone. Default is `3035`.

- progress:

  Logical. Whether to print progress messages for the main hotspot
  search steps. This is useful for larger portfolios and for
  `n_hotspots > 1`. Default is `TRUE`.

- method:

  Hotspot search strategy. `"continuous"` is the default and searches
  for a centre that may lie between observed points. `"observed"`
  searches only observed point locations as candidate centres. `"grid"`
  uses the original grid-refinement workflow.

- top_n:

  Deprecated. Use `n_hotspots` instead.

- grid_precision:

  Deprecated. Use `grid_spacing` instead.

## Value

An object of class `hotspot`. The main components are `hotspots`,
containing the selected centre coordinates and summed values, and
`contributing_points`, containing the points inside the selected hotspot
radii. The summed value column is named from `value`; for example,
`value = "amount"` creates an `amount_sum` column. In
`contributing_points`, `data_row` gives the row number of the
contributing point in the original input data.

## Details

The default `method = "continuous"` first uses terra rasterisation and
focal sums to identify candidate areas above an automatically estimated
lower bound. It then refines all retained areas using observed local
points and the circle centres implied by local point pairs. Local
refinement subsets are retrieved from the raster cells that can affect
each candidate area, using a conservative margin based on `radius` and
`cell_size`. If more than `max_refinement_points` local points are
involved, it falls back to the grid refinement used by
`method = "grid"`. In that fallback case, `grid_spacing` controls the
local refinement grid; otherwise the pair-intersection step does not use
`grid_spacing`. The focal window includes the radius plus a raster-cell
diagonal. For non-negative values this makes the focal sum an upper
bound for exact centres in that cell. With the default automatic lower
bound, exact scoring is consequently restricted to observed or
pair-intersection centres whose own raster cell passed screening; the
two centres generated by a point pair are tested separately. Each
retained centre is nevertheless scored against the complete active
portfolio, not only the points used to generate it. This centre-level
pruning is disabled for a user-supplied threshold or negative values.
The pair-refined result is exact within the retained candidate areas
under these assumptions. The `"observed"` method is fast and
deterministic, but can miss a larger hotspot when the optimal centre
lies between observed points. The `"grid"` method uses a grid-based
search with local refinement; smaller `grid_spacing` values generally
increase search resolution and computation time. Use
[`prepare_spatialrisk`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
[`select_candidates`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md),
and
[`optimize_hotspot`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
when these steps need to be run or inspected separately. The high-level
function always uses the screened production workflow.

The underlying continuous hotspot problem can be viewed as a
fixed-radius weighted circle placement problem. For point observations
with non-negative values in a projected metric coordinate system,
candidate centres formed by observed point locations and by
intersections of radius-\`r\` circles around pairs of observations are
sufficient to characterise the first single-circle optimum. The
practical `method = "continuous"` implementation uses spatial screening
and local refinement, so its result should be interpreted according to
the selected search settings described above.

Calling
[`optimize_hotspot()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
directly on an object returned by
[`prepare_spatialrisk()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
instead performs the complete geometric candidate search over the full
active portfolio. Calling it after
[`select_candidates()`](https://mharinga.github.io/spatialrisk/reference/prepare_spatialrisk.md)
restricts candidate generation to the screened search state. In both
routes, every retained centre is scored against the complete active
portfolio. The direct full route does not use grid fallback and is
principally intended for small validation or benchmark problems.

For `n_hotspots > 1`, hotspots are selected greedily: after each hotspot
is found, the covered observations are removed before the next hotspot
is computed. The resulting sequence is not necessarily globally optimal
as a joint multi-circle problem.

## References

Chazelle, B. M. and Lee, D. T. (1986). On a circle placement problem.
Computing, 36(1–2), 1–16.
[doi:10.1007/BF02238188](https://doi.org/10.1007/BF02238188).

## Author

Martin Haringa

## Examples

``` r
portfolio <- Groningen[1:200, c("lon", "lat", "amount")]

hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  n_hotspots = 2,
  cell_size = 100,
  progress = FALSE
)

hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.558472 53.19492       1315
#> 2  2 6.572691 53.21873       1147
head(hotspot$contributing_points)
#>   id data_row      lon      lat amount distance_m amount_sum
#> 1  1       60 6.557329 53.19326    110   200.0000       1315
#> 2  1       66 6.556074 53.19490    728   160.2410       1315
#> 3  1      130 6.557804 53.19414     36    98.2272       1315
#> 4  1      159 6.555898 53.19584    441   200.0000       1315
#> 5  2        1 6.570229 53.21846     24   167.1041       1147
#> 6  2      134 6.570342 53.21826     23   165.2818       1147

observed_hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  method = "observed",
  progress = FALSE
)

rbind(
  continuous = hotspot$hotspots[1, ],
  observed = observed_hotspot$hotspots
)
#>            id      lon      lat amount_sum
#> continuous  1 6.558472 53.19492       1315
#> observed    1 6.556074 53.19490       1205
```
