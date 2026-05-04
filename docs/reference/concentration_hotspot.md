# Identify fixed-radius concentration hotspots

Identifies centre coordinates of fixed-radius circles with high local
concentration. In insurance applications this can be used to find
locations where the total insured value within a prescribed radius is
largest.

## Usage

``` r
concentration_hotspot(
  data,
  value,
  top_n = 1,
  radius = 200,
  cell_size = 100,
  grid_precision = 1,
  max_refinement_points = 1000,
  lon = "lon",
  lat = "lat",
  crs_metric = 3035,
  progress = TRUE,
  method = c("continuous", "grid", "observed")
)
```

## Arguments

- data:

  A data.frame containing point-level exposures. Must include columns
  for longitude, latitude, and the value of interest.

- value:

  A string giving the name of the numeric column in `data` to aggregate
  within each radius.

- top_n:

  Positive integer greater or equal to 1. Specifies how many
  non-overlapping hotspots are returned. Default is `1`.

- radius:

  Numeric. Radius of the circle in meters. This is typically the
  regulatory or scenario radius. Default is `200`.

- cell_size:

  Numeric. Size of the initial screening cells in meters. This is used
  by `method = "continuous"` and `method = "grid"`. Smaller values give
  a finer initial search but increase computation time.
  `method = "observed"` searches observed point locations and does not
  use this value as a search-grid resolution. Default is `100`.

- grid_precision:

  Numeric. Approximate spacing in meters used for grid-based refinement.
  This is used by `method = "grid"` and by `method = "continuous"` only
  when the local subset is larger than `max_refinement_points` and the
  method falls back to grid refinement. It is not used by
  `method = "observed"`. Smaller values evaluate more candidate centres
  and increase search precision. Default is `1`.

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
  `top_n > 1`. Default is `TRUE`.

- method:

  Hotspot search strategy. `"continuous"` is the default and searches
  for a centre that may lie between observed points. `"observed"`
  searches only observed point locations as candidate centres. `"grid"`
  uses the original grid-refinement workflow.

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
focal sums to locate an approximate hotspot area. It then refines the
local result by evaluating observed local points and the circle centres
implied by local point pairs. The local refinement subset is chosen
automatically around the terra-selected approximate centre, using a
conservative margin based on `radius` and `cell_size`. If more than
`max_refinement_points` local points are involved, it falls back to the
grid refinement used by `method = "grid"`. In that fallback case,
`grid_precision` controls the local refinement grid; otherwise the
pair-intersection step does not use `grid_precision`. The pair-refined
result is exact only within the terra-selected local search area. The
`"observed"` method is fast and deterministic, but can miss a larger
hotspot when the optimal centre lies between observed points. The
`"grid"` method uses a grid-based search with local refinement; smaller
`grid_precision` values generally increase precision and computation
time.

## Author

Martin Haringa

## Examples

``` r
portfolio <- Groningen[1:200, c("lon", "lat", "amount")]

hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE,
  top_n = 2
)

hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.554816 53.19424       1315
#> 2  2 6.572691 53.21873       1147
head(hotspot$contributing_points)
#> # A tibble: 6 × 7
#>      id data_row   lon   lat amount distance_m amount_sum
#>   <int>    <int> <dbl> <dbl>  <dbl>      <dbl>      <dbl>
#> 1     1       60  6.56  53.2    110       200.       1315
#> 2     1       66  6.56  53.2    728       112.       1315
#> 3     1      130  6.56  53.2     36       200.       1315
#> 4     1      159  6.56  53.2    441       192.       1315
#> 5     2        1  6.57  53.2     24       167.       1147
#> 6     2      134  6.57  53.2     23       165.       1147

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
#> continuous  1 6.554816 53.19424       1315
#> observed    1 6.556074 53.19490       1205
```
