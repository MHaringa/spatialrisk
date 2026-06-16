# Prepare fixed-radius concentration hotspot analysis

\`prepare_spatialrisk()\`, \`select_candidates()\`, and
\`optimize_hotspot()\` expose the main steps used by
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
They are useful when the intermediate search state needs to be inspected
or when the same prepared portfolio is used in more than one hotspot
search strategy.

## Usage

``` r
prepare_spatialrisk(
  data,
  value,
  radius = 200,
  cell_size = 100,
  lon = "lon",
  lat = "lat",
  crs_metric = 3035
)

select_candidates(
  x,
  grid_precision = 1,
  max_refinement_points = 1000,
  method = c("continuous", "grid", "observed"),
  threshold = NULL,
  progress = TRUE
)

optimize_hotspot(x, top_n = 1, progress = TRUE)

# S3 method for class 'spatialrisk_hotspot_workflow'
plot(x, type = c("auto", "raster", "candidates"), ...)
```

## Arguments

- data:

  A data.frame containing point-level exposures. Must include longitude,
  latitude, and the value of interest.

- value:

  A string giving the numeric column in \`data\` to aggregate within
  each radius.

- radius:

  Numeric. Radius of the circle in meters.

- cell_size:

  Numeric. Size of the raster cells used for the initial screening
  raster.

- lon:

  A string giving the longitude column in \`data\`.

- lat:

  A string giving the latitude column in \`data\`.

- crs_metric:

  Numeric. EPSG code for a projected CRS with meter units. The default
  \`3035\` is ETRS89 / LAEA Europe.

- x:

  A prepared spatial-risk workflow object returned by
  \`prepare_spatialrisk()\` or \`select_candidates()\`.

- grid_precision:

  Numeric. Approximate spacing in meters used for grid-based refinement.

- max_refinement_points:

  Positive integer. Maximum number of local points used for
  pair-intersection refinement before falling back to grid refinement.

- method:

  Hotspot search strategy. \`"continuous"\` is the default and searches
  for centres that may lie between observed points. \`"observed"\`
  searches only observed point locations. \`"grid"\` uses the
  grid-refinement workflow.

- threshold:

  Optional numeric lower bound for candidate focal cells. If \`NULL\`,
  the lower bound is estimated using the same preliminary refinement
  step as \`concentration_hotspot()\`.

- progress:

  Logical. Whether to print progress messages.

- top_n:

  Positive integer. Number of non-overlapping hotspots to return.

- type:

  Plot type. \`"auto"\` shows the prepared raster before candidate
  selection and selected focal candidate cells afterwards.

- ...:

  Additional arguments passed to \`mapview::mapview()\`.

## Value

\`prepare_spatialrisk()\` and \`select_candidates()\` return an object
of class \`spatialrisk_hotspot_workflow\`. \`optimize_hotspot()\`
returns the same \`hotspot\` object structure as
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).

## Details

The three-step interface decomposes the hotspot workflow without
replacing \`concentration_hotspot()\`. The wrapper remains the simplest
public function for normal use, while the decomposed functions make the
intermediate candidate selection visible.

In \`select_candidates()\`, \`threshold = NULL\` estimates a lower bound
by taking the highest focal raster cells, refining those cells on a
small local grid, and using the best refined value as the candidate-cell
threshold. The selected candidates are focal cells whose moving-window
sum is at least this lower bound. These candidates describe the current
search state. When \`optimize_hotspot(top_n \> 1)\` or
\`concentration_hotspot(top_n \> 1)\` is used, the points in the
selected hotspot are removed and the candidate-selection logic is run
again for the next hotspot. Therefore the number of candidate cells
shown by \`select_candidates()\` for the first iteration does not limit
the number of hotspots returned by \`top_n\`.

## Author

Martin Haringa

## Examples

``` r
portfolio <- Groningen[1:200, c("lon", "lat", "amount")]

model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                             cell_size = 100)
model <- select_candidates(model, progress = FALSE)
hotspot <- optimize_hotspot(model, top_n = 1, progress = FALSE)

hotspot$hotspots
#>   id      lon      lat amount_sum
#> 1  1 6.554816 53.19424       1315
```
