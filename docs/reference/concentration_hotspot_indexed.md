# Identify fixed-radius concentration hotspots using spatial indexing

Experimental Rcpp alternative to
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).
It uses a uniform-grid spatial index in metric coordinates instead of
the terra moving-window workflow. The terra-based
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
remains the default implementation.

## Usage

``` r
concentration_hotspot_indexed(
  df,
  value,
  top_n = 1,
  radius = 200,
  lon = "lon",
  lat = "lat",
  crs_metric = 3035,
  print_progress = TRUE,
  cell_size = radius
)

max_cover_circle_indexed(
  df,
  value,
  top_n = 1,
  radius = 200,
  lon = "lon",
  lat = "lat",
  crs_metric = 3035,
  print_progress = TRUE,
  cell_size = radius
)
```

## Arguments

- df:

  A data.frame containing point-level exposures. Must include columns
  for longitude, latitude, and the value of interest.

- value:

  A string giving the name of the numeric column in `df` to aggregate
  within each radius.

- top_n:

  Positive integer greater or equal to 1. Specifies how many
  non-overlapping hotspots are returned. Default is `1`.

- radius:

  Numeric. Radius of the circle in meters. Default is `200`.

- lon:

  A string giving the longitude column in `df`. Default is `"lon"`.

- lat:

  A string giving the latitude column in `df`. Default is `"lat"`.

- crs_metric:

  Numeric. Metric CRS used for the indexed search. Default is `3035`.

- print_progress:

  Logical. Whether to print progress messages when `top_n > 1`. Default
  is `TRUE`.

- cell_size:

  Numeric. Width of the spatial-index cells in meters. The default is
  `radius`.

## Value

An object of class `hotspot`. The returned list contains `conc_df` and
`pts_df`, matching the existing hotspot output structure. The components
`hotspots` and `contributing_points` are also included for the newer
descriptive API.

## Details

This implementation avoids terra moving windows and uses an Rcpp
uniform-grid spatial index to reduce unnecessary distance calculations.
It evaluates only observed point locations as candidate centres, making
it a fast deterministic benchmark for the terra implementation. It is
intended for comparison with
[`concentration_hotspot()`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md),
not as a replacement.
