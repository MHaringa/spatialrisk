# Create interactive point map

Deprecated.

Creates an interactive map for a data.frame containing point
coordinates, optionally colored by a selected variable.

## Usage

``` r
plot_points(
  df,
  value = NULL,
  lon = "lon",
  lat = "lat",
  crs = 4326,
  at = NULL,
  layer_name = NULL,
  ...
)

map_points(
  data,
  value = NULL,
  lon = "lon",
  lat = "lat",
  crs = 4326,
  at = NULL,
  layer_name = NULL,
  ...
)
```

## Arguments

- df:

  Deprecated. Use `data` instead.

  \`plot_points()\` was renamed to \[map_points()\].

- value:

  A string giving the name of the column in `data` to be visualized. If
  `NULL`, points are mapped without a color variable.

- lon:

  A string with the name of the column containing longitude values.
  Default is `"lon"`.

- lat:

  A string with the name of the column containing latitude values.
  Default is `"lat"`.

- crs:

  Integer; EPSG code for the coordinate reference system. Default is
  `4326`.

- at:

  Optional numeric vector; breakpoints used for visualization.

- layer_name:

  Optional layer name passed to `mapview`.

- ...:

  Additional arguments passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).

- data:

  A data.frame containing columns for longitude and latitude.

## Value

An interactive `mapview` object.

## Examples

``` r
if (FALSE) { # \dontrun{
map_points(Groningen, value = "amount")
} # }
```
