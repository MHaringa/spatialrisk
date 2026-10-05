# Create interactive point map

Creates an interactive map for a data.frame containing point
coordinates, optionally colored by a selected variable.

## Usage

``` r
map_points(
  data,
  value = NULL,
  lon = "lon",
  lat = "lat",
  crs = 4326,
  at = NULL,
  layer_name = NULL,
  ...,
  basemaps = c("Esri.WorldGrayCanvas", "OpenStreetMap", "Esri.WorldImagery")
)
```

## Arguments

- data:

  A data.frame containing columns for longitude and latitude.

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

- basemaps:

  Character vector of basemap providers. The first provider is shown
  initially; the others are available in the layer control. Defaults to
  `"Esri.WorldGrayCanvas"`, `"OpenStreetMap"`, and
  `"Esri.WorldImagery"`. Passed to
  [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).

## Value

An interactive `mapview` object.

## Examples

``` r
if (FALSE) { # \dontrun{
map_points(Groningen, value = "amount")
map_points(Groningen, value = "amount", basemaps = "OpenStreetMap")
} # }
```
