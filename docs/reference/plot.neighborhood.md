# Plot deprecated geohash neighbourhood results

Deprecated plotting method for objects produced by
[`neighborhood_gh_search()`](https://mharinga.github.io/spatialrisk/reference/neighborhood_gh_search.md).
For current hotspot results, use
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the object
returned by
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).

## Usage

``` r
# S3 method for class 'neighborhood'
plot(
  x,
  buffer = 0,
  legend_title = NULL,
  palette = "viridis",
  legend_position = "bottomleft",
  palette_circle = "YlOrRd",
  legend_position_circle = "bottomright",
  legend_title_circle = "Highest concentration",
  providers = c("CartoDB.Positron", "nlmaps.luchtfoto"),
  ...
)
```

## Arguments

- x:

  Legacy object of class `neighborhood`.

- buffer:

  Numeric. Buffer around the circle in meters.

- legend_title:

  Optional legend title for the point layer.

- palette:

  Palette used for the point layer.

- legend_position:

  Legend position for the point layer.

- palette_circle:

  Palette used for the circle layer.

- legend_position_circle:

  Legend position for the circle layer.

- legend_title_circle:

  Optional legend title for the circle layer.

- providers:

  Leaflet tile providers.

- ...:

  Additional arguments passed to the interactive map.

## Value

An interactive map.

## Author

Martin Haringa
