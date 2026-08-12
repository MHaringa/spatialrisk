# Plot deprecated geohash hotspot results

Deprecated plotting method for objects produced by
[`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md).
For current hotspot results, use
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the object
returned by
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md).

## Usage

``` r
# S3 method for class 'conc'
plot(
  x,
  grid_points = TRUE,
  legend_title = NULL,
  palette = "viridis",
  legend_position = "bottomleft",
  providers = c("CartoDB.Positron", "nlmaps.luchtfoto"),
  ...
)
```

## Arguments

- x:

  Legacy object of class `conc`.

- grid_points:

  Logical. Whether to show grid points.

- legend_title:

  Optional legend title.

- palette:

  Palette used for the point layer.

- legend_position:

  Legend position for the point layer.

- providers:

  Leaflet tile providers.

- ...:

  Additional arguments passed to the interactive map.

## Value

An interactive map.

## Author

Martin Haringa
