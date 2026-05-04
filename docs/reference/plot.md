# Automatically create a plot for objects obtained from neighborhood_gh_search()

Takes an object produced by \`neighborhood_gh_search()\`, and creates an
interactive map.

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

  object neighborhood object produced by \`neighborhood_gh_search()\`

- buffer:

  numeric value, show objects within buffer (in meters) from circle
  (defaults to 0)

- legend_title:

  title of legend

- palette:

  palette for points (defaults to "viridis")

- legend_position:

  legend position for points legend (defaults to "bottomleft")

- palette_circle:

  palette for circles (default to "YlOrRd")

- legend_position_circle:

  legend position for circles legend (defaults to "bottomright")

- legend_title_circle:

  title of legend for circles

- providers:

  providers to show. See \`leaflet::providers\` for a list.

- ...:

  additional arguments affecting the interactive map produced

## Value

Interactive view of highest concentration on map

## Author

Martin Haringa
