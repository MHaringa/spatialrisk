# Automatically create a plot for objects obtained from highest_concentration()

Takes an object produced by \`highest_concentration()\`, and creates an
interactive map.

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

  object of class \`conc\` obtained from \`highest_concentration()\`

- grid_points:

  show grid points (TRUE), or objects (FALSE)

- legend_title:

  title of legend

- palette:

  palette for grid points (defaults to "viridis")

- legend_position:

  legend position for grid points legend (defaults to "bottomleft")

- providers:

  providers to show. See \`leaflet::providers\` for a list.

- ...:

  additional arguments affecting the interactive map produced

## Value

Interactive view of geohashes with highest concentrations

## Author

Martin Haringa
