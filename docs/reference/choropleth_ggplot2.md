# Choropleth map of an sf object with ggplot2

Deprecated. Use \[choropleth()\] instead.

## Usage

``` r
choropleth_ggplot2(
  sf_object,
  value = output,
  n = 7,
  dig.lab = 2,
  legend_title = "Class",
  option = "D",
  direction = 1
)
```

## Arguments

- sf_object:

  An object of class `sf` containing polygon geometries.

- value:

  Column in `sf_object` used to shade the polygons.

- n:

  Integer. Number of clusters to use in Fisher classification.

- dig.lab:

  Integer. Number of digits to display in legend labels.

- legend_title:

  Character. Title for the legend.

- option:

  Character string indicating the colormap option passed to `ggplot2`.

- direction:

  Numeric. Order of colors in the scale.

## Value

A `ggplot` object containing the choropleth map.

## Details

\`choropleth_ggplot2()\` is deprecated. Use \[choropleth()\] instead.
