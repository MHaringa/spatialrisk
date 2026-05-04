# Identify the focal cells exceeding the threshold

Generate a data.frame containing the cell indices of the focal cells
surpassing the specified threshold. Additionally, include columns for
the coordinates (xy) corresponding to the center of each cell.

## Usage

``` r
cells_above_threshold(focal, threshold)
```

## Arguments

- focal:

  focal as output from
  [`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html).

- threshold:

  lower (numeric) threshold boundary.

## Author

Martin Haringa
