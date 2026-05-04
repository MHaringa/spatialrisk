# Identify the focal indices with the highest values

Generate a data.frame containing the cell indices of the focal cells
with the `n` highest values. Additionally, include columns for the
coordinates `(xy)` corresponding to the center of each cell.

## Usage

``` r
top_n_focals(focal, n)
```

## Arguments

- focal:

  focal as output from
  [`terra::focal()`](https://rspatial.github.io/terra/reference/focal.html).

- n:

  positive integer value greater or equal to 1.

## Author

Martin Haringa
