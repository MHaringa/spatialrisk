# Deprecated alias for radius_sum()

\`concentration()\` is deprecated. Use \[radius_sum()\] instead.

## Usage

``` r
concentration(
  sub,
  full,
  value,
  lon_sub = lon,
  lat_sub = lat,
  lon_full = lon,
  lat_full = lat,
  radius = 200,
  display_progress = TRUE,
  result_col = "radius_sum"
)
```

## Arguments

- sub:

  Deprecated. Use `targets` in \[radius_sum()\] instead.

- full:

  Deprecated. Use `reference` in \[radius_sum()\] instead.

- value:

  Column name with values to sum.

- lon_sub:

  Deprecated. Use `lon_targets` instead.

- lat_sub:

  Deprecated. Use `lat_targets` instead.

- lon_full:

  Deprecated. Use `lon_reference` instead.

- lat_full:

  Deprecated. Use `lat_reference` instead.

- radius:

  Radius in meters.

- display_progress:

  Deprecated. Use `progress` in
  [`radius_sum()`](https://mharinga.github.io/spatialrisk/reference/radius_sum.md)
  instead.

- result_col:

  Name of the output column.

## Value

See \[radius_sum()\].
