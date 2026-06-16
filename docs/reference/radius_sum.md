# Sum values within a radius around target coordinates

Calculates the sum of all observations from a reference data set that
fall within a given radius (in meters) of each target point.

## Usage

``` r
radius_sum(
  targets,
  reference,
  value,
  lon_targets = "lon",
  lat_targets = "lat",
  lon_reference = "lon",
  lat_reference = "lat",
  radius = 200,
  progress = TRUE,
  result_col = "radius_sum"
)
```

## Arguments

- targets:

  A data.frame of target points for which sums are calculated. Must
  include at least columns for longitude and latitude.

- reference:

  A data.frame containing reference points. Must include at least
  columns for longitude, latitude, and the value of interest to
  summarize.

- value:

  A string giving the name of the column in \`reference\` to be summed.

- lon_targets:

  A string with the name of the longitude column in \`targets\`. Default
  is \`"lon"\`.

- lat_targets:

  A string with the name of the latitude column in \`targets\`. Default
  is \`"lat"\`.

- lon_reference:

  A string with the name of the longitude column in \`reference\`.
  Default is \`"lon"\`.

- lat_reference:

  A string with the name of the latitude column in \`reference\`.
  Default is \`"lat"\`.

- radius:

  Numeric. Radius of the circle in meters. Must be positive (default:
  200).

- progress:

  Logical. Whether to display a progress bar. Default is \`TRUE\`.

- result_col:

  A string giving the name of the output column. Default is
  \`"radius_sum"\`.

## Value

A data.frame equal to \`targets\` with an additional numeric column
named by \`result_col\` containing the summed values from \`reference\`.

## Details

This function uses a C++ backend for efficient distance calculations
(Haversine formula).

## Author

Martin Haringa

## Examples

``` r
targets <- data.frame(location = c("p1", "p2"),
                      lon = c(6.561561, 6.561398),
                      lat = c(53.21369, 53.21326))

reference <- data.frame(lon = c(6.5614, 6.5620, 6.5630),
                        lat = c(53.2132, 53.2140, 53.2150),
                        amount = c(10, 20, 15))

radius_sum(targets, reference, value = "amount", radius = 100,
           progress = FALSE)
#>   location      lon      lat radius_sum
#> 1       p1 6.561561 53.21369         30
#> 2       p2 6.561398 53.21326         30
```
