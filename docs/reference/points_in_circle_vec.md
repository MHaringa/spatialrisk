# Filter observations within circle (vectorized)

Filter all observations in a data.frame that fall within a circle of a
specified radius drawn around a given latitude and longitude point.

## Usage

``` r
points_in_circle_vec(
  data,
  lon_center,
  lat_center,
  lon = lon,
  lat = lat,
  radius = 200
)
```

## Arguments

- data:

  data.frame with at least columns for longitude and latitude.

- lon_center:

  numeric. Representing the longitude of the circle's center.

- lat_center:

  numeric. Representing the latitude of the circle's center.

- lon:

  column name in `data` containing longitudes (default is `lon`).

- lat:

  column name in `data` containing latitudes (default is `lat`).

- radius:

  radius of the circle in meters (default is 200m).

## Value

A subset of the input data.frame containing only the observations that
fall within the specified circle.

## Author

Martin Haringa

## Examples

``` r
points_in_circle_vec(Groningen, lon_center = c(6.571561, 6.56561),
lat_center = c(53.21326, 53.20326), radius = 60)
#>      id distance_m center_index     street number letter suffix postal_code
#> 1 16611   31.36533            1 Heresingel      5   <NA>   <NA>      9711EP
#> 2  3003   47.79962            1 Heresingel     11   <NA>   <NA>      9711ER
#> 3  1191   57.56884            1 Zuiderpark   1003   <NA>   <NA>      9724AK
#>        city      lon      lat amount
#> 1 Groningen 6.571338 53.21351      5
#> 2 Groningen 6.571767 53.21367     11
#> 3 Groningen 6.571620 53.21274   1003
```
