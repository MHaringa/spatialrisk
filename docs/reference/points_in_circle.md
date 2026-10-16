# Deprecated alias for points_within_radius()

\`points_in_circle()\` is deprecated. Use \[points_within_radius()\]
instead.

## Usage

``` r
points_in_circle(
  data,
  lon_center,
  lat_center,
  lon = lon,
  lat = lat,
  radius = 200,
  sort = TRUE
)
```

## Arguments

- data:

  A data.frame containing point coordinates.

- lon_center:

  Longitude of the circle centre.

- lat_center:

  Latitude of the circle centre.

- lon:

  Column name with longitude.

- lat:

  Column name with latitude.

- radius:

  Radius in meters.

- sort:

  Logical. Whether to sort output by distance.

## Value

See \[points_within_radius()\].
