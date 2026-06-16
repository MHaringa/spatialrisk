# Find points within radius around one or more center coordinates

This function selects rows from a data frame whose longitude/latitude
coordinates fall within a given radius (in meters) from one or more
specified center points. It also calculates the distance of each point
to the center.

## Usage

``` r
points_within_radius(
  data,
  lon_center,
  lat_center,
  lon = "lon",
  lat = "lat",
  radius = 200,
  sort = TRUE
)
```

## Arguments

- data:

  A data frame containing at least longitude and latitude columns.

- lon_center:

  Numeric scalar or vector, longitude(s) of the circle center(s).

- lat_center:

  Numeric scalar or vector, latitude(s) of the circle center(s).

- lon:

  A string with the name of the longitude column in \`data\`.

- lat:

  A string with the name of the latitude column in \`data\`.

- radius:

  Numeric, circle radius in meters. Default is 200.

- sort:

  Logical, if \`TRUE\` results are sorted by distance within each
  center.

## Value

A data frame subset of \`data\` with an extra column \`distance_m\` and
if multiple centers are provided, also a column \`center_index\`.
