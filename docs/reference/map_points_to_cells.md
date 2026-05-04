# Map point coordinates to cell indices

Map point coordinates to cell indices.

## Usage

``` r
map_points_to_cells(pts, focal, lon, lat, crs_from, crs_to, r = NULL)
```

## Arguments

- pts:

  data.frame with `lon` and `lat` columns in CRS 4326.

- focal:

  focal (SpatRaster).

- lon:

  character.

- lat:

  character.

- crs_from:

  crs from

- crs_to:

  crs to

- r:

  buffer around extent (in units of the crs).

## Author

Martin Haringa
