# Convert Coordinate Reference System (CRS)

Convert Coordinate Reference System (CRS) of a data.frame from one CRS
to another.

## Usage

``` r
convert_crs_df(
  df,
  crs_from = 3035,
  crs_to = 4326,
  lon_from = "x",
  lat_from = "y",
  lon_to = "lon",
  lat_to = "lat"
)
```

## Arguments

- df:

  data.frame to be converted.

- crs_from:

  CRS code of the original coordinate system (default: 3035).

- crs_to:

  CRS code of the target coordinate system (default: 4326).

- lon_from:

  column name of longitude values in `df` (default: "x").

- lat_from:

  column name of latitude values in `df` (default: "y").

- lon_to:

  column name for longitude values in the converted data frame (default:
  "lon").

- lat_to:

  column name for latitude values in the converted data frame (default:
  "lat").

## Value

data.frame with converted coordinates

## Author

Martin Haringa
