# Convert data.frame to simple features (sf) object

This function converts a data.frame to a simple features (sf) object.

## Usage

``` r
convert_df_to_sf(df, lon = "lon", lat = "lat", crs_from = 4326, crs_to = 3035)
```

## Arguments

- df:

  data.frame containing longitude and latitude columns

- lon:

  column name for longitude values (default: "lon").

- lat:

  column name for latitude values (default: "lat").

- crs_from:

  crs of the original coordinate system (default: 4326).

- crs_to:

  crs of the target coordinate system (default: 3035).

## Value

Returns an sf object with the specified coordinate reference system.

## Author

Martin Haringa
