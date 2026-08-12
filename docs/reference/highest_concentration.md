# Deprecated geohash hotspot search

\`highest_concentration()\` is deprecated. Use
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
for fixed-radius hotspot detection in new analyses.

## Usage

``` r
highest_concentration(
  df,
  value,
  lon = lon,
  lat = lat,
  lowerbound = NULL,
  radius = 200,
  grid_distance = 25,
  gh_precision = 6,
  display_progress = TRUE
)
```

## Arguments

- df:

  Deprecated. Data frame of point locations.

- value:

  Deprecated. Column with values to aggregate.

- lon:

  Deprecated. Longitude column.

- lat:

  Deprecated. Latitude column.

- lowerbound:

  Deprecated. Lower bound used by the legacy geohash search.

- radius:

  Deprecated. Radius in meters.

- grid_distance:

  Deprecated. Grid distance in meters.

- gh_precision:

  Deprecated. Geohash precision used by the legacy search.

- display_progress:

  Deprecated. Whether to show a progress bar.

## Value

A legacy data frame with candidate hotspot coordinates and concentration
values.

## Details

This legacy function used a geohash-based screening workflow. It is
retained only for backward compatibility. The current high-level
interface is
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md),
which documents the supported hotspot search methods and returns the
current `hotspot` object structure.

## Author

Martin Haringa
