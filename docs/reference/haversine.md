# Haversine great-circle distance

Calculates the shortest distance between two points on the Earth's
surface using the Haversine formula, also known as the great-circle
distance or "as the crow flies".

## Usage

``` r
haversine(lat_from, lon_from, lat_to, lon_to, r = 6378137)
```

## Arguments

- lat_from:

  Numeric. Latitude(s) of the starting point(s) in decimal degrees
  (EPSG:4326).

- lon_from:

  Numeric. Longitude(s) of the starting point(s) in decimal degrees
  (EPSG:4326).

- lat_to:

  Numeric. Latitude(s) of the destination point(s) in decimal degrees
  (EPSG:4326).

- lon_to:

  Numeric. Longitude(s) of the destination point(s) in decimal degrees
  (EPSG:4326).

- r:

  Numeric. Radius of the Earth in meters (default = 6378137).

## Value

A numeric vector with distances in the same unit as \`r\` (default:
meters).

## Details

This function is vectorized: if multiple coordinates are supplied, it
returns one distance for each corresponding pair of points.

## References

Sinnott, R.W, 1984. Virtues of the Haversine. Sky and Telescope 68(2):
159.

## Examples

``` r
haversine(53.24007, 6.520386, 53.24054, 6.520386)
#> [1] 52.32016

lat_from <- c(53.24, 52.37)
lon_from <- c(6.52, 4.90)
lat_to   <- c(48.85, 51.92)
lon_to   <- c(2.35, 4.48)
haversine(lat_from, lon_from, lat_to, lon_to)
#> [1] 568981.78  57728.29
```
