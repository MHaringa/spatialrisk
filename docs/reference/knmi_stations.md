# KNMI weather stations

A data frame containing station IDs and location metadata for official
KNMI weather stations in the Netherlands.

## Usage

``` r
knmi_stations
```

## Format

A data frame with 50 rows and 7 variables:

- station:

  Station ID.

- city:

  City where the station is located.

- lon:

  Longitude of the station in EPSG:4326.

- lat:

  Latitude of the station in EPSG:4326.

- altitude:

  Altitude of the station in meters.

- X:

  Projected X coordinate of the station in EPSG:32631.

- Y:

  Projected Y coordinate of the station in EPSG:32631.

## Source

Royal Netherlands Meteorological Institute (KNMI), adapted for package
examples.

## Author

Martin Haringa
