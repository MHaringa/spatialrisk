# Municipalities in the Netherlands

An `sf` object with municipal geometries for the Netherlands in 2021.
Centroid coordinates are included in EPSG:4326.

## Usage

``` r
nl_gemeente
```

## Format

A simple feature object with 380 rows and 6 variables:

- id:

  Municipality identifier.

- code:

  Municipality code.

- areaname:

  Municipality name.

- lon:

  Longitude of the municipality centroid.

- lat:

  Latitude of the municipality centroid.

- geometry:

  Municipality geometry.

## Source

Statistics Netherlands (CBS), adapted for package examples.

## Author

Martin Haringa
