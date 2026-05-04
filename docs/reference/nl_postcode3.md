# Three-digit postcode regions in the Netherlands

An `sf` object with 3-digit postcode region geometries for the
Netherlands. Centroid coordinates are included in EPSG:4326.

## Usage

``` r
nl_postcode3
```

## Format

A simple feature object with 799 rows and 4 variables:

- areaname:

  3-digit postcode area.

- geometry:

  Postcode region geometry.

- lon:

  Longitude of the 3-digit postcode centroid.

- lat:

  Latitude of the 3-digit postcode centroid.

## Source

Adapted from Dutch postcode boundary data for package examples.

## Details

Postal codes in the Netherlands are alphanumeric and consist of four
digits followed by two uppercase letters. This object aggregates those
codes to their first three digits.

## Author

Martin Haringa
