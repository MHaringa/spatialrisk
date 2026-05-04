# Four-digit postcode regions in the Netherlands

An `sf` object with 4-digit postcode region geometries for the
Netherlands. Centroid coordinates are included in EPSG:4326.

## Usage

``` r
nl_postcode4
```

## Format

A simple feature object with 4053 rows and 7 variables:

- pc4:

  4-digit postcode.

- areaname:

  Name of corresponding 4-digit postcode area.

- city:

  City name.

- biggest_20cities:

  Whether the area is in one of the twenty largest cities in the
  Netherlands.

- geometry:

  Postcode region geometry.

- lon:

  Longitude of the 4-digit postcode centroid.

- lat:

  Latitude of the 4-digit postcode centroid.

## Source

Adapted from Dutch postcode boundary data for package examples.

## Details

Postal codes in the Netherlands are alphanumeric and consist of four
digits followed by two uppercase letters. This object aggregates those
codes to their first four digits.

## Author

Martin Haringa
