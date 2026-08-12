# Deprecated geohash neighbourhood refinement

\`neighborhood_gh_search()\` is deprecated. Use
[`concentration_hotspot`](https://mharinga.github.io/spatialrisk/reference/concentration_hotspot.md)
for fixed-radius hotspot detection in new analyses.

## Usage

``` r
neighborhood_gh_search(
  hc,
  highest_geohash = 1,
  max.call = 1000,
  verbose = TRUE,
  seed = 1
)
```

## Arguments

- hc:

  Deprecated. Object returned by
  [`highest_concentration()`](https://mharinga.github.io/spatialrisk/reference/highest_concentration.md).

- highest_geohash:

  Deprecated. Number of geohashes used by the legacy refinement.

- max.call:

  Deprecated. Maximum number of calls used by the legacy
  simulated-annealing search.

- verbose:

  Deprecated. Whether to show messages from the legacy search.

- seed:

  Deprecated. Random seed for the legacy search.

## Value

A legacy data frame with refined hotspot coordinates.

## Author

Martin Haringa
